;
; mon_dos.asm
;

;
; note: this module currently requires 2.0 includes to assemble
;

		nolist
		include "exec/types.i"
		include "exec/memory.i"
		include	"exec/libraries.i"
		include "dos/dos.i"
		include	"dos/dosextens.i"
		include	"dos/dostags.i"
		include "offsets.i"
		list

		include	"monitor.i"
;
; This module defines the following command routines:
;
;	directory,loadseg,unloadseg,showseglist,abs_save,abs_load,redirect
;	shell,deletefile,current_dir
;
; and the following public subroutine:
;
;	find_hunk_addr,loadseg1,unload_seg
;
		xref	generic_error
		xref	out_range_error
		xref	out_memory_error


*** DIRECTORY ***
		cmd	directory

		moveq	#0,D7
		move.l	#fib_SIZEOF,D0	;allocate FileInfoBlock
*** we use the same memory space for the InfoData structure
		move.l	#MEMF_PUBLIC!MEMF_CLEAR,D1
		lib	Exec,AllocMem
		tst.l	D0
		beq	out_memory_error
		move.l	D0,a5		;we keep the fib-pointer in a5

		call	GetName
		move.l	d0,d1
		bne	dir_lock
		lea	null_txt(pc),a0
		move.l	a0,d1

dir_lock	moveq	#SHARED_LOCK,D2
		lib	Dos,Lock
		move.l	D0,D7		;we keep the lock pointer in D7
		beq.b	dir_error		;if i/o error

		move.l	D7,D1
		move.l	a5,D2
		lib	Examine
		tst.l	D0
		beq.b	dir_error		;branch if Examine failed

		tst.l	fib_DirEntryType(a5)
		bpl.b	dir_loop
		bsr.b	DisplayDirLine	;if Examine found a file, display information about it
		bra.b	dir_show_free_blocks		;then number of free blocks

dir_loop	;Examine found a directory, ExNext() it
		move.l	D7,D1
		move.l	a5,D2
		lib	ExNext
		tst.l	D0
		bne.b	dir_display_entry
		lib	IoErr
		cmp.l	#ERROR_NO_MORE_ENTRIES,D0
		bne.b	dir_error1		;branch if a real dos error
		bra.b	dir_show_free_blocks	;no more entries, the display free blocks

dir_display_entry
		bsr.b	DisplayDirLine
		bne.b	dir_show_free_blocks	;branch if user pressed break (control-c)
		bra.b	dir_loop

dir_error	lib	IoErr
dir_error1	tst.l	D0
		beq.b	dir_end
		call	DosErr
		bra.b	dir_end

dir_show_free_blocks
		move.l	D7,D1
		move.l	a5,D2
		lib	Info		;get info about the disk
		tst.l	D0
		beq.b	dir_error

		move.l	id_NumBlocks(a5),D0
		sub.l	id_NumBlocksUsed(a5),D0
		lea	freeblocks_fmt(pc),a0
		call	printf

dir_end		move.l	D7,D1		;it is safe to UnLock a zero lock
		lib	UnLock
		move.l	a5,A1
		move.l	#fib_SIZEOF,D0
		jlib	Exec,FreeMem		;free FileInfoBlock

*** PRINT ONE LINE OF DIREECTORY ***
DisplayDirLine	;fib-pointer in a5
		lea	fib_FileName(a5),a0
		move.l	a0,d0
		tst.l	fib_DirEntryType(a5)	;positive=dir, negative=file
		bmi.b	printf_file
		lea	dir_fmt(pc),a0
		bra.b	printf_dirfile

printf_file	move.l	fib_Size(a5),d1
		lea	file_fmt(pc),a0
printf_dirfile	call	printf
		call	JUMP,CheckKeys

**** LOAD SEGMENT ****
		cmd	loadseg

		tst.l	mon_SegList(a4)
		bne	oldseg		;can't do this before old segment is unloaded

		moveq	#0,d5
		cmp.b	#'+',(a3)
		bne.b	1$
		addq.l	#1,a3
		st	d5

1$		call	GetName
		move.l	d0,d1
		beq	generic_error

		pub	loadseg1

		move.l	d1,d4
		lib	Dos,LoadSeg
		move.l	D0,mon_SegList(a4)
		beq.b	segerr		;branch if LoadSeg failed

		move.l	d0,d1
		lsl.l	#2,d0		;BPTR->APTR
		addq.l	#4,d0		;skip next segment pointer
		move.l	d0,mon_RegPC(a4) ;PC points to first instruction
		move.l	d0,mon_CurrentAddr(a4)
		move.l	d0,-(sp)

		moveq	#0,d0
01$		lsl.l	#2,d1
		beq.b	02$
		move.l	d1,a0
		move.l	(a0),d1
		addq.l	#1,d0
		bra.b	01$

02$		move.l	d0,mon_NumHunks(a4)
		lea	s_txt(pc),a0
		moveq	#1,d2
		cmp.l	d0,d2
		bne.b	03$
		lea	null_txt(pc),a0
03$		move.l	a0,d1
		move.l	(sp)+,d2
		lea	seg_addr_fmt(pc),a0
		call	printf

;
; setup an empty command line if not running this on startup
;
		btst	#MONB_STARTUP,mon_Flags(a4)
		bne.b	04$

		lea	mon_CmdLineBuf(a4),a0
		move.b	#LF,(a0)
		move.l	a0,mon_AddrRegs(a4)
		moveq	#1,d0
		move.l	d0,mon_DataRegs(a4)

04$
		tst.b	d5
		beq.b	ret_01

		move.l	d4,a0
		call	JUMP,load_symbols

oldseg		;message 'unload old segment first'
		lea	unload_old_txt(pc),A0
		call	JUMP,printstring_a0_window

segerr		;come here if LoadSeg failed

 		pub	DosIoErr

DosIoErr	lib	Dos,IoErr

*** PRINT DOS ERROR NUMBER ***
		pub	DosErr

		;error number in D0
		cmp.l	#ERROR_OBJECT_NOT_FOUND,d0
		beq.b	dos_err205
		lea	dos_error_fmt(pc),a0
		call	JUMP,printf_window

dos_err205	lea	not_found_txt(pc),a0
		call	JUMP,printstring_a0_window

nosegerr	;error 'no segment loaded'
		lea	no_segment_txt(pc),A0
		call	JUMP,printstring_a0_window

**** UNLOAD SEGMENT ****
		cmd	unloadseg

		move.l	mon_SegList(a4),D1
		beq.b	nosegerr	;branch if no segment

		pub	unload_seg

		st	mon_RelBaseReg(a4)
		move.l	mon_SegList(a4),d1
		beq.b	ret_01

		lib	Dos,UnLoadSeg
		clr.l	mon_SegList(a4)	;remember to clear the seglist pointer

		move.l	mon_HunkTypeTable(a4),d1
		beq.b	clr_hunkvars
		move.l	d1,a1
		move.l	mon_NumHunks(a4),d0
		lsl.l	#(2+1),d0		;2*4 bytes/hunk
		lib	Exec,FreeMem
		clr.l	mon_HunkTypeTable(a4)

clr_hunkvars	call	clear_hunk_vars

unload_01	clr.l	mon_NumHunks(a4)
ret_01		rts

**** SEGMENT LIST ****
		cmd	showseglist

		move.l	mon_SegList(a4),D4
		beq.b	nosegerr	;branch if no seglist

		lea	seghead_txt(pc),A0
		call	printstring_a0

		move.l	mon_HunkTypeTable(a4),a5
		moveq	#0,d5
		move.l	a5,d0
		beq.b	1$

		lea	type_txt(pc),a0
		call	printstring_a0

1$		emit	LF

segloop		lsl.l	#2,d4		;BPTR->APTR
		move.l	d4,a2
		move.l	d4,d1
		addq.l	#4,d1
		move.l	-4(a2),d3
		subq.l	#8,d3
		move.l	d1,d2
		add.l	d3,d2
		subq.l	#1,d2
		move.l	d5,d0
		lea	seglist_fmt(pc),a0
		call	fmtstring

		lea	mon_OutputBuf(a4),a3
1$		tst.b	(a3)+
		bne.b	1$
		subq.l	#1,a3

		move.l	a5,d0
		beq.b	seglist_endline

		moveq	#SPACE,d1
		move.b	d1,(a3)+
		move.b	d1,(a3)+

		move.l	(a5)+,d0
		and.l	#$3fffffff,d0
		sub.l	#$3e9,d0		;HUNK_CODE
		bcs.b	hunk_err
		moveq	#2,d1
		cmp.l	d1,d0
		bhi.b	hunk_err
		lea	hunktypes_txt(pc),a0
		call	getnth
		move.l	a0,a1
		call	putstring

		moveq	#0,d0
		move.b	-4(a5),d0
		lsr.b	#6,d0
		subq.b	#1,d0
		bmi.b	2$

		putchr	SPACE
		lea	chipfast_txt(pc),a0
		call	getnth
		move.l	a0,a1
		call	putstring

2$		move.l	-4(a2),d0
		subq.l	#8,d0		;d0 := allocated hunk length
		move.l	d0,d2
		sub.l	(a5)+,d2
		beq.b	seglist_endline
		sub.l	d2,d0

		putchr	SPACE
		putchr	'('
		call	put_decimal_number
		putchr	'/'
		move.l	d2,d0
		call	put_decimal_number
		putchr	')'
		bra.b	seglist_endline

hunk_err	lea	err1_txt(pc),a1
		call	putstring

seglist_endline	endline
		call	printstring

		addq.l	#1,d5
		move.l	(A2),D4		;get next segment pointer
		call	CheckKeys
		bne.b	3$
		tst.l	D4
		bne	segloop
3$		rts

;
; return hunk start address (actually addr of pointer to next hunk)
; if the given memory block is in a hunk, else return zero.
;
; inputs:
;  a0 - address
;
; outputs:
;  d0 - hunk start addr or zero
;  d1 - hunk number if d0 is nonzero
;
		pub	find_hunk_addr

		movem.l	d2/a2,-(sp)
		moveq	#0,d1
		move.l	mon_SegList(a4),d0

1$		lsl.l	#2,d0
		beq.b	9$
		move.l	d0,a1
		lea	4(a1),a2
		cmp.l	a2,a0
		bcs.b	2$
		move.l	-4(a1),d2
		lea	-4(a1,d2.l),a2
		cmp.l	a2,a0
		bcs.b	9$

2$		move.l	(a1),d0
		addq.l	#1,d1
		bra.b	1$

9$		movem.l	(sp)+,d2/a2
		rts

*** SAVE ABSOLUTE (using DOS Write)****
		cmd	abs_save

		call	GetExpr
		move.l	D0,a5
		call	skipspaces
		cmp.b	#':',(a3)
		beq.b	abs_save_get_len1
		cmp.b	#'.',(a3)
		bne.b	abs_save_get_len2

w_periods	cmp.b	#'.',(a3)+
		beq.b	w_periods
		subq.l	#1,a3
		call	GetExpr		;get end addr
		sub.l	a5,d0
		bcs	out_range_error
		addq.l	#1,d0
		bra.b	abs_save_set_len

abs_save_get_len1
		addq.l	#1,a3
abs_save_get_len2
		call	GetExpr		;get length
abs_save_set_len
		move.l	D0,D6

		call	GetName
		move.l	d0,d1
		beq	generic_error

		move.l	#MODE_NEWFILE,D2
		lib	Dos,Open
		move.l	D0,D7
		bne.b	abs_save_1
		call	JUMP,DosIoErr

abs_save_1	move.l	D7,D1
		move.l	a5,D2
		move.l	D6,D3
		lib	Write
		move.l	D7,D1
		jlib	Close

*** LOAD ABSOLUTE (using DOS Read) ***
		cmd	abs_load

		call	GetExpr
		move.l	D0,a5

		call	GetName
		move.l	d0,d1
		beq	generic_error

		move.l	#MODE_OLDFILE,D2
		lib	Dos,Open	;open file
		move.l	D0,D7
		beq	DosIoErr
		move.l	D7,D1
		move.l	a5,D2

		moveq	#-1,d3
		lsr.l	#1,d3	;d3 := $7fffffff
				;MaxInt (the file can't be longer than this)
		lib	Read	;read from file, until EOF, return actual length
		tst.l	D0
		ble.b	abs_load_1

		move.l	a5,D5
		move.l	D0,D6
		call	showrange
		move.l	a5,mon_CurrentAddr(a4)

abs_load_1	move.l	D7,D1
		jlib	Close	;close the file

*** REDIRECT OUTPUT ***
;
; Now tries to append, if file already exists. -- 1991-07-26 -- 1.42
;
		cmd	redirect

		getbase	Dos
		move.l	mon_OutputFile(a4),D1	;is output currently redirected
		cmp.l	mon_WinFile(a4),D1
		beq.b	redir1
		lib	Close		;if so, then close redirection file
		move.l	mon_WinFile(a4),mon_OutputFile(a4)	;standard output

redir1		call	skipspaces
		tst.b	(A3)
		beq.b	ret_02

		call	GetName
		move.l	d0,d3
		beq	generic_error

		move.l	d3,d1
		move.l	#MODE_OLDFILE,d2
		lib	Open
		move.l	d0,d1
		beq	redir_no_oldfile
		move.l	d0,mon_OutputFile(a4)
		moveq	#0,d2
		moveq	#OFFSET_END,d3
		jlib	Seek

redir_no_oldfile
		move.l	d3,d1
		move.l	#MODE_NEWFILE,D2
		lib	Open	;open redirection file
		tst.l	D0
		beq	DosIoErr

		move.l	D0,mon_OutputFile(a4)
ret_02		rts

*** NEW SHELL/CLI ***
;
; Now tries first to start a shell...TR 1990-05-24
; -- and now can also be used to give any cli/shell commands.. TR 1991-11-03
;
		cmd	shell

		call	GetName
		getbase	Dos
		move.l	mon_WinFile(a4),d3
		move.l	d0,d5
		beq.b	new_shell

	ifnd	SET_CONSOLE_TASK
		move.l	mon_Task(a4),a2
		move.l	pr_ConsoleTask(a2),d6

		move.l	d3,d1
		lsl.l	#2,d1
		move.l	d1,a1
		move.l	fh_Type(a1),pr_ConsoleTask(a2)
	endc

		lea	con_name(pc),a0
		move.l	a0,d1
		move.l	#MODE_OLDFILE,d2
		lib	Open

	ifnd	SET_CONSOLE_TASK
		move.l	d6,pr_ConsoleTask(a2)
	endc

		move.l	d0,d4
		beq.b	ret_02

		move.l	d3,d0
		moveq	#0,d1
		call	SetConMode

		move.l	d5,d1
		bsr.b	shellcmd

		move.l	d3,d0
		moveq	#1,d1
		call	SetConMode
		move.l	d4,d1
		jlib	Close

new_shell	move.l	d3,d4
		moveq	#0,d3
		lea	NewShellCom(pc),A0
		move.l	A0,D1
		bsr.b	shellcmd
		tst.l	d0
		beq.b	ret_02

		lea	NewCLICom(pc),a0
		move.l	a0,d1

shellcmd	cmp.w	#36,LIB_VERSION(a6)
		bcc.b	use_system
; on 1.3, use Execute()
		moveq	#0,d2
		lib	Execute
		tst.l	d0
		beq.b	1$
		lib	IoErr
		rts
1$		moveq	#1,d0
		rts

; on 2.0 use System()
use_system	lea	-20(sp),sp
		move.l	sp,a0

		ifne	SYS_Output-$80000022
		FAIL	SYS_Output Tag value wrong!
		endc

		moveq	#$45,d0
		ror.l	#1,d0		;now d0 should be SYS_Output...
		move.l	d0,(a0)+
		move.l	d4,(a0)+
		subq.l	#SYS_Output-SYS_Input,d0
		move.l	d0,(a0)+
		move.l	d3,(a0)+

		ifne	TAG_DONE
		move.l	#TAG_DONE,(a0)
		endc
		ifeq	TAG_DONE
		clr.l	(a0)
		endc

		move.l	sp,d2
		lib	SystemTagList
		lea	20(sp),sp
		rts

*** DELETE A FILE ***
		cmd	deletefile

		call	GetName
		move.l	d0,d1
		beq	generic_error

		lib	Dos,DeleteFile
		tst.l	D0
		beq	DosIoErr
rts_01		rts

*** CHANGE CURRENT DIRECTORY (CD) ***
		cmd	current_dir

		getbase	Dos
		call	GetName
		move.l	d0,d1
		beq.b	cd_2	;if no name set currentdir to zero lock

		moveq	#SHARED_LOCK,D2
		lib	Lock
		move.l	D0,D1
		beq	DosIoErr

cd_2		lib	CurrentDir
		bclr	#MONB_FIRSTCD,mon_Flags(a4)
		bne.b	rts_01
		move.l	D0,D1
		jlib	UnLock	;unlock previous current directory

;
;
unload_old_txt	dc.b	'Unload old segment first',LF,0
seg_addr_fmt	dc.b	'%ld hunk%s at '
		dc.b	'$%08lx',LF,0
s_txt		dc.b	's, first hunk'
null_txt	dc.b	0
dos_error_fmt	dc.b	'Dos error %ld',LF,0

file_fmt	dc.b	'%-24.30s %5ld',LF,0
dir_fmt		dc.b	'%-24.30s (dir)',LF,0
freeblocks_fmt	dc.b	'%ld Blocks free.',LF,0


NewShellCom	dc.b	'NewShell',0
NewCLICom	dc.b	'NewCLI',0
con_name	dc.b	'*',0

hunktypes_txt	dc.b	'code',0
		dc.b	'data',0
		dc.b	'bss',0

chipfast_txt	dc.b	'chip',0
		dc.b	'fast',0
err1_txt	dc.b	'???',0

seghead_txt	dc.b	'Segment list:',LF
		dc.b	'  #   startloc   endloc    length',0
type_txt	dc.b	'  type',0

seglist_fmt	dc.b	'%3ld  $%08lx  $%08lx %7ld',0

no_segment_txt	dc.b	'No segment loaded',LF,0
not_found_txt	dc.b	'File/dir not found',LF,0


		end
