;
; mon_dos.asm
;

		nolist
		include "exec/types.i"
		include "exec/memory.i"
		include "libraries/dos.i"
		include "offsets.i"
		list

		include	"monitor.i"
;
; This module defines the following command routines:
;
;	directory,loadseg,unloadseg,showseglist,abs_save,abs_load,redirect
;	new_cli,deletefile,current_dir
;
; and the following public subroutine:
;
;	find_hunk_addr,loadseg1,unload_seg
;
		xref	generic_error
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
		beq.s	dir_error		;if i/o error

		move.l	D7,D1
		move.l	a5,D2
		lib	Examine
		tst.l	D0
		beq.s	dir_error		;branch if Examine failed

		tst.l	fib_DirEntryType(a5)
		bpl.s	dir_loop
		bsr.s	DisplayDirLine	;if Examine found a file, display information about it
		bra.s	dir_show_free_blocks		;then number of free blocks

dir_loop	;Examine found a directory, ExNext() it
		move.l	D7,D1
		move.l	a5,D2
		lib	ExNext
		tst.l	D0
		bne.s	dir_display_entry
		lib	IoErr
		cmp.l	#ERROR_NO_MORE_ENTRIES,D0
		bne.s	dir_error1		;branch if a real dos error
		bra.s	dir_show_free_blocks	;no more entries, the display free blocks

dir_display_entry
		bsr.s	DisplayDirLine
		bne.s	dir_show_free_blocks	;branch if user pressed break (control-c)
		bra.s	dir_loop

dir_error	lib	IoErr
dir_error1	tst.l	D0
		beq.s	dir_end
		bsr	DOSErr
		bra.s	dir_end

dir_show_free_blocks
		move.l	D7,D1
		move.l	a5,D2
		lib	Info		;get info about the disk
		tst.l	D0
		beq.s	dir_error

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
		bmi.s	printf_file
		lea	dir_fmt(pc),a0
		bra.s	printf_dirfile

printf_file	move.l	fib_Size(a5),d1
		lea	file_fmt(pc),a0
printf_dirfile	call	printf
		call	JUMP,CheckKeys

*** PRINT DOS ERROR NUMBER ***
DOSErr		;error number in D0
		cmp.l	#ERROR_OBJECT_NOT_FOUND,d0
		beq.s	01$
		lea	dos_error_fmt(pc),a0
		call	JUMP,printf_window

01$		lea	not_found_txt(pc),a0
		call	JUMP,printstring_a0_window

**** LOAD SEGMENT ****
		cmd	loadseg

		tst.l	mon_SegList(a4)
		bne.s	oldseg		;can't do this before old segment is unloaded

		moveq	#0,d5
		cmp.b	#'+',(a3)
		bne.s	1$
		addq.l	#1,a3
		st	d5

1$		call	GetName
		move.l	d0,d1
		beq	generic_error

		pub	loadseg1

		move.l	d1,d4
		lib	Dos,LoadSeg
		move.l	D0,mon_SegList(a4)
		beq.s	segerr		;branch if LoadSeg failed

		move.l	d0,d1
		lsl.l	#2,d0		;BPTR->APTR
		addq.l	#4,d0		;skip next segment pointer
		move.l	d0,mon_RegPC(a4) ;PC points to first instruction
		move.l	d0,mon_CurrentAddr(a4)
		move.l	d0,-(sp)

		moveq	#0,d0
01$		lsl.l	#2,d1
		beq.s	02$
		move.l	d1,a0
		move.l	(a0),d1
		addq.l	#1,d0
		bra.s	01$

02$		move.l	d0,mon_NumHunks(a4)
		lea	s_txt(pc),a0
		moveq	#1,d2
		cmp.l	d0,d2
		bne.s	03$
		lea	null_txt(pc),a0
03$		move.l	a0,d1
		move.l	(sp)+,d2
		lea	seg_addr_fmt(pc),a0
		call	printf

		tst.b	d5
		beq.s	04$

		move.l	d4,a0
		call	load_symbols

04$		bra.s	mjump

oldseg		;message 'unload old segment first'
		lea	unload_old_txt(pc),A0
		call	printstring_a0_window
		bra.s	mjump

segerr		;come here if LoadSeg failed
		lib	IoErr
		bsr	DOSErr
mjump		rts

nosegerr	;error 'no segment loaded'
		lea	no_segment_txt(pc),A0
		call	printstring_a0_window
		bra.s	mjump

**** UNLOAD SEGMENT ****
		cmd	unloadseg

		move.l	mon_SegList(a4),D1
		beq.s	nosegerr	;branch if no segment

		pub	unload_seg

		move.l	mon_SegList(a4),d1
		beq.s	2$
		lib	Dos,UnLoadSeg
		clr.l	mon_SegList(a4)	;remember to clear the seglist pointer

		move.l	mon_HunkTypeTable(a4),d1
		beq.s	1$
		move.l	d1,a1
		move.l	mon_NumHunks(a4),d0
		lsl.l	#2,d0
		lib	Exec,FreeMem
		clr.l	mon_HunkTypeTable(a4)

1$		call	clear_hunk_vars
		clr.l	mon_NumHunks(a4)
2$		st	mon_RelBaseReg(a4)
		rts

**** SEGMENT LIST ****
		cmd	showseglist

		move.l	mon_SegList(a4),D4
		beq.s	nosegerr	;branch if no seglist

		lea	seghead_txt(pc),A0
		call	printstring_a0

		move.l	mon_HunkTypeTable(a4),a3
		moveq	#0,d5
		move.l	a3,d0
		beq.s	1$

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
		call	printf

		move.l	a3,d0
		beq.s	1$
		move.l	(a3)+,d0
		and.l	#$3fffffff,d0
		sub.l	#$3e9,d0
		bcs.s	1$
		moveq	#2,d1
		cmp.l	d1,d0
		bhi.s	1$
		lsl.l	#2,d0
		move.b	-4(a3),d1
		lsr.b	#4,d1
		eor.b	#$0c,d1
		lea	chipfast_txt(pc),a0
		add.l	a0,d1
		lea	hunktypes_txt(pc),a0
		add.l	a0,d0
		lea	htype_fmt(pc),a0
		call	printf

1$		emit LF

		addq.l	#1,d5
		move.l	(A2),D4		;get next segment pointer
		call	CheckKeys
		bne.s	mjump2
		tst.l	D4
		bne.s	segloop
mjump2		rts

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
		beq.s	9$
		move.l	d0,a1
		lea	4(a1),a2
		cmp.l	a2,a0
		bcs.s	2$
		move.l	-4(a1),d2
		lea	-4(a1,d2.l),a2
		cmp.l	a2,a0
		bcs.s	9$

2$		move.l	(a1),d0
		addq.l	#1,d1
		bra.s	1$

9$		movem.l	(sp)+,d2/a2
		rts

*** SAVE ABSOLUTE (using DOS Write)****
		cmd	abs_save

		call	GetExpr
		move.l	D0,a5
		call	GetExpr
		move.l	D0,D6

		call	GetName
		move.l	d0,d1
		beq	generic_error

		move.l	#MODE_NEWFILE,D2
		lib	Dos,Open
		move.l	D0,D7
		bne.s	abs_save_1

dos_err		lib	IoErr
		bra	DOSErr

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
		beq.s	dos_err
		move.l	D7,D1
		move.l	a5,D2
		move.l	#$7FFFFFFF,D3	;MaxInt (the file can't be longer than this)
		lib	Read		;read from file, until EOF, return actual length
		tst.l	D0
		ble.s	abs_load_1
		move.l	a5,D5
		move.l	D0,D6
		call	showrange
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
		beq.s	redir1
		lib	Close		;if so, then close redirection file
		move.l	mon_WinFile(a4),mon_OutputFile(a4)	;standard output

redir1		call	skipspaces
		tst.b	(A3)
		beq.s	redir9

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
		lib	Seek
		rts

redir_no_oldfile
		move.l	d3,d1
		move.l	#MODE_NEWFILE,D2
		lib	Open	;open redirection file
		tst.l	D0
		beq	dos_err

		move.l	D0,mon_OutputFile(a4)
redir9		rts

*** NEW SHELL/CLI ***
;
; Now tries first to start a shell...TR 1990-05-24
;
		cmd	new_cli

		lea	NewShellCom(pc),A0
		move.l	A0,D1
		moveq	#0,D2
		move.l	mon_WinFile(a4),D3
		lib	Dos,Execute
		tst.l	d0
		beq.s	01$
		lib	IoErr
		tst.l	d0
		beq.s	redir9

01$		lea	NewCLICom(pc),a0
		move.l	a0,d1	
		moveq	#0,d2
		move.l	mon_WinFile(a4),d3
		jlib	Execute

*** DELETE A FILE ***
		cmd	deletefile

		call	GetName
		move.l	d0,d1
		beq	generic_error

		lib	Dos,DeleteFile
		tst.l	D0
		beq	dos_err
		rts

*** CHANGE CURRENT DIRECTORY (CD) ***
		cmd	current_dir

		getbase	Dos
		call	GetName
		move.l	d0,d1
		beq.s	cd_2	;if no name set currentdir to zero lock

		moveq	#SHARED_LOCK,D2
		lib	Lock
		tst.l	D0
		beq	dos_err
		move.l	D0,D1

cd_2		lib	CurrentDir
		bclr	#MONB_FIRSTCD,mon_Flags(a4)
		bne.s	cd_3
		move.l	D0,D1
		lib	UnLock	;unlock previous current directory

cd_3		rts


unload_old_txt	dc.b	'Unload old segment first',LF,0
seg_addr_fmt	dc.b	'%ld hunk%s at '
		dc.b	'$%08lx',LF,0
s_txt		dc.b	's, first hunk'
null_txt	dc.b	0
dos_error_fmt	dc.b	'DOS error %ld',LF,0

file_fmt	dc.b	'%-24.30s %5ld',LF,0
dir_fmt		dc.b	'%-24.30s (dir)',LF,0
freeblocks_fmt	dc.b	'%ld Blocks free.',LF,0


NewShellCom	dc.b	'NewShell',0
NewCLICom	dc.b	'NewCLI',0

hunktypes_txt	dc.b	'codedatabss '
chipfast_txt	dc.b	'????fastchip',0
htype_fmt	dc.b	'   %4.4s %1.4s',0

seghead_txt	dc.b	'Segment list:',LF
		dc.b	'  #   startloc   endloc    length',0
type_txt	dc.b	'   type',0

seglist_fmt	dc.b	'%3ld  $%08lx  $%08lx %7ld',0

no_segment_txt	dc.b	'No segment loaded',LF,0
not_found_txt	dc.b	'File/dir not found',LF,0


		end
