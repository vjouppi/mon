;
; mon_dos.asm
;

		include	"monitor.i"

		xdef	loadseg1

		xref	seghead
		xref	seglistfmt

		xref	do_options

		xref	putch
		xref	showrange

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
		beq.s	dir7		;if i/o error
		move.l	D7,D1
		move.l	a5,D2
		lib	Examine
		tst.l	D0
		beq.s	dir7		;branch if Examine failed
		tst.l	fib_DirEntryType(a5)
		bpl.s	dir1
		bsr.s	DisplayDirLine	;if Examine found a file, display information about it
		bra.s	dir7b		;then number of free blocks
dir1	;Examine found a directory, ExNext() it
		move.l	D7,D1
		move.l	a5,D2
		lib	ExNext
		tst.l	D0
		bne.s	dir2
		lib	IoErr
		cmp.l	#ERROR_NO_MORE_ENTRIES,D0
		bne.s	dir7a		;branch if a real dos error
		bra.s	dir7b		;no more entries, the display free blocks
dir2		bsr.s	DisplayDirLine
		bne.s	dir7b		;branch if user pressed break (control-c)
		bra.s	dir1
dir7		lib	IoErr
dir7a		tst.l	D0
		beq.s	dir7c
		bsr	DOSErr
		bra.s	dir7c
dir7b	;directory ready, now display number of free blocks
		move.l	D7,D1
		move.l	a5,D2
		lib	Info		;get info about the disk
		tst.l	D0
		beq.s	dir7
		move.l	id_NumBlocks(a5),D0
		sub.l	id_NumBlocksUsed(a5),D0
		lea	freeblkfmt(pc),a0
		call	printf
dir7c		move.l	D7,D1
		lib	UnLock
dir8		move.l	a5,A1
		move.l	#fib_SIZEOF,D0
		jlib	Exec,FreeMem		;free FileInfoBlock

*** PRINT ONE LINE OF DIREECTORY ***
DisplayDirLine	;fib-pointer in a5
		startline
		lea	fib_FileName(a5),A1
		moveq	#0,D1
txmovloop	;copy file name to output buffer
		move.b	(A1)+,D0
		beq.s	txmov2
		move.b	D0,(A3)+
		addq.w	#1,D1
		cmp.w	#30,D1		;max 30 chars
		bcs.s	txmovloop
txfloop		putchr	SPACE
		addq.w	#1,D1
txmov2		cmp.w	#24,D1
		bcs.s	txfloop
		putchr	SPACE
		tst.l	fib_DirEntryType(a5)	;positive=dir, negative=file
		bmi.s	txputlen
		lea	dnam(pc),A1		;'(dir)'
		call	putstring
		bra.s	txmov9

txputlen	movem.l	a2/a6,-(sp)
		lea	numfmt(pc),a0
		lea	fib_Size(a5),a1
		lea	putch(pc),a2
		lib	Exec,RawDoFmt
		movem.l	(sp)+,a2/a6
		bra.s	txmov10
txmov9		endline
txmov10		call	printstring
		call	JUMP,CheckKeys

*** PRINT DOS ERROR NUMBER ***
DOSErr		;error number in D0
		cmp.l	#ERROR_OBJECT_NOT_FOUND,d0
		beq.s	01$
		lea	doserrfmt(pc),a0
		call	JUMP,printf_window
01$		lea	notfound_txt(pc),a0
		call	JUMP,printstring_a0_window

**** LOAD SEGMENT ****
		cmd	loadseg

		tst.l	SegList(a4)
		bne.s	oldseg		;can't do this before old segment is unloaded

		call	GetName
		move.l	d0,d1
		beq	generic_error

loadseg1	lib	Dos,LoadSeg
		move.l	D0,SegList(a4)
		beq.s	segerr		;branch if LoadSeg failed

		move.l	d0,d1
		lsl.l	#2,d0		;BPTR->APTR
		addq.l	#4,d0		;skip next segment pointer
		move.l	d0,RegPC(a4)	;PC points to first instruction
		move.l	d0,Addr(a4)
		move.l	d0,-(sp)

		moveq	#0,d0
01$		lsl.l	#2,d1
		beq.s	02$
		move.l	d1,a0
		move.l	(a0),d1
		addq.l	#1,d0
		bra.s	01$

02$		lea	s_txt(pc),a0
		moveq	#1,d2
		cmp.l	d0,d2
		bne.s	03$
		lea	null_txt(pc),a0
03$		move.l	a0,d1
		move.l	(sp)+,d2
		lea	segadrmes(pc),a0
		call	printf
		bra.s	mjump

oldseg		;message 'unload old segment first'
		lea	ulserr(pc),A0
		call	printstring_a0_window
		bra.s	mjump

segerr		;come here if LoadSeg failed
		lib	IoErr
		bsr.s	DOSErr
mjump		rts

nosegerr	;error 'no segment loaded'
		lea	nosegmes(pc),A0
		call	printstring_a0_window
		bra.s	mjump

**** UNLOAD SEGMENT ****
		cmd	unloadseg

		move.l	SegList(a4),D1
		beq.s	nosegerr	;branch if no segment
		lib	Dos,UnLoadSeg
		clr.l	SegList(a4)	;remember to clear the seglist pointer
		bra.s	mjump

**** SEGMENT LIST ****
		cmd	showseglist

		move.l	SegList(a4),D4
		beq.s	nosegerr	;branch if no seglist
		lea	seghead(pc),A0
		call	printstring_a0
		moveq	#0,d5
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
		lea	seglistfmt(pc),a0
		call	printf
		addq.l	#1,d5
		move.l	(A2),D4		;get next segment pointer
		call	CheckKeys
		bne.s	mjump2
		tst.l	D4
		bne.s	segloop
mjump2		rts

*** SAVE ABSOLUTE (using DOS Write)****
		cmd	abs_save

		call	get_expr
		move.l	D0,a5
		call	get_expr
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

		call	get_expr
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
		bsr	showrange
abs_load_1	move.l	D7,D1
		jlib	Close	;close the file

*** REDIRECT OUTPUT ***
		cmd	redirect

		getbase	Dos
		move.l	OutputFile(a4),D1	;is output currently redirected
		cmp.l	WinFile(a4),D1
		beq.s	redir1
		lib	Close			;if so, then close redirection file
		move.l	WinFile(a4),OutputFile(a4)	;standard output
redir1		call	skipspaces
		tst.b	(A3)
		beq.s	redir9

		call	GetName
		move.l	d0,d1
		beq	generic_error

		move.l	#MODE_NEWFILE,D2
		lib	Open	;open redirection file
		tst.l	D0
		beq	dos_err
		move.l	D0,OutputFile(a4)

redir9		rts

*** NEW SHELL/CLI ***
;
; Now tries first to start a shell...TR 1990-05-24
;
		cmd	new_cli

		lea	NewShellCom(pc),A0
		move.l	A0,D1
		moveq	#0,D2
		move.l	WinFile(a4),D3
		lib	Dos,Execute
		tst.l	d0
		beq.s	01$
		lib	IoErr
		tst.l	d0
		beq.s	redir9
01$		lea	NewCLICom(pc),a0
		move.l	a0,d1	
		moveq	#0,d2
		move.l	WinFile(a4),d3
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
		bclr	#MONB_FIRSTCD,flags(a4)
		bne.s	cd_3
		move.l	D0,D1
		lib	UnLock	;unlock previous current directory
cd_3		rts

ulserr		dc.b	'Unload old segment first',LF,0
segadrmes	dc.b	'%ld hunk%s at '
hexfmt		dc.b	'$%08lx',LF,0
s_txt		dc.b	's, first hunk'
null_txt	dc.b	0
doserrfmt	dc.b	'DOS error '
numfmt		dc.b	'%ld',LF,0
notfound_txt	dc.b	'File not found',LF,0

NewShellCom	dc.b	'NewShell',0
NewCLICom	dc.b	'NewCLI',0

nosegmes	dc.b	'No segment loaded',LF,0

dnam		dc.b	'(dir)',0
freeblkfmt	dc.b	'%ld Blocks free.',LF,0

		end
