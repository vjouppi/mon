;
; mon_main.asm
;

		include	"monitor.i"

		xdef	error
		xdef	errcom
		xdef	mainloop

		xdef	help

		xref	getdecnum
		xref	ChrOut
		xref	ChrOutWin
		xref	FreeAllMem
		xref	remove_all_breaks
		xref	clear_all_variables
		xref	loadseg1

		xref	trapreturn	;execute.asm
		xref	returncode	;execute.asm

		xdef	monitor_code_start

ONE_POINT_TWO	equ	33

;
; although the monitor can be started from workbench, that is not
; recommended, because if you run a program from the monitor it runs
; as a part of the monitor process and many program require CLI
; process environment (or if they are not started from a CLI they wait
; for workbench startup message which in that case doesn't arrive...).
;
; BCPL programs or other programs that use the internal BCPL library
; cannot be started from the monitor without a special calling routine
; that allocates BCPL stack and sets up the BCPL register environment...
; (not recommended...and using the BCPL library in your own programs is
; absolutely not recommended!)
;

;;;		section	MonitorCode,CODE

*** THIS IS THE WORKBENCH/CLI STARTUP CODE ***
monitor_code_start

		move.l	a0,a3			get ptr to command line
		move.l	d0,d3

		moveq	#0,d5
		move.l	d5,a1
		lib	Exec,FindTask		;find our task
		move.l	D0,a5
		tst.l	pr_CLI(a5)		;started from CLI ?
		bne.s	main			;branch if yes
		lea	pr_MsgPort(a5),A0	;started from workbench
		lib	WaitPort		;wait for WB startup message
		lea	pr_MsgPort(a5),A0
		lib	GetMsg			;get it
		move.l	D0,d5

main		move.l	#MonitorData_SIZE,d0
		move.l	#MEMF_PUBLIC!MEMF_CLEAR,d1
		lib	AllocMem
		tst.l	d0
		beq	exit10
		move.l	d0,a4

		move.l	4(sp),StackSize(a4)
		move.l	d5,WBenchMsg(a4)
		move.l	a5,MyTask(a4)
		move.l	sp,StackPtr(a4)

		moveq	#-1,d0
		move.l	d0,OldCD(a4)
		bset	#MONB_FIRSTCD,flags(a4)
*
* OPEN DOS LIBRARY
*
		lea	dosname(pc),A1
		moveq	#ONE_POINT_TWO,d0
		lib	OpenLibrary
		move.l	d0,_DosBase(a4)
		beq	cleanexit

		moveq	#-1,d4			window width
		moveq	#-1,d5			window height

		move.l	WBenchMsg(a4),d0
		beq.s	handle_cli_start
;
; workbench startup....set current directory
;
		move.l	d0,a0
		move.l	sm_ArgList(a0),a0
		move.l	wa_Lock(a0),d1
		lib	Dos,CurrentDir
		move.l	d0,OldCD(a4)
		bra.s	cmdline_done

handle_cli_start
		move.l	pr_CurrentDir(a5),OldCD(a4)

		clr.b	-1(a3,d3.l)		null-terminate command line
parse_cmdline	call	skipspaces
		tst.b	(a3)
		beq.s	cmdline_done
		cmp.b	#'?',(a3)
		beq.s	put_usage
		cmp.b	#'-',(a3)
		bne.s	no_opt
		addq.l	#1,a3
		move.b	(a3)+,d0
		cmp.b	#SPACE,d0
		bls.s	no_opt
		cmp.b	#'w',d0
		bne.s	no_w
		bsr	getdecnum
		bcs.s	put_usage
		move.l	d0,d4
		bra.s	parse_cmdline
no_w		cmp.b	#'h',d0
		bne.s	no_h
		bsr	getdecnum
		bcs.s	put_usage
		move.l	d0,d5
		bra.s	parse_cmdline
no_h
put_usage	lea	usage_txt(pc),a0
		call	puts_stdout
		bra	cleanexit

no_opt		call	GetName
		move.l	d0,InitialFileName(a4)

cmdline_done
;
; set default number base, currently hex....
;
		move.b	#16,defbase(a4)

		moveq	#0,d0
		moveq	#0,d1
		move.w	d4,d1
		bmi.s	win_1
		moveq	#0,d2
		move.w	d5,d2
		bpl.s	open_win

win_1		lib	Dos,Input
		move.l	d0,WinFile(a4)
		beq.s	open_win1
		move.l	d0,d1
		lib	IsInteractive
		tst.l	d0
		beq.s	open_win1
		move.l	WinFile(a4),d0
		bra.s	win_com

nogfx_win	moveq	#0,d0
		move.l	#200,d2
		bra.s	open_win2

open_win1	lea	gfxname(pc),a1
		moveq	#ONE_POINT_TWO,d0
		lib	Exec,OpenLibrary
		tst.l	d0
		beq.s	nogfx_win
		move.l	d0,a1
		moveq	#0,d2
		move.w	gb_NormalDisplayRows(a1),d2
		lib	CloseLibrary

		moveq	#0,d0
		cmp.w	#200,d2
		bcs.s	open_win2

		moveq	#8,d0
		sub.w	#18,d2	

open_win2	move.w	#640,d1

open_win	lea	windowfmt(pc),a0
		call	fmtstring

		bset	#MONB_OWNWINDOW,flags(a4)
		lea	OutputBuf(a4),a0
		move.l	a0,d1
		move.l	#MODE_OLDFILE,D2
		lib	Dos,Open		open the window

win_com		move.l	d0,WinFile(a4)
		beq	cleanexit
		move.l	D0,OutputFile(a4) 	;default output is monitor window

;
; find pointer to intuition window structure of the monitor output window
; also find pointer to the console device unit structure
;
		lsl.l	#2,d0			filehandle BPTR->APTR
		move.l	d0,a0
		move.l	fh_Type(a0),a0
		moveq	#ACTION_DISK_INFO,d0
		lea	OutputBuf(a4),a1	we use output buffer for InfoData
		move.l	a1,d1
		lsr.l	#2,d1			infodataptr APTR->BPTR
		call	sendpacket
		tst.l	d0
		beq	01$

		move.l	OutputBuf+id_InUse(a4),d0	console IORequest ptr
		beq.s	01$
		move.l	d0,a0
		move.l	IO_UNIT(a0),ConsoleUnit(a4)
		;; set stdout to raw mode

01$		move.l	WinFile(a4),d0
		moveq	#-1,d1
		call	SetConMode
		tst.l	d0
		beq	cleanexit

		;;;;;;;
;
; set default disk device name ("trackdisk.device")
;
		lea	tdname(pc),a0
		lea	DevNameBuf(a4),a1
02$		move.b	(a0)+,(a1)+
		bne.s	02$

;
; set the the task trap code and data pointers
;
		lea	trapreturn(pc),A1
		move.l	MyTask(a4),A0
		move.l	TC_TRAPCODE(A0),OldTrapCode(a4)	;save old TrapCode
		move.l	TC_Userdata(a0),OldUserData(a4)	; & UserData
		move.l	A1,TC_TRAPCODE(A0)		;and set a new one
		move.l	a4,TC_Userdata(a0)
; save pr_ReturnAddr pointer
		move.l	pr_ReturnAddr(a0),OldRetAddr(a4)
		bset	#MONB_TASKSET,flags(a4)

;
; set the stack pointer
;
		move.l	sp,a0
		sub.w	#MONSTACK,a0	;a safe area from this task's stack
		move.l	a0,d0
		and.b	#$fc,d0			;long word align
		move.l	d0,RegSP(a4)		;set stack pointer
		move.l	d0,StackHigh(a4)
		lea	returncode(pc),a1
		move.l	d0,a0
		move.l	a1,(a0)+
		move.l	StackSize(a4),a1
		sub.w	#MONSTACK,a1
		move.l	a1,(a0)
; set pr_ReturnAddr
		move.l	MyTask(a4),a1
		move.l	a0,pr_ReturnAddr(a1)
;;
;; set the PC-register to an odd value, so jump/go/trace without
;; setting pc is error...
;;
;;		moveq	#-1,d0
;;		move.l d0,RegPC(a4)

		lea	cls_str(pc),a0
		call	printstring_a0_window
		lea	welcometxt(pc),A0
		call	printstring_a0_window ;display welcome message

;
; try to load the file specified on the command line
;
		move.l	InitialFileName(a4),d1
		beq.s	mainloop
		bsr	loadseg1

*** JUMP HERE AFTER EXECUTION OF A COMMAND ***
mainloop	move.l	StackPtr(a4),sp		;restore stack pointer
		bclr	#MONB_QTRACE,flags(a4)

		moveq	#0,D0		;clear CTRL-C/D/E/F flags
		move.l	#SIGBREAKF_CTRL_C!SIGBREAKF_CTRL_D!SIGBREAKF_CTRL_E!SIGBREAKF_CTRL_F,D1
		lib	Exec,SetSignal

		lea	main_prompt(pc),A0
		call	printstring_a0_window	;display prompt

		moveq	#0,D0
		call	GetInput

		tst.b	(A3)
		bne.s	execute_command

		move.l	WinFile(a4),D0		;empty command line
		cmp.l	OutputFile(a4),D0
		beq.s	mainloop

		emit	LF
		bra.s	mainloop

execute_command	call	skipspaces
		lea	command_names(pc),a0
		moveq	#-1,d2
01$		addq.w	#1,d2
		move.l	a3,a1
02$		move.b	(a0)+,d1
		beq.s	cmd_found
		move.b	(a1)+,d0
		call	tolower
		cmp.b	d0,d1
		beq.s	02$
03$		tst.b	(a0)+
		bne.s	03$
		tst.b	(a0)
		bne.s	01$
		bra	error	; command not found

cmd_found	move.l	a1,a3
		call	skipspaces
		lea	command_table(pc),a0
		lsl.w	#2,d2
		add.w	d2,a0
		jsr	(a0)
		bra	mainloop

error		lea	errtx(pc),A0	;command not found, print error message
errcom		call	printstring_a0_window
		emitwin	LF
		bra	mainloop

;
;*** THE CLS COMMAND ***
;
		cmd	clear_screen

		lea	cls_str(pc),a0
		bra.s	errcom

;
;*** THE HELP COMMAND ***
;
		cmd	help

help		lea	cls_str(pc),a0
		call	printstring_a0
		lea	helptext(pc),A0
hinfo		call	JUMP,printstring_a0


;
;*** INFO ***
;
		cmd	info

		lea	cls_str(pc),a0
		call	printstring_a0
		lea	infotext(pc),A0
		bra.s	hinfo

;
;*** EXIT FROM MONITOR ***
;
		cmd	exit_monitor

cleanexit	move.l	StackPtr(a4),sp

		move.l	WinFile(a4),d0
		beq.s	01$
		moveq	#0,d1
		call	SetConMode

01$		btst	#MONB_TASKSET,flags(a4)
		beq.s	02$
;
; restore task TrapCode/UserData & pr_ReturnAddr fields
;
		move.l	MyTask(a4),A0
		move.l	OldTrapCode(a4),TC_TRAPCODE(A0)	 ;restore old TrapCode
		move.l	OldUserData(a4),TC_Userdata(a0)	 ; and UserData
		move.l	OldRetAddr(a4),pr_ReturnAddr(a0) ; and ReturnAddr

02$		bsr	FreeAllMem		free all memory allocated by commands & and (
		bsr	remove_all_breaks	remove all breakpoints (free memory)
		bsr	clear_all_variables

		getbase	Dos,d0
		beq.s	exit9
		move.l	d0,a6

		move.l	SegList(a4),D1		if a seglist is loaded, unload it
		beq.s	03$
		lib	UnLoadSeg

03$		move.l	OutputFile(a4),D1	if output is redirected, close output file
		cmp.l	WinFile(a4),D1
		beq.s	04$
		lib	Close

04$		btst	#MONB_OWNWINDOW,flags(a4)
		beq.s	05$
		move.l	WinFile(a4),D1
		beq.s	05$
		lib	Close			close window file

05$		moveq	#-1,d0
		move.l	OldCD(a4),d1
		cmp.l	d0,d1
		beq.s	06$
		lib	CurrentDir

06$		move.l	a6,a1
		lib	Exec,CloseLibrary	close dos library

exit9		move.l	WBenchMsg(a4),D3	started from workbench??

		move.l	a4,a1
		move.l	#MonitorData_SIZE,d0
		lib	Exec,FreeMem

exit10		tst.l	d3
		beq.s	exit99
		lib	Forbid		;forbid, so WB can't UnloadSeg() before exit
		move.l	D3,A1
		lib	ReplyMsg	;reply the WB startup message

exit99		moveq	#0,D0	;error return code
		rts		;return control to the system...


**** Command & address tables ****

command		macro
		ifnd	\1_cmd
		xref	\1_cmd
		endc
		bra	\1_cmd
		endm

command_names	dc.b	'?',0		;calculator
		dc.b	'@',0		;command line
		dc.b	'^',0		;showtrap/baseptr
		dc.b	'\',0		;newshell
		dc.b	'#',0		;bootblock checksum
		dc.b	'=',0		;disk block checksum
		dc.b	'!',0		;play digisound
		dc.b	'>',0		;write disk blocks
		dc.b	'<',0		;read disk blocks
		dc.b	']',0		;absolute disk write
		dc.b	'[',0		;absolute file read
		dc.b	'&',0		;allocate memory at absolute addr.
		dc.b	')',0		;free memory
		dc.b	'(',0		;allocate memory
		dc.b	'dir',0		;display directory
		dc.b	'del',0		;delete file
		dc.b	'dev',0		;set/show disk device
		dc.b	'd',0		;disassemble
		dc.b	'u',0		;unload segment
		dc.b	'l',0		;load segment (executable file)
		dc.b	'sl',0		;show seglist
		dc.b	'sm',0		;show allocated memory
		dc.b	'set',0		;set variable
		dc.b	'z',0		;skip instruction
		dc.b	'e',0		;execute one instruction
		dc.b	'w',0		;trace (walk)
		dc.b	'q',0		;quicktrace
		dc.b	'j',0		;jump to subroutine
		dc.b	'g',0		;go (execute code)
		dc.b	'ba',0		;set/show base
		dc.b	'br',0		;remove breakpoints
		dc.b	'bl',0		;list breakpoints
		dc.b	'b',0		;set breakpoint
		dc.b	'r',0		;set/show registers
		dc.b	'a',0		;assemble
		dc.b	':',0		;modify memory
		dc.b	'cls',0		;clear screen
		dc.b	'cd',0		;change current directory
		dc.b	'cv',0		;clear variables
		dc.b	'c',0		;compare memory
		dc.b	'help',0	;display help text
		dc.b	'h',0		;hunt memory
		dc.b	't',0		;transfer memory
		dc.b	'f',0		;fill memory
		dc.b	'mi',0		;memory info
		dc.b	'm',0		;display memory
		dc.b	'x',0		;exit monitor
		dc.b	'opt',0		;set/reset options
		dc.b	'o',0		;redirect
		dc.b	'i',0		;display info text
		dc.b	0		;end of command table

command_table	command	calculator
		command	cmdline
		command	showtrap
		command	new_cli
		command	boot_check
		command	block_check
		command	digisound
		command	disk_write
		command	disk_read
		command	abs_save
		command	abs_load
		command	alloc_abs
		command	free_mem
		command	allocate_mem
		command	directory
		command	deletefile
		command	setshow_device
		command	disassem
		command	unloadseg
		command	loadseg
		command	showseglist
		command	showmemory
		command	setvariable
		command	skip_one
		command	exe_one
		command	walk
		command	quicktrace
		command	jumpsr
		command	go
		command	setshow_base
		command	remove_break
		command	list_breaks
		command	set_break
		command	setshow_regs
		command	assemble
		command modifymem
		command	clear_screen
		command	current_dir
		command	clearvars
		command	memcomp
		command	help
		command	memhunt
		command	memtransfer
		command	memfill
		command	memory_info
		command	memdisplay
		command	exit_monitor
		command	options
		command	redirect
		command	info


**** HELP TEXT ****
helptext	dc.b	TAB,TAB,'-- Amiga Monitor v'
		VERSION
		dc.b	' help (i=info, x=exit) --',LF
		dc.b	'o [name]',TAB,':redirect output'
		dc.b	'| \',TAB,TAB,TAB,': NewShell',LF
		dc.b	'dir [name]',TAB,': directory',TAB
		dc.b	'| [ addr name',TAB,TAB,': load absolute',LF
		dc.b	'cd [name]',TAB,': current dir',TAB
		dc.b	'| ] addr length name',TAB,': save absolute',LF
		dc.b	'l name',TAB,TAB,': load exefile',TAB
		dc.b	'| < addr dr block cnt',TAB,': read disk blocks',LF
		dc.b	'sl',TAB,TAB,': hunk list',TAB
		dc.b	'| > addr dr block cnt',TAB,': write disk blocks',LF
		dc.b	'u',TAB,TAB,': unload exefile'
		dc.b	'| dev [devicename]',TAB,': set disk device',LF
		dc.b	'r [reg=num]',TAB,': set/show regs',TAB
		dc.b	'| = addr [len offs]',TAB,': disk block checksum',LF
		dc.b	'a addr',TAB,TAB,': assemble',TAB
		dc.b	'| # addr [len offs]',TAB,': bootblock checksum',LF
		dc.b	'd addr1 addr2',TAB,': disassemble',TAB
		dc.b	'| ! addr len per [cnt]',TAB,': play digisound',LF
		dc.b	'm addr1 addr2',TAB,': display memory'
		dc.b	'| g [addr]',TAB,TAB,': execute (go)',LF
		dc.b	': addr bytes',TAB,': modify memory',TAB
		dc.b	'| j [addr]',TAB,TAB,': jump to subroutine',LF
		dc.b	'b addr',TAB,TAB,': set breakpoint'
		dc.b	'| w [addr]',TAB,TAB,': single step (walk)',LF
		dc.b	'bl',TAB,TAB,': list brkpoints'
		dc.b	'| e [addr]',TAB,TAB,': execute one instr.',LF
		dc.b	'br addr/all',TAB,':remove brkpoint'
		dc.b	'| q [addr]',TAB,TAB,': quicktrace',LF
		dc.b	'f addr1 addr2 bytes: fill mem',TAB
		dc.b	'| ( length',TAB,TAB,': allocate memory',LF
		dc.b	'@ [line]',TAB,': enter cmd line'
		dc.b	'| & addr length',TAB,TAB,': allocate absolute',LF
		dc.b	'ba [decnum]',TAB,': set/show base',TAB
		dc.b	'| ) addr/all',TAB,TAB,': free memory',LF
		dc.b	'? [expr]',TAB,': calculator',TAB
		dc.b	'| sm',TAB,TAB,TAB,': show allocated mem',LF
		dc.b	'mi addr',TAB,TAB,': memory info',TAB
		dc.b	'| c addr1 addr2 dest',TAB,': compare memory',LF
		dc.b	'cv',TAB,TAB,': clear vars',TAB
		dc.b	'| t addr1 addr2 dest',TAB,': transfer memory',LF
		dc.b	'h addr1 addr2 bytes :hunt memory'
		dc.b	'| set [var=expr]',TAB,': set/show variables',LF,0

**** INFO TEXT ****
infotext	dc.b LF
		dc.b	TAB,TAB,'Monitor info (version '
		VERSION
		dc.b	')',LF
		dc.b	TAB,TAB,'---------------------------',LF
		dc.b	TAB,' Copyright 1987-1991 by Timo Rossi',LF,LF
		dc.b	'   This is a machine code monitor/debugger for the Amiga.',LF
		dc.b	' Pressing the HELP-key displays a list of commands.',LF,LF
		dc.b	' Note1: Some of the assembler instructions require the',LF
		dc.b	' size specifier (.B, .W or .L), but it can''t be used by some others.',LF,LF
		dc.b	' Note2: the default number base is again hex, use ''_'' as prefix',LF
		dc.b	' for decimal or change the base with the ba-command.',LF
		dc.b	' You can now use expressions in most places where',LF
		dc.b	' numbers are needed.',LF,LF
		dc.b	' I hope you find this program useful, but if you find any bugs in this',LF
		dc.b	' program, please let me know.',LF,LF
		dc.b	' Read the ''mon.doc''-file for more information. (My address is also there)',LF,0

dosname		dc.b	'dos.library',0
gfxname		dc.b	'graphics.library',0
tdname		dc.b	'trackdisk.device',0

windowfmt	dc.b	'RAW:0/%ld/%ld/%ld/Amiga Monitor v'
		VERSION
		dc.b	0

welcometxt	dc.b	LF,TAB,TAB,TAB,' --- Amiga Monitor ---',LF,LF
		dc.b	TAB,'   Copyright 1987-1991 by Timo Rossi, version '
		VERSION
		ifne	BETA
		dc.b	LF,LF,TAB
		dc.b	'THIS IS A BETA VERSION, DO NOT DISTRIBUTE!'
		endc
		dc.b	LF,LF,0

main_prompt	dc.b	'-> ',0

usage_txt	dc.b	'Usage: mon [-w<winwidth>] [-h<winheight>] [filename]',LF,0
win_openerr_txt	dc.b	'Can''t open window',LF,0
errtx		dc.b	'???',0

cls_str		dc.b	ESC,'[H',ESC,'[J',0

		end