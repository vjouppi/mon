;
; mon_main.asm -- monitor main program
;
; must be first module to be linked in
;
		nolist
		include	"exec/types.i"
		include "exec/memory.i"
		include "exec/tasks.i"
		include "libraries/dosextens.i"
		include "workbench/startup.i"
		include	"intuition/intuition.i"
		include	"devices/conunit.i"
		include "offsets.i"
		list

		include	"monitor.i"
		include	"script.i"
		include "mon_version.i"

;
; This module defines the following command routines:
;
;	clear_screen,help,info,exit_monitor
;

		xdef	mainloop
		xdef	error_com
		xdef	exec_cmd

		xdef	help

		xref	trapreturn	;execute.asm
		xref	returncode	;execute.asm

		xdef	generic_error
		xdef	odd_address_error
		xdef	out_range_error
		xdef	out_memory_error
		xdef	expression_error
		xdef	addrmode_error
		xdef	scriptonly_error
		xdef	undeflabel_error
		xdef	brk_already_set_error
		xdef	brk_not_set_error
		xdef	out_range_txt
		xdef	break_txt

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
		lib	AbsExec,FindTask		;find our task
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
		move.l	a6,_ExecBase(a4)
		move.l	4(sp),mon_StackSize(a4)
		move.l	d5,mon_WBenchMsg(a4)
		move.l	a5,mon_Task(a4)
		move.l	sp,mon_StackPtr(a4)

		or.b	#MONF_STARTUP!MONF_FIRSTCD,mon_Flags(a4)
		moveq	#-1,d0
		move.l	d0,mon_OrigCD(a4)

		lea	expansion_name(pc),a1
		moveq	#ONE_POINT_TWO,d0
		lib	OpenLibrary
		move.l	d0,_ExpansionBase(a4)
		beq	exit9a

*
* OPEN DOS LIBRARY
*
		lea	dos_name(pc),A1
		moveq	#ONE_POINT_TWO,d0
		lib	OpenLibrary
		move.l	d0,_DosBase(a4)
		beq	exit9

		moveq	#-1,d4			window width
		moveq	#-1,d5			window height

		move.l	mon_WBenchMsg(a4),d0
		beq.s	handle_cli_start
;
; workbench startup....set current directory
;
		move.l	d0,a0
		move.l	sm_ArgList(a0),a0
		move.l	wa_Lock(a0),d1
		lib	Dos,CurrentDir
		move.l	d0,mon_OrigCD(a4)
		bra	cmdline_done

handle_cli_start
		move.l	pr_CurrentDir(a5),mon_OrigCD(a4)

		moveq	#0,d6
		clr.b	-1(a3,d3.l)		;null-terminate command line
parse_cmdline	call	skipspaces
		tst.b	(a3)
		beq	cmdline_done
		cmp.b	#'?',(a3)
		beq.s	put_usage0
		cmp.b	#'-',(a3)
		bne.s	no_opt
		addq.l	#1,a3
		move.b	(a3)+,d0
		cmp.b	#SPACE,d0
		bls.s	no_opt

		cmp.b	#'w',d0
		bne.s	no_w
		call	GetDecNum
		bcs.s	put_usage
		move.l	d0,d4
		bra	parse_cmdline

no_w		cmp.b	#'h',d0
		bne.s	no_h
		call	GetDecNum
		bcs.s	put_usage
		move.l	d0,d5
		bra	parse_cmdline

no_h		cmp.b	#'o',d0
		bne.s	no_o
		call	GetHexNum
		bcs.s	put_usage
		move.b	d0,mon_Options(a4)
		bra	parse_cmdline

no_o		cmp.b	#'s',d0
		bne.s	put_usage
		call	GetName
		move.l	d0,mon_InitialScriptName(a4)
		beq	put_usage
		bra	parse_cmdline

put_usage0	lea	version_msg(pc),a0
		call	puts_stdout

put_usage	lea	usage_txt(pc),a0
		call	puts_stdout
		bra	cleanexit

no_opt		cmp.b	#'+',(a3)
		bne.s	get_name
		st	d6
		addq.l	#1,a3
		bra	parse_cmdline

get_name	call	GetName
		move.l	d0,mon_InitialFileName(a4)

		call	skipspaces
		call	set_cmdline	; set a0/d0 for cmdline...
					; (rest of actual monitor command line)
cmdline_done
;
; set default number base, currently hex....
;
		move.b	#16,mon_DefNumBase(a4)

;
; open a new window if a window size has been specified or
; the standard input is not interactive (mon has been started with run)
;

;
; Window dimension/position logic:
;
; Open intuition/GetScreenData() from WB screen/Close intuition
; if something fails, the LeftEdge/TopEdge/Width/Height fields remain zero
;
; If window height/width were specified on the command line, use these
; and position to window to -LeftEdge,-TopEdge
;
; Else if the window height is less than 256, open a window
; with the screen width/height, and if the height is more than
; 256, use $eeee/$10000 * height as window height and position the
; window a few pixels down. if the height is more than 400,
; use $cccc/$10000 * height...
;
; Note that GetScreenData() returns the width/height of the
; visible region of the screen.
;
		moveq	#0,d2
		move.w	d4,d2
		moveq	#0,d3
		move.w	d5,d3
		bpl.b	open_win
		tst.w	d2
		bpl.b	open_win

		lib	Dos,Input
		move.l	d0,mon_WinFile(a4)
		beq.s	open_win
		move.l	d0,d1
		lib	IsInteractive
		tst.l	d0
		beq.b	open_win
		move.l	mon_WinFile(a4),d0
		bra	win_open

open_win	lea	intuition_name(pc),a1
		moveq	#ONE_POINT_TWO,d0
		lib	Exec,OpenLibrary
		tst.l	d0
		beq.b	n_int
		move.l	d0,a6

		lea	mon_InputBuf(a4),a0
		moveq	#sc_Height+2,d0
		moveq	#WBENCHSCREEN,d1
		clra	a1
		lib	GetScreenData
		tst.l	d0
		beq.b	n_sc

		neg.w	mon_InputBuf+sc_LeftEdge(a4)
		bpl.s	1$
		clr.w	mon_InputBuf+sc_LeftEdge(a4)

1$		neg.w	mon_InputBuf+sc_TopEdge(a4)
		bpl.s	2$
		clr.w	mon_InputBuf+sc_TopEdge(a4)

2$
n_sc		move.l	a6,a1
		lib	Exec,CloseLibrary
n_int
		tst.w	d2
		bpl.b	1$
		move.w	mon_InputBuf+sc_Width(a4),d2
		bne.b	1$
		move.w	#640,d2

1$		tst.w	d3
		bpl.b	2$
		move.w	mon_InputBuf+sc_Height(a4),d3
		bne.b	2$
		move.w	#200,d3
2$
		cmp.w	#256,d3
		bcs.b	4$
		move.w	#$eeee,d0
		cmp.w	#400,d3
		bcs.b	3$
		move.w	#$cccc,d0

3$		mulu	d0,d3
		clr.w	d3
		swap	d3
		add.w	#8,mon_InputBuf+sc_TopEdge(a4)

4$		moveq	#0,d0
		move.w	mon_InputBuf+sc_LeftEdge(a4),d0
		moveq	#0,d1
		move.w	mon_InputBuf+sc_TopEdge(a4),d1
		lea	window_fmt(pc),a0
		call	fmtstring

		bset	#MONB_OWNWINDOW,mon_Flags(a4)
		lea	mon_OutputBuf(a4),a0
		move.l	a0,d1
		move.l	#MODE_OLDFILE,D2
		lib	Dos,Open		open the window
		move.l	d0,mon_WinFile(a4)
		bne.s	own_win_open

		lea	win_openerr_txt(pc),a0
		call	puts_stdout
		bra	cleanexit

own_win_open

	ifd	SET_CONSOLE_TASK
		move.l	d0,d1
		lsl.l	#2,d1
		move.l	d1,a1
		move.l	mon_Task(a4),a0
		move.l	pr_ConsoleTask(a0),mon_OrigConTask(a4)
		move.l	fh_Type(a1),pr_ConsoleTask(a0)
	endc

win_open	move.l	D0,mon_OutputFile(a4) 	;default output is monitor window

		call	FindConUnit		;this may return zero
		move.l	d0,mon_ConsoleUnit(a4)

		;; set stdout to raw mode

01$		move.l	mon_WinFile(a4),d0
		moveq	#-1,d1
		call	SetConMode
		tst.l	d0
		beq	cleanexit

		;;;;;;;
;
; set default disk device name ("trackdisk.device")
;
		lea	trackdisk_name(pc),a0
		lea	mon_DevNameBuf(a4),a1
02$		move.b	(a0)+,(a1)+
		bne.s	02$

;
; set the the task trap code and data pointers
;
		lea	trapreturn(pc),A1
		move.l	mon_Task(a4),A0
		move.l	TC_TRAPCODE(A0),mon_OrigTrapCode(a4)	;save old TrapCode
		move.l	TC_TRAPDATA(a0),mon_OrigTrapData(a4)	; & TrapData

set_tc		move.l	A1,TC_TRAPCODE(A0)		;and set a new one
		move.l	a4,TC_TRAPDATA(a0)

; save pr_ReturnAddr pointer
		move.l	pr_ReturnAddr(a0),mon_OrigRetAddr(a4)
		bset	#MONB_TASKSET,mon_Flags(a4)

;
; set the stack pointer
;
		move.l	sp,a0
		sub.w	#MONSTACK,a0	;a safe area from this task's stack
		move.l	a0,d0
		and.b	#$fc,d0			;long word align
		move.l	d0,mon_RegSP(a4)	;set stack pointer
		move.l	d0,mon_StackHigh(a4)
		lea	returncode(pc),a1
		move.l	d0,a0
		move.l	a1,(a0)+
		move.l	mon_StackSize(a4),a1
		sub.w	#MONSTACK,a1
		move.l	a1,(a0)
; set pr_ReturnAddr
		move.l	mon_Task(a4),a1
		move.l	a0,pr_ReturnAddr(a1)
;;
;; set the PC-register to an odd value, so jump/go/trace without
;; setting pc is error...
;;
;;		moveq	#-1,d0
;;		move.l d0,mon_RegPC(a4)

		st	mon_RelBaseReg(a4)	;no base register currently used

		lea	welcome_txt(pc),A0
		call	printstring_a0_window ;display welcome message

;
; try to load the file specified on the command line
;
		move.l	mon_InitialFileName(a4),d1
		beq.s	check_initial_script

		move.l	d6,d5		;'+' flag from cmdline
		call	loadseg1

check_initial_script
		bclr	#MONB_STARTUP,mon_Flags(a4)

		move.l	mon_InitialScriptName(a4),d0
		beq.s	mainloop

		move.l	d0,a0
		call	exec_script

*** JUMP HERE AFTER EXECUTION OF A COMMAND ***
mainloop	move.l	mon_StackPtr(a4),sp	;restore stack pointer
		move.l	mon_ScriptList(a4),d2
		beq.b	do_cmdline

		moveq	#0,d0
		moveq	#0,d1
		lib	Exec,SetSignal
		btst	#SIGBREAKB_CTRL_C,d0
		bne	break_error

do_script	move.l	d2,a0
		call	get_script_line
		bra.s	do_command

do_cmdline	lea	main_prompt(pc),A0
		call	printstring_a0_window	;display prompt

		moveq	#0,D0
		call	GetInput

do_command	lea	mon_InputBuf(a4),a3
		moveq	#0,D0		;clear CTRL-C/D/E/F flags
		move.l	#SIGBREAKF_CTRL_C!SIGBREAKF_CTRL_D!SIGBREAKF_CTRL_E!SIGBREAKF_CTRL_F,D1
		lib	Exec,SetSignal

		btst	#OPTB_CMDECHO,mon_Options(a4)
		beq.s	do_cmd1

		move.l	a3,a0
		call	printstring_a0
		emit	LF

do_cmd1		call	skipspaces
		cmp.b	#'.',(a3)	;label?
		bne.s	exec_cmd

; skip label
1$		addq.l	#1,a3
		move.b	(a3),d0
		beq.s	3$
		cmp.b	#SPACE,d0
		beq.s	2$
		cmp.b	#TAB,d0
		beq.s	2$
		cmp.b	#':',d0
		bne.s	1$

2$		addq.l	#1,a3
3$		call	skipspaces

exec_cmd	cmp.b	#';',(a3)		;comment?
		beq	mainloop
		tst.b	(A3)
		bne	do_cmd2

		move.l	mon_WinFile(a4),D0	;empty command line
		cmp.l	mon_OutputFile(a4),D0
		beq	mainloop

		emit	LF
		bra	mainloop

do_cmd2		lea	command_names(pc),a0
		call	find_name

		tst.l	d0
		bmi.s	unknown_command

		move.l	a1,a3
		call	skipspaces
		lea	command_table(pc),a0
		lsl.w	#2,d0
		add.w	d0,a0
		jsr	(a0)
		bra	mainloop

unknown_command	move.l	a3,d0
		lea	unknown_cmd_fmt(pc),a0
		call	printf_window
		bra.s	error_com1

;#
;# error handling routines
;#
error_routines
generic_error		bsr.s	errorhandler
odd_address_error	bsr.s	errorhandler
out_range_error		bsr.s	errorhandler
out_memory_error	bsr.s	errorhandler
expression_error	bsr.s	errorhandler
addrmode_error		bsr.s	errorhandler
scriptonly_error	bsr.s	errorhandler
undeflabel_error	bsr.s	errorhandler
brk_already_set_error	bsr.s	errorhandler
brk_not_set_error	bsr.s	errorhandler
break_error		bsr.s	errorhandler
		nop
errorhandler	lea	error_routines+2(pc),a0
		move.l	(sp)+,d0
		sub.l	a0,d0
		lsr.l	#1,d0
		lea	error_messages(pc),a0
		call	getnth
		call	printstring_a0_window
error_com1	emitwin	LF
error_com	move.l	mon_ScriptList(a4),d0
		beq.s	1$
		move.l	d0,a0
		lea	sh_Name(a0),a1
		move.l	a1,d0
		move.l	sh_LineNum(a0),d1
		lea	script_err_fmt(pc),a0
		call	printf_window
1$		call	free_all_scripts
		bra	mainloop

;
;*** THE CLS COMMAND ***
;
		cmd	clear_screen

cls		lea	cls_str(pc),a0
		btst	#OPTB_DUMBTERM,mon_Options(a4)
		beq.s	do_cls
		addq.l	#cls2_str-cls_str,a0
do_cls		call	JUMP,printstring_a0_window

;
;*** THE HELP COMMAND ***
;
		cmd	help

help		lea	help_text(pc),A2

pager_display	moveq	#0,d7
		move.l	mon_ConsoleUnit(a4),d0
		beq.b	1$
		move.l	d0,a0
		move.w	cu_YMax(a0),d7
		subq.w	#2,d7
		bcc.b	1$
		moveq	#0,d7
1$		move.w	d7,d6

pager_loop	lea	mclr_txt(pc),a0
		call	printstring_a0

		move.l	a2,d2
2$		tst.b	(a2)
		beq.b	pager_end
		cmp.b	#LF,(a2)+
		bne.b	2$
		move.l	mon_OutputFile(a4),d1
		move.l	a2,d3
		sub.l	d2,d3
		lib	Dos,Write
		tst.b	(a2)
		beq.b	pager_end
		call	CheckKeys
		bne	pager_end
		tst.w	d7
		beq.b	pager_loop
		subq.w	#1,d6
		bne.b	pager_loop
		lea	more_prompt(pc),a0
		call	printstring_a0
		move.w	d7,d6
		call	GetKey
		cmp.w	#CtrlC,d0
		bne.b	pager_loop

pager_end	rts

;
;show version & date
;
		cmd	version

		lea	version_msg(pc),a0
		call	JUMP,printstring_a0

;
;quickhelp
;
		cmd	list_commands

		lea	command_names(pc),a2
list_cmd_loop	startline
		lea	60(a3),a5
1$		move.b	(a2)+,(a3)+
		bne.s	1$
		move.b	#SPACE,-1(a3)
		tst.b	(a2)
		beq.s	2$
		cmp.l	a5,a3
		bcs.s	1$
2$		endline
		call	printstring
		call	CheckKeys
		bne.s	9$
		tst.b	(a2)
		bne.s	list_cmd_loop
9$		rts

;
;*** EXIT FROM MONITOR ***
;
		cmd	exit_monitor

cleanexit	move.l	mon_StackPtr(a4),sp

;
; remove file handle buffer if it points to mon_CmdLineBuf
;
		lib	Dos,Input
		tst.l	d0
		beq.b	00$
		lsl.l	#2,d0
		move.l	d0,a0
		move.l	fh_Buf(a0),d0
		lsl.l	#2,d0
		lea	mon_CmdLineBuf(a4),a1
		cmp.l	a1,d0
		bne.b	00$
		move.l	mon_OldFhBuf(a4),fh_Buf(a0)
		move.l	mon_OldPos(a4),fh_Pos(a0)
		move.l	mon_OldEnd(a4),fh_End(a0)
;
; and remove the file handle if it is the monitor fake file handle
;
		lea	mon_FakeFH(a4),a1
		cmp.l	a0,a1
		bne.b	00$

		move.l	mon_Task(a4),a0
		clr.l	pr_CIS(a0)

00$		move.l	mon_WinFile(a4),d0
		beq.s	01$
		moveq	#0,d1
		call	SetConMode

01$		btst	#MONB_TASKSET,mon_Flags(a4)
		beq.s	02$
;
; restore task TrapCode/TrapData & pr_ReturnAddr fields
;
		move.l	mon_Task(a4),A0
		move.l	mon_OrigTrapCode(a4),TC_TRAPCODE(A0)	;restore old TrapCode
		move.l	mon_OrigTrapData(a4),TC_TRAPDATA(a0)	;and TrapData
		move.l	mon_OrigRetAddr(a4),pr_ReturnAddr(a0)	;and ReturnAddr

02$		call	free_all_mem		free all memory allocated by commands & and (
		call	unload_seg		unload exefile, if necessary
		call	remove_all_breaks	remove all breakpoints (free memory)
		call	clear_all_variables
		call	free_all_scripts

		getbase Dos
		move.l	mon_OutputFile(a4),D1	if output is redirected, close output file
		cmp.l	mon_WinFile(a4),D1
		beq.s	04$
		lib	Close

04$		btst	#MONB_OWNWINDOW,mon_Flags(a4)
		beq.s	05$

	ifd	SET_CONSOLE_TASK
		move.l	mon_Task(a4),a0
		move.l	mon_OrigConTask(a4),pr_ConsoleTask(a4)
	endc

		move.l	mon_WinFile(a4),D1
		beq.s	05$
		lib	Close			close window file

05$		moveq	#-1,d0
		move.l	mon_OrigCD(a4),d1
		cmp.l	d0,d1
		beq.s	06$
		lib	CurrentDir
		btst	#MONB_FIRSTCD,mon_Flags(a4)
		bne.s	06$
		move.l	d0,d1
		lib	UnLock

06$		move.l	a6,a1
		lib	Exec,CloseLibrary	close dos library

exit9		move.l	_ExpansionBase(a4),a1
		lib	CloseLibrary

exit9a		move.l	mon_WBenchMsg(a4),D3	started from workbench??

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

command_names
		dc.b	'??',0		;list commands
		dc.b	'?',0		;calculator
		dc.b	'exec',0	;execute script
		dc.b	'quit',0	;return from script
		dc.b	'goto',0	;script goto command
		dc.b	'if',0		;script if command
		dc.b	'pokel',0	;put longword in memory
		dc.b	'pokew',0	;put word in memory
		dc.b	'poke',0	;put byte in memory
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
		dc.b	'echo',0	;display text
		dc.b	'u',0		;unload segment
		dc.b	'l',0		;load segment (executable file)
		dc.b	'sl',0		;show seglist
		dc.b	'sm',0		;show allocated memory
		dc.b	'set',0		;set variable
		dc.b	'sr',0		;stack reset
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
		dc.b	'rb',0		;addr. reg relative base addr. set/show
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
		dc.b	'mf',0		;formatted memory display
		dc.b	'mi',0		;memory info
		dc.b	'm',0		;display memory
		dc.b	'x',0		;exit monitor
		dc.b	'opt',0		;set/reset options
		dc.b	'o',0		;redirect
		dc.b	'i',0		;ASCII-only memory dump
		dc.b	'ver',0		;show version & date
		dc.b	'vt',0		;list tasks
		dc.b	'vl',0		;list libraries
		dc.b	'vd',0		;list devices
		dc.b	'vr',0		;list resources
		dc.b	'vp',0		;list ports
		dc.b	'vs',0		;list semaphores
		dc.b	'vi',0		;list interrupts
		dc.b	've',0		;list expansion devices

		dc.b	0		;end of command table

		ds.w	0

command_table	command	list_commands
		command	calculator
		command	exec_script
		command	quit_script
		command	goto
		command	if
		command	pokel
		command pokew
		command poke
		command	cmdline
		command	showtrap
		command	shell
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
		command	echo
		command	unloadseg
		command	loadseg
		command	showseglist
		command	showmemory
		command	setvariable
		command	stackreset
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
		command	setshow_relbase
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
		command	fmt_memdisplay
		command	memory_info
		command	memdisplay
		command	exit_monitor
		command	options
		command	redirect
		command	ascii_dump
		command	version
		command list_tasks
		command list_libraries
		command list_devices
		command list_resources
		command list_ports
		command list_semaphores
		command	list_interrupts
		command	list_expansion

**** HELP TEXT ****
help_text	dc.b	TAB,'-- Amiga Monitor v'
		VERSION
		dc.b	'--',LF

;
; not in this list:
;	quit, goto, if, echo (script commands)
;
		dc.b	'??',TAB,TAB,TAB,TAB,'short list of commands',LF
		dc.b	'ver',TAB,TAB,TAB,TAB,'show version',LF
		dc.b	'help',TAB,TAB,TAB,TAB,'this help',LF
		dc.b	'x',TAB,TAB,TAB,TAB,'exit the monitor',LF
		dc.b	'? expr',TAB,TAB,TAB,TAB,'calculator',LF
		dc.b	'exec <filename>',TAB,TAB,TAB,'execute a script',LF
		dc.b	'poke/pokew/pokel <addr> <value>',TAB,'write to memory',LF
		dc.b	'@ [cmdline]',TAB,TAB,TAB,'enter command line',LF
		dc.b	'ba [base]',TAB,TAB,TAB,'set/show default number base',LF
		dc.b	'\ [command]',TAB,TAB,TAB,'shell command/newshell',LF
		dc.b	'cls',TAB,TAB,TAB,TAB,'clear window',LF
		dc.b	'dir [directory]',TAB,TAB,TAB,'display a directory',LF
		dc.b	'del <filename>',TAB,TAB,TAB,'delete a file',LF
		dc.b	'cd <directory',TAB,TAB,TAB,'change current dir',LF
		dc.b	'o [filename]',TAB,TAB,TAB,'redirect output',LF
		dc.b	'i [addr1] [addr2]',TAB,TAB,'ASCII memory dump',LF
		dc.b	'm [addr1] [addr2],'TAB,TAB,'Hex/ASCII memory dump',LF
		dc.b	'mf [addr1] [addr2] "fmtstring"',TAB,'formatted memory dump',LF
		dc.b	'f <addr1> <addr2> <data>',TAB,'fill memory',LF
		dc.b	't <addr1> <addr2> <dest>',TAB,'transfer memory',LF
		dc.b	'c <addr1> <addr2> <dest>',TAB,'compare memory',LF
		dc.b	'h <addr1> <addr2> <string>',TAB,'search for a string',LF
		dc.b	': <addr> <data>',TAB,TAB,TAB,'write bytes to memory',LF
		dc.b	'opt [+/-<num>]',TAB,TAB,TAB,'set/show option flags',LF
		dc.b	'rb [addr] [reg]',TAB,TAB,TAB,'set/show relative base reg',LF
		dc.b	'! <addr> <len> <period> [count]',TAB,'play digisound',LF
		dc.b	'# <addr> [len] [offs]',TAB,TAB,'correct bootblock checksum',LF
		dc.b	'= <addr> [len] [offs]',TAB,TAB,'correct disk block checksum',LF
		dc.b	'< <addr> <unit> <start> <n>',TAB,'read disk sectors',LF
		dc.b	'> <addr> <unit> <start> <n>',TAB,'write disk sectors',LF
		dc.b	'dev [devname]',TAB,TAB,TAB,'set/show disk device',LF
		dc.b	'[ <addr> <filename>',TAB,TAB,'read a file',LF
		dc.b	'] <addr> <len> <filename>',TAB,'write a file',LF
		dc.b	'& <addr> <len>',TAB,TAB,TAB,'allocate absolute memory',LF
		dc.b	'( <len> [c]',TAB,TAB,TAB,'allocate a memory block',LF
		dc.b	') <addr>',TAB,TAB,TAB,'free a memory block',LF
		dc.b	'sm',TAB,TAB,TAB,TAB,'show allocated memory',LF
		dc.b	'l [+] <filename>',TAB,TAB,'load an executable file',LF
		dc.b	'u',TAB,TAB,TAB,TAB,'unload executable file',LF
		dc.b	'sl',TAB,TAB,TAB,TAB,'segment list',LF
		dc.b	'r [reg=number]',TAB,TAB,TAB,'show/set registers',LF
		dc.b	'sr',TAB,TAB,TAB,TAB,'reset stack pointer',LF
		dc.b	'g <addr>',TAB,TAB,TAB,'execute code',LF
		dc.b	'j <addr>',TAB,TAB,TAB,'execute subroutine',LF
		dc.b	'w <addr>',TAB,TAB,TAB,'trace code',LF
		dc.b	'e <addr>',TAB,TAB,TAB,'extended trace',LF
		dc.b	'q <addr>',TAB,TAB,TAB,'quicktrace',LF
		dc.b	'z <addr>',TAB,TAB,TAB,'skip an instruction',LF
		dc.b	'b <addr>',TAB,TAB,TAB,'set a breakpoint',LF
		dc.b	'br <addr>/#n/all',TAB,TAB,'remove a breakpoint',LF
		dc.b	'bl',TAB,TAB,TAB,TAB,'list breakpoints',LF
		dc.b	'set [var][=value]',TAB,TAB,'set/clear/show variables',LF
		dc.b	'cv',TAB,TAB,TAB,TAB,'clear all variables',LF
		dc.b	'd [addr] [endaddr]',TAB,TAB,'disassemble',LF
		dc.b	'a [addr] [code]',TAB,TAB,TAB,'assemble',LF
		dc.b	'vt',TAB,TAB,TAB,TAB,'show tasks',LF
		dc.b	'vl',TAB,TAB,TAB,TAB,'show libraries',LF
		dc.b	'vd',TAB,TAB,TAB,TAB,'show devices',LF
		dc.b	'vr',TAB,TAB,TAB,TAB,'show resources',LF
		dc.b	'vp',TAB,TAB,TAB,TAB,'show public message ports',LF
		dc.b	'vs',TAB,TAB,TAB,TAB,'show public semaphores',LF
		dc.b	'vi',TAB,TAB,TAB,TAB,'show interrupts',LF
		dc.b	've',TAB,TAB,TAB,TAB,'show autoconfig devices',LF
		dc.b	0

dos_name	dc.b	'dos.library',0
intuition_name	dc.b	'intuition.library',0
trackdisk_name	dc.b	'trackdisk.device',0
expansion_name	dc.b	'expansion.library',0

version_string	dc.b	0,'$VER: '
version_msg	dc.b	'Amiga Monitor v'
		VERSION
		dc.b	' ('
		DATE
		dc.b	')',LF,0

window_fmt	dc.b	'RAW:%ld/%ld/%ld/%ld/Amiga Monitor v'
		VERSION
		dc.b	0

welcome_txt	dc.b	LF,TAB,TAB,TAB,' --- Amiga Monitor ---',LF,LF
		dc.b	TAB,'   Copyright 1987-1994 by Timo Rossi, version '
		VERSION
		ifne	BETA
		dc.b	LF,LF,TAB
		dc.b	'THIS IS A BETA VERSION, DO NOT DISTRIBUTE!'
		endc
		dc.b	LF,LF,0

main_prompt	dc.b	'-> ',0

usage_txt	dc.b	'Usage: [run] mon [-w<width>] [-h<height>] '
		dc.b	'[-o<opt>]',LF
		dc.b	'    [-s<script>] [[+] filename [cmdline]]',LF,0
win_openerr_txt	dc.b	'Can''t open window',LF,0

cls_str		dc.b	ESC,'[H',ESC,'[J',0
cls2_str	dc.b	12,12,0

unknown_cmd_fmt	dc.b	"Unknown command '%s'",0

script_err_fmt	dc.b	"in script '%s' line %ld",LF,0

error_messages	dc.b	'Syntax error',0		; 0
		dc.b	'odd address',0			; 1
out_range_txt	dc.b	'out of range',0		; 2
		dc.b	'out of memory',0		; 3
		dc.b	'invalid expression',0		; 4
		dc.b	'illegal addrmode',0		; 5
		dc.b	'script only',0			; 6
		dc.b	'undefined label',0		; 7
		dc.b	'breakpoint already set',0	; 8
		dc.b	'no such breakpoint',0		; 9
break_txt	dc.b	'***Break',0			;10

more_prompt	dc.b	'<More>',0
mclr_txt	dc.b	CR,'      ',CR,0

		end
