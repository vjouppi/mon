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
		include "graphics/gfxbase.i"
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

		moveq	#-1,d0
		move.l	d0,mon_OrigCD(a4)
		bset	#MONB_FIRSTCD,mon_Flags(a4)
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

		moveq	#0,d0
		moveq	#0,d1
		move.w	d4,d1
		bmi.s	win_1
		moveq	#0,d2
		move.w	d5,d2
		bpl.s	open_win

win_1		lib	Dos,Input
		move.l	d0,mon_WinFile(a4)
		beq.s	open_win1
		move.l	d0,d1
		lib	IsInteractive
		tst.l	d0
		beq.s	open_win1
		move.l	mon_WinFile(a4),d0
		bra	win_open

nogfx_win	moveq	#0,d0
		move.l	#200,d2
		bra.s	open_win2

open_win1	lea	gfx_name(pc),a1
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

open_win	lea	window_fmt(pc),a0
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

own_win_open	move.l	d0,d1
		lsl.l	#2,d1
		move.l	d1,a1
		move.l	mon_Task(a4),a0
		move.l	pr_ConsoleTask(a0),mon_OrigConTask(a4)
		move.l	fh_Type(a1),pr_ConsoleTask(a0)

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
		move.l	TC_Userdata(a0),mon_OrigUserData(a4)	; & UserData

set_tc		move.l	A1,TC_TRAPCODE(A0)		;and set a new one
		move.l	a4,TC_Userdata(a0)

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
		move.l	mon_InitialScriptName(a4),d0
		beq.s	mainloop

		move.l	d0,a0
		call	exec_script

*** JUMP HERE AFTER EXECUTION OF A COMMAND ***
mainloop	move.l	mon_StackPtr(a4),sp	;restore stack pointer
		move.l	mon_ScriptList(a4),d2
		beq.s	do_cmdline

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

help		bsr.s	cls
		lea	help_text(pc),A0
hinfo		call	JUMP,printstring_a0


;
;*** INFO ***
;
		cmd	info

		bsr.s	cls
		lea	info_text(pc),A0
		bra.s	hinfo

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

		move.l	mon_WinFile(a4),d0
		beq.s	01$
		moveq	#0,d1
		call	SetConMode

01$		btst	#MONB_TASKSET,mon_Flags(a4)
		beq.s	02$
;
; restore task TrapCode/UserData & pr_ReturnAddr fields
;
		move.l	mon_Task(a4),A0
		move.l	mon_OrigTrapCode(a4),TC_TRAPCODE(A0)	;restore old TrapCode
		move.l	mon_OrigUserData(a4),TC_Userdata(a0)	;and UserData
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
		move.l	mon_Task(a4),a0
		move.l	mon_OrigConTask(a4),pr_ConsoleTask(a4)
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

exit9		move.l	mon_WBenchMsg(a4),D3	started from workbench??

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
		dc.b	'i',0		;display info text
		dc.b	'ver',0		;show version & date
		dc.b	'vt',0		;list tasks
		dc.b	'vl',0		;list libraries
		dc.b	'vd',0		;list devices
		dc.b	'vr',0		;list resources
		dc.b	'vp',0		;list ports
		dc.b	'vs',0		;list semaphores

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
		command	info
		command	version
		command list_tasks
		command list_libraries
		command list_devices
		command list_resources
		command list_ports
		command list_semaphores

**** HELP TEXT ****
help_text	dc.b	TAB,'-- Amiga Monitor v'
		VERSION
		dc.b	' help (??=list cmds, i=info, x=exit) --',LF
		dc.b	'o [name]',TAB,':redirect output'
		dc.b	'| vt,vl,vd,vr,vp,vs :show tasks/libs/devs/etc',LF
		dc.b	'dir [name]',TAB,': directory',TAB
		dc.b	'| [ addr name',TAB,TAB,': load absolute',LF
		dc.b	'cd [name]',TAB,': current dir',TAB
		dc.b	'| ] addr length name',TAB,': save absolute',LF
		dc.b	'l [+] name',TAB,': load exefile',TAB
		dc.b	'| < addr unit block cnt',TAB,': read disk blocks',LF
		dc.b	'sl',TAB,TAB,': hunk list',TAB
		dc.b	'| > addr unit block cnt',TAB,': write disk blocks',LF
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
		dc.b	'b addr [cnt]',TAB,': set breakpoint'
		dc.b	'| w [addr]',TAB,TAB,': single step (walk)',LF
		dc.b	'bl',TAB,TAB,': list brkpoints'
		dc.b	'| e [addr]',TAB,TAB,': execute one instr.',LF
		dc.b	'br addr/#n/all',TAB,':remove brkpoint'
		dc.b	'| q [addr]',TAB,TAB,': quicktrace',LF
		dc.b	'f adr1 adr2 bytes: fill memory',TAB
		dc.b	'| ( length',TAB,TAB,': allocate memory',LF
		dc.b	'@ [line]',TAB,': enter cmd line'
		dc.b	'| & addr length',TAB,TAB,': allocate absolute',LF
		dc.b	'ba [decnum]',TAB,': set/show base',TAB
		dc.b	'| ) addr/all',TAB,TAB,': free memory',LF
		dc.b	'? [expr]',TAB,': calculator',TAB
		dc.b	'| sm',TAB,TAB,TAB,': show allocated mem',LF
		dc.b	'mi addr',TAB,TAB,': memory info',TAB
		dc.b	'| c addr1 addr2 dest',TAB,': compare memory',LF
		dc.b	'h adr1 adr2 bytes: hunt memory',TAB
		dc.b	'| t addr1 addr2 dest',TAB,': transfer memory',LF
		dc.b	'cv',TAB,TAB,': clear vars',TAB
		dc.b	'| set [var=expr] [hunk]',TAB,': set/show variables',LF,0

**** INFO TEXT ****
info_text	dc.b LF
		dc.b	TAB,TAB,'Monitor info (version '
		VERSION
		dc.b	')',LF
		dc.b	TAB,TAB,'---------------------------',LF
		dc.b	TAB,'   Copyright 1987-1991 by Timo Rossi',LF,LF
		dc.b	'   This is a machine code monitor/debugger for the Amiga.',LF
		dc.b	' Pressing the HELP-key displays a list of commands.',LF,LF
		dc.b	' Note1: Some of the assembler instructions require the',LF
		dc.b	' size specifier (.B, .W or .L), but it can''t be used by some others.',LF,LF
		dc.b	' Note2: the default number base is hex, use ''_'' as prefix',LF
		dc.b	' for decimal or change the base with the ba-command.',LF
		dc.b	' You can use expressions in most places where',LF
		dc.b	' numbers are needed. This version also supports symbols',LF
		dc.b	' and has a built-in script language.',LF,LF
		dc.b	' I hope you find this program useful, but if you find any bugs',LF
		dc.b	' in this program, please let me know.',LF,LF
		dc.b	' Read the ''mon.doc''-file for more information.',LF,0

dos_name	dc.b	'dos.library',0
gfx_name	dc.b	'graphics.library',0
trackdisk_name	dc.b	'trackdisk.device',0

version_string	dc.b	0,'$VER: '
version_msg	dc.b	'Amiga Monitor v'
		VERSION
		dc.b	' ('
		DATE
		dc.b	')',LF,0

window_fmt	dc.b	'RAW:0/%ld/%ld/%ld/Amiga Monitor v'
		VERSION
		dc.b	0

welcome_txt	dc.b	LF,TAB,TAB,TAB,' --- Amiga Monitor ---',LF,LF
		dc.b	TAB,'   Copyright 1987-1991 by Timo Rossi, version '
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

error_messages	dc.b	'Syntax error',0	; 0
		dc.b	'odd address',0		; 1
out_range_txt	dc.b	'out of range',0	; 2
		dc.b	'out of memory',0	; 3
		dc.b	'invalid expression',0	; 4
		dc.b	'illegal addrmode',0	; 5
		dc.b	'script only',0		; 6
		dc.b	'undefined label',0	; 7
break_txt	dc.b	'***Break',0		; 8

		end
