;
; execute.asm
;
		nolist
		include "exec/types.i"
		include "exec/memory.i"
		include "exec/tasks.i"
		include "exec/execbase.i"
		include "offsets.i"
		list

		include	"monitor.i"
		include "breakpoint.i"

;
; This module defines the following command routines:
;
;	set_break,remove_break,list_breaks,quicktrace,walk,skip_one
;	exe_one,go,jumpsr,showtrap
;
; And the following public subroutines:
;
;	find_brk_num,remove_all_breaks
;
		forward	find_brk_num

		xdef	trapreturn
		xdef	returncode

		xref	displayregs
		xref	displayregs_d
		xref	mainloop

		xref	generic_error
		xref	out_memory_error

;
;*** SET BREAKPOINT ***
;
		cmd	set_break

		call	GetExpr
		btst	#0,D0
		bne.s	brk_err
		move.l	D0,a5

		moveq	#1,d2
		tst.b	(a3)
		beq.s	1$

		call	GetExpr
		move.l	d0,d2
		ble.s	brk_err

1$		move.l	a5,d0
		bsr	find_break
		bpl.s	brk_err		;break already set

		move.l	A1,D3
		moveq	#brk_SIZE,D0
		move.l	#MEMF_CLEAR!MEMF_PUBLIC,D1
		lib	Exec,AllocMem
		tst.l	D0
		beq	out_memory_error
		move.l	D0,A0
		move.l	a5,brk_Address(A0)
		move.w	d2,brk_Count(a0)
		tst.l	D3
		bne.s	no_start_of_list
		move.l	mon_BreakList(a4),(A0)
		move.l	A0,mon_BreakList(a4)
		rts

no_start_of_list
		move.l	D3,A1
		move.l	(A1),(A0)
		move.l	A0,(A1)
brset9		rts

brk_err		bra	generic_error

;
;*** REMOVE BREAKPOINT ***
; syntax: br all | br <addr> | br #num
;
		cmd	remove_break

		call	skipspaces
		move.b	(a3),d0
		call	tolower
		cmp.b	#'#',d0
		beq.s	rembrk_num
		cmp.b	#'a',d0
		bne.s	break_rem1	;check 'all'
		move.b	1(a3),d0
		call	tolower
		cmp.b	#'l',d0
		bne.s	break_rem1
		move.b	2(a3),d0
		call	tolower
		cmp.b	#'l',d0
		beq	rem_all_breaks

break_rem1	call	GetExpr
		bsr	find_break
		bmi.s	brk_err		;break not set

do_remove_brk	move.l	A1,D0
		bne.s	no_remove_from_start_of_list
		move.l	(A0),mon_BreakList(a4)
		bra.s	break_remove_1

no_remove_from_start_of_list
		move.l	(A0),(A1)
break_remove_1	move.l	A0,A1
		moveq	#brk_SIZE,D0
		jlib	Exec,FreeMem

;
; remove a numbered breakpoint (br #num)
;
rembrk_num	addq.l	#1,a3
		call	GetExpr
		call.s	find_brk_num
		moveq	#-1,d1
		cmp.l	d1,d0
		beq.s	brk_err
		bra.s	do_remove_brk
;
; params: breakpoint number in d0, returns breakpoint address or -1
;
		pub	find_brk_num

		clra	a1
		move.l	mon_BreakList(a4),d1
1$		beq	3$
		tst.l	d0
		beq.s	2$
		subq.l	#1,d0
		move.l	d1,a1
		move.l	brk_Next(a1),d1
		bra.s	1$

2$		move.l	d1,a0
		move.l	brk_Address(a0),d0
		rts

3$		moveq	#-1,d0
		rts

;
;*** LIST BREAKPOINTS ***
;* note: the list is automatically in order
; 1991-03-08 -- added breakpoint numbers in listing
;
		cmd	list_breaks

		move.l	mon_BreakList(a4),d4
		bne.s	break_list_1
		lea	nobreak_txt(pc),A0
		call	JUMP,printstring_a0_window

break_list_1	lea	breaklist_txt(pc),A0
		call	printstring_a0
		moveq	#0,d3

break_list_loop	tst.l	d4
		beq	brset9
		move.l	d4,a5
		move.l	d3,d0
		move.l	brk_Address(a5),D1
		moveq	#0,d2
		move.w	brk_Count(a5),d2
		lea	brk_fmt(pc),a0
		call	printf
		call	CheckKeys
		bne	brset9
		move.l	brk_Next(a5),d4
		addq.l	#1,d3
		bra.s	break_list_loop

*** FIND BREAKPOINT FROM LINKED LIST ***
* address in D0
* if N=1 then not found, A1 points to where new node should be inserted
*  if A1=0 then insert to start of the list
* if N=0 then found, A0 points to the node, A1 points to predecessor
find_break	clra	a1
		move.l	mon_BreakList(a4),D1
find_br_1	beq.s	break_not_found
		move.l	D1,A0
		cmp.l	brk_Address(A0),D0
		beq.s	break_found
		bcs.s	break_not_found
		move.l	A0,A1
		move.l	brk_Next(A0),D1
		bra.s	find_br_1

break_not_found	moveq	#-1,D0
		rts

break_found	moveq	#0,D0
		rts

		pub	remove_all_breaks
* executed before exit of when the 'br all' command is given
; this changes d2 and a6
rem_all_breaks	getbase	Exec
		move.l	mon_BreakList(a4),D2

all_breaks_loop	tst.l	D2
		beq.s	brk_ret2
		move.l	D2,A1
		move.l	brk_Next(A1),D2
		moveq	#brk_SIZE,D0
		lib	FreeMem
		bra.s	all_breaks_loop

brk_ret2	clr.l	mon_BreakList(a4)
		rts

*** PUT THE ILLEGAL ($4AFC) INSTRUCTION TO BREAKPOINTS ***
* (first save original contents in the BreakPoint structure)
;
; don't set a breakpoint if the current PC is pointing to it
; but save the address of the breakpoint structure so the breakpoint
; can be activated from the trap handler...
;#
;# ->v1.22 1990-01-06
;#
;# a temporary breakpoint address is given as a parameter to this routine
;# in the register d0. if it is zero, no temporary breakpoint is used.
;# temporary breakpoint is used by the e-command.
;#
SetBreaks	bset	#MONB_BRKACTIVE,mon_Flags(a4)
		tst.l	d0
		beq.s	SetBr0
		move.l	d0,mon_TempBreakAddr(a4)
		bset	#MONB_TMPBRK,mon_Flags(a4)
		move.l	d0,a0
		move.w	(a0),mon_TempBreakSave(a4)
		move.w	#ILLEGAL_INSTR,(a0)

SetBr0		move.l	mon_BreakList(a4),D2
SetBr1		tst.l	D2
		beq.s	Flush_Cache
		move.l	D2,A1
		move.l	brk_Address(A1),A0
		cmp.l	mon_RegPC(a4),a0
		bne.s	01$
		move.l	a1,mon_GoBreakPtr(a4)
		bra.s	SetBr2

01$		move.w	(A0),brk_Content(A1)
		move.w	#ILLEGAL_INSTR,(A0)
SetBr2		move.w	brk_Count(a1),brk_ActCount(a1)
		move.l	(A1),D2
		bra.s	SetBr1

Flush_Cache	getbase	Exec,a6
		cmp.w	#36,LIB_VERSION(a6)
		bcs.s	flsh_end
		lib	CacheClearU
flsh_end	rts

*** RESTORE ORIGINAL CONTENTS OF BREAKPOINTS ***
RemBreaks	bclr	#MONB_BRKACTIVE,mon_Flags(a4)
		beq.s	flsh_end
RemBr0		move.l	mon_BreakList(a4),D2
RemBr1		tst.l	D2
		beq.s	RemBr3
		move.l	D2,A1
		move.l	brk_Address(A1),A0
		move.w	brk_Content(A1),(A0)
;#		clr.w	brk_Content(a1)		this helps debugging...
RemBr2		move.l	(A1),D2
		bra.s	RemBr1

RemBr3		bclr	#MONB_TMPBRK,mon_Flags(a4)
		beq.s	Flush_Cache

		move.l	mon_TempBreakAddr(a4),a0
		move.w	mon_TempBreakSave(a4),(a0)
		bra.s	Flush_Cache

;
; check stack pointer and reset it if necessary
; now resets also if stack pointer is odd
;
check_stackptr	move.l	mon_RegSP(a4),d0
		btst	#0,d0
		bne.s	reset_stack
		move.l	d0,a2

		move.l	mon_StackHigh(a4),a0
		cmp.l	a0,a2
		bhi.s	reset_stack
		lea	-MONSTACK(a0),a1
		cmp.l	a1,a2
		bcc.s	stack_set

reset_stack	move.l	a0,a2
		lea	returncode(pc),a1
		move.l	a1,(a0)			;put return addr. in stack
		lea	stackreset_txt(pc),a0
		call	printstring_a0_window

stack_set	move.l	a2,mon_RegSP(a4)		;sp
rts_001		rts

*** QUICK TRACE ***
;executes code in trace mode until a flow control-instruction is encountered
		cmd	quicktrace

		bsr.s	check_stackptr
		bsr.s	getpc
		bset	#MONB_QTRACE,mon_Flags(a4)
		moveq	#0,d0
		bsr	SetBreaks
		bra.s	walk_001

*** SINGLE STEP (WALK) ***
* NOTE: This ignores breakpoints
		cmd	walk

		bsr	check_stackptr
		bsr.s	getpc
walk_001	or	#2,ccr		;set overflow flag
		trapv			;let the trap handler do the rest...
walk_here	;a label so we can reference it in the handler routine

getpc		call	skipspaces
		tst.b	(a3)
		beq.s	01$
		call	GetExpr
		move.l	d0,mon_RegPC(a4)
01$		btst	#0,mon_RegPC+3(a4)	;error if current PC is odd
		beq.s	rts_001

*** SKIP ONE INSTRUCTION
;
; find the length of the current instruction by disassembling it
; Note that this does not execute or trace any code, it simply
; advances the monitor internal PC register.
;
		cmd	skip_one

		bsr	check_stackptr
		bsr.s	getpc
		move.l	mon_RegPC(a4),a5
		lea	mon_OutputBuf(a4),a3
		call	Disassemble
		move.l	a5,mon_RegPC(a4)
		lea	skip_txt(pc),a0
		call	printstring_a0
		bra	displayregs_d

*** EXECUTE ONE INSTRUCTION ***
;
; execute one instruction by placing a temporary breakpoint after it.
;
; find the length of the current instruction by disassembling it
; then set temporary breakpoint to the following instruction and
; do normal go...
;
		cmd	exe_one

		bsr	check_stackptr
		bsr.s	getpc
		move.l	mon_RegPC(a4),a5
		lea	mon_OutputBuf(a4),a3
		call	Disassemble
		move.l	a5,d0
		bra.s	go_com

**** EXECUTE MACHINE CODE (GO) ****
		cmd	go

		bsr	check_stackptr
		bsr.s	getpc
		moveq	#0,d0		;no temporary breakpoint
		bra.s	go_com

**** JUMP TO SUBROUTINE (RETURN WITH RTS) ***
		cmd	jumpsr

		bsr	check_stackptr
		bsr.s	getpc
		moveq	#0,d0		;no temporary breakpoint
;
; put return address in stack
;
		move.l	mon_RegSP(a4),a0
		lea	returncode(pc),A1
		move.l	a1,-(a0)
		move.l	a0,mon_RegSP(a4)

;
; common code for jump/go/execute/skip
;  temporary breakpoint address in d0, if d0==0 then no temporary
;  breakpoint is used
;
go_com		bsr	SetBreaks
		move.l	mon_RegPC(a4),d0
		bsr	find_break
		bpl.s	go_special

		move.l	mon_RegSP(a4),sp
		move.l	mon_RegPC(a4),-(sp)
;
; move <ea>,ccr size is WORD! assemblers should check size specifiers better.
;
		move.w	mon_RegCCR_W(a4),ccr
		movem.l	mon_DataRegs(a4),D0-D7/A0-A6
		rts	;this really jumps to the user program

go_special	or	#2,ccr		set overflow flag
		trapv			let the trap handler do the rest...
special_go_here

*** CONTROLS RETURNS HERE AFTER THE Jsr COMMAND ***
returncode	movem.l	d0-d2/a0-a1/a4/a6,-(sp)
		lib	AbsExec,GetCC
		move.l	d0,d2
		clra	a1
		lib	FindTask
		move.l	d0,a1
		move.l	TC_TRAPDATA(a1),a4
		move.b	d2,mon_RegCCR_B(a4)
		movem.l	(sp)+,d0-d2/a0-a1
		movem.l	d0-d7/a0-a3,mon_DataRegs(a4)
		move.l	a5,mon_AddrRegs+4*5(a4)		a5
		move.l	(sp)+,mon_AddrRegs+4*4(a4)	a4
		move.l	(sp)+,mon_AddrRegs+4*6(a4)	a6
		move.l	sp,mon_RegSP(a4)		sp

		move.l	mon_StackPtr(a4),sp		restore monitor sp

		bclr	#MONB_QTRACE,mon_Flags(a4)
		bsr	RemBreaks

		lea	returned_txt(pc),A0
		call	printstring_a0
		bsr	displayregs_d
		bra	mainloop

*** TASK TRAP CODE ***
trapreturn	;Note! We are in supervisor mode!
		movem.l	d0/a0/a4/a6,-(sp)
		getbase	AbsExec,a6
		move.l	ThisTask(a6),a4
		move.l	TC_TRAPDATA(a4),a4

		cmp.l	#9,4*4(sp)			trace?
		bne.s	noskipbrk
		bclr	#MONB_BRKSKIP,mon_Flags(a4)
		beq.s	noskipbrk
;
; we have just skipped a breakpoint in trace mode
;
; note: user stack pointer does not need to be initialized here
;
		move.l	mon_GoBreakPtr(a4),a4
		move.l	brk_Address(a4),a0
		move.w	(a0),brk_Content(a4)
		move.w	#ILLEGAL_INSTR,(a0)
;
; it might be a good idea to try to flush the cache here...
;
		movem.l	(sp)+,d0/a0/a4/a6
		addq.l	#4,sp			remove exception # from stack
		bclr	#7,(sp)			clear trace bit
		rte				continue at full speed

noskipbrk	cmp.l	#7,4*4(sp)		is this a TRAPV-trap
		bne.s	normtrap_1		(possibly by the walk routine)
		lea	walk_here(pc),a0
		cmp.l	4*4+6(sp),a0		check program counter
		beq.s	walk_trap
		lea	special_go_here(pc),a0
		cmp.l	4*4+6(sp),a0
		bne.s	normtrap_1

; this is the special go-routine if the address we are going to enter
; the code contains a breakpoint. in that case we trace over the
; instruction and then continue at full speed
		bset	#MONB_BRKSKIP,mon_Flags(a4)

; fall to walk trap routine
walk_trap	lea	4*4+4(sp),sp		clean up stack
		move.l	mon_RegPC(a4),2(sp)
		move	sr,d0
		move.b	mon_RegCCR_B(a4),d0
		bclr	#13,d0			supervisor mode off
		bset	#15,d0			trace mode on
		move.w	d0,(sp)
		move.l	mon_RegSP(a4),a0
		move.l	a0,usp
		movem.l	mon_DataRegs(a4),d0-d7/a0-a6
		rte

normtrap_1	movem.l	(sp)+,d0/a0
		movem.l	d0-d7/a0-a3,mon_DataRegs(a4)
		move.l	a5,mon_AddrRegs+4*5(a4)		;a5
		move.l	(sp)+,mon_AddrRegs+4*4(a4)	;a4
		move.l	(sp)+,mon_AddrRegs+6*4(a4)	;a6
		move	usp,a0
		move.l	a0,mon_RegSP(a4)

		move.w	AttnFlags(A6),D1
		move.l	(sp)+,D5
		cmp.w	#3,D5
		bhi.s	pop_SR_and_PC	jump if not bus error or address error
		btst	#AFB_68010,D1	we must check the type of the processor!
		bne.s	pop_SR_and_PC
		addq.l	#8,sp		;clean stack info left by bus/addr errors
					;by the 68000

pop_SR_and_PC	move.w	(sp)+,D0	status register
		move.b	D0,mon_RegCCR_B(a4)
		move.l	(sp)+,a5
		cmp.w	#ILLEGAL_INSTR,(a5)
		seq	d7
		move.l	a5,mon_RegPC(a4)

		btst	#AFB_68010,d1
		beq.s	go_user_mode

;
; here we handle 68010/20 stack frames
;
		move.w	(sp)+,d3	get stack frame type/exception number word
		and.w	#$f000,d3	extract frame type bits
		beq.s	go_user_mode	just continue if it is 4-word format
		moveq	#4,d2
		cmp.w	#$2000,d3	instruction error format
		beq.s	drop_stack
		moveq	#12,d2
		cmp.w	#$9000,d3	coprocessor mid-instruction format
		beq.s	drop_stack
		moveq	#50,d2
		cmp.w	#$8000,d3	68010 long format
		beq.s	drop_stack
		moveq	#24,d2
		cmp.w	#$a000,d3	68020 short format
		beq.s	drop_stack
		moveq	#84,d2
		cmp.w	#$b000,d3	68020 long format
		bne.s	go_user_mode

drop_stack	add.w	d2,sp

go_user_mode	and.w	#$5FFF,D0	clear supervisor & trace bits
		move	D0,SR		back to the user mode!

		movea.l	mon_StackPtr(a4),sp
					;restore monitor (user) stack pointer
;
; if MONF_BRKSKIP is set when we enter here, some other exception has
; occurred before the trace-exception that should have cleared the
; flag and activated the breakpoint. in that case we must activate the
; breakpoint here (RemBreaks tries to remove it anyway, and that could
; cause problems...)
;
		bclr	#MONB_BRKSKIP,mon_Flags(a4)
		beq.s	01$
		move.l	mon_GoBreakPtr(a4),a1		activate breakpoint
		move.l	brk_Address(a1),a3
		move.w	(a3),brk_Content(a1)
		move.w	#ILLEGAL_INSTR,(a3)
01$
		btst	#MONB_BRKACTIVE,mon_Flags(a4)
		bne.s	02$
		clr.b	d7
02$
		ifd	DEBUG
		btst	#AFB_68010,d1
		beq.s	099$
		lsl.l	#4,d3
		swap	d3
		moveq	#0,d0
		move.w	d3,d0
		lea	frametypefmt(pc),a0
		call	printf_window
099$
		endc

		cmp.w	#9,d5		;trace ?
		bne.s	no_qtrace
		btst	#MONB_QTRACE,mon_Flags(a4)
		beq.s	no_qtrace

handle_qtrace	move.l	mon_RegPC(a4),d0
		btst	#0,d0
		bne.s	no_qtrace

		move.l	d0,a5
		move.w	(a5),d0
		bsr	is_flowctrl
		bcc	walk_001

		lea	mon_OutputBuf(a4),a3
		call	Disassemble
		tst.w	d0
		bne	walk_001

		lea	qtrace_txt(pc),a0
		bra.s	trap_msg_dregs

no_qtrace	move.l	mon_RegPC(a4),D0
		cmp.w	#4,D5
		bne.s	normal_trap
		cmp.l	mon_TempBreakAddr(a4),d0
		bne.s	1$
		lea	etrace_txt(pc),a0
		bra.s	trap_msg_dregs

1$		bsr	find_break
		bmi.s	normal_trap
		tst.b	d7
		beq.s	normal_trap
;
; handle breakpoint counts here
;
		sub.w	#1,brk_ActCount(a0)
		bne.s	handle_brk_count

do_brkpoint	lea	brkpoint_txt(pc),A0

trap_msg_dregs	call	printstring_a0
		bra.s	trap_dregs

handle_brk_count
		btst	#MONB_QTRACE,mon_Flags(a4)
		bne	do_brkpoint

		move.l	brk_Address(a0),a1
		move.w	brk_Content(a0),(a1)
		move.l	a0,mon_GoBreakPtr(a4)

		bsr	Flush_Cache
		bra	go_special

normal_trap	bsr.s	show_trap_name

trap_dregs	bclr	#MONB_QTRACE,mon_Flags(a4)
		bsr	RemBreaks
		bsr	displayregs
		move.l	mon_RegPC(a4),D0
		btst	#0,D0
		bne.s	tr99
		move.l	D0,mon_CurrentAddr(a4)
		move.l	D0,a5
		call	put_label
		startline
		call	Disassemble
		call	printstring
tr99		bra	mainloop

;
; test if a given instruction opcode word represents a flow-control
; instruction. return carry flag set if a flow-control instruction
;
is_flowctrl	lea	flowctrl_table(pc),a0

01$		tst.w	(a0)			;this clears carry
		beq.s	02$
		move.w	d0,d1
		and.w	(a0)+,d1
		cmp.w	(a0)+,d1
		bne.s	01$
		sec
02$		rts

flowctrl_table	dc.w	$f0f8,$50c8		;DBcc
		dc.w	$f000,$6000		;Bcc
		dc.w	$ff80,$4e80		;jmp/jsr
		dc.w	$fffd,$4e75		;rts/rtr
		dc.w	$fff0,$4e40		;trap #n
		dc.w	$f1c0,$4180		;chk
		dc.w	$ffff,$4e76		;trapv
		dc.w	0			;end of table

show_trap_name	;trap number in D5
		startline
		move.l	#'*** ',(A3)+
		move.w	D5,D0
		cmp.w	#$30,D0
		bcc.s	unknown_trap
		cmp.w	#$20,D0
		bcc.s	traps
		subq.w	#2,D0
		cmp.w	#10,D0
		bcs.s	txout1

unknown_trap	lea	unknowntrap(pc),a1
		call	putstring
		move.l	d5,d0
		call	put_signed_hexnum
		bra.s	notraps

traps		moveq	#10,D0
txout1		lea	trapnametable(pc),A0
		call	getnth
		move.l	a0,a1
		call	putstring
		cmp.w	#$20,D5
		bcs.s	notraps
		move.w	D5,D0
		sub.w	#$20,D0
		call	put_signed_hexnum

notraps		putchr	SPACE
		moveq	#'*',D0
		move.b	D0,(A3)+
		move.b	D0,(A3)+
		move.b	D0,(A3)+
		endline
		call	JUMP,printstring


*** SHOW TRAP NAME (USAGE: ^ num) ***
;#
;# ->1.21  1990-01-06
;# also debug command. ^^ shows monitor data area pointer
;#
		cmd	showtrap

		cmp.b	#'^',(a3)
		beq.s	dbug
		call	GetExpr
		move.l	D0,D5
		bra	show_trap_name

dbug		lea	hexfmt(pc),a0
		move.l	a4,d0
		call	printf
mloop_j		rts

returned_txt	dc.b	'*** Returned ***',LF,0
brkpoint_txt	dc.b	'*** Breakpoint ***',LF,0
etrace_txt	dc.b	'*** ExtTrace ***',LF,0
qtrace_txt	dc.b	'*** QTrace ***',LF,0

unknowntrap	dc.b	'Exception #',0

*** TRAP NAMES ***
trapnametable	dc.b	'Bus error',0
		dc.b	'Address error',0
		dc.b	'Illegal instruction',0
		dc.b	'Zero divide',0
		dc.b	'CHK instruction trap',0
		dc.b	'TRAPV instruction trap',0
		dc.b	'Privilege violation',0
		dc.b	'Trace',0
		dc.b	'Line-A emulator trap',0
		dc.b	'Line-F emulator trap',0
		dc.b	'TRAP instruction #',0

		ifd	DEBUG
frametypefmt	dc.b	'Frame type #$%02lx',LF,0
		endc

nobreak_txt	dc.b	'No Breakpoints set',LF,0
breaklist_txt	dc.b	'Breakpoints:',LF,0

skip_txt	dc.b	'*** Skipping ***',LF,0
stackreset_txt	dc.b	'*** Stack reset ***',LF,0

brk_fmt		dc.b	'%3ld   $%08lx   [%d]',LF,0
hexfmt		dc.b	'$%08lx',LF,0

		end
