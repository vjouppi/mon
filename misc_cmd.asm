;
; misc_cmd.asm
;
		include	"monitor.i"

		xref	getstring
		xref	getdecnum
		xref	puthex1_68
		xref	phex1_8
		xref	mainloop

		xref	generic_error

		xdef	set_cmdline

**** DISPLAY MEMORY ****
;#
;# ->v1.22  1990-01-06
;# memdisplay now reads memory as bytes and can be started at odd address.
;#
		cmd	memdisplay

display_memory	call	getparams
		moveq	#0,d6
		move.l	ConsoleUnit(a4),d0
		beq.s	00$
		move.l	d0,a0
		cmp.w	#65,cu_XMax(a0)
		scs	d6
00$		move.l	Addr(a4),a5

disploop	startline
		move.l	a5,D0
		bsr	puthex1_68
		putchr	<':'>
		putchr	SPACE
		moveq	#16/4-1,D2

01$		call	mgetw
		swap	d0
		call	mgetw
		bsr	phex1_8
		putchr	SPACE
		dbf	D2,01$

		sub.w	#16,a5
		putchr	SPACE
		tst.b	d6
		bmi.s	100$
		putchr	<''''>
100$		moveq	#16-1,D2

02$		move.b	(a5)+,D0	;printable codes are $20-$7F and $A0-$FF
		call	to_printable
		move.b	D0,(A3)+
		dbf	D2,02$

		tst.b	d6
		bmi.s	101$
		putchr	<''''>
101$		endline
		call	printstring
		call	CheckEnd
		bne.s	disploop

		move.l	a5,Addr(a4)
		bra	mainloop

;
; display/change default number base
;
		cmd	setshow_base

		call	tolower
		tst.b	(a3)
		beq.s	showbase
		bsr	getdecnum
		bcs.s	base_err
		moveq	#2,d1
		cmp.b	d1,d0
		bcs.s	base_err
		moveq	#36,d1
		cmp.b	d1,d0
		bhi.s	base_err
		move.b	d0,defbase(a4)
mloop_bx	bra	mainloop

base_err	bra	generic_error

showbase	moveq	#0,d0
		move.b	defbase(a4),d0
		lea	basefmt(pc),a0
		call	printf_window
		bra.s	mloop_bx

*** enter command line ***
		cmd	cmdline

		tst.b	(a3)
		bne.s	01$
		lea	cmdline_prompt(pc),a0
		call	printstring_a0_window
		moveq	#0,d0
		call	GetInput
01$		bsr.s	set_cmdline
		bra	mainloop

set_cmdline	lea	CmdLineBuf(a4),a1
		move.l	a1,a2
02$		move.b	(a3)+,(a1)+
		bne.s	02$
		move.b	#LF,-1(a1)
		clr.b	(a1)
		sub.l	a2,a1
		move.l	a1,DataRegs(a4)		;set d0
		move.l	a2,AddrRegs(a4)		;set a0
		rts


*** Calculator command ***
		cmd	calculator

		call	tolower
		tst.b	(a3)
		bne.s	01$
		lea	calc_prompt(pc),a0
		call	printstring_a0_window
		moveq	#0,d0
		call	GetInput
01$		call	get_expr

		tst.b	(a3)
		bne	generic_error

*** DISPLAY NUMBER IN HEX, DECIMAL, OCTAL, BINARY and ASCII ***
		move.l	D0,D5
		move.l	#$00070010,d4
		move.l	#'$hex',d6
		bsr.s	PrintNumber
		move.l	#$ffff000a,d4
		move.l	#'dec',d6
		bsr.s	PrintNumber
		move.l	#$ffff0008,d4
		move.l	#'@oct',d6
		bsr.s	PrintNumber
		move.l	#$001f0002,d4
		move.l	#'%bin',d6
		bsr.s	PrintNumber
		startline
		move.l	#' chr',(a3)+
		move.w	#': ',(a3)+
		move.b	#'''',(a3)+
		addq.l	#4,a3
		move.l	a3,a0
		moveq	#4-1,d1
2$		move.b	d5,d0
		call	to_printable
		move.b	d0,-(a0)
		lsr.l	#8,d5
		dbf	d1,2$
		move.b	#'''',(a3)+
		endline
		call	printstring
		bra	mainloop

;print number in one base (both signed & unsigned if negative)
;d4:high contains number of digits or $ffff, d4:low contains numeric base
PrintNumber	startline
		tst.l	D5
		bpl.s	num_A1
		lea	signtxt(pc),A1
		call	putstring
num_A1		swap	d6
		move.w	d6,d3
		and.w	#$00ff,d6
		or.w	#SPACE<<8,d6
		swap	d6
		lsr.w	#8,d3
		move.l	d6,D0
		call	PutLong
		putchr	<':'>
		putchr	SPACE
		tst.b	d3
		beq.s	170$
		move.b	d3,(a3)+
170$		move.l	D5,D0
		move.w	d4,d2
		move.l	d4,d1
		swap	d1
		call	PutNum
		endline
		call	printstring
		tst.l	D5
		bpl.s	num_A2
		startline
		moveq	#SPACE,D0
		move.b	D0,(A3)+
		move.b	D0,(A3)+
		lea	signtxt+2(pc),A1
		call	putstring
		move.l	d6,D0
		call	PutLong
		putchr	<':'>
		putchr	SPACE
		move.l	D5,D0
		neg.l	D0
		putchr	<'-'>
		tst.b	d3
		beq.s	180$
		move.b	d3,(a3)+
180$		move.w	d4,d2
		move.l	d4,d1
		swap	d1
		call	PutNum
		endline
		call	printstring
num_A2		rts

;
; set/show options command (opt +/- <num>)
;
		cmd	options

		move.b	(a3)+,d0
		beq.s	show_options

		cmp.b	#'+',d0
		beq.s	add_option
		cmp.b	#'-',d0
		bne	generic_error
; remove option
		call	get_expr
		subq.w	#1,d0
		bclr	d0,MonOptions(a4)
		bra.s	opt_9

add_option	call	get_expr
		subq.w	#1,d0
		bset	d0,MonOptions(a4)
		bra.s	opt_9

show_options	moveq	#0,d3
		lea	option_strings(pc),a2

0$		lea	off_txt(pc),a1
		btst	d3,MonOptions(a4)
		beq.s	1$
		addq.l	#on_txt-off_txt,a1
1$		move.l	a1,d2
		move.l	a2,d1
		move.l	d3,d0
		addq.w	#1,d0
		lea	option_fmt(pc),a0
		call	printf
		addq.w	#1,d3
2$		tst.b	(a2)+
		bne.s	2$
		tst.b	(a2)
		bne	0$

		moveq	#0,d0
		move.b	MonOptions(a4),d0
		lea	optvalue_fmt(pc),a0
		call	printf

opt_9		bra	mainloop

;
;*** MODIFY MEMORY ***
;
		cmd	modifymem

		call	get_expr
		move.l	D0,A0
		bsr	getstring
		bra	mainloop

signtxt		dc.b	'unsigned',0
basefmt		dc.b	'Base is %ld',LF,0
cmdline_prompt	dc.b	'Cmdline> ',0
calc_prompt	dc.b	'Calc> ',0

option_fmt	dc.b	'(%ld) %s: %s',LF,0
optvalue_fmt	dc.b	'Option flag value %02lx hex',LF,0
off_txt		dc.b	'off',0
on_txt		dc.b	'on',0

option_strings	dc.b	'Narrow disassembly',0
		dc.b	'8BitChars printable',0
		dc.b	'Dumb terminal',0
		dc.b	0

		end
