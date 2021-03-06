;
; misc_cmd.asm
;
		include	"monitor.i"
;
; This module defines the following command routines:
;
;	setshow_base,cmdline,calculator,options,echo
;	pokel,pokew,poke
;
; And the following public subroutine:
;
;	set_cmdline
;

		include	"offsets.i"

		xref	generic_error
		xref	odd_address_error

;
; display/change default number base
;
		cmd	setshow_base

		call	skipspaces
		tst.b	(a3)
		beq.s	showbase
		call	GetDecNum
		bcs.s	base_err
		moveq	#2,d1
		cmp.b	d1,d0
		bcs.s	base_err
		moveq	#36,d1
		cmp.b	d1,d0
		bhi.s	base_err
		move.b	d0,mon_DefNumBase(a4)
		rts

base_err	bra	generic_error

showbase	moveq	#0,d0
		move.b	mon_DefNumBase(a4),d0
		lea	base_fmt(pc),a0
		call	JUMP,printf_window

*** enter command line ***
		cmd	cmdline

		tst.b	(a3)
		bne.s	set_cmd_line
		lea	cmdline_prompt(pc),a0
		call	printstring_a0_window
		moveq	#0,d0
		call	GetInput

;
; copy string from input buffer to command line & set d0 & a0
;
		pub	set_cmdline

set_cmd_line	lea	mon_CmdLineBuf(a4),a1
		move.l	a1,a2
1$		move.b	(a3)+,(a1)+
		bne.s	1$
		move.b	#LF,-1(a1)
		clr.b	(a1)
		sub.l	a2,a1
		move.l	a1,mon_DataRegs(a4)		;set d0
		move.l	a2,mon_AddrRegs(a4)		;set a0

;
; ReadArgs()-compatibility
;
		lib	Dos,Input
		tst.l	d0
		beq.b	2$

		lsl.l	#2,d0
		move.l	d0,a0
		bra.b	3$

2$		lea	mon_FakeFH(a4),a0
		move.l	a0,d0
		lsr.l	#2,d0
		move.l	mon_Task(a4),a1
		move.l	d0,pr_CIS(a1)

3$		tst.l	mon_OldFhBuf(a4)
		bne.b	4$
		move.l	fh_Buf(a0),mon_OldFhBuf(a4)
		move.l	fh_Pos(a0),mon_OldPos(a4)
		move.l	fh_End(a0),mon_OldEnd(a4)

4$		move.l	mon_AddrRegs(a4),d0
		lsr.l	#2,d0
		move.l	d0,fh_Buf(a0)
		clr.l	fh_Pos(a0)
		move.l	mon_DataRegs(a4),fh_End(a0)
		rts

;
; display text command
;
		cmd	echo

echo_loop	call	skipspaces
		cmp.b	#'[',(a3)
		beq.s	echo_dec
		cmp.b	#'$',(a3)
		beq.s	echo_hex
		call	GetName
		tst.l	d0
		beq.s	echo_end
		move.l	d0,a0
		call	printstring_a0

echo_space	emit	SPACE
		bra.s	echo_loop

echo_end	moveq	#LF,d0
		call	JUMP,ChrOut

echo_dec	addq.l	#1,a3
		call	GetExpr
		lea	echo_dec_fmt(pc),a0
echo_num_com	call	printf
		cmp.b	#']',(a3)+
		bne	generic_error
		bra.s	echo_space

echo_hex	addq.l	#1,a3
		cmp.b	#'[',(a3)+
		bne	generic_error
		call	GetExpr
		lea	echo_hex_fmt(pc),a0
		bra.s	echo_num_com

*** Calculator command ***
		cmd	calculator

		tst.b	(a3)
		bne.s	01$
		lea	calc_prompt(pc),a0
		call	printstring_a0_window
		moveq	#0,d0
		call	GetInput
01$		call	GetExpr

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
numdisp_loop	move.b	d5,d0
		call	to_printable
		move.b	d0,-(a0)
		lsr.l	#8,d5
		dbf	d1,numdisp_loop
		move.b	#'''',(a3)+
		endline
		call	JUMP,printstring

;print number in one base (both signed & unsigned if negative)
;d4:high contains number of digits or $ffff, d4:low contains numeric base
PrintNumber	startline
		tst.l	D5
		bpl.s	num_A1
		lea	sign_txt(pc),A1
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
		putchr	SPACE
		tst.b	d3
		beq.s	170$
		move.b	d3,(a3)+
170$		move.l	D5,D0
		move.w	d4,d2
		move.l	d4,d1
		swap	d1
		call	put_number
		endline
		call	printstring
		tst.l	D5
		bpl.s	num_A2
		startline
		moveq	#SPACE,D0
		move.b	D0,(A3)+
		move.b	D0,(A3)+
		lea	sign_txt+2(pc),A1
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
		call	put_number
		endline
		call	JUMP,printstring
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
		call	GetExpr
		subq.w	#1,d0
		bclr	d0,mon_Options(a4)
		rts

add_option	call	GetExpr
		subq.w	#1,d0
		bset	d0,mon_Options(a4)
		rts

show_options	moveq	#0,d3
		lea	option_strings(pc),a2

showopt_loop	lea	off_txt(pc),a1
		btst	d3,mon_Options(a4)
		beq.s	1$
		addq.l	#on_txt-off_txt,a1
1$		move.l	a1,d2
		move.l	a2,d1
		move.l	d3,d0
		addq.w	#1,d0
		lea	option_fmt(pc),a0
		call	printf
		addq.w	#1,d3
skip_opt_str	tst.b	(a2)+
		bne.s	skip_opt_str
		tst.b	(a2)
		bne	showopt_loop

		moveq	#0,d0
		move.b	mon_Options(a4),d0
		lea	optvalue_fmt(pc),a0
		call	JUMP,printf
;
; poke[w|l]
;
		cmd	poke

		call	GetExpr
		move.l	d0,a2
		call	GetExpr
		move.b	d0,(a2)
		rts

		cmd	pokew

		call	GetExpr
		btst	#0,d0
		bne	odd_address_error
		move.l	d0,a2
		call	GetExpr
		move.w	d0,(a2)
		rts

		cmd	pokel

		call	GetExpr
		btst	#0,d0
		bne	odd_address_error
		move.l	d0,a2
		call	GetExpr
		move.l	d0,(a2)
		rts

sign_txt	dc.b	'unsigned',0
base_fmt	dc.b	'Base is %ld',LF,0
cmdline_prompt	dc.b	'Cmdline> ',0
calc_prompt	dc.b	'Calc> ',0

echo_hex_fmt	dc.b	'%lx',0
echo_dec_fmt	dc.b	'%ld',0

option_fmt	dc.b	'(%ld) %s: %s',LF,0
optvalue_fmt	dc.b	'Option flag value %02lx hex',LF,0
off_txt		dc.b	'off',0
on_txt		dc.b	'on',0

option_strings	dc.b	'Narrow disassembly',0
		dc.b	'8BitChars printable',0
		dc.b	'Dumb terminal',0
		dc.b	'Echo commands',0
		dc.b	'No auto stackreset',0
		dc.b	'lib()/etc. errors return zero',0
		dc.b	0

		end
