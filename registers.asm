;
;  registers.asm
;

		include	"monitor.i"
;
; This module defines the following command routine:
;
;	setshow_regs
;
		xdef	displayregs
		xdef	displayregs_d

		xref	generic_error

*** DISPLAY & CHANGE REGISTERS ***
		cmd	setshow_regs

		call	skipspaces
		tst.b	(A3)
		bne.s	changeregs

displayregs_d	bsr	displayregs
		move.l	mon_RegPC(a4),D0
		beq.s	rr9
		btst	#0,D0
		bne.s	rr9
		move.l	D0,a5
		tst.w	(a5)
		beq.s	rr9
		call	put_label
		startline
		call	Disassemble
		tst.w	d0
		bne.s	rr9
		call	printstring
rr9		rts

skipequal ;the syntax of the register change command can include a '=' sign
		call	skipspaces
		cmp.b	#'=',(A3)	;but it is not necessary
		bne.s	01$
		addq.l	#1,A3
01$		rts

changeregs	move.b	(A3)+,D0	;** CHANGE REGISTERS **
		call	tolower
		lsl.w	#8,D0
		move.b	(A3)+,D0	;get possibly unaligned word
		call	tolower
		cmp.w	#'pc',D0	;program counter
		bne.s	nopc
		bsr.s	skipequal
		call	GetExpr
		move.l	D0,mon_RegPC(a4)
		rts

;#
;# now 'ccr' can be used instead of 'cc' -- 1989-12-09
;#
nopc		cmp.w	#'cc',D0	;condition code register
		bne.s	nocc
		move.b	(a3),d0
		call	tolower
		cmp.b	#'r',d0
		bne.s	01$
		addq.l	#1,a3
01$		bsr.s	skipequal
		call	GetExpr
		move.b	D0,mon_RegCCR_B(a4)
		rts

nocc		cmp.w	#'d0',D0
		bcs.s	nodr
		cmp.w	#'d7',D0
		bhi.s	nodr
		sub.w	#'d0',D0
		lsl.w	#2,D0
		move.w	D0,D2
		bsr	skipequal
		call	GetExpr
		lea	mon_DataRegs(a4),A0
		move.l	D0,0(A0,D2.W)
		rts

nodr		cmp.w	#'sp',d0
		bne.s	nosp
		moveq	#7,d0
		bra.s	setareg
nosp		cmp.w	#'a0',D0
		bcs	generic_error
		cmp.w	#'a7',D0
		bhi	generic_error
		sub.w	#'a0',D0
setareg		lsl.w	#2,D0
		move.w	D0,D2
		bsr	skipequal
		call	GetExpr
		lea	mon_AddrRegs(a4),A0
		move.l	D0,0(A0,D2.W)
		rts

*** DISPLAY REGISTERS ***
;#
;# condition code register is now displayed as ccr, not cc
;#
displayregs	startline
		move.l	#' PC=',(A3)+
		move.l	mon_RegPC(a4),D0
		call	puthex8
		move.l	#' CCR',(A3)+
		putchr	<'='>
		move.b	mon_RegCCR_B(a4),D0
		move.b	D0,D2
		moveq	#2,d1
		call	put_hexnum1
		moveq	#4,D1

flagloop	putchr	SPACE
;
; hmm..it seems that a68k does not handle the pc-relative indexed addressing
; mode if you do not specify the "(pc,". this is also a problem with
; the monitor assembler...*now monitor assembler also understands (pc,indx)*
;
		move.b	flagstring(pc,D1.L),(A3)+
		putchr	<'='>
		moveq	#'0',D0
		btst	D1,D2
		beq.s	flag1
		addq.b	#1,D0
flag1		move.b	D0,(A3)+
		subq.l	#1,D1
		bpl.s	flagloop
		endline
		call	printstring
		lea	mon_DataRegs(a4),A2
		move.l	#' D0=',D2
		bsr.s	PrintRegLine
		bsr.s	PrintRegLine
		move.l	#' A0=',D2
		bsr	PrintRegLine

PrintRegLine	startline
		moveq	#4-1,D3
regl1		move.l	D2,(A3)+
		move.l	(A2)+,D0
		call	puthex8
		add.w	#$0100,D2
		dbf	D3,regl1
		endline
		call	JUMP,printstring

flagstring	dc.b	'CVZNX'

		end
