;
; Monitor Disassembly module
;
;
		include	"monitor.i"
		include "variables.i"
		include	"instructions.i"

;
; This module defines the following public subroutine:
;
;	Disassemble
;
; And the command routine 'disassem'
;

		forward	Disassemble

		xdef	instr_names
		xdef	ccodes

;
; equates for disassembler routine
;

EABITS		equ	$1f
EA_DATA		equ	1
EA_MEMORY	equ	2
EA_CONTROL	equ	4
EA_ALTER	equ	8
EA_NOTIMM	equ	$10	;not immediate

SIZBITS		equ	$60
SIZSHFT		equ	5
S_BYTE		equ	$20
S_WORD		equ	$40
S_LONG		equ	$60

; this bit means that there is a special routine to handle this instruction
SPECIAL_CASE	equ	$80

DRB	equ	$80	; data register, number from bits 11-9
DR2	equ	$81	; data register, number from bits 2-0
ARB	equ	$82	; address register, number from bits 11-9
AR2	equ	$83	; address register, number from bits 2-0
EA	equ	$84	; effective address
SZ	equ	$85	; size specifier
IMM	equ	$86	; immediate data byte/word/long
IMMQ	equ	$87	; immediate data for addq/subq/shifts
IMVQ	equ	$88	; immediate data for moveq
SCCR	equ	$89	; sr/ccr
SD	equ	$8a	; shift direction
STP1	equ	$8b	; register shift type
STP2	equ	$8c	; memory shift type
CC	equ	$8d	; condition code or 'sr' or 'ra'
SOFFS	equ	$8e	; 8-bit pc-relative offset (short branches)
OFFS	equ	$8f	; 16-bit pc-relative offset
IMMOFFS	equ	$90	; immediate word constant, no '#'-sign
TRAPN	equ	$91	; trap number
BIT	equ	$92	; bit instruction type

;
;*** DISASSEMBLE ****
;
		cmd	disassem

		call	GetParams
		bclr	#0,mon_CurrentAddr+3(a4)
		move.l	mon_CurrentAddr(a4),a5

disasmloop	move.l	a5,d0
		call	put_label

		startline
		call.s	Disassemble
		call	printstring
		call	CheckEnd
		bne.s	disasmloop
		move.l	a5,mon_CurrentAddr(a4)
		rts


*** DISASSEMBLE ONE INSTRUCTION ***
;
; disassembler routine main entry point
;
; a5 contains memory address of opcode on entry, incremented to
; next instruction on exit.
;
; a3 contains address of output text buffer, incremented to end of
; text on exit.
;
; return pointer to start of instruction mnemonic name in a0
; return d0 zero if valid instruction was disasembled
; (now line-[af] and ILLEGAL are not considered as valid instructions)
;
		pub	Disassemble

		movem.l	d2-d7/a2/a6,-(sp)

		move.l	a5,d0
		call	puthex68

		moveq	#SPACE,d0
		btst	#OPTB_NARROWDIS,mon_Options(a4)
		beq.s	00$

		clr.l	mon_dis_Addr1(a4)
		move.b	d0,(a3)+
		move.b	d0,(a3)+
		bra.s	10$

00$		move.l	a3,a0
		addq.l	#1,a0
		move.l	a0,mon_dis_Addr1(a4)
		moveq	#24-1,d1
01$		move.b	d0,(a3)+
		dbf	d1,01$
10$		move.l	a3,-(sp)

; store stack pointer
		move.l	sp,mon_dis_StackStore(a4)

		move.l	a3,d3

		bsr	GetWord			get opcode
		move.l	a5,a6
		move.w	d0,d7
		bne.s	no_dcw
		move.w	(a5),d1
		and.w	#$ff00,d1
		beq.s	no_dcw
		lea	dcw_txt(pc),a0
		bsr	put_str
		moveq	#0,d0
		moveq	#2,d1
		call	put_hexnum
		moveq	#-1,d0
		bra	dis_end

no_dcw		tst.l	mon_dis_Addr1(a4)
		bne.s	03$

		and.w	#$f000,d0
		cmp.w	#$a000,d0
		beq	invalid
		cmp.w	#$f000,d0
		beq	invalid

03$		lea	DisAsmTable(pc),a2

instr_loop	move.w	d7,d0
		and.w	(a2)+,d0
		cmp.w	(a2)+,d0
		beq.s	i_found
		moveq	#0,d1
		move.b	1(a2),d1
		move.l	a2,d0			get next table entry
		add.l	d1,d0
		addq.l	#3,d0
		and.b	#$fe,d0
		move.l	d0,a2
		bra.s	instr_loop

i_found		move.b	(a2)+,d6		get flags
		bmi.s	special_routine

		clr.w	d4			get size
		move.b	d6,d4
		lsr.b	#SIZSHFT,d4
		and.b	#3,d4
		bne.s	siz01

		move.b	d7,d4
		lsr.b	#6,d4			get size from opcode bits 7-6
		addq.b	#1,d4
		and.b	#3,d4
siz01		and.b	#EABITS,d6

		clr.w	d5
		move.b	(a2)+,d5		get string length
		subq.w	#1,d5			for dbf

strloop		bsr.s	fmt_char
		dbf	d5,strloop

;#
;# -> v1.21
;# added test for line-[af] 1990-01-06
;# also ILLEGAL ($4afc) is not a valid instruction
;#
		cmp.w	#ILLEGAL_INSTR,d7
		beq.s	invalid_instr
		and.w	#$f000,d7
		cmp.w	#$a000,d7
		beq.s	invalid_instr
		cmp.w	#$f000,d7
		bne.s	valid_instr

invalid_instr	moveq	#-1,d0
		bra.s	dis_end

special_routine	clr.w	d4
		move.b	d6,d4
		lsr.b	#SIZSHFT,d4
		and.b	#3,d4
		and.b	#EABITS,d6

		clr.w	d5
		move.b	(a2)+,d5
		subq.w	#3,d5
		move.l	a2,d2
		lea	2(a2),a2

01$		bsr.s	fmt_char
		dbf	d5,01$

		move.l	d2,a0
		add.w	(a0),a0
		jsr	(a0)

valid_instr	moveq	#0,d0

dis_end		move.b	#LF,(a3)+
		clr.b	(a3)
		move.l	(sp)+,a0
		movem.l	(sp)+,d2-d7/a2/a6
		rts

fmt_char	moveq	#0,d0
		move.b	(a2)+,d0
		bmi.s	do_special
		cmp.b	#TAB,d0
		beq.s	do_tab
		move.b	d0,(a3)+
rt1		rts

do_tab		move.l	4(sp),a0
		addq.l	#8,a0
01$		cmp.l	a0,a3
		bcc.s	rt1
		move.b	#SPACE,(a3)+
		bra.s	01$

do_special	cmp.b	#TOKSTART,d0
		bcc.s	str_tok
		and.b	#$7f,d0
		add.w	d0,d0
		lea	str_routines(pc,d0.w),a0
		add.w	(a0),a0
		jmp	(a0)

str_tok		sub.w	#TOKSTART,d0
		lea	instr_names(pc),a0
		call	getnth
		bra	put_str

str_routines	rw	datar11,datar2,addrr11,addrr2
		rw	effective_address
		rw	size_specifier
		rw	immediate_data
		rw	immediate_quick,immediate_moveq
		rw	sr_or_ccr
		rw	shiftdir,regshifttype,memshifttype
		rw	condcodes
		rw	short_pc_offset,pc_offset
		rw	immoffs,trapnum,bit_instr_type

;
; note: GetWord & GetLong do not modify d1
;
GetWord		movem.l	d1/a3,-(sp)
		move.w	(a5)+,d0
		tst.l	mon_dis_Addr1(a4)
		beq.s	getw_end
		move.l	mon_dis_Addr1(a4),a3
		move.w	d0,-(sp)
		moveq	#4,d1
		call	put_hexnum1
		move.w	(sp)+,d0
		move.b	#SPACE,(a3)+
		move.l	a3,mon_dis_Addr1(a4)
getw_end	movem.l	(sp)+,d1/a3
		rts

GetLong		movem.l	d1/a3,-(sp)
		move.l	(a5)+,d0
		tst.l	mon_dis_Addr1(a4)
		beq.s	01$
		move.l	mon_dis_Addr1(a4),a3
		move.l	d0,-(sp)
		moveq	#8,d1
		call	put_hexnum1
		move.l	(sp)+,d0
		move.b	#SPACE,(a3)+
		move.l	a3,mon_dis_Addr1(a4)
01$		movem.l	(sp)+,d1/a3
		rts

datar11		move.b	#'d',(a3)+
num11		move.w	d7,d0
		lsr.w	#8,d0
		lsr.w	#1,d0
num2		and.b	#7,d0
nump		add.b	#'0',d0
		move.b	d0,(a3)+
		rts

datar2		move.b	#'d',(a3)+
		move.w	d7,d0
		bra.s	num2

addrr11		move.w	d7,d0
		lsr.w	#8,d0
		lsr.w	#1,d0
		bra.s	ar01

addrr2		move.w	d7,d0
ar01		and.w	#7,d0
		bra	ardirect

immediate_quick	move.w	d7,d0
		lsr.w	#8,d0
		lsr.w	#1,d0
		and.b	#7,d0
		bne.s	01$
		moveq	#8,d0
01$		move.b	#'#',(a3)+
		bra.s	nump

size_specifier	tst.w	d4
		beq.s	invalid
		move.b	#'.',(a3)+
		move.b	sizes-1(pc,d4.w),(a3)+
		rts

sizes		dc.b	'bwl'

nothing_txt	dc.b	'nothing',0
pc_txt		dc.b	'(pc',0
dcw_txt		dc.b	'dc.w    ',0

		even

invalid		move.l	mon_dis_StackStore(a4),sp	restore stack ptr

		move.l	d3,a3

		tst.l	mon_dis_Addr1(a4)
		bne.s	00$

		lea	dcw_txt(pc),a0
		bsr	put_str
		moveq	#0,d0
		move.w	-2(a5),d0
		moveq	#4,d1
		call	put_hexnum
		move.l	a6,a5
		bra.s	02$

00$		moveq	#'?',d0
		move.b	d0,(a3)+
		move.b	d0,(a3)+
		move.b	d0,(a3)+

		move.l	d3,a0
		moveq	#SPACE,d0
		moveq	#18-1,d1
01$		move.b	d0,-(a0)
		dbf	d1,01$
02$		moveq	#-1,d0
		bra	dis_end

immediate_data	move.b	#'#',(a3)+
		move.w	d4,d0
		add.w	d0,d0
		jmp	immsizes(pc,d0.w)

immsizes	bra.s	invalid
		bra.s	imm_byte
		bra.s	imm_word
; imm_long
		bsr	GetLong
		move.b	#'$',(a3)+
		call	JUMP,puthex68a

imm_byte	bsr	GetWord
		moveq	#2,d1
		call	JUMP,put_hexnum

imm_word	bsr	GetWord
		moveq	#4,d1
		call	JUMP,put_hexnum

immoffs		bsr	GetWord
sword		ext.l	d0
		call	JUMP,put_signed_hexnum

immediate_moveq		;%%signed number
		move.b	#'#',(a3)+
		move.w	d7,d0
		ext.w	d0
		bra.s	sword

sr_or_ccr	tst.w	d4
		beq	invalid
		cmp.w	#3,d4
		beq	invalid
		cmp.w	#1,d4
		beq.s	o_ccr
		move.b	#'s',(a3)+
		bra.s	o_sccr
o_ccr		moveq	#'c',d0
		move.b	d0,(a3)+
		move.b	d0,(a3)+
o_sccr		move.b	#'r',(a3)+
		rts

shiftdir	moveq	#'r',d0
		btst	#8,d7
		beq.s	01$
		moveq	#'l',d0
01$		move.b	d0,(a3)+
		rts

regshifttype	move.w	d7,d0
		lsr.w	#3,d0
		bra.s	shifttype

memshifttype	move.w	d7,d0
		lsr.w	#8,d0
		lsr.w	#1,d0

shifttype	and.w	#3,d0
		lea	instr_names(pc),a0
		call	getnth
		bra	put_str

condcodes	move.w	d7,d1
		lsr.w	#8,d1
		and.w	#$0f,d1
		cmp.w	#2,d1
		bcc.s	02$
		move.w	d7,d0
		and.w	#$f000,d0
		cmp.w	#$6000,d0
		beq.s	02$
		moveq	#'t',d0
		tst.w	d1
		beq.s	01$
		moveq	#'f',d0
01$		move.b	d0,(a3)+
		rts

02$		add.w	d1,d1
		lea	ccodes(pc,d1.w),a0
		move.b	(a0)+,(a3)+
		move.b	(a0)+,(a3)+
		rts

ccodes		dc.b	'rasrhilscccsneeqvcvsplmigeltgtle'
		dc.b	'hslo'
		even

short_pc_offset	move.b	d7,d0
		ext.w	d0
		ext.l	d0
		add.l	a5,d0
		bra.s	put_addr

pc_offset	move.l	a5,d1
		bsr	GetWord
		ext.l	d0
		add.l	d1,d0

put_addr	tst.l	mon_HunkTypeTable(a4)
		beq.s	put_adr1a
		move.l	d0,-(sp)
		call	find_var_value
		tst.l	d0
		beq.s	put_adr1
		move.l	d0,a0

		moveq	#30,d1
		lea	var_Name(a0),a1
1$		move.b	(a1)+,(a3)+
		dbeq	d1,1$

		move.b	#'[',-1(a3)
		move.l	(sp)+,d0
		move.b	#'$',(a3)+
		call	puthex68a
		move.b	#']',(a3)+
		rts

put_adr1	move.l	(sp)+,d0
put_adr1a	move.b	#'$',(a3)+
		call	JUMP,puthex68a

trapnum		move.b	#'#',(a3)+
		move.w	d7,d0
		and.w	#$0f,d0
		moveq	#2,d1
		call	JUMP,put_hexnum

bit_instr_type	move.w	d7,d0
		lsr.w	#4,d0
		and.w	#$0c,d0
		lea	btypes(pc,d0.w),a0
01$		bra	put_str

btypes		dc.b	'tst',0,'chg',0,'clr',0,'set',0
		even

eacategories	dc.b	EA_DATA!EA_ALTER!EA_NOTIMM			; 0 - data reg direct
		dc.b	EA_ALTER!EA_NOTIMM				; 1 - address reg direct
		dc.b	EA_DATA!EA_MEMORY!EA_CONTROL!EA_ALTER!EA_NOTIMM	; 2 - address reg indirect
		dc.b	EA_DATA!EA_MEMORY!EA_ALTER!EA_NOTIMM		; 3 - addr. reg. ind. ++
		dc.b	EA_DATA!EA_MEMORY!EA_ALTER!EA_NOTIMM		; 4 - addr. reg. ind. --
		dc.b	EA_DATA!EA_MEMORY!EA_CONTROL!EA_ALTER!EA_NOTIMM	; 5 - addr. reg. ind. disp.
		dc.b	EA_DATA!EA_MEMORY!EA_CONTROL!EA_ALTER!EA_NOTIMM	; 6 - addr. reg. ind. index
		dc.b	EA_DATA!EA_MEMORY!EA_CONTROL!EA_ALTER!EA_NOTIMM	; 7 - absolute short
		dc.b	EA_DATA!EA_MEMORY!EA_CONTROL!EA_ALTER!EA_NOTIMM	; 8 - absolute long
		dc.b	EA_DATA!EA_MEMORY!EA_CONTROL!EA_NOTIMM		; 9 - pc relative
		dc.b	EA_DATA!EA_MEMORY!EA_CONTROL!EA_NOTIMM		;10 - pc relative index
		dc.b	EA_DATA!EA_MEMORY				;11 - immediate
		even

effective_address
		move.w	d7,d0
		move.w	d7,d1
		lsr.w	#3,d1
;
; mode in d1, reg in d0
;
handle_ea	and.w	#7,d0
		and.w	#7,d1
		movem.l	d0-d1,-(sp)
		cmp.w	#7,d1
		bcs.s	01$
		add.w	d0,d1
01$		cmp.b	#1,d1			address register ?
		bne.s	02$
		cmp.b	d1,d4			byte size ?
		beq.s	03$
02$		move.b	eacategories(pc,d1.w),d0
		and.b	d6,d0
		cmp.b	d6,d0
		eor	#%100,ccr		reverse zero flag
03$		movem.l	(sp)+,d0-d1		flags don't change
		beq	invalid
		add.w	d1,d1
		jmp	eajumps1(pc,d1.w)

eajumps1	bra.s	drdirect
		bra.s	ardirect
		bra.s	arindirect
		bra.s	postincr
		bra.s	predecr
		bra.s	displ1
		bra.s	indx
; handle 7-modes
		add.w	d0,d0
		jmp	eajumps2(pc,d0.w)

displ1		bra	displ

drdirect	move.b	#'d',(a3)+
		bra.s	putreg

ardirect	cmp.w	#7,d0
		beq.s	stackptr
		move.b	#'a',(a3)+
putreg		add.b	#'0',d0
		move.b	d0,(a3)+
		rts

arindirect	move.b	#'(',(a3)+
		bsr	ardirect
		bra	endpar

stackptr	move.b	#'s',(a3)+
		move.b	#'p',(a3)+
		rts

postincr	bsr	arindirect
		move.b	#'+',(a3)+
		rts

predecr		move.b	#'-',(a3)+
		bra.s	arindirect

imm_br		bra	immediate_data

eajumps2	bra.s	abs_short
		bra.s	abs_long
		bra.s	pcrel
		bra.s	pcrel_indx
		bra.s	imm_br
		bra.s	invalid_br
		nop
invalid_br	bra	invalid

indx		move.w	d0,-(sp)
		bsr	GetWord
		move.w	d0,d2
		ext.w	d0
		bsr	sword		;%% signed
		move.w	(sp)+,d0
		move.b	#'(',(a3)+
		bsr	ardirect
		move.b	#',',(a3)+
		bsr	handle_index
		bra	endpar

abs_short	bsr	GetWord		;%%%signed
		bra	sword

abs_long	bsr	GetLong
		bra	put_addr

pcrel		move.l	a5,d1
		bsr	GetWord
		ext.l	d0
		add.l	d1,d0
		bsr	put_addr
		lea	pc_txt(pc),a0
		bsr.s	put_str
		bra.s	endpar

pcrel_indx	move.l	a5,d1
		bsr	GetWord
		move.w	d0,d2
		ext.w	d0
		ext.l	d0
		add.l	d1,d0
		bsr	put_addr
		lea	pc_txt(pc),a0
		bsr	put_str
		move.b	#',',(a3)+
		bsr	handle_index
endpar		move.b	#')',(a3)+
		rts

displ		move.w	d0,-(sp)
		bsr	GetWord

		tst.l	mon_HunkTypeTable(a4)
		beq.s	8$

		move.b	5(sp),d1
		cmp.b	mon_RelBaseReg(a4),d1
		bne.s	8$

		ext.l	d0
		add.l	mon_RelBaseAddr(a4),d0

		call	find_var_value
		tst.l	d0
		beq.s	7$
		move.l	d0,a0

		moveq	#30,d1
		lea	var_Name(a0),a1
1$		move.b	(a1)+,(a3)+
		dbeq	d1,1$

		move.b	#'[',-1(a3)
		move.w	-2(a5),d0
		bsr	sword
		move.b	#']',(a3)+
		bra.s	9$

7$		move.w	-2(a5),d0

8$		bsr	sword		;%% signed
9$		move.w	(sp)+,d0
		bra	arindirect
;
;
put_str		move.b	(a0)+,(a3)+
		bne.s	put_str
		subq.l	#1,a3
		rts
;
; index word in d2
;
handle_index	move.w	d2,d0
		lsr.w	#8,d0
		lsr.w	#4,d0
		and.w	#7,d0
		tst.w	d2
		bmi.s	01$
		bsr	drdirect
		bra.s	02$
01$		bsr	ardirect
02$		move.b	#'.',(a3)+
		moveq	#'w',d0
		btst	#11,d2
		beq.s	03$
		moveq	#'l',d0
03$		move.b	d0,(a3)+
		rts

handle_move	moveq	#0,d6
		bsr	effective_address
		move.b	#',',(a3)+
		moveq	#EA_DATA!EA_ALTER,d6
		move.w	d7,d1
		lsr.w	#6,d1
		move.w	d1,d0
		lsr.w	#3,d0
		bra	handle_ea

handle_movem	bsr	GetWord		get register list in d0

		move.w	d7,d1
		and.w	#$0038,d1	get addressing mode bits in d1 (mode field)

		btst	#10,d7
		bne.s	fromMemory

		cmp.w	#$0018,d1
		beq	invalid		movem regs,(An)+ is invalid

		cmp.w	#$0020,d1	predecrement ?
		bne.s	02$

		move.w	d0,d1
		move.w	#16-1,d5	reverse register list
01$		roxr.w	#1,d1
		roxl.w	#1,d0
		dbf	d5,01$
02$		bsr.s	put_reglist
		move.b	#',',(a3)+
		bra	effective_address

fromMemory	cmp.w	#$0020,d1
		beq	invalid		;movem	-(An),regs is invalid

		move.w	d0,d5
		bsr	effective_address
		move.b	#',',(a3)+
		move.w	d5,d0
; drop to put_reglist

put_reglist	tst.l	d0
		bne.s	reglist1
		lea	nothing_txt(pc),a0
		bra	put_str

reglist1	movem.l	d3-d5,-(sp)
		move.w	d0,d2
		moveq	#0,d4		flag: registers found
		moveq	#'d',d5		register type
		bsr.s	regset
		moveq	#'a',d5
		bsr.s	regset
		movem.l	(sp)+,d3-d5
		rts

regset		moveq	#0,d3		flag: in range
		moveq	#0,d1		bit counter
rloop1		lsr.w	#1,d2
		bcc.s	nlist1
		tst.w	d3
		bne.s	inrange
		cmp.w	#7,d1
		beq.s	nostartrange
		btst	d3,d2		d3 is zero if we got here...
		beq.s	nostartrange
		tst.w	d4
		beq.s	strng1
		move.b	#'/',(a3)+
strng1		bsr.s	putd1reg
		move.b	#'-',(a3)+
		moveq	#-1,d3		now in range
		bra.s	nlist1
inrange		cmp.w	#7,d1
		beq.s	endrange
		btst	#0,d2
		bne.s	nlist1
endrange	bsr.s	putd1reg
		moveq	#0,d3		not in range
		moveq	#-1,d4		regs found
		bra.s	nlist1
nostartrange	tst.l	d4
		beq.s	no_found
		move.b	#'/',(a3)+
no_found	bsr.s	putd1reg
		moveq	#-1,d4
nlist1		addq.w	#1,d1
		cmp.w	#8,d1
		bcs.s	rloop1
		rts

putd1reg	move.b	d5,(a3)+
		move.b	d1,d0
		and.b	#7,d0
		add.b	#'0',d0
		move.b	d0,(a3)+
		rts

;
; disassembler instruction code table
;
;
; structure of the table
;  dc.w mask,value  specifies what bit pattern identifies this instruction
;  dc.b flags       addressing mode and size flags
;  str <string>     instruction "format string" (or special case routine
;		    address)
;
DisAsmTable
; abcd Dn,Dn
		dc.w	$f1f8,$c100
		dc.b	S_BYTE
		str	<t_abcd,TAB,DR2,',',DRB>
		even
; abcd -(An),-(An)
		dc.w	$f1f8,$c108
		dc.b	S_BYTE
		str	<t_abcd,TAB,'-(',AR2,'),-(',ARB,')'>
		even
; sbcd Dn,Dn
		dc.w	$f1f8,$8100
		dc.b	S_BYTE
		str	<t_sbcd,TAB,DR2,',',DRB>
		even
; sbcd -(An),-(An)
		dc.w	$f1f8,$8108
		dc.b	S_BYTE
		str	<t_sbcd,TAB,'-(',AR2,'),-(',ARB,')'>
		even
; adda.w ea,An
		dc.w	$f1c0,$d0c0
		dc.b	S_WORD		; all addressing modes allowed
		str	<t_add,'a.w',TAB,EA,',',ARB>
		even
; adda.l ea,An
		dc.w	$f1c0,$d1c0
		dc.b	S_LONG		; all addressing modes allowed
		str	<t_add,'a.l',TAB,EA,',',ARB>
		even
; suba.w ea,An
		dc.w	$f1c0,$90c0
		dc.b	S_WORD		; all addressing modes allowed
		str	<t_sub,'a.w',TAB,EA,',',ARB>
		even
; suba.l ea,An
		dc.w	$f1c0,$91c0
		dc.b	S_LONG		; all addressing modes allowed
		str	<t_sub,'a.l',TAB,EA,',',ARB>
		even
; addx Dn,Dn
		dc.w	$f138,$d100
		dc.b	0
		str	<t_add,'x',SZ,TAB,DR2,',',DRB>
		even
; addx -(An),-(An)
		dc.w	$f138,$d108
		dc.b	0
		str	<t_add,'x',SZ,TAB,'-(',AR2,'),-(',ARB,')'>
		even
; subx Dn,Dn
		dc.w	$f138,$9100
		dc.b	0
		str	<t_sub,'x',SZ,TAB,DR2,',',DRB>
		even
; subx -(An),-(An)
		dc.w	$f138,$9108
		dc.b	0
		str	<t_sub,'x',SZ,TAB,'-(',AR2,'),-(',ARB,')'>
		even
; add ea,Dn
		dc.w	$f100,$d000
		dc.b	0	; all addressing modes are legal
		str	<t_add,SZ,TAB,EA,',',DRB>
		even
; add Dn,ea
		dc.w	$f100,$d100
		dc.b	EA_DATA!EA_ALTER
		str	<t_add,SZ,TAB,DRB,',',EA>
		even
; sub ea,Dn
		dc.w	$f100,$9000
		dc.b	0	; all addressing modes are legal
		str	<t_sub,SZ,TAB,EA,',',DRB>
		even
; sub Dn,ea
		dc.w	$f100,$9100
		dc.b	EA_DATA!EA_ALTER
		str	<t_sub,SZ,TAB,DRB,',',EA>
		even
; addi #data,ea
		dc.w	$ff00,$0600
		dc.b	EA_DATA!EA_ALTER
		str	<t_add,'i',SZ,TAB,IMM,',',EA>
		even
; subi #data,ea
		dc.w	$ff00,$0400
		dc.b	EA_DATA!EA_ALTER
		str	<t_sub,'i',SZ,TAB,IMM,',',EA>
		even
; Dbcc Dn,offs
		dc.w	$f0f8,$50c8
		dc.b	S_WORD
		str	<'db',CC,TAB,DR2,',',OFFS>
		even
; Scc
		dc.w	$f0c0,$50c0
		dc.b	S_BYTE!EA_DATA!EA_ALTER
		str	<'s',CC,TAB,EA>
		even
; addq #data,ea
		dc.w	$f100,$5000
		dc.b	EA_ALTER
		str	<t_add,'q',SZ,TAB,IMMQ,',',EA>
		even
; subq #data,ea
		dc.w	$f100,$5100
		dc.b	EA_ALTER
		str	<t_sub,'q',SZ,TAB,IMMQ,',',EA>
		even
; cmpa.w ea,An
		dc.w	$f1c0,$b0c0
		dc.b	S_WORD	; all addressing modes are legal
		str	<t_cmp,'a.w',TAB,EA,',',ARB>
		even
; cmpa.l ea,An
		dc.w	$f1c0,$b1c0
		dc.b	S_LONG	; all addressing modes are legal
		str	<t_cmp,'a.l',TAB,EA,',',ARB>
		even
; cmpm (An)+,(An)+
		dc.w	$f138,$b108
		dc.b	0
		str	<t_cmp,'m',SZ,TAB,'(',AR2,')+,(',ARB,')+'>
		even
; cmp ea,Dn
		dc.w	$f100,$b000
		dc.b	0	; all addressing modes are legal
		str	<t_cmp,SZ,TAB,EA,',',DRB>
		even
; cmpi #imm,ea
		dc.w	$ff00,$0c00
		dc.b	EA_DATA!EA_ALTER		;%%68020?
		str	<t_cmp,'i',SZ,TAB,IMM,',',EA>
		even
; divs
		dc.w	$f1c0,$81c0
		dc.b	EA_DATA!S_WORD
		str	<t_div,'s',TAB,EA,',',DRB>
		even
; divu
		dc.w	$f1c0,$80c0
		dc.b	EA_DATA!S_WORD
		str	<t_div,'u',TAB,EA,',',DRB>
		even
; muls
		dc.w	$f1c0,$c1c0
		dc.b	EA_DATA!S_WORD
		str	<t_mul,'s',TAB,EA,',',DRB>
		even
; mulu
		dc.w	$f1c0,$c0c0
		dc.b	EA_DATA!S_WORD
		str	<t_mul,'u',TAB,EA,',',DRB>
		even
; exg Dn,Dn
		dc.w	$f1f8,$c140
		dc.b	0
		str	<t_exg,TAB,DR2,',',DRB>
		even
;
; my documentation has a bug here. thanks to John van Dijk for informing
; me about it...
;
; exg Dn,An
		dc.w	$f1f8,$c188
		dc.b	S_LONG
		str	<t_exg,TAB,DRB,',',AR2>
		even
; exg An,An
		dc.w	$f1f8,$c148
		dc.b	S_LONG
		str	<t_exg,TAB,AR2,',',ARB>
		even
; and ea,Dn
		dc.w	$f100,$c000
		dc.b	EA_DATA
		str	<t_and,SZ,TAB,EA,',',DRB>
		even
; and Dn,ea
		dc.w	$f100,$c100
		dc.b	EA_DATA!EA_MEMORY!EA_ALTER
		str	<t_and,SZ,TAB,DRB,',',EA>
		even
; or ea,Dn
		dc.w	$f100,$8000
		dc.b	EA_DATA
		str	<t_or,SZ,TAB,EA,',',DRB>
		even
; or Dn,ea
		dc.w	$f100,$8100
		dc.b	EA_DATA!EA_MEMORY!EA_ALTER
		str	<t_or,SZ,TAB,DRB,',',EA>
		even
; eor Dn,ea
		dc.w	$f100,$b100
		dc.b	EA_DATA!EA_ALTER
		str	<t_eor,SZ,TAB,DRB,',',EA>
		even
; andi #data,sr/ccr
		dc.w	$ff3f,$023c
		dc.b	0
		str	<t_and,'i',TAB,IMM,',',SCCR>
		even
; ori #data,sr/ccr
		dc.w	$ff3f,$003c
		dc.b	0
		str	<t_or,'i',TAB,IMM,',',SCCR>
		even
; eori #data,sr/ccr
		dc.w	$ff3f,$0a3c
		dc.b	0
		str	<t_eor,'i',TAB,IMM,',',SCCR>
		even
; andi #data,ea
		dc.w	$ff00,$0200
		dc.b	EA_DATA!EA_ALTER
		str	<t_and,'i',SZ,TAB,IMM,',',EA>
		even
; ori #data,ea
		dc.w	$ff00,$0000
		dc.b	EA_DATA!EA_ALTER
		str	<t_or,'i',SZ,TAB,IMM,',',EA>
		even
; eori #data,ea
		dc.w	$ff00,$0a00
		dc.b	EA_DATA!EA_ALTER
		str	<t_eor,'i',SZ,TAB,IMM,',',EA>
		even
; shift? ea
		dc.w	$f8c0,$e0c0
		dc.b	EA_DATA!EA_MEMORY!EA_ALTER!S_WORD
		str	<STP2,SD,TAB,EA>
		even
; shift? Dn,Dn
		dc.w	$f020,$e020
		dc.b	0
		str	<STP1,SD,SZ,TAB,DRB,',',DR2>
		even
; shift? #imm,Dn
		dc.w	$f020,$e000
		dc.b	0
		str	<STP1,SD,SZ,TAB,IMMQ,',',DR2>
		even
; Bcc addr
		dc.w	$f0ff,$6000
		dc.b	S_WORD
		str	<'b',CC,TAB,OFFS>
		even
; Bcc.s addr
		dc.w	$f000,$6000
		dc.b	S_BYTE
		str	<'b',CC,'.s',TAB,SOFFS>
		even
; movep.w offs(An),Dn
		dc.w	$f1f8,$0108
		dc.b	S_WORD
		str	<t_move,'p.w',TAB,IMMOFFS,'(',AR2,'),',DRB>
		even
; movep.l offs(An),Dn
		dc.w	$f1f8,$0148
		dc.b	S_LONG
		str	<t_move,'p.l',TAB,IMMOFFS,'(',AR2,'),',DRB>
		even
; movep.w Dn,offs(An)
		dc.w	$f1f8,$0188
		dc.b	S_WORD
		str	<t_move,'p.w',TAB,DRB,',',IMMOFFS,'(',AR2,')'>
		even
; movep.l Dn,offs(An)
		dc.w	$f1f8,$01c8
		dc.b	S_LONG
		str	<t_move,'p.l',TAB,DRB,',',IMMOFFS,'(',AR2,')'>
		even
;
; btst has more legal addressing modes than other b??? instructions
; %%note that even btst Dn,#imm is legal (with size of BYTE).
;
; btst Dn,ea
		dc.w	$f1c0,$0100
		dc.b	EA_DATA!S_BYTE
		str	<'b',BIT,TAB,DRB,',',EA>
		even
; btst #imm,ea
		dc.w	$ffc0,$0800
		dc.b	EA_DATA!EA_NOTIMM
		str	<'b',BIT,TAB,'#',IMMOFFS,',',EA>
		even
; b??? Dn,ea
		dc.w	$f100,$0100
		dc.b	EA_DATA!EA_ALTER
		str	<'b',BIT,TAB,DRB,',',EA>
		even
; b??? #imm,ea
		dc.w	$ff00,$0800
		dc.b	EA_DATA!EA_ALTER
		str	<'b',BIT,TAB,'#',IMMOFFS,',',EA>
		even
; chk ea,Dn
		dc.w	$f1c0,$4180
		dc.b	EA_DATA!S_WORD
		str	<t_chk,TAB,EA,',',DRB>
		even
; clr ea
		dc.w	$ff00,$4200
		dc.b	EA_DATA!EA_ALTER
		str	<t_clr,SZ,TAB,EA>
		even
; nbcd ea
		dc.w	$ffc0,$4800
		dc.b	EA_DATA!EA_ALTER!S_BYTE
		str	<t_nbcd,TAB,EA>
		even
; ext.w Dn
		dc.w	$fff8,$4880
		dc.b	S_WORD
		str	<t_ext,'.w',TAB,DR2>
		even
; ext.l Dn
		dc.w	$fff8,$48c0
		dc.b	S_LONG
		str	<t_ext,'.l',TAB,DR2>
		even
; jmp ea
		dc.w	$ffc0,$4ec0
		dc.b	EA_CONTROL
		str	<t_jmp,TAB,EA>
		even
; jsr ea
		dc.w	$ffc0,$4e80
		dc.b	EA_CONTROL
		str	<t_jsr,TAB,EA>
		even
; lea ea,An
		dc.w	$f1c0,$41c0
		dc.b	EA_CONTROL
		str	<t_lea,TAB,EA,',',ARB>
		even
; link An,#imm
		dc.w	$fff8,$4e50
		dc.b	S_WORD
		str	<t_link,TAB,AR2,',#',IMMOFFS>
		even
; unlk An
		dc.w	$fff8,$4e58
		dc.b	0
		str	<t_unlk,TAB,AR2>
		even
; illegal
		dc.w	$ffff,$4afc
		dc.b	0
		str	<t_illegal>
		even
; tas ea
		dc.w	$ffc0,$4ac0
		dc.b	EA_DATA!EA_ALTER!S_BYTE
		str	<t_tas,TAB,EA>
		even
; move usp,An
		dc.w	$fff8,$4e68
		dc.b	S_LONG
		str	<t_move,TAB,'usp,',AR2>
		even
; move An,usp
		dc.w	$fff8,$4e60
		dc.b	S_LONG
		str	<t_move,TAB,AR2,',usp'>
		even
; movea.w ea,An
		dc.w	$f1c0,$3040
		dc.b	S_WORD
		str	<t_move,'a.w',TAB,EA,',',ARB>
		even
; movea.l ea,An
		dc.w	$f1c0,$2040
		dc.b	S_LONG
		str	<t_move,'a.l',TAB,EA,',',ARB>
		even
; move.b ea,ea
		dc.w	$f000,$1000
		dc.b	S_BYTE!SPECIAL_CASE
		dc.b	6
		rw	handle_move
		dc.b	t_move,'.b',TAB
		even
; move.w ea,ea
		dc.w	$f000,$3000
		dc.b	S_WORD!SPECIAL_CASE
		dc.b	6
		rw	handle_move
		dc.b	t_move,'.w',TAB
		even
; move.l ea,ea
		dc.w	$f000,$2000
		dc.b	S_LONG!SPECIAL_CASE
		dc.b	6
		rw	handle_move
		dc.b	t_move,'.l',TAB
		even
; movem.w from memory
		dc.w	$ffc0,$4c80
		dc.b	EA_MEMORY!EA_NOTIMM!S_WORD!SPECIAL_CASE
		dc.b	7
		rw	handle_movem
		dc.b	t_move,'m.w',TAB
		even
; movem.w to memory
		dc.w	$ffc0,$4880
		dc.b	EA_MEMORY!EA_ALTER!S_WORD!SPECIAL_CASE
		dc.b	7
		rw	handle_movem
		dc.b	t_move,'m.w',TAB
		even
; movem.l from memory
		dc.w	$ffc0,$4cc0
		dc.b	EA_MEMORY!EA_NOTIMM!S_LONG!SPECIAL_CASE
		dc.b	7
		rw	handle_movem
		dc.b	t_move,'m.l',TAB
		even
; movem.l to memory
		dc.w	$ffc0,$48c0
		dc.b	EA_MEMORY!EA_ALTER!S_LONG!SPECIAL_CASE
		dc.b	7
		rw	handle_movem
		dc.b	t_move,'m.l',TAB
		even
; moveq #imm,Dn
		dc.w	$f100,$7000
		dc.b	S_LONG
		str	<t_move,'q',TAB,IMVQ,',',DRB>
		even
; move ea,ccr
		dc.w	$ffc0,$44c0
		dc.b	EA_DATA!S_BYTE
		str	<t_move,TAB,EA,',ccr'>
		even
; move ea,SR
		dc.w	$ffc0,$46c0
		dc.b	EA_DATA!S_WORD
		str	<t_move,TAB,EA,',sr'>
		even
; move sr,ea
		dc.w	$ffc0,$40c0
		dc.b	EA_DATA!EA_ALTER!S_WORD
		str	<t_move,TAB,'sr,',EA>
		even
; neg ea
		dc.w	$ff00,$4400
		dc.b	EA_DATA!EA_ALTER
		str	<t_neg,SZ,TAB,EA>
		even
; negx ea
		dc.w	$ff00,$4000
		dc.b	EA_DATA!EA_ALTER
		str	<t_neg,'x',SZ,TAB,EA>
		even
; not ea
		dc.w	$ff00,$4600
		dc.b	EA_DATA!EA_ALTER
		str	<t_not,SZ,TAB,EA>
		even
; swap Dn
		dc.w	$fff8,$4840
		dc.b	0
		str	<t_swap,TAB,DR2>
		even
; pea ea
		dc.w	$ffc0,$4840
		dc.b	EA_CONTROL
		str	<t_pea,TAB,EA>
		even
; reset
		dc.w	$ffff,$4e70
		dc.b	0
		str	<t_reset>
		even
; nop
		dc.w	$ffff,$4e71
		dc.b	0
		str	<t_nop>
		even
; stop
		dc.w	$ffff,$4e72
		dc.b	S_WORD
		str	<t_stop,TAB,IMM>
		even
; rte
		dc.w	$ffff,$4e73
		dc.b	0
		str	<t_rte>
		even
; rts
		dc.w	$ffff,$4e75
		dc.b	0
		str	<t_rts>
		even
; trapv
		dc.w	$ffff,$4e76
		dc.b	0
		str	<t_trap,'v'>
		even
; rtr
		dc.w	$ffff,$4e77
		dc.b	0
		str	<t_rtr>
		even
; trap
		dc.w	$fff0,$4e40
		dc.b	0
		str	<t_trap,TAB,TRAPN>
		even
; tst ea
		dc.w	$ff00,$4a00
		dc.b	EA_DATA!EA_ALTER	;%%68020?
		str	<t_tst,SZ,TAB,EA>
		even
; line-a
		dc.w	$f000,$a000
		dc.b	0
		str	<'line-a'>
		even
; line-f
		dc.w	$f000,$f000
		dc.b	0
		str	<'line-f'>
		even
; end of table marker
		dc.w	0,0		matches anything
		dc.b	SPECIAL_CASE
		dc.b	3
		rw	invalid
		dc.b	0

*** INSTRUCTION NAMES ***
instr_names	dc.b	'as',0
		dc.b	'ls',0
		dc.b	'rox',0
		dc.b	'ro',0
		dc.b	'move',0
		dc.b	'add',0
		dc.b	'sub',0
		dc.b	'and',0
		dc.b	'or',0
		dc.b	'abcd',0
		dc.b	'sbcd',0
		dc.b	'mul',0
		dc.b	'div',0
		dc.b	'exg',0
		dc.b	'eor',0
		dc.b	'cmp',0
		dc.b	'btst',0
		dc.b	'bchg',0
		dc.b	'bclr',0
		dc.b	'bset',0
		dc.b	'chk',0
		dc.b	'lea',0
		dc.b	'ext',0
		dc.b	'clr',0
		dc.b	'neg',0
		dc.b	'not',0
		dc.b	'tst',0
		dc.b	'nbcd',0
		dc.b	'swap',0
		dc.b	'pea',0
		dc.b	'link',0
		dc.b	'unlk',0
		dc.b	'reset',0
		dc.b	'nop',0
		dc.b	'stop',0
		dc.b	'rte',0
		dc.b	'tas',0
		dc.b	'rts',0
		dc.b	'trapv',0
		dc.b	'rtr',0
		dc.b	'jsr',0
		dc.b	'jmp',0
		dc.b	'trap',0
		dc.b	'illegal',0,0

		end
