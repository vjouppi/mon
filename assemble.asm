;
; assemble.asm
;

		include	"monitor.i"

		xref	ccodes
		xref	instr_names

		xref	UpAndClearEol

		xref	getstring

		xref	generic_error
		xref	odd_address_error
		xref	out_range_error
		xref	addrmode_error

*** ASSEMBLE ***
* command format:
* a       :assembles at the current address, asks instruction
* a <addr>:assembles at <addr>, asks instruction
* a <addr> <instruction> : assembles <instruction> at <addr>
* in all cases, if no errors occurred, prompts '<next_addr>:'
* and asks a new instruction. use <CR> to exit this mode
* Ctrl-E can be used to edit the disassembled instruction at this location

		cmd	assemble

		call	skipspaces
		tst.b	(A3)
		bne.s	assem_01
		move.l	Addr(a4),D0
		addq.l	#1,d0
		and.b	#$fe,d0		;round to next even addr - /1.31/
		move.l	d0,Addr(a4)
		bra.s	assem_02
assem_01	call	get_expr
		btst	#0,D0
		bne	odd_address_error	;assembling to odd address is illegal
		move.l	D0,Addr(a4)
assem_02	move.l	D0,EndAddr(a4)
		call	skipspaces
		tst.b	(A3)
		bne.s	assem1a

assem1		move.l	Addr(a4),D0
		move.l	D0,EndAddr(a4)
		lea	assemfmt(pc),a0
		call	printf_window
		moveq	#1,D0

assem1a0	call	GetInput
		tst.w	D0		;check for Ctrl-E (GetInput returns -1)
		bpl.s	assem1a1	;branch if not Ctrl-E

*** disassemble at current address and put result in input buffer ***
		move.l	Addr(a4),a5
		startline
		call	disassemble
		lea	InputBuf(a4),a1
		moveq	#LF,D0
300$		move.b	(A0)+,(A1)+	copy instruction to input buffer
		cmp.b	(A0),D0 	until LF found
		bne.s	300$
		clr.b	(A1)		mark end of string
		moveq	#2,D0		GetInput mode: edit existing line
		bra.s	assem1a0
assem1a1	tst.b	(A3)
		bne.s	assem1a
		rts

assem1a		lea	instr_names(pc),a0
		call	find_name
		tst.l	d0
		bmi	try_branch

;
; instruction found from list, execute its assembly routine...
;
		move.l	A1,A3
		add.w	D0,D0
		lea	instrjumps(pc),A0
		add.w	D0,A0
		add.w	(A0),A0
		jmp	(A0)

	;if instruction not found in table, it can be
	;a branch, DBcc, Scc or dc.?
try_branch	move.b	(a3)+,d0
		call	tolower
		cmp.b	#'b',d0
		bne.s	try_Scc
		bsr	check_cond
		tst.w	D1
		bpl.s	branch_1
br_err		bra	generic_error

branch_1	lsl.w	#8,D1
		or.w	#$6000,D1
		cmp.b	#'.',(A3)
		bne.s	long_branch
		addq.l	#1,A3
		move.b	(a3)+,d0
		call	tolower
		cmp.b	#'l',d0
		beq.s	long_branch
		cmp.b	#'s',d0
		bne.s	br_err
		call	get_expr	;short branch
		sub.l	Addr(a4),D0
		subq.l	#2,D0
		move.b	D0,D2
		ext.w	D2
		ext.l	D2
		cmp.l	D0,D2
		bne	out_range_error
		and.w	#$FF,D0		short branch can't branch zero bytes
		beq.s	br_err
		or.w	D1,D0
		bra	one_word_instr

long_branch	call	get_expr
		sub.l	Addr(a4),D0
		subq.l	#2,D0
		move.w	D0,D2
		ext.l	D2
		cmp.l	D0,D2
		bne	out_range_error
		exg	D0,D1
		bra	two_words_instr

try_Scc		cmp.b	#'s',d0
		bne.s	try_DBcc
		move.b	(a3)+,d0
		call	tolower
		cmp.b	#'f',d0
		bne.s	Scc_1
		moveq	#1,D1
		bra.s	Scc_3
Scc_1		cmp.b	#'t',d0
		bne.s	Scc_2
		moveq	#0,D1
		bra.s	Scc_3
Scc_2		subq.l	#1,A3
		bsr	check_cond
		tst.w	D1
		bmi.s	cc_err
		cmp.w	#2,D1
		bcs.s	cc_err
Scc_3		move.w	D1,D3
		lsl.w	#8,D3
		addq.l	#2,Addr(a4)
		bsr	Get_Dest_EA
		or.w	D3,D0
		or.w	#$50C0,D0
		bra	set_instr_word

cc_err		bra	generic_error

try_DBcc	cmp.b	#'d',d0
		bne.s	cc_err
		move.b	(a3)+,d0
		call	tolower
		cmp.b	#'b',d0
		bne.s	try_dc
		move.b	(a3)+,d0
		call	tolower
		cmp.b	#'t',d0
		bne.s	DBcc_1
		moveq	#0,D1
		bra.s	DBcc_3
DBcc_1		cmp.b	#'f',d0
		bne.s	DBcc_2
DBcc_1a		moveq	#1,D1
		bra.s	DBcc_3
DBcc_2		subq.l	#1,A3
		bsr	check_cond
		tst.w	D1
		beq.s	DBcc_1a
		bmi.s	cc_err
		cmp.w	#1,D1
		beq.s	cc_err
DBcc_3		move.w	D1,D3
		call	skipspaces
		bsr	getdreg
		move.w	d1,d0
		lsl.w	#8,D3
		or.w	D0,D3
		or.w	#$50C8,D3
		bsr	SkipComma
		call	get_expr	
		sub.l	Addr(a4),D0
		subq.l	#2,D0
		move.l	D0,D1
		ext.l	D0
		cmp.l	D0,D1
		bne	out_range_error
		move.w	D3,D0
		bra	two_words_instr

;
; define constant byte/word/longword
;
try_dc		cmp.b	#'c',d0
		bne.s	cc_err
		bsr	GetSize	;*** DC.B, DC.W, DC.L ***
		call	skipspaces
		move.l	Addr(a4),A0
		move.w	size(a4),D0
		beq.s	dc_byte
		subq.w	#1,D0
		beq.s	dc_word
dc_long		call	get_expr
		move.l	D0,(A0)+
		call	skipspaces
		cmp.b	#COMMA,(a3)+
		beq.s	dc_long
		bra.s	dc_exit
dc_word		call	get_expr
		move.w	D0,(A0)+
		call	skipspaces
		cmp.b	#COMMA,(a3)+
		beq.s	dc_word
		bra.s	dc_exit
dc_byte		bsr	getstring
		move.l	A1,A0
dc_exit		move.l	A0,D0	;align address to word boundary
		btst	#0,D0
		beq.s	dc_exit1
		addq.l	#1,D0

dc_exit1	move.l	D0,Addr(a4)
		bra.s	dc_exit2

assem9		call	skipspaces
		tst.b	(a3)
		bne	generic_error

		lea	UpAndClearEol(pc),A0	;move cursor to previous line and clear the line
		call	printstring_a0_window
		move.l	EndAddr(a4),a5
		startline
		call	disassemble
		call	printstring_window
dc_exit2	bra	assem1

; check if the chars at A3 form a condition code
; result returned in D1, condition number or -1 if not found
; now unserstands hs and lo-conditions (same as cc and cs)
check_cond	move.b	(A3)+,D0
		call	tolower
		lsl.w	#8,D0
		move.b	(A3)+,D0
		call	tolower
		lea	ccodes(pc),A0
		moveq	#0,D1

chk_cond1	cmp.w	(A0)+,D0
		beq.s	cond9
		addq.w	#1,D1
		cmp.w	#18,D1
		bcs.s	chk_cond1
		moveq	#-1,D1
cond9		cmp.w	#16,d1
		bcs.s	cond10
		sub.w	#12,d1
cond10		rts

*** SHIFT INSTRUCTIONS ***
as_asm		moveq	#$00,D3
		bra.s	shift_instr

ls_asm		moveq	#$08,D3
		bra.s	shift_instr

rox_asm		moveq	#$10,D3
		bra.s	shift_instr

rot_asm		moveq	#$18,D3

shift_instr	or.w	#$e000,d3
		move.b	(a3)+,d0
		call	tolower
		cmp.b	#'r',d0			;get direction
		beq.s	shift_1
		cmp.b	#'l',d0
		bne.s	sh_err
		bset	#8,D3			;left shift
shift_1		cmp.b	#'.',(A3)
		bne.s	shift_mem
		bsr	GetSize
		move.w	size(a4),D0
		lsl.w	#6,D0
		or.w	D0,D3
		call	skipspaces
		cmp.b	#'#',(A3)
		bne.s	count_in_reg
		addq.l	#1,A3
		bsr	get_expr_1_8
		bra.s	shift_2

count_in_reg	bsr	getdreg
		move.w	d1,d0
		bset	#5,D3
shift_2		lsl.w	#8,D0
		lsl.w	#1,D0
		or.w	D0,D3
		bsr	SkipComma
		bsr	getdreg
		move.w	d1,d0
		or.w	D3,D0
		bra	one_word_instr

sh_err		bra	generic_error

shift_mem	addq.l	#2,Addr(a4)
		moveq	#0,D0
		move.b	D3,D0
		lsl.w	#6,D0
		or.w	D0,D3
		and.w	#$F7C0,D3
		or.w	#$C0,D3
		bsr	Get_Dest_EA
		move.w	d0,d1
		and.w	#$38,d1
		beq	addrmode_error
		or.w	D3,D0
		bra	set_instr_word

*** ADD & SUB ***
add_asm		move.w	#$D000,D3
		bra.s	a_s_asm

sub_asm		move.w	#$9000,D3

a_s_asm		move.b	(a3),d0
		call	tolower
		cmp.b	#'x',d0
		bne.s	no_as_ext
		addq.l	#1,A3
		bsr	GetSize
		bset	#8,D3
		bra	ext_as_asm

no_as_ext	addq.l	#2,Addr(a4)
		move.b	(a3),d0
		call	tolower
		cmp.b	#'q',d0
		bne.s	no_as_quick
		addq.l	#1,A3
		bsr	GetSize
		call	skipspaces
		cmp.b	#'#',(A3)+
		bne	addrmode_error
		bsr	get_expr_1_8
		lsl.w	#8,D0
		lsl.w	#1,D0
		cmp.w	#$9000,D3
		bne.s	as_quick_1
		bset	#8,D0

as_quick_1	or.w	#$5000,D0
		move.w	size(a4),D1
		lsl.w	#6,D1
		or.w	D1,D0
		move.w	D0,D3
		bsr	SkipComma
		bsr	GetEA
		tst.w	size(a4)
		bne.s	as_quick_z1
		cmp.w	#1,D1
		beq.s	as_err

;#
;# fixed addq/subq addressing mode checking 1989-08-28
;#
as_quick_z1	bsr	Do_EA_1
		or.w	D3,D0
zcom_1		bra	set_instr_word

as_err		bra	generic_error

no_as_quick	move.b	(a3),d0
		call	tolower
		cmp.b	#'i',d0
		bne.s	no_as_imm
		addq.l	#1,A3
		bsr	GetSize
		call	skipspaces
		cmp.b	#'#',(A3)+
		bne	addrmode_error

as_imm_1	bsr	PutImm
		bsr	SkipComma
		bsr	GetEA
		cmp.w	#1,D1
		beq.s	as_addr_imm_1
		bsr	Do_EA_1
		move.w	size(a4),D1
		lsl.w	#6,D1
		or.w	D1,D0
		bset	#10,D0
		cmp.w	#$9000,D3
		beq.s	zcom_1
		bset	#9,D0
		bra.s	zcom_1

as_addr_imm_1	lsl.w	#8,D0
		lsl.w	#1,D0
		move.w	size(a4),D1
		beq.s	as_err
		subq.w	#1,D1
		lsl.w	#8,D1
		or.w	D1,D0
		or.w	#$FC,D0
		or.w	D3,D0
		bra.s	zcom_1

no_as_imm	moveq	#0,D5
		move.b	(a3),d0
		call	tolower
		cmp.b	#'a',d0
		bne.s	as_norm_1
		moveq	#-1,D5
		addq.l	#1,A3

as_norm_1	bsr	GetSize
		call	skipspaces
		tst.l	D5
		bne.s	as_norm_2
		cmp.b	#'#',(A3)+
		beq.s	as_imm_1
		subq.l	#1,A3

as_norm_2	move.b	(a3),d0
		call	tolower
		cmp.b	#'d',d0
		beq.s	as_data_reg_source
		bsr	GetEA
		tst.w	size(a4)
		bne.s	as_norm_3
		cmp.w	#1,D1		;address register direct can't be used
		beq	addrmode_error	;with byte size

as_norm_3	lsl.w	#3,D1
		or.w	D1,D0
		or.w	D0,D3
		bsr	SkipComma
		move.b	(a3),d0
		call	tolower
		cmp.b	#'d',d0
		bne.s	try_as_addr
		tst.l	D5
		bne.s	as_err2
		bsr	getdreg
		move.w	d1,d0
		lsl.w	#8,D0
		lsl.w	#1,D0
		or.w	D3,D0
		move.w	size(a4),D1
		lsl.w	#6,D1
		or.w	D1,D0
		bra.s	zcom_2
as_err2		bra	generic_error

try_as_addr	bsr	getareg
		move.w	d1,d0
		lsl.w	#8,D0
		lsl.w	#1,D0
		or.w	D3,D0
		or.w	#$C0,D0
		move.w	size(a4),D1
		beq.s	as_err2
		subq.w	#1,D1
		lsl.w	#8,D1
		or.w	D1,D0
zcom_2		bra	set_instr_word

as_data_reg_source
		bsr	getdreg
		lsl.w	#8,D1
		lsl.w	#1,D1
		or.w	D1,D3
		bsr	SkipComma
		bsr	GetEA
		tst.w	D1
		bne.s	as_datasource_1
		tst.l	d5
		bne.s	as_err2
		lsl.w	#8,D0
		lsl.w	#1,D0
		move.w	D3,D1
		and.w	#$F000,D3
		lsr.w	#8,D1
		lsr.w	#1,D1
		and.w	#7,D1
		or.w	D3,D0
		or.w	D1,D0
		bra.s	as_ds_1

as_datasource_1	cmp.w	#1,D1
		beq.s	as_special
		tst.l	d5
		bne.s	as_err2
		bsr	Do_EA_1
		or.w	D3,D0
		bset	#8,D0
as_ds_1		move.w	size(a4),D1
		lsl.w	#6,D1
		or.w	D1,D0
		bra.s	zcom_2

as_special	;handle adda/suba
		lsl.w	#8,D0
		lsl.w	#1,D0
		move.w	D3,D1
		and.w	#$F000,D3
		or.w	D3,D0
		lsr.w	#8,D1
		lsr.w	#1,D1
		and.w	#7,D1
		or.w	D1,D0
		move.w	size(a4),D1
		beq.s	cmp_err
		subq.w	#1,D1
		lsl.w	#8,D1
		or.w	D1,D0
		or.w	#$C0,D0
		bra	set_instr_word

*** CMP ***
cmp_asm		move.b	(a3),d0
		call	tolower
		cmp.b	#'m',d0
		bne.s	no_mem_cmp
		addq.l	#1,A3
		bsr	GetSize	;CMPM
		call	skipspaces
		cmp.b	#'(',(A3)+
		bne.s	cmp_err
		bsr	getareg
		move.w	d1,d3
		cmp.b	#')',(A3)+
		bne.s	cmp_err
		cmp.b	#'+',(A3)+
		bne.s	cmp_err
		bsr	SkipComma
		cmp.b	#'(',(A3)+
		bne.s	cmp_err
		bsr	getareg
		move.w	d1,d0
		cmp.b	#')',(A3)+
		bne.s	cmp_err
		cmp.b	#'+',(A3)+
		bne.s	cmp_err
		lsl.w	#8,D0
		lsl.w	#1,D0
		or.w	D3,D0
;#
;# cmpm size specifiers fixed 1989-08-28
;#
		move.w	size(a4),d1
		lsl.w	#6,d1
		or.w	d1,d0
		or.w	#$B108,D0
		bra	one_word_instr

cmp_err		bra	generic_error

no_mem_cmp	addq.l	#2,Addr(a4)
		move.b	(a3),d0
		call	tolower
		cmp.b	#'i',d0
		bne.s	no_cmp_imm
		addq.l	#1,A3
		bsr	GetSize
		call	skipspaces
		cmp.b	#'#',(A3)+
		bne	addrmode_error

cmp_imm_1	bsr	PutImm
		bsr	SkipComma
		bsr	GetEA
		cmp.w	#1,D1
		beq.s	cmp_addr_imm1
		bsr	Do_EA_1
		move.w	size(a4),D1
		lsl.w	#6,D1
		or.w	D1,D0
		or.w	#$0C00,D0
		bra	set_instr_word

cmp_addr_imm1	lsl.w	#8,D0
		lsl.w	#1,D0
		move.w	size(a4),D1
		beq.s	cmp_err
		subq.w	#1,D1
		lsl.w	#8,D1
		or.w	D1,D0
		or.w	#$B0FC,D0
		bra	set_instr_word

no_cmp_imm	moveq	#0,D5
		move.b	(a3),d0
		call	tolower
		cmp.b	#'a',d0
		bne.s	no_cmp_addr
		addq.l	#1,A3
		moveq	#-1,D5

no_cmp_addr	bsr	GetSize
		call	skipspaces
		tst.l	D5
		bne.s	cmp_1
		cmp.b	#'#',(A3)+
		beq.s	cmp_imm_1
		subq.l	#1,A3
cmp_1		bsr	GetEA
		cmp.w	#1,D1
		bne.s	cmp_2
		tst.w	size(a4)
		beq.s	cmp_err2
cmp_2		lsl.w	#3,D1
		or.w	D1,D0
		move.w	D0,D3
		bsr	SkipComma
		bsr	getreg
		btst	#3,D1
		bne.s	cmp_addr
		tst.l	D5
		bne.s	cmp_err2
		move.w	size(a4),D0
		lsl.w	#6,D0
		or.w	D3,D0
		and.w	#7,D1
		lsl.w	#8,D1
		lsl.w	#1,D1
		or.w	D1,D0
		or.w	#$B000,D0
		bra	set_instr_word

cmp_err2	bra	generic_error

cmp_addr	move.w	size(a4),D0
		beq.s	cmp_err2
		subq.w	#1,D0
		lsl.w	#8,D0
		or.w	D3,D0
		and.w	#7,D1
		lsl.w	#8,D1
		lsl.w	#1,D1
		or.w	D1,D0
		or.w	#$B0C0,D0
		bra	set_instr_word

*** AND & OR ***
and_asm		move.w	#$C000,D3
		bra.s	a_o_asm

or_asm		move.w	#$8000,D3

a_o_asm		addq.l	#2,Addr(a4)
		move.b	(a3),d0
		call	tolower
		cmp.b	#'i',d0
		bne.s	a_o_2
		addq.l	#1,A3
		cmp.b	#'.',(A3)
		beq.s	a_o_0
		lsr.w	#5,D3
		and.w	#$0200,D3
		bra	logical_status

a_o_0		bsr	GetSize
		call	skipspaces
		cmp.b	#'#',(A3)+
		bne	addrmode_error
a_o_imm		bsr	PutImm
		bsr	SkipComma
		bsr	Get_Dest_EA
		move.w	size(a4),D1
		lsl.w	#6,D1
		or.w	D1,D0
		cmp.w	#$C000,D3
		bne.s	zcom_3
		bset	#9,D0
		bra.s	zcom_3

a_o_2		bsr	GetSize
		call	skipspaces
		cmp.b	#'#',(A3)+
		beq.s	a_o_imm
		move.w	size(a4),D0
		lsl.w	#6,D0
		or.w	D0,D3
		subq.l	#1,A3
		move.b	(a3),d0
		call	tolower
		cmp.b	#'d',d0
		bne.s	reg_dest
		bsr	getdreg
		lsl.w	#8,D1
		lsl.w	#1,D1
		or.w	D1,D3
		bsr	SkipComma
		bsr	GetEA
		tst.w	D1
		beq.s	a_o_3a
		cmp.w	#1,D1
		beq	addrmode_error
		bsr	Do_EA_1
		bset	#8,D0
		or.w	D3,D0
zcom_3		bra	set_instr_word

a_o_3a		move.w	D0,D7
		move.w	D3,D0
		lsr.w	#8,D0
		lsr.w	#1,D0
		and.w	#7,D0
		and.w	#$F0C0,D3
		lsl.w	#8,D7
		lsl.w	#1,D7
		or.w	D7,D3
		lsl.w	#3,D1
		or.w	D1,D0
		or.w	D3,D0
		bra.s	zcom_3

reg_dest	bsr	GetEA
		cmp.w	#1,D1
		beq	addrmode_error
		lsl.w	#3,D1
		or.w	D1,D0
;# this check prevented the use of pcrelative addressing modes
;# with and/or - fixed 1989-08-27
;#	cmp.w	#$39,D0
;#	bhi.s	a_o_err2
		or.w	D0,D3
		bsr	SkipComma
		bsr	getdreg
		move.w	d1,d0
		lsl.w	#8,D0
		lsl.w	#1,D0
		or.w	D3,D0
		bra.s	zcom_3

*** ABCD & SBCD & ADDX & SUBX ***
abcd_asm	move.w	#$C100,D3
		bra.s	bcd_asm

sbcd_asm	move.w	#$8100,D3

bcd_asm		clr.w	size(a4)
ext_as_asm	call	skipspaces
		move.b	(a3),d0
		call	tolower
		cmp.b	#'d',d0
		bne.s	bcd_mem
		bsr	getdreg
		or.w	D1,D3
		bsr	SkipComma
		bsr	getdreg
bcd_1		lsl.w	#8,D1
		lsl.w	#1,D1
		or.w	D3,D1
		move.w	size(a4),D0
		lsl.w	#6,D0
		or.w	D1,D0
		bra	one_word_instr

bcd_mem		cmp.b	#'-',(A3)+
		bne.s	err_bcd
		cmp.b	#'(',(A3)+
		bne.s	err_bcd
		bsr	getareg
		or.w	d1,d3
		bset	#3,d3
		cmp.b	#')',(A3)+
		bne.s	err_bcd
		bsr	SkipComma
		cmp.b	#'-',(A3)+
		bne.s	err_bcd
		cmp.b	#'(',(A3)+
		bne.s	err_bcd
		bsr	getareg
		cmp.b	#')',(A3)+
		beq.s	bcd_1
err_bcd		bra	generic_error

*** EOR ***
eor_asm		addq.l	#2,Addr(a4)
		moveq	#0,D5
		move.b	(a3),d0
		call	tolower
		cmp.b	#'i',d0
		bne.s	eor_1
		addq.l	#1,A3
		cmp.b	#'.',(A3)
		beq.s	eor_0
		move.w	#$0A00,D3
		bra.s	logical_status

eor_0		moveq	#-1,D5
eor_1		bsr	GetSize
		call	skipspaces
		cmp.b	#'#',(A3)
		bne.s	no_eor_imm
		addq.l	#1,A3
		bsr	PutImm
		bsr	SkipComma
		bsr	Get_Dest_EA
		move.w	size(a4),D1
		lsl.w	#6,D1
		or.w	D1,D0
		or.w	#$0A00,D0
		bra.s	zcom_4

no_eor_imm	tst.l	D5
		bne.s	err_bcd
		bsr	getdreg
		move.w	D1,D3
		bsr	SkipComma
		bsr	Get_Dest_EA
		lsl.w	#8,D3
		lsl.w	#1,D3
		or.w	D3,D0
		move.w	size(a4),D1
		lsl.w	#6,D1
		or.w	D1,D0
		or.w	#$B100,D0
zcom_4		bra	set_instr_word

*** AND & OR & EOR SR & CCR ***
logical_status	or.w	#$3C,D3
		call	skipspaces
		cmp.b	#'#',(A3)+
		bne	addrmode_error
		move.w	#WSIZE,size(a4)
		bsr	PutImm
		bsr	SkipComma
		move.b	(a3),d0
		call	tolower
		cmp.b	#'s',d0
		bne.s	no_log_sr
		addq.l	#1,A3
		move.b	(a3)+,d0
		call	tolower
		cmp.b	#'r',d0
		bne	generic_error
		or.w	#$40,D3
		bra.s	log_st1

no_log_sr	lea	ccr_str(pc),a0
		bsr	checkstring

log_st1		move.w	D3,D0
		bra.s	zcom_4

*** MOVE ***
move_asm	move.b	(a3)+,d0
		call	tolower
		cmp.b	#'q',d0
		bne.s	no_move_quick
		call	skipspaces
		cmp.b	#'#',(A3)+
		bne	addrmode_error
		call	get_expr
		moveq	#-$80,d1	; d1 := $ffffff80
		cmp.l	d1,d0
		blt	out_range_error
		not.l	d1		; d1 := $0000007f
		st	d1		; d1 := $000000ff
		cmp.l	d1,d0
		bgt	out_range_error
		move.b	D0,D2
		bsr	SkipComma
		bsr	getdreg
		move.w	d1,d0
		lsl.w	#8,D0
		lsl.w	#1,D0
		or.w	#$7000,D0
		move.b	D2,D0
		bra	one_word_instr

no_move_quick	cmp.b	#'p',d0
		beq	move_peripheral
		cmp.b	#'a',d0
		bne.s	no_move_addr
		moveq	#-1,D5
		bra	normal_move_2

no_move_addr	cmp.b	#'m',d0
		beq	movem_asm
		cmp.b	#'.',d0
		beq	normal_move
		call	skipspaces
;#
;# move sp,usp didn't work because conflict with 's' in 'sr'
;# fixed in 1989-11-30
;#
		move.b	(a3),d0
		call	tolower
		cmp.b	#'s',d0
		bne.s	01$
		move.b	1(a3),d0
		call	tolower
		cmp.b	#'p',d0
		bne.s	move_status
		bra.s	move_to_usp

01$		cmp.b	#'u',d0
		beq.s	move_from_usp
		cmp.b	#'a',d0
		bne.s	move_to_status_or_ccr

move_to_usp	bsr	getareg
		move.w	d1,d3
		bsr	SkipComma
		lea	usp_str(pc),a0
		bsr	checkstring
		move.w	D3,D0
		or.w	#$4E60,D0
		bra	one_word_instr

move_from_usp	;MOVE	usp,An
		addq.l	#1,A3
		lea	sp_str(pc),a0
		bsr	checkstring
		bsr	SkipComma
		bsr	getareg
		move.w	d1,d0
		or.w	#$4E68,D0
		bra	one_word_instr

move_status	;MOVE sr,EA
		addq.l	#2,Addr(a4)
		addq.l	#1,A3
		move.b	(a3)+,d0
		call	tolower
		cmp.b	#'r',d0
		bne.s	move_err2
		bsr	SkipComma
		bsr	Get_Dest_EA
		or.w	#$40C0,D0
		bra.s	zcom_5

move_to_status_or_ccr	;MOVE EA,sr, MOVE EA,ccr
		move.w	#WSIZE,size(a4)
		addq.l	#2,Addr(a4)
		bsr	GetEA
		lsl.w	#3,D1
		or.w	D0,D1
		bsr	SkipComma
		move.b	(a3),d0
		call	tolower
		cmp.b	#'s',d0
		bne.s	try_move_ccr
		addq.l	#1,A3
		move.b	(a3)+,d0
		call	tolower
		cmp.b	#'r',d0
		bne.s	move_err2
		move.w	D1,D0
		or.w	#$46C0,D0
		bra.s	zcom_5
move_err2	bra	generic_error

try_move_ccr	lea	ccr_str(pc),a0
		bsr	checkstring
		move.w	D1,D0
		or.w	#$44C0,D0
zcom_5		bra	set_instr_word

normal_move	moveq	#0,D5
		subq.l	#1,a3
normal_move_2	addq.l	#2,Addr(a4)
		bsr	GetSize
		bsr	GetEA
		tst.w	size(a4)
		bne.s	nm_1
		cmp.w	#1,D1
		beq	addrmode_error
nm_1		lsl.w	#3,D1
		or.w	D1,D0
		move.w	D0,D3
		bsr	SkipComma
		bsr	GetEA
		move.w	d1,d2
		lsl.w	#3,d2
		or.w	d0,d2
		cmp.w	#$39,d2		;checking only...
		bhi	addrmode_error
		tst.l	D5
		bne.s	movea01
		cmp.w	#1,d1		;addr reg direct -> MOVEA
		bne.s	norm_move_2

movea01		cmp.w	#1,D1
		bne	addrmode_error	;MOVEA destination must be addr reg
movea02		tst.w	size(a4)
		beq.s	move_err2	;MOVEA size can't be BYTE

norm_move_2	lsl.w	#6,D1
		lsl.w	#8,D0
		lsl.w	#1,D0
		or.w	D1,D0
		or.w	D3,D0
		move.w	size(a4),D1
		bne.s	not_byte_move
		or.w	#$1000,D0
		bra.s	zump1

not_byte_move	addq.w	#1,D1
		bchg	#0,D1
		lsl.w	#8,D1
		lsl.w	#4,D1
		or.w	D1,D0
zump1		bra	set_instr_word

*** MOVEP ***
move_peripheral	addq.l	#2,Addr(a4)
		bsr	GetSize
		tst.w	size(a4)
		beq.s	move_err3
		call	skipspaces
		move.b	(a3),d0
		call	tolower
		cmp.b	#'d',d0
		bne.s	move_from_peripheral
		bsr	getdreg
		lsl.w	#8,D1
		lsl.w	#1,D1
		move.w	D1,D3
		bsr	SkipComma
		bsr	GetEA
		cmp.w	#5,D1
		bne	addrmode_error
		or.w	D3,D0
		or.w	#$0188,D0

move_per_1	move.w	size(a4),D1
		subq.w	#1,D1
		lsl.w	#6,D1
		or.w	D1,D0
		bra	set_instr_word

move_from_peripheral
		bsr	GetEA
		cmp.w	#5,D1
		bne	addrmode_error
		move.w	D0,D3
		bsr	SkipComma
		bsr	getdreg
		move.w	d1,d0
		lsl.w	#8,D0
		lsl.w	#1,D0
		or.w	D3,D0
		or.w	#$0108,D0
		bra.s	move_per_1

move_err3	bra	generic_error

*** MOVEM ***
movem_asm	addq.l	#4,Addr(a4)
		bsr	GetSize
		tst.w	size(a4)
		beq.s	move_err3
		call	skipspaces
		move.b	(a3),d0
		call	tolower
		cmp.b	#'d',d0
		beq.s	regs_to_mem
		cmp.b	#'a',d0
		beq.s	regs_to_mem
		cmp.b	#'s',d0
		bne.s	regs_from_mem

regs_to_mem	bsr	getreglist
		move.w	D2,D3
		bsr	SkipComma
		bsr	GetEA
		cmp.w	#2,D1
		bcs.s	addrmod_err1
		cmp.w	#3,D1
		beq.s	addrmod_err1
		move.w	D1,D2
		lsl.w	#3,D2
		or.w	D0,D2
		cmp.w	#$39,D2
		bhi.s	addrmod_err1
		cmp.w	#4,D1
		bne.s	regs_to_mem_1
		moveq	#15,D2

1$		lsr.w	#1,D3	;reverse bit order
		roxl.w	#1,D4
		dbf	D2,1$
		move.w	D4,D3

regs_to_mem_1	lsl.w	#3,D1
		or.w	D1,D0
		or.w	#$4880,D0
do_movem	move.w	size(a4),D1
		subq.w	#1,D1
		lsl.w	#6,D1
		or.w	D1,D0
		move.l	EndAddr(a4),A0
		move.w	D0,(A0)+
		move.w	D3,(A0)
		bra	assem9

addrmod_err1	bra	addrmode_error

regs_from_mem	bsr	GetEA
		cmp.w	#2,D1
		bcs.s	addrmod_err1
		cmp.w	#4,D1
		beq.s	addrmod_err1
		lsl.w	#3,D1
		or.w	D1,D0
		cmp.w	#$3B,D0
		bhi.s	addrmod_err1
		move.w	D0,D3
		bsr	SkipComma
		bsr	getreglist
		move.w	D3,D0
		move.w	D2,D3
		or.w	#$4C80,D0
		bra.s	do_movem

*** BTST & BCHG & BCLR & BSET ***
btst_asm	moveq	#0,D3
		bra.s	bit_ins_1

bchg_asm	moveq	#1,D3
		bra.s	bit_ins_1

bclr_asm	moveq	#2,D3
		bra.s	bit_ins_1

bset_asm	moveq	#3,D3

bit_ins_1	clr.w	size(a4)		for btst Dn,#imm
		addq.l	#2,Addr(a4)
		lsl.w	#6,D3
		call	skipspaces
		cmp.b	#'#',(A3)+
		bne.s	bit_reg_mode
		call	get_expr
		move.l	Addr(a4),A0
		move.w	D0,(A0)+
		move.l	A0,Addr(a4)
		bset	#11,D3
		bra.s	bit_get_ea
;#
;# btst Dn,#imm is now accepted
;#
bit_reg_mode	subq.l	#1,a3
		bsr	getdreg
		lsl.w	#8,D1
		lsl.w	#1,D1
		or.w	D1,D3
		bset	#8,D3

bit_get_ea	bsr	SkipComma
		bsr	GetEA
		cmp.w	#1,D1
		beq.s	addrmod_err2
		lsl.w	#3,D1
		or.w	D1,D0
		move.w	D3,D1
		and.w	#$C0,D1
		beq.s	bit_ea_1
		cmp.w	#$39,D0
		bhi.s	addrmod_err2

bit_ea_1	cmp.w	#$3c,D0
		bhi	addrmod_err2
		or.w	D3,D0
		cmp.w	#$083c,d0
		beq.s	addrmod_err2
		bra	set_instr_word

addrmod_err2	bra	addrmode_error

*** CHK ***
chk_asm		move.w	#$4180,D3
		bra.s	mul_div1

*** MUL & DIV ***
mul_asm		move.w	#$C0C0,D3
		bra.s	mul_div

div_asm		move.w	#$80C0,D3

mul_div		call	skipspaces
		move.b	(a3)+,d0
		call	tolower
		cmp.b	#'u',d0
		beq.s	mul_div1
		cmp.b	#'s',d0
		bne	generic_error
		bset	#8,D3		;signed multiply/divide

mul_div1	move.w	#WSIZE,size(a4)
		addq.l	#2,Addr(a4)
		bsr	GetEA
		cmp.w	#1,D1
		beq.s	addrmod_err2
		lsl.w	#3,D1
		or.w	D1,D0
		or.w	D0,D3
		bsr	SkipComma
		bsr	getdreg
		move.w	d1,d0
		lsl.w	#8,D0
		lsl.w	#1,D0
		or.w	D3,D0
		bra.s	set_instr_word

*** TAS ***
tas_asm		addq.l	#2,Addr(a4)
		bsr	Get_Dest_EA
		or.w	#$4AC0,D0
		bra.s	set_instr_word

*** JMP ***
jmp_asm		move.w	#$4EC0,D3
		bra.s	pp_1

*** JSR ***
jsr_asm		move.w	#$4E80,D3
		bra.s	pp_1

*** PEA ***
pea_asm		move.w	#$4840,D3

pp_1		addq.l	#2,Addr(a4)
		bsr.s	get_jump_ea
		or.w	D3,D0
		bra.s	set_instr_word

;
; get addressing mode for lea, pea, jmp and jsr
; allowed modes are in the CONTROL category
;
get_jump_ea	bsr	GetEA
		cmp.w	#2,d1
		beq.s	1$
		cmp.w	#5,d1
		bcs	addrmode_error
1$		lsl.w	#3,d1
		or.w	d1,d0
		cmp.w	#$3c,d0
		bcc	addrmode_error
		rts

*** LEA ***
lea_asm		addq.l	#2,Addr(a4)
		bsr.s	get_jump_ea
		move.w	d0,d2
		bsr	SkipComma
		bsr	getareg
		move.w	d1,d0
		lsl.w	#8,D0
		lsl.w	#1,D0
		or.w	d2,d0
		or.w	#$41C0,D0

set_instr_word	move.l	EndAddr(a4),A0
		move.w	D0,(A0)
		bra	assem9

*** EXT ***
ext_asm		bsr	GetSize
		move.w	size(a4),D2
		beq	generic_error
		addq.w	#1,D2
		lsl.w	#6,D2
		call	skipspaces
;#
;# fixed register checking 1989-08-28
;# (previously accepted address registers)
;#
		bsr	getdreg
		move.w	d1,d0
		or.w	D2,D0
		or.w	#$4800,D0
		bra	one_word_instr

*** NBCD ***
nbcd_asm	addq.l	#2,Addr(a4)
		clr.w	size(a4)
		move.w	#$4800,D3
		bra.s	one_arg_2

*** CLR ***
clr_asm		move.w	#$4200,D3

one_arg_com	addq.l	#2,Addr(a4)
		bsr	GetSize

one_arg_2	bsr	Get_Dest_EA
		move.w	size(a4),D1
		lsl.w	#6,D1
		or.w	D1,D0
		or.w	D3,D0
		move.l	EndAddr(a4),A0
		move.w	D0,(A0)
		bra	assem9

*** NEG & NEGX ***
neg_asm		move.b	(a3),d0
		call	tolower
		cmp.b	#'x',d0
		bne.s	no_negx
		addq.l	#1,A3
		move.w	#$4000,D3
		bra.s	one_arg_com

no_negx		move.w	#$4400,D3
		bra.s	one_arg_com

*** NOT ***
not_asm		move.w	#$4600,D3
		bra.s	one_arg_com

*** TST ***
tst_asm		move.w	#$4A00,D3
		bra.s	one_arg_com

*** SWAP ***
swap_asm	call	skipspaces
		bsr	getdreg
		move.w	d1,d0
		or.w	#$4840,D0
		bra.s	one_word_instr

reset_asm	moveq	#$00,D0			;RESET
		bra.s	instr_4e7x

nop_asm		moveq	#$01,D0			;NOP
		bra.s	instr_4e7x

rte_asm		moveq	#$03,D0			;RTE
		bra.s	instr_4e7x

rts_asm		moveq	#$05,D0			;RTS
		bra.s	instr_4e7x

trapv_asm	moveq	#$06,D0			;TRAPV
		bra.s	instr_4e7x

rtr_asm		moveq	#$07,D0			;RTR

instr_4e7x	add.w	#$4e70,d0

one_word_instr	move.l	Addr(a4),A0
		move.w	D0,(A0)+
		move.l	A0,Addr(a4)
		bra	assem9

*** ILLEGAL ***
illegal_asm	move.w	#ILLEGAL_INSTR,D0
		bra.s	one_word_instr

addrmod_err3	bra	addrmode_error

*** TRAP ***
trap_asm	call	skipspaces
		cmp.b	#'#',(A3)+
		bne.s	addrmod_err3
		call	get_expr
		and.w	#$0f,D0
		or.w	#$4E40,D0
		bra.s	one_word_instr

*** UNLK ***
unlk_asm	call	skipspaces
		bsr	getareg
		move.w	d1,d0
		or.w	#$4E58,D0
		bra.s	one_word_instr

*** LINK ***
link_asm	call	skipspaces
		bsr	getareg
		move.w	d1,d3
		bsr	SkipComma
		cmp.b	#'#',(A3)+
		bne.s	addrmod_err3
		call	get_expr
		move.w	D0,D1
		move.w	D3,D0
		or.w	#$4E50,D0
		bra.s	two_words_instr

*** STOP ***
stop_asm	call	skipspaces
		cmp.b	#'#',(A3)+
		bne.s	addrmod_err3
		call	get_expr
		move.w	D0,D1
		move.w	#$4E72,D0
two_words_instr	move.l	Addr(a4),A0
		move.w	D0,(A0)+
		move.w	D1,(A0)+
		move.l	A0,Addr(a4)
		bra	assem9

*** EXG ***
exg_asm		bsr	getreg
		bsr	SkipComma
		swap	D1
		bsr	getreg
		swap	D1
		btst	#16+3,D1
		beq.s	no_exg_1
		btst	#3,D1
		bne.s	no_exg_1
		swap	D1

no_exg_1	move.l	D1,D0
		move.l	D1,D3
		lsr.l	#7,D0
		and.w	#$0E00,D0
		and.w	#$07,D1
		or.w	D1,D0
		or.w	#$C100,D0
		move.l	D3,D2
		swap	D2
		eor.w	D3,D2
		btst	#3,D2
		bne.s	data_and_addr
		bset	#6,D0
		btst	#3,D3
		beq.s	exg_9
		bset	#3,D0
		bra.s	exg_9

data_and_addr	or.w	#$88,D0
exg_9		bra	one_word_instr

*** GET REGISTER LIST (for MOVEM) ***
; result is returned in d2
getreglist	clr.w	D2

reglistx	bsr.s	getreg
		bset	D1,D2
		cmp.b	#'/',(A3)+
		beq.s	reglistx
		cmp.b	#'-',-1(A3)
		bne.s	reglist9
		move.w	D1,D7
		bsr.s	getreg
		move.w	D1,D0
		eor.w	D7,D0
		btst	#3,D0
		bne	generic_error	;register in range are different types
		cmp.w	D1,D7
		bls.s	regr_01
		exg	d1,d7		;register range out of order
		bra.s	regrange
regr_01		beq.s	reg_r1

regrange	bset	D7,D2
		addq.w	#1,D7
		cmp.w	D1,D7
		bls.s	regrange
reg_r1		cmp.b	#'/',(A3)+
		beq.s	reglistx

reglist9	subq.l	#1,A3
		moveq	#0,D0
		rts			;register mask returned in D2

*** GET ONE REGISTER, number in D1, 0=D0..15=sp ***
;#
;# must not change the high word of d1
;# (it zeroed the high word if 'sp' was given and
;#  that caused exg Rx,sp to work incorrectly)
;#
getreg		clr.w	D1
		call	skipspaces
		move.b	(a3)+,d0
		call	tolower
		cmp.b	#'s',d0
		beq.s	check_sp
		cmp.b	#'d',d0
		beq.s	g_r_1
		cmp.b	#'a',d0
		bne.s	greg_err
		bset	#3,D1

g_r_1		move.b	(A3)+,D0
		sub.b	#'0',D0
		bcs.s	greg_err
		cmp.b	#8,D0
		bcc.s	greg_err
		ext.w	D0
		or.b	D0,D1
g_r_2		rts	;register returned in D1

check_sp	move.b	(a3)+,d0
		call	tolower
		cmp.b	#'p',d0
		bne.s	greg_err
		move.w	#$0f,d1		;can't use moveq
		rts

greg_err	bra	generic_error
;
; get address register, return in d1
;
getareg		bsr	getreg
		bmi.s	01$
		bclr	#3,d1
		beq.s	greg_err
01$		rts

;
; get data register, return in d1
;
getdreg		bsr	getreg
		bmi.s	01$
		btst	#3,d1
		bne.s	greg_err
01$		rts

*** Get a number in range 1..8, used by 'quick' instructions and shifts ***
* return -1 if error
get_expr_1_8	call	get_expr
		tst.l	d0
		beq	out_range_error
		moveq	#8,d1
		cmp.l	d1,d0
		bhi	out_range_error
		and.w	#$07,d0
		rts

*** SKIP COMMA & SPACES AROUND IT ***
; error if no comma found
SkipComma	call	skipspaces
		cmp.b	#',',(A3)+
		bne.s	greg_err
		call	skipspaces
		moveq	#0,D0
		rts

;#
;# get destination EA for some instructions
;# addr. reg. direct, pcel and immediate are not allowed
;# returns combined EA code in d0
;#
Get_Dest_EA	bsr.s	GetEA
		cmp.w	#1,D1	; addr. reg. direct
		beq	addrmode_error
Do_EA_1		lsl.w	#3,D1
		or.w	D1,D0
		cmp.w	#$39,D0		; no pcrel or immediate
		bhi	addrmode_error
		rts

GetEA: ;get effective address, mode=D1,reg=D0
* put displacements etc. in memory at address pointed by Addr(a4)
* and increment Addr(a4)
		call	skipspaces
		move.b	(a3),d0
		call	tolower
		cmp.b	#'d',d0
		beq.s	reg_direct
		cmp.b	#'a',d0
		beq.s	reg_direct
		cmp.b	#'s',d0
		bne.s	no_reg_direct

reg_direct	bsr	getreg
		move.w	d1,d0
		moveq	#0,d1		;data register direct
		bclr	#3,d0
		beq.s	reg_09
		moveq	#1,d1		;address register direct
reg_09		rts

no_reg_direct	cmp.b	#'(',d0
		bne.s	no_indirect_or_postincrement
		addq.l	#1,a3
		bsr	getareg
		move.w	d1,d0
		cmp.b	#')',(A3)+
		bne	generic_error
		cmp.b	#'+',(A3)
		beq.s	postincrement
		moveq	#2,D1
		rts

postincrement	addq.l	#1,A3
		moveq	#3,D1		;addr. reg. indirect with postincrement
		rts

no_indirect_or_postincrement
		cmp.b	#'-',(a3)
		bne.s	no_predecrement
		cmp.b	#'(',1(A3)
		bne.s	no_predecrement
		addq.l	#2,A3
		bsr	getareg
		move.w	d1,d0
		cmp.b	#')',(A3)+
		bne	generic_error
		moveq	#4,D1		;addr. reg. indirect with predecrement
		rts

indirect_with_index
		move.l	D0,D7
		bsr	GetIndex
		move.l	D7,D0
		moveq	#6,D1		;addr. reg. indirect with index
		rts

no_predecrement	cmp.b	#'#',(a3)
		bne.s	no_immediate
		addq.l	#1,A3

PutImm		call	get_expr
		move.l	Addr(a4),A0
		move.w	size(a4),D1
		bne.s	sz1
		and.w	#$FF,D0
		move.w	D0,(A0)+
		bra.s	sz9
sz1		cmp.w	#1,D1
		bne.s	sz2
		move.w	D0,(A0)+
		bra.s	sz9
sz2		move.l	D0,(A0)+

sz9		moveq	#4,D0		;immediate
		bra.s	s_addr_seven

no_immediate	call	get_expr
		move.l	D0,D2
		cmp.b	#'(',(a3)
		bne	absolute_mode
		addq.l	#1,A3
		move.b	(a3),d0
		call	tolower
		cmp.b	#'a',d0
		beq.s	01$
		cmp.b	#'s',d0
		bne.s	no_displacement_or_index
01$		bsr	getareg
		move.w	d1,d0
		cmp.b	#',',(A3)+
		beq.s	indirect_with_index
		cmp.b	#')',-1(A3)
		bne	generic_error
		move.l	Addr(a4),A0
		move.w	D2,(A0)+
		move.l	A0,Addr(a4)
		moveq	#5,D1		; addr reg. indirect with displacement
		rts

abs_short_mode	move.l	Addr(a4),A0
		move.w	D2,(A0)+
		moveq	#0,D0		;absolute short

s_addr_seven	move.l	a0,Addr(a4)

seven_up	moveq	#7,d1
		rts

absolute_mode	move.w	D2,D1
		ext.l	D1
		cmp.l	D2,D1
		beq.s	abs_short_mode
		move.l	Addr(a4),A0
		move.l	D2,(A0)+
		moveq	#1,D0		;absolute long
		bra.s	s_addr_seven

no_displacement_or_index
		cmp.b	#'p',d0
		bne.s	pcrel_indx_2
		addq.l	#1,a3
		move.b	(a3)+,d0
		call	tolower
		cmp.b	#'c',d0
		bne	generic_error
		cmp.b	#')',(A3)+
		bne.s	pcrel_indx_1
		sub.l	Addr(a4),D2
		move.l	Addr(a4),A0
		move.w	D2,(A0)+
		move.l	A0,Addr(a4)
		move.w	d2,d0
		ext.l	d0
		cmp.l	d0,d2
		bne	out_range_error

		moveq	#2,D0		;pc relative
		bra.s	seven_up

;
; pc-relative indexed syntax can now be in the correct format '(pc,Rn.w)'
;
pcrel_indx_1	cmp.b	#',',-1(a3)
		bne	generic_error
pcrel_indx_2	sub.l	Addr(a4),D2
		move.b	d2,d0
		ext.w	d0
		ext.l	d0
		cmp.l	d0,d2
		bne	out_range_error
		bsr.s	GetIndex
		moveq	#3,D0		;pc relative with index
		bra.s	seven_up

*** GET SIZE (put it in size(a4), 0=B, 1=W, 2=L) ***
GetSize		cmp.b	#'.',(A3)+
		bne	generic_error
		move.b	(A3)+,D0
		call	tolower
		cmp.b	#'b',D0
		bne.s	siz1
		moveq	#BSIZE,d0
		bra.s	siz3
siz1		cmp.b	#'w',D0
		bne.s	siz2
		moveq	#WSIZE,d0
		bra.s	siz3
siz2		cmp.b	#'l',D0
		bne	generic_error
		moveq	#LSIZE,d0
siz3		move.w	d0,size(a4)
		rts

GetIndex	;displacement value in d2
		bsr	getreg
		bmi.s	index_error
		moveq	#0,d0
		bclr	#3,d1
		beq.s	01$
		bset	#15,d0
01$		lsl.w	#8,d1
		lsl.w	#4,d1
		or.w	d0,d1
		and.w	#$ff,d2
		or.w	d2,d1
		cmp.b	#'.',(A3)+
		bne.s	index_error
		move.b	(a3)+,d0
		call	tolower
		cmp.b	#'w',d0
		beq.s	index_2
		cmp.b	#'l',d0
		bne.s	index_error
		bset	#11,D1
index_2		cmp.b	#')',(A3)+
		bne.s	index_error
		move.l	Addr(a4),A0
		move.w	D1,(A0)+
		move.l	A0,Addr(a4)
rt001		rts

;
; check if a given string follows. error if not.
; case insensitive (but string must be in lower case).
; pointer to null-terminated string in a0
;
checkstring	move.l	a3,a1
0$		tst.b	(a0)
		beq.s	ckstr1
		move.b	(a1)+,d0
		call	tolower
		cmp.b	(a0)+,d0
		beq.s	0$

index_error	bra	generic_error

ckstr1		move.l	a1,a3
		rts

;
; instruction jump table for assembler
;
instrjumps	rw	as_asm,ls_asm,rox_asm,rot_asm		;0-3
		rw	move_asm,add_asm,sub_asm
		rw	and_asm,or_asm
		rw	abcd_asm,sbcd_asm
		rw	mul_asm,div_asm
		rw	exg_asm,eor_asm,cmp_asm
		rw	btst_asm,bchg_asm,bclr_asm,bset_asm
		rw	chk_asm,lea_asm,ext_asm,clr_asm
		rw	neg_asm,not_asm,tst_asm,nbcd_asm
		rw	swap_asm,pea_asm,link_asm,unlk_asm
		rw	reset_asm,nop_asm,stop_asm,rte_asm,tas_asm
		rw	rts_asm,trapv_asm,rtr_asm
		rw	jsr_asm,jmp_asm,trap_asm
		rw	illegal_asm

assemfmt	dc.b	'%08lx: ',0

usp_str		dc.b	'u'
sp_str		dc.b	'sp',0
ccr_str		dc.b	'ccr',0

		end
