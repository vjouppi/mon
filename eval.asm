;
; eval.asm
;

;
; modulo operator --> '%' -- 1991-07-26 -- 1.42
;
		nolist
		include "exec/types.i"
		include "exec/memory.i"
		include "exec/execbase.i"
		include "libraries/dosextens.i"
		include "offsets.i"
		list

		include	"monitor.i"

;
; this module defines the following public subroutines
;
;		GetExpr, GetDecNum, GetHexNum
;

;
;
		xref	findvar
		xref	find_brk_num

		xref	expression_error
		xref	odd_address_error
;
;
; The expression evaluation routines
;
; NOTE: GetExpr must not change d1/a0/a1
;
**********************************************************
*							 *
* operations supported (32 bit integer arithmetic):	 *
*							 *
* + - * / %  -- add, subtract, multiply, divide, modulo  *
*							 *
* !| & ^      -- bitwise or, and, xor			 *
* << >>       -- left & right bit shifts		 *
* - + ~       -- unary plus, minus, bit complement	 *
*							 *
* '*'	      -- 'current address'			 *
*							 *
* hunk(n)     -- start address of a hunk		 *
* hlen(n)     -- length of a hunk			 *
* hend(n)     -- end address of hunk			 *
* nhunks      -- number of hunks			 *
* abs(x)      -- absolute value				 *
* peek(addr)  -- byte value of memory location		 *
* peekw(addr) -- word value of memory location		 *
* peekl(addr) -- longword value of memory location	 *
*							 *
* numbers can be ('_'-prefix),	hex ($-prefix),		 *
* octal (@-prefix), binary (%-prefix)	 		 *
* or strings of ascii-characters between single quotes.	 *
*							 *
* if no prefix is used, the default base is assumed	 *
*							 *
*		no overflow checking!			 *
*							 *
**********************************************************

		pub	GetExpr

		movem.l	d1-d2/a0-a1/a6,-(sp)
		bsr.s	get_ex1
		move.l	d0,d2

get_expr_loop	call	skipspaces
		move.b	(a3)+,d0
		cmp.b	#'+',d0
		bne.s	expr1
		bsr.s	get_ex1
		add.l	d0,d2
		bra.s	get_expr_loop
expr1		cmp.b	#'-',d0
		bne.s	expr2
		bsr.s	get_ex1
		sub.l	d0,d2
		bra.s	get_expr_loop
expr2		cmp.b	#'!',d0
		beq.s	bit_or
		cmp.b	#'|',d0
		bne.s	expr3
bit_or		bsr.s	get_ex1
		or.l	d0,d2
		bra.s	get_expr_loop
expr3		subq.l	#1,a3
		move.l	d2,d0
		movem.l	(sp)+,d1-d2/a0-a1/a6
		rts

get_ex1		move.l	d2,-(sp)
		bsr.s	get_ex2
		move.l	d0,d2
get_ex1_loop	call	skipspaces
		move.b	(a3)+,d0
		cmp.b	#'*',d0
		bne.s	ex11
		bsr.s	get_ex2
		move.l	d2,d1
		bsr	multiply
		move.l	d0,d2
		bra.s	get_ex1_loop
ex11		cmp.b	#'/',d0
		bne.s	ex12
		bsr.s	get_ex2
		tst.l	d0
		beq	expression_error		;divide by zero
		move.l	d2,d1
		bsr	divide
		move.l	d0,d2
		bra.s	get_ex1_loop
ex12		cmp.b	#'%',d0
		bne.s	ex13
		bsr.s	get_ex2
		tst.l	d0
		beq	expression_error		;divide by zero
		move.l	d2,d1
		bsr	modulo
		move.l	d0,d2
		bra.s	get_ex1_loop
ex13		cmp.b	#'&',d0
		bne.s	ex14
		bsr.s	get_ex2
		and.l	d0,d2
		bra.s	get_ex1_loop
ex14		cmp.b	#'^',d0
		bne.s	ex15
		bsr.s	get_ex2
		eor.l	d0,d2
		bra.s	get_ex1_loop
ex15		subq.l	#1,a3
		move.l	d2,d0
		move.l	(sp)+,d2
		rts

get_ex2		move.l	d2,-(sp)
		bsr.s	get_ex3
		move.l	d0,d2
get_ex2_loop	call	skipspaces
		move.b	(a3)+,d0
		cmp.b	#'<',d0
		bne.s	ex21
		cmp.b	#'<',(a3)+
		bne	expression_error		;syntax error
		bsr.s	get_ex3
		lsl.l	d0,d2
		bra.s	get_ex2_loop
ex21		cmp.b	#'>',d0
		bne.s	ex22
		cmp.b	#'>',(a3)+
		bne	expression_error		;syntax error
		bsr.s	get_ex3
		lsr.l	d0,d2
		bra.s	get_ex2_loop
ex22		subq.l	#1,a3
		move.l	d2,d0
		move.l	(sp)+,d2
		rts

get_ex3		call	skipspaces
		move.b	(a3)+,d0
		cmp.b	#'(',d0
		bne.s	ex31
		call	GetExpr
		call	skipspaces
		cmp.b	#')',(a3)+
		bne	expression_error		;right parenthesis expected
		rts
ex31		cmp.b	#'-',d0
		bne.s	ex32
		bsr.s	get_ex3
		neg.l	d0
		rts
ex32		cmp.b	#'+',d0
		beq.s	get_ex3
		cmp.b	#'~',d0
		bne.s	ex33
		bsr.s	get_ex3
		not.l	d0
		rts

ex33		cmp.b	#'''',d0
		beq	get_strnum

		cmp.b	#'$',d0		;hex prefix '$'
		bne.s	ex34
		moveq	#16,d0
		bra.s	gnum_j

ex34		cmp.b	#'%',d0		;binary prefix '%'
		bne.s	ex37
		moveq	#2,d0
gnum_j		bra	get_num

ex37		cmp.b	#'*',d0		;current address
		bne.s	ex37a
		move.l	Addr(a4),d0
		rts

ex37a		cmp.b	#'[',d0		;register?
		bne.s	ex38
		move.b	(a3)+,d0
		call	tolower
		lsl.w	#8,d0
		move.b	(a3)+,d0
		call	tolower
		lea	RegPC(a4),a0
		cmp.w	#'pc',d0
		beq.s	ex37x
		cmp.w	#'cc',d0
		bne.s	ex37b
		move.b	(a3),d0
		call	tolower
		cmp.b	#'r',d0
		bne.s	01$
		addq.l	#1,a3
01$		moveq	#0,d0
		move.b	RegCCR_B(a4),d0
		bra.s	ex37z

ex37b		lea	RegSP(a4),a0
		cmp.w	#'sp',d0
		beq.s	ex37x
		sub.w	#'a0',d0
		bcs.s	ex37_err
		cmp.w	#7,d0
		bhi.s	ex37c
		lea	AddrRegs(a4),a0
		bra.s	ex37r
ex37c		sub.w	#'d0'-'a0',d0
		bcs.s	ex37_err
		cmp.w	#7,d0
		bhi.s	ex37_err
		lea	DataRegs(a4),a0
ex37r		lsl.w	#2,d0
		add.w	d0,a0
ex37x		move.l	(a0),d0
ex37z		cmp.b	#']',(a3)+
		beq.s	exrt01
ex37_err	bra	expression_error

ex38		subq.l	#1,a3
		bsr	get_token
		tst.l	d0
		bmi.s	ex38b
		lea	tokfuncs(pc),a0
		add.l	d0,d0
		add.l	d0,a0
		add.w	(a0),a0
		jmp	(a0)

ex38b		move.b	(a3),d0
		cmp.b	#'@',d0
		beq.s	00$
		cmp.b	#'.',d0
		beq.s	00$
		call	isalpha
		bcc.s	ex39b

00$		move.l	a3,a0			;variable?
		addq.l	#1,a0

01$		move.b	(a0)+,d0
		call	isalnum
		bcs.s	01$
		subq.l	#1,a0

		move.b	(a0),d0
		clr.b	(a0)

		movem.l	d0/a0,-(sp)
		move.l	a3,a0
		bsr	findvar
		movem.l	(sp)+,d1/a0
		bcs.s	ex39

		move.b	d1,(a0)
		move.l	a0,a3
exrt01		rts

ex39		move.b	d1,(a0)
ex39b		cmp.b	#'@',(a3)		; octal prefix
		bne.s	ex39c
		addq.l	#1,a3
		moveq	#8,d0
		bra.s	ex39x

ex39c		moveq	#10,d0
		cmp.b	#'_',(a3)+		;decimal perix '_'
		beq.s	ex39x
		subq.l	#1,a3
		move.b	defbase(a4),d0
ex39x		bra	get_num

multiply	movem.l	d4-d6,-(sp)
		moveq	#0,d4
		tst.l	d0
		bpl.s	mul1
		neg.l	d0
		not.w	d4
mul1		tst.l	d1
		bpl.s	mul2
		neg.l	d1
		not.w	d4
mul2		move.l	d0,d5
		swap	d5
		mulu	d1,d5
		swap	d5
		move.l	d1,d6
		swap	d6
		mulu	d0,d6
		swap	d6
		add.l	d5,d6
		mulu	d1,d0
		add.l	d6,d0
		tst.w	d4
		bpl.s	mul9
		neg.l	d0
mul9		movem.l	(sp)+,d4-d6
		rts

divide		move.l	d4,-(sp)
		moveq	#0,d4
		tst.l	d0
		bpl.s	div02
		neg.l	d0
		not.w	d4
div02		tst.l	d1
		bpl.s	div03
		neg.l	d1
		not.w	d4
div03		bsr.s	div001
		tst.w	d4
		bpl.s	div04
		neg.l	d0
div04		move.l	(sp)+,d4
		rts

modulo		move.l	d4,-(sp)
		move.l	d0,d4
		bpl.s	mod01
		neg.l	d0
mod01		tst.l	d1
		bpl.s	mod02
		neg.l	d1
mod02		bsr.s	div001
		move.l	d1,d0
		tst.l	d4
		bpl.s	mod03
		neg.l	d0
mod03		move.l	(sp)+,d4
		rts

div001
;inputs: jaettava d1, jakaja d0, unsigned
;result: quotient in d0, remainder in d1
		movem.l	d4-d7,-(sp)
		moveq	#0,d5
		moveq	#32-1,d4
divloop		lsl.l	#1,d1
		roxl.l	#1,d5
		move.l	d5,d6
		sub.l	d0,d6
		bcs.s	diz02
		move.l	d6,d5
diz02		roxl.l	#1,d7
		dbf	d4,divloop
		move.l	d7,d0
		not.l	d0
		move.l	d5,d1
		movem.l	(sp)+,d4-d7
		rts

r_hunk		bsr.s	gethunk
		move.l	d1,d0
		addq.l	#4,d0
		bra	no_more_args

r_hlen		bsr.s	gethunk
		move.l	d1,a0
		move.l	-4(a0),d0
		subq.l	#8,d0
		bra	no_more_args

r_hend		bsr.s	gethunk
		move.l	d1,d0
		move.l	d1,a0
		add.l	-4(a0),d0
		subq.l	#5,d0
		bra	no_more_args

r_nhunks	moveq	#0,d0
		move.l	SegList(a4),d1
01$		lsl.l	#2,d1
		beq.s	g_rts
		move.l	d1,a0
		move.l	(a0),d1
		addq.l	#1,d0
		bra.s	01$

gethunk		bsr	get_first_arg
		move.l	SegList(a4),d1
01$		lsl.l	#2,d1
		beq	expression_error		;hunk not found
		tst.l	d0
		beq.s	g_rts
		move.l	d1,a0
		move.l	(a0),d1
		subq.l	#1,d0
		bra.s	01$
g_rts		rts

r_abs		bsr	get_first_arg
		tst.l	d0
		bpl.s	fcom
		neg.l	d0
fcom		bra	no_more_args

r_peek		bsr	get_first_arg
		move.l	d0,a0
		moveq	#0,d0
		move.b	(a0)+,d0
		bra	no_more_args

r_peekw		bsr	get_first_arg
		btst	#0,d0
		bne	odd_address_error
		move.l	d0,a0
		moveq	#0,d0
		move.w	(a0),d0
		bra	no_more_args

r_peekl		bsr	get_first_arg
		btst	#0,d0
		bne	odd_address_error
		move.l	d0,a0
		move.l	(a0),d0
		bra	no_more_args

r_avail		bsr	get_first_arg
		move.l	d0,d1
		lib	Exec,AvailMem
		bra	no_more_args

;
; find named port, library, device or resource from exec lists
;
r_port		move.w	#PortList,d1
		bra.s	r_execlist
r_lib		move.w	#LibList,d1
		bra.s	r_execlist
r_dev		move.w	#DeviceList,d1
		bra.s	r_execlist
r_res		move.w	#ResourceList,d1
r_execlist	call	skipspaces
		cmp.b	#'(',(a3)+
		bne	expression_error
		call	GetName
		tst.l	d0
		beq	expression_error
		move.l	d0,a1
		lib	Exec,Forbid	;Forbid() & Permit are quaranteed
		lea	0(a6,d1.w),a0	;to preserve all registers
		lib	FindName
		lib	Permit
		bra	no_more_args

;
; find a task by name or CLI number (or find current task)
;
r_task		call	skipspaces
		cmp.b	#'(',(a3)+
		bne	expression_error
		call	skipspaces
		cmp.b	#'"',(a3)
		beq	r_taskname
		call	GetExpr	;find task by CLI number
		tst.l	d0
		beq.s	r_ftask

		getbase Dos,a0		;Forbid() here??
		move.l	dl_Root(a0),a0
		move.l	rn_TaskArray(a0),a0
		add.l	a0,a0		; BCPL pointer conversion
		add.l	a0,a0
		move.l	(a0),d1		; max. number of CLI processes
		cmp.l	d1,d0
		bhi	expression_error
		lsl.l	#2,d0
		move.l	0(a0,d0.l),d0
		beq	no_more_args
		moveq	#pr_MsgPort,d1
		sub.l	d1,d0
		bra	no_more_args
 
r_taskname	call	GetName
		tst.l	d0
		beq	expression_error
r_ftask		move.l	d0,a1
		lib	Exec,FindTask
		bra	no_more_args

r_brk		bsr	get_first_arg
		bsr	find_brk_num
		moveq	#-1,d1
		cmp.l	d0,d1
		beq	expression_error
		bra	no_more_args

get_first_arg	call	skipspaces
		cmp.b	#'(',(a3)+
		bne	expression_error		;left parenhesis expected
		call	JUMP,GetExpr

;get_arg	call	skipspaces
;		cmp.b	#',',(a3)+
;		bne	expression_error		;comma expected
;		call	JUMP,GetExpr

no_more_args	;D0 not changed!
		call	skipspaces
		cmp.b	#')',(a3)+
		bne	expression_error		;right parenthesis expected
		rts

get_num		;radix in D0
		bsr.s	getnum0
		bcs	expression_error
		rts

		pub	GetHexNum

		call	skipspaces
		moveq	#16,d0
		bra.s	getnum0

		pub	GetDecNum

		call	skipspaces
		moveq	#10,d0
;
; get a number, radix in d0, return carry set if error
;
getnum0		movem.l	d2/d3,-(sp)
		move.l	d0,d2
		moveq	#0,d0
		move.l	a3,a0
getnum1		moveq	#0,d1
		move.b	(a3)+,d1
		cmp.b	#'a',d1
		bcs.s	getnum2
		and.b	#$df,d1
getnum2		sub.b	#'0',d1
		bcs.s	getnum9
		cmp.b	#10,d1
		bcs.s	getnum3
		cmp.b	#17,d1
		bcs.s	getnum9
		subq.b	#7,d1
getnum3		cmp.b	d2,d1
		bcc.s	getnum9
		move.l	d0,d3
		mulu	d2,d3
		swap	d0
		mulu	d2,d0
		swap	d0
		add.l	d3,d0
		add.l	d1,d0
		bra.s	getnum1
getnum9		subq.l	#1,a3
		cmp.l	a3,a0
		eor	#1,ccr		complement carry
		movem.l	(sp)+,d2/d3
		rts

get_strnum	moveq	#0,d0
strnum1		move.b	(a3)+,d1
		beq	expression_error		;syntax error
		cmp.b	#'''',d1
		beq.s	strnum2
strnum1a	lsl.l	#8,d0
		move.b	d1,d0
		bra.s	strnum1
strnum2		cmp.b	#'''',(a3)+
		beq.s	strnum1a
		subq.l	#1,a3
strnum9		rts

get_token	movem.l	d2/a2,-(sp)
		lea	tokentable(pc),a0
		moveq	#0,d0

gt1		move.l	a3,a2
		tst.b	(a0)
		beq.s	gt_nf
		move.l	a0,a1
gt2		move.b	(a2)+,d1
		move.b	(a1)+,d2
		or.b	#$20,d1
		cmp.b	#'a',d1
		bcs.s	gt2a
		cmp.b	#'z',d1
		bls.s	gt3
gt2a		tst.b	d2
		bne.s	gt_next
		move.l	a2,a3
		subq.l	#1,a3
		bra.s	gt_ret
gt3		cmp.b	d1,d2
		beq.s	gt2

gt_next		tst.b	(a0)+
		bne.s	gt_next
		addq.l	#1,d0
		bra.s	gt1

gt_nf		moveq	#-1,d0
gt_ret		movem.l	(sp)+,d2/a2
		rts

tokfuncs	rw	r_hunk
		rw	r_hlen
		rw	r_hend
		rw	r_nhunks
		rw	r_abs
		rw	r_peek
		rw	r_peekw
		rw	r_peekl
		rw	r_avail
		rw	r_lib
		rw	r_dev
		rw	r_res
		rw	r_task
		rw	r_port
		rw	r_brk

tokentable	dc.b	'hunk',0
		dc.b	'hlen',0
		dc.b	'hend',0
		dc.b	'nhunks',0
		dc.b	'abs',0
		dc.b	'peek',0
		dc.b	'peekw',0
		dc.b	'peekl',0
		dc.b	'avail',0
		dc.b	'lib',0
		dc.b	'dev',0
		dc.b	'res',0
		dc.b	'task',0
		dc.b	'port',0
		dc.b	'brk',0
		dc.b	0

		end
