;
; variables.asm
;
		include	"monitor.i"

		xdef	clear_all_variables
		xdef	findvar

		xref	GetKey
		xref	ChrOut
		xref	ChrOutWin

		xref	generic_error
		xref	out_memory_error

; clear all variables
; called before exit and when the cv-command is executed
; changes d2 and a6
clear_all_variables
		getbase	Exec
		move.l	VarList(a4),d2

clrvarloop	tst.l	d2
		beq.s	cleared_vars
		move.l	d2,a1
		moveq	#0,d0
		move.w	var_Length(a1),d0
		move.l	var_Next(a1),d2
		lib	FreeMem
		bra.s	clrvarloop

cleared_vars	clr.l	VarList(a4)
		rts

;
; set [var=expr]
;
		cmd	setvariable

		tst.b	(a3)
		bne	set_variable
		move.l	VarList(a4),d5
		bne.s	showvar1
		lea	novartxt(pc),a0
		call	printstring_a0_window	;'no variables defined'
		bra.s	xvar_end

showvar1	lea	varhead(pc),a0
		call	printstring_a0

showvarloop	move.l	d5,a2
		lea	var_Name(a2),a0
		move.l	a0,d0
		move.l	var_Value(a2),d1
		move.l	d1,d2
		lea	varfmt(pc),a0
		call	printf
		call	CheckKeys
		bne.s	xvar_end
		move.l	var_Next(a2),d5
		bne.s	showvarloop
xvar_end	rts

set_variable	move.b	(a3),d0
		call	isalpha
		bcc	generic_error
		move.l	a3,a5
01$		move.b	(a3)+,d0
		call	isalnum
		bcs.s	01$
		subq.l	#1,a3
		move.l	a3,a2
		cmp.l	a3,a5
		beq	generic_error
		call	skipspaces
		tst.b	(a3)
		beq.s	remvar
		cmp.b	#'=',(a3)+
		beq.s	02$
		subq.l	#1,a3
02$		call	get_expr
		move.l	a5,a0
		clr.b	(a2)
		bsr.s	setvar
		bra.s	xvar_end
remvar		move.l	a5,a0
		clr.b	(a2)
		bsr	findvar
		bcs.s	xvar_end
		bsr	deletevar
		bra.s	xvar_end

;
; set value of existing or new variable
; a0 - name of variable, null-terminated
; d0 - value of variable
;
setvar		movem.l	d0/a0,-(sp)
		bsr	findvar
		bcc.s	01$
		move.l	4(sp),a0
		bsr.s	addvar
		move.l	d0,a0
01$		movem.l	(sp)+,d0/a1
		move.l	d0,var_Value(a0)
		rts

;
; add a new variable
; a0 - null-terminated name
; return variable pointer in d0
;
addvar		movem.l	d2/a2-a3/a6,-(sp)
		move.l	a0,a2
01$		tst.b	(a0)+
		bne.s	01$
		lea	var_Name(a0),a0
		move.l	a0,d0
		sub.l	a2,d0
		move.l	d0,d2
		move.l	#MEMF_CLEAR!MEMF_PUBLIC,d1
		lib	Exec,AllocMem
		tst.l	d0
		beq	out_memory_error
		move.l	d0,a3
		move.w	d2,var_Length(a3)
		lea	var_Name(a3),a0
05$		move.b	(a2)+,(a0)+
		bne.s	05$

;
; insert to the list. keep list sorted.
;#
;# ->v1.21 ... sorting is now case insensitive
;#
;
		lea	VarList(a4),a2
06$		move.l	(a2),d0
		beq.s	add_now
		move.l	d0,a0
		lea	var_Name(a0),a0
		lea	var_Name(a3),a1
07$		bsr.s	str_comp_ch
		bls.s	add_now
		move.l	(a2),a2
		bra.s	06$

add_now		move.l	(a2),(a3)
		move.l	a3,(a2)
		move.l	a3,d0

		movem.l	(sp)+,d2/a2-a3/a6
		rts

str_comp_ch	move.b	(a1)+,d0
		call	tolower
		move.b	d0,d1
		move.b	(a0)+,d0
		call	tolower
		cmp.b	d0,d1
		bne.s	01$
		tst.b	d0
		bne.s	str_comp_ch
01$		rts

;
; find a variable and return its value in d0 and variable structure
; address in a0. return carry set if not found, else carry clear
; parameters: pointer to null-terminated name in a0
;
;#
;# ->v1.21  1990-01-06
;# variable names are now case insensitive
;#
findvar		move.l	VarList(a4),d1
fvloop		beq.s	varnotfound
		move.l	d1,a1
		movem.l	d1/a0-a1,-(sp)
		lea	var_Name(a1),a1
		bsr.s	str_comp_ch
		movem.l	(sp)+,d1/a0-a1
		beq.s	varfound
		move.l	var_Next(a1),d1
		bra.s	fvloop

varfound	move.l	a1,a0
		move.l	var_Value(a0),d0	;this clears carry
		rts

varnotfound	sec
		rts

;
; remove variable, variable structure pointer in a0
;
deletevar	lea	VarList(a4),a1
01$		cmp.l	(a1),a0
		beq.s	02$
		move.l	(a1),d1
		beq.s	09$
		move.l	d1,a1
		bra.s	01$
02$		exg	a0,a1
		move.l	(a1),(a0)
		moveq	#0,d0
		move.w	var_Length(a1),d0
		move.l	a6,-(sp)
		lib	Exec,FreeMem
		move.l	(sp)+,a6
09$		rts

;
; clear variables
;
		cmd	clearvars

		lea	clvartxt(pc),a0
		call	printstring_a0_window
		bsr	GetKey
		call	tolower
		cmp.b	#'y',d0
		bne.s	cv_end
		bsr	ChrOutWin
		bsr	clear_all_variables
cv_end		moveq	#LF,d0
		bra	ChrOut

novartxt	dc.b	'No variables defined',LF,0
varhead		dc.b	'Variables:',LF,0
varfmt		dc.b	'%s = $%08lx (%ld)',LF,0
clvartxt	dc.b	'Clear vars (y/n)? ',0

		end
