;
; variables.asm
;
		nolist
		include	"exec/types.i"
		include	"exec/memory.i"
		include "offsets.i"
		list

		include	"monitor.i"
		include "variables.i"

;
; This module defines the following command routines:
;
;	setvariable,clearvars
;
; and the following public subroutines:
;
;	clear_all_variables,clear_hunk_vars,find_variable
;	set_variable,put_label
;

		xref	generic_error
		xref	out_memory_error


; clear all variables
; called before exit and when the cv-command is executed
; changes d2 and a6
		pub	clear_all_variables

		getbase	Exec
		move.l	mon_VarList(a4),d2

clrvarloop	tst.l	d2
		beq.s	cleared_vars
		move.l	d2,a1
		moveq	#0,d0
		move.w	var_Length(a1),d0
		move.l	(a1),d2
		lib	FreeMem
		bra.s	clrvarloop

cleared_vars	clr.l	mon_VarList(a4)
		rts

; 
; clear all hunk-associated variables. called from 'u'-command
; 
		pub	clear_hunk_vars

		getbase	Exec
		lea	mon_VarList(a4),a2
		move.l	(a2),d2

clrhvarloop	tst.l	d2
		beq.s	cleared_hvars
		move.l	d2,a1
		tst.w	var_HunkNum(a1)
		bmi.s	no_clrvar

		move.l	(a1),(a2)	;fix next-pointer

		moveq	#0,d0
		move.w	var_Length(a1),d0

		move.l	(a1),d2

		lib	FreeMem
		bra.s	clrhvarloop

no_clrvar	move.l	d2,a2
		move.l	(a1),d2
		bra.s	clrhvarloop

cleared_hvars	rts

;
; set [var=expr]
;
		cmd	setvariable

		tst.b	(a3)
		bne	set_var_cmd

		move.l	mon_VarList(a4),d5
		bne.s	showvar1
		lea	no_variables_txt(pc),a0
		call	printstring_a0_window	;'no variables defined'
		bra.s	xvar_end

showvar1	lea	var_head_txt(pc),a0
		call	printstring_a0

showvarloop	move.l	d5,a2
		lea	var_Name(a2),a0
		move.l	a0,d0
		move.l	var_Value(a2),d1
		move.l	d1,d2

		lea	var_fmt1(pc),a0
		tst.w	var_HunkNum(a2)
		bmi.s	1$

		moveq	#0,d2
		move.w	var_HunkNum(a2),d2
		lea	var_fmt2(pc),a0

1$		call	printf
		call	CheckKeys
		bne.s	xvar_end
		move.l	(a2),d5
		bne.s	showvarloop

xvar_end	rts

;
; set a variable. variable name may begin with '@', '.' ,'_' or alphabetic
; character and contain '_' and alphanumeric characters.
;
set_var_cmd	move.b	(a3),d0
		cmp.b	#'@',d0
		beq.s	00$
		cmp.b	#'.',d0
		beq.s	00$
		call	isalpha
		bcc	generic_error

00$		move.l	a3,a5
		addq.l	#1,a3

;
; variable names can contain '.' and '$'
;
01$		move.b	(a3)+,d0
		cmp.b	#'.',d0
		beq.s	01$
		cmp.b	#'$',d0
		beq.s	01$
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

02$		call	GetExpr
		clr.b	(a2)

		moveq	#-1,d1
		tst.b	(a3)
		beq.s	03$

		move.l	d0,-(sp)
		call	GetExpr
		move.w	d0,d1
		move.l	(sp)+,d0

03$		move.l	a5,a0
		call.s	set_variable
		tst.l	d0
		beq	out_memory_error
		bra.s	xvar_end

remvar		move.l	a5,a0
		clr.b	(a2)
		bsr	find_variable
		bcs.s	xvar_end
		bsr	deletevar
		bra.s	xvar_end

;
; set value of existing or new variable
; a0 - name of variable, null-terminated
; d0 - value of variable
; d1 - hunk number or -1 if none
;
; returns zero if fails, nonzero if successfull
;
		pub	set_variable

		movem.l	d0/d1/a0,-(sp)
		bsr	find_variable
		bcc.s	setvar1
		move.l	8(sp),a0
		bsr.s	addvar
		tst.l	d0
		beq.s	setvar_ret
		move.l	d0,a0
setvar1		movem.l	(sp)+,d0/d1/a1		;a1 value no used...
		move.l	d0,var_Value(a0)
		move.w	d1,var_HunkNum(a0)
		moveq	#1,d0	;TRUE: successfull
setvar_ret	rts

;
; add a new variable
; a0 - null-terminated name
; return variable pointer in d0 or zero if out of memory
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
		beq	addvar_ret
		move.l	d0,a3
		move.w	d2,var_Length(a3)
		lea	var_Name(a3),a0
05$		move.b	(a2)+,(a0)+
		bne.s	05$

;
; insert to the list. keep list sorted.
;#
;# ->v1.21 ... sorting is now case insensitive
;# ->v1.47 ... case sensitive again
;#
;
		lea	mon_VarList(a4),a2
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
addvar_ret	rts

;
; compare strings (case sensitive)
; string pointers in a0 and a1
;
; note: '.' is special. it comes last in comparison.
;
str_comp_ch	move.b	(a1)+,d1
		cmp.b	#'.',d1
		bne.s	1$
		st	d1
1$		move.b	(a0)+,d0
		cmp.b	#'.',d0
		bne.s	2$
		st	d0
2$		cmp.b	d0,d1
		bne.s	str_comp_end
		tst.b	d0
		bne.s	str_comp_ch
str_comp_end	rts

;
; find a variable and return its value in d0 and variable structure
; address in a0. return carry set if not found, else carry clear
;
; parameters: pointer to null-terminated name in a0
;
;#
;# -> v1.21  1990-01-06
;# variable names are now case insensitive
;#
;# -> v1.47 -> case sensitive again
;#
		pub	find_variable

find_variable	move.l	mon_VarList(a4),d1

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
deletevar	lea	mon_VarList(a4),a1
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

		lea	clear_var_txt(pc),a0
		call	printstring_a0_window
		call	GetKey
		call	tolower
		cmp.b	#'y',d0
		bne.s	cv_end
		call	ChrOutWin
		call	clear_all_variables
cv_end		moveq	#LF,d0
		call	JUMP,ChrOut


;
; check if there is a symbol with the given value, if true, display symbol name
; used to add labels in the disassembly listing.
;
		pub	put_label

		tst.l	mon_HunkTypeTable(a4)
		beq.s	1$

		call.s	find_var_value
		tst.l	d0
		beq.s	1$
		move.l	d0,a0
		lea	var_Name(a0),a0
		move.l	a0,d0
		lea	label_fmt(pc),a0
		call	printf
1$		rts

;
; find variable with a given value (and non-$ffff hunknum)
; inputs: value (address) in d0
; returns: pointer to variable structure in d0, or zero if failed
;
		pub	find_var_value

		move.l	d0,d1
		move.l	mon_VarList(a4),d0

fv_loop		beq.s	fv_ret			;not found
		move.l	d0,a0
		cmp.l	var_Value(a0),d1
		bne.s	fv_next
		tst.w	var_HunkNum(a0)
		bpl.s	fv_ret			;found

fv_next		move.l	(a0),d0
		bra.s	fv_loop

fv_ret		rts

;
; set/show address register (normally a4) relative base address
;
		cmd	setshow_relbase

		tst.b	(a3)
		beq.s	show_relbase

		moveq	#4,d3		;register: default a4

		call	GetExpr		;get address
		move.l	d0,d2
		beq	relbase_off

		tst.b	(a3)
		beq.s	set_relbase

		move.b	(a3)+,d0
		cmp.b	#'A',d0
		beq.s	1$
		cmp.b	#'a',d0
		bne	generic_error

1$		move.b	(a3)+,d3
		sub.b	#'0',d3
		bcs	generic_error
		cmp.b	#8,d3
		bcc	generic_error

set_relbase	move.l	d2,mon_RelBaseAddr(a4)
		move.b	d3,mon_RelBaseReg(a4)
		rts

relbase_off	st	mon_RelBaseReg(a4)
		rts

show_relbase	moveq	#0,d1
		move.b	mon_RelBaseReg(a4),d1
		bmi.s	no_relbase

		move.l	mon_RelBaseAddr(a4),d0
		lea	relbase_fmt(pc),a0
		call	JUMP,printf

no_relbase	lea	no_relbase_txt(pc),a0
		call	JUMP,printstring_a0

;
; strings...
;
no_variables_txt
		dc.b	'No variables defined',LF,0
var_head_txt	dc.b	'Variables:',LF,0
var_fmt1	dc.b	'%-10.50s = $%08lx (%ld)',LF,0
var_fmt2	dc.b	'%-20.50s = $%08lx  [%ld]',LF,0
clear_var_txt	dc.b	'Clear vars (y/n)? ',0
label_fmt	dc.b	'%-1.50s:',LF,0
no_relbase_txt	dc.b	'No base reg/addr defined',LF,0
relbase_fmt	dc.b	'Relative base addr $%08lx, register A%ld',LF,0

		end
