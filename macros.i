;
; macros.i -- Timo Rossi   1989
;

;
; edited 1989-07-15 by TR
;	 1989-07-19 by TR -- added rw and some comments
;	 1989-07-20 by TR -- added LT & GT, changed endvars-macro
;	 1989-08-05 by TR -- added calljmp, libnames & devnames
;	 1989-08-12 by TR -- changed xx's to underscores,
;			     added VarBaseReg, vars, clrvars, enter, return
;	 1989-08-24 by TR -- added FF (form feed)
;	 1989-12-30 by TR -- added clra, edited comments...

; start code section
codestart	macro
		section	ProgramCode,CODE
		endm

VarBaseReg	equr	a5

; load global variable base address
useglobals	macro
		lea	Bss_Start,VarBaseReg
		endm

; define global variable section and tell var-macro to use globals
globalvars	macro
__VarMode	set	0
		section	GlobalVars,BSS
Bss_Start	equ	*
		endm

; tell var-macro to use locals
localvars	macro
__VarMode	set	1
__LocalVarOffs	set	0
		endm

; this really does something only with local variables
endvars	macro
		ifne	__VarMode
		 ifne	__LocalVarOffs&1
__LocalVarOffs	  set	__LocalVarOffs-1
		 endc
		 ifc	'\1',''
LinkValue	  set	__LocalVarOffs
		 endc
		 ifnc	'\1',''
\1		  equ	__LocalVarOffs
		 endc
VarBytes	 set	-__LocalVarOffs
		endc	
	endm

; the following three macros should only be used with local variables
clrvars	macro
	move.l	VarBaseReg,a0
__Belymt	set	(VarBytes/2)-1
	iflt	__Belymt-128
	moveq	#__Belymt,d0
	endc
	ifge	__Belymt-128
	move.w	#__Belymt,d0
	endc
clrv\@	clr.w	-(a0)
	dbf	d0,clrv\@
	endm

;
; enter a program or subroutine, allocate local variables
; from stack using the link-instruction.
; optional parameter specifies register to be saved
;
enter	macro	;[saved-regs]
	link	VarBaseReg,#LinkValue
	ifnc	'\1',''
	movem.l	\1,-(sp)
	endc
	endm

;
; return from program or subroutine, deallocating parameters
; in stack using unlk.
; optional parameter specifies register to be restored
;
return	macro	;[saved-regs]
	ifnc	'\1',''
	movem.l	(sp)+,\1
	endc
	unlk	VarBaseReg
	rts
	endm

;
; this is the main variable defining macro.
; it defines both local and global variables
; and all sizes (.B, .W and .L).
; call:	var.{B|W|L} name,[howmany]
;
var	macro
		ifc	'\2',''
__VarCnt	set	1
		endc
		ifnc	'\2',''
__VarCnt	set	\2
		endc

		ifeq	__VarMode
__\1_\@			ds.\0	__VarCnt
\1			equ	__\1_\@-Bss_Start
			endc
		ifne	__VarMode
			ifc	'\0','B'
__LocalVarOffs			set	__LocalVarOffs-__VarCnt
			endc
			ifc	'\0','W'
__LocalVarOffs			set	__LocalVarOffs-(__VarCnt*2)
				ifne	__LocalVarOffs&1
__LocalVarOffs				set	__LocalVarOffs-1
				endc
			endc
			ifc	'\0','L'
__LocalVarOffs			set	__LocalVarOffs-(__VarCnt*4)
				ifne	__LocalVarOffs&1
__LocalVarOffs				set	__LocalVarOffs-1
				endc
			endc
\1			equ	__LocalVarOffs
		endc
	endm

;
; macro to define multiple variables
; call vars.{B|W|L} name1,name2,name3...
;
vars	macro
	var.\0	\1
	ifnc	'\2',''
	vars.\0	\2,\3,\4,\5,\6,\7,\8,\9
	endc
	endm

;
; call system routines, load base if given two arguments
; call: call [base],routine
;
call	macro
	ifeq	NARG-1
	jsr	_LVO\1(a6)
	endc
	ifeq	NARG-2
	move.l	\1,a6
	jsr	_LVO\2(a6)
	endc
	endm

;
; jump to system routines, load base if given two arguments
; call: calljmp [base],routine
;
calljmp	macro
	ifeq	NARG-1
	jmp	_LVO\1(a6)
	endc
	ifeq	NARG-2
	move.l	\1,a6
	jmp	_LVO\2(a6)
	endc
	endm

;
; call a library routine using the given base, save & restore old a6 value
; call: call2 base,routine
;
call2	macro
	move.l	a6,-(sp)
	move.l	\1,a6
	jsr	_LVO\2(a6)
	move.l	(sp)+,a6
	endm

;
; define library names
; call: libnames library1,library2...
;
libnames	macro
		ifnc	'\1',''
		ifnc	'\1','gfx'
\1Name		dc.b	'\1.library',0
		endc
		ifc	'\1','gfx'
\1Name		dc.b	'graphics.library',0
		endc
		libnames \2,\3,\4,\5,\6,\7,\8,\9
		endc
		endm

;
; define device names
; call: devnames device1,device2...
;
devnames	macro
		ifnc	'\1',''
\1Name		dc.b	'\1.device',0
		devnames \2,\3,\4,\5,\6,\7,\8,\9
		endc
		endm

; well, I really don't use these but they are here anyway...
push	macro
	movem.l	\1,-(sp)
	endm

pop	macro
	movem.l	(sp)+,\1
	endm

; this is for use with the push/pop-macros (or movem...)
all	reg	d0-d7/a0-a6

; clear carry
clc	macro
	and	#%11111110,CCR
	endm

; set carry
sec	macro
	or	#%00000001,CCR
	endm

; force even alignment
even	macro
	ds.w	0
	endm

; clear address register
clra	macro	;An
	suba.l	\1,\1
	endm

; define a string with length byte in beginning
; call: str <string>
str	macro
	dc.b	stryy\@-strxx\@
strxx\@	dc.b	\1
stryy\@
	endm

; define a word (or words) relative to itself
; call: rw label1,label2...
rw	macro
	dc.w	\1-*
	ifnc	'\2',''
	rw	\2,\3,\4,\5,\6,\7,\8,\9
	endc
	endm

; some common ASCII control characters
BS	equ	8
TAB	equ	9
LF	equ	$0a
FF	equ	$0c
CR	equ	$0d
DEL	equ	$7f
ESC	equ	$1b
CSI	equ	$9b

; these two are useful in string used as macro parameters
LT	equ	'<'
GT	equ	'>'

; the only absolute address in the Amiga operating system
ExecBase	equ	4

