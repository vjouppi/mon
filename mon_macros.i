;
; mon_macros.i
;

; clear carry
clc		macro
		and	#%11111110,CCR
		endm

; set carry
sec		macro
		or	#%00000001,CCR
		endm

; force even alignment
even		macro
		ds.w	0
		endm

; clear address register
clra		macro	;An
		suba.l	\1,\1
		endm

; define a string with length byte in beginning
; call: str <string>
str		macro
		dc.b	stryy\@-strxx\@
strxx\@		dc.b	\1
stryy\@
		endm

; define a word (or words) relative to itself
; call: rw label1,label2...
rw		macro
		dc.w	\1-*
		ifnc	'\2',''
		rw	\2,\3,\4,\5,\6,\7,\8,\9
		endc
		endm

; the only absolute address in the Amiga operating system
_AbsExecBase	equ	4

;
; library call macros
;

;
; get library base to a register, default is a6
;
getbase		macro
		ifc	'\1','AbsExec'
		  ifeq	NARG-2
		    move.l _AbsExecBase,\2
		  endc
		  ifeq	NARG-1
		    move.l _AbsExecBase,a6
		  endc
		endc
		ifnc	'\1','AbsExec'
		  ifeq	NARG-2
		    move.l _\1Base(a4),\2
		  endc
		  ifeq	NARG-1
		    move.l _\1Base(a4),a6
		  endc
		endc
		endm

;
; call Amiga library routine
;
; call name     ->  jsr _LVOname(a6)
; call lib,name -> getbase lib; jsr _LVOname(a6)
;
lib		macro
		ifeq	NARG-2
		  getbase	\1
		  lib	\2
		endc
		ifeq	NARG-1
		  jsr	_LVO\1(a6)
		endc
		endm

;
; jump to Amiga library routine
;
jlib		macro
		ifeq	NARG-2
		  getbase  \1
		  jlib	\2
		endc
		ifeq	NARG-1
		  jmp	_LVO\1(a6)
		endc
		endm

;
; call Amiga library routine, preserve value of a6
;
slib		macro
		move.l  a6,-(sp)
		lib	\1,\2
		move.l  (sp)+,a6
		endm

;
; define a public routine
;
pub		macro
\1_routine	;equ	*
		ifnd	\1_routine_flag
		  xdef	\1_routine
\1_routine_flag	  set	1
		endc
		endm

;
; this macro must be used if a public routine is used in the same
; source file as it has been defined before the definition.
;
forward		macro	;public_routine_name
		ifnd	\1_routine_flag
		xdef	\1_routine
\1_routine_flag	  set	1
		endc
		endm

;
; call a public routine
;
;   call name      ->  bsr name_routine
;   call JUMP,name ->  bra name_routine
;
call		macro
		ifnc	'\1','JUMP'
		  ifnd	\1_routine_flag
		     xref  \1_routine
\1_routine_flag	     set   1
		  endc
		  ifc '\0',''
		    bsr \1_routine
		  endc
		  ifnc '\0',''
		    bsr.\0 \1_routine
		  endc
		endc
		ifc	'\1','JUMP'
		  ifnd	\2_routine_flag
		    xref  \2_routine
\2_routine_flag     set   1
		  endc
		  ifc '\0',''
		    bra \2_routine
		  endc
		  ifnc '\0',''
		    bra.\0 \2_routine
		  endc
		endc
		endm

