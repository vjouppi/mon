;
; mon_misc.asm
;

;
; NOTE: this must be the last module linked in
;
		nolist
		include "exec/types.i"
		include "libraries/dos.i"
		include "devices/conunit.i"
		include "offsets.i"
		list

		include	"monitor.i"
;
; This module defines the following public subroutines:
;
;	GetString,CheckKeys,GetParams,CheckEnd,showrange
;

		xdef	monitor_code_end

;
;start addr in D5, length in D6
;
		pub	showrange

		move.l	d5,d1
		move.l	d6,d0
		move.l	d1,d2
		add.l	d0,d2
		subq.l	#1,d2
		lea	range_fmt(pc),a0
		call	JUMP,printf

*** GET STRING FROM INPUT LINE TO ADDR IN A0, LENGTH IN D2 ***
* NOTE: this version requires commas between numbers and strings
; a1 is end addr of string on return

		pub	GetString

		move.l	A0,A1
gstr1		call	tolower
		move.b	(A3)+,D0
		beq.s	gstr9		;branch if end of input line
		cmp.b	#'''',D0	;if single quote, then string follows
		beq.s	stringi
		subq.l	#1,A3
		call	GetExpr		;else get number
		move.b	D0,(A1)+
gstr1a		call	tolower
		cmp.b	#COMMA,(a3)+
		bne.s	gstr9			;if comma not found, then end
		bra.s	gstr1
stringi		move.b	(A3)+,D0
		beq.s	gstr9
		cmp.b	#'''',D0
		beq.s	gstr2
string1		move.b	D0,(A1)+
		bra.s	stringi
gstr2		cmp.b	#'''',(A3)+	;test for double-single-quote
		beq.s	string1
		subq.l	#1,A3
		bra.s	gstr1a
gstr9		move.l	A1,D2
		sub.l	A0,D2		;calculate length of the string in D2
		rts

**** WAIT IF SPACE PRESSED, BREAK IF CTRL-C (status non-zero) ****
;
; 1991-09-01 --> now works also with Ctrl-S/Ctrl-Q
;
		pub	CheckKeys

		movem.l	D2/a6,-(sp)
;#
;# check for SIGBREAK... also... GMC does not seem to pass CTRL-C
;# to Read() in RAW mode...
;#
		moveq	#0,d0
		moveq	#0,d1
		lib	Exec,SetSignal
		btst	#SIGBREAKB_CTRL_C,d0
		bne.s	break
;#
;# is timeout zero really safe?...I have found no problems yet...
;#
		move.l	mon_WinFile(a4),D1
		moveq	#0,D2		;timeout=0
		lib	Dos,WaitForChar
		tst.l	D0
		beq.s	nobreak		;branch if no key pressed
		call	GetKey
		cmp.w	#CtrlS,d0
		beq.s	waitspc
		cmp.w	#SPACE,D0
		bne.s	nospc

waitspc		;space pressed, wait for another space or Ctrl-C
		call	GetKey
		cmp.w	#CtrlC,D0
		beq.s	break
		cmp.w	#CtrlQ,d0
		beq.s	nobreak
		cmp.w	#SPACE,D0
		bne.s	waitspc
		bra.s	nobreak

nospc		cmp.w	#CtrlC,D0
		bne.s	nobreak
break		lea	break_txt(pc),A0	;message '*** break ***'
		call	printstring_a0_window
		moveq	#-1,D0
		bra.s	brk_1

nobreak		moveq	#0,D0
brk_1		movem.l	(sp)+,D2/a6
		rts

**** PARAMETERS FOR DISPLAY AND DISASSEMBLE ****
* get Addr and EndAddr & number of lines to display in D7
* if D7 is zero then display from Addr to EndAddr
* if D7 is non-zero EndAddr is ignored
		pub	GetParams

		call	tolower
		tst.b	(A3)
		beq.s	param9
		call	GetExpr
		move.l	D0,mon_CurrentAddr(a4)
		call	tolower
		tst.b	(A3)
		beq.s	param9
		call	GetExpr
		move.l	D0,mon_EndAddr(a4)
		moveq	#0,D7
		rts

param9		clr.l	mon_EndAddr(a4)
		moveq	#20,D7
		move.l	mon_OutputFile(a4),d0
		cmp.l	mon_WinFile(a4),d0
		bne.s	09$
;
; get the number of text lines that will fit in the window
; from the console device unit structure
;
		move.l	mon_ConsoleUnit(a4),d0
		beq.s	09$

		move.l	d0,a0
		move.w	cu_YMax(a0),d7
		subq.w	#2,d7
		moveq	#1,d0
		cmp.w	d0,d7
		bcc.s	09$
		move.w	d0,d7
09$		rts

;
;**** CHECK IF WE SHOULD STOP MEMDISPLAY OR DISASSEMBLE ****
;* if D7 is non-zero decrement it, if it becomes zero then stop
;* else if Addr>EndAddr then stop
;* return Z-flag set if we should stop
;
		pub	CheckEnd

		call	CheckKeys
		bne.s	stop1
		tst.l	mon_EndAddr(a4)
		beq.s	no_addr_test
		cmp.l	mon_EndAddr(a4),a5
		bhi.s	stop1

no_addr_test	tst.l	D7
		beq.s	cont
		subq.l	#1,D7
		rts

stop1		moveq	#0,D0
		rts

cont		moveq	#-1,D0
		rts

**** text data ****

range_fmt	dc.b	'%ld bytes read from $%08lx to $%08lx',LF,0
break_txt	dc.b	'*** Break ***',LF,0

		cnop	0,4

monitor_code_end

		END
