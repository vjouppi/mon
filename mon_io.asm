;
; mon_io.asm
;
		nolist
		include "exec/types.i"
		include "libraries/dos.i"
		include "offsets.i"
		list

		include	"monitor.i"
;
; This module defines the following public subroutines
;
;	printf,printstring,printstring_a0,printf_window,printstring_window
;	printstring_a0_window,puts_stdout,fmtstring
;	ChrOutWin,ChrOut,GetKey,GetInput
;
		forward	fmtstring

		xdef	UpAndClearEol

*** SOME SPECIAL KEY CODES (returned by GetKey)
CURSOR_UP		equ	$0100
CURSOR_DOWN		equ	$0200
CURSOR_RIGHT		equ	$0300
CURSOR_LEFT		equ	$0400
SHIFT_CURSOR_UP		equ	$0500
SHIFT_CURSOR_DOWN	equ	$0600
SHIFT_CURSOR_LEFT	equ	$0700
SHIFT_CURSOR_RIGHT	equ	$0800

;
; output routines
;

		pub	printf

		call.s	fmtstring
; fall to printstring
		pub	printstring

		lea	mon_OutputBuf(a4),a0
; fall to printstring_a0
*** Output a string (possibly redirected output) ***
		pub	printstring_a0

		move.l	mon_OutputFile(a4),d1
;
; print text, line at time
; inputs: a0 - pointer to zero-terminated string
; 	  d1 - filehandle
;
print_text	movem.l	d2-d4/a2/a6,-(sp)
		move.l	d1,d4
		getbase	Dos
		move.l	a0,a2

print_loop	move.l	a2,d2
000$		move.b	(a2)+,d0
		beq.s	001$
		cmp.b	#LF,d0
		bne.s	000$
001$		move.l	a2,d3
		sub.l	d2,d3
		move.l	d4,d1
		tst.b	d0
		bne.s	002$
		subq.l	#1,d3
		beq.s	003$
002$		lib	Write
		tst.b	-1(a2)
		bne.s	print_loop
003$		movem.l	(sp)+,d2-d4/a2/a6
		rts

		pub	printf_window

		call.s	fmtstring

		pub	printstring_window

		lea	mon_OutputBuf(a4),a0
*** Output a string to the window ***
* used in error messages etc.
		pub	printstring_a0_window

		move.l	mon_WinFile(a4),d1
		bra.s	print_text

;
; pointer to null-terminated string to be printed in a0
;
		pub	puts_stdout

		movem.l	a2/a6,-(sp)
		move.l	a0,a2
		lib	Dos,Output
		move.l	d0,d1
		beq.s	01$
		move.l	a2,a0
		bsr.s	print_text
01$		movem.l	(sp)+,a2/a6
		rts

;
; format a string in output buffer using RawDoFmt
; arguments in registers d0-d3
;
		pub	fmtstring

		movem.l	a2-a3/a6,-(sp)
		movem.l	d0-d4,-(sp)
		move.l	sp,a1
		lea	putch(pc),a2
		lea	mon_OutputBuf(a4),a3
		lib	Exec,RawDoFmt
		lea	20(sp),sp
		movem.l	(sp)+,a2-a3/a6
		rts

; character output routine for RawDoFmt
putch		move.b	d0,(a3)+
		rts

; Output a character to the window
		pub	ChrOutWin

		move.l	mon_WinFile(a4),D1
		bra.s	xChrOut

; output a character to current output
		pub	ChrOut

		move.l	mon_OutputFile(a4),d1

xChrOut		movem.l	D2-D3/a6,-(sp)
		move.b	d0,-(sp)
		move.l	sp,d2
		moveq	#1,D3
		lib	Dos,Write
		addq.l	#2,sp
		movem.l	(sp)+,D2-D3/a6
		rts

**** Get a character ****
GetChar		movem.l	D2-D3/a6,-(sp)
		move.l	mon_WinFile(a4),D1
		clr.b	-(sp)
		move.l	sp,d2
		moveq	#1,d3
		lib	Dos,Read
		moveq	#0,D0
		move.b	(sp)+,D0
		movem.l	(sp)+,D2-D3/a6
		rts

		pub	GetKey
* get a word value describing the key pressed
* if high byte is zero, this is the ASCII-code of the key
* else it is a special code or -1 if key is not recognised
* by this routine (for example the function keys)
;
; ESC [ is accepted as CSI (for vt100-compatible terminal use)
;
; ESC CSI or ESC ESC does not confuse this routine any more...tr 1991-04-18
;
		bsr.s	GetChar
		cmp.b	#ESC,d0
		bne.s	01$
00$		bsr.s	GetChar
		cmp.b	#CSI,d0
		beq.s	02$
		cmp.b	#ESC,d0
		beq.s	00$
		cmp.b	#'[',d0
		bne.s	key99
		bra.s	02$

01$		cmp.b	#CSI,D0
		bne.s	ret9
02$		bsr.s	GetChar
		cmp.b	#'A',D0
		bcs.s	key1
		cmp.b	#'D',D0
		bhi.s	key1
		sub.b	#$40,D0
		asl.w	#8,D0
		rts	;codes $0100..$0400	;cursor up/down/right/left
key1		cmp.b	#'S',D0
		bne.s	key2
		move.w	#SHIFT_CURSOR_DOWN,D0
		rts
key2		cmp.b	#'T',D0
		bne.s	key3
		move.w	#SHIFT_CURSOR_UP,D0
		rts
key3		cmp.b	#SPACE,D0
		bne.s	key8
		bsr.s	GetChar
		cmp.b	#'A',D0
		bne.s	key4
		move.w	#SHIFT_CURSOR_LEFT,D0
		rts
key4		cmp.b	#'@',D0
		bne.s	key9
		move.w	#SHIFT_CURSOR_RIGHT,D0
		rts
key8		cmp.b	#'?',D0
		bne.s	key9
		bsr	GetChar
		move.w	#'??',D0	;THE HELP KEY
		rts
key9		cmp.b	#'~',D0
		beq.s	key99
		cmp.b	#'|',d0		;close gadget in 2.0...
		beq.s	key90
		bsr	GetChar
		bra.s	key9
key90		move.w	#'CL',d0
		rts
key99		moveq	#-1,D0	;unknown key
ret9		rts

;
; jump table for input routine
;
inpjump		macro
		rw	(\2)-4
		dc.w	\1
		endm

input_jumps	inpjump	CR,input_end
		inpjump	CtrlE,handle_CtrlE
		inpjump	CtrlX,handle_CtrlX
		inpjump	CURSOR_LEFT,moveleft
		inpjump	CURSOR_RIGHT,moveright
		inpjump	CURSOR_UP,previousline
		inpjump	CURSOR_DOWN,nextline
		inpjump	SHIFT_CURSOR_LEFT,left_edge
		inpjump	SHIFT_CURSOR_RIGHT,rightedge
		inpjump	SHIFT_CURSOR_UP,prev_line_ret
		inpjump	BS,backspace
		inpjump	DEL,delchar
		inpjump	<'??'>,inp_help
		inpjump	<'CL'>,inp_close
		dc.l	0


**** THE INPUT ROUTINE ****
* special code in D0
* 0 = normal operation
* 1 = respond ctrl-e (assembler)
* 2 = edit existing line (created by disassembler)
* returns the special code or -1 if Ctrl-E pressed
*********
;
; inpspecial moved to d3 -- tr 1991-04-18 (v1.38)
;
; this may modify any register...it actually now modifies d0-d7/a0-a3/a6
;
		pub	GetInput

		btst	#OPTB_DUMBTERM,mon_Options(a4)
		bne	Dumb_GetInput

		move.w	D0,d3
		moveq	#0,D4
		lea	mon_InputBuf(a4),a2
		moveq	#0,D5
		moveq	#0,D7	;length
		cmp.w	#2,D0
		bne.s	inp0
		move.l	a2,A0
		call	printstring_a0_window
		move.l	a2,A0
inp0len		tst.b	(A0)+
		bne.s	inp0len
		sub.l	a2,A0
		subq.l	#1,A0
		move.l	A0,D7
inp0		move.l	D7,D6	;current position
input_loop	call	GetKey

		lea	input_jumps(pc),a0
inp_j1		move.l	(a0)+,d1
		beq.s	inp_j2
		cmp.w	d0,d1
		bne.s	inp_j1
		swap	d1
		add.w	d1,a0
		jmp	(a0)

handle_CtrlE	tst.w	d3		;ctrl-e - edit mode (if allowed)
		beq.s	inp_j2
		bsr	eraseline
		moveq	#-1,d3
		bra	input_end

handle_CtrlX	bsr	eraseline	;ctrl-x - clear input line
		bra.s	input_loop

left_edge	bsr	gotoleftedge	;shift-cursor-left
		bra.s	input_loop

inp_help	moveq	#-1,D5		;help
		moveq	#'h',d0
		bra.s	put_char_to_input

inp_close	moveq	#-1,d5		;close gadget (hopefully)
		moveq	#'x',d0
		bra.s	put_char_to_input

prev_line_ret	moveq	#-1,D5		;shift-cursor-up
		bra	previousline

inp_j2		cmp.w	#SPACE,D0
		bcs	input_loop
		cmp.w	#DEL,D0
		bcs.s	put_char_to_input
		cmp.w	#$A0,D0
		bcs	input_loop
		cmp.w	#$100,d0
		bcc	input_loop

put_char_to_input
		cmp.w	#64,D7		;is input line full?
		bcc	input_loop
		cmp.l	D7,D6
		bhi	input_loop
		beq.s	iputchr
		move.l	D7,D2

; make room for a new character
080$		move.b	-1(a2,D2.L),0(a2,D2.L)
		subq.l	#1,D2
		cmp.l	D2,D6
		bne.s	080$

iputchr		move.b	D0,D2
		lea	insert_char_txt(pc),a0
		call	printstring_a0_window
		move.b	D2,D0
		call	ChrOutWin
		move.b	D2,0(a2,D6.L)
		addq.l	#1,D6
		addq.l	#1,D7
		tst.l	D5	;auto-CR flag
		beq	input_loop

input_end	clr.b	0(a2,D7.L)
		move.l	a2,A3
		call	skipspaces
		tst.b	(A3)
		beq.s	inp99

		lea	mon_History(a4),A0
		move.w	#(NLINES-1)*LEN/4-1,D0
099$		move.l	LEN(A0),(A0)+
		dbf	D0,099$		;scroll command line history to make space for current line

100$		move.b	(a2)+,(a0)+	;add current line to command line history
		bne.s	100$

inp99		tst.w	d3
		bmi.s	inp99a
		emitwin	LF
inp99a		move.w	d3,D0
		rts

rightedge	;Shift-Cursor right
		cmp.l	D6,D7
		beq	input_loop
		moveq	#0,d0
		move.b	D7,D0
		sub.b	D6,D0
		move.l	D7,D6
right01		lea	go_right_fmt(pc),a0
		call	printf_window
		bra	input_loop

moveleft	tst.l	D6
		beq	input_loop
		subq.l	#1,D6
		emitwin	BS
		bra	input_loop

moveright	cmp.l	D6,D7
		beq	input_loop
		addq.l	#1,D6
		moveq	#0,d0
		bra.s	right01

backspace	tst.l	D6
		beq	input_loop
		subq.l	#1,D6
		emitwin	BS
delchar		cmp.l	D6,D7
		beq	input_loop
		lea	del_char_txt(pc),a0
		call	printstring_a0_window
		subq.l	#1,D7
		cmp.l	D6,D7
		beq	input_loop
		move.l	D6,D0

105$		move.b	1(a2,D0.L),0(a2,D0.L)
		addq.l	#1,D0
		cmp.l	D0,D7
		bne.s	105$

		bra	input_loop

*** COMMAND LINE HISTORY ***
nextline	bsr.s	eraseline
		moveq	#1,D1
		bra.s	prnxl1

previousline	bsr.s	eraseline
		moveq	#-1,D1
prnxl1		add.b	D1,D4
		bmi.s	linct_neg
		cmp.b	#NLINES,D4
		bcs.s	linct_ok
		moveq	#0,D4
		bra.s	linct_ok

linct_neg	move.b	#NLINES-1,D4

linct_ok	move.b	D4,D0
		lea	mon_History(a4),A0
		ext.w	D0
		mulu	#LEN,D0
		add.l	D0,A0

prevloop	move.b	(A0)+,D0
		beq.s	prev2
		move.b	D0,0(a2,D7.L)
		addq.l	#1,D7
		bra.s	prevloop
prev2		clr.b	0(a2,D7.L)
		move.l	a2,A0
		call	printstring_a0_window
		move.l	D7,D6
		tst.l	D5
		bne	input_end
		bra	input_loop

gotoleftedge	;Shift-Cursor left
		tst.l	D6
		beq.s	lef9
		moveq	#0,d0
		move.b	d6,d0
		lea	go_left_fmt(pc),a0
		call	printf_window
lef9		moveq	#0,D6
		rts

eraseline	bsr.s	gotoleftedge
		moveq	#0,D7
		lea	clear_eol_txt(pc),a0
		call	JUMP,printstring_a0_window

;
; GetInput for dumb terminal
;
Dumb_GetInput	lea	mon_InputBuf(a4),a2
		move.l	a2,a3
		moveq	#0,d7

dumb_input_loop	bsr	GetChar
		cmp.b	#CR,d0
		beq	dumb_input_end
		cmp.b	#CtrlX,d0
		beq	dumb_delline
		cmp.b	#BS,d0
		beq.s	dumb_delch
		cmp.b	#DEL,d0
		bne.s	dumb_input1

dumb_delch	cmp.l	a3,a2
		beq.s	dumb_input_loop
		moveq	#0,d3

dumb_delch_loop	subq.l	#1,a2
		subq.w	#1,d7
		lea	dumb_delch_txt(pc),a0
		call	printstring_a0_window
dumb_delch1	dbf	d3,dumb_delch_loop
		bra.s	dumb_input_loop

dumb_delline	move.w	d7,d3
		bra.s	dumb_delch1

dumb_input1	cmp.b	#$20,d0
		bcs.s	dumb_input_loop
		cmp.b	#$7e,d0
		bhi.s	dumb_input_loop
		cmp.w	#64,d7
		bcc.s	dumb_input_loop

		move.b	d0,(a2)+
		addq.w	#1,d7
		call	ChrOutWin
		bra.s	dumb_input_loop

dumb_input_end	clr.b	(a2)
		call	skipspaces
		emitwin	LF
		moveq	#0,d0
		rts

UpAndClearEol	dc.b	ESC,'[A'
clear_eol_txt	dc.b	ESC,'[K',0
del_char_txt	dc.b	ESC,'[P',0
insert_char_txt	dc.b	ESC,'[@',0
go_left_fmt	dc.b	ESC,'[%ldD',0
go_right_fmt	dc.b	ESC,'[%ldC',0
dumb_delch_txt	dc.b	BS,' ',BS,0

		end
