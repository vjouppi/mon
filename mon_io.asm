;
; mon_io.asm
;

		include	"monitor.i"

		xdef	UpAndClearEol

		xdef	putch
		xdef	GetKey
		xdef	ChrOut
		xdef	ChrOutWin
;
; output routines
;

		pub	printf

		bsr.s	fmtstring_routine	;;;;
; fall to printstring
		pub	printstring

		lea	OutputBuf(a4),a0
; fall to printstring_a0
*** Output a string (possibly redirected output) ***
		pub	printstring_a0

		move.l	OutputFile(a4),d1
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

		bsr.s	fmtstring_routine	;;;;

		pub	printstring_window

		lea	OutputBuf(a4),a0
*** Output a string to the window ***
* used in error messages etc.
		pub	printstring_a0_window

		move.l	WinFile(a4),d1
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
		movem.l	d0-d3,-(sp)
		move.l	sp,a1
		lea	putch(pc),a2
		lea	OutputBuf(a4),a3
		lib	Exec,RawDoFmt
		lea	16(sp),sp
		movem.l	(sp)+,a2-a3/a6
		rts

; character output routine for RawDoFmt
putch		move.b	d0,(a3)+
		rts

; Output a character to the window
ChrOutWin	move.l	WinFile(a4),D1
		bra.s	xChrOut
; output a character to current output
ChrOut		move.l	OutputFile(a4),d1
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
		move.l	WinFile(a4),D1
		clr.b	-(sp)
		move.l	sp,d2
		moveq	#1,d3
		lib	Dos,Read
		moveq	#0,D0
		move.b	(sp)+,D0
		movem.l	(sp)+,D2-D3/a6
		rts

GetKey
* get a word value describing the key pressed
* if high byte is zero, this is the ASCII-code of the key
* else it is a special code or -1 if key is not recognised
* by this routine (for example the function keys)
		bsr.s	GetChar
		cmp.b	#ESC,d0
		bne.s	01$
		bsr.s	GetChar
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
		bsr	GetChar
		bra.s	key9
key99		moveq	#-1,D0	;unknown key
ret9		rts

**** THE INPUT ROUTINE ****
* special code in D0
* 0 = normal operation
* 1 = respond ctrl-e (assembler)
* 2 = edit existing line (created by disassembler)
* returns the special code or -1 if Ctrl-E pressed
*********
; this may modify any register...
		pub	GetInput

		move.w	D0,inpspecial(a4)
		moveq	#0,D4
		lea	InputBuf(a4),a5
		moveq	#0,D5
		moveq	#0,D7	;length
		cmp.w	#2,D0
		bne.s	inp0
	;;!!	lea	ClearEol(pc),a0		erase to end of line
	;;!!	call	printstring_a0_window
		move.l	a5,A0
		call	printstring_a0_window
		move.l	a5,A0
inp0len		tst.b	(A0)+
		bne.s	inp0len
		sub.l	a5,A0
		subq.l	#1,A0
		move.l	A0,D7
inp0		move.l	D7,D6	;current position
inp1		bsr	GetKey
		cmp.w	#CR,D0	;return
		beq	inp9
		cmp.w	#CtrlE,D0
		bne.s	noCtrlE
		tst.w	inpspecial(a4)
		beq.s	noCtrlE
		bsr	eraseline
		move.w	#-1,inpspecial(a4)
		bra	inp9

noCtrlE		cmp.w	#CtrlX,D0	;Ctrl-x clears the input line
		bne.s	noCtrlX
		bsr	eraseline
		bra.s	inp1

noCtrlX		cmp.w	#CURSOR_RIGHT,D0
		beq	moveright
		cmp.w	#CURSOR_LEFT,D0	;cursor left
		beq	moveleft
		cmp.w	#SHIFT_CURSOR_LEFT,D0
		bne.s	noleftedge
		bsr	gotoleftedge
		bra.s	inp1

noleftedge	cmp.w	#'??',D0
		bne.s	nohelp
		moveq	#-1,D5
		moveq	#'h',d0
		bra.s	put_char_to_input

nohelp		cmp.w	#SHIFT_CURSOR_UP,D0
		bne.s	no_do_prev
		moveq	#-1,D5
		bra	previousline

no_do_prev	cmp.w	#SHIFT_CURSOR_RIGHT,D0
		beq	rightedge
		cmp.w	#CURSOR_UP,D0	;cursor up--previous input line
		beq	previousline
		cmp.w	#CURSOR_DOWN,D0
		beq	nextline
		cmp.w	#BS,D0	;backspace
		beq	backspace
		cmp.w	#DEL,D0	;delete
		beq	delchar
		cmp.w	#SPACE,D0
		bcs	inp1
		cmp.w	#DEL,D0
		bcs.s	put_char_to_input
		cmp.w	#$A0,D0
		bcs	inp1
		tst.w	d0
		bmi	inp1

put_char_to_input
		cmp.w	#64,D7		;is input line full?
		bcc	inp1
		cmp.l	D7,D6
		bhi	inp1
		beq.s	iputchr
		move.l	D7,D2

; make room for a new character
080$		move.b	-1(a5,D2.L),0(a5,D2.L)
		subq.l	#1,D2
		cmp.l	D2,D6
		bne.s	080$

iputchr		move.b	D0,D2
		lea	insch(pc),a0
		call	printstring_a0_window
		move.b	D2,D0
		bsr	ChrOutWin
		move.b	D2,0(a5,D6.L)
		addq.l	#1,D6
		addq.l	#1,D7
		tst.l	D5	;auto-CR flag
		beq	inp1

inp9		clr.b	0(a5,D7.L)
		move.l	a5,A3
		call	skipspaces
		tst.b	(A3)
		beq.s	inp99

		lea	History(a4),A0
		move.w	#(NLINES-1)*LEN/4-1,D0
099$		move.l	LEN(A0),(A0)+
		dbf	D0,099$		;scroll command line history to make space for current line

100$		move.b	(a5)+,(a0)+	;add current line to command line history
		bne.s	100$

inp99		tst.w	inpspecial(a4)
		bmi.s	inp99a
		emitwin	LF
inp99a		move.w	inpspecial(a4),D0
		rts

rightedge	;Shift-Cursor right
		cmp.l	D6,D7
		beq	inp1
		moveq	#0,d0
		move.b	D7,D0
		sub.b	D6,D0
		move.l	D7,D6
right01		lea	go_right_fmt(pc),a0
		call	printf_window
		bra	inp1

moveleft	tst.l	D6
		beq	inp1
		subq.l	#1,D6
		emitwin	BS
		bra	inp1

moveright	cmp.l	D6,D7
		beq	inp1
		addq.l	#1,D6
		moveq	#0,d0
		bra.s	right01

backspace	tst.l	D6
		beq	inp1
		subq.l	#1,D6
		emitwin	BS
delchar		cmp.l	D6,D7
		beq	inp1
		lea	delch(pc),a0
		call	printstring_a0_window
		subq.l	#1,D7
		cmp.l	D6,D7
		beq	inp1
		move.l	D6,D0

105$		move.b	1(a5,D0.L),0(a5,D0.L)
		addq.l	#1,D0
		cmp.l	D0,D7
		bne.s	105$

		bra	inp1

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
		lea	History(a4),A0
		ext.w	D0
		mulu	#LEN,D0
		add.l	D0,A0

prevloop	move.b	(A0)+,D0
		beq.s	prev2
		move.b	D0,0(a5,D7.L)
		addq.l	#1,D7
		bra.s	prevloop
prev2		clr.b	0(a5,D7.L)
		move.l	a5,A0
		call	printstring_a0_window
		move.l	D7,D6
		tst.l	D5
		bne	inp9
		bra	inp1
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
		lea	ClearEol(pc),a0
		call	JUMP,printstring_a0_window

UpAndClearEol	dc.b	ESC,'[A'
ClearEol	dc.b	ESC,'[K',0
delch		dc.b	ESC,'[P',0
insch		dc.b	ESC,'[@',0
go_left_fmt	dc.b	ESC,'[%ldD',0
go_right_fmt	dc.b	ESC,'[%ldC',0
cls_str		dc.b	ESC,'[H',ESC,'[J',0

		end
