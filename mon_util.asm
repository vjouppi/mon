;
; mon_util.asm
;
;
		include	"monitor.i"

		xdef	put_hexnum
		xdef	put_hexnum1
		xdef	puthex_68
		xdef	puthex1_68
		xdef	phex1_8

;
; Convert char in D0 to lower case ***
;
		pub	tolower

tolower		cmp.b	#'A',D0
		bcs.s	low1
		cmp.b	#'Z',D0
		bhi.s	low1
		bset	#5,D0
low1		rts

;
; test if character in d0 is alphanumeric
; return carry set if yes, carry clear if no
; does not change d0
;
		pub	isalnum

		cmp.b	#'0',d0
		bcs.s	no
		cmp.b	#'9',d0
		bls.s	yes
;
; test if character is alphabetic or '_'
; return carry set if yes
;
		pub	isalpha

		cmp.b	#'A',d0
		bcs.s	no
		cmp.b	#'Z',d0
		bls.s	yes
		cmp.b	#'_',d0
		beq.s	yes
		cmp.b	#'a',d0
		bcs.s	no
		cmp.b	#'z',d0
		bls.s	yes
no		clc
		rts
yes		sec
		rts

;
; **** SKIP SPACES ****
;
		pub	skipspaces

1$		cmp.b	#SPACE,(A3)+
		beq.s	1$
		subq.l	#1,A3
		rts

;
; get a string from a list of null-terminated strings
; in:	d0 - number
;	a0 - string table
; out:	a0 - nth string
;
nth1		tst.b	(a0)+
		bne.s	nth1

		pub	getnth

		dbf	d0,nth1
		rts

;
;*** STRING OUTPUT ***
;string pointer in A1
;
		pub	putstring

1$		move.b	(a1)+,(a3)+
		bne.s	1$
		subq.l	#1,a3
		rts


;
;*** PUT LONGWORD (to possibly odd address) ***
;longword in d0
;
		pub	PutLong

		swap	D0
		move.b	D0,-(sp)
		lsr.w	#8,D0
		move.b	D0,(A3)+
		move.b	(sp)+,(A3)+
		swap	D0
		move.b	D0,-(sp)
		lsr.w	#8,D0
		move.b	D0,(A3)+
		move.b	(sp)+,(A3)+
		rts

;
; subroutine to read an unaligned word
;
		pub	mgetw

		move.b	(a5)+,d0
		lsl.w	#8,d0
		move.b	(a5)+,d0
		rts

;
;*** CREATE MESSAGE PORT ***
;* no name, priority 0
;
		pub	MyCreatePort

		movem.l	D2/A6,-(sp)
		moveq	#-1,D0
		lib	Exec,AllocSignal
		moveq	#-1,D1
		cmp.l	D0,D1
		beq.s	CrepFail
		move.l	D0,D2
		moveq	#MP_SIZE,D0
		move.l	#MEMF_PUBLIC!MEMF_CLEAR,D1
		lib	AllocMem
		tst.l	D0
		bne.s	Crep1
		move.b	D2,D0
		lib	FreeSignal
		bra.s	CrepFail
Crep1		move.l	D0,A0
;#
;# no need to call FindTask here...get the pointer from MyTask-variable
;# changed in version 1.22  1990-01-06
;#
		move.l	MyTask(a4),MP_SIGTASK(A0)
		move.b	D2,MP_SIGBIT(A0)
		move.b	#NT_MSGPORT,LN_TYPE(A0)
; PA_SIGNAL is zero...
;#		move.b	#PA_SIGNAL,MP_FLAGS(A0)
		lea	MP_MSGLIST(A0),A0
		NEWLIST	A0
		bra.s	Crep9

CrepFail	moveq	#0,D0
Crep9		movem.l	(sp)+,D2/A6	;port addr in D0 or zero if failed
		rts

;
;*** DELETE MESSAGE PORT ***
; no name (not a public port)
;port addr in A1
;
		pub	MyDeletePort
		movem.l	A2/A6,-(sp)
		moveq	#0,D0
		move.b	MP_SIGBIT(A1),D0
		move.l	A1,A2
		lib	Exec,FreeSignal
		move.l	A2,A1
		moveq	#MP_SIZE,D0
		lib	FreeMem
		movem.l	(sp)+,A2/A6
		rts

;
;*** CREATE IO REQUEST ***
;port addr in A1, size in D0
;
		pub	MyCreateIO
		movem.l	D0/A1/a6,-(sp)
		move.l	#MEMF_PUBLIC!MEMF_CLEAR,D1
		lib	Exec,AllocMem
		movem.l	(sp)+,D1/A1/a6
		tst.l	D0
		beq.s	CreIO9	;no memory
		move.l	D0,A0
		move.l	A1,MN_REPLYPORT(A0)
		move.b	#NT_MESSAGE,LN_TYPE(A0)
		move.w	D1,MN_LENGTH(A0)
CreIO9		rts

;
;*** DELETE IO REQUEST ***
;IoRequest In A1
;
		pub	MyDeleteIO
		moveq	#0,D0
		move.w	MN_LENGTH(A1),D0
		move.l	a6,-(sp)
		lib	Exec,FreeMem
		move.l	(sp)+,a6
		rts

;
; Set Console window mode (RAW/CON)
; d0 - FileHandle
; d1 - boolean flag (TRUE=RAW)
;
		pub	SetConMode

		lsl.l	#2,d0
		move.l	d0,a0
		move.l	fh_Type(a0),a0
		move.l	#ACTION_SCREEN_MODE,d0
;
; drop to sendpacket...
;

*** SEND A DOS PACKET TO A HANDLER ***
; code from sendpacket.a
*
* sendpacket -- send a DOS packet to a process and wait it to return
* inputs:
*  a0    -   pointer to handler process MsgPort (APTR, may be zero)
*  d0    -   packet type
*  d1-d7 -   packet arguments dp_Arg1...dp_Arg7
* results:
*  d0    -   primary return code dp_Res1 or zero if something failed
*  d1    -   secondary return code dp_Res2
*
* registers affected:
*  d0/d1/a0/a1
*
		pub	sendpacket

		movem.l	a2-a3/a6,-(sp)
		getbase	Exec			only exec will be called here
		move.l	d0,a2			save packet type temporarily
		move.l	a0,d0			check if handler port is zero
		beq	sp9			if it is, just return zero
		move.l	a0,a3			save handler port pointer

		move.l	d1,-(sp)
		moveq	#sp_SIZEOF+MP_SIZE,d0
		move.l	#MEMF_CLEAR!MEMF_PUBLIC,d1
		lib	AllocMem
		move.l	(sp)+,d1
		tst.l	d0
		beq.s	sp9			branch if allocmem failed

		move.l	d0,a1
		move.l	a2,sp_Pkt+dp_Type(a1)
		move.l	d0,a2
		movem.l	d1-d7,sp_Pkt+dp_Arg1(a2) save args in DosPacket structure

		moveq	#-1,d0
		lib	AllocSignal
		move.b	d0,sp_SIZEOF+MP_SIGBIT(a2)
		bmi.s	sp8			branch if allocsignal failed

;# FindTask()... was missing in original code....
;# but here it is not needed, we can use MyTask-variable
;# changed in version 1.30, 1990-08-22

		move.l	MyTask(a4),sp_SIZEOF+MP_SIGTASK(a2)
		lea	sp_SIZEOF+MP_MSGLIST(a2),a0
		NEWLIST	a0

		lea	sp_Pkt(a2),a0		link StandardPacket and message
		move.l	a0,LN_NAME(a2)		to each other
		move.l	a2,sp_Pkt+dp_Link(a2)
		lea	sp_SIZEOF(a2),a0	pointer to replyport
		move.l	a0,sp_Pkt+dp_Port(a2)

		move.l	a3,a0
		move.l	a2,a1			sp_Msg
		lib	PutMsg
		lea	sp_SIZEOF(a2),a0
		lib	WaitPort
		lea	sp_SIZEOF(a2),a0
		lib	GetMsg

		moveq	#0,d0
		move.b	sp_SIZEOF+MP_SIGBIT(a2),d0
		lib	FreeSignal			free signal bit

sp8		move.l	sp_Pkt+dp_Res2(a2),-(sp)	save result codes to stack
		move.l	sp_Pkt+dp_Res1(a2),-(sp)	in reverse order

		move.l	a2,a1
		moveq	#sp_SIZEOF+MP_SIZE,d0
		lib	FreeMem				free StandardPacket & MsgPort

		movem.l	(sp)+,d0-d1			get result codes in d0/d1

sp9		movem.l	(sp)+,a2-a3/a6
		rts

*{
;
; string=GetName(cmdline)
;   d0             a3
;
; Inputs:
;  a3  -  Pointer to string or command line
;
; Returns:
;  d0  -  Pointer to string argument or zero if failed.
;  a3  -  Updated to the position after the end of the argument in the string.
;
; Gets the next string argument from command line pointed by a3 to
; d0. returns zero if no valid string argument found. Arguments are
; separated by spaces and can enclosed in double quotes if they contain
; spaces. After the call, a3 points to the command line to a position
; immediately after the argument.
; NOTE: The command line itself is modified as this routine null-terminates
; the strings it returns.
;
; modifies only registers d0 and a3.
;
; no libraries used.
;
*}

		pub	GetName

01$		cmp.b	#' ',(a3)+
		beq.s	01$		;skip spaces
		bcs.s	noname
		subq.l	#1,a3
		cmp.b	#'"',(a3)
		beq.s	quoted
		move.l	a3,D0

getname1	cmp.b	#' ',(a3)+
		bhi.s	getname1
		clr.b	-1(a3)
		rts

quoted		addq.l	#1,a3
		move.l	a3,D0

getname2	cmp.b	#' ',(a3)
		bcs.s	noname
		cmp.b	#'"',(a3)+
		bne.s	getname2
		clr.b	-1(a3)
		rts

noname		moveq	#0,D0
		rts

*** HEX OUTPUT ROUTINES ***
;
; output a 6 or 8 digit hex number (6 digits if high byte is zero)
;  added checking of 60 or 80 column default font
;  now checks the window width from console unit
;
puthex1_68
		move.l	ConsoleUnit(a4),d1
		beq.s	phex1_8
		move.l	d1,a0
		cmp.w	#65,cu_XMax(a0)
		bcs.s	phex1_68
		bra.s	phex1_8
puthex_68	putchr	'$'
phex1_68	move.l	d0,d1
		swap	d1
		and.w	#$ff00,d1
		bne.s	phex1_8
		moveq	#6,d1
		bra.s	put_hexnum1
phex1_8		moveq	#8,d1
		bra.s	put_hexnum1
;
; put a signed hexnum in buffer pointed by a3
;
		pub	put_signed_hexnum

		tst.l	d0
		bpl.s	shex1
		neg.l	d0
		putchr	<'-'>
shex1		moveq	#-2,d1
		; fall to hexnum
;
; put a hex number in buffer pointed by a3
; d0: number, d1: # of digits
;
put_hexnum	putchr	<'$'>
put_hexnum1	movem.l	d2-d3,-(sp)
		tst.l	d1
		bpl.s	00$
		neg.w	d1
00$		subq.w	#1,d1
		moveq	#-1,d2
01$		move.b	d0,d3
		lsr.l	#4,d0
		and.b	#$0f,d3
		cmp.b	#10,d3
		bcs.s	02$
		add.b	#'A'-'0'-10,d3
02$		add.b	#'0',d3
		move.b	d3,-(sp)
		addq.w	#1,d2
		tst.l	d1
		bpl.s	100$
		subq.w	#1,d1
		tst.l	d0
		bne.s	01$
		tst.w	d1
		bpl.s	01$
		bra.s	03$
100$		dbf	d1,01$
03$		move.b	(sp)+,(a3)+
		dbf	d2,03$
		movem.l	(sp)+,d2-d3
		rts

;
; number output routine d0:number d1:length d2:base
;
		pub	PutNum

another_dummy_label_here	;_to_separate_blocks_with_identical_local_labels
		movem.l	D2-d4,-(sp)
		move.w	d1,d4
		move.w	d2,d3
		moveq	#-1,D2
01$	; 32 bit division
		move.w	D0,-(sp)	;save low word
		clr.w	D0
		swap	D0		;D0.L == high word
		divu	d3,D0
		move.w	D0,D1		;D1.W == quotient high
		move.w	(sp)+,D0
		divu	d3,D0
		swap	D1
		move.w	D0,D1
		swap	D0
		cmp.b	#10,d0
		bcs.s	10$
		addq.b	#7,d0
10$		add.b	#'0',D0
		move.b	D0,-(sp)
		move.l	D1,D0
		addq.w	#1,D2
		subq.w	#1,d4
		tst.l	D0
		bne.s	01$
		tst.w	d4
		bpl.s	01$
02$		move.b	(sp)+,(A3)+
		dbf	D2,02$
		movem.l	(sp)+,D2-d4
ret99		rts

		end
