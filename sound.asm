;
; sound.asm
;
		include	"monitor.i"
;
; This module defines the following command routine:
;
;	digisound
;

		xref	generic_error
		xref	odd_address_error


*** PLAY DIGITIZED SOUND ***
		cmd	digisound

dummy_label_here	;_because_devpac_doesnt_work_without_it
		call	GetExpr
		btst	#0,d0
		bne	odd_address_error

		move.l	D0,D5
		call	GetExpr
		tst.l	D0
		beq	generic_error		;error: zero length
		btst	#0,D0
		bne	generic_error
		move.l	D0,D6
		call	GetExpr	;period (speed)
		move.w	D0,D7
		move.w	#1,size(a4)
		call	skipspaces
		tst.b	(a3)
		beq.s	00$
		call	GetExpr		;# of cycles, defaults to zero (loop)
		move.w	d0,size(a4)
00$		call	MyCreatePort
		move.l	d0,d2
		beq	digi9
		move.l	D0,A1
		moveq	#ioa_SIZEOF,D0
		call	MyCreateIO
		tst.l	D0
		beq	digi8
		move.l	D0,A2
		move.b	#127,LN_PRI(A2)	;maximum priority (so nobody can steal the channel)
		lea	allocmap(pc),A0	;channel allocation map (any channel)
		move.l	A0,ioa_Data(A2)
		moveq	#4,D0		;size of the allocation map
		move.l	D0,ioa_Length(A2)
		move.l	A2,A1
		lea	audioname(pc),A0
		moveq	#0,D0
		moveq	#0,D1
		lib	Exec,OpenDevice	;open audio.device
		tst.l	D0
		bne	digi_openerr
		move.l	D5,ioa_Data(A2)
		move.l	D6,ioa_Length(A2)
		move.w	D7,ioa_Period(A2)
		move.w	size(a4),ioa_Cycles(A2)
		move.w	#64,ioa_Volume(A2)	;maximum volume
		move.b	#ADIOF_PERVOL,IO_FLAGS(A2) ;flag to set the volume & period
		move.w	#CMD_WRITE,IO_COMMAND(A2)	;audio output=CMD_WRITE
		move.l	A2,A1
		BEGINIO	;can't use SendIO, because it clears the ADIO_PERVOL flag
		lea	audiotxt(pc),A0
		call	printstring_a0_window		;message: 'press Ctrl-C...'
		moveq	#0,D0
		move.l	#SIGBREAKF_CTRL_C,d2
		move.l	d2,d1
		lib	SetSignal	;clear CTRL-C signal
		move.l	d2,d0
		move.l	MN_REPLYPORT(a2),a0
		move.b	MP_SIGBIT(a0),d3
		bset	d3,d0			wait until the sound finishes or
		lib	Wait			user presses Ctrl-C
		move.l	a2,a1
		lib	AbortIO
		move.l	a2,a1
		lib	WaitIO
		move.l	A2,A1
		lib	CloseDevice
digi7		move.l	MN_REPLYPORT(a2),d2
		move.l	A2,A1
		call	MyDeleteIO
digi8		move.l	d2,A1
		call	MyDeletePort
digi9		rts

digi_openerr	lea	aud_openerr_msg(pc),a0
		call	printstring_a0_window
		bra.s	digi7

audioname	dc.b	'audio.device',0
allocmap	dc.b	1,8,2,4

audiotxt	dc.b	'Press Ctrl-C to stop...',LF,0
aud_openerr_msg	dc.b	'Audio open failed',LF,0

		end
