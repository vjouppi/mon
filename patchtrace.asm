;
; patchtrace.a -- a little program to help with the problem of
; tracing trap-instructions (and chk & trapv & divide by zero).
; if a trace-exception happens in supervisor mode, just return from exception
; and the trace flag will be automatically cleared...
;
; created  1989-08-24  TR
;
; Modified 1990-07-13 --> Now works if the vector base register is
;			  nonzero on a 68010/20/30/40... system
;	   1990-08-22 --> added includes, added this to makefile, uses
;			  new macros...
;

		include	'include.i'
		include	'offsets.i'
		include	'macros.i'

trace_vector	equ	$24	;offset from start of vector table


	getbase	Exec

	lea	supercode(pc),a5
	lib	Supervisor		;get VBR into a4

	moveq	#copylen+4,d0
	moveq	#MEMF_PUBLIC,d1
	lib	AllocMem
	tst.l	d0
	beq.s	exit
	move.l	d0,a1
	move.l	d0,a2
	lea	copymod(pc),a0
	moveq	#copylen/2-1,d1
cploop	move.w	(a0)+,(a1)+
	dbf	d1,cploop
	move.l	trace_vector(a4),(a1)+
	move.l	a2,trace_vector(a4)
exit	moveq	#0,d0
	rts

;
; this is executed in supervisor mode to get the start address of
; the processor vector table. this address is always zero on
; a 68000 prosessor, and the contents of the vbr-register with
; 68010/20 etc.
;
supercode
	suba.l	a4,a4
	btst	#AFB_68010,AttnFlags+1(a6)
	beq.s	01$
	dc.w	$4e7a,$c801	; movec vbr,a4
01$	rte

copymod	btst	#5,(sp)		check supervisor mode bit in saved sr
	beq.s	nosup		jump to old vector if not supervisor mode
	rte			just return

nosup	dc.w	$4ef9		jmp-instruction

copylen	equ	*-copymod


	end
