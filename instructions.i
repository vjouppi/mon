TOKSTART	equ	$a0

instruction	macro
n_\1		equ	InstrCount
t_\1		equ	TokCount
InstrCount	set	InstrCount+1
TokCount	set	TokCount+1
		endm

InstrCount	set	0
TokCount	set	TOKSTART

		instruction	as		;0
		instruction	ls		;1
		instruction	rox		;2
		instruction	rot		;3
		instruction	move		;4	
		instruction	add		;5
		instruction	sub		;6
		instruction	and		;7
		instruction	or		;8
		instruction	abcd		;9
		instruction	sbcd		;10
		instruction	mul		;11
		instruction	div		;12
		instruction	exg		;13
		instruction	eor		;14
		instruction	cmp		;15
		instruction	btst		;16
		instruction	bchg		;17
		instruction	bclr		;18
		instruction	bset		;19
		instruction	chk		;20
		instruction	lea		;21
		instruction	ext		;22
		instruction	clr		;23
		instruction	neg		;24
		instruction	not		;25
		instruction	tst		;26
		instruction	nbcd		;27
		instruction	swap		;28
		instruction	pea		;29
		instruction	link		;30
		instruction	unlk		;31
		instruction	reset		;32
		instruction	nop		;33
		instruction	stop		;34
		instruction	rte		;35
		instruction	tas		;36
		instruction	rts		;37
		instruction	trapv		;38
		instruction	rtr		;39
		instruction	jsr		;40
		instruction	jmp		;41
		instruction	trap		;42
		instruction	illegal		;43
	;;;	dc.w	0	;end mark
