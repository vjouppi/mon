;
; memory.asm
;
		include	"monitor.i"
;
; This module defines the following command routines:
;
;	showmemory,allocate_mem,alloc_abs,free_mem,memory_info,memtransfer
;	memfill,memhunt,memcomp,memdisplay,fmt_memdisplay,modifymem
;

		xref	monitor_code_start
		xref	monitor_code_end
		xref	mainloop

		xdef	FreeAllMem

		xref	help

		xref	puthex_68
		xref	puthex1_68
		xref	phex1_8

		xref	loctext
		xref	memlistfmt

		xref	generic_error

;
;*** MEMORY LIST ***
;
		cmd	showmemory

		move.l	MemoryList(a4),D5
		bne.s	showm2
not_at_all_allocated
		lea	no_alloc(pc),A0
		call	JUMP,printstring_a0_window	;error 'no memory allocated'

showm2		lea	memlisttx(pc),A0
		call	printstring_a0
		lea	loctext(pc),A0
		call	printstring_a0

showmemloop	move.l	d5,a2
		move.l	a2,d0
		addq.l	#8,d0
		move.l	4(a2),d2
		move.l	d0,d1
		add.l	d2,d1
		subq.l	#1,d1
		lea	memlistfmt(pc),a0
		call	printf
		call	CheckKeys
		bne.s	01$
		move.l	(A2),D5
		bne.s	showmemloop
01$		rts

*** FREE ALL MEMORY ALLOCATED WITH THE ( AND & COMMANDS ***
FreeAllMem	move.l	a6,-(sp)
		move.l	MemoryList(a4),D5
		beq.s	freeall9
		getbase	Exec
freeall_loop	move.l	D5,A1
		move.l	4(A1),D0
		addq.l	#8,D0
		move.l	(A1),D5
		lib	FreeMem
		tst.l	D5
		bne.s	freeall_loop
		clr.l	MemoryList(a4)
freeall9	move.l	(sp)+,a6
		rts

*** ALLOCATE MEMORY ***
		cmd	allocate_mem

		call	GetExpr
		move.l	D0,D5
		call	skipspaces
		moveq	#0,d1
		move.b	(a3),d0
		call	tolower
		cmp.b	#'c',d0
		bne.s	01$
		moveq	#MEMF_CHIP,d1
01$		move.l	D5,D0
		addq.l	#8,D0
		or.l	#MEMF_CLEAR!MEMF_PUBLIC,D1
		lib	Exec,AllocMem
		tst.l	D0
		beq.s	alloc_failed
		move.l	D0,A0

alloc_mem_1	;add allocated memory to linked list of memory blocks
** A0 points to memory block, D5 is length
		move.l	D5,4(A0)
		moveq	#0,D0
		move.l	MemoryList(a4),D1

alloc_find1	beq.s	alloc_find2	;keep the linked list in order, lowest address first
		move.l	D1,A1
		cmp.l	A1,A0
		bcs.s	alloc_find2
		move.l	D1,D0
		move.l	(A1),D1
		bra.s	alloc_find1

alloc_find2	tst.l	D0
		bne.s	alloc_do2
		move.l	MemoryList(a4),(A0)	;add new memory node to start of list
		move.l	A0,MemoryList(a4)
		bra.s	alloc_display

alloc_do2	move.l	D1,(A0)		;add new memory node to middle or end of list
		move.l	D0,A1
		move.l	A0,(A1)

alloc_display	move.l	a0,d0
		addq.l	#8,d0
		move.l	d0,Addr(a4)
		move.l	d0,d1
		add.l	4(a0),d1
		subq.l	#1,d1
		lea	allocfmt(pc),a0
		call	JUMP,printf

alloc_failed		;error 'allocation failed'
		lea	alloc_fail_msg(pc),A0
		call	JUMP,printstring_a0_window

*** ALLOCATE ABSOLUTE ***
		cmd	alloc_abs

		call	GetExpr
		subq.l	#8,D0	;remember to subtract 8 from the starting address
		move.l	D0,D7	;(next block pointer & length)
		call	GetExpr
		move.l	D0,D5
		beq	generic_error
		addq.l	#8,D0	;add 8 to length
		move.l	D7,A1
		lib	Exec,AllocAbs
		tst.l	D0
		beq.s	alloc_failed
		move.l	D7,A0
		bra	alloc_mem_1

*** FREE MEMORY ***
		cmd	free_mem

		call	skipspaces
		move.b	(a3),d0
		call	tolower
		cmp.b	#'a',d0		;check 'all'
		bne.s	free_norm
		moveq	#'l',D1
		move.b	1(a3),d0
		call	tolower
		cmp.b	d1,d0
		bne.s	free_norm
		move.b	2(a3),d0
		call	tolower
		cmp.b	d1,d0
		bne.s	free_norm
		bra	FreeAllMem

free_norm	call	GetExpr
		subq.l	#8,D0
		move.l	MemoryList(a4),D1
		beq	not_at_all_allocated
		moveq	#0,D2
find_mem_to_free_loop
		move.l	D1,A1
		cmp.l	D0,D1			;is this the block we want to free
		beq.s	found_do_free_mem
		move.l	D1,D2
		move.l	(A1),D1			;get next block pointer
		bne.s	find_mem_to_free_loop
		lea	noalloctx(pc),A0	;error 'not allocated'
		call	JUMP,printstring_a0_window

found_do_free_mem	;remove memory block from linked list
		tst.l	D2
		bne.s	notfirst
		move.l	(A1),MemoryList(a4)
		bra.s	do_free

notfirst	move.l	D2,A0
		move.l	(A1),(A0)

do_free		move.l	4(A1),D0
		addq.l	#8,D0
		jlib	Exec,FreeMem
;
; *** memory info
;
		cmd	memory_info

		call	GetExpr
		move.l	d0,d5

; walk the memory list

		lib	Exec,Forbid

		move.l	MemList(a6),a5

01$		tst.l	(a5)
		beq.s	09$

		cmp.l	MH_LOWER(a5),d5
		bcs.s	08$
		cmp.l	MH_UPPER(a5),d5
		bcc.s	08$

; address is in this memory region

		move.l	MH_FIRST(a5),d2
02$		beq.s	10$
		move.l	d2,a1
		cmp.l	a1,d5
		bcs.s	05$
		move.l	4(a1),d0
		lea	0(a1,d0.L),a0
		cmp.l	a0,d5
		bcs.s	10$

05$		move.l	(a1),d2
		bra.s	02$

08$		move.l	(a5),a5
		bra.s	01$

09$		suba.l	a5,a5

10$		lib	Permit

; if a5 is zero, location is not in memory list
; else if d2 is zero, location is allocated, else free

		startline
		move.l	d5,d0
		bsr	puthex_68
		move.b	#':',(a3)+
		move.b	#SPACE,(a3)+

		move.l	a5,d0
		bne.s	100$
		lea	notmemtxt(pc),a1
		call	putstring
		bra.s	999$

100$		btst	#MEMB_CHIP,MH_ATTRIBUTES+1(a5)
		beq.s	101$
		move.l	#'chip',d0
		call	PutLong
		move.b	#SPACE,(a3)+

101$		btst	#MEMB_FAST,MH_ATTRIBUTES+1(a5)
		beq.s	102$
		move.l	#'fast',d0
		call	PutLong
		move.b	#SPACE,(a3)+

102$		tst.l	d2
		beq.s	103$
		move.l	#'not ',d0
		call	PutLong

103$		lea	allotxt(pc),a1
		call	putstring

999$		clr.b	(a3)
		call	printstring

;
; check if location is in a hunk and print hunk number if it is
;
		move.l	SegList(a4),d0
		moveq	#0,d4
500$		lsl.l	#2,d0
		beq.s	ret_01
		move.l	d0,a2
		lea	4(a2),a0
		cmp.l	a0,d5
		bcs.s	510$
		move.l	-4(a2),d0
		lea	-4(a2,d0.L),a1
		cmp.l	a1,d5
		bcc.s	510$

		move.l	d4,d0
		move.l	d5,d1
		sub.l	a0,d1
		lea	inhunkfmt(pc),a0
		call	JUMP,printf

510$		move.l	(a2),d0
		addq.l	#1,d4
		bra.s	500$

ret_01		rts

**** TRANSFER MEMORY ****
		cmd	memtransfer

		call	GetExpr
		move.l	D0,A0
		call	GetExpr
		move.l	D0,A1
		call	GetExpr
		move.l	D0,A2
		cmp.l	A2,A0
		bcs.s	backwards	;if destination > source, transfer backwards
;#
;# this may fail if the end address is $ffffffff. normally this is not
;# true, so it is not so much a problem...
;#
trf1		cmp.l	A1,A0
		bhi.s	ret_01
		move.b	(A0)+,(A2)+
		bra.s	trf1
;#
;# here is a problem if the start address (here really end address)
;# is zero, because after the subtraction of the pointers, the address
;# is -1 or $ffffffff unsigned and that is greater than zero. so the
;# test for the transfer to end never succeeds....
;#
;# this was fixed in version 1.17 -- 1989-12-09
;# (note the extra compare instruction in case the start address is
;# greater than end address)
;#
backwards	add.l	A1,A2
		sub.l	A0,A2
		cmp.l	A0,A1
		bcs.s	ret_01
trf2		move.b	(A1),(A2)
		cmp.l	a0,a1
		bls.s	ret_01
		subq.l	#1,A1
		subq.l	#1,A2
		bra.s	trf2

**** FILL MEMORY ****
* This version can fill memory with a pattern
		cmd	memfill

		call	GetExpr
		move.l	D0,-(sp)
		call	GetExpr
		move.l	D0,A2
		lea	InputBuf(a4),a0
		call	GetString
		move.l	(sp)+,A1
		tst.l	D2
		beq.s	ret_01
fill0		moveq	#0,D0
fill1		cmp.l	A2,A1
		bhi.s	ret_01
		move.b	0(A0,D0.L),(A1)+
		addq.l	#1,D0
		cmp.l	D2,D0
		bcs.s	fill1
		bra.s	fill0

**** HUNT MEMORY ****
		cmd	memhunt

		call	skipspaces
		tst.b	(a3)
		beq	help

		bsr	get_n_per_line
		move.w	d6,D7
		call	GetExpr
		move.l	D0,-(sp)
		call	GetExpr
		move.l	D0,A2
		lea	InputBuf(a4),a0
		call	GetString
		move.l	(sp)+,A1
		tst.l	D2	;string length
		beq	ret_01

hunt0		move.b	(A0),D0
hunt1		cmp.l	A2,A1
		bhi.s	lf_and_exit
		cmp.b	(A1)+,D0
		bne.s	hunt1
		moveq	#0,D1

hunt2		addq.l	#1,D1
		cmp.l	D2,D1
		bcc.s	huntfound
		move.b	-1(A1,D1.L),D0
		cmp.b	0(A0,D1.L),D0
		beq.s	hunt2
		bra.s	hunt0

huntfound	movem.l	A0-A1,-(sp)
;
; check if the found data was in the monitor code/data area
; and don't print it if it is
;
		lea	monitor_code_start(pc),a0
		cmp.l	a0,a1
		bcs.s	hunt_ck1
		lea	monitor_code_end(pc),a0
		cmp.l	a0,a1
		bcs.s	huntf2

hunt_ck1	cmp.l	a4,a1
		bcs.s	huntprint
		lea	MonitorData_SIZE(a4),a0
		cmp.l	a0,a1
		bcs.s	huntf2

huntprint	move.l	A1,D0
		subq.l	#1,D0
		lea	comp_hunt_fmt(pc),a0
		call	printf
		subq.w	#1,D7
		bne.s	huntf1
		emit	LF
		move.w	d6,d7
huntf1		call	CheckKeys
		bne.s	lf_and_exit

huntf2		movem.l	(sp)+,A0-A1
		bra.s	hunt0

lf_and_exit	emit	LF
		bra	mainloop

**** COMPARE MEMORY ****
		cmd	memcomp

		bsr.s	get_n_per_line
		move.w	d6,D7
		call	GetExpr
		move.l	D0,A0
		call	GetExpr
		move.l	D0,A1
		call	GetExpr
		move.l	D0,A2

comp1		cmp.l	A1,A0
		bhi	lf_and_exit
		cmpm.b	(A0)+,(A2)+
		beq.s	comp1
		movem.l	A0-A1,-(sp)
		move.l	A0,D0
		subq.l	#1,D0
		lea	comp_hunt_fmt(pc),a0
		call	printf
		subq.w	#1,D7
		bne.s	compf1
		emit	LF
		move.w	d6,D7

compf1		call	CheckKeys
		bne	lf_and_exit
		movem.l	(sp)+,A0-A1
		bra.s	comp1

;
; find out how many addresses can be printed on one line
; in the c and h commands. return result in d6.
;
get_n_per_line	moveq	#80,d6
		move.l	ConsoleUnit(a4),d0
		beq.s	01$
		move.l	d0,a0
		move.w	cu_XMax(a0),d6
01$		divu	#9,d6
		bne.s	09$
		moveq	#1,d6
09$		rts

**** DISPLAY MEMORY ****
;#
;# ->v1.22  1990-01-06
;# memdisplay now reads memory as bytes and can be started at odd address.
;#
		cmd	memdisplay

display_memory	call	GetParams
		moveq	#0,d6
		move.l	ConsoleUnit(a4),d0
		beq.s	00$
		move.l	d0,a0
		cmp.w	#65,cu_XMax(a0)
		scs	d6
00$		move.l	Addr(a4),a5

disploop	startline
		move.l	a5,D0
		bsr	puthex1_68
		putchr	<':'>
		putchr	SPACE
		moveq	#16/4-1,D2

01$		call	mgetw
		swap	d0
		call	mgetw
		bsr	phex1_8
		putchr	SPACE
		dbf	D2,01$

		sub.w	#16,a5
		putchr	SPACE
		tst.b	d6
		bmi.s	100$
		putchr	<''''>
100$		moveq	#16-1,D2

02$		move.b	(a5)+,D0	;printable codes are $20-$7F and $A0-$FF
		call	to_printable
		move.b	D0,(A3)+
		dbf	D2,02$

		tst.b	d6
		bmi.s	101$
		putchr	<''''>
101$		endline
		call	printstring
		call	CheckEnd
		bne.s	disploop

		move.l	a5,Addr(a4)
		bra	mainloop
;
; subroutines for 'mf' command
;
p_align		move.l	a5,d0
		btst	#0,d0
		beq.s	p_align_end
		emit	'*'
		addq.l	#1,a5
p_align_end	rts

dec_num		move.l	d2,-(sp)
		move.l	d5,d2
		moveq	#-1,d1
		call	PutNum
		move.l	(sp)+,d2
		clr.b	(a3)
		call	JUMP,printstring

p_byte		startline
		moveq	#0,d0
		move.b	(a5)+,d0
		moveq	#2,d1
		bra.s	h_com

p_word		startline
		bsr.s	p_align
		moveq	#0,d0
		move.w	(a5)+,d0
		moveq	#4,d1
		bra.s	h_com

p_long		startline
		bsr.s	p_align
		move.l	(a5)+,d0
h8_com		moveq	#8,d1

h_com		cmp.b	#10,d5
		beq.s	dec_num
		call	put_hexnum
		clr.b	(a3)
		call	JUMP,printstring

p_addr		startline
		move.l	a5,d0
		bra.s	h8_com

;
; formatted memory display
; mf [addr [end_addr]] "fmt_string"
;
; format string:
;
;  %b  -  hex byte
;  %w  -  hex word
;  %l  -  hex longword
;  %db -  decimal byte
;  %dw -  decimal word
;  %dl -  decimal long
;  %a  -  hex address
;  %da -  decimal address
;  \n  -  linefeed
;
;
		cmd	fmt_memdisplay

		clr.l	EndAddr(a4)
		call	skipspaces
		cmp.b	#'"',(a3)
		beq.s	get_fmtstring

		call	GetExpr
		move.l	d0,Addr(a4)

		call	skipspaces
		cmp.b	#'"',(a3)
		beq.s	get_fmtstring

		call	GetExpr
		move.l	d0,EndAddr(a4)	;!!!!!!

get_fmtstring	call	GetName
		move.l	d0,instrad(a4)

		move.l	Addr(a4),a5

fmt_memdisp_next
		move.l	instrad(a4),a2

fmt_memdisp_loop
		move.l	a2,d2

1$		move.b	(a2)+,d0
		beq.s	p_end
		cmp.b	#'\',d0
		beq.s	p_esc
		cmp.b	#'%',d0
		bne.s	1$

		subq.l	#1,a2
		bsr.s	p_putstr
		addq.l	#1,a2
		move.b	(a2)+,d0

		moveq	#16,d5		;base hex
		cmp.b	#'d',d0
		bne.s	10$
		moveq	#10,d5		;base decimal
		move.b	(a2)+,d0

10$		lea	p_chars(pc),a0
		move.l	a0,d1
2$		tst.b	(a0)
		beq.s	fmt_memdisp_loop
		cmp.b	(a0)+,d0
		bne.s	2$
		sub.l	d1,a0
		add.w	a0,a0
		lea	p_jumps-2(pc,a0.w),a0
		add.w	(a0),a0
		jsr	(a0)
		jmp	fmt_memdisp_loop

p_esc		move.b	(a2)+,d0
		beq.s	p_end
		cmp.b	#'n',d0
		bne.s	p_str		;!!
		subq.l	#2,a2
		bsr.s	p_putstr
		addq.l	#2,a2
		emit	LF
		bra.s	p_end_test

p_str		bsr.s	p_putstr

p_end_test	tst.b	-1(a2)
		bne.s	fmt_memdisp_loop
	;!!!!

p_end		move.l	a5,Addr(a4)
		bsr.s	p_putstr
		emit	LF
		call	CheckKeys
		bne.s	p_main
		cmp.w	#0,a5
		beq.s	p_main
		cmp.l	EndAddr(a4),a5
		bls	fmt_memdisp_next

p_main		bra	mainloop

p_percent	emit	'%'
		bra	fmt_memdisp_loop

p_putstr	move.l	a2,d3
		sub.l	d2,d3
		beq.s	1$
		move.l	OutputFile(a4),d1
		lib	Dos,Write
1$		rts

;
;
p_jumps		rw	p_byte
		rw	p_word
		rw	p_long
		rw	p_addr
		rw	p_percent

p_chars		dc.b	'bwla%',0
		ds.w	0

;
;*** MODIFY MEMORY ***
;
		cmd	modifymem

		call	GetExpr
		move.l	D0,A0
		call	GetString
		bra	mainloop
;
;
comp_hunt_fmt	dc.b	'%08lx ',0

alloc_fail_msg	dc.b	'Allocation failed',LF,0
noalloctx	dc.b	'Not allocated that',LF,0

no_alloc	dc.b	'No memory '
allotxt		dc.b	'allocated',LF,0
memlisttx	dc.b	'Allocated memory:',LF,0

notmemtxt	dc.b	'Not in MemList',LF,0
inhunkfmt	dc.b	'(in hunk %ld, offset $%lx)',LF,0

allocfmt	dc.b	'Allocated from $%08lx to $%08lx',LF,0

		end
