;
; mem_cmd.asm
;

		nolist
		include "exec/types.i"
		include "exec/memory.i"
		include "exec/execbase.i"
		include "devices/conunit.i"
		include "offsets.i"
		list

		include	"monitor.i"
		include	"variables.i"

;
; This module defines the following command routines:
;
;	showmemory,allocate_mem,alloc_abs,free_mem,memory_info,memtransfer
;	memfill,memhunt,memcomp,memdisplay,fmt_memdisplay,modifymem,free_all_mem
;

		xref	monitor_code_start
		xref	monitor_code_end
		xref	mainloop
		xref	error_com

		xref	help
		xref	generic_error
		xref	odd_address_error

		forward	find_mem_blk_num


;
;*** MEMORY LIST ***
;
		cmd	showmemory

		move.l	mon_MemoryList(a4),D5
		bne.b	showm2
not_at_all_allocated
		lea	nomem_alloc_txt(pc),A0
		call	JUMP,printstring_a0_window	;error 'no memory allocated'

showm2		lea	memlist_txt(pc),A0
		call	printstring_a0
		lea	loc_txt(pc),A0
		call	printstring_a0
		moveq	#-1,d4

showmemloop	addq.l	#1,d4
		move.l	d4,d0
		move.l	d5,a2
		move.l	a2,d1
		addq.l	#8,d1
		move.l	4(a2),d3
		move.l	d1,d2
		add.l	d3,d2
		subq.l	#1,d2
		lea	memlist_fmt(pc),a0
		call	printf
		call	CheckKeys
		bne.b	01$
		move.l	(A2),D5
		bne	showmemloop
01$		rts

*** FREE ALL MEMORY ALLOCATED WITH THE ( AND & COMMANDS ***
		pub	free_all_mem

		move.l	a6,-(sp)
		move.l	mon_MemoryList(a4),D5
		beq.b	freeall9
		getbase	Exec

freeall_loop	move.l	D5,A1
		move.l	4(A1),D0
		addq.l	#8,D0
		move.l	(A1),D5
		lib	FreeMem
		tst.l	D5
		bne.b	freeall_loop

		clr.l	mon_MemoryList(a4)
freeall9	move.l	(sp)+,a6
		rts

*** ALLOCATE MEMORY ***
		cmd	allocate_mem

		call	GetExpr
		move.l	D0,D5
		bmi.b	alloc_failed
		call	skipspaces
		moveq	#0,d1
		move.b	(a3),d0
		call	tolower
		cmp.b	#'c',d0
		bne.b	01$
		moveq	#MEMF_CHIP,d1
01$		move.l	D5,D0
		addq.l	#8,D0
		or.l	#MEMF_CLEAR!MEMF_PUBLIC,D1
		lib	Exec,AllocMem
		tst.l	D0
		beq.b	alloc_failed
		move.l	D0,A0

alloc_mem_1	;add allocated memory to linked list of memory blocks
** A0 points to memory block, D5 is length
		move.l	D5,4(A0)
		moveq	#0,D0
		move.l	mon_MemoryList(a4),D1

alloc_find1	beq.b	alloc_find2	;keep the linked list in order, lowest address first
		move.l	D1,A1
		cmp.l	A1,A0
		bcs.b	alloc_find2
		move.l	D1,D0
		move.l	(A1),D1
		bra.b	alloc_find1

alloc_find2	tst.l	D0
		bne.b	alloc_do2
		move.l	mon_MemoryList(a4),(A0)	;add new memory node to start of list
		move.l	A0,mon_MemoryList(a4)
		bra.b	alloc_display

alloc_do2	move.l	D1,(A0)		;add new memory node to middle or end of list
		move.l	D0,A1
		move.l	A0,(A1)

alloc_display	move.l	a0,d0
		addq.l	#8,d0
		move.l	d0,mon_CurrentAddr(a4)
		move.l	d0,d1
		add.l	4(a0),d1
		subq.l	#1,d1
		lea	alloc_fmt(pc),a0
		call	JUMP,printf

alloc_failed		;error 'allocation failed'
		lea	allocfail_txt(pc),A0
		call	printstring_a0_window
		bra	error_com

*** ALLOCATE ABSOLUTE ***
		cmd	alloc_abs

		call	GetExpr
		btst	#0,d0
		bne	odd_address_error
		subq.l	#8,D0	;remember to subtract 8 from the starting address
		move.l	D0,D7	;(next block pointer & length)
		call	GetExpr
		move.l	D0,D5
		beq	generic_error
		addq.l	#8,D0	;add 8 to length
		move.l	D7,A1
		lib	Exec,AllocAbs
		tst.l	D0
		beq.b	alloc_failed
		move.l	D7,A0
		bra	alloc_mem_1

*** FREE MEMORY ***
		cmd	free_mem

		call	skipspaces
		move.b	(a3),d0
		cmp.b	#'#',d0
		beq.b	free_bl_num

		call	tolower
		cmp.b	#'a',d0		;check 'all'
		bne.b	free_norm
		moveq	#'l',D1
		move.b	1(a3),d0
		call	tolower
		cmp.b	d1,d0
		bne.b	free_norm
		move.b	2(a3),d0
		call	tolower
		cmp.b	d1,d0
		bne.b	free_norm
		call	JUMP,free_all_mem

free_norm	call	GetExpr

do_free_mem	subq.l	#8,D0
		move.l	mon_MemoryList(a4),D1
		beq	not_at_all_allocated
		moveq	#0,D2
find_mem_to_free_loop
		move.l	D1,A1
		cmp.l	D0,D1			;is this the block we want to free
		beq.b	found_do_free_mem
		move.l	D1,D2
		move.l	(A1),D1			;get next block pointer
		bne.b	find_mem_to_free_loop

not_alloc	lea	noalloc_txt(pc),A0	;error 'not allocated'
		call	JUMP,printstring_a0_window

found_do_free_mem	;remove memory block from linked list
		tst.l	D2
		bne.b	notfirst
		move.l	(A1),mon_MemoryList(a4)
		bra.b	do_free

notfirst	move.l	D2,A0
		move.l	(A1),(A0)

do_free		move.l	4(A1),D0
		addq.l	#8,D0
		jlib	Exec,FreeMem

free_bl_num	addq.l	#1,a3
		call	GetExpr
		call	find_mem_blk_num
		tst.l	d0
		beq.b	not_alloc
		bra.b	do_free_mem

;
; find a memory block by number.
;
; parameters: d0 - memory block number (0..number-of-blocks - 1)
; return: d0 -- address of memory block (address of link field + 8)
;
		pub	find_mem_blk_num

		move.l	mon_MemoryList(a4),d1
1$		beq.b	8$
		tst.l	d0
		beq.b	9$

		move.l	d1,a0
		subq.l	#1,d0
		move.l	(a0),d1
		bra.b	1$

8$		moveq	#0,d0
		rts

9$		move.l	d1,d0
		addq.l	#8,d0
		rts

;
; *** memory info
;
		cmd	memory_info

		call	GetExpr
		move.l	d0,d5

; walk the memory list

		lib	Exec,Forbid

		move.l	MemList(a6),a5	;ExecBase->MemList

01$		tst.l	(a5)
		beq.b	09$

		cmp.l	MH_LOWER(a5),d5
		bcs.b	08$
		cmp.l	MH_UPPER(a5),d5
		bcc.b	08$

; address is in this memory region

		move.l	MH_FIRST(a5),d2
02$		beq.b	10$
		move.l	d2,a1
		cmp.l	a1,d5
		bcs.b	05$
		move.l	4(a1),d0
		lea	0(a1,d0.L),a0
		cmp.l	a0,d5
		bcs.b	10$

05$		move.l	(a1),d2
		bra.b	02$

08$		move.l	(a5),a5
		bra.b	01$

09$		suba.l	a5,a5

10$		lib	Permit

; if a5 is zero, location is not in memory list
; else if d2 is zero, location is allocated, else free

		startline
		move.l	d5,d0
		move.b	#'$',(a3)+
		call	puthex68
		move.b	#':',(a3)+
		move.b	#SPACE,(a3)+

		move.l	a5,d0
		bne.b	in_memlist
		lea	not_memlist_txt(pc),a1
		call	putstring
		bra.b	minfo_end_first_line

in_memlist	btst	#MEMB_CHIP,MH_ATTRIBUTES+1(a5)
		beq.b	no_chip
		move.l	#'chip',d0
		call	PutLong
		move.b	#SPACE,(a3)+

no_chip		btst	#MEMB_FAST,MH_ATTRIBUTES+1(a5)
		beq.b	no_fast
		move.l	#'fast',d0
		call	PutLong
		move.b	#SPACE,(a3)+

no_fast		tst.l	d2
		beq.b	alloc_show
		move.l	#'not ',d0
		call	PutLong

alloc_show	lea	allocated_txt(pc),a1
		call	putstring

minfo_end_first_line
		clr.b	(a3)
		call	printstring

;
; check if location is in a hunk and print hunk number if it is
;
		move.l	d5,a0
		call	find_hunk_addr
		tst.l	d0
		beq.b	105$

		exg	d0,d1
		addq.l	#4,d1
		sub.l	d5,d1
		neg.l	d1
		lea	inhunk_fmt(pc),a0
		call	printf

105$		tst.l	mon_HunkTypeTable(a4)
		beq.b	ret_01

		move.l	d5,d0
		call	find_var_value
		tst.l	d0
		beq.b	ret_01

		move.l	d0,a0
		lea	var_Name(a0),a0
		move.l	a0,d0
		lea	minfo_var_fmt(pc),a0
		call	printf

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
		bcs.b	backwards	;if destination > source, transfer backwards
;#
;# this may fail if the end address is $ffffffff. normally this is not
;# true, so it is not so much a problem...
;#
trf1		cmp.l	A1,A0
		bhi.b	ret_01
		move.b	(A0)+,(A2)+
		bra.b	trf1
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
		bcs.b	ret_01
trf2		move.b	(A1),(A2)
		cmp.l	a0,a1
		bls.b	ret_01
		subq.l	#1,A1
		subq.l	#1,A2
		bra.b	trf2

**** FILL MEMORY ****
* This version can fill memory with a pattern
		cmd	memfill

		call	GetExpr
		move.l	D0,-(sp)
		call	GetExpr
		move.l	D0,A2
		lea	mon_InputBuf(a4),a0
		call	GetString
		move.l	(sp)+,A1
		tst.l	D2
		beq.b	ret_01
fill0		moveq	#0,D0
fill1		cmp.l	A2,A1
		bhi.b	ret_01
		move.b	0(A0,D0.L),(A1)+
		addq.l	#1,D0
		cmp.l	D2,D0
		bcs.b	fill1
		bra.b	fill0

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
		lea	mon_InputBuf(a4),a0
		call	GetString
		move.l	(sp)+,A1
		tst.l	D2	;string length
		beq	ret_01

hunt0		move.b	(A0),D0
hunt1		cmp.l	A2,A1
		bhi.b	lf_and_exit
		cmp.b	(A1)+,D0
		bne.b	hunt1
		moveq	#0,D1

hunt2		addq.l	#1,D1
		cmp.l	D2,D1
		bcc.b	huntfound
		move.b	-1(A1,D1.L),D0
		cmp.b	0(A0,D1.L),D0
		beq.b	hunt2
		bra.b	hunt0

huntfound	movem.l	A0-A1,-(sp)
;
; check if the found data was in the monitor code/data area
; and don't print it if it is
;
		lea	monitor_code_start(pc),a0
		cmp.l	a0,a1
		bcs.b	hunt_ck1
		lea	monitor_code_end(pc),a0
		cmp.l	a0,a1
		bcs.b	huntf2

hunt_ck1	cmp.l	a4,a1
		bcs.b	huntprint
		lea	MonitorData_SIZE(a4),a0
		cmp.l	a0,a1
		bcs.b	huntf2

huntprint	move.l	A1,D0
		subq.l	#1,D0
		lea	comp_hunt_fmt(pc),a0
		call	printf
		subq.w	#1,D7
		bne.b	huntf1
		emit	LF
		move.w	d6,d7
huntf1		call	CheckKeys
		bne.b	lf_and_exit

huntf2		movem.l	(sp)+,A0-A1
		bra.b	hunt0

lf_and_exit	emit	LF
		bra	mainloop

**** COMPARE MEMORY ****
		cmd	memcomp

		bsr.b	get_n_per_line
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
		beq.b	comp1
		movem.l	A0-A1,-(sp)
		move.l	A0,D0
		subq.l	#1,D0
		lea	comp_hunt_fmt(pc),a0
		call	printf
		subq.w	#1,D7
		bne.b	compf1
		emit	LF
		move.w	d6,D7

compf1		call	CheckKeys
		bne	lf_and_exit
		movem.l	(sp)+,A0-A1
		bra.b	comp1

;
; find out how many addresses can be printed on one line
; in the c and h commands. return result in d6.
;
get_n_per_line	moveq	#80,d6
		move.l	mon_ConsoleUnit(a4),d0
		beq.b	01$
		move.l	d0,a0
		move.w	cu_XMax(a0),d6
01$		divu	#9,d6
		bne.b	09$
		moveq	#1,d6
09$		rts

;
; Memory dump in ASCII. Added in version 1.65 (1994-03-10)
;
		cmd	ascii_dump
		call	GetParams
		move.l	mon_CurrentAddr(a4),a5

asc_dump_loop	startline
		move.l	a5,d0
		move.l	a5,D0
		call	puthex68
		putchr	<':'>
		putchr	SPACE
		moveq	#64-1,d2

dump_asc_loop	move.b	(a5)+,D0	;printable codes are $20-$7F and $A0-$FF
		call	to_printable
		move.b	D0,(A3)+
		tst.l	mon_EndAddr(a4)
		beq.b	dump_asc_cont
		cmpa.l	mon_EndAddr(a4),a5
		bhi.b	dump_asc_end
dump_asc_cont	dbf	D2,dump_asc_loop

dump_asc_end	endline
		call	printstring
		call	CheckEnd
		bne	asc_dump_loop

		move.l	a5,mon_CurrentAddr(a4)
		rts

**** DISPLAY MEMORY ****
;#
;# ->v1.22  1990-01-06
;# memdisplay now reads memory as bytes and can be started at odd address.
;#
;# ->v1.57  1993-03-05
;# now stops memory display at end address (doesn't show rest of the line)
;#
		cmd	memdisplay

display_memory	call	GetParams
		moveq	#0,d6
		move.l	mon_ConsoleUnit(a4),d0
		beq.b	00$
		move.l	d0,a0
		cmp.w	#65,cu_XMax(a0)
		scs	d6
00$		move.l	mon_CurrentAddr(a4),a5

disploop	startline
		move.l	a5,D0
		call	puthex68
		putchr	<':'>
		putchr	SPACE
		moveq	#16-1,D2
		moveq	#0,d5

disp_hex_loop	move.b	(a5)+,d0
		moveq	#2,d1
		call	put_hexnum1
		move.b	d2,d1
		and.b	#3,d1
		bne.b	1$
		putchr	SPACE
1$		tst.l	mon_EndAddr(a4)
		beq.b	disp_hex_cont
		cmpa.l	mon_EndAddr(a4),a5
		bhi.b	disp_hex_end
disp_hex_cont	dbf	D2,disp_hex_loop
		bra.b	disp_hex_e2

disp_hex_e1	moveq	#SPACE,d0
		move.b	d0,(a3)+
		move.b	d0,(a3)+
		move.b	d2,d1
		and.b	#3,d1
		bne.b	1$
		move.b	d0,(a3)+
1$		addq.l	#1,a5
disp_hex_end	dbf	d2,disp_hex_e1

disp_hex_e2	sub.w	#16,a5
		putchr	SPACE
		tst.b	d6
		bmi.b	100$
		putchr	<''''>
100$		moveq	#16-1,D2

disp_asc_loop	move.b	(a5)+,D0	;printable codes are $20-$7F and $A0-$FF
		call	to_printable
		move.b	D0,(A3)+
		tst.l	mon_EndAddr(a4)
		beq.b	disp_asc_cont
		cmpa.l	mon_EndAddr(a4),a5
		bhi.b	disp_asc_end
disp_asc_cont	dbf	D2,disp_asc_loop

disp_asc_end	tst.b	d6
		bmi.b	101$
		putchr	<''''>
101$		endline
		call	printstring
		call	CheckEnd
		bne	disploop

		move.l	a5,mon_CurrentAddr(a4)
		rts

;
; subroutines for 'mf' command
;
p_align		move.l	a5,d0
		btst	#0,d0
		beq.b	p_align_end
		emit	'*'
		addq.l	#1,a5
p_align_end	rts

dec_num		move.l	d2,-(sp)
		move.l	d5,d2
		moveq	#-1,d1
		call	put_number
		move.l	(sp)+,d2
		clr.b	(a3)
		bra.b	p_prt2

p_byte		startline
		moveq	#0,d0
		move.b	(a5)+,d0
		moveq	#2,d1
		bra.b	h_com

p_word		startline
		bsr.b	p_align
		moveq	#0,d0
		move.w	(a5)+,d0
		moveq	#4,d1
		bra.b	h_com

p_long		startline
		bsr.b	p_align
		move.l	(a5)+,d0

h8_com		moveq	#8,d1

h_com		cmp.b	#10,d5
		beq.b	dec_num
		call	put_hexnum
		clr.b	(a3)
		bra.b	p_prt2

p_addr		startline
		move.l	a5,d0
		bra.b	h8_com

p_char		move.b	(a5)+,d0
		call	to_printable
		call	JUMP,ChrOut

p_null		lea	null_txt(pc),a0
		call	JUMP,printstring_a0

;
; NUL-terminated string
;
p_string	bsr.b	p_align
		move.l	(a5)+,d2
		beq.b	p_null
; length limit 128
		startline
		move.l	d2,a1
		moveq	#100-1,d1
1$		move.b	(a1)+,d0
		beq.b	p_prt1
		call	to_printable
		move.b	d0,(a3)+
		dbf	d1,1$
p_prt1		clr.b	(a3)
p_prt2		call	JUMP,printstring

;
; BCPL string
;
p_bstr		bsr	p_align
		move.l	(a5)+,d2
		beq.b	p_null
		startline
		lsl.l	#2,d2
		move.l	d2,a1
		moveq	#0,d1
		move.b	(a1)+,d1
		cmp.b	#100,d1
		bcs.b	3$
		moveq	#100,d1
		bra.b	3$

2$		move.b	(a1)+,d0
		call	to_printable
		move.b	d0,(a3)+
3$		dbf	d1,2$
		bra.b	p_prt1

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
;  %c  -  (printable) ASCII character
;  %s  -  string (longword pointer to nul-terminated string)
;  %x  -  BCPL string
;
		cmd	fmt_memdisplay

		cmp.b	#' ',(a1)
		beq.b	0$
		subq.l	#1,a3
		bra	display_memory

0$		clr.l	mon_EndAddr(a4)
		call	skipspaces
		cmp.b	#'"',(a3)
		beq.b	get_fmtstring

		call	GetExpr
		move.l	d0,mon_CurrentAddr(a4)

		call	skipspaces
		cmp.b	#'"',(a3)
		beq.b	get_fmtstring

		call	GetExpr
		move.l	d0,mon_EndAddr(a4)

get_fmtstring	call	GetName
		move.l	d0,d7
		move.l	d0,a0
		tst.b	(a0)
		beq	generic_error

		move.l	mon_CurrentAddr(a4),a5

fmt_memdisp_next
		move.l	d7,a2

fmt_memdisp_loop
		move.l	a2,d2

1$		move.b	(a2)+,d0
		beq.b	p_end
		cmp.b	#'%',d0
		bne.b	1$

		subq.l	#1,a2
		bsr.b	p_putstr
		addq.l	#1,a2
		move.b	(a2)+,d0

		moveq	#16,d5		;base hex
		cmp.b	#'d',d0
		bne.b	10$
		moveq	#10,d5		;base decimal
		move.b	(a2)+,d0

10$		lea	p_chars(pc),a0
		move.l	a0,d1
2$		tst.b	(a0)
		beq.b	fmt_memdisp_loop
		cmp.b	(a0)+,d0
		bne.b	2$
		sub.l	d1,a0
		add.w	a0,a0
		lea	p_jumps-2(pc,a0.w),a0
		add.w	(a0),a0
		jsr	(a0)
		bra	fmt_memdisp_loop

p_str		bsr.b	p_putstr

p_end_test	tst.b	-1(a2)
		bne.b	fmt_memdisp_loop

p_end		cmp.l	mon_CurrentAddr(a4),a5
		beq	generic_error
		move.l	a5,mon_CurrentAddr(a4)
		bsr.b	p_putstr
		emit	LF
		call	CheckKeys
		bne.b	p_rts1

		cmp.l	mon_EndAddr(a4),a5
		bls	fmt_memdisp_next
p_rts1		rts

p_percent	emit	'%'
		bra	fmt_memdisp_loop

p_putstr	move.l	a2,d3
		sub.l	d2,d3
		beq.b	p_rts1
p_write		move.l	mon_OutputFile(a4),d1
		jlib	Dos,Write

;
; format character routines for mf-command
;
p_jumps		rw	p_byte
		rw	p_word
		rw	p_long
		rw	p_addr
		rw	p_char
		rw	p_string
		rw	p_bstr
		rw	p_percent
;
; format characters for mf-command
;
p_chars		dc.b	'bwlacsx%',0
		ds.w	0

;
;*** MODIFY MEMORY ***
;
		cmd	modifymem

		call	GetExpr
		move.l	D0,A0
		call	JUMP,GetString
;
;
comp_hunt_fmt	dc.b	'%08lx ',0

allocfail_txt	dc.b	'Allocation failed',LF,0
noalloc_txt	dc.b	'Not allocated that',LF,0

nomem_alloc_txt	dc.b	'No memory '
allocated_txt	dc.b	'allocated',LF,0
memlist_txt	dc.b	'Allocated memory:',LF,0

memlist_fmt	dc.b	'%3ld  $%08lx  $%08lx  %ld',LF,0
loc_txt		dc.b	'  #   startloc   endloc    length',LF,0

not_memlist_txt	dc.b	'Not in MemList',LF,0
inhunk_fmt	dc.b	'(in hunk %ld, offset $%lx)',LF,0
minfo_var_fmt	dc.b	' = %1.50s',LF,0

alloc_fmt	dc.b	'Allocated from $%08lx to $%08lx',LF,0

null_txt	dc.b	'<NULL>',0

		end
