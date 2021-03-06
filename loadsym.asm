;
; loadsym.asm
;

;
; 27.9.1993 -- adding some buffering...
;

;
; this module defines the following public routine:
;
; load_symbols
;
		nolist
		include "exec/types.i"
		include "exec/memory.i"
		include "dos/dos.i"
		include	"dos/doshunks.i"
		include	"offsets.i"
		list

		include	"monitor.i"

		xref	out_memory_error

RBUFSIZE	equ	4096

;
; data structure used by load_symbols-routine
;
		STRUCTURE LSymData,0
		 WORD	lsym_RBufOffs
		 WORD	lsym_RBufMax
		 STRUCT	lsym_ReadBuf,RBUFSIZE
		 STRUCT	lsym_Buffer,256+4
		 LONG	lsym_SLen
		LABEL LSymData_SIZE

;
; read symbol information from executable file
;

		pub	load_symbols

		movem.l	d2-d7/a2/a3/a5,-(sp)
		move.l	a0,a2

		move.l	#LSymData_SIZE,d0
		move.l	#MEMF_CLEAR,d1
		lib	Exec,AllocMem
		tst.l	d0
		beq	out_memory_error
		move.l	d0,a5
		move.w	#RBUFSIZE,lsym_RBufOffs(a5)

		move.l	mon_NumHunks(a4),d0
		lsl.l	#(2+1),d0		;allocate 2*4*NumHunks bytes
		move.l	#MEMF_CLEAR,d1
		lib	AllocMem
		move.l	d0,mon_HunkTypeTable(a4)
		beq	out_mem1
		move.l	d0,a3

		move.l	a2,d1
		move.l	#MODE_OLDFILE,d2
		lib	Dos,Open
		move.l	d0,d6
		beq	lsym_open_err

		bsr	lsym_GetLong
		cmp.l	#HUNK_HEADER,d0
		bne	lsym_fmt_err

		bsr	lsym_GetLong
		tst.l	d0
		bne	lsym_fmt_err		;reslibs not supported...

		bsr	lsym_GetLong		;table size
		move.l	d0,a2
		bsr	lsym_GetLong		;first hunk #
		tst.l	d0
		bne	lsym_fmt_err		;error if not zero
		bsr	lsym_GetLong		;last hunk #
		addq.l	#1,d0
		cmp.l	a2,d0
		bne	overlay_err
		cmp.l	mon_NumHunks(a4),d0
		bne	lsym_fmt_err		;wrong number of hunks
		lsl.l	#2,d0			;-># of longwords
		bsr	lsym_Skip

		moveq	#0,d7			;hunk counter
		moveq	#0,d4			;symbol counter
		move.l	mon_SegList(a4),d5
		lsl.l	#2,d5

lsym_hunkloop	bsr	lsym_GetLong
		move.l	d0,d1
		and.l	#$3fffffff,d0
		cmp.l	#HUNK_CODE,d0
		bne.s	no_code
		move.l	d1,(a3)+
		bra.s	skip_n

no_code		cmp.l	#HUNK_DATA,d0
		bne.s	no_data
		move.l	d1,(a3)+

skip_n		bsr	lsym_GetLong
		lsl.l	#2,d0
		move.l	d0,(a3)+
skip_n1		bsr	lsym_Skip
		bra.s	lsym_hunkloop

no_data		cmp.l	#HUNK_BSS,d0
		bne.s	no_bss
		move.l	d1,(a3)+
		bsr	lsym_GetLong	;skip bss length
		lsl.l	#2,d0
		move.l	d0,(a3)+
		bra.s	lsym_hunkloop

no_bss		cmp.l	#HUNK_DEBUG,d0
		bne.s	no_debug
		bsr	lsym_GetLong
		lsl.l	#2,d0
		bra.s	 skip_n1

no_debug	cmp.l	#HUNK_OVERLAY,d0
		beq	overlay_err

		cmp.l	#HUNK_END,d0
		bne.s	no_end

		move.l	d5,a0
		move.l	(a0),d5
		lsl.l	#2,d5
		beq	lsym_end

		addq.l	#1,d7
		cmp.l	mon_NumHunks(a4),d7
		bcs.s	lsym_hunkloop
		bra	lsym_end

no_end		cmp.l	#HUNK_RELOC32,d0
		bne.s	no_reloc

reloc_loop	bsr	lsym_GetLong
		tst.l	d0
		beq	lsym_hunkloop
		addq.l	#1,d0
		lsl.l	#2,d0
		bsr	lsym_Skip
		bra.s	reloc_loop

no_reloc	cmp.l	#HUNK_SYMBOL,d0
		bne	lsym_fmt_err

symbol_loop	bsr	lsym_GetLong
		tst.l	d0
		beq	lsym_hunkloop
		cmp.l	#64,d0		;4 * 64 = 256
		bcc	lsym_fmt_err
		addq.l	#1,d0
		lsl.l	#2,d0
		move.l	d0,lsym_SLen(a5)
		exg	a2,d7
		moveq	#0,d7

sym_read_loop	bsr.w	lsym_GetLong
		lea	lsym_Buffer(a5),a0
		move.l	d0,0(a0,d7.l)
		addq.l	#4,d7
		cmp.l	lsym_SLen(a5),d7
		bcs.b	sym_read_loop

		exg	a2,d7

		move.l	-4(a0,a2.l),d0
		clr.b	-4(a0,a2.l)

		add.l	d5,d0
		addq.l	#4,d0
		lea	lsym_Buffer(a5),a0
		move.l	d7,d1
		call	set_variable
		tst.l	d0
		beq	lsym_mem_err
		addq.l	#1,d4
		bra	symbol_loop

lsym_end	move.l	d4,d0
		lea	nsym_fmt(pc),a0
		call	printf

lsym_ex97	move.l	d6,d1
		lib	Dos,Close

lsym_ex98	move.l	a5,a1
		move.l	#LSymData_SIZE,d0
		lib	Exec,FreeMem

		movem.l	(sp)+,d2-d7/a2/a3/a5
		rts

out_mem1	move.l	a5,a1
		move.l	#LSymData_SIZE,d0
		lib	Exec,FreeMem
		bra	out_memory_error

lsym_Skip	move.l	d6,d1
		move.l	d0,d2
		moveq	#0,d0
		move.w	lsym_RBufMax(a5),d0
		sub.w	lsym_RBufOffs(a5),d0
		sub.l	d0,d2
		moveq	#OFFSET_CURRENT,d3
		lib	Dos,Seek
		tst.l	d0
		bmi.s	lsym_read_err1
		move.w	#RBUFSIZE,lsym_RBufOffs(a5)
		rts

lsym_read_err1	addq.l	#4,sp

lsym_fmt_err
lsym_mem_err
lsym_read_err	lea	read_errtxt(pc),a0
lsym_msg1	call	printstring_a0_window
		bra.s	lsym_ex97

overlay_err	lea	overlay_err_txt(pc),a0
		bra.s	lsym_msg1

lsym_open_err	lea	open_errtxt(pc),a0
		call	printstring_a0_window
		bra.s	lsym_ex98


lsym_GetLong	move.w	lsym_RBufOffs(a5),d0
		cmp.w	lsym_RBufMax(a5),d0
		bcc.b	rd_more
		move.l	lsym_ReadBuf(a5,d0.w),d0
		addq.w	#4,lsym_RBufOffs(a5)
		rts
rd_more		move.l	d6,d1
		move.l	a5,d2
		addq.l	#lsym_ReadBuf,d2
		move.l	#RBUFSIZE,d3
		lib	Dos,Read
		moveq	#4,d1
		cmp.l	d1,d0
		blt.b	lsym_read_err1
		move.w	d0,lsym_RBufMax(a5)
		clr.w	lsym_RBufOffs(a5)
		bra.b	lsym_GetLong

read_errtxt
open_errtxt	dc.b	'Problems reading symbols',LF,0
overlay_err_txt	dc.b	'Overlays not supported',LF,0
nsym_fmt	dc.b	'(%ld symbols)',LF,0

		end
