;
; lister.asm -- list libraries, devices, resources, ports & semaphores
;
;
		nolist
		include "exec/types.i"
		include "exec/memory.i"
		include "exec/tasks.i"
		include "exec/execbase.i"
		include "libraries/dosextens.i"
		include	"libraries/configregs.i"
		include	"libraries/configvars.i"
		include	"hardware/intbits.i"
		include "offsets.i"
		list

		include	"monitor.i"

		xref	out_memory_error

;
; structure for storing library/device/etc data
;
		STRUCTURE BufData,0
		 APTR	bdat_NodeAddr
		 UWORD	bdat_Flags1
		 WORD	bdat_OpenCount
		 UWORD	bdat_Version
		 UWORD	bdat_Revision
		 STRUCT	bdat_Name,32
		LABEL	bdat_SIZE
;
; structure for storing task data
;
		STRUCTURE BufTaskData,0
		 APTR	btd_NodeAddr
		 UBYTE	btd_Type
		 UBYTE	btd_State
		 UBYTE	btd_Pri
		 STRUCT	btd_Name,32
		LABEL	btd_SIZE

;
; structure for storing interrupt data
;
		STRUCTURE BufIntData,0
		  ULONG  bid_NodeAddr
		  ULONG	 bid_CodeAddr
		  ULONG	 bid_DataAddr
		  UBYTE  bid_Type
		  UBYTE  bid_Pri
		  STRUCT bid_Name,32
		LABEL bid_SIZE

START_BUF_SIZE	equ	2048


		cmd	list_semaphores

		lea	res_hdr_txt(pc),a0
		call	printstring_a0
		move.w	#SemaphoreList,d0
		bra.s	ListResSem

		cmd	list_resources

		lea	res_hdr_txt(pc),a0
		call	printstring_a0
		move.w	#ResourceList,d0

ListResSem	lea	ResPrint(pc),a1
		bra.s	ListCommon

		cmd	list_ports

		lea	port_hdr_txt(pc),a0
		call	printstring_a0
		move.w	#PortList,d0
		lea	PortPrint(pc),a1
		bra.s	ListCommon

		cmd	list_devices

		lea	lib_hdr_txt(pc),a0
		call	printstring_a0
		move.w	#DeviceList,d0
		bra.s	ListLibsDevs

		cmd	list_libraries

		lea	lib_hdr_txt(pc),a0
		call	printstring_a0
		move.w	#LibList,d0

ListLibsDevs	lea	LibPrint(pc),a1

ListCommon	movem.l	d2-d6/a2/a3/a5,-(sp)
		move.l	a1,a5
		move.w	d0,d5
		move.l	#START_BUF_SIZE,d6

ll_alloc	move.l	d6,d0
		move.l	#MEMF_PUBLIC,d1
		lib	Exec,AllocMem
		tst.l	d0
		beq	out_memory_error
		move.l	d0,a2

		move.l	a2,a0
		move.l	d6,d0
		move.w	d5,d1
		bsr	CollectList
		bne.s	list_ok

		move.l	a2,a1
		move.l	d6,d0
		lib	Exec,FreeMem
		add.l	d6,d6	; *= 2
		bra.s	ll_alloc

list_ok		move.l	a2,a3

list_p_loop	tst.l	(a3)
		beq	list_end

		jsr	(a5)
		call	CheckKeys
		bne	list_end

1$		tst.b	(a3)+
		bne.s	1$
		move.l	a3,d0
		btst	#0,d0
		beq.s	2$
		addq.l	#1,a3
2$		bra.s	list_p_loop

list_end	move.l	a2,a1
		move.l	d6,d0
		lib	Exec,FreeMem

List_End	movem.l	(sp)+,d2-d6/a2/a3/a5
		rts
;
;
LibPrint	move.l	bdat_NodeAddr(a3),d0
		moveq	#0,d1
		move.w	bdat_Version(a3),d1
		moveq	#0,d2
		move.w	bdat_Revision(a3),d2
		move.w	bdat_OpenCount(a3),d3
		ext.l	d3	
		lea	bdat_Name(a3),a3
		move.l	a3,d4
		lea	lib_list_fmt(pc),a0
		call	JUMP,printf
;
;
PortPrint	move.l	bdat_NodeAddr(a3),d0
		moveq	#0,d1
		move.b	bdat_Flags1+1(a3),d1	;sigbit
		moveq	#0,d2
		move.b	bdat_Flags1(a3),d2	;flags
		lea	bdat_Name(a3),a3
		move.l	a3,d3
		lea	port_list_fmt(pc),a0
		call	JUMP,printf

ResPrint	move.l	bdat_NodeAddr(a3),d0
		lea	bdat_Name(a3),a3
		move.l	a3,d1
		lea	res_list_fmt(pc),a0
		call	JUMP,printf

;
; task list
;
		cmd	list_tasks

		lea	task_hdr_txt(pc),a0
		call	printstring_a0
		movem.l	d2-d6/a2/a3/a5,-(sp)
		move.l	#START_BUF_SIZE,d6

tl_alloc	move.l	d6,d0
		move.l	#MEMF_PUBLIC,d1
		lib	Exec,AllocMem
		tst.l	d0
		beq	out_memory_error
		move.l	d0,a2

		move.l	a2,a0
		move.l	d6,d0
		bsr	CollectTaskList
		bne.s	tasklist_ok

		move.l	a2,a1
		move.l	d6,d0
		lib	Exec,FreeMem
		add.l	d6,d6	; *= 2
		bra.s	tl_alloc

tasklist_ok	move.l	a2,a3

list_tasks	tst.l	(a3)
		beq	list_tasks_end

		move.l	btd_NodeAddr(a3),d0

		moveq	#'c',d1
		move.b	btd_Type(a3),d2
		cmp.b	#NT_PROCESS+1,d2
		beq.s	10$
		moveq	#'p',d1
		cmp.b	#NT_PROCESS,d2
		beq.s	10$
		moveq	#'t',d1
		cmp.b	#NT_TASK,d2
		beq.s	10$
		moveq	#'?',d1

10$		moveq	#0,d2
		move.b	btd_State(a3),d2
		cmp.b	#TS_REMOVED,d2
		bls.s	11$
		moveq	#TS_REMOVED+1,d2
11$		lea	t_states(pc),a0
		move.b	0(a0,d2.w),d2

		move.b	btd_Pri(a3),d3
		ext.w	d3
		ext.l	d3

		lea	btd_Name(a3),a3
		move.l	a3,d4
		lea	task_list_fmt(pc),a0
		call	printf

		call	CheckKeys
		bne	list_tasks_end

1$		tst.b	(a3)+
		bne.s	1$
		move.l	a3,d0
		btst	#0,d0
		beq.s	2$
		addq.l	#1,a3
2$		bra.s	list_tasks

list_tasks_end	move.l	a2,a1
		move.l	d6,d0
		lib	Exec,FreeMem

ListTasks_End	movem.l	(sp)+,d2-d6/a2/a3/a5
		rts


;
; buffer ptr in a0, length in d0, ListOffset in d1.w
;
CollectList	movem.l	d2/a2/a3,-(sp)
		move.l	a0,a3
		move.w	d1,d2
		move.l	a3,a2
		add.l	d0,a2
		sub.w	#bdat_SIZE,a2

		getbase	Exec
		lib	Forbid
		move.l	0(a6,d2.w),a0

collect_list_loop
		cmp.l	a2,a3
		bcc.s	list_bufover

		move.l	(a0),d0
		beq	collect_list_end

		move.l	a0,(a3)+
		move.w	LIB_FLAGS(a0),(a3)+
		move.w	LIB_OPENCNT(a0),(a3)+
		move.l	LIB_VERSION(a0),(a3)+
		move.l	LN_NAME(a0),d1
		beq.s	2$
		move.l	d1,a1
		moveq	#30,d1
1$		move.b	(a1)+,(a3)+
		dbeq	d1,1$
		beq.s	3$
2$		clr.b	(a3)+
3$		move.l	a3,d1
		btst	#0,d1
		beq.s	4$
		clr.b	(a3)+
4$		move.l	d0,a0
		bra.s	collect_list_loop

collect_list_end
		lib	Permit

list_end1	clr.l	(a3)+
		movem.l	(sp)+,d2/a2/a3
		moveq	#1,d0
		rts

list_bufover	lib	Permit

list_bufo1	movem.l	(sp)+,d2/a2/a3
		moveq	#0,d0
		rts

;
; (here we must Disable() -- Forbid() is not enough)
;
; buffer ptr a0, length d0
;
CollectTaskList	movem.l	d2/a2/a3,-(sp)
		move.l	a0,a3
		move.l	a3,a2
		add.l	d0,a2
		sub.w	#btd_SIZE,a2

		getbase	Exec
		lib	Disable

		move.l	TaskWait(a6),a0

tw_loop		cmp.l	a2,a3
		bcc.s	task_bufo
		move.l	(a0),d0
		beq.s	do_tr
		bsr.s	do_task
		move.l	d0,a0
		bra.s	tw_loop

do_tr		move.l	TaskReady(a6),a0

tr_loop		cmp.l	a2,a3
		bcc.s	task_bufo
		move.l	(a0),d0
		beq.s	do_curr
		bsr.s	do_task
		move.l	d0,a0
		bra.s	tr_loop

do_curr		move.l	ThisTask(a6),a0
		bsr.s	do_task

		lib	Enable
		bra.s	list_end1

task_bufo	lib	Enable
		bra.s	list_bufo1
;
; here we collect task data to buffer. if the task is a cli process,
; the command name is used, else task LN_NAME is used as task/process name.
;
do_task		move.l	a0,(a3)+
		move.b	LN_TYPE(a0),(a3)+
		move.b	TC_STATE(a0),(a3)+
		move.b	LN_PRI(a0),(a3)+
		cmp.b	#NT_PROCESS,LN_TYPE(a0)
		bne.s	task_name
		move.l	pr_CLI(a0),d1
		beq.s	task_name
		tst.l	pr_TaskNum(a0)
		beq.s	task_name
		addq.b	#1,-3(a3)
		lsl.l	#2,d1
		move.l	d1,a1
		move.l	cli_CommandName(a1),a1
		add.l	a1,a1
		add.l	a1,a1
		moveq	#0,d1
		move.b	(a1)+,d1
		move.b	#'[',(a3)+
		cmp.b	#28,d1
		bcs.s	2$
		moveq	#28,d1
		bra.s	2$
1$		move.b	(a1)+,(a3)+
2$		dbf	d1,1$
		move.b	#']',(a3)+
		clr.b	(a3)+
		bra.s	t_align

task_name	move.l	LN_NAME(a0),d1
		beq.s	2$
		move.l	d1,a1
		moveq	#30,d1
1$		move.b	(a1)+,(a3)+
		dbeq	d1,1$
		beq.s	t_align
2$		clr.b	(a3)+
t_align		move.l	a3,d1
		btst	#0,d1
		beq.s	rts1
		clr.b	(a3)+
rts1		rts

;
;
		cmd	list_expansion

		getbase	Expansion
		clra	a5

		lea	list_exp_hdr(pc),a0
		call	printstring_a0

list_exp_loop	moveq	#-1,d0
		moveq	#-1,d1
		move.l	a5,a0
		lib	FindConfigDev
		tst.l	d0
		beq.b	rts1
		move.l	d0,a5

		lea	list_exp_fmt1(pc),a0
		move.l	a5,d0
		move.l	cd_BoardAddr(a5),d1
		moveq	#0,d2
		move.w	cd_Rom+er_Manufacturer(a5),d2
		moveq	#0,d3
		move.b	cd_Rom+er_Product(a5),d3
		move.l	cd_BoardSize(a5),d4
		lsr.l	#8,d4
		lsr.l	#2,d4
		call	printf
		lea	list_exp_fmt2(pc),a0
		moveq	#0,d0
		move.b	cd_Flags(a5),d0
		moveq	#0,d1
		move.b	cd_Rom+er_Flags(a5),d1
		call	printf
		bra	list_exp_loop

;
; list interrupts
;
il_need_more_mem
		lib	Permit
		move.l	a2,a1
		move.l	d6,d0
		lib	FreeMem
		add.l	d6,d6
		bra.b	il_alloc

		cmd	list_interrupts

		lea	int_hdr_txt(pc),a0
		call	printstring_a0
		move.l	#START_BUF_SIZE,d6

il_alloc	move.l	d6,d0
		move.l	#MEMF_PUBLIC,d1
		lib	Exec,AllocMem
		tst.l	d0
		beq	out_memory_error
		move.l	d0,a2
		move.l	d0,a3

		lib	Forbid

		move.l	IVTBE+IV_NODE(a6),a0
		moveq	#INTB_TBE,d2
		bsr	int_entry
		beq	il_need_more_mem

		move.l	IVDSKBLK+IV_NODE(a6),a0
		moveq	#INTB_DSKBLK,d2
		bsr	int_entry
		beq	il_need_more_mem

		move.l	IVPORTS+IV_DATA(a6),a0
		move.b	#INTB_PORTS+$80,d2
		bsr	server_chain
		beq	il_need_more_mem

		move.l	IVCOPER+IV_DATA(a6),a0
		move.b	#INTB_COPER+$80,d2
		bsr	server_chain
		beq	il_need_more_mem

		move.l	IVVERTB+IV_DATA(a6),a0
		move.b	#INTB_VERTB+$80,d2
		bsr	server_chain
		beq	il_need_more_mem

		move.l	IVBLIT+IV_NODE(a6),a0
		moveq	#INTB_BLIT,d2
		bsr	int_entry
		beq	il_need_more_mem

		move.l	IVAUD0+IV_NODE(a6),a0
		moveq	#INTB_AUD0,d2
		bsr	int_entry
		beq	il_need_more_mem
		move.l	IVAUD1+IV_NODE(a6),a0
		moveq	#INTB_AUD1,d2
		bsr	int_entry
		beq	il_need_more_mem
		move.l	IVAUD2+IV_NODE(a6),a0
		moveq	#INTB_AUD2,d2
		bsr	int_entry
		beq	il_need_more_mem
		move.l	IVAUD3+IV_NODE(a6),a0
		moveq	#INTB_AUD3,d2
		bsr	int_entry
		beq	il_need_more_mem

		move.l	IVRBF+IV_NODE(a6),a0
		moveq	#INTB_RBF,d2
		bsr	int_entry
		beq	il_need_more_mem

		move.l	IVDSKSYNC+IV_NODE(a6),a0
		moveq	#INTB_DSKSYNC,d2
		bsr	int_entry
		beq	il_need_more_mem

		move.l	IVEXTER+IV_DATA(a6),a0
		move.b	#INTB_EXTER+$80,d2
		bsr	server_chain
		beq	il_need_more_mem

		clr.l	(a3)
		lib	Permit

		move.l	a2,a3

int_listing_loop
		move.l	(a3),d0
		beq	int_listing_end
		move.l	bid_CodeAddr(a3),d1
		move.l	bid_DataAddr(a3),d2
		move.b	bid_Pri(a3),d3
		ext.w	d3
		ext.l	d3
		lea	int_list_fmt1(pc),a0
		call	printf

		moveq	#'H',d1
		moveq	#0,d0
		move.b	bid_Type(a3),d0
		bpl.b	0$
		moveq	#'S',d1
0$		and.b	#$7f,d0
		lea	int_types(pc),a0
		call	getnth
		move.l	a0,d0
		exg	d0,d1

		lea	bid_Name(a3),a0
		move.l	a0,d2
		lea	int_list_fmt2(pc),a0
		call	printf

		call	CheckKeys
		bne	int_listing_end

		lea	bid_Name(a3),a3
1$		tst.b	(a3)+
		bne.b	1$
		move.l	a3,d0
		btst	#0,d0
		beq.s	2$
		addq.l	#1,a3
2$		bra.b	int_listing_loop

int_listing_end	move.l	a2,a1
		move.l	d6,d0
		lib	FreeMem
		rts

server_chain	move.l	(a0),a0

server_loop	move.l	(a0),d0
		beq.b	rt1
		bsr.b	int_entry
		beq	rt0
		move.l	d0,a0
		bra.b	server_loop

int_entry	move.l	a0,d1
		beq.b	rt1

		lea	-(bid_SIZE+2)(a2,d6.l),a1
		cmp.l	a1,a3
		bcc.b	rt0		

		move.l	a0,(a3)+
		move.l	IS_CODE(a0),(a3)+
		move.l	IS_DATA(a0),(a3)+
		move.b	d2,(a3)+	;int. type
		move.b	LN_PRI(a0),(a3)+
		move.l	LN_NAME(a0),d1
		beq.b	2$
		move.l	d1,a1
		moveq	#30,d1
1$		move.b	(a1)+,(a3)+
		dbeq	d1,1$
		beq.b	3$
2$		clr.b	(a3)+
3$		move.l	a3,d1
		btst	#0,d1
		beq.b	rt1
		clr.b	(a3)+	;word align
rt1		moveq	#1,d1	;clear the zero flag
		rts

rt0		moveq	#0,d1	;set the zero flag
		rts

int_types	dc.b	'SerialOut',0	;0
		dc.b	'DiskBlock',0	;1
		dc.b	'Soft',0	;2
		dc.b	'CiaA/Ext2',0	;4
		dc.b	'Copper',0	;5
		dc.b	'VBlank',0	;6
		dc.b	'Blitter',0	;7
		dc.b	'Audio0',0	;8
		dc.b	'Audio1',0	;9
		dc.b	'Audio2',0	;10
		dc.b	'Audio3',0	;11
		dc.b	'SerialIn',0	;12
		dc.b	'DiskSync',0	;13
		dc.b	'CiaB/Ext6',0	;14

int_hdr_txt	dc.b	'  Node      Code      Data    Pri       Type      Name',LF,0
int_list_fmt1	dc.b	'%08lx  %08lx  %08lx %4ld',0
int_list_fmt2	dc.b	'   %lc %-10s  %s',LF,0
;
;
task_hdr_txt	dc.b	'  Node   Type State Pri  Name',LF,0
task_list_fmt	dc.b	'%08lx   %lc   %lc   %3ld   %s',LF,0

t_states	dc.b	'iacrwez?'

lib_hdr_txt	dc.b	'  Node    Ver  Rev  Cnt   Name',LF,0
lib_list_fmt	dc.b	'%08lx %4ld %4ld %4ld   %s',LF,0
port_hdr_txt	dc.b	'  Node  SigBit Flags  Name',LF,0
port_list_fmt	dc.b	'%08lx  %3ld   %02lx    %s',LF,0
res_hdr_txt	dc.b	'  Node     Name',LF,0
res_list_fmt	dc.b	'%08lx   %s',LF,0

list_exp_hdr	dc.b	'ConfigDev  BoardAddr  Manuf. Prod. Size(KB) cd_Flags er_Flags',LF,0
list_exp_fmt1	dc.b	'%08lx   %08lx  %5ld   %3ld   %6ld',0
list_exp_fmt2	dc.b	'      %02lx      %02lx',LF,0

		end
