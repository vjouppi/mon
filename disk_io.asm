;
; disk_io.asm
;
		include	"monitor.i"

		xref	ChrOut
		xref	ChrOutWin

		xref	showrange

		xref	generic_error
		xref	odd_address_error
		xref	out_memory_error

		xref	out_range_txt

*** READ & WRITE DISK ***
;#
;# v1.07->
;#  disk read and write use now a buffer in chip memory, and copy data
;#  from/to that buffer, so the actual transfer address does not need
;#  to be in chip memory.
;#
		cmd	disk_read

		moveq	#0,D7	;D7 is read/write flags
		bra.s	disk_rw

		cmd	disk_write

		moveq	#-1,D7

disk_rw		call	get_expr
		btst	#0,D0
		bne	odd_address_error
		move.l	D0,Addr(a4)
		beq	generic_error
		call	get_expr
		move.l	D0,D3		;Unit number (drive)
		call	get_expr
		move.l	D0,D4		;starting sector
		call	get_expr
		move.l	D0,D5		;length
		beq	generic_error		;error: zero length
		move.l	#DISKBLOCKSIZE,d0
		move.l	#MEMF_CLEAR!MEMF_CHIP,d1
		lib	Exec,AllocMem
		tst.l	d0
		beq	out_memory_error
		move.l	d0,a5
		call	MyCreatePort
		move.l	D0,D6
		beq	disk_io_9	;branch if CreatePort failed
		move.l	D0,A1
		moveq	#IOSTD_SIZE,D0
		call	MyCreateIO
		tst.l	D0
		beq	disk_io_8	;branch if CreateIO failed
		move.l	D0,A2

		move.l	D3,D0
		lea	DevNameBuf(a4),A0
		move.l	A2,A1
		moveq	#0,D1
		lib	OpenDevice
		tst.l	D0
		bne	opendev_fail

		move.l	a5,IO_DATA(A2)
		move.l	#DISKBLOCKSIZE,IO_LENGTH(A2)

		moveq	#DISKBLOCKSHIFT,D0
		lsl.l	D0,D4			multiply by DISKBLOCKSIZE
		move.l	D4,IO_OFFSET(A2)

		lsl.l	D0,D5			multiply by DISKBLOCKSIZE
		move.l	d5,d4

		tst.l	D7
		bne.s	disk_wr
;
; read from disk
;
disk_rd		move.w	#CMD_READ,IO_COMMAND(A2)	;read from disk
		move.l	A2,A1
		lib	DoIO
		tst.l	D0
		bne	disk_io_err
		move.l	#DISKBLOCKSIZE,d0
		move.l	a5,a0
		move.l	Addr(a4),a1
		lib	CopyMem
		move.l	#DISKBLOCKSIZE,d0
		add.l	d0,IO_OFFSET(a2)
		add.l	d0,Addr(a4)
		sub.l	d0,d4
		bgt.s	disk_rd
		move.l	d5,d6
		move.l	Addr(a4),d5
		sub.l	d6,d5
		bsr	showrange
		bra.s	disk_io_5
;
; write to disk
;
disk_wr		move.l	Addr(a4),a0
		move.l	a5,a1
		move.l	#DISKBLOCKSIZE,d0
		lib	CopyMem
		move.w	#CMD_WRITE,IO_COMMAND(A2)	;write to disk
		move.l	A2,A1
		lib	DoIO
		tst.l	D0
		bne.s	disk_io_err
		move.l	#DISKBLOCKSIZE,d0
		add.l	d0,IO_OFFSET(a2)
		add.l	d0,Addr(a4)
		sub.l	d0,d4
		bgt.s	disk_wr
		move.w	#CMD_UPDATE,IO_COMMAND(A2)	;make sure that the buffer is written
		move.l	A2,A1
		lib	DoIO
		tst.l	D0
		beq.s	disk_io_5

disk_io_err	;Print TrackDisk error number
		cmp.b	#IOERR_BADLENGTH,d0
		bne.s	00$
		lea	out_range_txt(pc),a0
		bra.s	02$
00$		cmp.b	#TDERR_DiskChanged,d0
		bne.s	01$
		lea	nodisktxt(pc),a0
		bra.s	02$
01$		cmp.b	#TDERR_WriteProt,d0
		bne.s	03$
		lea	wrprotxt(pc),a0
02$		call	printstring_a0_window
		bra.s	99$
03$		lea	td_errfmt(pc),a0
		call	printf_window
99$		emitwin	LF

disk_io_5	;stop drive motor
		move.w	#TD_MOTOR,IO_COMMAND(A2)
		clr.l	IO_LENGTH(A2)
		move.l	A2,A1
		lib	DoIO

disk_io_6	move.l	A2,A1
		lib	CloseDevice
		bra.s	disk_io_7

opendev_fail	lea	opendevfailfmt(pc),a0
		call	printf_window

disk_io_7	move.l	MN_REPLYPORT(a2),d6
		move.l	A2,A1
		call	MyDeleteIO

disk_io_8	move.l	D6,A1
		call	MyDeletePort

disk_io_9	move.l	a5,a1
		move.l	#DISKBLOCKSIZE,d0
		lib	FreeMem
d_mloop_1	rts

;
; set disk device, dev-command
;
		cmd	setshow_device

		call	skipspaces
		tst.b	(a3)
		beq.s	showdev

		call	GetName
		tst.l	d0
		beq	generic_error

		move.l	d0,a0
		lea	DevNameBuf(a4),a1
		moveq	#DNBUFSIZE-2,d1
01$		move.b	(a0)+,(a1)+
		dbeq	d1,01$
		clr.b	(a0)
		bra.s	d_mloop_1

showdev		lea	DevNameBuf(a4),a0
		call	printstring_a0
		emit	LF
		bra.s	d_mloop_1

;
; get parameters for checksum commands
;
get_sum_params	call	get_expr
		tst.l	d0
		beq.s	errx02
		btst	#0,d0		;error if odd address
		bne	odd_address_error
		move.l	D0,a5
		move.l	D0,a2
;
; d4 : block length (longwords)
; d5 : checksum offset
;
		call	skipspaces
		tst.b	(a3)
		beq.s	sum_1

		call	get_expr
		tst.l	d0
		beq.s	errx02
		move.l	d0,d4
		and.b	#3,d0
		bne.s	errx02

		call	get_expr
		move.l	d0,d5

sum_1		moveq	#0,D0
		move.l	d4,d1
		lsr.l	#2,d1
		rts

*** DISK BLOCK CHECKSUM ***
;
; extended to handle variable length blocks and block offsets.
; useful for example for RigidDiskBlock checksum calculations
; ( "= addr $100 $08" for rigiddiskblocks)
;
		cmd	block_check

		move.l	#DISKBLOCKSIZE,d4 ;normal values for disk blocks
		moveq	#4*5,d5

		bsr	get_sum_params

bl_check	add.l	(a5)+,D0
		subq.l	#1,d1
		bne.s	bl_check

		add.l	d5,a2		;checksum located at longword #5 in block
		move.l	(a2),D6
		sub.l	D0,(a2)
		move.l	(a2),D7
		bra.s	ShowSum

errx02		bra	generic_error

*** BOOTBLOCK CHECKSUM ***
;
; extended to handle variable length blocks and block offsets.
; useful for example for kickstart checksum calculations
;  ( "# <addr> $40000 $3ffe8" for pre-2.0 kickstart images)
;
		cmd	boot_check

		move.l	#BOOTBLOCKSIZE,d4 ;normal values for disk bootblocks
		moveq	#4,d5

		bsr	get_sum_params

boot_ch		add.l	(a5)+,D0
		bcc.s	boot_1
		addq.l	#1,D0		;remember to add the carry
boot_1		subq.l	#1,d1
		bne.s	boot_ch

		add.l	d5,a2		;checksum in second longword
		move.l	(a2),D6
		sub.l	D0,(a2)
		bcc.s	boot_2
		subq.l	#1,(a2)
boot_2		move.l	(a2),D7

*** SHOW OLD AND NEW CHECKSUM (jump from the previous two commands)
ShowSum ;old sum in D6, new sum in D7
		move.l	d6,d0
		move.l	d7,d1
		lea	sumfmt(pc),a0
		call	JUMP,printf

sumfmt		dc.b	'Old: $%08lx New: $%08lx',LF,0
nodisktxt	dc.b	'No disk in drive',0
wrprotxt	dc.b	'Disk write protected',0
opendevfailfmt	dc.b	'Can''t open device, error #%ld',LF,0
td_errfmt	dc.b	'Disk error %ld',0

		end
