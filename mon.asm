************************************
*                                  *
*    Amiga machine code monitor    *
*    Timo Rossi   1987-1988-1989   *
*                                  *
* v1.06 -> last mod. 1988-05-02    *
* v1.07 -> last mod. 1989-08-28    *
* v1.08 -> last mod. 1989-08-29    *
*                                  *
************************************

;
;1989-08-24 -->
; so it's time to start debugging this...first I must try if it
; assembles at all with a68k...(and with all the spaces converted to tabs...)
; it's really over a year ago I last assembled this...and it was with the
; metacomco assembler...oh, it seems that TxEd is working quite hard when I
; am trying to edit the start of this over 100kbyte file...
;

;
; there seems to be problems tracing traps (and chk & trapv instructions
; and divisions by zero). a trace exception occurs in the trap handler
; routine (in supervisor mode) and that can't be caught by TC_TRAPCODE
; because exec calls directly the guru alert routine if an exception
; happens in supervisor mode.
; now the problem exists also when you go or jump to a breakpoint, because
; the monitor internally tries to trace over it...
; there is nothing that can be done to this problem if you don't change
; the processor exception vector (and that is against the rules...)

;
; created a separate program, patchtrace to help with the trap-trace
; problem. it changes to processor trace-exception vector to point to
; a new routine that checks the supervisor mode bit and just executes rte
; if the trace exception happens in supervisor mode. this program will be
; included with the monitor.
;

;
; bugs to fix:
;  - the exg Dn,An problem					*fixed*
;  - the pc-relative indexed mode problem with the assembler	*fixed*
;  - signed offsets						*fixed*
;  - move.b Dn,An & addi/subi #imm,pcrel in assembler		*fixed*
;  - all the disassembler bugs displaying invalid codes as valid instructions
;	*eor: fixed* *mulu/muls An,Dn: fixed* *jmp: fixed* *and/or: fixed*
;	*movem: fixed*
;
; and if there is time to do these...
;  - display a7 as sp (this is easy)				*done*
;  - btst Dn,#imm	(is this really necessary?)		*done*
;  - go to breakpoint should trace over the breakpoint		*done*
;  - disk read/write should work with non-chip memory		*done*
;  - topaz60 default font support				*done*
;
; thse will probably be done in versions 1.08+...
;  - assembler should handle size specifiers better
;  - add expression evaluation routines...(calculator)
;  - 68010 support
;  - 68020/68881 support (quite difficult...)
;

;
; modifications in versions 1.07+:
;
;   1989-08-24  --> v1.07
;		- changed startline/endline/printstring so that there is
;		  no need to keep line buffer address in a0 all the time...
;		- fixed the exg-instruction handling in both assembler and
;		  disassembler
;		- added signed offsets and a new hex output routine
;		- stack pointer is now printed as 'sp' and 'sp' is also
;		  accepted in the assembler.
;		- number output routines changed to use RawDoFmt
;		- help and info texts are now printed one line at time
;		- go now traces over breakpoint in current pc address
;   1989-08-25  - disk read/write can now be used with non-chip memory
;		- dosbase is no longer held in a6 all the time
;		- assembler addressing mode parser works again...
;		- absolute short address is now displayed as signed
;		- trackdisk errors 'no disk' and 'write protected' are
;		  now displayed as text, not error number
;   1989-08-26	- changed register order in exg Dn,An, helps comparing
;		  with new disassembler. also disassembler now understands
;		  zero word paddings (by the linker, for example) and
;		  does not get "out of sync".
;		- added "play n times" option to play digisound command
;		- window width and height are now gotten from intuition
;		  using GetScreenData(). should work with morerows etc...
;		- edited GetInput, now all number output is done with
;		  printf_window, removed NumOut-routine.
;		- added a new command to set up a CLI command line
;		- added 6/8-digit hex output routine
;		- now default and Jump stack pointers are long word aligned
;		- print_text no longer outputs ascii NULLs at end of text
;   1989-08-27	- now uses 6-digit hex numbers in memdisplay & disassemble
;		  addresses only if preferences default font is topaz60
;		  now also comprare/hunt decide how many number to print
;		  on one line according the default font.
;		- tested the disassembler once again. no bugs found
;		  after fixing long immediate data output.
;		- removed one unnecessary test in RegisterList
;		- fixed some minor bugs in assembler
;		- added 'out of range' error message and range checking
;		  in pcrelative indexed addressing mode.
;   1989-08-28	- reformatted help text to use tabs, a little smaller
;		  executable size...
;		- the assembler no longer accepts extra garbage characters
;		  after a valid instruction
;		- the assembler now unserstands blo/bhs/slo/shs/dblo/sbhs
;		  and assembles them as bcs/bcc/scs/scc/dbcs/dbcc
;		- fixed some small bugs in the assembler (it accepted
;		  some illegal addressing modes and cmpm-instruction size
;		  specifiers did not work)
;
;	      --> version 1.07 ready for release. will include patchtrace
;		  program. updated document file.
;
;   1989-08-29 --> v1.08
;		- fixed the exception (trap) handling on 68010/68020
;		  (previously left an extra word on the supervisor stack)
;

;
; lots of includes...
;
	include	'exec/types.i'
	include	'exec/nodes.i'
	include	'exec/lists.i'
	include	'exec/errors.i'
	include	'exec/tasks.i'
	include	'exec/ports.i'
	include	'exec/memory.i'
	include	'exec/libraries.i'
	include	'exec/devices.i'
	include	'exec/io.i'
	include	'exec/interrupts.i'
	include	'exec/execbase.i'
	include	'devices/trackdisk.i'
	include	'devices/audio.i'
	include	'libraries/dos.i'
	include	'libraries/dosextens.i'
	include	'intuition/intuition.i'

*** This macro is an easy way to update the version number ***
VERSION	macro
	dc.b	'1.08'
	endm

*** macro to generate 16-bit self-relative addresses ***
rw	macro
	dc.w	\1-*
	endm

even	macro
	ds.w	0
	endm

*** macro to generate external references to library vector offsets ***
xlib	macro
	xref	_LVO\1
	endm

*** macros to call system routines ***
callsys	macro
	jsr	_LVO\1(A6)
	endm

callexe	macro
	move.l	_ExecBase,a6
	callsys	\1
	endm

calldos	macro
	move.l	dosbase(a5),a6
	callsys	\1
	endm

*** macro to display a single character ***
emit	macro
	moveq	#\1,D0
	bsr	ChrOut
	endm

*** start output line ***
startline	macro
	move.l	outbufadr(A5),A3
	endm

*** end output line (line feed+NULL) ***
endline	macro
	putchr	LF
	clr.b	(A3)
	endm

putchr	macro
	move.b	#\1,(a3)+
	endm

; definitions of the instruction size variable
BSIZE	equ	0
WSIZE	equ	1
LSIZE	equ	2

;
; symbol 'ExecBase' is already defined if you use the standard includes
; and include exec/execbase.i
;
_ExecBase	equ	4

*** the following system routines are used ***
	xlib	OpenLibrary
	xlib	CloseLibrary
	xlib	AllocMem
	xlib	AllocAbs
	xlib	FreeMem
	xlib	GetCC
	xlib	FindTask
	xlib	Forbid
	xlib	WaitPort
	xlib	Wait
	xlib	SetSignal
	xlib	GetMsg
	xlib	ReplyMsg
	xlib	Remove
	xlib	AllocSignal
	xlib	FreeSignal
	xlib	OpenDevice
	xlib	CloseDevice
	xlib	DoIO
	xlib	RawDoFmt
	xlib	CopyMem
	xlib	Open
	xlib	Close
	xlib	Read
	xlib	Write
	xlib	Input
	xlib	Output
	xlib	WaitForChar
	xlib	Info
	xlib	CurrentDir
	xlib	DeleteFile
	xlib	Examine
	xlib	ExNext
	xlib	Lock
	xlib	UnLock
	xlib	IoErr
	xlib	LoadSeg
	xlib	UnLoadSeg
	xlib	Execute
	xlib	GetPrefs
	xlib	GetScreenData

*** SOME SPECIAL CHARACTERS ***
CtrlC	equ	3	;control-c, break
CtrlE	equ	5	;control-key to edit existing assembler instruction
BS	equ	8	;backspace
TAB	equ	9	;tabulator
LF	equ	10	;line feed (new line)
CtrlK	equ	11	;cursor up
CLS	equ	12	;clear screen (form feed)
CR	equ	13	;carriage return (moves cursor to start to the current line)
CtrlX	equ	24	;control-x, clear input line
SPACE	equ	32
DEL	equ	127	;delete current char
CSI	equ	$9B	;control sequence introducer for ANSI-sequences
NCSI	equ	CSI-$100	;this one can be used with MOVEQ

COMMA	equ	','

*** SOME SPECIAL KEY CODES (returned by GetKey)
CURSOR_UP		equ	$0100
CURSOR_DOWN		equ	$0200
CURSOR_RIGHT		equ	$0300
CURSOR_LEFT		equ	$0400
SHIFT_CURSOR_UP		equ	$0500
SHIFT_CURSOR_DOWN	equ	$0600
SHIFT_CURSOR_LEFT	equ	$0700
SHIFT_CURSOR_RIGHT	equ	$0800

LEN	equ	100	;length of input & output buffers
NLINES	equ	10	;number of lines of command line history

ILLEGAL	equ	$4AFC	;illegal instruction (used by breakpoints)

*** BREAKPOINT STRUCTURE ***
* struct BreakPoint { 
*  struct BreakPoint *NextBreak; /* linked list */
*  CPTR   BreakAddress;  /* address of breakpoint */
*  UWORD  BreakContents; /* word contents of that location */
* };      /* because it is temporarily replaced by ILLEGAL */

brk_Address	equ	4
brk_Content	equ	8
brk_SIZE	equ	10

** local variables **
winfile		equ	-4	;file handle for main window
Addr		equ	-8	;current address, used by many commands
EndAddr		equ	-12	;end address for disassembler & memdisplay
dosbase		equ	-16	;dos library base
segment		equ	-20	;BPTR to currently loaded segment, zero if none
instrad		equ	-24	;address of the instruction (disassembler)
opcode		equ	-26	;operation code of instruction (disassembler)
outbufadr	equ	-30	;address of output buffer (avoids absolute addr mode)
memlist		equ	-34	;pointer to linked list of allocated memory
OldTrapCode	equ	-38	;TC_TRAPCODE, when monitor was started
WBenchMsg	equ	-42	;workbench startup message
thistask	equ	-46	;pointer to this tasks task control block
OutputFile	equ	-50	;output file handle (redirection)
BreakList	equ	-54	;pointer to linked list of breakpoints
size		equ	-56	;instruction size(0=B,1=W,2=L)
flags		equ	-58	;bit 0:breakpoints set, bit 15:60col mode
inpspecial	equ	-60	;input special mode, see GetInput:

	section	MonitorCode,CODE

;
; although the monitor can be started from workbench, that is not
; recommended, because if you run a program from the monitor it runs
; as a part of the monitor process and many program require CLI
; process environment (or if they are not started from a CLI they wait
; for workbench startup message which in that case doesn't arrive...).
; BCPL programs or other programs that use the internal BCPL library
; cannot be started from the monitor without a special calling routine
; that allocates BCPL stack and sets up the BCPL register environment...
; (not recommended...and using the BCPL library in your own programs is
; absolutely not recommended!)
;
*** THIS IS THE WORKBENCH/CLI STARTUP CODE ***
startup	link	A5,#-60		;allocate space for local variables
	clr.l	WBenchMsg(A5)
	suba.l	A1,A1	;A1:=0
	callexe	FindTask	;find our task
	move.l	D0,thistask(A5)
	move.l	D0,A4
	tst.l	pr_CLI(A4)		;started from CLI ?
	bne.s	main			;branch if yes
	lea	pr_MsgPort(A4),A0	;started from workbench
	callsys	WaitPort		;wait for WB startup message
	lea	pr_MsgPort(A4),A0
	callsys	GetMsg			;get it
	move.l	D0,WBenchMsg(A5)

main	move.l	#OutBuf,outbufadr(a5)
;
; Find the width and height of workbench screen before opening the window
; so this program works ok with NTSC & PAL & LACEWB & MoreRows etc..
; (how about A2024 or ECS superhires mode with kickstart 1.4???)
;
	lea	intuname(pc),A1
	moveq	#33,D0		  version 33: kickstart 1.2 or later
	callsys	OpenLibrary
	tst.l	D0
	beq	exit9
	move.l	d0,a6

	move.l	outbufadr(a5),a0
	lea	20(a0),a2
	suba.l	a1,a1
	moveq	#sc_Height+2,d0	  enought length to get width and height
	moveq	#WBENCHSCREEN,d1
	callsys	GetScreenData
	move.l	d0,d2		  save result

	move.l	a2,a0
	moveq	#1,d0		  get only default font height
	callsys	GetPrefs
	moveq	#0,d0
	cmp.b	#9,(a2)		  is it topaz60?
	seq	d0
	swap	d0
	lsr.l	#1,d0
	move.w	d0,flags(a5)	  now bit 15 is set if font is topaz60

	move.l	a6,a1
	callexe	CloseLibrary
	tst.l	d2
	beq	exit9

	move.l	outbufadr(a5),a0
	movem.w	sc_Width(a0),d0-d1	this sign-extends to long
	sub.w	#16,d1
;
; window "filename" is created using RawDoFmt in the output buffer
;
	lea	windowfmt(pc),a0
	bsr	fmtstring

	moveq	#0,D0
	move.l	D0,Addr(A5)			clear some variables
	move.l	D0,segment(A5)
	move.l	D0,memlist(A5)
	move.l	D0,BreakList(A5)
	clr.b	SkipBreakFlag
	lea	dosname(pc),A1
	callsys	OpenLibrary			Open the DOS library...
	tst.l	D0
	beq	exit9
	move.l	d0,dosbase(a5)
	move.l	D0,A6
	move.l	outbufadr(a5),d1
	move.l	#MODE_OLDFILE,D2
	callsys	Open				open the window
	move.l	D0,winfile(A5)
	beq	exit8
	move.l	D0,OutputFile(A5) 	;default output is monitor window

	lea	welcometxt(pc),A0
	bsr	printstring_a0_window		;display welcome message
	move.l	sp,A0
	sub.w	#$100,A0		;a safe area from this task's stack
	move.l	a0,d0
	and.b	#$fc,d0			;long word align
	move.l	d0,AddrRegs+4*7		;set stack pointer
	lea	trapreturn(pc),A1
	move.l	thistask(A5),A0
	move.l	TC_TRAPCODE(A0),OldTrapCode(A5)	;save old TrapCode
	move.l	A1,TC_TRAPCODE(A0)		;and set a new one

*** JUMP HERE AFTER EXECUTION OF A COMMAND ***
mainloop
	moveq	#0,D0		;clear CTRL-C/D/E/F flags
	move.l	#SIGBREAKF_CTRL_C!SIGBREAKF_CTRL_D!SIGBREAKF_CTRL_E!SIGBREAKF_CTRL_F,D1
	callexe	SetSignal
	lea	prompt(pc),A0
	bsr	printstring_a0_window	;display prompt
	moveq	#0,D0
	bsr	GetInput
	move.b	(A3)+,D0
	bne.s	ml1
	move.l	winfile(A5),D0	;empty command line
	cmp.l	OutputFile(A5),D0
	beq.s	mainloop
	move.l	outbufadr(A5),A0
	move.w	#$0A00,(A0)
	bsr	printstring_a0	;print line feed (useful with printer)
	bra.s	mainloop
ml1	bsr	tolower
	lea	comtable(pc),A0
	moveq	#0,D1		;find command...

findcom	cmp.b	(A0),D0
	beq.s	foundcom
	addq.l	#1,D1
	cmp.b	#$FF,(A0)+	;end of command table ??
	bne.s	findcom

error	lea	errtx(pc),A0	;command not found, print error message
	bsr	printstring_a0_window
	emit	LF
	bra.s	mainloop

foundcom			;command found, execute it
	add.l	D1,D1		;D1 = D1 * 2
	lea	comadrs(pc),A0
	add.l	D1,A0
	add.w	(A0),A0		;get command address
	jmp	(A0)		;jump to command

*** THE HELP COMMAND ***
help	lea	helptext(pc),A0
hinfo	bsr	printstring_a0
	bra.s	mainloop
*** INFO ***
info	lea	infotext(pc),A0
	bra.s	hinfo

*** EXIT FROM MONITOR ***
exit	move.l	thistask(A5),A0
	move.l	OldTrapCode(A5),TC_TRAPCODE(A0)	;restore old TrapCode
	bsr	FreeAllMem		;free all memory allocated by commands & and (
	bsr	remove_all_breaks	;remove all breakpoints (free memory)
	move.l	dosbase(a5),a6
	move.l	segment(A5),D1		;if a segment is loaded, unload it
	beq.s	exit7
	callsys	UnLoadSeg

exit7	move.l	OutputFile(A5),D1	;if output is redirected, close output file
	cmp.l	winfile(A5),D1
	beq.s	exit7a
	callsys	Close

exit7a	move.l	winfile(A5),D1
	callsys	Close			;close window file

exit8	move.l	A6,A1
	callexe	CloseLibrary		;close dos library

exit9	move.l	WBenchMsg(A5),D3	;started from workbench??
	beq.s	exit99
	callsys	Forbid		;forbid, so WB can't UnloadSeg before exit
	move.l	D3,A1
	callsys	ReplyMsg	;reply the WB startup message

exit99	unlk	A5	;clean up stack
	moveq	#0,D0	;error return code
	rts		;return control to the system...

*** DIRECTORY ***
dir	addq.l	#2,A3
	moveq	#0,D7
	move.l	#fib_SIZEOF,D0	;allocate FileInfoBlock
*** we use the same memory space for the InfoData structure
	move.l	#MEMF_PUBLIC!MEMF_CLEAR,D1
	callexe	AllocMem
	tst.l	D0
	beq	OutOfMem
	move.l	D0,A4		;we keep the fib-pointer in A4
	bsr	skipspaces
	move.l	A3,D1		;use rest of command line as a name
	moveq	#SHARED_LOCK,D2
	calldos	Lock
	move.l	D0,D7		;we keep the lock pointer in D7
	beq.s	dir7		;if i/o error
	move.l	D7,D1
	move.l	A4,D2
	callsys	Examine
	tst.l	D0
	beq.s	dir7		;branch if Examine failed
	tst.l	fib_DirEntryType(A4)
	bpl.s	dir1
	bsr.s	DisplayDirLine	;if Examine found a file, display information about it
	bra.s	dir7b		;then number of free blocks
dir1	;Examine found a directory, ExNext it
	move.l	D7,D1
	move.l	A4,D2
	callsys	ExNext
	tst.l	D0
	bne.s	dir2
	callsys	IoErr
	cmp.l	#ERROR_NO_MORE_ENTRIES,D0
	bne.s	dir7a		;branch if a real dos error
	bra.s	dir7b		;no more entries, the display free blocks
dir2	bsr.s	DisplayDirLine
	bne.s	dir7b		;branch if user pressed break (control-c)
	bra.s	dir1
dir7	callsys	IoErr
dir7a	tst.l	D0
	beq.s	dir7c
	bsr	DOSErr
	bra.s	dir7c
dir7b	;directory ready, now display number of free blocks
	move.l	D7,D1
	move.l	A4,D2
	callsys	Info		;get info about the disk
	tst.l	D0
	beq.s	dir7
	move.l	id_NumBlocks(A4),D0
	sub.l	id_NumBlocksUsed(A4),D0
	lea	freeblkfmt(pc),a0
	bsr	printf
dir7c	move.l	D7,D1
	callsys	UnLock
dir8	move.l	A4,A1
	move.l	#fib_SIZEOF,D0
	callexe	FreeMem	;free FileInfoBlock
jmp_mainloop_0	bra	mainloop

OutOfMem	;print error message 'out of memory'
	lea	memerr(pc),A0
	bsr	printstring_a0_window
	bra.s	jmp_mainloop_0

*** PRINT ONE LINE OF DIREECTORY ***
DisplayDirLine	;fib-pointer in A4
	startline
	lea	fib_FileName(A4),A1
	moveq	#0,D1
txmovloop	;copy file name to output buffer
	move.b	(A1)+,D0
	beq.s	txmov2
	move.b	D0,(A3)+
	addq.w	#1,D1
	cmp.w	#30,D1	;max 30 chars
	bcs.s	txmovloop
txfloop	putchr	SPACE
	addq.w	#1,D1
txmov2	cmp.w	#24,D1
	bcs.s	txfloop
	putchr	SPACE
	tst.l	fib_DirEntryType(A4)	;positive=dir, negative=file
	bmi.s	txputlen
	lea	dnam(pc),A1	;'(dir)'
	bsr	putstring
	bra.s	txmov9
txputlen
	movem.l	a2/a6,-(sp)
	lea	numfmt(pc),a0
	lea	fib_Size(a4),a1
	lea	putch(pc),a2
	callexe	RawDoFmt
	movem.l	(sp)+,a2/a6
	bra.s	txmov10
txmov9	endline
txmov10	bsr	printstring
	bra	CheckKeys

*** PRINT DOS ERROR NUMBER ***
DOSErr		;error number in D0
	lea	doserrfmt(pc),a0
	bra	printf_window

**** LOAD SEGMENT ****
loadseg	tst.l	segment(A5)
	bne.s	oldseg		;can't do this before old segment is unloaded
	bsr	skipspaces
	move.l	A3,D1
	calldos	LoadSeg
	move.l	D0,segment(A5)
	beq.s	segerr		;branch if LoadSeg failed
	lea	segadrmes(pc),a0
	lsl.l	#2,d0
	addq.l	#4,d0
	move.l	d0,regPC
	move.l	d0,Addr(a5)
	bsr	printf
	bra.s	mjump
oldseg		;message 'unload old segment first'
	lea	ulserr(pc),A0
	bsr	printstring_a0_window
	bra.s	mjump
segerr		;come here if LoadSeg failed
	callsys	IoErr
	bsr.s	DOSErr
mjump	bra	mainloop
nosegerr	;error 'no segment loaded'
	lea	nosegmes(pc),A0
	bsr	printstring_a0_window
	bra.s	mjump

**** UNLOAD SEGMENT ****
unloadseg
	move.l	segment(A5),D1
	beq.s	nosegerr	;branch if no segment
	calldos	UnLoadSeg
	clr.l	segment(A5)	;remember to clear the segment pointer
	bra.s	mjump

**** SEGMENT LIST ****
seglist
	move.l	segment(A5),D3
	beq.s	nosegerr	;branch if no segment
	lea	seghead(pc),A0
	bsr	printstring_a0
	lea	loctext(pc),A0
	bsr	printstring_a0
segloop	lsl.l	#2,d3
	move.l	d3,a2
	move.l	d3,d0
	addq.l	#4,d0
	move.l	-4(a2),d2
	subq.l	#8,d2
	move.l	d0,d1
	add.l	d2,d1
	subq.l	#1,d1
	lea	memlistfmt(pc),a0
	bsr	printf
	move.l	(A2),D3		;get next segment pointer
	bsr	CheckKeys
	bne.s	mjump2
	tst.l	D3
	bne.s	segloop
mjump2	bra	mainloop

*** SHOW SEGMENT LIST & ALLOCATED MEMORY ***
show	bsr	skipspaces
	move.b	(A3),D0
	bsr	tolower
	cmp.b	#'l',D0
	beq.s	seglist
	cmp.b	#'m',D0
	bne	error

*** MEMORY LIST ***
showmemlist
	move.l	memlist(A5),D5
	bne.s	showm2
not_at_all_allocated
	lea	no_alloc(pc),A0
	bsr	printstring_a0_window	;error 'no memory allocated'
	bra	mainloop
showm2
	lea	memlisttx(pc),A0
	bsr	printstring_a0
	lea	loctext(pc),A0
	bsr	printstring_a0
showmemloop
	move.l	d5,a2
	move.l	a2,d0
	addq.l	#8,d0
	move.l	4(a2),d2
	move.l	d0,d1
	add.l	d2,d1
	subq.l	#1,d1
	lea	memlistfmt(pc),a0
	bsr	printf
	bsr	CheckKeys
	bne.s	showmem_end
	move.l	(A2),D5
	bne.s	showmemloop
showmem_end
	bra	mainloop

*** FREE ALL MEMORY ALLOCATED WITH THE ( AND & COMMANDS ***
FreeAllMem
	move.l	a6,-(sp)
	move.l	memlist(A5),D5
	beq.s	freeall9
	move.l	_ExecBase,A6
freeall_loop
	move.l	D5,A1
	move.l	4(A1),D0
	addq.l	#8,D0
	move.l	(A1),D5
	callsys	FreeMem
	tst.l	D5
	bne.s	freeall_loop
	clr.l	memlist(A5)
freeall9
	move.l	(sp)+,a6
	rts

*** ALLOCATE MEMORY ***
allocate_mem
	bsr	GetNum
	tst.l	D0
	beq	error
	move.l	D0,D5
	bsr	GetNum
	moveq	#0,D1
	cmp.l	#'CHIP',D0	;check if chip memory required
	bne.s	alloc_1
	moveq	#MEMF_CHIP,D1
alloc_1
	move.l	D5,D0
	addq.l	#8,D0
	or.l	#MEMF_CLEAR!MEMF_PUBLIC,D1
	callexe	AllocMem
	tst.l	D0
	beq.s	allocfailed
	move.l	D0,A0
alloc_mem_1	;add allocated memory to linked list of memory blocks
** A0 points to memory block, D5 is length
	move.l	D5,4(A0)
	moveq	#0,D0
	move.l	memlist(A5),D1
alloc_find1
	beq.s	alloc_find2	;keep the linked list in order, lowest address first
	move.l	D1,A1
	cmp.l	A1,A0
	bcs.s	alloc_find2
	move.l	D1,D0
	move.l	(A1),D1
	bra.s	alloc_find1
alloc_find2
	tst.l	D0
	bne.s	alloc_do2
	move.l	memlist(A5),(A0)	;add new memory node to start of list
	move.l	A0,memlist(A5)
	bra.s	alloc_display
alloc_do2
	move.l	D1,(A0)		;add new memory node to middle or end of list
	move.l	D0,A1
	move.l	A0,(A1)
alloc_display
	move.l	a0,d0
	addq.l	#8,d0
	move.l	d0,d1
	add.l	4(a0),d1
	subq.l	#1,d1
	lea	allocfmt(pc),a0
	bsr	printf
jmp_mainloop_1
	bra	mainloop

allocfailed		;error 'allocation failed'
	lea	allocfail(pc),A0
	bsr	printstring_a0_window
	bra.s	jmp_mainloop_1

*** ALLOCATE ABSOLUTE ***
alloc_abs
	bsr	GetNum
	subq.l	#8,D0	;remember to subtract 8 from the starting address
	move.l	D0,D7	;(next block pointer & length)
	bsr	GetNum
	move.l	D0,D5
	beq	error
	addq.l	#8,D0	;add 8 to length
	move.l	D7,A1
	callexe	AllocAbs
	tst.l	D0
	beq.s	allocfailed
	move.l	D7,A0
	bra	alloc_mem_1

*** FREE MEMORY ***
free_mem
	bsr	skipspaces
	cmp.b	#'a',(A3)	;check 'all'
	bne.s	free_norm
	moveq	#'l',D0
	cmp.b	1(A3),D0
	bne.s	free_norm
	cmp.b	2(A3),D0
	bne.s	free_norm
	bsr	FreeAllMem
	bra	mainloop
free_norm
	bsr	GetNum
	subq.l	#8,D0
	move.l	memlist(A5),D1
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
	bsr	printstring_a0_window
	bra	mainloop
found_do_free_mem	;remove memory block from linked list
	tst.l	D2
	bne.s	notfirst
	move.l	(A1),memlist(A5)
	bra.s	do_free
notfirst
	move.l	D2,A0
	move.l	(A1),(A0)

do_free	move.l	4(A1),D0
	addq.l	#8,D0
	callexe	FreeMem
	bra	mainloop

*** SAVE ABSOLUTE (using DOS Write)****
abs_save
	bsr	GetNum
	move.l	D0,A4
	bsr	GetNum
	move.l	D0,D6
	bsr	skipspaces
	move.l	A3,D1
	move.l	#MODE_NEWFILE,D2
	calldos	Open
	move.l	D0,D7
	bne.s	abs_save_1

dos_err	callsys	IoErr
	bsr	DOSErr
	bra.s	jmp_mainloop_2a

abs_save_1
	move.l	D7,D1
	move.l	A4,D2
	move.l	D6,D3
	callsys	Write
	move.l	D7,D1
	callsys	Close
	bra.s	jmp_mainloop_2a

*** LOAD ABSOLUTE (using DOS Read) ***
abs_load
	bsr	GetNum
	move.l	D0,A4
	bsr	skipspaces
	move.l	A3,D1
	move.l	#MODE_OLDFILE,D2
	calldos	Open	;open file
	move.l	D0,D7
	beq.s	dos_err
	move.l	D7,D1
	move.l	A4,D2
	move.l	#$7FFFFFFF,D3	;MaxInt (the file can't be longer than this)
	callsys	Read		;read from file, until EOF, return actual length
	tst.l	D0
	ble.s	abs_load_1
	move.l	A4,D5
	move.l	D0,D6
	bsr.s	showrange
abs_load_1
	move.l	D7,D1
	callsys	Close	;close the file
jmp_mainloop_2a
	bra.s	jmp_mainloop_2

*** REDIRECT OUTPUT ***
redirect
	move.l	dosbase(a5),a6
	move.l	OutputFile(A5),D1	;is output currently redirected
	cmp.l	winfile(A5),D1
	beq.s	redir1
	callsys	Close			;if so, then close redirection file
	move.l	winfile(A5),OutputFile(A5)	;standard output
redir1	bsr	skipspaces
	tst.b	(A3)
	beq.s	jmp_mainloop_2
	move.l	A3,D1
	move.l	#MODE_NEWFILE,D2
	callsys	Open	;open redirection file
	tst.l	D0
	beq	dos_err
	move.l	D0,OutputFile(A5)
jmp_mainloop_2
	bra	mainloop

*** NEW CLI ***
new_cli	; Execute("NewCLI CON:....",0,winfile)
	lea	NewCliCom(pc),A0
	move.l	A0,D1
	moveq	#0,D2
	move.l	winfile(A5),D3
	calldos	Execute
	bra.s	jmp_mainloop_2

showrange	;start addr in D5, length in D6
	move.l	d5,d1
	move.l	d6,d0
	move.l	d1,d2
	add.l	d0,d2
	subq.l	#1,d2
	lea	rangefmt(pc),a0
	bra	printf

*** READ & WRITE DISK ***
;
; v1.07->
;  disk read and write use now a buffer in chip memory, and copy data
;  from/to that buffer, so the actual transfer address does not need
;  to be in chip memory.
;
disk_read
	moveq	#0,D7	;D7 is read/write flags
	bra.s	disk_rw
disk_write
	moveq	#-1,D7

disk_rw	bsr	GetNum
	btst	#0,D0
	bne	error		;error: odd address
	move.l	D0,Addr(A5)
	beq	error
	bsr	GetNum
	move.l	D0,D3		;Unit number (drive)
	bsr	GetNum
	move.l	D0,D4		;starting sector
	bsr	GetNum
	move.l	D0,D5		;length
	beq	error		;error: zero length
	move.l	#512,d0
	move.l	#MEMF_CLEAR!MEMF_CHIP,d1
	callexe	AllocMem
	tst.l	d0
	beq	OutOfMem
	move.l	d0,a4
	bsr	MyCreatePort
	move.l	D0,D6
	beq	disk_io_9	;branch if CreatePort failed
	move.l	D0,A1
	moveq	#IOSTD_SIZE,D0
	bsr	MyCreateIO
	tst.l	D0
	beq	disk_io_8	;branch if CreateIO failed
	move.l	D0,A2
	move.l	D3,D0
	lea	tdname(pc),A0
	move.l	A2,A1
	moveq	#0,D1
	callsys	OpenDevice
	tst.l	D0
	bne	disk_io_7		branch if OpenDevice failed

	move.l	a4,IO_DATA(A2)
	move.l	#512,IO_LENGTH(A2)

	moveq	#9,D0
	lsl.l	D0,D4			multiply by 512
	move.l	D4,IO_OFFSET(A2)

	lsl.l	D0,D5			multiply by 512
	move.l	d5,d4

	tst.l	D7
	bne.s	disk_wr
;
; read from disk
;
disk_rd	move.w	#CMD_READ,IO_COMMAND(A2)	;read from disk
	move.l	A2,A1
	callsys	DoIO
	tst.l	D0
	bne.s	disk_io_err
	move.l	IO_ACTUAL(a2),d0
	move.l	a4,a0
	move.l	Addr(a5),a1
	callsys	CopyMem
	move.l	IO_ACTUAL(a2),d0
	add.l	d0,IO_OFFSET(a2)
	add.l	d0,Addr(a5)
	sub.l	d0,d4
	bgt.s	disk_rd
	move.l	d5,d6
	move.l	Addr(a5),d5
	sub.l	d6,d5
	bsr	showrange
	bra.s	disk_io_5
;
; write to disk
;
disk_wr	move.l	Addr(a5),a0
	move.l	a4,a1
	move.l	#512,d0
	callsys	CopyMem
	move.w	#CMD_WRITE,IO_COMMAND(A2)	;write to disk
	move.l	A2,A1
	callsys	DoIO
	tst.l	D0
	bne.s	disk_io_err
	move.l	IO_ACTUAL(a2),d0
	add.l	d0,IO_OFFSET(a2)
	add.l	d0,Addr(a5)
	sub.l	d0,d4
	bgt.s	disk_wr
	move.w	#CMD_UPDATE,IO_COMMAND(A2)	;make sure that the buffer is written
	move.l	A2,A1
	callsys	DoIO
	tst.l	D0
	beq.s	disk_io_5

disk_io_err	;Print TrackDisk error number
	cmp.b	#IOERR_BADLENGTH,d0
	bne.s	00$
	lea	outrangetxt(pc),a0
	bra.s	02$
00$	cmp.b	#TDERR_DiskChanged,d0
	bne.s	01$
	lea	nodisktxt(pc),a0
	bra.s	02$
01$	cmp.b	#TDERR_WriteProt,d0
	bne.s	03$
	lea	wrprotxt(pc),a0
02$	bsr	printstring_a0_window
	bra.s	disk_io_5
03$	lea	td_errfmt(pc),a0
	bsr	printf_window

disk_io_5	;stop drive motor
	move.w	#TD_MOTOR,IO_COMMAND(A2)
	clr.l	IO_LENGTH(A2)
	move.l	A2,A1
	callsys	DoIO
disk_io_6
	move.l	A2,A1
	callsys	CloseDevice
disk_io_7
	move.l	MN_REPLYPORT(a2),d6
	move.l	A2,A1
	bsr	MyDeleteIO
disk_io_8
	move.l	D6,A1
	bsr	MyDeletePort
disk_io_9
	move.l	a4,a1
	move.l	#512,d0
	callsys	FreeMem
	bra	mainloop

*** PLAY DIGITIZED SOUND ***
digisound
	bsr	GetNum
	btst	#0,D0	;test: if the address if odd, then error
	beq.s	digi1
digi_err
	bra	error

digi1	move.l	D0,D5
	bsr	GetNum
	tst.l	D0
	beq.s	digi_err	;error: zero length
	btst	#0,D0
	bne.s	digi_err
	move.l	D0,D6
	bsr	GetNum		;period (speed)
	move.w	D0,D7
	bsr	GetNum		;# of cycles, defaults to zero (loop)
	move.w	d0,size(a5)
	bsr	MyCreatePort
	move.l	d0,d2
	beq	digi9
	move.l	D0,A1
	moveq	#ioa_SIZEOF,D0
	bsr	MyCreateIO
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
	callexe	OpenDevice	;open audio.device
	tst.l	D0
	bne.s	digi7
	move.l	D5,ioa_Data(A2)
	move.l	D6,ioa_Length(A2)
	move.w	D7,ioa_Period(A2)
	move.w	size(a5),ioa_Cycles(A2)
	move.w	#64,ioa_Volume(A2)	;maximum volume
	move.b	#ADIOF_PERVOL,IO_FLAGS(A2) ;flag to set the volume & period
	move.w	#CMD_WRITE,IO_COMMAND(A2)	;audio output=CMD_WRITE
	move.l	A2,A1
	BEGINIO	;can't use SendIO, because it clears the ADIO_PERVOL flag
	lea	audiotxt(pc),A0
	bsr	printstring_a0_window		;message: 'press Ctrl-C...'
	moveq	#0,D0
	move.l	#SIGBREAKF_CTRL_C,d2
	move.l	d2,d1
	callsys	SetSignal	;clear CTRL-C signal
	move.l	d2,d0
	move.l	MN_REPLYPORT(a2),a0
	move.b	MP_SIGBIT(a0),d3
	bset	d3,d0			wait until the sound finishes or
	callsys	Wait			user presses Ctrl-C
	btst	d3,d0
	beq.s	01$
	move.l	a2,a1
	callsys	Remove		remove the message from the port
01$	move.l	A2,A1
	callsys	CloseDevice
digi7	move.l	MN_REPLYPORT(a2),d2
	move.l	A2,A1
	bsr	MyDeleteIO
digi8	move.l	d2,A1
	bsr	MyDeletePort
digi9	bra	mainloop

*** DISK BLOCK CHECKSUM ***
block_check
	bsr	GetNum
	tst.l	D0	;just checking...
	beq	error
	move.l	D0,A0
	move.l	D0,A1
	moveq	#0,D0
	moveq	#512/4-1,D1	;disk block size 512 bytes
bl_check
	add.l	(A0)+,D0
	dbf	D1,bl_check
	add.w	#5*4,A1	;checksum located at longword #5 in block
	move.l	(A1),D6
	sub.l	D0,(A1)
	move.l	(A1),D7
	bra.s	ShowSum

*** BOOTBLOCK CHECKSUM ***
boot_check
	bsr	GetNum
	tst.l	D0		;just checking...
	beq	error
	move.l	D0,A0
	move.l	D0,A1
	moveq	#0,D0
	move.w	#1024/4-1,D1	;bootblock size 1024 bytes
boot_ch	add.l	(A0)+,D0
	bcc.s	boot_1
	addq.l	#1,D0		;remember to add the carry
boot_1	dbf	D1,boot_ch
	addq.l	#4,A1		;checksum in second longword
	move.l	(A1),D6
	sub.l	D0,(A1)
	bcc.s	boot_2
	subq.l	#1,(A1)
boot_2	move.l	(A1),D7

*** SHOW OLD AND NEW CHECKSUM (jump from the previous two commands)
ShowSum ;old sum in D6, new sum in D7
	move.l	d6,d0
	move.l	d7,d1
	lea	sumfmt(pc),a0
	bsr	printf
	bra	mainloop

*** DISPLAY NUMBER IN HEX, DECIMAL, OCTAL AND BINARY ***
number	bsr	GetNum
	move.l	D0,D5
	lea	put_hexnum(pc),A4	;output routine address
	move.l	#' hex',D2
	bsr.s	numzump
	lea	putdec1(pc),A4
	move.l	#' dec',D2
	bsr.s	numzump
	lea	octnum(pc),A4
	move.l	#' oct',D2
	bsr.s	numzump
	lea	binnum(pc),A4
	move.l	#' bin',D2
	bsr.s	numzump
	bra	mainloop

numzump ;print number in one base (both signed & unsigned if negative)
	startline
	tst.l	D5
	bpl.s	num_A1
	lea	signtxt(pc),A1
	bsr	putstring

num_A1	move.l	D2,D0
	bsr	PutLong
	putchr	<':'>
	putchr	SPACE
	cmp.w	#'ec',D2
	beq.s	num_z1
	putchr	SPACE

num_z1	move.l	D5,D0
	moveq	#8,d1
	jsr	(A4)
	putchr	LF
	tst.l	D5
	bpl.s	num_A2
	moveq	#SPACE,D0
	move.b	D0,(A3)+
	move.b	D0,(A3)+
	lea	signtxt+2(pc),A1
	bsr	putstring
	move.l	D2,D0
	bsr	PutLong
	putchr	<':'>
	putchr	SPACE
	move.l	D5,D0
	neg.l	D0
	putchr	<'-'>
	moveq	#8,d1
	jsr	(A4)
	putchr	LF

num_A2	clr.b	(A3)
	bra	printstring

*** OCTAL NUMBER OUTPUT ***
octnum	move.l	D2,-(sp)
	putchr	<'0'>
	moveq	#-1,D2
	tst.l	D0
	beq.s	oct3
oct1	move.b	D0,D1
	lsr.l	#3,D0	
	and.b	#7,D1
	add.b	#'0',D1
	move.b	D1,-(sp)
	addq.w	#1,D2
	tst.l	D0
	bne.s	oct1
oct2	move.b	(sp)+,(A3)+
	dbf	D2,oct2
oct3	move.l	(sp)+,D2
	rts

*** BINARY NUMBER OUTPUT ***
binnum	move.l	D2,-(sp)
	putchr	<'%'>
	moveq	#31,D2
bin1
	lsl.l	#1,D0
	move.b	#'0'/2,D1
	roxl.b	#1,D1
	move.b	D1,(A3)+
	dbf	D2,bin1
	move.l	(sp)+,D2
	rts	

*** DECIMAL OUTPUT WITH '+' IF NECESSARY ***
putdec1	cmp.b	#'-',-1(A3)
	beq.s	putdec2
	putchr	<'+'>
putdec2
*** DECIMAL NUMBER OUTPUT ***
PutNum	move.l	D2,-(sp)
	moveq	#-1,D2
01$	; 32 bit division
	move.w	D0,-(sp)	;save low word
	clr.w	D0
	swap	D0		;D0.L == high word
	divu	#10,D0
	move.w	D0,D1		;D1.W == quotient high
	move.w	(sp)+,D0
	divu	#10,D0
	swap	D1
	move.w	D0,D1
	swap	D0
	add.b	#'0',D0
	move.b	D0,-(sp)
	move.l	D1,D0
	addq.w	#1,D2
	tst.l	D0
	bne.s	01$
02$	move.b	(sp)+,(A3)+
	dbf	D2,02$
	move.l	(sp)+,D2
ret99	rts

*** MODIFY MEMORY ***
modifymem
	bsr	GetNum
	move.l	D0,A0
	bsr.s	getstring
	bra	mainloop

*** GET STRING FROM INPUT LINE TO ADDR IN A0, LENGTH IN D2 ***
* NOTE: this version requires commas between numbers and strings
; a1 is end addr of string on return
getstring
	move.l	A0,A1
gstr1	bsr	skipspaces
	move.b	(A3)+,D0
	beq.s	gstr9		;branch if end of input line
	cmp.b	#'''',D0	;if single quote, then string follows
	beq.s	stringi
	subq.l	#1,A3
	bsr	GetNum		;else get number
	move.b	D0,(A1)+
gstr1a	bsr	SkipComma
	bmi.s	gstr9		;if comma not found, then end
	bra.s	gstr1
stringi	move.b	(A3)+,D0
	beq.s	gstr9
	cmp.b	#'''',D0
	beq.s	gstr2
string1	move.b	D0,(A1)+
	bra.s	stringi
gstr2	cmp.b	#'''',(A3)+	;test for double-single-quote
	beq.s	string1
	subq.l	#1,A3
	bra.s	gstr1a
gstr9	move.l	A1,D2
	sub.l	A0,D2		;calculate length of the string in D2
	rts

*** DELETE A FILE ***
Deletefile
	addq.l	#2,A3
	bsr	skipspaces
	move.l	A3,D1
	calldos	DeleteFile
	tst.l	D0
	beq	dos_err
	bra.s	jmp_mainloop_3

*** CHANGE CURRENT DIRECTORY (CD) ***
CurrentDirectory
	addq.l	#1,A3
	bsr	skipspaces
	move.l	dosbase(a5),a6
	tst.b	(A3)
	bne.s	cd_1
	moveq	#0,D1	;if no name given set lock to zero (initial boot device root dir)
	bra.s	cd_2
cd_1	move.l	A3,D1
	moveq	#SHARED_LOCK,D2
	callsys	Lock
	tst.l	D0
	beq	dos_err
	move.l	D0,D1
cd_2	callsys	CurrentDir
	move.l	D0,D1
	callsys	UnLock	;unlock previous current directory
jmp_mainloop_3
	bra	mainloop

*** compare memory, set current directory or clear screen ***
memcomp	cmp.b	#'d',(A3)
	beq.s	CurrentDirectory
	cmp.b	#'l',(A3)
	bne.s	no_cls
	cmp.b	#'s',1(A3)
	bne.s	no_cls

*** THE CLS COMMAND ***
	emit	CLS	;Clear the Screen
	bra.s	jmp_mainloop_3
no_cls
**** COMPARE MEMORY ****
	bsr.s	get_n_per_line
	move.l	d6,D7
	bsr	GetNum
	move.l	D0,A0
	bsr	GetNum
	move.l	D0,A1
	bsr	GetNum
	move.l	D0,A2

comp1	cmp.l	A1,A0
	bhi.s	comp99
	cmpm.b	(A0)+,(A2)+
	beq.s	comp1
	movem.l	A0-A1,-(sp)
	move.l	A0,D0
	subq.l	#1,D0
	lea	comhfmt(pc),a0
	bsr	printf
	subq.l	#1,D7
	bne.s	compf1
	emit	LF
	move.l	d6,D7

compf1	bsr	CheckKeys
	bne.s	compbreak
	movem.l	(sp)+,A0-A1
	bra.s	comp1
compbreak
	addq.l	#8,sp

comp99	emit	LF
	bra	mainloop

get_n_per_line
	moveq	#8,d6
	tst.w	flags(a5)
	bpl.s	01$
	moveq	#6,d6
01$	rts

**** TRANSFER MEMORY ****
memtransfer
	bsr	GetNum
	move.l	D0,A0
	bsr	GetNum
	move.l	D0,A1
	bsr	GetNum
	move.l	D0,A2
	cmp.l	A2,A0
	bcs.s	backwards	;if destination > source, transfer backwards

trf1	cmp.l	A1,A0
	bhi.s	huntfilltrf2
	move.b	(A0)+,(A2)+
	bra.s	trf1

backwards
	add.l	A1,A2
	sub.l	A0,A2
trf2	cmp.l	A0,A1
	bcs.s	huntfilltrf2
	move.b	(A1),(A2)
	subq.l	#1,A1
	subq.l	#1,A2
	bra.s	trf2

**** FILL MEMORY ****
* This version can fill memory with a pattern
memfill	bsr	GetNum
	move.l	D0,-(sp)
	bsr	GetNum
	move.l	D0,A2
	move.l	outbufadr(A5),A0
	lea	(InpBuf-OutBuf)(A0),A0
	bsr	getstring
	move.l	(sp)+,A1
	tst.l	D2
	beq.s	huntfilltrf2
fill0	moveq	#0,D0
fill1	cmp.l	A2,A1
	bhi.s	huntfilltrf2
	move.b	0(A0,D0.L),(A1)+
	addq.l	#1,D0
	cmp.l	D2,D0
	bcs.s	fill1
	bra.s	fill0

huntfilltrf2
	bra	mainloop

**** HUNT MEMORY ****
memhunt	bsr	get_n_per_line
	move.l	d6,D7
	bsr	GetNum
	move.l	D0,-(sp)
	bsr	GetNum
	move.l	D0,A2
	move.l	outbufadr(A5),A0
	lea	(InpBuf-OutBuf)(A0),A0
	bsr	getstring
	move.l	(sp)+,A1
	tst.l	D2	;string length
	beq.s	huntfilltrf2

hunt0	move.b	(A0),D0
hunt1	cmp.l	A2,A1
	bhi.s	hunt99
	cmp.b	(A1)+,D0
	bne.s	hunt1
	moveq	#0,D1

hunt2	addq.l	#1,D1
	cmp.l	D2,D1
	bcc.s	huntfound
	move.b	-1(A1,D1.L),D0
	cmp.b	0(A0,D1.L),D0
	beq.s	hunt2
	bra.s	hunt0
huntfound
	movem.l	A0-A1,-(sp)
	move.l	A1,D0
	subq.l	#1,D0
	lea	comhfmt(pc),a0
	bsr	printf
	subq.l	#1,D7
	bne.s	huntf1
	emit	LF
	move.l	d6,d7
huntf1	bsr.s	CheckKeys
	bne.s	huntbreak
	movem.l	(sp)+,A0-A1
	bra.s	hunt0
huntbreak
	addq.l	#8,sp
hunt99	emit	LF
	bra	mainloop

**** WAIT IF SPACE PRESSED, BREAK IF CTRL-C (status non-zero) ****
CheckKeys
	move.l	winfile(A5),D1
	movem.l	D2/a6,-(sp)
;#
;# is timeout zero really safe?...I have found no problems yet...
;#
	moveq	#0,D2		;timeout=0
	calldos	WaitForChar
	movem.l	(sp)+,D2/a6
	tst.l	D0
	beq.s	nobreak		;branch if no key pressed
	bsr	GetKey
	cmp.w	#SPACE,D0
	bne.s	nospc
waitspc		;space pressed, wait for another space or Ctrl-C
	bsr	GetKey
	cmp.w	#CtrlC,D0
	beq.s	break
	cmp.w	#SPACE,D0
	bne.s	waitspc
	bra.s	nobreak
nospc	cmp.w	#CtrlC,D0
	bne.s	nobreak
break	lea	breaktx(pc),A0	;message '*** break ***'
	bsr	printstring_a0_window
	moveq	#-1,D0
	rts
nobreak	moveq	#0,D0
	rts

**** PARAMETERS FOR DISPLAY AND DISASSEMBLE ****
* get Addr and EndAddr & number of lines to display in D7
* if D7 is zero then display from Addr to EndAddr
* if D7 is non-zero EndAddr is ignored
getparams
	bsr	skipspaces
	tst.b	(A3)
	beq.s	param9
	bsr	GetNum
	bclr	#0,D0
	move.l	D0,Addr(A5)
	bsr	skipspaces
	tst.b	(A3)
	beq.s	param9
	bsr	GetNum
	move.l	D0,EndAddr(A5)
	moveq	#0,D7
	rts
param9	moveq	#20,D7	;number of lines to output, if zero then use EndAddr
	rts

**** CHECK IF WE SHOULD STOP MEMDISPLAY OR DISASSEMBLE ****
* if D7 is non-zero decrement it, if it becomes zero then stop
* else if Addr>EndAddr then stop
* return Z-flag set if we should stop
CheckEnd	;returns Z=1 if stop
	bsr	CheckKeys
	bne.s	stop1
	tst.l	D7
	beq.s	cmpadrs
	subq.l	#1,D7
	rts
cmpadrs	cmp.l	EndAddr(A5),A4
	bls.s	cont
stop1	moveq	#0,D0
	rts
cont	moveq	#-1,D0
	rts

disassem
*** Disassemble, delete file or directory **
	cmp.b	#'e',(A3)
	bne.s	nodel
	cmp.b	#'l',1(A3)
	beq	Deletefile
nodel	cmp.b	#'i',(A3)
	bne.s	nodir
	cmp.b	#'r',1(A3)
	beq	dir
nodir

**** DISASSEMBLE ****
	bsr.s	getparams
	move.l	Addr(A5),A4
disasmloop
	bsr.s	disasmline
	bsr	printstring
	bsr	CheckEnd
	bne.s	disasmloop
	move.l	A4,Addr(A5)
	bra	mainloop

*** DISASSEMBLE ONE INSTRUCTION ***
* address in A4
disasmline
	startline
	move.l	A4,D0
	bsr	puthex1_68		print address
	putchr	SPACE
	move.l	A3,A2
	move.l	a2,a3
	add.w	#27,A3
	move.l	a3,d6
	bsr	DGetWord
	move.w	d2,opcode(a5)
	move.l	A4,instrad(A5)
	move.w	D2,D0
	moveq	#12,D1
	lsr.w	D1,D0	;jump according to the first four bits (first hex digit)
	add.w	D0,D0
	lea	disadrtabl(pc),A1
	add.w	D0,A1
	add.w	(A1),A1
	jmp	(A1)

*** $4xxx ***
handlefour
	cmp.w	#ILLEGAL,D2	;illegal instruction $4AFC
	bne.s	no_illegal
	moveq	#n_illegal,D0
	bsr	put_instr_name
	bra	dis9
no_illegal
	btst	#8,D2
	beq.s	no_lea_or_chk
	btst	#7,D2
	beq.s	derr_4
	btst	#6,D2
	bne.s	lea_instr
	and.w	#$38,D2
	cmp.w	#$08,D2		;An addr mode illegal
	beq.s	derr_4
	moveq	#n_chk,D0		;chk instruction
	bsr	put_instr_name
	move.w	#WSIZE,size(a5)
	bsr	padd
	bsr	NormEA
	putchr	COMMA
	bsr	putDreg9
	bra.s	chk_lea_zap9
derr_4	bra	diserr
lea_instr
	bsr	check_jump_adrmode
	bne.s	derr_4
	moveq	#n_lea,D0
	bsr	put_instr_name
;%%	move.w	#LSIZE,size(a5) actually no size
	bsr	padd
	bsr	NormEA
	putchr	COMMA
	bsr	putAreg9
chk_lea_zap9
	bra	dis9

no_lea_or_chk
	move.w	D2,D0
	and.w	#$0E00,D0
	bne.s	no_negx_or_move_sr

negate	bsr	check_dest_adrmode
	bne.s	derr_4
	move.w	D2,D0
	and.w	#$C0,D0
	cmp.w	#$C0,D0
	beq.s	move_from_sr
	moveq	#n_neg,D0
	bsr	put_instr_name
	btst	#10,D2
	bne.s	noextneg
	putchr	<'x'>
noextneg
	bsr	putsize
	bsr	padd
	bsr	NormEA
	bra.s	mngxzap9
move_from_sr	;MOVE	sr,Ea
	moveq	#n_move,D0
	bsr	put_instr_name
	move.w	#WSIZE,size(a5)
	bsr	padd
	move.l	#'sr, ',d0
	bsr	PutLong
	subq.l	#1,a3
	bsr	NormEA
mngxzap9
	bra	dis9
no_negx_or_move_sr
	cmp.w	#$0200,D0
	bne.s	noclr
	move.w	D2,D0	;clr instruction
	and.w	#$C0,D0
	cmp.w	#$C0,D0
	beq.s	derr_zap
	bsr	check_dest_adrmode
	bne.s	derr_zap
	moveq	#n_clr,D0
	bsr	put_instr_name
	bsr	putsize
	bsr	padd
	bsr	NormEA
	bra.s	mngxzap9
derr_zap
	bra	diserr

noclr	cmp.w	#$0400,D0
	bne.s	no_neg_or_move_ccr
	move.w	D2,D0
	and.w	#$C0,D0
	cmp.w	#$C0,D0
	bne	negate
	and.w	#$38,D2
	cmp.w	#$08,D2	;An addr mode is illegal
	beq.s	derr_zap
	moveq	#n_move,D0	;move to ccr
	bsr	put_instr_name
	clr.w	size(a5)	byte size
	bsr	padd
	bsr	NormEA
	move.l	#',ccr',D0
	bsr	PutLong
	bra.s	nzapp99

no_neg_or_move_ccr
	cmp.w	#$0600,D0
	bne.s	no_not_or_move_sr
	move.w	D2,D0
	and.w	#$C0,D0
	cmp.w	#$C0,D0
	beq.s	move_to_sr
	bsr	check_dest_adrmode	;not-insturuction
	bne.s	derr_zap
	moveq	#n_not,d0
	bsr	put_instr_name
	bsr	putsize
	bsr	padd
	bsr	NormEA
nzapp99
	bra	dis9
move_to_sr
	and.w	#$38,D2
	cmp.w	#$08,D2		;An addr mode illegal
	beq.s	derr_zump
	moveq	#n_move,D0		;move to SR
	bsr	put_instr_name
	move.w	#WSIZE,size(a5)
	bsr	padd
	bsr	NormEA
	move.l	#',sr ',D0
	bsr	PutLong
	subq.l	#1,A3
	bra.s	nzapp99
derr_zump
	bra	diserr

no_not_or_move_sr
	cmp.w	#$0A00,D0
	bne.s	no_tst_or_tas
	bsr	check_dest_adrmode
	bne.s	derr_zump
	move.w	D2,D0
	and.w	#$C0,D0
	cmp.w	#$C0,D0
	beq.s	tas_instr
	moveq	#n_tst,d0	;tst-instruction
	bsr	put_instr_name
	bsr	putsize
	bsr	padd
	bsr	NormEA
	bra.s	ttzap9
tas_instr
	moveq	#n_tas,D0
	bsr	put_instr_name
	clr.w	size(a5)	byte size
	bsr	padd
	bsr	NormEA
ttzap9	bra	dis9

no_tst_or_tas
	cmp.w	#$0800,D0
	bne	no_48
	btst	#7,D2
	bne.s	movem_or_ext
	btst	#6,D2
	beq.s	negate_bcd
	move.w	D2,D0
	and.w	#$38,D0
	beq.s	swap_instr
	bsr	check_jump_adrmode	;pea-instruction
	bne.s	derr_zip
	moveq	#n_pea,D0
	bsr	put_instr_name
	bsr	padd
;%% no size
	bsr	NormEA
	bra.s	swapzap9
swap_instr
	moveq	#n_swap,d0
	bsr	put_instr_name
	bsr	padd
	bsr	putDreg0
swapzap9
	bra	dis9
derr_zip
	bra	diserr
negate_bcd
	bsr	check_dest_adrmode
	bne.s	derr_zip
	moveq	#n_nbcd,D0
	bsr	put_instr_name
;%% byte size, immediate mode not valid
	bsr	padd
	bsr	NormEA
	bra.s	swapzap9
movem_or_ext
	move.w	D2,D0
	and.w	#$38,D0
	bne.s	movem_to_mem
	moveq	#n_ext,D0	;ext
	bsr	put_instr_name
	bsr	put_wl_size
	bsr	padd
	bsr	putDreg0
	bra	dis9
movem_to_mem
	bsr	check_dest_adrmode
	bne.s	derr_zip
	and.w	#$38,D2
	cmp.w	#$18,D2
	beq.s	derr_zip
	moveq	#n_move,D0
	bsr	put_instr_name
	putchr	<'m'>
	bsr	put_wl_size
	bsr	padd
	bsr	DGetWord
	bsr	RegisterList
	putchr	COMMA
	bsr	NormEA
	bra.s	dazap9

no_48	cmp.w	#$0C00,D0
	bne.s	special_4e
	btst	#7,d2
	beq.s	derr_4e
	and.w	#$3F,D2	;lots of testing for illegal addressing modes
	cmp.w	#$3B,D2
	bhi.s	derr_4e
	and.w	#$38,D2
	cmp.w	#$20,D2
	beq.s	derr_4e
	cmp.w	#$10,D2
	bcs.s	derr_4e
	moveq	#n_move,D0	;movem from mem
	bsr	put_instr_name
	putchr	<'m'>
	bsr	put_wl_size
	bsr	padd
	bsr	DGetWord
	move.w	D2,D5
	bsr	NormEA
	putchr	COMMA
	move.w	D5,D2
	bsr	RegisterList
dazap9	bra	dis9
derr_4e	bra	diserr

*** $4Exx ***
special_4e
	cmp.w	#$4E40,D2
	bcs.s	derr_4e
	cmp.w	#$4e50,D2
	bcc.s	notrapinst
	moveq	#n_trap,D0	;TRAP #n
	bsr	put_instr_name
	bsr	padd
	putchr	<'#'>
	move.w	D2,D0
	and.b	#$0F,D0
	moveq	#2,d1
	bsr	put_hexnum
	bra.s	dazap9
notrapinst
	cmp.w	#$4E58,D2
	bcc.s	nolink
	moveq	#n_link,D0
	bsr	put_instr_name	;LINK An,#cc
	bsr	padd
	bsr	putAreg0
	putchr	COMMA
	putchr	<'#'>
	bsr	DGetWord
	move.w	D2,D0
	ext.l	d0
	bsr	put_signed_hexnum	%%signed offset
	bra.s	dazap9
nolink
	cmp.w	#$4E60,D2
	bcc.s	nounlk
	moveq	#n_unlk,D0	;UNLK An
	bsr	put_instr_name
	bsr	padd
	bsr	putAreg0
	bra.s	dazap9
nounlk	cmp.w	#$4E68,D2
	bcc.s	no_move_to_usp
	moveq	#n_move,D0	
	bsr	put_instr_name	;MOVE An,usp
	bsr	padd
	bsr	putAreg0
	putchr	COMMA
	lea	USPnam(pc),A1
	bsr	putstring
uspzap9	bra	dis9
no_move_to_usp
	cmp.w	#$4E70,D2
	bcc.s	no_move_from_usp
	moveq	#n_move,D0
	bsr	put_instr_name	;MOVE usp,An
	bsr	padd
	lea	USPnam(pc),A1
	bsr	putstring
	putchr	COMMA
	bsr	putAreg0
	bra.s	uspzap9
no_move_from_usp
	cmp.w	#$4E78,D2
	bcc.s	jumps
	cmp.w	#$4E74,D2
	beq.s	derr_zero	;$4E74 is illegal
	move.w	D2,D0
	and.w	#7,D0
	add.w	#n_reset,D0
	bsr	put_instr_name	;RTS, NOP, RTE, STOP, RESET, RTR...
	cmp.w	#$4E72,D2	;STOP ?
	bne.s	uspzap9
	bsr	padd
	bsr	DGetWord	;if it is, print '#num'
	move.w	D2,D0
	putchr	<'#'>
	moveq	#4,d1
	bsr	put_hexnum
	bra.s	jumpzap9

jumps	bsr	check_jump_adrmode
	bne.s	derr_zero
	move.w	D2,D0
	lsr.w	#6,D0
	and.w	#3,d0
	subq.w	#2,d0
	bcs.s	derr_zero
	add.w	#n_jsr,D0
	bsr	put_instr_name	; JMP & JSR
;%% no size
	bsr	padd
	bsr	NormEA
jumpzap9
	bra	dis9
derr_zero
	bra	diserr

*** $0xxx ***
handlezero
	tst.w	d2
	bne.s	nozero
;#
;# handle zero "padding words"
;# these would normally be listed as ori.b #data,d0 -instructions
;# and cause the disassembler to 'get out of sync'
;#
	move.w	(a4),d0
	and.w	#$ff00,d0
	beq.s	nozero
	lea	zerotxt(pc),a1
	bsr	putstring
	bra.s	jumpzap9
nozero	btst	#8,D2
	bne	movep_etc
	move.w	D2,D0
	and.w	#$0E00,D0
	beq.s	or_imm
	cmp.w	#$0200,D0
	beq.s	and_imm
	cmp.w	#$0400,D0
	beq.s	sub_imm
	cmp.w	#$0600,D0
	beq.s	add_imm
	cmp.w	#$0800,D0
	beq	bits_imm
	cmp.w	#$0A00,D0
	beq.s	eor_imm
	cmp.w	#$0C00,D0
	bne.s	derr_zero
* cmpi *
	moveq	#n_cmp,D0
	bra.s	imm_com
add_imm	moveq	#n_add,D0
	bra.s	imm_com
sub_imm	moveq	#n_sub,D0
	bra.s	imm_com
or_imm	moveq	#n_or,D0
	bra.s	logic_imm
and_imm	moveq	#n_and,D0
	bra.s	logic_imm
eor_imm	moveq	#n_eor,D0
logic_imm
	move.w	D2,D1
	and.w	#$3F,D1
	cmp.w	#$3C,D1
	bne.s	imm_com
	bsr	put_instr_name	;mode=SR or CCR
	putchr	<'i'>
	bsr	padd
	move.w	opcode(a5),d0
	and.w	#$c0,d0
	beq.s	immccr
	cmp.w	#$40,d0
	bne.s	derr_imm
	move.w	#WSIZE,size(a5)
	bsr	Immediate
	move.l	#',sr ',d0
	bsr	PutLong
	subq.l	#1,a3
sxcd9	bra	dis9

immccr	clr.w	size(a5)
	bsr	Immediate
	move.l	#',ccr',d0
	bsr	PutLong
	bra.s	sxcd9

imm_com	bsr	put_instr_name
	move.w	D2,D0
	and.w	#$C0,D0
	cmp.w	#$C0,D0
	beq.s	derr_imm
	move.w	D2,D0
	and.w	#$38,D0
	cmp.w	#$08,D0
	beq.s	derr_imm
	bsr	check_dest_adrmode
	bne.s	derr_imm
	putchr	<'i'>
	bsr	putsize
	bsr	padd
	bsr	Immediate
	putchr	COMMA
	bsr	NormEA
	bra	dis9
derr_imm
	bra	diserr

*** Bxxx #num,EA ***
bits_imm
	move.w	D2,D0
	and.w	#$3F,D0
	cmp.w	#$3B,D0
	bhi.s	derr_imm
	and.w	#$38,D0
	cmp.w	#$08,D0
	beq.s	derr_imm
	move.w	D2,D0
	lsr.w	#6,D0
	and.w	#3,D0
	beq.s	bits_imm_ok2
	and.w	#$3F,D2
	cmp.w	#$39,D2
	bhi.s	derr_imm
bits_imm_ok2
	add.w	#n_btst,D0
	bsr	put_instr_name
	bsr	padd
	bsr	DGetWord
	putchr	<'#'>
	moveq	#0,d0
	move.w	D2,D0
	bsr	put_signed_hexnum
	putchr	COMMA
; size byte or long, immediate mode not valid (no btst #imm,#imm)
	bsr	NormEA
	bra	dis9

*** MOVEP, bit manipulations ***
movep_etc
	move.w	D2,D0
	and.w	#$38,D0
	cmp.w	#$08,D0
	beq.s	move_p
;#
;# btst Dn,#data now displayed correctly
;#
*** Bxxx Dn,EA ***
	cmp.w	#$083c,d2
	beq.s	derr_imm
	move.w	D2,D0
	and.w	#$3F,D0
	cmp.w	#$3c,D0
	bhi.s	derr_imm
	and.w	#$38,D0
	cmp.w	#$08,D0
	beq.s	derr_imm	data register direct not allowed
	move.w	D2,D0
	lsr.w	#6,D0
	and.w	#3,D0
	beq.s	bits_dreg_ok2
	and.w	#$3F,D2
	cmp.w	#$39,D2
	bhi	diserr
bits_dreg_ok2
	add.w	#n_btst,D0
	bsr	put_instr_name
	clr.w	size(a5)	byte size (for btst Dn,#imm)
	bsr	padd
	bsr	putDreg9
	putchr	COMMA
	bsr	NormEA
	bra	dis9

*** MOVEP ***
move_p	moveq	#n_move,D0
	bsr	put_instr_name
	putchr	<'p'>
	bsr	put_wl_size
	bsr	padd
	btst	#7,D2
	beq.s	noregsource
	bsr	putDreg9
	putchr	COMMA
noregsource
	bsr	DGetWord
	move.w	D2,D0
	ext.l	d0
	bsr	put_signed_hexnum	%%signed offset
	putchr	<'('>
	move.w	opcode(A5),D2
	bsr	putAreg0
	putchr	<')'>
	btst	#7,D2
	bne.s	noregdest
	putchr	COMMA
	bsr	putDreg9
noregdest
	bra	dis9

*** CMP & CMPM & EOR ***
handle_11
	btst	#8,D2
	beq.s	compare
	move.w	D2,D0
	and.w	#$C0,D0
	cmp.w	#$C0,D0
	beq.s	compare
	and.w	#$38,D2
	cmp.w	#$08,D2
	beq.s	compmem

*** EOR ***
	move.w	opcode(a5),d2
	and.w	#$3F,D2
	cmp.w	#$39,D2
	bhi	diserr
	moveq	#n_eor,D0
	bsr	put_instr_name
	bsr	putsize
	bsr	padd
	bsr	putDreg9
	putchr	COMMA
	bsr	NormEA
	bra	dis9

*** CMPM ***
compmem	moveq	#n_cmp,D0
	bsr	put_instr_name
	putchr	<'m'>
	bsr	putsize
	bsr	padd
	putchr	<'('>
	bsr	putAreg0
	move.l	#')+,(',D0
	bsr	PutLong
	bsr	putAreg9
	putchr	<')'>
	putchr	<'+'>
	bra.s	cmpzap9

*** CMP ***
compare	moveq	#n_cmp,D0
	bsr	put_instr_name
	move.w	D2,D0
	and.w	#$C0,D0
	cmp.w	#$C0,D0
	bne.s	cmp1
	putchr	<'a'>	;cmpa
cmp1	bsr	putsize
	cmp.b	#'b',-1(A3)
	bne.s	cmp2
	and.w	#$38,D2
	cmp.w	#$08,D2
	beq	diserr	;byte size not allowed if addr reg
cmp2	bsr	padd
	bsr	NormEA
	putchr	COMMA
	move.w	opcode(A5),D0
	and.w	#$C0,D0
	cmp.w	#$C0,D0
	bne.s	datacmp
	bsr	putAreg9
	bra.s	cmpzap9
datacmp	bsr	putDreg9
cmpzap9	bra	dis9

*** AND	& MUL & ABCD & EXG ***
handle_12
	move.w	D2,D0
	and.w	#$0130,D0
	cmp.w	#$0100,D0
	bne.s	noexg
	move.w	D2,D0
	and.w	#$C0,D0
	beq.s	noexg
	cmp.w	#$C0,D0
	beq.s	noexg
	move.w	D2,D0
	and.w	#$F8,D0
	cmp.w	#$80,D0
	beq.s	noexg
	and.w	#$30,D0
	bne.s	noexg

*** EXG ***
;#
;# exg Dn,An is now fixed. there was an error in my documentation
;# that reversed the address and data register numbers...
;# thanks to John van Dijk for informing me about that bug.
;#
	moveq	#n_exg,D0
	bsr	put_instr_name
	bsr	padd
	move.w	D2,D1
	and.w	#$F8,D1
	cmp.w	#$48,D1
	beq.s	both_adr
	cmp.w	#$40,d1
	beq.s	both_data
	bsr	putDreg9
	putchr	COMMA
	bsr	putAreg0
	bra.s	dzujump9
both_adr
	bsr	putAreg0
	putchr	COMMA
	bsr	putAreg9
	bra.s	dzujump9
both_data
	bsr	putDreg0
	putchr	COMMA
	bsr	putDreg9
dzujump9
	bra	dis9

noexg	move.w	D2,D0
	and.w	#$C0,D0
	cmp.w	#$C0,D0
	beq.s	mul1
	moveq	#n_and,D0
	bra.s	and_or
mul1	moveq	#n_mul,D0
	bra.s	divmul

*** OR & DIV & SBCD ***
handle_eight	
	move.w	D2,D0
	and.w	#$C0,D0
	cmp.w	#$C0,D0	
	bne.s	do_or
	moveq	#n_div,D0	;disivion
divmul	move.w	d2,d1
	and.w	#$38,d1
	cmp.w	#$08,d1		address register is not a valid source
	beq	derr_abc	for division or multiplication
	bsr	put_instr_name
	btst	#8,D2
	beq.s	unsigned_divm
	moveq	#'s',D0
	bra.s	divmul1
unsigned_divm
	moveq	#'u',D0
divmul1	move.b	D0,(A3)+
	bsr	padd
	move.w	#WSIZE,size(a5)
	bsr	NormEA
	putchr	COMMA
	bsr	putDreg9
	bra	dis9
do_or	moveq	#n_or,D0

*** AND & OR **
and_or	;instruction number in D0
	move.w	D2,D1
	and.w	#$01F0,D1
	cmp.w	#$0100,D1
	beq.s	bcd_oper
	move.w	D2,D1
	and.w	#$0138,D1
	cmp.w	#$0100,D1
	beq.s	derr_abc
	bsr	put_instr_name
	bsr	putsize
	bsr	padd
	btst	#8,D2
	bne.s	regsource1
	move.w	D2,D0	;AND/OR EA,Dn
	and.w	#$38,D0
	cmp.w	#$08,D0
	beq.s	derr_abc
	bsr	NormEA
	putchr	COMMA
	bsr	putDreg9
	bra.s	ddzap9
regsource1	;AND/OR Dn,EA
	bsr	check_dest_adrmode
	bne.s	derr_abc
	bsr	putDreg9
	putchr	COMMA
	bsr	NormEA
ddzap9	bra	dis9
derr_abc
	bra	diserr

*** ABCD & SBCD ***
bcd_oper
	addq.w	#2,D0
	bsr	put_instr_name
ext_addsub
	bsr	padd
	btst	#3,D2
	bne.s	mem_to_mem
	bsr	putDreg0
	putchr	COMMA
	bsr	putDreg9
	bra.s	dzap9
mem_to_mem
	putchr	<'-'>
	putchr	<'('>
	bsr	putAreg0
	move.l	#'),-(',D0
	bsr	PutLong
	bsr	putAreg9
	putchr	<')'>
dzap9	bra	dis9

*** MOVE ***
movebyte
	move.w	D2,D0
	and.w	#$01C0,D0
	cmp.w	#$40,D0
	beq.s	derr_move
	move.w	D2,D0
	and.w	#$38,D0
	cmp.w	#$08,D0	;An allowed if byte size
	beq.s	derr_move
movelong
moveword
	move.w	D2,D0
	and.w	#$01C0,D0
	cmp.w	#$01C0,D0
	bne.s	move_ok1
	move.w	D2,D0
	and.w	#$0E00,D0
	cmp.w	#$0400,D0
	bcc.s	derr_move
move_ok1
	moveq	#n_move,D0
	bsr	put_instr_name
	and.w	#$01C0,D2
	cmp.w	#$40,D2
	bne.s	move1
	putchr	<'a'>	;move address
move1	bsr	putsize
	bsr	padd
	bsr	NormEA
	putchr	COMMA
	move.w	opcode(A5),D1
	move.w	D1,D0
	lsr.w	#8,D0
	lsr.w	#1,D0
	lsr.w	#6,D1
	and.b	#7,D0
	and.b	#7,D1
	bsr	HandleEA
	bra.s	d9jzump
derr_move
	bra	diserr

*** MOVEQ ***
movequick
	btst	#8,D2
	bne.s	derr_move
	moveq	#n_move,D0
	bsr	put_instr_name
	putchr	<'q'>
	bsr	padd
	putchr	<'#'>
	move.b	D2,D0
	ext.w	d0
	ext.l	d0
	bsr	put_signed_hexnum	%%signed long value
	putchr	COMMA
	bsr	putDreg9
d9jzump	bra	dis9

*** BRANCH INSTRUCTIONS ***
branch	tst.b	D2
	beq.s	disp16bit
	ext.w	D2
	bra.s	bracom
disp16bit
	bsr	DGetWord
bracom	putchr	<'b'>
	bsr	putcond
	move.w	opcode(A5),D0
	tst.b	D0
	beq.s	branch1
	putchr	<'.'>
	putchr	<'s'>
branch1	bsr	padd
	move.l	D2,D0
	ext.l	D0
	add.l	instrad(A5),D0
	bsr	puthex_68		%% branch address
	bra	dis9

**** ADD & SUB ****
subtract
	moveq	#n_sub,D0
	bra.s	addsubcom

addinst	moveq	#n_add,D0

addsubcom
	bsr	put_instr_name
	move.w	D2,D0
	and.w	#$C0,D0
	cmp.w	#$C0,D0
	beq.s	addsubadr
	bsr	putsize
	btst	#8,D2
	bne.s	regsource
	move.w	D2,D0
	and.w	#$38,D0
	cmp.w	#$08,D0
	bne.s	add_sub_1	
	move.w	D2,D0
	and.w	#$C0,D0
	beq	diserr
add_sub_1
	bsr	padd
	bsr	NormEA
	putchr	COMMA
	bsr	putDreg9
	bra.s	d9jmp
ext_addsub1
	move.b	-1(A3),(A3)
	move.b	-2(A3),-1(A3)
	move.b	#'x',-2(A3)
	addq.l	#1,A3
	bsr	padd
	bra	ext_addsub
regsource
	move.w	D2,D1
	and.w	#$38,D1
	beq.s	ext_addsub1
	cmp.w	#$08,D1
	beq.s	ext_addsub1
	bsr	padd
	bsr	check_dest_adrmode
	bne	diserr
	bsr	putDreg9
	putchr	COMMA
	bsr	NormEA
	bra.s	d9jmp

addsubadr	;*** ADDA & SUBA ***
	putchr	<'a'>
	bsr	putsize
	bsr	padd
	bsr	NormEA
	putchr	COMMA
	bsr	putAreg9
d9jmp	bra	dis9

**** $5XXX ****
handle_five
	move.w	D2,D0
	and.w	#$3F,D0
	cmp.w	#$39,D0
	bhi	diserr
	move.w	D2,D0
	and.w	#$C0,D0
	cmp.w	#$C0,D0
	bne.s	addsubquick
	move.w	D2,D0
	and.w	#$38,D0
	cmp.w	#$08,D0
	bne.s	SccInst

*** DBcc (decrement, test condition and branch) instruction ***
	bsr	DGetWord
	move.w	D2,-(sp)
	move.w	opcode(A5),D2
	putchr	<'d'>
	putchr	<'b'>
	bsr	putcond
	bsr	padd
	bsr	putDreg0
	putchr	COMMA
	move.w	(sp)+,D0
	ext.l	D0
	add.l	instrad(A5),D0
	bsr	puthex_68		%% branch address
	bra	dis9

*** Scc (Set according to condition) instruction ***
SccInst	putchr	<'s'>
	bsr	putcond
;%%	clr.w	size(a5)	byte size
	bsr	padd
	bsr	NormEA
	bra.s	d9jzwumps

*** ADDQ, SUBQ ***
addsubquick
	btst	#8,D2
	beq.s	addquick
	moveq	#n_sub,D0
	bra.s	adsubq2
addquick
	moveq	#n_add,D0

adsubq2	bsr	put_instr_name
	putchr	<'q'>
	bsr	putsize
	cmp.b	#'b',-1(A3)
	bne.s	adsubqz1
	move.w	opcode(A5),D0
	and.w	#$38,D0
	cmp.w	#$08,D0
	beq.s	derr_sf
adsubqz1
	bsr	padd
	bsr	putqnum
	putchr	COMMA
	bsr	NormEA
	bra.s	d9jzwumps

*** SHIFT INSTRUCTIONS ***
derr_sf	bra.s	diserr

shifts	move.w	D2,D0
	and.w	#$C0,D0
	cmp.w	#$C0,D0
	beq.s	shiftmemory

*** shift register ***
	move.w	D2,D0
	lsr.w	#3,D0
	bsr	shiftinstruction
	bsr	putsize
	bsr.s	padd
	btst	#5,D2
	bne.s	cntreg
	bsr	putqnum
	bra.s	putdestreg
cntreg	bsr	putDreg9
putdestreg
	putchr	COMMA
	bsr	putDreg0
d9jzwumps
	bra.s	dis9
shiftmemory
	moveq	#11,d0
	btst	d0,d2
	bne.s	derr_sf
	move.w	D2,D0
	and.w	#$3F,D0
	cmp.w	#$10,D0
	bcs.s	derr_sf
	cmp.w	#$39,D0
	bhi.s	derr_sf
	move.w	D2,D0
	lsr.w	#8,D0
	lsr.w	#1,D0
	bsr.s	shiftinstruction
;%%	move.w	#WSIZE,size(a5)
	bsr.s	padd
	bsr	NormEA
	bra.s	dis9

*** LINE-A & LINE-F unimplemented codes ***
lineA	moveq	#'A',D3
	bra.s	lines
lineF	moveq	#'F',D3
lines	lea	linenam(pc),A1
	bsr.s	putstring
	move.b	D3,(A3)+
	bra.s	dis9a

*** UNKNOWN OPCODE ***
diserr	move.l	d6,a3
	lea	errtx(pc),A1
	bsr.s	putstring
dis9a	moveq	#-1,d0
	bra.s	dis9b
dis9	moveq	#0,d0
dis9b	move.l	d6,a1
10$	cmp.l	A1,A2
	bcc.s	11$
	move.b	#SPACE,(A2)+
	bra.s	10$
11$	endline
	rts		return code in d0: zero if valid instruction

padd	move.l	outbufadr(a5),A1
	add.w	#44,A1
01$	putchr	SPACE
	cmp.l	A1,A3
	bcs.s	01$
	rts

*** DISPLAY INSTRUCTION NAME ***
put_instr_name	;code in D0
	add.w	D0,D0
	lea	instradrs(pc),A1
	add.w	D0,A1
	add.w	(A1),A1

*** STRING OUTPUT ***
;string pointer in A1
putstring	move.b	(a1)+,(a3)+
	bne.s	putstring
	subq.l	#1,a3
	rts

shiftinstruction
	and.w	#3,D0
	bsr.s	put_instr_name
	btst	#8,D2
	bne.s	01$
	moveq	#'r',d0
	bra.s	02$
01$	moveq	#'l',d0
02$	move.b	d0,(a3)+
	rts

putqnum	putchr	<'#'>
	move.w	opcode(A5),D0
	lsr.w	#8,D0
	lsr.w	#1,D0
	and.b	#7,D0
	bne.s	putn1
	moveq	#8,D0
	bra.s	putn1

putAreg0
	move.w	opcode(a5),d0
	bra.s	putacom

putAreg9
	move.w	opcode(a5),d0
	lsr.w	#8,d0
	lsr.w	#1,d0

putacom	and.b	#7,d0
	cmp.b	#7,d0
	beq.s	stackptr
	putchr	<'a'>
	bra.s	putn1

putDreg0
	move.w	opcode(A5),D0
	bra.s	putdcom

putDreg9
	move.w	opcode(A5),D0
	lsr.w	#8,D0
	lsr.w	#1,D0

putdcom	putchr	<'d'>
	and.b	#7,D0
putn1	add.b	#'0',D0
	move.b	D0,(A3)+
	rts

stackptr
	putchr	<'s'>
	putchr	<'p'>
	rts

put_wl_size	;size:word or long
	move.w	opcode(A5),D0
	btst	#6,D0
	beq.s	wl_word
	moveq	#'l',D0
	moveq	#2,d1
	bra.s	wl_com
wl_word	moveq	#'w',D0
	moveq	#1,d1
wl_com	putchr	<'.'>
	move.b	D0,(A3)+
	move.w	d1,size(a5)
	rts

check_dest_adrmode ;returns false (not equal) if illegal mode for dest.
;allowed modes Dn, (An), (An)+, -(An), disp(An), disp(An,Rx), absolute
;note: An not allowed
	move.w	opcode(A5),D0
	and.w	#$3F,D0
	cmp.w	#$39,D0
	bhi.s	illegal_1
	and.w	#$38,D0
	cmp.w	#$08,D0
	beq.s	illegal_1
	moveq	#0,D0
	rts
illegal_1
	moveq	#-1,D0
	rts

check_jump_adrmode ;(An),disp(An),disp(An,Rx),absolute,PC rel,PC rel(Rx)
	move.w	opcode(A5),D0
	and.w	#$3F,D0
	cmp.w	#$3B,D0
	bhi.s	illegal_1
	and.w	#$38,D0
	cmp.w	#$10,D0
	beq.s	legal_1
	cmp.w	#$28,D0
	bcs.s	illegal_1
legal_1	moveq	#0,D0
	rts

*** DISPLAY REGISTER LIST (for MOVEM) ***
; this changes d2-d5
RegisterList ;mask word in D2
	move.w	opcode(A5),D0
	btst	#10,D0
	bne.s	normlist
	and.w	#$38,D0
	cmp.w	#$20,D0
	bne.s	normlist
	move.w	D2,D0
	moveq	#15,D1
bitloop	;reverse bit order
	lsr.w	#1,D0
	roxl.w	#1,D2
	dbf	D1,bitloop
normlist
	moveq	#0,D4	;flag:registers found
	moveq	#'d',D5	;register type
	bsr.s	regset
	moveq	#'a',D5

regset	moveq	#0,D3	;flag:in range
	moveq	#0,D1	;bit counter
regloop1
	lsr.w	#1,D2
	bcc.s	nlist1
	tst.l	D3
	bne.s	inrange
	cmp.w	#7,D1
	beq.s	nostartrange
	btst	#0,D2
	beq.s	nostartrange
;# removed this test 1989-08-27
;#	tst.l	D3
;#	bne.s	nostartrange	;already in range
	tst.l	D4
	beq.s	strng1
	putchr	<'/'>
strng1	bsr.s	putD1reg
	putchr	<'-'>
	moveq	#-1,D3	;now in range
	bra.s	nlist1
inrange	cmp.w	#7,D1
	beq.s	endrange
	btst	#0,D2
	bne.s	nlist1
endrange
	bsr.s	putD1reg
	moveq	#0,D3	;not in range
	moveq	#-1,D4	;regs found
	bra.s	nlist1
nostartrange
	tst.l	D4
	beq.s	no_found
	putchr	<'/'>
no_found
	bsr.s	putD1reg
	moveq	#-1,D4
nlist1	addq.w	#1,D1
	cmp.w	#8,D1
	bcs.s	regloop1
	rts
putD1reg
	move.b	D5,(A3)+
	move.b	D1,D0
	and.b	#7,D0
	add.b	#'0',D0
	move.b	D0,(A3)+
	rts

*** put condition code ***
* handles correctly bra, bsr & dbf, dbt & scf, sct
putcond	move.w	opcode(A5),D0
	move.w	D0,D1
	lsr	#7,D0
	and.w	#$1E,D0
	and.w	#$F000,D1
	cmp.w	#$6000,D1
	beq.s	putcnd1
	cmp.b	#2,D0
	bhi.s	putcnd1
	beq.s	condf
	moveq	#'t',d0
	bra.s	condtf
condf	moveq	#'f',d0
condtf	move.b	d0,(a3)+
	rts
putcnd1	lea	condcodes(pc),A1
	add.w	D0,A1
	move.b	(A1)+,(A3)+
	move.b	(A1)+,(A3)+
retu	rts

*** OUTPUT NORMAL EFFECTIVE ADDRESS (mode=bits 5-3, reg=bits 2-0)
NormEA:	move.w	opcode(A5),D0
	move.w	D0,D1
	and.w	#7,D0
	lsr.w	#3,D1
	and.w	#7,D1
HandleEA	;D1 = mode, D0 = register
	tst.b	D1
	bne.s	no_zero
	bra	putdcom		data register direct

no_zero	cmp.b	#1,D1
	bne.s	no_one
	bra	putacom		address register direct

no_one	cmp.b	#3,D1
	bhi.s	more_than_three

twocase	putchr	<'('>	;(An) & (An)+
	bsr	putacom
	putchr	<')'>
	cmp.b	#3,D1
	bne.s	Ej
	putchr	<'+'>
Ej	rts

more_than_three
	cmp.b	#4,D1
	bne.s	no_four		;-(An)
	putchr	<'-'>
	bra.s	twocase

no_four	cmp.b	#5,D1
	bne.s	no_five
	move.b	D0,-(sp)	;disp(An)
	bsr	DGetWord
	move.w	D2,D0
	ext.l	d0
	bsr	put_signed_hexnum	%%signed offset
	move.b	(sp)+,D0
	bra.s	twocase

no_five	cmp.b	#6,D1
	bne.s	must_be_seven
	move.b	D0,-(sp)	;disp(An,index)
	bsr	DGetWord
	move.b	D2,D0
	ext.w	d0
	ext.l	d0
	bsr	put_signed_hexnum	%%signed offset
	putchr	<'('>
	move.b	(sp)+,d0
	bsr	putacom
	putchr	COMMA
	bsr	put_index
	putchr	<')'>
	rts

must_be_seven
	dbf	D0,no_abs_short		;jump if reg<>0
	bsr	DGetWord
	move.w	D2,D0
	ext.l	d0
	bra	put_signed_hexnum	;absolute short -- %%signed

no_abs_short
	dbf	D0,no_abs_long		;jump if reg<>1
	bsr	DGetLong
	move.l	D2,D0
	bra	puthex_68		%%absolute long address

no_abs_long
	dbf	D0,no_pc_rel		;jump if reg<>2
	bsr	DGetWord
	move.w	D2,D0
	ext.l	D0
	add.l	A4,D0
	subq.l	#2,D0
	bsr	puthex_68		%%pc relative address
	lea	pcnam(pc),A1
	bra	putstring

no_pc_rel
	dbf	D0,no_pc_ind		;jump if reg<>3
	bsr	DGetWord
	move.b	D2,D0
	ext.w	D0
	ext.l	D0
	add.l	A4,D0
	subq.l	#2,D0			pc-relative with index
	bsr	puthex_68		%%pc relative address
	move.l	#'(pc,',d0
	bsr	PutLong
	bsr.s	put_index
	putchr	<')'>
	rts
no_pc_ind
	dbf	D0,illegal_mode
Immediate
	putchr	<'#'>
	move.w	size(a5),d0
	tst.w	d0
	beq.s	bytesize
	subq.w	#1,d0
	beq.s	wordsize
; longsize
	bsr	DGetLong
	move.l	d2,d0
	bra	puthex_68
wordsize
	bsr	DGetWord
	moveq	#4,d1
	bra.s	immc
bytesize
	bsr	DGetWord
	moveq	#2,d1
immc	move.l	d2,d0
	bra	put_hexnum

illegal_mode
	addq.l	#4,sp	;pop return address
	bra	diserr

put_index	;index byte in D2
	move.w	D2,D0
	lsr.w	#8,D0
	lsr.w	#4,D0
	tst.w	d2
	bmi.s	adreg
	bsr	putdcom
	bra.s	Ind1
adreg	bsr	putacom
Ind1	putchr	<'.'>
	btst	#11,D2
	bne.s	IndLong
	putchr	<'w'>
	rts
IndLong	putchr	<'l'>
	rts

get_size	;0=byte, 1=word, 2=long
	move.w	opcode(A5),D0
	move.w	D0,D1
	and.w	#$C000,D0
	bne.s	normsize1
	move.w	D1,D0
	and.w	#$3000,D0
	beq.s	normsize1
	cmp.w	#$1000,D0
	beq.s	bytesize1
	cmp.w	#$2000,D0
	beq.s	longsize1
	bra.s	wordsize1
normsize1
	move.w	d1,d0
	lsr.w	#6,d0
	and.w	#3,d0
	cmp.w	#3,d0
	bne.s	rsize_9
	btst	#8,d1
	bne.s	longsize1
wordsize1
	moveq	#1,d0
	rts
longsize1
	moveq	#2,d0
	rts
bytesize1
	moveq	#0,d0
rsize_9	rts

putsize	bsr	get_size
	move.w	d0,size(a5)
	putchr	<'.'>
	move.b	sizch(pc,d0.w),(a3)+
	rts

sizch	dc.b	'bwl'
	even

*** GET WORD FOR DISASSEMBLER ***
; result returned in d2
DGetWord
	move.w	(A4)+,D0
	move.w	D0,D2
	move.l	A3,-(sp)
	move.l	A2,A3
	moveq	#4,d1
	bsr	put_hexnum1
	move.l	A3,A2
	move.l	(sp)+,A3
	move.b	#SPACE,(A2)+
	rts

*** GET LONGWORD FOR DISASSEMBLER ***
; result returned in d2
DGetLong
	move.l	(A4)+,D0
	move.l	D0,D2
	move.l	A3,-(sp)
	move.l	A2,A3
	bsr	phex1_8
	move.l	A3,A2
	move.l	(sp)+,A3
	move.b	#SPACE,(A2)+
	rts

**** DISPLAY MEMORY ****
memdisplay
	bsr	getparams
	move.l	Addr(A5),A4
disploop
	startline
	move.l	A4,D0
	bsr	puthex1_68
	putchr	<':'>
	putchr	SPACE
	moveq	#16/4-1,D2
01$	move.l	(A4)+,D0
	bsr	phex1_8
	putchr	SPACE
	dbf	D2,01$
	sub.w	#16,A4
	putchr	SPACE
	move.w	flags(a5),d6
	bmi.s	100$
	putchr	<''''>
100$	moveq	#16-1,D2
02$	move.b	(A4)+,D0	;printable codes are $20-$7F and $A0-$FF
	cmp.b	#SPACE+$80,D0
	bcc.s	03$
	cmp.b	#SPACE,D0
	bge.s	03$		;note: signed comparison handles correctly codes >= $80
	move.b	#'.',D0
03$	move.b	D0,(A3)+
	dbf	D2,02$
	tst.w	d6
	bmi.s	101$
	putchr	<''''>
101$	endline
	bsr	printstring
	bsr	CheckEnd
	bne.s	disploop
	move.l	A4,Addr(A5)
	bra	mainloop

**** SKIP SPACES ****
skipspaces
	cmp.b	#SPACE,(A3)+
	beq.s	skipspaces
	subq.l	#1,A3
	rts

*** PART OF GETNUM (Get Ascii number in single quotes) ***
AsciiNum
	addq.l	#1,A3
Ascii_loop
	move.b	(A3)+,D1
	beq.s	Ascii_2
	cmp.b	#'''',D1
	beq.s	Ascii_1
Ascii_0	lsl.l	#8,D0
	move.b	D1,D0
	bra.s	Ascii_loop
Ascii_1	cmp.b	#'''',(A3)+	;check for double single quote
	beq.s	Ascii_0
	subq.l	#1,A3
Ascii_2	move.l	(sp)+,D2
	rts

**** GET A HEX OR DECIMAL NUMBER ****
* (no check for overflow!)
* number formats:
*  'abcd'    : ascii string representing a longword
*  nnn, $nnn : positive hex number
*  -$nnn     : negative hex number
*  +nnn      : positive decimal number
*  -nnn      : negative decimal numbber
GetNum	move.l	D2,-(sp)
	bsr.s	skipspaces
	moveq	#0,D2
	moveq	#0,D0
	cmp.b	#'''',(A3)
	beq.s	AsciiNum
	cmp.b	#'-',(A3)
	bne.s	posnum
	moveq	#-1,D2
	addq.l	#1,A3
	cmp.b	#'$',(A3)+
	beq.s	gethex1
	subq.l	#1,A3
	bra.s	getdec
posnum	cmp.b	#'+',(A3)
	bne.s	GetHex
	addq.l	#1,A3
getdec	moveq	#0,D1		;get decimal number
numloop	move.b	(A3)+,D1
	sub.b	#'0',D1
	blt.s	num9
	cmp.b	#9,D1
	bgt.s	num9
	move.l	D0,-(sp)	;multiply by 10 ...
	asl.l	#2,D0
	add.l	(sp)+,D0
	asl.l	#1,D0
	add.l	D1,D0		;add the digit
	bra.s	numloop
GetHex	;get hexadecimal number
	cmp.b	#'$',(A3)+
	beq.s	gethex1
	subq.l	#1,A3
gethex1	move.b	(A3)+,D1	;get hex number
	cmp.b	#'0',D1
	bcs.s	num9
	cmp.b	#'9',D1
	bls.s	hxnum
	cmp.b	#'A',D1
	bcs.s	num9
	and.b	#$DF,D1
	cmp.b	#'A',D1
	bcs.s	num9
	cmp.b	#'F',D1
	bhi.s	num9
	sub.b	#55,D1
	bra.s	hxl1
hxnum	and.b	#$0F,D1
hxl1	asl.l	#4,D0
	or.b	D1,D0
	bra.s	gethex1
num9	subq.l	#1,A3
	tst.l	D2
	beq.s	num99
	neg.l	D0
num99	move.l	(sp)+,D2
	rts

*** Convert char in D0 to lower case ***
tolower	cmp.b	#'A',D0
	bcs.s	low1
	cmp.b	#'Z',D0
	bhi.s	low1
	bset	#5,D0
low1	rts

*** ASSEMBLE ***
* command format:
* a       :assembles at the current address, asks instruction
* a <addr>:assembles at <addr>, asks instruction
* a <addr> <instruction> : assembles <instruction> at <addr>
* in all cases, if no errors occurred, prompts '<next_addr>:'
* and asks a new instruction. use <CR> to exit this mode
* Ctrl-E can be used to edit the disassembled instruction at this location
assemble:
	bsr	skipspaces
	tst.b	(A3)
	bne.s	assem_01
	move.l	Addr(A5),D0
	bra.s	assem_02
assem_01
	bsr	GetNum
	btst	#0,D0
	bne	error	;assembling to odd address is illegal
	move.l	D0,Addr(A5)
assem_02
	move.l	D0,EndAddr(A5)
	bsr	skipspaces
	tst.b	(A3)
	bne.s	assem1a

assem1	move.l	Addr(A5),D0
	move.l	D0,EndAddr(A5)
	lea	assemfmt(pc),a0
	bsr	printf_window
	moveq	#1,D0

assem1a0
	bsr	GetInput
	tst.w	D0		;check for Ctrl-E (GetInput returns -1)
	bpl.s	assem1a1	;branch if not Ctrl-E
*** disassemble at current address and put result in input buffer ***
	move.l	Addr(A5),A4
	bsr	disasmline
	move.l	outbufadr(A5),A0
	lea	(InpBuf-OutBuf)(A0),A1
	move.l	d6,a0
	moveq	#LF,D0
300$	move.b	(A0)+,(A1)+	copy instruction to input buffer
	cmp.b	(A0),D0 	until LF found
	bne.s	300$
	clr.b	(A1)		mark end of string
	moveq	#2,D0		GetInput mode: edit existing line
	bra.s	assem1a0
assem1a1
	tst.b	(A3)
	beq	mainloop

assem1a	move.l	A3,A1
	moveq	#0,D1		;quote mode flag
lowerloop			;convert to lower case when not in quotes
	move.b	(A1),D0
	beq.s	asmlow9
	cmp.b	#'''',D0
	bne.s	noquote
	not.b	d1
noquote	tst.b	D1
	bne.s	asmlow1
	bsr	tolower
asmlow1	move.b	D0,(A1)+
	bra.s	lowerloop
asmlow9	lea	instradrs(pc),A2
	moveq	#-1,D1
find_instr_loop	;find the instruction opcode from the opcode table
	addq.w	#1,D1
	move.l	A3,A1
	move.l	A2,A0
	tst.w	(A2)+
	beq.s	try_branch
	add.w	(A0),A0
comp_str
	tst.b	(A0)
	beq.s	instr_found
	cmp.b	(A0)+,(A1)+
	beq.s	comp_str
	bra.s	find_instr_loop

try_branch			;if instruction not found in table, it can be
	cmp.b	#'b',(A3)	;a branch, DBcc, Scc or dc.?
	bne.s	try_Scc
	addq.l	#1,A3
	bsr	check_cond
	tst.w	D1
	bpl.s	branch_1
br_err	bra	error

branch_1
	lsl.w	#8,D1
	or.w	#$6000,D1
	cmp.b	#'.',(A3)
	bne.s	long_branch
	addq.l	#1,A3
	cmp.b	#'l',(A3)+
	beq.s	long_branch
	cmp.b	#'s',-1(A3)
	bne.s	br_err
	bsr	GetNum
	sub.l	Addr(A5),D0
	subq.l	#2,D0
	move.b	D0,D2
	ext.w	D2
	ext.l	D2
	cmp.l	D0,D2
	bne.s	out_of_range
	and.w	#$FF,D0		short branch can't branch zero bytes
	beq.s	br_err
	or.w	D1,D0
	bra	one_word_instr
long_branch
	bsr	GetNum
	sub.l	Addr(A5),D0
	subq.l	#2,D0
	move.w	D0,D2
	ext.l	D2
	cmp.l	D0,D2
	bne.s	out_of_range
	exg	D0,D1
	bra	two_words_instr

instr_found
	move.l	A1,A3
	add.w	D1,D1
	lea	instrjumps(pc),A0
	add.w	D1,A0
	add.w	(A0),A0
	jmp	(A0)

out_of_range
	lea	outrangetxt(pc),a0
	bsr	printstring_a0_window
	bra	mainloop

try_Scc	cmp.b	#'s',(A3)
	bne.s	try_DBcc
	addq.l	#1,A3
	cmp.b	#'f',(A3)+
	bne.s	Scc_1
	moveq	#1,D1
	bra.s	Scc_3
Scc_1	cmp.b	#'t',-1(A3)
	bne.s	Scc_2
	moveq	#0,D1
	bra.s	Scc_3
Scc_2	subq.l	#1,A3
	bsr	check_cond
	tst.w	D1
	bmi.s	cc_err
	cmp.w	#2,D1
	bcs.s	cc_err
Scc_3	move.w	D1,D3
	addq.l	#2,Addr(A5)
	bsr	GetEA
	cmp.w	#1,D1
	beq.s	cc_err
	lsl.w	#8,D3
	lsl.w	#3,D1
	or.w	D1,D0
	cmp.w	#$39,D0
	bhi.s	cc_err
	or.w	D3,D0
	or.w	#$50C0,D0
	bra	zzcom

cc_err	bra	error

try_DBcc
	cmp.b	#'d',(A3)+
	bne.s	cc_err
	cmp.b	#'b',(A3)+
	bne.s	try_dc
	cmp.b	#'t',(A3)+
	bne.s	DBcc_1
	moveq	#0,D1
	bra.s	DBcc_3
DBcc_1	cmp.b	#'f',-1(A3)
	bne.s	DBcc_2
DBcc_1a	moveq	#1,D1
	bra.s	DBcc_3
DBcc_2	subq.l	#1,A3
	bsr	check_cond
	tst.w	D1
	beq.s	DBcc_1a
	bmi.s	cc_err
	cmp.w	#1,D1
	beq.s	cc_err
DBcc_3	move.w	D1,D3
	bsr	skipspaces
	bsr	getdreg
	bmi.s	cc_err
	move.w	d1,d0
	lsl.w	#8,D3
	or.w	D0,D3
	or.w	#$50C8,D3
	bsr	SkipComma
	bmi.s	cc_err
	bsr	GetNum	
	sub.l	Addr(A5),D0
	subq.l	#2,D0
	move.l	D0,D1
	ext.l	D0
	cmp.l	D0,D1
	bne	out_of_range
	move.w	D3,D0
	bra	two_words_instr
try_dc	cmp.b	#'c',-1(A3)
	bne.s	cc_err
	bsr	GetSize	;*** DC.B, DC.W, DC.L ***
	bsr	skipspaces
	move.l	Addr(A5),A0
	move.w	size(A5),D0
	beq.s	dc_byte
	subq.w	#1,D0
	beq.s	dc_word
dc_long	bsr	GetNum
	move.l	D0,(A0)+
	bsr	SkipComma
	bpl.s	dc_long
	bra.s	dc_exit
dc_word	bsr	GetNum
	move.w	D0,(A0)+
	bsr	SkipComma
	bpl.s	dc_word
	bra.s	dc_exit
dc_byte	bsr	getstring
	move.l	A1,A0
dc_exit	move.l	A0,D0	;align address to word boundary
	btst	#0,D0
	beq.s	dc_exit1
	addq.l	#1,D0
dc_exit1
	move.l	D0,Addr(A5)
	bra.s	dc_exit2

assem9	bsr	skipspaces
	tst.b	(a3)
	bne	error

	lea	UpAndClearEol(pc),A0	;move cursor to previous line and clear the line
	bsr	printstring_a0_window
	move.l	EndAddr(A5),A4
	bsr	disasmline
	bsr	printstring_window
dc_exit2
	bra	assem1

; check if the chars at A3 form a condition code
; result returned in D1, condition number or -1 if not found
; now unserstands hs and lo-conditions (same as cc and cs)
check_cond
	move.b	(A3)+,D0
	lsl.w	#8,D0
	move.b	(A3)+,D0
	lea	condcodes(pc),A0
	moveq	#0,D1
chk_cond1
	cmp.w	(A0)+,D0
	beq.s	cond9
	addq.w	#1,D1
	cmp.w	#18,D1
	bcs.s	chk_cond1
	moveq	#-1,D1
cond9	cmp.w	#16,d1
	bcs.s	cond10
	sub.w	#12,d1
cond10	rts

*** SHIFT INSTRUCTIONS ***
as_asm	move.w	#$E000,D3
	bra.s	shift_instr

ls_asm	move.w	#$E008,D3
	bra.s	shift_instr

rox_asm	move.w	#$E010,D3
	bra.s	shift_instr

rot_asm	move.w	#$E018,D3

shift_instr
	cmp.b	#'r',(A3)+		get direction
	beq.s	shift_1
	cmp.b	#'l',-1(A3)
	bne.s	sh_err
	bset	#8,D3
shift_1	cmp.b	#'.',(A3)
	bne.s	shift_mem
	bsr	GetSize
	move.w	size(A5),D0
	lsl.w	#6,D0
	or.w	D0,D3
	bsr	skipspaces
	cmp.b	#'#',(A3)
	bne.s	count_in_reg
	addq.l	#1,A3
	bsr	GetNum_1_8
	bmi.s	sh_err
	bra.s	shift_2
count_in_reg
	bsr	getdreg
	bmi.s	sh_err
	move.w	d1,d0
	bset	#5,D3
shift_2	lsl.w	#8,D0
	lsl.w	#1,D0
	or.w	D0,D3
	bsr	SkipComma
	bmi.s	sh_err
	bsr	getdreg
	bmi.s	sh_err
	move.w	d1,d0
	or.w	D3,D0
	bra	one_word_instr

sh_err	bra	error

shift_mem
	addq.l	#2,Addr(A5)
	moveq	#0,D0
	move.b	D3,D0
	lsl.w	#6,D0
	or.w	D0,D3
	and.w	#$F7C0,D3
	or.w	#$C0,D3
	bsr	GetEA
	cmp.w	#2,D1
	bcs.s	sh_err
	lsl.w	#3,D1
	or.w	D1,D0
	cmp.w	#$39,D0
	bhi.s	sh_err
	or.w	D3,D0
	bra.s	zcom_1

*** ADD & SUB ***
add_asm	move.w	#$D000,D3
	bra.s	a_s_asm

sub_asm	move.w	#$9000,D3

a_s_asm	cmp.b	#'x',(A3)
	bne.s	no_as_ext
	addq.l	#1,A3
	bsr	GetSize
	bset	#8,D3
	bra	ext_as_asm
no_as_ext
	addq.l	#2,Addr(A5)
	cmp.b	#'q',(A3)
	bne.s	no_as_quick
	addq.l	#1,A3
	bsr	GetSize
	bsr	skipspaces
	cmp.b	#'#',(A3)+
	bne.s	as_err
	bsr	GetNum_1_8
	bmi.s	as_err
	lsl.w	#8,D0
	lsl.w	#1,D0
	cmp.w	#$9000,D3
	bne.s	as_quick_1
	bset	#8,D0
as_quick_1
	or.w	#$5000,D0
	move.w	size(A5),D1
	lsl.w	#6,D1
	or.w	D1,D0
	move.w	D0,D3
	bsr	SkipComma
	bmi.s	as_err
	bsr	GetEA
	tst.w	size(A5)
	bne.s	as_quick_z1
	cmp.w	#1,D1
	beq.s	as_err
as_quick_z1
	lsl.w	#3,D1
	or.w	D1,D0
;#
;# fixed addq/subq addressing mode checking 1989-08-28
;#
	cmp.w	#$39,d0
	bhi.s	as_err
	or.w	D3,D0
zcom_1	bra	zzcom

as_err	bra	error

no_as_quick
	cmp.b	#'i',(A3)
	bne.s	no_as_imm
	addq.l	#1,A3
	bsr	GetSize
	bsr	skipspaces
	cmp.b	#'#',(A3)+
	bne.s	as_err
as_imm_1
	bsr	PutImm
	bsr	SkipComma
	bmi.s	as_err
	bsr	GetEA
	cmp.w	#1,D1
	beq.s	as_addr_imm_1
	lsl.w	#3,D1
	or.w	D1,D0
	move.w	d0,d1
	and.w	#$3f,d1
	cmp.w	#$39,d1
	bhi.s	as_err
	move.w	size(A5),D1
	lsl.w	#6,D1
	or.w	D1,D0
	bset	#10,D0
	cmp.w	#$9000,D3
	beq.s	zcom_1
	bset	#9,D0
	bra.s	zcom_1

as_addr_imm_1
	lsl.w	#8,D0
	lsl.w	#1,D0
	move.w	size(A5),D1
	beq.s	as_err
	subq.w	#1,D1
	lsl.w	#8,D1
	or.w	D1,D0
	or.w	#$FC,D0
	or.w	D3,D0
	bra.s	zcom_1

no_as_imm
	moveq	#0,D5
	cmp.b	#'a',(A3)
	bne.s	as_norm_1
	moveq	#-1,D5
	addq.l	#1,A3
as_norm_1
	bsr	GetSize
	bsr	skipspaces
	tst.l	D5
	bne.s	as_norm_2
	cmp.b	#'#',(A3)+
	beq.s	as_imm_1
	subq.l	#1,A3
as_norm_2
	cmp.b	#'d',(A3)
	beq.s	as_data_reg_source
	bsr	GetEA
	tst.w	size(A5)
	bne.s	as_norm_3
	cmp.w	#1,D1		address register diret can't be used
	beq.s	as_err2		with byte size
as_norm_3
	lsl.w	#3,D1
	or.w	D1,D0
	or.w	D0,D3
	bsr	SkipComma
	bmi.s	as_err2
	cmp.b	#'d',(A3)
	bne.s	try_as_addr
	tst.l	D5
	bne.s	as_err2
	bsr	getdreg
	bmi.s	as_err2
	move.w	d1,d0
	lsl.w	#8,D0
	lsl.w	#1,D0
	or.w	D3,D0
	move.w	size(A5),D1
	lsl.w	#6,D1
	or.w	D1,D0
	bra.s	zcom_2
as_err2	bra	error

try_as_addr
	bsr	getareg
	bmi.s	as_err2
	move.w	d1,d0
	lsl.w	#8,D0
	lsl.w	#1,D0
	or.w	D3,D0
	or.w	#$C0,D0
	move.w	size(A5),D1
	beq.s	as_err2
	subq.w	#1,D1
	lsl.w	#8,D1
	or.w	D1,D0
zcom_2	bra	zzcom

as_data_reg_source
	bsr	getdreg
	bmi.s	as_err2
	lsl.w	#8,D1
	lsl.w	#1,D1
	or.w	D1,D3
	bsr	SkipComma
	bmi.s	as_err2
	bsr	GetEA
	tst.w	D1
	bne.s	as_datasource_1
	tst.l	d5
	bne.s	as_err2
	lsl.w	#8,D0
	lsl.w	#1,D0
	move.w	D3,D1
	and.w	#$F000,D3
	lsr.w	#8,D1
	lsr.w	#1,D1
	and.w	#7,D1
	or.w	D3,D0
	or.w	D1,D0
	bra.s	as_ds_1

as_datasource_1
	cmp.w	#1,D1
	beq.s	as_special
	tst.l	d5
	bne.s	as_err2
	lsl.w	#3,D1
	or.w	D1,D0
	cmp.w	#$39,D0
	bhi.s	as_err2
	or.w	D3,D0
	bset	#8,D0
as_ds_1	move.w	size(A5),D1
	lsl.w	#6,D1
	or.w	D1,D0
	bra.s	zcom_2

as_special		;handle adda/suba
	lsl.w	#8,D0
	lsl.w	#1,D0
	move.w	D3,D1
	and.w	#$F000,D3
	or.w	D3,D0
	lsr.w	#8,D1
	lsr.w	#1,D1
	and.w	#7,D1
	or.w	D1,D0
	move.w	size(A5),D1
	beq.s	cmp_err
	subq.w	#1,D1
	lsl.w	#8,D1
	or.w	D1,D0
	or.w	#$C0,D0
	bra	zzcom

*** CMP ***
cmp_asm	cmp.b	#'m',(A3)
	bne.s	no_mem_cmp
	addq.l	#1,A3
	bsr	GetSize	;CMPM
	bsr	skipspaces
	cmp.b	#'(',(A3)+
	bne.s	cmp_err
	bsr	getareg
	bmi.s	cmp_err
	move.w	d1,d3
	cmp.b	#')',(A3)+
	bne.s	cmp_err
	cmp.b	#'+',(A3)+
	bne.s	cmp_err
	bsr	SkipComma
	bmi.s	cmp_err
	cmp.b	#'(',(A3)+
	bne.s	cmp_err
	bsr	getareg
	bmi.s	cmp_err
	move.w	d1,d0
	cmp.b	#')',(A3)+
	bne.s	cmp_err
	cmp.b	#'+',(A3)+
	bne.s	cmp_err
	lsl.w	#8,D0
	lsl.w	#1,D0
	or.w	D3,D0
;#
;# cmpm size specifiers fixed 1989-08-28
;#
	move.w	size(a5),d1
	lsl.w	#6,d1
	or.w	d1,d0
	or.w	#$B108,D0
	bra	one_word_instr
cmp_err	bra	error
no_mem_cmp
	addq.l	#2,Addr(A5)
	cmp.b	#'i',(A3)
	bne.s	no_cmp_imm
	addq.l	#1,A3
	bsr	GetSize
	bsr	skipspaces
	cmp.b	#'#',(A3)+
	bne.s	cmp_err
cmp_imm_1
	bsr	PutImm
	bsr	SkipComma
	bmi.s	cmp_err
	bsr	GetEA
	cmp.w	#1,D1
	beq.s	cmp_addr_imm1
	lsl.w	#3,D1
	or.w	D1,D0
	cmp.w	#$39,D0
	bhi.s	cmp_err
	move.w	size(A5),D1
	lsl.w	#6,D1
	or.w	D1,D0
	or.w	#$0C00,D0
	bra	zzcom

cmp_addr_imm1
	lsl.w	#8,D0
	lsl.w	#1,D0
	move.w	size(A5),D1
	beq.s	cmp_err
	subq.w	#1,D1
	lsl.w	#8,D1
	or.w	D1,D0
	or.w	#$B0FC,D0
	bra	zzcom

no_cmp_imm
	moveq	#0,D5
	cmp.b	#'a',(A3)
	bne.s	no_cmp_addr
	addq.l	#1,A3
	moveq	#-1,D5
no_cmp_addr
	bsr	GetSize
	bsr	skipspaces
	tst.l	D5
	bne.s	cmp_1
	cmp.b	#'#',(A3)+
	beq.s	cmp_imm_1
	subq.l	#1,A3
cmp_1	bsr	GetEA
	cmp.w	#1,D1
	bne.s	cmp_2
	tst.w	size(A5)
	beq.s	cmp_err2
cmp_2	lsl.w	#3,D1
	or.w	D1,D0
	move.w	D0,D3
	bsr	SkipComma
	bmi.s	cmp_err2
	bsr	getreg
	bmi.s	cmp_err2
	btst	#3,D1
	bne.s	cmp_addr
	tst.l	D5
	bne.s	cmp_err2
	move.w	size(A5),D0
	lsl.w	#6,D0
	or.w	D3,D0
	and.w	#7,D1
	lsl.w	#8,D1
	lsl.w	#1,D1
	or.w	D1,D0
	or.w	#$B000,D0
	bra	zzcom

cmp_err2
	bra	error

cmp_addr
	move.w	size(A5),D0
	beq.s	cmp_err2
	subq.w	#1,D0
	lsl.w	#8,D0
	or.w	D3,D0
	and.w	#7,D1
	lsl.w	#8,D1
	lsl.w	#1,D1
	or.w	D1,D0
	or.w	#$B0C0,D0
	bra	zzcom

*** AND & OR ***
and_asm	move.w	#$C000,D3
	bra.s	a_o_asm

or_asm	move.w	#$8000,D3

a_o_asm	addq.l	#2,Addr(A5)
	cmp.b	#'i',(A3)
	bne.s	a_o_2
	addq.l	#1,A3
	cmp.b	#'.',(A3)
	beq.s	a_o_0
	lsr.w	#5,D3
	and.w	#$0200,D3
	bra	logical_status

a_o_0	bsr	GetSize
	bsr	skipspaces
	cmp.b	#'#',(A3)+
	bne.s	a_o_err
a_o_imm	bsr	PutImm
	bsr	SkipComma
	bmi.s	a_o_err
	bsr	GetEA
	cmp.w	#1,D1
	beq.s	a_o_err
	lsl.w	#3,D1
	or.w	D1,D0
	cmp.w	#$39,D0
	bhi.s	a_o_err
	move.w	size(A5),D1
	lsl.w	#6,D1
	or.w	D1,D0
	cmp.w	#$C000,D3
	bne.s	zcom_3
	bset	#9,D0
	bra.s	zcom_3
a_o_err	bra	error

a_o_2	bsr	GetSize
	bsr	skipspaces
	cmp.b	#'#',(A3)+
	beq.s	a_o_imm
	move.w	size(A5),D0
	lsl.w	#6,D0
	or.w	D0,D3
	subq.l	#1,A3
	cmp.b	#'d',(A3)
	bne.s	reg_dest
	bsr	getdreg
	bmi.s	a_o_err
	lsl.w	#8,D1
	lsl.w	#1,D1
	or.w	D1,D3
	bsr	SkipComma
	bmi.s	a_o_err
	bsr	GetEA
	tst.w	D1
	beq.s	a_o_3a
	cmp.w	#1,D1
	beq.s	a_o_err
	lsl.w	#3,D1
	or.w	D1,D0
	cmp.w	#$39,D0
	bhi.s	a_o_err
	bset	#8,D0
	or.w	D3,D0
zcom_3	bra	zzcom

a_o_3a	move.w	D0,D7
	move.w	D3,D0
	lsr.w	#8,D0
	lsr.w	#1,D0
	and.w	#7,D0
	and.w	#$F0C0,D3
	lsl.w	#8,D7
	lsl.w	#1,D7
	or.w	D7,D3
	lsl.w	#3,D1
	or.w	D1,D0
	or.w	D3,D0
	bra.s	zcom_3
reg_dest
	bsr	GetEA
	cmp.w	#1,D1
	beq.s	a_o_err2
	lsl.w	#3,D1
	or.w	D1,D0
;# this check prevented the use of pcrelative addressing modes
;# with and/or - fixed 1989-08-27
;#	cmp.w	#$39,D0
;#	bhi.s	a_o_err2
	or.w	D0,D3
	bsr	SkipComma
	bmi.s	a_o_err2
	bsr	getdreg
	bmi.s	a_o_err2
	move.w	d1,d0
	lsl.w	#8,D0
	lsl.w	#1,D0
	or.w	D3,D0
	bra.s	zcom_3
a_o_err2
	bra	error

*** ABCD & SBCD & ADDX & SUBX ***
abcd_asm
	move.w	#$C100,D3
	bra.s	bcd_asm
sbcd_asm
	move.w	#$8100,D3

bcd_asm	clr.w	size(A5)
ext_as_asm
	bsr	skipspaces
	cmp.b	#'d',(A3)
	bne.s	bcd_mem
	bsr	getdreg
	bmi.s	err_bcd
	or.w	D1,D3
	bsr	SkipComma
	bmi.s	err_bcd
	bsr	getdreg
	bmi.s	err_bcd
bcd_1	lsl.w	#8,D1
	lsl.w	#1,D1
	or.w	D3,D1
	move.w	size(A5),D0
	lsl.w	#6,D0
	or.w	D1,D0
	bra	one_word_instr

bcd_mem	cmp.b	#'-',(A3)
	bne.s	err_bcd
	addq.l	#1,a3
	cmp.b	#'(',(A3)+
	bne.s	err_bcd
	bsr	getareg
	bmi.s	err_bcd
	or.w	d1,d3
	bset	#3,d3
	cmp.b	#')',(A3)+
	bne.s	err_bcd
	bsr	SkipComma
	bmi.s	err_bcd
	cmp.b	#'-',(A3)+
	bne.s	err_bcd
	cmp.b	#'(',(A3)+
	bne.s	err_bcd
	bsr	getareg
	bmi.s	err_bcd
	cmp.b	#')',(A3)+
	beq.s	bcd_1
err_bcd	bra	error

*** EOR ***
eor_asm	addq.l	#2,Addr(A5)
	moveq	#0,D5
	cmp.b	#'i',(A3)
	bne.s	eor_1
	addq.l	#1,A3
	cmp.b	#'.',(A3)
	beq.s	eor_0
	move.w	#$0A00,D3
	bra	logical_status
eor_0
	moveq	#-1,D5
eor_1
	bsr	GetSize
	bsr	skipspaces
	cmp.b	#'#',(A3)
	bne.s	no_eor_imm
	addq.l	#1,A3
	bsr	PutImm
	bsr	SkipComma
	bmi.s	err_bcd
	bsr	GetEA
	cmp.w	#1,D1
	beq.s	err_bcd
	lsl.w	#3,D1
	or.w	D1,D0
	cmp.w	#$39,D0
	bhi.s	err_bcd
	move.w	size(A5),D1
	lsl.w	#6,D1
	or.w	D1,D0
	or.w	#$0A00,D0
	bra.s	zcom_4
no_eor_imm
	tst.l	D5
	bne.s	err_bcd
	bsr	getdreg
	bmi.s	err_bcd
	move.w	D1,D3
	bsr	SkipComma
	bmi.s	err_bcd	
	bsr	GetEA
	cmp.w	#1,D1
	beq	error
	lsl.w	#3,D1
	or.w	D1,D0
	cmp.w	#$39,D0
	bhi	error
	lsl.w	#8,D3
	lsl.w	#1,D3
	or.w	D3,D0
	move.w	size(A5),D1
	lsl.w	#6,D1
	or.w	D1,D0
	or.w	#$B100,D0
zcom_4	bra	zzcom

*** AND & OR & EOR SR & CCR ***
logical_status
	or.w	#$3C,D3
	bsr	skipspaces
	cmp.b	#'#',(A3)+
	bne.s	move_err
	move.w	#WSIZE,size(A5)
	bsr	PutImm
	bsr	SkipComma
	bmi.s	move_err
	cmp.b	#'s',(A3)
	bne.s	no_log_sr
	addq.l	#1,A3
	cmp.b	#'r',(A3)+
	bne.s	move_err
	or.w	#$40,D3
	bra.s	log_st1
no_log_sr
	moveq	#'c',D0
	cmp.b	(A3)+,D0
	bne.s	move_err
	cmp.b	(A3)+,D0
	bne.s	move_err
	cmp.b	#'r',(a3)+
	bne.s	move_err
log_st1
	move.w	D3,D0
	bra.s	zcom_4

*** MOVE ***
move_asm
	cmp.b	#'q',(A3)
	bne.s	no_move_quick
	addq.l	#1,A3
	bsr	skipspaces
	cmp.b	#'#',(A3)+
	bne.s	move_err
	bsr	GetNum
	move.b	D0,D2
	bsr	SkipComma
	bmi.s	move_err
	bsr	getdreg
	bmi.s	move_err
	move.w	d1,d0
	lsl.w	#8,D0
	lsl.w	#1,D0
	or.w	#$7000,D0
	move.b	D2,D0
	bra	one_word_instr
move_err
	bra	error
no_move_quick
	cmp.b	#'p',(A3)
	beq	move_peripheral
	cmp.b	#'a',(A3)
	bne.s	no_move_addr
	moveq	#-1,D5
	addq.l	#1,A3
	bra	normal_move_2
no_move_addr
	cmp.b	#'m',(A3)
	beq	movem_asm
	cmp.b	#'.',(A3)
	beq	normal_move
	bsr	skipspaces
	cmp.b	#'s',(A3)
	beq.s	move_status
	cmp.b	#'u',(A3)
	beq.s	move_from_usp
	cmp.b	#'s',(a3)
	beq.s	move_to_usp
	cmp.b	#'a',(A3)
	bne.s	move_to_status_or_ccr
move_to_usp
	bsr	getareg
	bmi.s	move_err
	move.w	d1,d3
	bsr	SkipComma
	bmi.s	move_err
	cmp.b	#'u',(A3)+
	bne.s	move_err
	cmp.b	#'s',(A3)+
	bne.s	move_err
	cmp.b	#'p',(A3)+
	bne.s	move_err
	move.w	D3,D0
	or.w	#$4E60,D0
	bra	one_word_instr
move_from_usp	;MOVE	usp,An
	addq.l	#1,A3
	cmp.b	#'s',(A3)+
	bne.s	move_err2
	cmp.b	#'p',(A3)+
	bne.s	move_err2
	bsr	SkipComma
	bmi.s	move_err2
	bsr	getareg
	bmi.s	move_err2
	move.w	d1,d0
	or.w	#$4E68,D0
	bra	one_word_instr
move_status	;MOVE sr,EA
	addq.l	#2,Addr(A5)
	addq.l	#1,A3
	cmp.b	#'r',(A3)+
	bne.s	move_err2
	bsr	SkipComma
	bmi.s	move_err2
	bsr	GetEA
	cmp.w	#1,D1
	beq.s	move_err2
	lsl.w	#3,D1
	or.w	D1,D0
	cmp.w	#$39,D0
	bhi.s	move_err2
	or.w	#$40C0,D0
	bra.s	zcom_5
move_to_status_or_ccr	;MOVE EA,sr, MOVE EA,ccr
	move.w	#WSIZE,size(A5)
	addq.l	#2,Addr(A5)
	bsr	GetEA
	lsl.w	#3,D1
	or.w	D0,D1
	bsr	SkipComma
	bmi.s	move_err2
	cmp.b	#'s',(A3)
	bne.s	try_move_ccr
	addq.l	#1,A3
	cmp.b	#'r',(A3)+
	bne.s	move_err2
	move.w	D1,D0
	or.w	#$46C0,D0
	bra.s	zcom_5
move_err2
	bra	error
try_move_ccr
	moveq	#'c',D0
	cmp.b	(A3)+,D0
	bne.s	move_err2
	cmp.b	(A3)+,D0
	bne.s	move_err2
	cmp.b	#'r',(A3)+
	bne.s	move_err2
	move.w	D1,D0
	or.w	#$44C0,D0
zcom_5	bra	zzcom

normal_move
	moveq	#0,D5
normal_move_2
	addq.l	#2,Addr(A5)
	bsr	GetSize
	bsr	GetEA
	tst.w	size(A5)
	bne.s	nm_1
	cmp.w	#1,D1
	beq.s	move_err2
nm_1	lsl.w	#3,D1
	or.w	D1,D0
	move.w	D0,D3
	bsr	SkipComma
	bmi.s	move_err2
	bsr	GetEA
	move.l	D1,D2
	lsl.w	#3,D2
	or.w	D0,D2
	cmp.w	#$39,D2
	bhi.s	move_err2
	tst.l	D5
	bne.s	movea01
	cmp.w	#1,d1
	bne.s	norm_move_2
movea01	cmp.w	#1,D1
	bne.s	move_err2	;MOVEA destination must be addr reg
movea02	tst.w	size(A5)
	beq.s	move_err2	;MOVEA size can't be BYTE
norm_move_2
	lsl.w	#6,D1
	lsl.w	#8,D0
	lsl.w	#1,D0
	or.w	D1,D0
	or.w	D3,D0
	move.w	size(A5),D1
	bne.s	not_byte_move
	or.w	#$1000,D0
	bra.s	zump1
not_byte_move
	addq.w	#1,D1
	bchg	#0,D1
	lsl.w	#8,D1
	lsl.w	#4,D1
	or.w	D1,D0
zump1	bra	zzcom

*** MOVEP ***
move_peripheral
	addq.l	#1,A3
	addq.l	#2,Addr(A5)
	bsr	GetSize
	tst.w	size(A5)
	beq.s	move_err3
	bsr	skipspaces
	cmp.b	#'d',(A3)
	bne.s	move_from_peripheral
	bsr	getdreg
	bmi.s	move_err3
	lsl.w	#8,D1
	lsl.w	#1,D1
	move.w	D1,D3
	bsr	SkipComma
	bmi.s	move_err3
	bsr	GetEA
	cmp.w	#5,D1
	bne.s	move_err3
	or.w	D3,D0
	or.w	#$0188,D0
move_per_1
	move.w	size(A5),D1
	subq.w	#1,D1
	lsl.w	#6,D1
	or.w	D1,D0
	bra	zzcom

move_from_peripheral
	bsr	GetEA
	cmp.w	#5,D1
	bne.s	move_err3
	move.w	D0,D3
	bsr	SkipComma
	bmi.s	move_err3
	bsr	getdreg
	bmi.s	move_err3
	move.w	d1,d0
	lsl.w	#8,D0
	lsl.w	#1,D0
	or.w	D3,D0
	or.w	#$0108,D0
	bra.s	move_per_1
move_err3
	bra	error

*** MOVEM ***
movem_asm
	addq.l	#1,A3
	addq.l	#4,Addr(A5)
	bsr	GetSize
	tst.w	size(A5)
	beq.s	move_err3
	bsr	skipspaces
	cmp.b	#'d',(A3)
	beq.s	regs_to_mem
	cmp.b	#'a',(A3)
	beq.s	regs_to_mem
	cmp.b	#'s',(a3)
	bne.s	regs_from_mem
regs_to_mem
	bsr	getreglist
	bmi.s	move_err3
	move.w	D2,D3
	bsr	SkipComma
	bmi.s	move_err3
	bsr	GetEA
	cmp.w	#2,D1
	bcs.s	move_err3
	cmp.w	#3,D1
	beq.s	move_err3
	move.w	D1,D2
	lsl.w	#3,D2
	or.w	D0,D2
	cmp.w	#$39,D2
	bhi.s	move_err3
	cmp.w	#4,D1
	bne.s	regs_to_mem_1
	moveq	#15,D2
juzumps
	lsr.w	#1,D3	;reverse bit order
	roxl.w	#1,D4
	dbf	D2,juzumps
	move.w	D4,D3
regs_to_mem_1
	lsl.w	#3,D1
	or.w	D1,D0
	or.w	#$4880,D0
zumpsis	move.w	size(A5),D1
	subq.w	#1,D1
	lsl.w	#6,D1
	or.w	D1,D0
	move.l	EndAddr(A5),A0
	move.w	D0,(A0)+
	move.w	D3,(A0)
	bra	assem9
move_err4
	bra	error
regs_from_mem
	bsr	GetEA
	cmp.w	#2,D1
	bcs.s	move_err4
	cmp.w	#4,D1
	beq.s	move_err4
	lsl.w	#3,D1
	or.w	D1,D0
	cmp.w	#$3B,D0
	bhi.s	move_err4
	move.w	D0,D3
	bsr	SkipComma
	bmi.s	move_err4
	bsr	getreglist
	bmi.s	move_err4
	move.w	D3,D0
	move.w	D2,D3
	or.w	#$4C80,D0
	bra.s	zumpsis

*** BTST & BCHG & BCLR & BSET ***
btst_asm
	moveq	#0,D3
	bra.s	bit_ins_1
bchg_asm
	moveq	#1,D3
	bra.s	bit_ins_1
bclr_asm
	moveq	#2,D3
	bra.s	bit_ins_1
bset_asm
	moveq	#3,D3

bit_ins_1
	clr.w	size(a5)		for btst Dn,#imm
	addq.l	#2,Addr(A5)
	lsl.w	#6,D3
	bsr	skipspaces
	cmp.b	#'#',(A3)+
	bne.s	bit_reg_mode
	bsr	GetNum
	move.l	Addr(A5),A0
	move.w	D0,(A0)+
	move.l	A0,Addr(A5)
	bset	#11,D3
	bra.s	bit_get_ea
;#
;# btst Dn,#imm is now accepted
;#
bit_reg_mode
	subq.l	#1,a3
	bsr	getdreg
	bmi.s	bits_err
	lsl.w	#8,D1
	lsl.w	#1,D1
	or.w	D1,D3
	bset	#8,D3
bit_get_ea
	bsr	SkipComma
	bmi.s	bits_err
	bsr	GetEA
	cmp.w	#1,D1
	beq.s	bits_err
	lsl.w	#3,D1
	or.w	D1,D0
	move.w	D3,D1
	and.w	#$C0,D1
	beq.s	bit_ea_1
	cmp.w	#$39,D0
	bhi.s	bits_err
bit_ea_1
	cmp.w	#$3c,D0
	bhi.s	bits_err
	or.w	D3,D0
	cmp.w	#$083c,d0
	beq.s	bits_err
	bra	zzcom

bits_err
	bra	error

*** CHK ***
chk_asm	move.w	#$4180,D3
	bra.s	mul_div1

*** MUL & DIV ***
mul_asm	move.w	#$C0C0,D3
	bra.s	mul_div

div_asm	move.w	#$80C0,D3

mul_div	bsr	skipspaces
	cmp.b	#'u',(A3)+
	beq.s	mul_div1
	cmp.b	#'s',-1(A3)
	bne.s	bits_err
	bset	#8,D3

mul_div1
	move.w	#WSIZE,size(A5)
	addq.l	#2,Addr(A5)
	bsr	GetEA
	cmp.w	#1,D1
	beq.s	bits_err
	lsl.w	#3,D1
	or.w	D1,D0
	or.w	D0,D3
	bsr	SkipComma
	bmi.s	bits_err	
	bsr	getdreg
	bmi.s	bits_err
	move.w	d1,d0
	lsl.w	#8,D0
	lsl.w	#1,D0
	or.w	D3,D0
	bra.s	zzcom

*** TAS ***
tas_asm	addq.l	#2,Addr(A5)
	bsr	GetEA
	cmp.w	#1,D1
	beq.s	zz_err
	lsl.w	#3,D1
	or.w	D1,D0
	cmp.w	#$39,D0
	bhi.s	zz_err
	or.w	#$4AC0,D0
	bra.s	zzcom

*** PEA ***
pea_asm	move.w	#$4840,D3

pp_1	addq.l	#2,Addr(A5)
	bsr	GetEA
	cmp.w	#2,D1
	beq.s	pea_A_ok
	cmp.w	#5,D1
	bcs.s	zz_err
pea_A_ok	
	lsl.w	#3,D1
	or.w	D1,D0
	cmp.w	#$3C,D0
	bcc.s	zz_err
	or.w	D3,D0
	bra.s	zzcom

*** LEA ***
lea_asm	addq.l	#2,Addr(A5)
	bsr	GetEA
	cmp.w	#2,D1
	beq.s	lea_A_ok
	cmp.w	#5,D1
	bcs.s	zz_err
lea_A_ok
	lsl.w	#3,D1
	or.w	D0,D1
	cmp.w	#$3C,D1
	bcc.s	zz_err
	move.w	d1,d2
	bsr	SkipComma
	bmi.s	zz_err
	bsr	getareg
	bmi.s	zz_err
	move.w	d1,d0
	lsl.w	#8,D0
	lsl.w	#1,D0
	or.w	d2,d0
	or.w	#$41C0,D0

zzcom	move.l	EndAddr(A5),A0
	move.w	D0,(A0)
	bra	assem9

zz_err	bra	error

*** EXT ***
ext_asm
	bsr	GetSize
	move.w	size(A5),D2
	beq.s	zz_err
	addq.w	#1,D2
	lsl.w	#6,D2
	bsr	skipspaces
;#
;# fixed register checking 1989-08-28
;# (previously accepted address registers)
;#
	bsr	getdreg
	bmi.s	zz_err
	move.w	d1,d0
	or.w	D2,D0
	or.w	#$4800,D0
	bra	one_word_instr

*** JMP ***
jmp_asm	move.w	#$4EC0,D3
	bra	pp_1

*** JSR ***
jsr_asm	move.w	#$4E80,D3
	bra	pp_1

*** NBCD ***
nbcd_asm
	addq.l	#2,Addr(A5)
	clr.w	size(A5)
	move.w	#$4800,D3
	bra.s	one_arg_2

*** CLR ***
clr_asm	move.w	#$4200,D3

one_arg_com
	addq.l	#2,Addr(A5)
	bsr	GetSize

one_arg_2
	bsr	GetEA
	cmp.w	#1,D1		no address register direct mode
	beq.s	qlumps_err
	lsl.w	#3,D1
	or.w	D1,D0
	cmp.w	#$39,D0		check dest addr mode (no pcrel)
	bhi.s	qlumps_err
	move.w	size(A5),D1
	lsl.w	#6,D1
	or.w	D1,D0
	or.w	D3,D0
	move.l	EndAddr(A5),A0
	move.w	D0,(A0)
	bra	assem9

*** NEG & NEGX ***
neg_asm	cmp.b	#'x',(A3)
	bne.s	no_negx
	addq.l	#1,A3
	move.w	#$4000,D3
	bra.s	one_arg_com

qlumps_err
	bra	error

no_negx	move.w	#$4400,D3
	bra.s	one_arg_com

*** NOT ***
not_asm	move.w	#$4600,D3
	bra.s	one_arg_com

*** TST ***
tst_asm	move.w	#$4A00,D3
	bra.s	one_arg_com

*** SWAP ***
swap_asm
	bsr	skipspaces
	bsr	getdreg
	bmi.s	qlumps_err
	move.w	d1,d0
	or.w	#$4840,D0
	bra.s	one_word_instr

*** RESET ***
reset_asm
	move.w	#$4E70,D0
	bra.s	one_word_instr

*** NOP ***
nop_asm	move.w	#$4E71,D0
	bra.s	one_word_instr

*** RTE ***
rte_asm	move.w	#$4E73,D0
	bra.s	one_word_instr

*** RTS ***
rts_asm	move.w	#$4E75,D0
	bra.s	one_word_instr

*** TRAPV ***
trapv_asm
	move.w	#$4E76,D0
	bra.s	one_word_instr

*** RTR ***
rtr_asm	move.w	#$4E77,D0

one_word_instr
	move.l	Addr(A5),A0
	move.w	D0,(A0)+
	move.l	A0,Addr(A5)
	bra	assem9

*** ILLEGAL ***
illegal_asm
	move.w	#ILLEGAL,D0
	bra.s	one_word_instr

*** TRAP ***
trap_asm
	bsr	skipspaces
	cmp.b	#'#',(A3)+
	bne.s	zump_err
	bsr	GetNum
	and.w	#$0f,D0
	or.w	#$4E40,D0
	bra.s	one_word_instr

*** UNLK ***
unlk_asm
	bsr	skipspaces
	bsr	getareg
	bmi.s	zump_err
	move.w	d1,d0
	or.w	#$4E58,D0
	bra.s	one_word_instr

*** LINK ***
link_asm
	bsr	skipspaces
	bsr	getareg
	bmi.s	zump_err
	move.w	d1,d3
	bsr	SkipComma
	bne.s	zump_err
	cmp.b	#'#',(A3)+
	bne.s	zump_err
	bsr	GetNum
	move.w	D0,D1
	move.w	D3,D0
	or.w	#$4E50,D0
	bra.s	two_words_instr

*** STOP ***
stop_asm
	bsr	skipspaces
	cmp.b	#'#',(A3)+
	bne.s	zump_err
	bsr	GetNum
	move.w	D0,D1
	move.w	#$4E72,D0
two_words_instr
	move.l	Addr(A5),A0
	move.w	D0,(A0)+
	move.w	D1,(A0)+
	move.l	A0,Addr(A5)
	bra	assem9

zump_err
	bra	error

*** EXG ***
exg_asm	bsr	getreg
	bmi.s	zump_err
	bsr	SkipComma
	bmi.s	zump_err
	swap	D1
	bsr	getreg
	bmi.s	zump_err
	swap	D1
	btst	#16+3,D1
	beq.s	no_exg_1
	btst	#3,D1
	bne.s	no_exg_1
	swap	D1
no_exg_1
	move.l	D1,D0
	move.l	D1,D3
	lsr.l	#7,D0
	and.w	#$0E00,D0
	and.w	#$07,D1
	or.w	D1,D0
	or.w	#$C100,D0
	move.l	D3,D2
	swap	D2
	eor.w	D3,D2
	btst	#3,D2
	bne.s	data_and_addr
	bset	#6,D0
	btst	#3,D3
	beq.s	exg_9
	bset	#3,D0
	bra.s	exg_9
data_and_addr
	or.w	#$88,D0
exg_9	bra	one_word_instr

*** GET REGISTER LIST (for MOVEM) ***
; result is returned in d2
; the N-flag is set on error
getreglist
	clr.w	D2
reglist1
	bsr.s	getreg
	bmi.s	reglist9
	bset	D1,D2
	cmp.b	#'/',(A3)+
	beq.s	reglist1
	cmp.b	#'-',-1(A3)
	bne.s	reglist9
	move.w	D1,D7
	bsr.s	getreg
	bmi	return_error
	move.w	D1,D0
	eor.w	D7,D0
	btst	#3,D0
	bne	return_error	;register in range are different types
	cmp.w	D1,D7
	bls.s	regr_01
	exg	d1,d7		;register range out of order
	bra.s	regrange
regr_01	beq.s	reg_r1
regrange
	bset	D7,D2
	addq.w	#1,D7
	cmp.w	D1,D7
	bls.s	regrange
reg_r1	cmp.b	#'/',(A3)+
	beq.s	reglist1
reglist9
	subq.l	#1,A3
	moveq	#0,D0
	rts			;register mask returned in D2

*** GET ONE REGISTER, number in D1, 0=D0..15=sp ***
; set N-flag on error
getreg	clr.w	D1
	bsr	skipspaces
	cmp.b	#'s',(a3)
	beq.s	check_sp
	cmp.b	#'d',(A3)+
	beq.s	g_r_1
	cmp.b	#'a',-1(A3)
	bne.s	return_error
	bset	#3,D1
g_r_1	move.b	(A3)+,D0
	sub.b	#'0',D0
	bcs.s	return_error
	cmp.b	#8,D0
	bcc.s	return_error
	ext.w	D0
	or.b	D0,D1
g_r_2	rts	;register returned in D1

check_sp
	cmp.b	#'p',1(a3)
	bne.s	return_error
	moveq	#$0f,d1
	addq.l	#2,a3
	rts
;
; get address register, return in d1, set N-flag on error
;
getareg	bsr	getreg
	bmi.s	01$
	bclr	#3,d1
	beq.s	return_error
01$	rts

;
; get data register, return in d1, set N-flag on error
;
getdreg	bsr	getreg
	bmi.s	01$
	btst	#3,d1
	bne.s	return_error
01$	rts

*** Get a number in range 1..8, used by 'quick' instructions and shifts ***
* return -1 if error
GetNum_1_8
	bsr	GetNum
	swap	D0
	tst.w	D0
	bne.s	return_error
	swap	D0
	tst.w	D0
	beq.s	return_error
	cmp.w	#8,D0
	bhi.s	return_error
	and.w	#7,D0
	rts

*** SKIP COMMA & SPACES AROUND IT ***
* if first non-space character is not a comma, return -1 else return 0
SkipComma
	bsr	skipspaces
	cmp.b	#',',(A3)+
	beq.s	S_K_1
return_error
	moveq	#-1,D0
	rts
S_K_1	bsr	skipspaces
	moveq	#0,D0
	rts

GetEA: ;get effective address, mode=D1,reg=D0
* put displacements etc. in memory at address pointed by Addr(A5)
* and increment Addr(A5)
	bsr	skipspaces
	move.l	a3,-(sp)
	bsr	getreg
	bmi.s	no_reg_direct
	addq.l	#4,sp
	move.w	d1,d0
	moveq	#0,d1
	bclr	#3,d0
	beq.s	reg_09
	moveq	#1,d1
reg_09	rts
no_reg_direct
	move.l	(sp)+,a3
	cmp.b	#'(',(a3)
	bne.s	no_indirect_or_postincrement
	addq.l	#1,A3
	bsr	getareg
	bmi	ea_error
	move.w	d1,d0
	cmp.b	#')',(A3)+
	bne	ea_error
	cmp.b	#'+',(A3)
	beq.s	postincrement
	moveq	#2,D1
	rts
postincrement
	addq.l	#1,A3
	moveq	#3,D1
	rts
no_indirect_or_postincrement
	cmp.b	#'-',(a3)
	bne.s	no_predecrement
	cmp.b	#'(',1(A3)
	bne.s	no_predecrement
	addq.l	#2,A3
	bsr	getareg
	bmi	ea_error
	move.w	d1,d0
	cmp.b	#')',(A3)+
	bne	ea_error
	moveq	#4,D1
	rts
no_predecrement
	cmp.b	#'#',(a3)
	bne.s	no_immediate
	addq.l	#1,A3

PutImm:	bsr	GetNum
	move.l	Addr(A5),A0
	move.w	size(A5),D1
	bne.s	sz1
	and.w	#$FF,D0
	move.w	D0,(A0)+
	bra.s	sz9
sz1	cmp.w	#1,D1
	bne.s	sz2
	move.w	D0,(A0)+
	bra.s	sz9
sz2	move.l	D0,(A0)+
sz9	move.l	A0,Addr(A5)
	moveq	#7,D1
	moveq	#4,D0
	rts

no_immediate
	bsr	GetNum
	move.l	D0,D2
	cmp.b	#'(',(a3)
	bne	absolute_mode
	addq.l	#1,A3
	move.l	a3,-(sp)
	bsr	getareg
	bmi.s	no_displacement_or_index
	addq.l	#4,sp
	move.w	d1,d0
	cmp.b	#',',(A3)+
	beq.s	indirect_with_index
	cmp.b	#')',-1(A3)
	bne	ea_error
	move.l	Addr(A5),A0
	move.w	D2,(A0)+
	move.l	A0,Addr(A5)
	moveq	#5,D1
	rts

indirect_with_index
	move.l	D0,D7
	bsr	GetIndex
	move.l	D7,D0
	moveq	#6,D1
	rts

no_displacement_or_index
	move.l	(sp)+,a3
	cmp.b	#'p',(a3)
	bne.s	pcrel_indx_2
	addq.l	#1,a3
	cmp.b	#'c',(A3)+
	bne.s	ea_error
	cmp.b	#')',(A3)+
	bne.s	pcrel_indx_1
	sub.l	Addr(A5),D2
	move.l	Addr(A5),A0
	move.w	D2,(A0)+
	move.l	A0,Addr(A5)
	move.w	d2,d0
	ext.l	d0
	cmp.l	d0,d2
	beq.s	pcrel_ok

ea_out_of_range
	addq.l	#4,sp
	bra	out_of_range

pcrel_ok
	moveq	#7,D1
	moveq	#2,D0
	rts
;
; pc-relative indexed syntax can now be in the correct format '(pc,Rn.w)'
;
pcrel_indx_1
	cmp.b	#',',-1(a3)
	bne.s	ea_error
pcrel_indx_2
	sub.l	Addr(A5),D2
	move.b	d2,d0
	ext.w	d0
	ext.l	d0
	cmp.l	d0,d2
	bne.s	ea_out_of_range
	bsr.s	GetIndex
	moveq	#7,D1
	moveq	#3,D0
	rts

absolute_mode
	move.w	D2,D1
	ext.l	D1
	cmp.l	D2,D1
	beq.s	abs_short
	move.l	Addr(A5),A0
	move.l	D2,(A0)+
	move.l	A0,Addr(A5)
	moveq	#7,D1
	moveq	#1,D0
	rts
abs_short
	move.l	Addr(A5),A0
	move.w	D2,(A0)+
	move.l	A0,Addr(A5)
	moveq	#7,D1
	moveq	#0,D0
	rts
ea_error
	addq.l	#4,sp
	bra	error

*** GET SIZE (put it in size(A5), 0=B, 1=W, 2=L) ***
GetSize	cmp.b	#'.',(A3)+
	bne.s	ea_error
	move.b	(A3)+,D0
	cmp.b	#'b',D0
	bne.s	siz1
	moveq	#BSIZE,d0
	bra.s	siz3
siz1	cmp.b	#'w',D0
	bne.s	siz2
	moveq	#WSIZE,d0
	bra.s	siz3
siz2	cmp.b	#'l',D0
	bne.s	ea_error
	moveq	#LSIZE,d0
siz3	move.w	d0,size(a5)
	rts

GetIndex	;displacement value in d2
	bsr	getreg
	bmi.s	index_error
	moveq	#0,d0
	bclr	#3,d1
	beq.s	01$
	bset	#15,d0
01$	lsl.w	#8,d1
	lsl.w	#4,d1
	or.w	d0,d1
	and.w	#$ff,d2
	or.w	d2,d1
	cmp.b	#'.',(A3)+
	bne.s	index_error
	cmp.b	#'w',(A3)+
	beq.s	index_2
	cmp.b	#'l',-1(A3)
	bne.s	index_error
	bset	#11,D1
index_2	cmp.b	#')',(A3)+
	bne.s	index_error
	move.l	Addr(A5),A0
	move.w	D1,(A0)+
	move.l	A0,Addr(A5)
	rts
index_error
	addq.l	#8,sp
	bra	error

*** DISPLAY & CHANGE REGISTERS ***
regs	bsr	skipspaces
	tst.b	(A3)
	bne.s	changeregs
displayregs_d
	bsr	displayregs
	move.l	regPC,D0
	beq.s	rr9
	btst	#0,D0
	bne.s	rr9
	move.l	D0,A4
	tst.w	(a4)
	beq.s	rr9
	bsr	disasmline
	tst.w	d0
	bne.s	rr9
	bsr	printstring
rr9	bra.s	regs_mainloop_jmp

skipequal ;the syntax of the register change command can include a '=' sign
	cmp.b	#'=',(A3)	;but it is not necessary
	bne.s	01$
	addq.l	#1,A3
01$	rts

changeregs
	move.b	(A3)+,D0	;** CHANGE REGISTERS **
	bsr	tolower
	lsl.w	#8,D0
	move.b	(A3)+,D0	;get possibly unaligned word
	bsr	tolower
	cmp.w	#'pc',D0	;program counter
	bne.s	nopc
	bsr.s	skipequal
	bsr	GetNum
	move.l	D0,regPC
	bra.s	regs_mainloop_jmp

nopc	cmp.w	#'cc',D0	;condition code register
	bne.s	nocc
	bsr.s	skipequal
	bsr	GetNum
	move.b	D0,regCC
regs_mainloop_jmp
	bra	mainloop

nocc	cmp.w	#'d0',D0
	bcs.s	nodr
	cmp.w	#'d7',D0
	bhi.s	nodr
	sub.w	#'d0',D0
	lsl.w	#2,D0
	move.w	D0,D2
	bsr	skipequal
	bsr	GetNum
	lea	DataRegs,A0
	move.l	D0,0(A0,D2.W)
	bra.s	regs_mainloop_jmp

nodr	cmp.w	#'sp',d0
	bne.s	nosp
	moveq	#7,d0
	bra.s	setareg
nosp	cmp.w	#'a0',D0
	bcs	error
	cmp.w	#'a7',D0
	bhi	error
	sub.w	#'a0',D0
setareg	lsl.w	#2,D0
	move.w	D0,D2
	bsr	skipequal
	bsr	GetNum
	lea	AddrRegs,A0
	move.l	D0,0(A0,D2.W)
	bra.s	regs_mainloop_jmp

*** DISPLAY REGISTERS ***
displayregs
	startline
	move.l	#' PC=',(A3)+
	move.l	regPC,D0
	bsr	phex1_8
	move.l	#' CC=',(A3)+
	move.b	regCC,D0
	move.b	D0,D2
	moveq	#2,d1
	bsr	put_hexnum1
	moveq	#4,D1
flagloop
	putchr	SPACE
;
; hmm..it seems that a68k does not handle the pc-relative indexed addressing
; mode if you do not specify the "(pc,". this is also a problem with
; the monitor assembler...*now monitor assembler also understands (pc,indx)*
;
	move.b	flagstring(pc,D1.L),(A3)+
	putchr	<'='>
	moveq	#'0',D0
	btst	D1,D2
	beq.s	flag1
	addq.b	#1,D0
flag1	move.b	D0,(A3)+
	subq.l	#1,D1
	bpl.s	flagloop
	endline
	bsr	printstring
	lea	DataRegs,A2
	move.l	#' D0=',D2
	bsr.s	PrintRegLine
	bsr.s	PrintRegLine
	move.l	#' A0=',D2
	bsr.s	PrintRegLine
	nop
PrintRegLine
	startline
	moveq	#4-1,D3
regl1	move.l	D2,(A3)+
	move.l	(A2)+,D0
	bsr	phex1_8
	add.w	#$0100,D2
	dbf	D3,regl1
	endline
	bra	printstring

flagstring
	dc.b	'CVZNX'
	even

*** BREAKPOINTS ***
breaks	bsr	skipspaces
	move.b	(A3)+,D0
	bsr	tolower
	cmp.b	#'l',D0
	beq	break_list
	cmp.b	#'r',D0
	beq.s	break_remove

*** SET BREAKPOINT ***
	subq.l	#1,A3
	bsr	GetNum
	tst.l	D0
	beq.s	brk_err
	btst	#0,D0
	bne.s	brk_err
	move.l	D0,A4
	bsr	find_break
	bpl.s	brk_err
	move.l	A1,D3
	moveq	#brk_SIZE,D0
	move.l	#MEMF_CLEAR,D1
	callexe	AllocMem
	tst.l	D0
	beq.s	brk_err
	move.l	D0,A0
	move.l	A4,brk_Address(A0)
	tst.l	D3
	bne.s	no_start_of_list
	move.l	BreakList(A5),(A0)
	move.l	A0,BreakList(A5)
	bra.s	brset9
no_start_of_list
	move.l	D3,A1
	move.l	(A1),(A0)
	move.l	A0,(A1)
brset9	bra	mainloop
brk_err	bra	error

*** REMOVE BREAKPOINT ***
break_remove
	bsr	skipspaces
	cmp.b	#'a',(A3)	;check 'all'
	bne.s	break_rem1
	moveq	#'l',D0
	cmp.b	1(A3),D0
	bne.s	break_rem1
	cmp.b	2(A3),D0
	bne.s	break_rem1
	bsr.s	remove_all_breaks
	bra.s	brset9
break_rem1
	bsr	GetNum
	bsr.s	find_break
	bmi.s	brk_err
	move.l	A1,D0
	bne.s	no_remove_from_start_of_list
	move.l	(A0),BreakList(A5)
	bra.s	break_remove_1
no_remove_from_start_of_list
	move.l	(A0),(A1)
break_remove_1
	move.l	A0,A1
	moveq	#brk_SIZE,D0
	callexe	FreeMem
	bra.s	brset9

*** LIST BREAKPOINTS ***
* note: the list is automatically in order
break_list
	move.l	BreakList(A5),D2
	bne.s	break_list_1
	lea	noBrkTxt(pc),A0
	bsr	printstring_a0_window
	bra	mainloop
break_list_1
	lea	brklistTx(pc),A0
	bsr	printstring_a0
break_list_loop
	tst.l	D2
	beq.s	brset9
	move.l	D2,A4
	move.l	brk_Address(A4),D0
	lea	hexfmt(pc),a0
	bsr	printf
	bsr	CheckKeys
	bne	brset9
	move.l	(A4),D2
	bra.s	break_list_loop

*** FIND BREAKPOINT FROM LINKED LIST ***
* address in D0
* if N=1 then not found, A1 points to where new node should be inserted
*  if A1=0 then insert to start of the list
* if N=0 then found, A0 points to the node, A1 points to predecessor
find_break
	sub.l	A1,A1	;A1=0
	move.l	BreakList(A5),D1
find_br_1
	beq.s	break_not_found
	move.l	D1,A0
	cmp.l	brk_Address(A0),D0
	beq.s	break_found
	bcs.s	break_not_found
	move.l	A0,A1
	move.l	(A0),D1
	bra.s	find_br_1
break_not_found
	moveq	#-1,D0
	rts
break_found
	moveq	#0,D0
brk_ret	rts

remove_all_breaks
* executed before exit of when the 'br all' command is given
; this changes d2 and a6
	move.l	BreakList(A5),D2
all_breaks_loop
	tst.l	D2
	beq.s	brk_ret2
	move.l	D2,A1
	move.l	(A1),D2
	moveq	#brk_SIZE,D0
	callexe	FreeMem
	bra.s	all_breaks_loop
brk_ret2
	clr.l	BreakList(A5)
	rts

*** PUT THE ILLEGAL ($4AFC) INSTRUCTION TO BREAKPOINTS ***
* (first save original contents in the BreakPoint structure)
;
; don't set a breakpoint if the current PC is pointing to it
; but save the address of the breakpoint structure so the breakpoint
; can be activated from the trap handler...
;
SetBreaks
	or.w	#1,flags(A5)
	move.l	BreakList(A5),D2
SetBr1	tst.l	D2
	beq.s	brk_ret
	move.l	D2,A1
	move.l	brk_Address(A1),A0
	cmp.l	regPC,a0
	bne.s	01$
	move.l	a1,GoBrkPtr
	bra.s	SetBr2
01$	move.w	(A0),brk_Content(A1)
	move.w	#ILLEGAL,(A0)
SetBr2	move.l	(A1),D2
	bra.s	SetBr1

*** RESTORE ORIGINAL CONTENTS OF BREAKPOINTS ***
RemBreaks
	bclr	#0,flags+1(a5)
	beq.s	brk_ret
	move.l	BreakList(A5),D2
RemBr1	tst.l	D2
	beq.s	brk_ret
	move.l	D2,A1
	move.l	brk_Address(A1),A0
	move.w	brk_Content(A1),(A0)
	clr.w	brk_Content(a1)		this helps debugging...
RemBr2	move.l	(A1),D2
	bra.s	RemBr1

**** SINGLE STEP (WALK) ****
* NOTE: This ignores breakpoints
walk	bsr.s	getpc
	movem.l	A5,-(sp)
	move.l	sp,StackPtr
	move.l	AddrRegs+7*4,sp
	or	#2,CCR		;set overflow flag
	trapv			;let the trap handler do the rest...
walk_here	;a label so we can reference it in the handler routine

getpc	bsr	skipspaces
	tst.b	(a3)
	beq.s	01$
	bsr	GetNum
	move.l	d0,regPC
01$	rts

**** EXECUTE MACHINE CODE (GO) ****
go	bsr	getpc
	bsr	SetBreaks
	movem.l	A5,-(sp)
	move.l	sp,StackPtr
	move.l	AddrRegs+7*4,sp
	bra.s	go_com

**** JUMP TO SUBROUTINE (RETURN WITH RTS) ***
jumpsr	bsr	getpc
	bsr	SetBreaks
	move.l	sp,A0	;set stack ptr
	sub.w	#$100,A0
	move.l	a0,d0
	and.b	#$fc,d0		;long word align
	move.l	d0,AddrRegs+4*7
	movem.l	A5,-(sp) 	;save frame pointer
	move.l	sp,StackPtr
	move.l	A0,sp
	lea	returncode(pc),A0
	move.l	A0,(sp)		;put return address in stack

go_com	move.l	regPC,d0
	bsr	find_break
	bpl.s	go_special

	move.l	regPC,-(sp)
	move.b	regCC,D0
	move	D0,CCR
	movem.l	DataRegs,D0-D7/A0-A6
	rts	;this really jumps to the user program

go_special
	or	#2,CCR		;set overflow flag
	trapv			;let the trap handler do the rest...
special_go_here

*** CONTROLS RETURNS HERE AFTER THE Jsr COMMAND ***
returncode
	addq.l	#4,sp			this does not change the flags
	movem.l	D0-D7/A0-A7,DataRegs	nor does this...
	move.l	StackPtr,sp		or this...
; the movea-instruction that is a part of the callexe macro does not
; change the flags either...
	callexe	GetCC			get the flags and save them
	move.b	D0,regCC
	move.l	(sp)+,A5	;restore frame pointer
	bsr	RemBreaks
	lea	rettx(pc),A0
	bsr	printstring_a0
	bra	displayregs_d

*** TASK TRAP CODE ***
trapreturn	;Note! We are in supervisor mode!
	cmp.l	#9,(sp)		trace?
	bne.s	noskipbrk
	bclr	#0,SkipBreakFlag
	beq.s	noskipbrk
;
; we have just skipped a breakpoint in trace mode
;
	addq.l	#4,sp			remove exception number from stack
	bclr	#7,(sp)			clear trace bit
	movem.l	a0/a1,-(sp)
	move.l	GoBrkPtr,a1
	move.l	brk_Address(a1),a0	activate breakpoint
	move.w	(a0),brk_Content(a1)
	move.w	#ILLEGAL,(a0)
	movem.l	(sp)+,a0/a1
	rte				continue at full speed

noskipbrk
	cmp.l	#7,(sp)	;is this a TRAPV-trap (possibly by the walk routine)
	bne.s	normtrap
	cmp.l	#walk_here,6(sp)	;check program counter
	beq.s	walk_trap
	cmp.l	#special_go_here,6(sp)
	bne.s	normtrap
; this is the special go-routine if the address we are going to enter
; the code contains a breakpoint. in that case we trace over the
; instruction and then continue at full speed
	bset	#0,SkipBreakFlag
; fall to walk trap routine
walk_trap
	lea	10(sp),sp	;yes, it was the walk routine, so clean up the stack
	move.l	regPC,-(sp)
	move	SR,D0		;this is legal bacause supervisor mode
	move.b	regCC,D0
	bclr	#13,D0		;supervisor mode bit off
	bset	#15,D0		;trace mode bit on
	move.w	D0,-(sp)
	movem.l	DataRegs,D0-D7/A0-A6
	rte	;back to user mode and user program...

normtrap
	movem.l	D0-D7/A0-A6,DataRegs
	move	usp,A0
	move.l	A0,AddrRegs+7*4	;save stack pointer
	move.l	_ExecBase,A6
	move.w	AttnFlags(A6),D1
	move.l	(sp)+,D5
	cmp.w	#3,D5
	bhi.s	pop_SR_and_PC	;jump if not bus error or address error
	btst	#AFB_68010,D1	;we must check the type of the processor!
	bne.s	pop_SR_and_PC
	addq.l	#8,sp
pop_SR_and_PC
	move.w	(sp)+,D0	;status register
	move.b	D0,regCC
	move.l	(sp)+,a4
	cmp.w	#ILLEGAL,(a4)
	seq	d7
	move.l	a4,regPC

	cmp.w	#3,D5
	bls.s	bus_or_addr
	btst	#AFB_68010,d1
	beq.s	go_user_mode
;
; drop the exception number left in the supervisor stack by the
; 68010/68020 processors
;
	addq.l	#2,sp
	bra.s	go_user_mode

bus_or_addr
	btst	#AFB_68020,D1	clean up stack info left by
	beq.s	no68020		bus or address errors with
	lea	82(sp),sp	68010 or 68020/30 processors
	bra.s	go_user_mode
no68020	btst	#AFB_68010,D1
	beq.s	go_user_mode	
	lea	52(sp),sp

go_user_mode
	and.w	#$5FFF,D0	;clear supervisor & trace bits
	move	D0,SR		;back to the user mode!
	move.l	StackPtr,sp	;this is user stack pointer
;
; if SkipBreakFlag is set when we enter here, some other exception has
; occurred before the trace-exception that should have cleared the
; flag and activated the breakpoint. in that case we must activate the
; breakpoint here (RemBreaks tries to remove it anyway, and that could
; cause problems...)
;
	bclr	#0,SkipBreakFlag
	beq.s	01$
	move.l	GoBrkPtr,a1		activate breakpoint
	move.l	brk_Address(a1),a3
	move.w	(a3),brk_Content(a1)
	move.w	#ILLEGAL,(a3)
01$	movem.l	(sp)+,A5	;restore frame pointer
	btst	#0,flags+1(a5)
	bne.s	02$
	clr.b	d7
02$	bsr	RemBreaks
	move.l	regPC,D0
	cmp.w	#4,D5
	bne.s	normal_trap
	bsr	find_break
	bne.s	normal_trap
	tst.b	d7
	beq.s	normal_trap
	lea	brkPtTxt(pc),A0
	bsr	printstring_a0
	bra.s	trap_dregs
normal_trap
	bsr.s	show_trap_name
trap_dregs
	bsr	displayregs
	move.l	regPC,D0
	btst	#0,D0
	bne.s	tr99
	move.l	D0,Addr(A5)
	move.l	D0,A4
	bsr	disasmline
	bsr	printstring
tr99	bra	mainloop

show_trap_name	;trap number in D5
	startline
	move.l	#'*** ',(A3)+
	lea	trapnamtabl(pc),A1
	move.w	D5,D0
	cmp.w	#$30,D0
	bcc.s	unknown_trap
	cmp.w	#$20,D0
	bcc.s	traps
	subq.w	#2,D0
	cmp.w	#10,D0
	bcs.s	txout1
unknown_trap
	lea	errtx(pc),a1
	bsr	putstring
	bra.s	notraps
traps	moveq	#10,D0
txout1	add.w	D0,D0
	add.w	D0,A1
	add.w	(A1),A1
	bsr	putstring
	cmp.w	#$20,D5
	bcs.s	notraps
	move.w	D5,D0
	sub.w	#$20,D0
	moveq	#2,d1
	bsr	put_hexnum
notraps	putchr	SPACE
	moveq	#'*',D0
	move.b	D0,(A3)+
	move.b	D0,(A3)+
	move.b	D0,(A3)+
	endline
	bra.s	printstring

*** SHOW TRAP NAME (USAGE: ^ num) ***
showtrap
	bsr	GetNum
	move.l	D0,D5
	bsr	show_trap_name
	bra	mainloop

*** enter command line ***
cmdline	bsr	skipspaces
	tst.b	(a3)
	bne.s	01$
	lea	cmdlinetxt(pc),a0
	bsr.s	printstring_a0_window
	moveq	#0,d0
	bsr	GetInput
01$	lea	cmdlinebuf,a1
	move.l	a1,a2
02$	move.b	(a3)+,(a1)+
	bne.s	02$
	move.b	#LF,-1(a1)
	clr.b	(a1)
	sub.l	a2,a1
	lea	DataRegs,a0
	move.l	a1,(a0)
	move.l	a2,4*8(a0)
	bra	mainloop

printf	bsr.s	fmtstring
; fall to printstring
printstring
	move.l	outbufadr(a5),a0
; fall to printstring_a0
*** Output a string (possibly redirected output) ***
printstring_a0
	move.l	OutputFile(a5),d1
;
; print text, line at time
; inputs: a0 - pointer to zero-terminated string
; 	  d1 - filehandle
;
print_text
	movem.l	d2-d4/a2/a6,-(sp)
	move.l	d1,d4
	move.l	dosbase(a5),a6
	move.l	a0,a2
print_loop
	move.l	a2,d2
000$	move.b	(a2)+,d0
	beq.s	001$
	cmp.b	#LF,d0
	bne.s	000$
001$	move.l	a2,d3
	sub.l	d2,d3
	move.l	d4,d1
	tst.b	d0
	bne.s	002$
	subq.l	#1,d3
	beq.s	003$
002$	callsys	Write
	tst.b	-1(a2)
	bne.s	print_loop
003$	movem.l	(sp)+,d2-d4/a2/a6
	rts

printf_window
	bsr.s	fmtstring
printstring_window
	move.l	outbufadr(a5),a0
*** Output a string to the window ***
* used in error messages etc.
printstring_a0_window
	move.l	winfile(a5),d1
	bra.s	print_text

;
; format a string in output buffer using RawDoFmt
; arguments in registers d0-d3
;
fmtstring
	movem.l	a2-a3/a6,-(sp)
	movem.l	d0-d3,-(sp)
	move.l	sp,a1
	lea	putch(pc),a2
	move.l	outbufadr(a5),a3
	callexe	RawDoFmt
	lea	16(sp),sp
	movem.l	(sp)+,a2-a3/a6
	rts

; character output routine for RawDoFmt
putch	move.b	d0,(a3)+
	rts

**** Output a character to the window ****
ChrOut	movem.l	D2-D3/a6,-(sp)
	move.l	winfile(A5),D1
	move.b	d0,-(sp)
	move.l	sp,d2
	moveq	#1,D3
	calldos	Write
	addq.l	#2,sp
	movem.l	(sp)+,D2-D3/a6
	rts

**** Get a character ****
GetChar	movem.l	D2-D3/a6,-(sp)
	move.l	winfile(A5),D1
	clr.b	-(sp)
	move.l	sp,d2
	moveq	#1,d3
	calldos	Read
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
	cmp.b	#CSI,D0
	bne.s	ret9
	bsr.s	GetChar
	cmp.b	#'A',D0
	bcs.s	key1
	cmp.b	#'D',D0
	bhi.s	key1
	sub.b	#$40,D0
	asl.w	#8,D0
	rts	;codes $0100..$0400	;cursor up/down/right/left
key1	cmp.b	#'S',D0
	bne.s	key2
	move.w	#SHIFT_CURSOR_DOWN,D0
	rts
key2	cmp.b	#'T',D0
	bne.s	key3
	move.w	#SHIFT_CURSOR_UP,D0
	rts
key3	cmp.b	#' ',D0
	bne.s	key8
	bsr.s	GetChar
	cmp.b	#'A',D0
	bne.s	key4
	move.w	#SHIFT_CURSOR_LEFT,D0
	rts
key4	cmp.b	#'@',D0
	bne.s	key9
	move.w	#SHIFT_CURSOR_RIGHT,D0
	rts
key8	cmp.b	#'?',D0
	bne.s	key9
	bsr	GetChar
	move.w	#'??',D0	;THE HELP KEY
	rts
key9	cmp.b	#'~',D0
	beq.s	key99
	bsr	GetChar
	bra.s	key9
key99	moveq	#-1,D0	;unknown key
ret9	rts

**** THE INPUT ROUTINE ****
* special code in D0
* 0 = normal operation
* 1 = respond ctrl-e (assembler)
* 2 = edit existing line (created by disassembler)
* returns the special code or -1 if Ctrl-E pressed
*********
; this may modify any register...
GetInput
	move.w	D0,inpspecial(A5)
	moveq	#0,D4
	move.l	outbufadr(A5),A4
	lea	(InpBuf-OutBuf)(A4),A4
	moveq	#0,D5
	moveq	#0,D7	;length
	cmp.w	#2,D0
	bne.s	inp0
	lea	ClearEol(pc),a0		erase to end of line
	bsr	printstring_a0_window
	move.l	A4,A0
	bsr	printstring_a0_window
	move.l	A4,A0
inp0len	tst.b	(A0)+
	bne.s	inp0len
	sub.l	A4,A0
	subq.l	#1,A0
	move.l	A0,D7
inp0	move.l	D7,D6	;current position
inp1	bsr	GetKey
	cmp.w	#CR,D0	;return
	beq	inp9
	cmp.w	#CtrlE,D0
	bne.s	noCtrlE
	tst.w	inpspecial(A5)
	beq.s	noCtrlE
	bsr	eraseline
	move.w	#-1,inpspecial(A5)
	bra	inp9

noCtrlE	cmp.w	#CtrlX,D0	;Ctrl-x clears the input line
	bne.s	noCtrlX
	bsr	eraseline
	bra.s	inp1

noCtrlX	cmp.w	#CURSOR_RIGHT,D0
	beq	moveright
	cmp.w	#CURSOR_LEFT,D0	;cursor left
	beq	moveleft
	cmp.w	#SHIFT_CURSOR_LEFT,D0
	bne.s	noleftedge
	bsr	gotoleftedge
	bra.s	inp1

noleftedge
	cmp.w	#'??',D0
	bne.s	nohelp
	moveq	#-1,D5
	bra.s	put_char_to_input

nohelp	cmp.w	#SHIFT_CURSOR_UP,D0
	bne.s	no_do_prev
	moveq	#-1,D5
	bra	previousline
no_do_prev	
	cmp.w	#SHIFT_CURSOR_RIGHT,D0
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
	cmp.w	#-1,D0
	beq	inp1

put_char_to_input
	cmp.w	#64,D7		is input line full?
	bcc	inp1
	cmp.l	D7,D6
	bhi	inp1
	beq.s	iputchr
	move.l	D7,D2

; make room for a new character
080$	move.b	-1(A4,D2.L),0(A4,D2.L)
	subq.l	#1,D2
	cmp.l	D2,D6
	bne.s	080$

iputchr	move.b	D0,D2
	lea	insch(pc),a0
	bsr	printstring_a0_window
	move.b	D2,D0
	bsr	ChrOut
	move.b	D2,0(A4,D6.L)
	addq.l	#1,D6
	addq.l	#1,D7
	tst.l	D5	;auto-CR flag
	beq	inp1

inp9	clr.b	0(A4,D7.L)
	move.l	A4,A3
	bsr	skipspaces
	tst.b	(A3)
	beq.s	inp99

	lea	history,A0
	move.w	#(NLINES-1)*LEN/4-1,D0
099$	move.l	LEN(A0),(A0)+
	dbf	D0,099$		;scroll command line history to make space for current line

100$	move.b	(A4)+,(a0)+	;add current line to command line history
	bne.s	100$

inp99	tst.w	inpspecial(A5)
	bmi.s	inp99a
	emit	LF
inp99a	move.w	inpspecial(A5),D0
	rts
rightedge	;Shift-Cursor right
	cmp.l	D6,D7
	beq	inp1
	moveq	#0,d0
	move.b	D7,D0
	sub.b	D6,D0
	move.l	D7,D6
right01	lea	go_right_fmt(pc),a0
	bsr	printf_window
	bra	inp1

moveleft
	tst.l	D6
	beq	inp1
	subq.l	#1,D6
	emit	BS
	bra	inp1
moveright
	cmp.l	D6,D7
	beq	inp1
	addq.l	#1,D6
	moveq	#0,d0
	bra.s	right01
backspace
	tst.l	D6
	beq	inp1
	subq.l	#1,D6
	emit	BS
delchar	cmp.l	D6,D7
	beq	inp1
	lea	delch(pc),a0
	bsr	printstring_a0_window
	subq.l	#1,D7
	cmp.l	D6,D7
	beq	inp1
	move.l	D6,D0

105$	move.b	1(A4,D0.L),0(A4,D0.L)
	addq.l	#1,D0
	cmp.l	D0,D7
	bne.s	105$

	bra	inp1

*** COMMAND LINE HISTORY ***
nextline
	bsr.s	eraseline
	moveq	#1,D1
	bra.s	prnxl1
previousline
	bsr.s	eraseline
	moveq	#-1,D1
prnxl1	add.b	D1,D4
	bmi.s	linct_neg
	cmp.b	#NLINES,D4
	bcs.s	linct_ok
	moveq	#0,D4
	bra.s	linct_ok
linct_neg
	move.b	#NLINES-1,D4
linct_ok
	move.b	D4,D0
	lea	history,A0
	ext.w	D0
	mulu	#LEN,D0
	add.l	D0,A0
prevloop
	move.b	(A0)+,D0
	beq.s	prev2
	move.b	D0,0(A4,D7.L)
	addq.l	#1,D7
	bra.s	prevloop
prev2	clr.b	0(A4,D7.L)
	move.l	A4,A0
	bsr	printstring_a0_window
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
	bsr	printf_window
lef9	moveq	#0,D6
	rts
eraseline
	bsr.s	gotoleftedge
	moveq	#0,D7
	lea	ClearEol(pc),a0
	bra	printstring_a0_window

*** HEX OUTPUT ROUTINES ***
;
; output a 6 or 8 digit hex number (6 digits if high byte is zero)
; added checking of 60 or 80 column default font
;
puthex1_68
	tst.w	flags(a5)	test 60col flag
	bmi.s	phex1_68
	bra.s	phex1_8
puthex_68
	putchr	'$'
phex1_68
	move.l	d0,d1
	swap	d1
	and.w	#$ff00,d1
	bne.s	phex1_8
	moveq	#6,d1
	bra.s	put_hexnum1
phex1_8	moveq	#8,d1
	bra.s	put_hexnum1
;
; put a signed hexnum in buffer pointed by a3
;
put_signed_hexnum
	tst.l	d0
	bpl.s	shex1
	neg.l	d0
	putchr	<'-'>
shex1	moveq	#-2,d1
	; fall to hexnum
;
; put a hex number in buffer pointed by a3
; d0: number, d1: # of digits
;
put_hexnum
	putchr	<'$'>
put_hexnum1
	movem.l	d2-d3,-(sp)
	tst.l	d1
	bpl.s	00$
	neg.w	d1
00$	subq.w	#1,d1
	moveq	#-1,d2
01$	move.b	d0,d3
	lsr.l	#4,d0
	and.b	#$0f,d3
	cmp.b	#10,d3
	bcs.s	02$
	add.b	#'A'-'0'-10,d3
02$	add.b	#'0',d3
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
100$	dbf	d1,01$
03$	move.b	(sp)+,(a3)+
	dbf	d2,03$
	movem.l	(sp)+,d2-d3
	rts

*** PUT LONGWORD (to possibly odd address) ***
PutLong	swap	D0
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

*** CREATE MESSAGE PORT ***
* no name, priority 0
MyCreatePort
	movem.l	D2/A6,-(sp)
	moveq	#-1,D0
	callexe	AllocSignal
	moveq	#-1,D1
	cmp.l	D0,D1
	beq.s	CrepFail
	move.l	D0,D2
	moveq	#MP_SIZE,D0
	move.l	#MEMF_PUBLIC!MEMF_CLEAR,D1
	callsys	AllocMem
	tst.l	D0
	bne.s	Crep1
	move.b	D2,D0
	callsys	FreeSignal
	bra.s	CrepFail
Crep1	move.l	D0,-(sp)
	sub.l	A1,A1
	callsys	FindTask
	move.l	D0,A1
	move.l	(sp)+,D0
	move.l	D0,A0
	move.l	A1,MP_SIGTASK(A0)
	move.b	D2,MP_SIGBIT(A0)
	move.b	#NT_MSGPORT,LN_TYPE(A0)
	move.b	#PA_SIGNAL,MP_FLAGS(A0)
	lea	MP_MSGLIST(A0),A0
	NEWLIST	A0
	bra.s	Crep9
CrepFail
	moveq	#0,D0
Crep9	movem.l	(sp)+,D2/A6	;port addr in D0 or zero if failed
	rts

*** DELETE MESSAGE PORT ***
* no name (not a public port)
MyDeletePort	;port addr in A1
	movem.l	A2/A6,-(sp)
	moveq	#0,D0
	move.b	MP_SIGBIT(A1),D0
	move.l	A1,A2
	callexe	FreeSignal
	move.l	A2,A1
	moveq	#MP_SIZE,D0
	callsys	FreeMem
	movem.l	(sp)+,A2/A6
	rts

*** CREATE IO REQUEST ***
MyCreateIO	;port addr in A1, size in D0
	movem.l	D0/A1/a6,-(sp)
	move.l	#MEMF_PUBLIC!MEMF_CLEAR,D1
	callexe	AllocMem
	movem.l	(sp)+,D1/A1/a6
	tst.l	D0
	beq.s	CreIO9	;no memory
	move.l	D0,A0
	move.l	A1,MN_REPLYPORT(A0)
	move.b	#NT_MESSAGE,LN_TYPE(A0)
	move.w	D1,MN_LENGTH(A0)
CreIO9	rts

*** DELETE IO REQUEST ***
MyDeleteIO	;IoRequest In A1
	moveq	#0,D0
	move.w	MN_LENGTH(A1),D0
	move.l	a6,-(sp)
	callexe	FreeMem
	move.l	(sp)+,a6
	rts

**** Command & address tables ****
comtable
	dc.b	'?ioxmfthc:arbgjwlsnud()&[]<>!=#\^@',$FF
	even

comadrs	rw	help
	rw	info
	rw	redirect
	rw	exit
	rw	memdisplay
	rw	memfill
	rw	memtransfer
	rw	memhunt
	rw	memcomp
	rw	modifymem
	rw	assemble
	rw	regs
	rw	breaks
	rw	go
	rw	jumpsr
	rw	walk
	rw	loadseg
	rw	show
	rw	number
	rw	unloadseg
	rw	disassem
	rw	allocate_mem
	rw	free_mem
	rw	alloc_abs
	rw	abs_load
	rw	abs_save
	rw	disk_read
	rw	disk_write
	rw	digisound
	rw	block_check
	rw	boot_check
	rw	new_cli
	rw	showtrap
	rw	cmdline

**** HELP TEXT ****
helptext
	dc.b	CLS,TAB,TAB,'-- Amiga Monitor Help (version '
	VERSION
	dc.b	') --',LF,LF
	dc.b	'?',TAB,TAB,': help (this)',TAB
	dc.b	'| [ addr name',TAB,TAB,': load absolute',LF
	dc.b	'x',TAB,TAB,': exit',TAB,TAB
	dc.b	'| ] addr1 length name',TAB,': save absolute',LF
	dc.b	'o [name]',TAB,':redirect output'
	dc.b	'| < addr dr block cnt',TAB,': read disk blocks',LF
	dc.b	'dir [name]',TAB,': directory',TAB
	dc.b	'| > addr dr block cnt',TAB,': write disk blocks',LF
	dc.b	'cd [name]',TAB,': current dir',TAB
	dc.b	'| del name',TAB,TAB,': delete file',LF
	dc.b	'cls',TAB,TAB,': clear screen',TAB
	dc.b	'| \',TAB,TAB,TAB,': new CLI',LF
	dc.b	'l name',TAB,TAB,': load segment',TAB
	dc.b	'| ! addr len per [cnt]',TAB,': play digisound',LF
	dc.b	'sl',TAB,TAB,': segment list',TAB
	dc.b	'| = addr',TAB,TAB,': disk block checksum',LF
	dc.b	'u',TAB,TAB,': unload segment'
	dc.b	'| # addr',TAB,TAB,': bootblock checksum',LF
	dc.b	'r',TAB,TAB,': show registers'
	dc.b	'| g [addr]',TAB,TAB,': execute (go)',LF
	dc.b	'r reg=num',TAB,': set register',TAB
	dc.b	'| j [addr]',TAB,TAB,': jump to subroutine',LF
	dc.b	'a addr',TAB,TAB,': assemble',TAB
	dc.b	'| w [addr]',TAB,TAB,': single step (walk)',LF
	dc.b	'd addr1 addr2',TAB,': disassemble',TAB
	dc.b	'| ( length',TAB,TAB,': allocate memory',LF
	dc.b	'm addr1 addr2',TAB,': display memory'
	dc.b	'| & addr length',TAB,TAB,': allocate absolute',LF
	dc.b	': addr bytes',TAB,': modify memory',TAB
	dc.b	'| ) addr/all',TAB,TAB,': free memory',LF
	dc.b	'b addr',TAB,TAB,': set breakpoint'
	dc.b	'| sm',TAB,TAB,TAB,': show allocated mem',LF
	dc.b	'bl',TAB,TAB,': list brkpoints'
	dc.b	'| c addr1 addr2 dest',TAB,': compare memory',LF
	dc.b	'br addr/all',TAB,':remove brkpoint'
	dc.b	'| t addr1 addr2 dest',TAB,': transfer memory',LF
	dc.b	'f addr1 addr2 bytes: fill mem',TAB
	dc.b	'| h addr1 addr2 bytes',TAB,': hunt memory',LF
	dc.b	'@ [line]',TAB,': enter cmd line'
	dc.b	'| n num :display number in hex/dec/oct/bin',LF
	dc.b	LF,0

**** disassembler data ****
	even
disadrtabl
	rw	handlezero	;0 -- immediate, bit manipulatios, movep
	rw	movebyte	;1 -- move.b
	rw	movelong	;2 -- move.l
	rw	moveword	;3 -- move.w
	rw	handlefour	;4 -- misc.
	rw	handle_five	;5 -- addq & subq , DBcc, Scc
	rw	branch		;6 -- branch instructions
	rw	movequick	;7 -- moveq #x,Dn
	rw	handle_eight 	;8 -- or, div, sbcd
	rw	subtract	;9 -- sub & suba
	rw	lineA		;10 Line-A -- unimplemented
	rw	handle_11	;11 -- cmp, cmpm, eor
	rw	handle_12	;12 -- and, mul, abcd, exg
	rw	addinst		;13 -- add & adda
	rw	shifts		;14 -- asl, asr, lsl, lsr, rol, ror, roxl, roxr
	rw	lineF		;15 Line-F -- unimplemented

condcodes	dc.b	'rasrhilscccsneeqvcvsplmigeltgtlehslo'

pcnam	dc.b	'(pc)',0	;program counter
USPnam	dc.b	'usp',0		;user stack pointer for Move An,usp & Move usp,An
zerotxt	dc.b	'dc.w    $00',0	;for zero "padding words"
linenam	dc.b	'line-',0	;for Line-A & Line-F
	even

*** INSTRUCTION NAMES ***
asnam		dc.b	'as',0
lsnam		dc.b	'ls',0
roxnam		dc.b	'rox',0
rotnam		dc.b	'ro',0
movenam		dc.b	'move',0
addnam		dc.b	'add',0
subnam		dc.b	'sub',0
andnam		dc.b	'and',0
ornam		dc.b	'or',0
abcdnam		dc.b	'abcd',0
sbcdnam		dc.b	'sbcd',0
mulnam		dc.b	'mul',0
divnam		dc.b	'div',0
exgnam		dc.b	'exg',0
eornam		dc.b	'eor',0
cmpnam		dc.b	'cmp',0
btstnam		dc.b	'btst',0
bchgnam		dc.b	'bchg',0
bclrnam		dc.b	'bclr',0
bsetnam		dc.b	'bset',0
chknam		dc.b	'chk',0
leanam		dc.b	'lea',0
extnam		dc.b	'ext',0
clrnam		dc.b	'clr',0
negnam		dc.b	'neg',0
notnam		dc.b	'not',0
tstnam		dc.b	'tst',0
nbcdnam		dc.b	'nbcd',0
swapnam		dc.b	'swap',0
peanam		dc.b	'pea',0
linknam		dc.b	'link',0
unlknam		dc.b	'unlk',0
resetnam	dc.b	'reset',0
nopnam		dc.b	'nop',0
stopnam		dc.b	'stop',0
rtenam		dc.b	'rte',0
trapnam		dc.b	'trap',0
rtsnam		dc.b	'rts',0
trapvnam	dc.b	'trapv',0
rtrnam		dc.b	'rtr',0
jsrnam		dc.b	'jsr',0
jmpnam		dc.b	'jmp',0
tasnam		dc.b	'tas',0
illegalnam	dc.b	'illegal',0

	even

instruction	macro
		rw	\1nam
n_\1		equ	InstrCount
InstrCount	set	InstrCount+1
		endm

InstrCount	set	0

instradrs
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
	dc.w	0	;end mark

instrjumps
	rw	as_asm		;0
	rw	ls_asm		;1
	rw	rox_asm		;2
	rw	rot_asm		;3
	rw	move_asm	;4	
	rw	add_asm		;5
	rw	sub_asm		;6
	rw	and_asm		;7
	rw	or_asm		;8
	rw	abcd_asm	;9
	rw	sbcd_asm	;10
	rw	mul_asm		;11
	rw	div_asm		;12
	rw	exg_asm		;13
	rw	eor_asm		;14
	rw	cmp_asm		;15
	rw	btst_asm	;16
	rw	bchg_asm	;17
	rw	bclr_asm	;18
	rw	bset_asm	;19
	rw	chk_asm		;20
	rw	lea_asm		;21
	rw	ext_asm		;22
	rw	clr_asm		;23
	rw	neg_asm		;24
	rw	not_asm		;25
	rw	tst_asm		;26
	rw	nbcd_asm	;27
	rw	swap_asm	;28
	rw	pea_asm		;29
	rw	link_asm	;30
	rw	unlk_asm	;31
	rw	reset_asm	;32
	rw	nop_asm		;33
	rw	stop_asm	;34
	rw	rte_asm		;35
	rw	tas_asm		;36
	rw	rts_asm		;37
	rw	trapv_asm	;38
	rw	rtr_asm		;39
	rw	jsr_asm		;40
	rw	jmp_asm		;41
	rw	trap_asm	;42
	rw	illegal_asm	;43

**** INFO TEXT ****
infotext dc.b CLS,LF,LF
	dc.b	TAB,TAB,'Monitor info (version '
	VERSION
	dc.b	')',LF
	dc.b	TAB,TAB,'---------------------------',LF
	dc.b	'   This is a machine code monitor for the Amiga.',LF
	dc.b	' Pressing the HELP-key displays a menu of available commands.',LF,LF
	dc.b	' Note1: Some of the assembler commands require the',LF
	dc.b	' size specifier (.B, .W or .L), but it can''t be used by some others.',LF,LF
	dc.b	' Note2: default number base is hex, use ''+'' prefix for decimal',LF
	dc.b	' (default base for negative numbers is decimal, use ''$'' prefix for hex)',LF
	dc.b	' Many assembler instructions require the ''$'' before a hex number starting',LF
	dc.b	' with ''D'' or ''A''',LF,LF
	dc.b	' This version corrects the exg Dn,An-bug',LF,LF
	dc.b	' This program can be freely distributed for non-commercial purposes.',LF
	dc.b	' I hope you find this program useful, but if you find any bugs in this',LF
	dc.b	' program, please let me know.',LF
	dc.b	'  Here is my address:',LF
	dc.b	'    Timo Rossi',LF
	dc.b	'    Kellankoski',LF
	dc.b	'    44300 KONNEVESI',LF
	dc.b	'    FINLAND',LF,LF,0

**** text data ****

UpAndClearEol	dc.b	CtrlK
ClearEol	dc.b	CSI,'K',0
delch		dc.b	CSI,'P',0
insch		dc.b	CSI,'@',0
go_left_fmt	dc.b	CSI,'%ldD',0
go_right_fmt	dc.b	CSI,'%ldC',0

dosname		dc.b	'dos.library',0
intuname	dc.b	'intuition.library',0
tdname		dc.b	'trackdisk.device',0
audioname	dc.b	'audio.device',0
allocmap	dc.b	1,8,2,4

windowfmt	dc.b	'RAW:0/0/%ld/%ld/Amiga Monitor v'
		VERSION
		dc.b	0
welcometxt
	dc.b	CLS,LF,TAB,TAB,TAB,' --- Amiga Monitor ---',LF,LF
	dc.b	TAB,TAB,'by Timo Rossi (c) 1987-1989,  version '
	VERSION
	dc.b	LF,LF,0

prompt	dc.b	'-> ',0

errtx	dc.b	'???',0

breaktx	dc.b	'*** Break ***',LF,0

NewCliCom	dc.b	'NewCLI "CON:0/12/640/100/New CLI Window"',0

comhfmt		dc.b	'%08lx ',0
sumfmt		dc.b	'Old: $%08lx New: $%08lx',LF,0
noBrkTxt	dc.b	'No Breakpoints set',LF,0
brklistTx	dc.b	'List of Breakpoints:',LF,0
audiotxt	dc.b	'Press Ctrl-C to stop...',LF,0
ulserr		dc.b	'Unload old segment first',LF,0

segadrmes	dc.b	'First segment at '
hexfmt		dc.b	'$%08lx',LF,0

allocfmt	dc.b	'Allocated from $%08lx to $%08lx',LF,0
rangefmt	dc.b	'%ld bytes read from $%08lx to $%08lx',LF,0
allocfail	dc.b	'Allocation failed',LF,0
noalloctx	dc.b	'Not allocated that',LF,0
no_alloc	dc.b	'No memory allocated',LF,0
memlisttx	dc.b	'Allocated memory:',LF,0
memlistfmt	dc.b	'$%08lx  $%08lx  %ld',LF,0
nosegmes	dc.b	'No segment loaded',LF,0

seghead		dc.b	'Segment list:',LF,0
loctext		dc.b	' startloc   endloc    length',LF,0

assemfmt	dc.b	'%08lx: ',0

memerr		dc.b	'Out of memory',LF,0

dnam		dc.b	'(dir)',0
freeblkfmt	dc.b	'%ld Blocks free.',LF,0

doserrfmt	dc.b	'DOS error %ld',LF,0

td_errfmt	dc.b	'Trackdisk error '
numfmt		dc.b	'%ld',LF,0

nodisktxt	dc.b	'No disk in drive',LF,0
wrprotxt	dc.b	'Disk is write protected',LF,0

outrangetxt	dc.b	'Out of range',LF,0

cmdlinetxt	dc.b	'Cmd line> ',0

rettx		dc.b	'*** Returned ***',LF,0
brkPtTxt	dc.b	'*** Breakpoint ***',LF,0
signtxt		dc.b	'unsigned',0

*** TRAP NAMES ***
buserrnam	dc.b	'Bus error',0
adrerrnam	dc.b	'Address error',0
illinstr	dc.b	'Illegal instruction',0
zerodiv		dc.b	'Zero divide',0
CHKnam		dc.b	'CHK instruction trap',0
TRAPVnam	dc.b	'TRAPV instruction trap',0
privilege	dc.b	'Privilege violation',0
tracenam	dc.b	'Trace',0
lineAnam	dc.b	'Line-A emulator trap',0
lineFnam	dc.b	'Line-F emulator trap',0
TRAPnam		dc.b	'TRAP instruction #',0

	ds.w 0

trapnamtabl
	rw	buserrnam	;0
	rw	adrerrnam	;1
	rw	illinstr	;2
	rw	zerodiv		;3
	rw	CHKnam		;4
	rw	TRAPVnam	;5
	rw	privilege	;6
	rw	tracenam	;7
	rw	lineAnam	;8
	rw	lineFnam	;9
	rw	TRAPnam		;10

*** UNINITIALIZED DATA SEGMENT ***

	section Monitor_Uninitialized_Data,BSS

DataRegs	ds.l	8		storage space for processor registers
AddrRegs	ds.l	8
regPC		ds.l	1		program counter
regCC		ds.b	1		condition code register
SkipBreakFlag	ds.b	1		bit #0 indicates skipping a brkpoint
GoBrkPtr	ds.l	1		address of skipped brkpoint struct
StackPtr	ds.l	1		saved monitor stack pointer
InpBuf		ds.b	LEN
OutBuf		ds.b	LEN
history		ds.b	LEN*NLINES	monitor command line history
cmdlinebuf	ds.b	LEN		for the @-command

	END
