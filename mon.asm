************************************
*                                  *
*   Amiga machine code monitor     *
*   Timo Rossi  1987-1988-1989     *
*                                  *
* v1.06 -> last mod. 1988-05-02    *
* v1.07 -> last mod. 1989-08-28    *
* v1.08 -> last mod. 1989-08-29    *
* v1.12 -> last mod. 1989-11-28    *
* v1.14 -> last mod. 1989-11-29    *
* v1.15 -> last mod. 1989-11-30    *
* v1.17 -> last mod. 1989-12-09    *
* v1.18 -> last mod. 1989-12-14    *
* v1.19 -> last mod. 1989-12-21    *
* v1.20 -> last mod. 1989-12-31    *
*                                  *
************************************

;DEBUG	set	1

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
; these will probably be done in versions 1.08+...
;  - m/d without parameters should check window height to decide how
;    many lines to display *done in version 1.09*
;  - assembler should handle size specifiers better
;  - add expression evaluation routines...(calculator)
;     * possibly change default number base to decimal (or user-defined...)
;     *done in version 1.10*
;  - 68010 support
;  - 68020/68881 support (quite difficult...)
;

;#
;# here are the starting comments from dis.a (the new disassembler routine)
;#
;
; dis.a  --  68000 disassembler
;
; created  1989-08-20  TR
;
; version 1.0 -- 1989-08-25
;
; 1989-08-24  TR  -->	now works quite well, tested, no bugs found!!
;			prints signed offsets, hex output leading zero
;			elimination works ok. zero word padding does not
;			confuse the disassembler any more...
;			now understands btst Dn,#immediate mode...
;			also changed hex output to use lower case letters.
;
;		  -->	it seems that you never find the last bug...just
;			when I though this was practically bug-free code
;			I received a letter from John van Dijk telling
;			that my monitor disassembler does not correctly
;			disassemble exg Dn,An-instruction. after little
;			examination I found that the bug was really in
;			my 68000 documents, I had made no error in the
;			program...at least it now works correctly in this
;			disassembler. and maybe I'll make a new version
;			of the monitor too...
;
; 1989-08-25  TR  -->	changed usp, sr & ccr to lower case, absolute
;			short addresses are now displayed as signed numbers
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
;		- the assembler now understands blo/bhs/slo/shs/dblo/sbhs
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
;   1989-08-31 --> v1.09a
;		- gets pointers to window and consoleunit at startup. uses
;		  window height in m/d window parameters. also does not
;		  any more call GetPrefs to set 60/80col flag, this
;		  information is not gotten from conunit.
;
;   1989-09-03 --> v1.09b
;		- better handler for 68010/20 stack frames
;
;   1989-09-26 --> v1.09c
;		- now should handle most 68010/20 stack frame types
;		  correctly.
;----
;   1989-11-25 --> v1.10
;		- now hunt doesn't find anything inside the monitor
;		  code/data area.
;		- show seglist now displays hunk numbers
;		- added expresion evaluation and user-defined default
;		  input number base.
;		- changed chip memory allocation syntax to '( length C'
;		- added hunk() and hlen() functions to expression
;		  evaluation routine
;
;		--> document: remove n cmd, help is h, ? is calc
;		     decimal is default, ba set/show defbase, chipmem alloc
;		--> future: read symbolds from load files,
;		     display hunk types (understand split data/bss hunks)
;
;   1989-11-26 --> v1.11
;		- added variables, set-command
;		- added avail()-function to expression parser
;
;	       --> v1.12
;		- all monitor variables are now in the bss-section
;		  (start address kept in a5)
;
;   1989-11-29 --> v1.14
;		- fixed a bug in isalpha that prevented the use
;		  of the '_'-character in variable names
;		- added 'memory info'-command
;		- variables beginning with '_' can now be used
;		  (possible conflict with decimal number prefix...)
;		- variables are now sorted alphabetically
;		- optimized code size
;
;   1989-11-30 --> v1.15
;		- more code size optimization (specifically in error
;		  handling code, because now mainloop restores stack pointer)
;		- move sp,usp now works
;		- exg Rn,sp now works (getreg must not
;		  change high word of d1)
;
;   1989-12-04 --> v1.16
;		- code is now re-entrant (can be made resident)
;		- monitor data area is now allocated with AllocMem()
;		- TC_TRAPDATA points to monitor data area
;		- default number base changed to hex again
;		- removed SkipBreakFlag, now all flags are in
;		  the flags-variable
;		- corrected some errors in comments
;
;   1989-12-09 --> v1.17
;		- register display now displays condition
;		  code register as 'ccr' and it is also accepted in
;		  the register set command.
;		- condition code register set/display now works again
;		- transfer from location zero now works. this bug was in
;		  the original code. strange that nobody noticed it...
;
;   1989-12-14 --> v1.18
;		- disassembler now uses new routines. shorter executable.
;
;   1989-12-21 --> v1.19
;		- disassembler now prints the dollar signs that were missing
;		  in previous version in immediate word/byte operands
;		  and trap numbers.
;
;   1989-12-31 --> v1.20
;		- '[register]'-syntax added to the expression parser
;
;  --> future: (next decade...)
;	- change the whole thing to use include.i & macros.i etc.
;	- use trlib
;	- read symbols from executable files
;	- display symbols in disassembly etc... (a real symbolic debugger...)
;
;
;   1990-01-02	- noticed some little problems. variables with upper case
;		  letters can't be used in the assembler because the
;		  assembler converts the entire command line to lower case.
;		  and variables beginning with 's' ,'d' and 'a' don't seem
;		  to work in the assembler even in lower case...because
;		  the addressing mode parser checks these letters for
;		  register names... ***NOT FIXED YET***
;
;   1990-01-06 --> v1.21
;		- variables are no longer case sensitive. this fixes the
;		  upper case variable problem with the assembler.
;		  (variables beginning with a,d or s still can't be
;		   used in some assembler instructions...)
;		- disassemble 'valid instruction'-return code
;		  is set correctly with line-[af] and ILLEGAL ($4afc).
;		- reformatted source code
;		- uses 'new' include files
;		- added ^^-command for debugging. this command displays
;		  the monitor data area pointer.
;		- (adding new trace modes...)
;

		include	'exec/types.i'
		include	'include.i'
		include	'offsets.i'
		include	'macros.i'

*** This macro is an easy way to update the version number ***
VERSION		macro
		dc.b	'1.21'
		endm

*** macro to display a single character ***
emit		macro
		moveq	#\1,D0
		bsr	ChrOut
		endm

emitwin		macro
		moveq	#\1,d0
		bsr	ChrOutWin
		endm

*** start output line ***
startline	macro
		lea	OutputBuffer(a5),a3
		endm

*** end output line (line feed+NULL) ***
endline		macro
		putchr	LF
		clr.b	(A3)
		endm

putchr		macro
		move.b	#\1,(a3)+
		endm

; definitions of the instruction size variable
BSIZE		equ	0
WSIZE		equ	1
LSIZE		equ	2


*** SOME SPECIAL CHARACTERS ***
CtrlC	equ	3	;control-c, break
CtrlE	equ	5	;control-key to edit existing assembler instruction
CtrlK	equ	11	;cursor up
CLS	equ	12	;clear screen (form feed)
CtrlX	equ	24	;control-x, clear input line
SPACE	equ	32
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

		STRUCTURE BrkPoint,0
		 APTR	brk_Next	;linked list
		 APTR	brk_Address	;address of breakpoint
		 UWORD	brk_Content	;word contents of that location
		LABEL brk_SIZE	;because it is temporarily replaced by ILLEGAL

*** Variable structure ***
; the size of this structure depends on the length of the variable name

		STRUCTURE Variable,0
		 APTR	var_Next	;linked list
		 LONG	var_Value	;32-bit interger value of variable
		 WORD	var_Length	;size of structure for FreeMem()
		LABEL	var_Name	;null-terminated string

** Data structure containing all variables **

		STRUCTURE MonitorData,0
; first the long word variables
		 APTR	DosBase		;DOS library base
		 APTR	WBenchMsg	;Workbench startup message pointer
		 APTR	MyTask		;pointer to TCB of monitor process
		 APTR	OldTrapCode	;task trap code when monitor was started
		 APTR	OldTrapData	;task trap data when monitor was started
		 LONG	WinFile		;main window file handle
		 APTR	ConsoleUnit	;console device unit pointer for main window
		 APTR	StackPtr	;saved monitor stack pointer
		 LONG	SegList		;currently loaded segment
		 APTR	MemoryList	;linked list of allocated memory
		 APTR	BreakList	;linked list of breakpoints
		 APTR	VarList		;linked list of variables
		 LONG	OutputFile	;current output file handle
		 APTR	Addr		;current address
		 APTR	EndAddr		;for disassembler & memdisplay
		 APTR	instrad		;instruction address for disassembler

		 APTR	GoBrkPtr	;address of skipped brkpoint struct

; the output buffer must be long word aligned
		 STRUCT	OutputBuffer,LEN
		 STRUCT	InputBuffer,LEN
		 STRUCT	History,LEN*NLINES
		 STRUCT	CmdLineBuf,LEN

; processor register storage
		 STRUCT	DataRegs,8*4	;data registers
		 STRUCT	AddrRegs,8*4	;address registers
		 APTR	RegPC		;program counter
		 UBYTE	RegCC		;condition code register
		 UBYTE	pad

; now word and byte variables
		 UWORD	opcode		;opcode for disassembler
		 UWORD	size		;current instruction size
		 UWORD	inpspecial	;input special mode for GetInput
		 UBYTE	flags		;flags, see below for bitdefs...
		 UBYTE	defbase		;current default number base for input
		LABEL MonitorData_SIZE

;
; monitor flags
;

		BITDEF	MON,BRKACTIVE,0
		BITDEF	MON,BRKSKIP,1

;
; equates for disassembler routine
;

EABITS		equ	$1f
DATA		equ	1
MEMORY		equ	2
CONTROL		equ	4
ALTER		equ	8
NIMM		equ	$10	;not immediate

SIZBITS		equ	$60
SIZSHFT		equ	5
BYTE		equ	$20
WORD		equ	$40
LONG		equ	$60

; this bit means that there is a special routine to handle this instruction
SPECIAL_CASE	equ	$80

DRB	equ	$80	; data register, number from bits 11-9
DR2	equ	$81	; data register, number from bits 2-0
ARB	equ	$82	; address register, number from bits 11-9
AR2	equ	$83	; address register, number from bits 2-0
EA	equ	$84	; effective address
SZ	equ	$85	; size specifier
IMM	equ	$86	; immediate data byte/word/long
IMMQ	equ	$87	; immediate data for addq/subq/shifts
IMVQ	equ	$88	; immediate data for moveq
SCCR	equ	$89	; sr/ccr
SD	equ	$8a	; shift direction
STP1	equ	$8b	; register shift type
STP2	equ	$8c	; memory shift type
CC	equ	$8d	; condition code or 'sr' or 'ra'
SOFFS	equ	$8e	; 8-bit pc-relative offset (short branches)
OFFS	equ	$8f	; 16-bit pc-relative offset
IMMOFFS	equ	$90	; immediate word constant, no '#'-sign
TRAPN	equ	$91	; trap number
BIT	equ	$92	; bit instruction type

TOKSTART	equ	$a0

;
		section	MonitorCode,CODE
;
; although the monitor can be started from workbench, that is not
; recommended, because if you run a program from the monitor it runs
; as a part of the monitor process and many program require CLI
; process environment (or if they are not started from a CLI they wait
; for workbench startup message which in that case doesn't arrive...).
;
; BCPL programs or other programs that use the internal BCPL library
; cannot be started from the monitor without a special calling routine
; that allocates BCPL stack and sets up the BCPL register environment...
; (not recommended...and using the BCPL library in your own programs is
; absolutely not recommended!)
;

*** THIS IS THE WORKBENCH/CLI STARTUP CODE ***
monitor_code_start
		moveq	#0,d5
		move.l	d5,a1
		call	ExecBase,FindTask		;find our task
		move.l	D0,A4
		tst.l	pr_CLI(A4)		;started from CLI ?
		bne.s	main			;branch if yes
		lea	pr_MsgPort(A4),A0	;started from workbench
		call	WaitPort		;wait for WB startup message
		lea	pr_MsgPort(A4),A0
		call	GetMsg			;get it
		move.l	D0,d5

main		move.l	#MonitorData_SIZE,d0
		move.l	#MEMF_PUBLIC!MEMF_CLEAR,d1
		call	AllocMem
		tst.l	d0
		beq	exit10
		move.l	d0,a5

		move.l	d5,WBenchMsg(a5)
		move.l	a4,MyTask(a5)
;
; set default number base, currently hex....
;
		move.b	#16,defbase(a5)
;
; Find the width and height of workbench screen before opening the window
; so this program works ok with NTSC & PAL & LACEWB & MoreRows etc..
; (how about A2024 or ECS superhires mode with kickstart 1.4???)
;
		lea	intuname(pc),A1
		moveq	#33,D0		  version 33: kickstart 1.2 or later
		call	OpenLibrary
		tst.l	D0
		beq	exit9
		move.l	d0,a6

		lea	OutputBuffer(a5),a0
		lea	20(a0),a2
		suba.l	a1,a1
		moveq	#sc_Height+2,d0		enough length to get width and height
		moveq	#WBENCHSCREEN,d1
		call	GetScreenData
		move.l	d0,d2		  	save result

		move.l	a6,a1
		call	ExecBase,CloseLibrary
		tst.l	d2
		beq	exit9

		lea	OutputBuffer(a5),a0
		movem.w	sc_Width(a0),d0-d1	this sign-extends to long
		subq.w	#8,d1
;
; window "filename" is created using RawDoFmt in the output buffer
;
		lea	windowfmt(pc),a0
		bsr	fmtstring

		lea	dosname(pc),A1
		moveq	#0,d0
		call	OpenLibrary		Open the DOS library...
		move.l	d0,DosBase(a5)
		beq	exit9
		move.l	D0,A6
		lea	OutputBuffer(a5),a0
		move.l	a0,d1
		move.l	#MODE_OLDFILE,D2
		call	Open			;open the window
		move.l	D0,WinFile(A5)
		beq	exit8
		move.l	D0,OutputFile(A5) 	;default output is monitor window

;
; set the the task trap code and data pointers
;
		lea	trapreturn(pc),A1
		move.l	MyTask(A5),A0
		move.l	TC_TRAPCODE(A0),OldTrapCode(A5)	;save old TrapCode
		move.l	TC_TRAPDATA(a0),OldTrapData(a5)
		move.l	A1,TC_TRAPCODE(A0)		;and set a new one
		move.l	a5,TC_TRAPDATA(a0)

;
; find pointer to intuition window structure of the monitor output window
; also find pointer to the console device unit structure
;
		lsl.l	#2,d0			filehandle BPTR->APTR
		move.l	d0,a0
		move.l	fh_Type(a0),a0
		moveq	#ACTION_DISK_INFO,d0
		lea	OutputBuffer(a5),a1	we use output buffer for InfoData
		move.l	a1,d1
		lsr.l	#2,d1			infodataptr APTR->BPTR
		bsr	sendpacket
		tst.l	d0
		beq	exit

		lea	OutputBuffer(a5),a0
		move.l	id_InUse(a0),a0		console IORequest ptr
		move.l	IO_UNIT(a0),ConsoleUnit(a5)

;
; set the stack pointer
;
		move.l	sp,A0
		sub.w	#$100,A0		;a safe area from this task's stack
		move.l	a0,d0
		and.b	#$fc,d0			;long word align
		move.l	d0,AddrRegs+4*7(a5)	;set stack pointer

		lea	welcometxt(pc),A0
		bsr	printstring_a0_window		;display welcome message

; this is for error handling...and for the retrun from the trap routine...
		move.l	sp,StackPtr(a5)

*** JUMP HERE AFTER EXECUTION OF A COMMAND ***
mainloop	move.l	StackPtr(a5),sp		;restore stack pointer
		moveq	#0,D0			;clear CTRL-C/D/E/F flags
		move.l	#SIGBREAKF_CTRL_C!SIGBREAKF_CTRL_D!SIGBREAKF_CTRL_E!SIGBREAKF_CTRL_F,D1
		call	ExecBase,SetSignal
		lea	prompt(pc),A0
		bsr	printstring_a0_window	;display prompt
		moveq	#0,D0
		bsr	GetInput
		move.b	(A3)+,D0
		bne.s	ml1

		move.l	WinFile(A5),D0		;empty command line
		cmp.l	OutputFile(A5),D0
		beq.s	mainloop

		emit	LF
		bra.s	mainloop

ml1		bsr	tolower
		lea	comtable(pc),A0
		moveq	#0,D1		;find command...

findcom		cmp.b	(A0),D0
		beq.s	foundcom
		addq.l	#1,D1
		cmp.b	#$FF,(A0)+	;end of command table ??
		bne.s	findcom

error		lea	errtx(pc),A0	;command not found, print error message
errcom		bsr	printstring_a0_window
		emitwin	LF
		bra.s	mainloop

foundcom			;command found, execute it
		add.l	D1,D1		;D1 = D1 * 2
		lea	comadrs(pc),A0
		add.l	D1,A0
		add.w	(A0),A0		;get command address
		jmp	(A0)		;jump to command

*** THE HELP COMMAND ***
help		lea	helptext(pc),A0
hinfo		bsr	printstring_a0
		bra.s	mainloop

*** INFO ***
info		lea	infotext(pc),A0
		bra.s	hinfo

*** EXIT FROM MONITOR ***
exit		move.l	MyTask(A5),A0
		move.l	OldTrapCode(A5),TC_TRAPCODE(A0)	;restore old TrapCode
		move.l	OldTrapData(a5),TC_TRAPDATA(a0)	; and TrapData
		bsr	FreeAllMem		;free all memory allocated by commands & and (
		bsr	remove_all_breaks	;remove all breakpoints (free memory)
		bsr	clear_all_variables
		move.l	DosBase(a5),a6
		move.l	SegList(A5),D1		;if a seglist is loaded, unload it
		beq.s	exit7
		call	UnLoadSeg

exit7		move.l	OutputFile(A5),D1	;if output is redirected, close output file
		cmp.l	WinFile(A5),D1
		beq.s	exit7a
		call	Close

exit7a		move.l	WinFile(A5),D1
		call	Close			;close window file

exit8		move.l	A6,A1
		call	ExecBase,CloseLibrary		;close dos library

exit9		move.l	WBenchMsg(A5),D3	;started from workbench??

		move.l	a5,a1
		move.l	#MonitorData_SIZE,d0
		call	FreeMem

exit10		tst.l	d3
		beq.s	exit99
		call	Forbid		;forbid, so WB can't UnloadSeg before exit
		move.l	D3,A1
		call	ReplyMsg	;reply the WB startup message

exit99		moveq	#0,D0	;error return code
		rts		;return control to the system...

*** DIRECTORY ***
dir		addq.l	#2,A3
		moveq	#0,D7
		move.l	#fib_SIZEOF,D0	;allocate FileInfoBlock
*** we use the same memory space for the InfoData structure
		move.l	#MEMF_PUBLIC!MEMF_CLEAR,D1
		call	ExecBase,AllocMem
		tst.l	D0
		beq	OutOfMem
		move.l	D0,A4		;we keep the fib-pointer in A4
		bsr	skipspaces
		move.l	A3,D1		;use rest of command line as a name
		moveq	#SHARED_LOCK,D2
		call	DosBase(a5),Lock
		move.l	D0,D7		;we keep the lock pointer in D7
		beq.s	dir7		;if i/o error
		move.l	D7,D1
		move.l	A4,D2
		call	Examine
		tst.l	D0
		beq.s	dir7		;branch if Examine failed
		tst.l	fib_DirEntryType(A4)
		bpl.s	dir1
		bsr.s	DisplayDirLine	;if Examine found a file, display information about it
		bra.s	dir7b		;then number of free blocks
dir1	;Examine found a directory, ExNext() it
		move.l	D7,D1
		move.l	A4,D2
		call	ExNext
		tst.l	D0
		bne.s	dir2
		call	IoErr
		cmp.l	#ERROR_NO_MORE_ENTRIES,D0
		bne.s	dir7a		;branch if a real dos error
		bra.s	dir7b		;no more entries, the display free blocks
dir2		bsr.s	DisplayDirLine
		bne.s	dir7b		;branch if user pressed break (control-c)
		bra.s	dir1
dir7		call	IoErr
dir7a		tst.l	D0
		beq.s	dir7c
		bsr	DOSErr
		bra.s	dir7c
dir7b	;directory ready, now display number of free blocks
		move.l	D7,D1
		move.l	A4,D2
		call	Info		;get info about the disk
		tst.l	D0
		beq.s	dir7
		move.l	id_NumBlocks(A4),D0
		sub.l	id_NumBlocksUsed(A4),D0
		lea	freeblkfmt(pc),a0
		bsr	printf
dir7c		move.l	D7,D1
		call	UnLock
dir8		move.l	A4,A1
		move.l	#fib_SIZEOF,D0
		call	ExecBase,FreeMem		;free FileInfoBlock
		bra	mainloop

OutOfMem	;print error message 'out of memory'
		lea	memerr(pc),A0
		bra	errcom

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
		cmp.w	#30,D1		;max 30 chars
		bcs.s	txmovloop
txfloop		putchr	SPACE
		addq.w	#1,D1
txmov2		cmp.w	#24,D1
		bcs.s	txfloop
		putchr	SPACE
		tst.l	fib_DirEntryType(A4)	;positive=dir, negative=file
		bmi.s	txputlen
		lea	dnam(pc),A1		;'(dir)'
		bsr	putstring
		bra.s	txmov9

txputlen	movem.l	a2/a6,-(sp)
		lea	numfmt(pc),a0
		lea	fib_Size(a4),a1
		lea	putch(pc),a2
		call	ExecBase,RawDoFmt
		movem.l	(sp)+,a2/a6
		bra.s	txmov10
txmov9		endline
txmov10		bsr	printstring
		bra	CheckKeys

*** PRINT DOS ERROR NUMBER ***
DOSErr		;error number in D0
		lea	doserrfmt(pc),a0
		bra	printf_window

**** LOAD SEGMENT ****
loadseg		tst.l	SegList(A5)
		bne.s	oldseg		;can't do this before old segment is unloaded
		bsr	skipspaces
		move.l	A3,D1
		call	DosBase(a5),LoadSeg
		move.l	D0,SegList(A5)
		beq.s	segerr		;branch if LoadSeg failed
		lea	segadrmes(pc),a0
		lsl.l	#2,d0
		addq.l	#4,d0
		move.l	d0,RegPC(a5)
		move.l	d0,Addr(a5)
		bsr	printf
		bra.s	mjump
oldseg		;message 'unload old segment first'
		lea	ulserr(pc),A0
		bsr	printstring_a0_window
		bra.s	mjump

segerr		;come here if LoadSeg failed
		call	IoErr
		bsr.s	DOSErr
mjump		bra	mainloop

nosegerr	;error 'no segment loaded'
		lea	nosegmes(pc),A0
		bsr	printstring_a0_window
		bra.s	mjump

**** UNLOAD SEGMENT ****
unloadseg	move.l	SegList(A5),D1
		beq.s	nosegerr	;branch if no segment
		call	DosBase(a5),UnLoadSeg
		clr.l	SegList(A5)	;remember to clear the seglist pointer
		bra.s	mjump

**** SEGMENT LIST ****
show_seglist	move.l	SegList(A5),D4
		beq.s	nosegerr	;branch if no seglist
		lea	seghead(pc),A0
		bsr	printstring_a0
		moveq	#0,d5
segloop		lsl.l	#2,d4
		move.l	d4,a2
		move.l	d4,d1
		addq.l	#4,d1
		move.l	-4(a2),d3
		subq.l	#8,d3
		move.l	d1,d2
		add.l	d3,d2
		subq.l	#1,d2
		move.l	d5,d0
		lea	seglistfmt(pc),a0
		bsr	printf
		addq.l	#1,d5
		move.l	(A2),D4		;get next segment pointer
		bsr	CheckKeys
		bne.s	mjump2
		tst.l	D4
		bne.s	segloop
mjump2		bra	mainloop

*** SHOW SEGMENT LIST & ALLOCATED MEMORY (and set-command) ***
show		bsr	skipspaces
		move.b	(A3),D0
		bsr	tolower
		cmp.b	#'l',D0
		beq.s	show_seglist
		cmp.b	#'e',d0
		beq.s	set_command
		cmp.b	#'m',D0
		beq	showmemlist
errx01		bra	error

; clear all variables
; called before exit and when the cv-command is executed
; changes d2 and a6
clear_all_variables
		move.l	ExecBase,a6
		move.l	VarList(a5),d2

clrvarloop	tst.l	d2
		beq.s	cleared_vars
		move.l	d2,a1
		moveq	#0,d0
		move.w	var_Length(a1),d0
		move.l	var_Next(a1),d2
		call	FreeMem
		bra.s	clrvarloop

cleared_vars	clr.l	VarList(a5)
		rts

;
; set [var=expr]
;
set_command	addq.l	#1,a3
		move.b	(a3)+,d0
		bsr	tolower
		cmp.b	#'t',d0
		bne.s	errx01
		bsr	skipspaces
		tst.b	(a3)
		bne.s	set_variable
; show variables
		move.l	VarList(a5),d5
		bne.s	showvar1
		lea	novartxt(pc),a0
		bsr	printstring_a0_window	;'no variables defined'
		bra.s	xvar_end

showvar1	lea	varhead(pc),a0
		bsr	printstring_a0

showvarloop	move.l	d5,a2
		lea	var_Name(a2),a0
		move.l	a0,d0
		move.l	var_Value(a2),d1
		move.l	d1,d2
		lea	varfmt(pc),a0
		bsr	printf
		bsr	CheckKeys
		bne.s	xvar_end
		move.l	var_Next(a2),d5
		bne.s	showvarloop
xvar_end	bra	mainloop

set_variable	move.b	(a3),d0
		bsr	isalpha
		bcc	error
		move.l	a3,a4
01$		move.b	(a3)+,d0
		bsr	isalnum
		bcs.s	01$
		subq.l	#1,a3
		move.l	a3,a2
		cmp.l	a3,a4
		beq	error
		bsr	skipspaces
		tst.b	(a3)
		beq.s	remvar
		cmp.b	#'=',(a3)+
		beq.s	02$
		subq.l	#1,a3
02$		bsr	get_expr
		move.l	a4,a0
		clr.b	(a2)
		bsr.s	setvar
		bra.s	xvar_end
remvar		move.l	a4,a0
		clr.b	(a2)
		bsr	findvar
		bcs.s	xvar_end
		bsr	deletevar
		bra.s	xvar_end

;
; set value of existing or new variable
; a0 - name of variable, null-terminated
; d0 - value of variable
;
setvar		movem.l	d0/a0,-(sp)
		bsr	findvar
		bcc.s	01$
		move.l	4(sp),a0
		bsr.s	addvar
		move.l	d0,a0
01$		movem.l	(sp)+,d0/a1
		move.l	d0,var_Value(a0)
		rts

;
; add a new variable
; a0 - null-terminated name
; return variable pointer in d0
;
addvar		movem.l	d2/a2-a3/a6,-(sp)
		move.l	a0,a2
01$		tst.b	(a0)+
		bne.s	01$
		lea	var_Name(a0),a0
		move.l	a0,d0
		sub.l	a2,d0
		move.l	d0,d2
		move.l	#MEMF_CLEAR!MEMF_PUBLIC,d1
		call	ExecBase,AllocMem
		tst.l	d0
		beq	OutOfMem
		move.l	d0,a3
		move.w	d2,var_Length(a3)
		lea	var_Name(a3),a0
05$		move.b	(a2)+,(a0)+
		bne.s	05$

;
; insert to the list. keep list sorted.
;#
;# ->v1.21 ... sorting is now case insensitive
;#
;
		lea	VarList(a5),a2
06$		move.l	(a2),d0
		beq.s	add_now
		move.l	d0,a0
		lea	var_Name(a0),a0
		lea	var_Name(a3),a1
07$		bsr.s	str_comp_ch
		bls.s	add_now
		move.l	(a2),a2
		bra.s	06$

add_now		move.l	(a2),(a3)
		move.l	a3,(a2)
		move.l	a3,d0

		movem.l	(sp)+,d2/a2-a3/a6
		rts

str_comp_ch	move.b	(a1)+,d0
		bsr	tolower
		move.b	d0,d1
		move.b	(a0)+,d0
		bsr	tolower
		cmp.b	d0,d1
		bne.s	01$
		tst.b	d0
		bne.s	str_comp_ch
01$		rts

;
; find a variable and return its value in d0 and variable structure
; address in a0. return carry set if not found, else carry clear
; parameters: pointer to null-terminated name in a0
;
;#
;# ->v1.21  1990-01-06
;# variable names are now case insensitive
;#
findvar		move.l	VarList(a5),d1
fvloop		beq.s	varnotfound
		move.l	d1,a1
		movem.l	d1/a0-a1,-(sp)
		lea	var_Name(a1),a1
		bsr.s	str_comp_ch
		movem.l	(sp)+,d1/a0-a1
		beq.s	varfound
		move.l	var_Next(a1),d1
		bra.s	fvloop

varfound	move.l	a1,a0
		move.l	var_Value(a0),d0	;this clears carry
		rts

varnotfound	sec
		rts

;
; remove variable, variable structure pointer in a0
;
deletevar	lea	VarList(a5),a1
01$		cmp.l	(a1),a0
		beq.s	02$
		move.l	(a1),d1
		beq.s	09$
		move.l	d1,a1
		bra.s	01$
02$		exg	a0,a1
		move.l	(a1),(a0)
		moveq	#0,d0
		move.w	var_Length(a1),d0
		move.l	a6,-(sp)
		call	ExecBase,FreeMem
		move.l	(sp)+,a6
09$		rts

*** MEMORY LIST ***
showmemlist	move.l	MemoryList(A5),D5
		bne.s	showm2
not_at_all_allocated
		lea	no_alloc(pc),A0
		bsr	printstring_a0_window	;error 'no memory allocated'
		bra.s	showmem_end

showm2		lea	memlisttx(pc),A0
		bsr	printstring_a0
		lea	loctext(pc),A0
		bsr	printstring_a0

showmemloop	move.l	d5,a2
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

showmem_end	bra	mainloop

*** FREE ALL MEMORY ALLOCATED WITH THE ( AND & COMMANDS ***
FreeAllMem	move.l	a6,-(sp)
		move.l	MemoryList(A5),D5
		beq.s	freeall9
		move.l	ExecBase,A6
freeall_loop	move.l	D5,A1
		move.l	4(A1),D0
		addq.l	#8,D0
		move.l	(A1),D5
		call	FreeMem
		tst.l	D5
		bne.s	freeall_loop
		clr.l	MemoryList(A5)
freeall9	move.l	(sp)+,a6
		rts

*** ALLOCATE MEMORY ***
allocate_mem	bsr	get_expr
		move.l	D0,D5
		bsr	skipspaces
		moveq	#0,d1
		move.b	(a3),d0
		bsr	tolower
		cmp.b	#'c',d0
		bne.s	01$
		moveq	#MEMF_CHIP,d1
01$		move.l	D5,D0
		addq.l	#8,D0
		or.l	#MEMF_CLEAR!MEMF_PUBLIC,D1
		call	ExecBase,AllocMem
		tst.l	D0
		beq.s	allocfailed
		move.l	D0,A0

alloc_mem_1	;add allocated memory to linked list of memory blocks
** A0 points to memory block, D5 is length
		move.l	D5,4(A0)
		moveq	#0,D0
		move.l	MemoryList(A5),D1

alloc_find1	beq.s	alloc_find2	;keep the linked list in order, lowest address first
		move.l	D1,A1
		cmp.l	A1,A0
		bcs.s	alloc_find2
		move.l	D1,D0
		move.l	(A1),D1
		bra.s	alloc_find1

alloc_find2	tst.l	D0
		bne.s	alloc_do2
		move.l	MemoryList(A5),(A0)	;add new memory node to start of list
		move.l	A0,MemoryList(A5)
		bra.s	alloc_display

alloc_do2	move.l	D1,(A0)		;add new memory node to middle or end of list
		move.l	D0,A1
		move.l	A0,(A1)

alloc_display	move.l	a0,d0
		addq.l	#8,d0
		move.l	d0,Addr(a5)
		move.l	d0,d1
		add.l	4(a0),d1
		subq.l	#1,d1
		lea	allocfmt(pc),a0
		bsr	printf
jmp_mainloop_1	bra	mainloop

allocfailed		;error 'allocation failed'
		lea	allocfail(pc),A0
		bsr	printstring_a0_window
		bra.s	jmp_mainloop_1

*** ALLOCATE ABSOLUTE ***
alloc_abs	bsr	get_expr
		subq.l	#8,D0	;remember to subtract 8 from the starting address
		move.l	D0,D7	;(next block pointer & length)
		bsr	get_expr
		move.l	D0,D5
		beq	error
		addq.l	#8,D0	;add 8 to length
		move.l	D7,A1
		call	ExecBase,AllocAbs
		tst.l	D0
		beq.s	allocfailed
		move.l	D7,A0
		bra	alloc_mem_1

*** FREE MEMORY ***
free_mem	bsr	skipspaces
		move.b	(a3),d0
		bsr	tolower
		cmp.b	#'a',d0		;check 'all'
		bne.s	free_norm
		moveq	#'l',D1
		move.b	1(a3),d0
		bsr	tolower
		cmp.b	d1,d0
		bne.s	free_norm
		move.b	2(a3),d0
		bsr	tolower
		cmp.b	d1,d0
		bne.s	free_norm
		bsr	FreeAllMem
mloop_xj	bra	mainloop

free_norm	bsr	get_expr
		subq.l	#8,D0
		move.l	MemoryList(A5),D1
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
		bra.s	mloop_xj
found_do_free_mem	;remove memory block from linked list
		tst.l	D2
		bne.s	notfirst
		move.l	(A1),MemoryList(A5)
		bra.s	do_free

notfirst	move.l	D2,A0
		move.l	(A1),(A0)

do_free		move.l	4(A1),D0
		addq.l	#8,D0
		call	ExecBase,FreeMem
		bra.s	mloop_xj

*** SAVE ABSOLUTE (using DOS Write)****
abs_save	bsr	get_expr
		move.l	D0,A4
		bsr	get_expr
		move.l	D0,D6
		bsr	skipspaces
		move.l	A3,D1
		move.l	#MODE_NEWFILE,D2
		call	DosBase(a5),Open
		move.l	D0,D7
		bne.s	abs_save_1

dos_err			call	IoErr
		bsr	DOSErr
		bra.s	jmp_mainloop_2a

abs_save_1	move.l	D7,D1
		move.l	A4,D2
		move.l	D6,D3
		call	Write
		move.l	D7,D1
		call	Close
		bra.s	jmp_mainloop_2a

*** LOAD ABSOLUTE (using DOS Read) ***
abs_load	bsr	get_expr
		move.l	D0,A4
		bsr	skipspaces
		move.l	A3,D1
		move.l	#MODE_OLDFILE,D2
		call	DosBase(a5),Open	;open file
		move.l	D0,D7
		beq.s	dos_err
		move.l	D7,D1
		move.l	A4,D2
		move.l	#$7FFFFFFF,D3	;MaxInt (the file can't be longer than this)
		call	Read		;read from file, until EOF, return actual length
		tst.l	D0
		ble.s	abs_load_1
		move.l	A4,D5
		move.l	D0,D6
		bsr.s	showrange
abs_load_1	move.l	D7,D1
		call	Close	;close the file
jmp_mainloop_2a	bra.s	jmp_mainloop_2

*** REDIRECT OUTPUT ***
redirect	move.l	DosBase(a5),a6
		move.l	OutputFile(A5),D1	;is output currently redirected
		cmp.l	WinFile(A5),D1
		beq.s	redir1
		call	Close			;if so, then close redirection file
		move.l	WinFile(A5),OutputFile(A5)	;standard output
redir1		bsr	skipspaces
		tst.b	(A3)
		beq.s	jmp_mainloop_2
		move.l	A3,D1
		move.l	#MODE_NEWFILE,D2
		call	Open	;open redirection file
		tst.l	D0
		beq	dos_err
		move.l	D0,OutputFile(A5)

jmp_mainloop_2	bra	mainloop

*** NEW CLI ***
new_cli		; Execute("NewCLI",0,WinFile)
		lea	NewCliCom(pc),A0
		move.l	A0,D1
		moveq	#0,D2
		move.l	WinFile(A5),D3
		call	DosBase(a5),Execute
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
;#
;# v1.07->
;#  disk read and write use now a buffer in chip memory, and copy data
;#  from/to that buffer, so the actual transfer address does not need
;#  to be in chip memory.
;#
disk_read	moveq	#0,D7	;D7 is read/write flags
		bra.s	disk_rw

disk_write	moveq	#-1,D7

disk_rw		bsr	get_expr
		btst	#0,D0
		bne	error		;error: odd address
		move.l	D0,Addr(A5)
		beq	error
		bsr	get_expr
		move.l	D0,D3		;Unit number (drive)
		bsr	get_expr
		move.l	D0,D4		;starting sector
		bsr	get_expr
		move.l	D0,D5		;length
		beq	error		;error: zero length
		move.l	#512,d0
		move.l	#MEMF_CLEAR!MEMF_CHIP,d1
		call	ExecBase,AllocMem
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
		call	OpenDevice
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
disk_rd		move.w	#CMD_READ,IO_COMMAND(A2)	;read from disk
		move.l	A2,A1
		call	DoIO
		tst.l	D0
		bne.s	disk_io_err
		move.l	IO_ACTUAL(a2),d0
		move.l	a4,a0
		move.l	Addr(a5),a1
		call	CopyMem
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
disk_wr		move.l	Addr(a5),a0
		move.l	a4,a1
		move.l	#512,d0
		call	CopyMem
		move.w	#CMD_WRITE,IO_COMMAND(A2)	;write to disk
		move.l	A2,A1
		call	DoIO
		tst.l	D0
		bne.s	disk_io_err
		move.l	IO_ACTUAL(a2),d0
		add.l	d0,IO_OFFSET(a2)
		add.l	d0,Addr(a5)
		sub.l	d0,d4
		bgt.s	disk_wr
		move.w	#CMD_UPDATE,IO_COMMAND(A2)	;make sure that the buffer is written
		move.l	A2,A1
		call	DoIO
		tst.l	D0
		beq.s	disk_io_5

disk_io_err	;Print TrackDisk error number
		cmp.b	#IOERR_BADLENGTH,d0
		bne.s	00$
		lea	outrangetxt(pc),a0
		bra.s	02$
00$		cmp.b	#TDERR_DiskChanged,d0
		bne.s	01$
		lea	nodisktxt(pc),a0
		bra.s	02$
01$		cmp.b	#TDERR_WriteProt,d0
		bne.s	03$
		lea	wrprotxt(pc),a0
02$		bsr	printstring_a0_window
		bra.s	99$
03$		lea	td_errfmt(pc),a0
		bsr	printf_window
99$		emitwin	LF

disk_io_5	;stop drive motor
		move.w	#TD_MOTOR,IO_COMMAND(A2)
		clr.l	IO_LENGTH(A2)
		move.l	A2,A1
		call	DoIO

disk_io_6	move.l	A2,A1
		call	CloseDevice

disk_io_7	move.l	MN_REPLYPORT(a2),d6
		move.l	A2,A1
		bsr	MyDeleteIO

disk_io_8	move.l	D6,A1
		bsr	MyDeletePort

disk_io_9	move.l	a4,a1
		move.l	#512,d0
		call	FreeMem
		bra	mainloop

*** PLAY DIGITIZED SOUND ***
digisound	bsr	get_expr
		btst	#0,D0	;test: if the address if odd, then error
		beq.s	digi1

digi_err	bra	error

digi1		move.l	D0,D5
		bsr	get_expr
		tst.l	D0
		beq.s	digi_err	;error: zero length
		btst	#0,D0
		bne.s	digi_err
		move.l	D0,D6
		bsr	get_expr	;period (speed)
		move.w	D0,D7
		clr.w	size(a5)
		bsr	skipspaces
		tst.b	(a3)
		beq.s	00$
		bsr	get_expr	;# of cycles, defaults to zero (loop)
		move.w	d0,size(a5)
00$		bsr	MyCreatePort
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
		call	ExecBase,OpenDevice	;open audio.device
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
		call	SetSignal	;clear CTRL-C signal
		move.l	d2,d0
		move.l	MN_REPLYPORT(a2),a0
		move.b	MP_SIGBIT(a0),d3
		bset	d3,d0			wait until the sound finishes or
		call	Wait			user presses Ctrl-C
		btst	d3,d0
		beq.s	01$
		move.l	a2,a1
		call	Remove		remove the message from the port
01$		move.l	A2,A1
		call	CloseDevice
digi7		move.l	MN_REPLYPORT(a2),d2
		move.l	A2,A1
		bsr	MyDeleteIO
digi8		move.l	d2,A1
		bsr	MyDeletePort
digi9		bra	mainloop

*** DISK BLOCK CHECKSUM ***
block_check	bsr	get_expr
		tst.l	d0
		beq.s	errx02
		move.l	D0,A0
		move.l	D0,A1
		moveq	#0,D0
		moveq	#512/4-1,D1	;disk block size 512 bytes

bl_check	add.l	(A0)+,D0
		dbf	D1,bl_check

		add.w	#5*4,A1		;checksum located at longword #5 in block
		move.l	(A1),D6
		sub.l	D0,(A1)
		move.l	(A1),D7
		bra.s	ShowSum

errx02		bra	error

*** BOOTBLOCK CHECKSUM ***
boot_check	bsr	get_expr
		tst.l	d0
		beq.s	errx02
		move.l	D0,A0
		move.l	D0,A1
		moveq	#0,D0
		move.w	#1024/4-1,D1	;bootblock size 1024 bytes

boot_ch		add.l	(A0)+,D0
		bcc.s	boot_1
		addq.l	#1,D0		;remember to add the carry
boot_1		dbf	D1,boot_ch

		addq.l	#4,A1		;checksum in second longword
		move.l	(A1),D6
		sub.l	D0,(A1)
		bcc.s	boot_2
		subq.l	#1,(A1)
boot_2		move.l	(A1),D7

*** SHOW OLD AND NEW CHECKSUM (jump from the previous two commands)
ShowSum ;old sum in D6, new sum in D7
		move.l	d6,d0
		move.l	d7,d1
		lea	sumfmt(pc),a0
		bsr	printf
		bra	mainloop

*** MODIFY MEMORY ***
modifymem	bsr	get_expr
		move.l	D0,A0
		bsr.s	getstring
		bra	mainloop

*** GET STRING FROM INPUT LINE TO ADDR IN A0, LENGTH IN D2 ***
* NOTE: this version requires commas between numbers and strings
; a1 is end addr of string on return
getstring	move.l	A0,A1
gstr1		bsr	skipspaces
		move.b	(A3)+,D0
		beq.s	gstr9		;branch if end of input line
		cmp.b	#'''',D0	;if single quote, then string follows
		beq.s	stringi
		subq.l	#1,A3
		bsr	get_expr		;else get number
		move.b	D0,(A1)+
gstr1a		bsr	skipspaces
		cmp.b	#COMMA,(a3)+
		bne.s	gstr9			;if comma not found, then end
		bra.s	gstr1
stringi		move.b	(A3)+,D0
		beq.s	gstr9
		cmp.b	#'''',D0
		beq.s	gstr2
string1		move.b	D0,(A1)+
		bra.s	stringi
gstr2		cmp.b	#'''',(A3)+	;test for double-single-quote
		beq.s	string1
		subq.l	#1,A3
		bra.s	gstr1a
gstr9		move.l	A1,D2
		sub.l	A0,D2		;calculate length of the string in D2
		rts

*** DELETE A FILE ***
Deletefile	addq.l	#2,A3
		bsr	skipspaces
		move.l	A3,D1
		call	DosBase(a5),DeleteFile
		tst.l	D0
		beq	dos_err
		bra.s	jmp_mainloop_3

*** CHANGE CURRENT DIRECTORY (CD) ***
CurrentDirectory
		addq.l	#1,A3
		bsr	skipspaces
		move.l	DosBase(a5),a6
		tst.b	(A3)
		bne.s	cd_1
		moveq	#0,D1	;if no name given set lock to zero (initial boot device root dir)
		bra.s	cd_2
cd_1		move.l	A3,D1
		moveq	#SHARED_LOCK,D2
		call	Lock
		tst.l	D0
		beq	dos_err
		move.l	D0,D1
cd_2		call	CurrentDir
		move.l	D0,D1
		call	UnLock	;unlock previous current directory

jmp_mainloop_3	bra	mainloop

clearvars	lea	clvartxt(pc),a0
		bsr	printstring_a0_window
		bsr	GetKey
		bsr	tolower
		cmp.b	#'y',d0
		bne.s	09$
		bsr	ChrOutWin
		bsr	clear_all_variables
09$		emitwin	LF
		bra.s	jmp_mainloop_3

*** compare memory, set current directory or clear screen or variables ***
memcomp		move.b	(a3),d0
		bsr	tolower
		cmp.b	#'v',d0
		beq.s	clearvars
		cmp.b	#'d',d0
		beq.s	CurrentDirectory
		cmp.b	#'l',d0
		bne.s	no_cls
		move.b	1(a3),d0
		bsr	tolower
		cmp.b	#'s',d0
		bne.s	no_cls

*** THE CLS COMMAND ***
		emitwin	CLS	;Clear the Screen
		bra.s	jmp_mainloop_3

**** COMPARE MEMORY ****
no_cls		bsr.s	get_n_per_line
		move.w	d6,D7
		bsr	get_expr
		move.l	D0,A0
		bsr	get_expr
		move.l	D0,A1
		bsr	get_expr
		move.l	D0,A2

comp1		cmp.l	A1,A0
		bhi.s	comp99
		cmpm.b	(A0)+,(A2)+
		beq.s	comp1
		movem.l	A0-A1,-(sp)
		move.l	A0,D0
		subq.l	#1,D0
		lea	comhfmt(pc),a0
		bsr	printf
		subq.w	#1,D7
		bne.s	compf1
		emit	LF
		move.w	d6,D7

compf1		bsr	CheckKeys
		bne.s	compbreak
		movem.l	(sp)+,A0-A1
		bra.s	comp1

compbreak
;#	addq.l	#8,sp	;stack pointer i reset in start of main loop
comp99		emit	LF
		bra	mainloop

;
; find out how many addresses can be printed on one line
; in the c and h commands
;
get_n_per_line	moveq	#0,d6
		move.l	ConsoleUnit(a5),a0
		move.w	cu_XMax(a0),d6
		divu	#9,d6
		bne.s	09$
		moveq	#1,d6
09$		rts

**** TRANSFER MEMORY ****
memtransfer	bsr	get_expr
		move.l	D0,A0
		bsr	get_expr
		move.l	D0,A1
		bsr	get_expr
		move.l	D0,A2
		cmp.l	A2,A0
		bcs.s	backwards	;if destination > source, transfer backwards
;#
;# this may fail if the end address is $ffffffff. normally this is not
;# true, so it is not so much a problem...
;#
trf1		cmp.l	A1,A0
		bhi.s	huntfilltrf2
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
		bcs.s	huntfilltrf2
trf2		move.b	(A1),(A2)
		cmp.l	a0,a1
		bls.s	huntfilltrf2
		subq.l	#1,A1
		subq.l	#1,A2
		bra.s	trf2

**** FILL MEMORY ****
* This version can fill memory with a pattern
memfill		bsr	get_expr
		move.l	D0,-(sp)
		bsr	get_expr
		move.l	D0,A2
		lea	InputBuffer(a5),a0
		bsr	getstring
		move.l	(sp)+,A1
		tst.l	D2
		beq.s	huntfilltrf2
fill0		moveq	#0,D0
fill1		cmp.l	A2,A1
		bhi.s	huntfilltrf2
		move.b	0(A0,D0.L),(A1)+
		addq.l	#1,D0
		cmp.l	D2,D0
		bcs.s	fill1
		bra.s	fill0

huntfilltrf2	bra	mainloop

**** HUNT MEMORY ****
memhunt		bsr	skipspaces
		tst.b	(a3)
		beq	help

		bsr	get_n_per_line
		move.w	d6,D7
		bsr	get_expr
		move.l	D0,-(sp)
		bsr	get_expr
		move.l	D0,A2
		lea	InputBuffer(a5),a0
		bsr	getstring
		move.l	(sp)+,A1
		tst.l	D2	;string length
		beq.s	huntfilltrf2

hunt0		move.b	(A0),D0
hunt1		cmp.l	A2,A1
		bhi.s	hunt99
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

hunt_ck1	cmp.l	a5,a1
		bcs.s	huntprint
		lea	MonitorData_SIZE(a5),a0
		cmp.l	a0,a1
		bcs.s	huntf2

huntprint	move.l	A1,D0
		subq.l	#1,D0
		lea	comhfmt(pc),a0
		bsr	printf
		subq.w	#1,D7
		bne.s	huntf1
		emit	LF
		move.w	d6,d7
huntf1		bsr.s	CheckKeys
		bne.s	huntbreak

huntf2		movem.l	(sp)+,A0-A1
		bra.s	hunt0

huntbreak
;#		addq.l	#8,sp		;stack pointer is reset in main loop
hunt99		emit	LF
		bra	mainloop

**** WAIT IF SPACE PRESSED, BREAK IF CTRL-C (status non-zero) ****
CheckKeys	move.l	WinFile(A5),D1
		movem.l	D2/a6,-(sp)
;#
;# is timeout zero really safe?...I have found no problems yet...
;#
		moveq	#0,D2		;timeout=0
		call	DosBase(a5),WaitForChar
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
nospc		cmp.w	#CtrlC,D0
		bne.s	nobreak
break		lea	breaktx(pc),A0	;message '*** break ***'
		bsr	printstring_a0_window
		moveq	#-1,D0
		rts
nobreak		moveq	#0,D0
		rts

**** PARAMETERS FOR DISPLAY AND DISASSEMBLE ****
* get Addr and EndAddr & number of lines to display in D7
* if D7 is zero then display from Addr to EndAddr
* if D7 is non-zero EndAddr is ignored
getparams	bsr	skipspaces
		tst.b	(A3)
		beq.s	param9
		bsr	get_expr
		bclr	#0,D0
		move.l	D0,Addr(A5)
		bsr	skipspaces
		tst.b	(A3)
		beq.s	param9
		bsr	get_expr
		move.l	D0,EndAddr(A5)
		moveq	#0,D7
		rts
param9		moveq	#20,D7
		move.l	OutputFile(a5),d0
		cmp.l	WinFile(a5),d0
		bne.s	09$
;
; get the number of text lines that will fit in the window
; from the console device unit structure
;
		move.l	ConsoleUnit(a5),a0
		move.w	cu_YMax(a0),d7
		subq.w	#2,d7
		moveq	#1,d0
		cmp.w	d0,d7
		bcc.s	09$
		move.w	d0,d7
09$		rts

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

cmpadrs		cmp.l	EndAddr(A5),A4
		bls.s	cont

stop1		moveq	#0,D0
		rts

cont		moveq	#-1,D0
		rts

disassem
*** Disassemble, delete file or directory **
		cmp.b	#'e',(A3)
		bne.s	nodel
		cmp.b	#'l',1(A3)
		beq	Deletefile
nodel		cmp.b	#'i',(A3)
		bne.s	nodir
		cmp.b	#'r',1(A3)
		beq	dir


**** DISASSEMBLE ****
nodir		bsr	getparams
		move.l	Addr(A5),A4

disasmloop	startline
		bsr.s	disassemble
		bsr	printstring
		bsr	CheckEnd
		bne.s	disasmloop

		move.l	A4,Addr(A5)
		bra	mainloop

*** DISASSEMBLE ONE INSTRUCTION ***
;
; disassembler routine main entry point
;
; a4 contains memory address of opcode on entry, incremented to
; next instruction on exit.
;
; a3 contains address of output text buffer, incremented to end of
; text on exit.
;
; return pointer to start of instruction mnemonic name in a0
; return d0 zero if valid instruction was disasembled
; (now line-[af] and ILLEGAL are not considered as valid instructions)
;
disassemble	movem.l	d2-d7/a2/a5-a6,-(sp)

		move.l	a4,d0
		bsr	puthex1_68

		move.l	a3,a5
		addq.l	#1,a5
		moveq	#SPACE,d0
		moveq	#24-1,d1
01$		move.b	d0,(a3)+
		dbf	d1,01$
		move.l	a3,-(sp)

; store stack pointer in high words of d4 and d5
		move.l	sp,d4
		move.w	d4,d5
		swap	d5

		move.l	a3,d3

		bsr	GetWord			get opcode
		move.l	a4,a6
		move.w	d0,d7
		bne.s	02$
		move.w	(a4),d1
		and.w	#$ff00,d1
		beq.s	02$
		lea	zero_txt(pc),a0
		bsr	put_str
		moveq	#-1,d0
		bra	dis_end

02$		lea	DisAsmTable(pc),a2

instr_loop	move.w	d7,d0
		and.w	(a2)+,d0
		cmp.w	(a2)+,d0
		beq.s	i_found
		moveq	#0,d1
		move.b	1(a2),d1
		move.l	a2,d0			get next table entry
		add.l	d1,d0
		addq.l	#3,d0
		and.b	#$fe,d0
		move.l	d0,a2
		bra.s	instr_loop

i_found		move.b	(a2)+,d6		get flags
		bmi.s	special_routine

		clr.w	d4			get size
		move.b	d6,d4
		lsr.b	#SIZSHFT,d4
		and.b	#3,d4
		bne.s	siz01

		move.b	d7,d4
		lsr.b	#6,d4			get size from opcode bits 7-6
		addq.b	#1,d4
		and.b	#3,d4
siz01		and.b	#EABITS,d6

		clr.w	d5
		move.b	(a2)+,d5		get string length
		subq.w	#1,d5			for dbf

strloop		bsr.s	fmt_char
		dbf	d5,strloop

;#
;# -> v1.21
;# added test for line-[af] 1990-01-06
;# also ILLEGAL ($4afc) is not a valid instruction
;#
		cmp.w	#ILLEGAL,d7
		beq.s	invalid_instr
		and.w	#$f000,d7
		cmp.w	#$a000,d7
		beq.s	invalid_instr
		cmp.w	#$f000,d7
		bne.s	valid_instr

invalid_instr	moveq	#-1,d0
		bra.s	dis_end

special_routine	clr.w	d4
		move.b	d6,d4
		lsr.b	#SIZSHFT,d4
		and.b	#3,d4
		and.b	#EABITS,d6

		clr.w	d5
		move.b	(a2)+,d5
		subq.w	#3,d5
		move.l	a2,d2
		lea	2(a2),a2

01$		bsr.s	fmt_char
		dbf	d5,01$

		move.l	d2,a0
		add.w	(a0),a0
		jsr	(a0)

valid_instr	moveq	#0,d0

dis_end		move.b	#LF,(a3)+
		clr.b	(a3)
		move.l	(sp)+,a0
		movem.l	(sp)+,d2-d7/a2/a5-a6
		rts

fmt_char	moveq	#0,d0
		move.b	(a2)+,d0
		bmi.s	do_special
		cmp.b	#TAB,d0
		beq.s	do_tab
		move.b	d0,(a3)+
rt1		rts

do_tab		move.l	4(sp),a0
		addq.l	#8,a0
01$		cmp.l	a0,a3
		bcc.s	rt1
		move.b	#SPACE,(a3)+
		bra.s	01$

do_special	cmp.b	#TOKSTART,d0
		bcc.s	str_tok
		and.b	#$7f,d0
		add.w	d0,d0
		lea	str_routines(pc,d0.w),a0
		add.w	(a0),a0
		jmp	(a0)

str_tok		add.w	d0,d0
		lea	instradrs-2*TOKSTART(pc),a0
		add.w	d0,a0
		add.w	(a0),a0
		bra	put_str

str_routines	rw	datar11,datar2,addrr11,addrr2
		rw	effective_address
		rw	size_specifier
		rw	immediate_data
		rw	immediate_quick,immediate_moveq
		rw	sr_or_ccr
		rw	shiftdir,regshifttype,memshifttype
		rw	condcodes
		rw	short_pc_offset,pc_offset
		rw	immoffs,trapnum,bit_instr_type

;
; note: GetWord & GetLong do not modify d1
;
GetWord		movem.l	d1/a3,-(sp)
		move.l	a5,a3
		move.w	(a4)+,d0
		move.w	d0,-(sp)
		moveq	#4,d1
		bsr	put_hexnum1
		move.w	(sp)+,d0
		move.b	#SPACE,(a3)+
		move.l	a3,a5
		movem.l	(sp)+,d1/a3
		rts

GetLong		movem.l	d1/a3,-(sp)
		move.l	a5,a3
		move.l	(a4)+,d0
		move.l	d0,-(sp)
		moveq	#8,d1
		bsr	put_hexnum1
		move.l	(sp)+,d0
		move.b	#SPACE,(a3)+
		move.l	a3,a5
		movem.l	(sp)+,d1/a3
		rts

datar11		move.b	#'d',(a3)+
num11		move.w	d7,d0
		lsr.w	#8,d0
		lsr.w	#1,d0
num2		and.b	#7,d0
nump		add.b	#'0',d0
		move.b	d0,(a3)+
		rts

datar2		move.b	#'d',(a3)+
		move.w	d7,d0
		bra.s	num2

addrr11		move.w	d7,d0
		lsr.w	#8,d0
		lsr.w	#1,d0
		bra.s	ar01

addrr2		move.w	d7,d0
ar01		and.w	#7,d0
		bra	ardirect

immediate_quick	move.w	d7,d0
		lsr.w	#8,d0
		lsr.w	#1,d0
		and.b	#7,d0
		bne.s	01$
		moveq	#8,d0
01$		move.b	#'#',(a3)+
		bra.s	nump

size_specifier	tst.w	d4
		beq.s	invalid
		move.b	#'.',(a3)+
		move.b	sizes-1(pc,d4.w),(a3)+
		rts

sizes		dc.b	'bwl'

nothing_txt	dc.b	'nothing',0
pc_txt		dc.b	'(pc',0
zero_txt	dc.b	'dc.w',TAB,'$00',0

		even

invalid		move.l	d4,d0		restore stack pointer
		move.l	d5,d1
		swap	d1
		move.w	d1,d0
		move.l	d0,sp

		move.l	d3,a3
		moveq	#'?',d0
		move.b	d0,(a3)+
		move.b	d0,(a3)+
		move.b	d0,(a3)+
		move.l	a6,a4
		move.l	d3,a0
		moveq	#SPACE,d0
		moveq	#18-1,d1
01$		move.b	d0,-(a0)
		dbf	d1,01$
		moveq	#-1,d0
		bra	dis_end

immediate_data	move.b	#'#',(a3)+
		move.w	d4,d0
		add.w	d0,d0
		jmp	immsizes(pc,d0.w)

immsizes	bra.s	invalid
		bra.s	imm_byte
		bra.s	imm_word
; imm_long
		bsr	GetLong
		bra	puthex_68

imm_byte	bsr	GetWord
		moveq	#2,d1
		bra	put_hexnum

imm_word	bsr	GetWord
		moveq	#4,d1
		bra	put_hexnum

immoffs		bsr	GetWord
sword		ext.l	d0
		bra	put_signed_hexnum

immediate_moveq		;%%signed number
		move.b	#'#',(a3)+
		move.w	d7,d0
		ext.w	d0
		bra.s	sword

sr_or_ccr	tst.w	d4
		beq	invalid
		cmp.w	#3,d4
		beq	invalid
		cmp.w	#1,d4
		beq.s	o_ccr
		move.b	#'s',(a3)+
		bra.s	o_sccr
o_ccr		moveq	#'c',d0
		move.b	d0,(a3)+
		move.b	d0,(a3)+
o_sccr		move.b	#'r',(a3)+
		rts

shiftdir	moveq	#'r',d0
		btst	#8,d7
		beq.s	01$
		moveq	#'l',d0
01$		move.b	d0,(a3)+
		rts

regshifttype	move.w	d7,d0
		lsr.w	#3,d0
		bra.s	shifttype

memshifttype	move.w	d7,d0
		lsr.w	#8,d0
		lsr.w	#1,d0

shifttype	and.w	#3,d0
		add.w	d0,d0
		lea	instradrs(pc),a0
		add.w	d0,a0
		add.w	(a0),a0
01$		bra	put_str

condcodes	move.w	d7,d1
		lsr.w	#8,d1
		and.w	#$0f,d1
		cmp.w	#2,d1
		bcc.s	02$
		move.w	d7,d0
		and.w	#$f000,d0
		cmp.w	#$6000,d0
		beq.s	02$
		moveq	#'t',d0
		tst.w	d1
		beq.s	01$
		moveq	#'f',d0
01$		move.b	d0,(a3)+
		rts

02$		add.w	d1,d1
		lea	ccodes(pc,d1.w),a0
		move.b	(a0)+,(a3)+
		move.b	(a0)+,(a3)+
		rts

ccodes		dc.b	'rasrhilscccsneeqvcvsplmigeltgtle'
		dc.b	'hslo'
		even

short_pc_offset	move.b	d7,d0
		ext.w	d0
		ext.l	d0
		add.l	a4,d0
		bra.s	put_addr

pc_offset	move.l	a4,d1
		bsr	GetWord
		ext.l	d0
		add.l	d1,d0
put_addr	bra	puthex_68

trapnum		move.b	#'#',(a3)+
		move.w	d7,d0
		and.w	#$0f,d0
		moveq	#2,d1
		bra	put_hexnum

bit_instr_type	move.w	d7,d0
		lsr.w	#4,d0
		and.w	#$0c,d0
		lea	btypes(pc,d0.w),a0
01$		bra	put_str

btypes		dc.b	'tst',0,'chg',0,'clr',0,'set',0
		even

eacategories	dc.b	DATA!ALTER!NIMM			; 0 - data reg direct
		dc.b	ALTER!NIMM			; 1 - address reg direct
		dc.b	DATA!MEMORY!CONTROL!ALTER!NIMM	; 2 - address reg indirect
		dc.b	DATA!MEMORY!ALTER!NIMM		; 3 - addr. reg. ind. ++
		dc.b	DATA!MEMORY!ALTER!NIMM		; 4 - addr. reg. ind. --
		dc.b	DATA!MEMORY!CONTROL!ALTER!NIMM	; 5 - addr. reg. ind. disp.
		dc.b	DATA!MEMORY!CONTROL!ALTER!NIMM	; 6 - addr. reg. ind. index
		dc.b	DATA!MEMORY!CONTROL!ALTER!NIMM	; 7 - absolute short
		dc.b	DATA!MEMORY!CONTROL!ALTER!NIMM	; 8 - absolute long
		dc.b	DATA!MEMORY!CONTROL!NIMM	; 9 - pc relative
		dc.b	DATA!MEMORY!CONTROL!NIMM	;10 - pc relative index
		dc.b	DATA!MEMORY			;11 - immediate
		even

effective_address
		move.w	d7,d0
		move.w	d7,d1
		lsr.w	#3,d1
;
; mode in d1, reg in d0
;
handle_ea	and.w	#7,d0
		and.w	#7,d1
		movem.l	d0-d1,-(sp)
		cmp.w	#7,d1
		bcs.s	01$
		add.w	d0,d1
01$		cmp.b	#1,d1			address register ?
		bne.s	02$
		cmp.b	d1,d4			byte size ?
		beq.s	03$
02$		move.b	eacategories(pc,d1.w),d0
		and.b	d6,d0
		cmp.b	d6,d0
		eor	#%100,ccr		reverse zero flag
03$		movem.l	(sp)+,d0-d1		flags don't change
		beq	invalid
		add.w	d1,d1
		jmp	eajumps1(pc,d1.w)

eajumps1	bra.s	drdirect
		bra.s	ardirect
		bra.s	arindirect
		bra.s	postincr
		bra.s	predecr
		bra.s	displ
		bra.s	indx
; handle 7-modes
		add.w	d0,d0
		jmp	eajumps2(pc,d0.w)

drdirect	move.b	#'d',(a3)+
		bra.s	putreg

ardirect	cmp.w	#7,d0
		beq.s	stackptr
		move.b	#'a',(a3)+
putreg		add.b	#'0',d0
		move.b	d0,(a3)+
		rts

arindirect	move.b	#'(',(a3)+
		bsr	ardirect
		move.b	#')',(a3)+
		rts

stackptr	move.b	#'s',(a3)+
		move.b	#'p',(a3)+
		rts

postincr	bsr	arindirect
		move.b	#'+',(a3)+
		rts

predecr		move.b	#'-',(a3)+
		bra.s	arindirect

imm_br		bra	immediate_data

eajumps2	bra.s	abs_short
		bra.s	abs_long
		bra.s	pcrel
		bra.s	pcrel_indx
		bra.s	imm_br
		bra.s	invalid_br
		nop
invalid_br	bra	invalid

displ		move.w	d0,-(sp)
		bsr	GetWord
		bsr	sword		;%% signed
		move.w	(sp)+,d0
		bra.s	arindirect

indx		move.w	d0,-(sp)
		bsr	GetWord
		move.w	d0,d2
		ext.w	d0
		bsr	sword		;%% signed
		move.w	(sp)+,d0
		move.b	#'(',(a3)+
		bsr	ardirect
		move.b	#',',(a3)+
		bsr.s	handle_index
		move.b	#')',(a3)+
		rts

abs_short	bsr	GetWord		;%%%signed
		bra	sword

abs_long	bsr	GetLong
		bra	put_addr

pcrel		move.l	a4,d1
		bsr	GetWord
		ext.l	d0
		add.l	d1,d0
		bsr	put_addr
		lea	pc_txt(pc),a0
		bsr.s	put_str
		bra.s	endpar

pcrel_indx	move.l	a4,d1
		bsr	GetWord
		move.w	d0,d2
		ext.w	d0
		ext.l	d0
		add.l	d1,d0
		bsr	put_addr
		lea	pc_txt(pc),a0
		bsr.s	put_str
		move.b	#',',(a3)+
		bsr.s	handle_index
endpar		move.b	#')',(a3)+
		rts

put_str		move.b	(a0)+,(a3)+
		bne.s	put_str
		subq.l	#1,a3
		rts
;
; index word in d2
;
handle_index	move.w	d2,d0
		lsr.w	#8,d0
		lsr.w	#4,d0
		and.w	#7,d0
		tst.w	d2
		bmi.s	01$
		bsr	drdirect
		bra.s	02$
01$		bsr	ardirect
02$		move.b	#'.',(a3)+
		moveq	#'w',d0
		btst	#11,d2
		beq.s	03$
		moveq	#'l',d0
03$		move.b	d0,(a3)+
		rts

handle_move	moveq	#0,d6
		bsr	effective_address
		move.b	#',',(a3)+
		moveq	#DATA!ALTER,d6
		move.w	d7,d1
		lsr.w	#6,d1
		move.w	d1,d0
		lsr.w	#3,d0
		bra	handle_ea

handle_movem	bsr	GetWord		get register list in d0

		move.w	d7,d1
		and.w	#$0038,d1	get addressing mode bits in d1 (mode field)

		btst	#10,d7
		bne.s	fromMemory

		cmp.w	#$0018,d1
		beq	invalid		movem regs,(An)+ is invalid

		cmp.w	#$0020,d1	predecrement ?
		bne.s	02$

		move.w	d0,d1
		move.w	#16-1,d5	reverse register list
01$		roxr.w	#1,d1
		roxl.w	#1,d0
		dbf	d5,01$
02$		bsr.s	put_reglist
		move.b	#',',(a3)+
		bra	effective_address

fromMemory	cmp.w	#$0020,d1
		beq	invalid		;movem	-(An),regs is invalid

		move.w	d0,d5
		bsr	effective_address
		move.b	#',',(a3)+
		move.w	d5,d0
; drop to put_reglist

put_reglist	tst.l	d0
		bne.s	reglist1
		lea	nothing_txt(pc),a0
		bra	put_str

reglist1	movem.l	d3-d5,-(sp)
		move.w	d0,d2
		moveq	#0,d4		flag: registers found
		moveq	#'d',d5		register type
		bsr.s	regset
		moveq	#'a',d5
		bsr.s	regset
		movem.l	(sp)+,d3-d5
		rts

regset		moveq	#0,d3		flag: in range
		moveq	#0,d1		bit counter
rloop1		lsr.w	#1,d2
		bcc.s	nlist1
		tst.w	d3
		bne.s	inrange
		cmp.w	#7,d1
		beq.s	nostartrange
		btst	d3,d2		d3 is zero if we got here...
		beq.s	nostartrange
		tst.w	d4
		beq.s	strng1
		move.b	#'/',(a3)+
strng1		bsr.s	putd1reg
		move.b	#'-',(a3)+
		moveq	#-1,d3		now in range
		bra.s	nlist1
inrange		cmp.w	#7,d1
		beq.s	endrange
		btst	#0,d2
		bne.s	nlist1
endrange	bsr.s	putd1reg
		moveq	#0,d3		not in range
		moveq	#-1,d4		regs found
		bra.s	nlist1
nostartrange	tst.l	d4
		beq.s	no_found
		move.b	#'/',(a3)+
no_found	bsr.s	putd1reg
		moveq	#-1,d4
nlist1		addq.w	#1,d1
		cmp.w	#8,d1
		bcs.s	rloop1
		rts

putd1reg	move.b	d5,(a3)+
		move.b	d1,d0
		and.b	#7,d0
		add.b	#'0',d0
		move.b	d0,(a3)+
		rts


*** STRING OUTPUT ***
;string pointer in A1
putstring	move.b	(a1)+,(a3)+
		bne.s	putstring
		subq.l	#1,a3
		rts

;
; memory info
;
memoryinfo	addq.l	#1,a3
		bsr	get_expr
		move.l	d0,d5

; walk the memory list

		call	ExecBase,Forbid

		move.l	MemList(a6),a4

01$		tst.l	(a4)
		beq.s	09$

		cmp.l	MH_LOWER(a4),d5
		bcs.s	08$
		cmp.l	MH_UPPER(a4),d5
		bcc.s	08$

; address is in this memory region

		move.l	MH_FIRST(a4),d2
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

08$		move.l	(a4),a4
		bra.s	01$

09$		suba.l	a4,a4

10$		call	Permit

; if a4 is zero, location is not in memory list
; else if d2 is zero, location is allocated, else free

		startline
		move.l	d5,d0
		bsr	puthex_68
		move.b	#':',(a3)+
		move.b	#SPACE,(a3)+

		move.l	a4,d0
		bne.s	100$
		lea	notmemtxt(pc),a1
		bsr	putstring
		bra.s	999$

100$		btst	#MEMB_CHIP,MH_ATTRIBUTES+1(a4)
		beq.s	101$
		move.l	#'chip',d0
		bsr	PutLong
		move.b	#SPACE,(a3)+

101$		btst	#MEMB_FAST,MH_ATTRIBUTES+1(a4)
		beq.s	102$
		move.l	#'fast',d0
		bsr	PutLong
		move.b	#SPACE,(a3)+

102$		tst.l	d2
		beq.s	103$
		move.l	#'not ',d0
		bsr	PutLong

103$		lea	allotxt(pc),a1
		bsr	putstring

999$		clr.b	(a3)
		bsr	printstring

;
; check if location is in a hunk and print hunk number if it is
;
		move.l	SegList(a5),d0
		moveq	#0,d4
500$		lsl.l	#2,d0
		beq.s	599$
		move.l	d0,a2
		lea	4(a2),a0
		cmp.l	a0,d5
		bcs.s	510$
		move.l	-4(a2),d0
		lea	-4(a2,d0.L),a1
		cmp.l	a1,d5
		bcc.s	510$

		move.l	d4,d0
		lea	inhunkfmt(pc),a0
		bsr	printf
		bra.s	599$

510$		move.l	(a2),d0
		addq.l	#1,d4
		bra.s	500$

599$		bra	mainloop


**** DISPLAY MEMORY ****
memdisplay	move.b	(a3),d0
		bsr	tolower
		cmp.b	#'i',d0
		beq	memoryinfo
		bsr	getparams
		move.l	ConsoleUnit(a5),a0
		cmp.w	#65,cu_XMax(a0)
		scs	d6
		move.l	Addr(A5),A4

disploop	startline
		move.l	A4,D0
		bsr	puthex1_68
		putchr	<':'>
		putchr	SPACE
		moveq	#16/4-1,D2
01$		move.l	(A4)+,D0
		bsr	phex1_8
		putchr	SPACE
		dbf	D2,01$
		sub.w	#16,A4
		putchr	SPACE
		tst.b	d6
		bmi.s	100$
		putchr	<''''>
100$		moveq	#16-1,D2
02$		move.b	(A4)+,D0	;printable codes are $20-$7F and $A0-$FF
		cmp.b	#SPACE+$80,D0
		bcc.s	03$
		cmp.b	#SPACE,D0
		bge.s	03$		;note: signed comparison handles correctly codes >= $80
		move.b	#'.',D0
03$		move.b	D0,(A3)+
		dbf	D2,02$
		tst.b	d6
		bmi.s	101$
		putchr	<''''>
101$		endline
		bsr	printstring
		bsr	CheckEnd
		bne.s	disploop

		move.l	A4,Addr(A5)
		bra	mainloop

**** SKIP SPACES ****
skipspaces	cmp.b	#SPACE,(A3)+
		beq.s	skipspaces
		subq.l	#1,A3
		rts

;
;
; The expression evaluation routines
;
; NOTE: get_expr must not change d1/a0/a1
;
**********************************************************
*							 *
* operations supported (32 bit integer arithmetic):	 *
*							 *
* + - * / %%  -- add, subtract, multiply, divide, modulo *
*							 *
* !| & ^      -- bitwise or, and, xor			 *
* << >>       -- left & right bit shifts		 *
* - + ~       -- unary plus, minus, bit complement	 *
*							 *
* '*'	      -- 'current address'			 *
*							 *
* hunk(n)     -- start address of a hunk		 *
* hlen(n)     -- length of a hunk			 *
* abs(x)      -- absolute value				 *
* peek(addr)  -- byte value of memory location		 *
* peekw(addr) -- word value of memory location		 *
* peekl(addr) -- longword value of memory location	 *
*							 *
* numbers can be decimal (no prefix or '_'-prefix),	 *
* hex($-prefix), octal (@-prefix), binary (%-prefix)	 *
* or strings of ascii-characters between single quotes.	 *
*							 *
*		no overflow checking!			 *
*							 *
**********************************************************

get_expr	movem.l	d1-d2/a0-a1/a6,-(sp)
		bsr.s	get_ex1
		move.l	d0,d2
get_expr_loop	bsr	skipspaces
		move.b	(a3)+,d0
		cmp.b	#'+',d0
		bne.s	expr1
		bsr.s	get_ex1
		add.l	d0,d2
		bra.s	get_expr_loop
expr1		cmp.b	#'-',d0
		bne.s	expr2
		bsr.s	get_ex1
		sub.l	d0,d2
		bra.s	get_expr_loop
expr2		cmp.b	#'!',d0
		beq.s	bit_or
		cmp.b	#'|',d0
		bne.s	expr3
bit_or		bsr.s	get_ex1
		or.l	d0,d2
		bra.s	get_expr_loop
expr3		subq.l	#1,a3
		move.l	d2,d0
		movem.l	(sp)+,d1-d2/a0-a1/a6
		rts

get_ex1		move.l	d2,-(sp)
		bsr.s	get_ex2
		move.l	d0,d2
get_ex1_loop	bsr	skipspaces
		move.b	(a3)+,d0
		cmp.b	#'*',d0
		bne.s	ex11
		bsr.s	get_ex2
		move.l	d2,d1
		bsr	multiply
		move.l	d0,d2
		bra.s	get_ex1_loop
ex11		cmp.b	#'/',d0
		bne.s	ex12
		bsr.s	get_ex2
		tst.l	d0
		beq	expr_error		;divide by zero
		move.l	d2,d1
		bsr	divide
		move.l	d0,d2
		bra.s	get_ex1_loop
ex12		cmp.b	#'%',d0
		bne.s	ex13
		cmp.b	#'%',(a3)
		bne.s	ex13
		addq.l	#1,a3
		bsr.s	get_ex2
		tst.l	d0
		beq	expr_error		;divide by zero
		move.l	d2,d1
		bsr	modulo
		move.l	d0,d2
		bra.s	get_ex1_loop
ex13		cmp.b	#'&',d0
		bne.s	ex14
		bsr.s	get_ex2
		and.l	d0,d2
		bra.s	get_ex1_loop
ex14		cmp.b	#'^',d0
		bne.s	ex15
		bsr.s	get_ex2
		eor.l	d0,d2
		bra.s	get_ex1_loop
ex15		subq.l	#1,a3
		move.l	d2,d0
		move.l	(sp)+,d2
		rts

get_ex2		move.l	d2,-(sp)
		bsr.s	get_ex3
		move.l	d0,d2
get_ex2_loop	bsr	skipspaces
		move.b	(a3)+,d0
		cmp.b	#'<',d0
		bne.s	ex21
		cmp.b	#'<',(a3)+
		bne	expr_error		;syntax error
		bsr.s	get_ex3
		lsl.l	d0,d2
		bra.s	get_ex2_loop
ex21		cmp.b	#'>',d0
		bne.s	ex22
		cmp.b	#'>',(a3)+
		bne	expr_error		;syntax error
		bsr.s	get_ex3
		lsr.l	d0,d2
		bra.s	get_ex2_loop
ex22		subq.l	#1,a3
		move.l	d2,d0
		move.l	(sp)+,d2
		rts

get_ex3		bsr	skipspaces
		move.b	(a3)+,d0
		cmp.b	#'(',d0
		bne.s	ex31
		bsr	get_expr
		bsr	skipspaces
		cmp.b	#')',(a3)+
		bne	expr_error		;right parenthesis expected
		rts
ex31		cmp.b	#'-',d0
		bne.s	ex32
		bsr.s	get_ex3
		neg.l	d0
		rts
ex32		cmp.b	#'+',d0
		beq.s	get_ex3
		cmp.b	#'~',d0
		bne.s	ex33
		bsr.s	get_ex3
		not.l	d0
		rts
ex33		cmp.b	#'''',d0
		beq	get_strnum
		cmp.b	#'$',d0		;hex prefix '$'
		bne.s	ex34
		moveq	#16,d0
		bra.s	gnum_j
ex34		cmp.b	#'@',d0		;octal prefix '@'
		bne.s	ex35
		moveq	#8,d0
		bra.s	gnum_j
ex35		cmp.b	#'%',d0		;binary prefix '%'
		bne.s	ex37
		moveq	#2,d0
gnum_j		bra	get_num
ex37		cmp.b	#'*',d0
		bne.s	ex37a
		move.l	Addr(a5),d0
		rts
ex37a		cmp.b	#'[',d0
		bne.s	ex38
		move.b	(a3)+,d0
		bsr	tolower
		lsl.w	#8,d0
		move.b	(a3)+,d0
		bsr	tolower
		lea	RegPC(a5),a0
		cmp.w	#'pc',d0
		beq.s	ex37x
		cmp.b	#'cc',d0
		bne.s	ex37b
		move.b	(a3),d0
		bsr	tolower
		cmp.b	#'r',d0
		bne.s	01$
		addq.l	#1,a3
01$		moveq	#0,d0
		move.b	RegCC(a5),d0
		bra.s	ex37z
ex37b		lea	AddrRegs+4*7(a5),a0
		cmp.b	#'sp',d0
		beq.s	ex37x
		sub.w	#'a0',d0
		bcs.s	ex37_err
		cmp.w	#7,d0
		bhi.s	ex37c
		lea	AddrRegs(a5),a0
		bra.s	ex37r
ex37c		sub.w	#'d0'-'a0',d0
		bcs.s	ex37_err
		cmp.w	#7,d0
		bhi.s	ex37_err
		lea	DataRegs(a5),a0
ex37r		lsl.w	#2,d0
		add.w	d0,a0
ex37x		move.l	(a0),d0
ex37z		cmp.b	#']',(a3)+
		beq.s	exrt01
ex37_err	bra	expr_error
ex38		subq.l	#1,a3
		bsr	get_token
		tst.l	d0
		bmi.s	ex38b
		lea	tokfuncs(pc),a0
		add.l	d0,d0
		add.l	d0,a0
		add.w	(a0),a0
		jmp	(a0)

ex38b		move.b	(a3),d0
		bsr	isalpha
		bcc.s	ex39b
		move.l	a3,a0
01$		move.b	(a0)+,d0
		bsr	isalnum
		bcs.s	01$
		subq.l	#1,a0
		move.b	(a0),d0
		clr.b	(a0)
		movem.l	d0/a0,-(sp)
		move.l	a3,a0
		bsr	findvar
		movem.l	(sp)+,d1/a0
		bcs.s	ex39
		move.b	d1,(a0)
		move.l	a0,a3
exrt01		rts

ex39		move.b	d1,(a0)
ex39b		moveq	#10,d0
		cmp.b	#'_',(a3)+		;decimal perix '_'
		beq.s	ex39x
		subq.l	#1,a3
		move.b	defbase(a5),d0
ex39x		bra	get_num

multiply	movem.l	d4-d6,-(sp)
		moveq	#0,d4
		tst.l	d0
		bpl.s	mul1
		neg.l	d0
		not.w	d4
mul1		tst.l	d1
		bpl.s	mul2
		neg.l	d1
		not.w	d4
mul2		move.l	d0,d5
		swap	d5
		mulu	d1,d5
		swap	d5
		move.l	d1,d6
		swap	d6
		mulu	d0,d6
		swap	d6
		add.l	d5,d6
		mulu	d1,d0
		add.l	d6,d0
		tst.w	d4
		bpl.s	mul9
		neg.l	d0
mul9		movem.l	(sp)+,d4-d6
		rts

divide		move.l	d4,-(sp)
		moveq	#0,d4
		tst.l	d0
		bpl.s	div02
		neg.l	d0
		not.w	d4
div02		tst.l	d1
		bpl.s	div03
		neg.l	d1
		not.w	d4
div03		bsr.s	div001
		tst.w	d4
		bpl.s	div04
		neg.l	d0
div04		move.l	(sp)+,d4
		rts

modulo		move.l	d4,-(sp)
		move.l	d0,d4
		bpl.s	mod01
		neg.l	d0
mod01		tst.l	d1
		bpl.s	mod02
		neg.l	d1
mod02		bsr.s	div001
		move.l	d1,d0
		tst.l	d4
		bpl.s	mod03
		neg.l	d0
mod03		move.l	(sp)+,d4
		rts

div001
;inputs: jaettava d1, jakaja d0, unsigned
;result: quotient in d0, remainder in d1
		movem.l	d4-d7,-(sp)
		moveq	#0,d5
		moveq	#32-1,d4
divloop		lsl.l	#1,d1
		roxl.l	#1,d5
		move.l	d5,d6
		sub.l	d0,d6
		bcs.s	diz02
		move.l	d6,d5
diz02		roxl.l	#1,d7
		dbf	d4,divloop
		move.l	d7,d0
		not.l	d0
		move.l	d5,d1
		movem.l	(sp)+,d4-d7
		rts

r_hunk		bsr.s	gethunk
		move.l	d1,d0
		addq.l	#4,d0
		bra.s	no_more_args

r_hlen		bsr.s	gethunk
		move.l	d1,a0
		move.l	-4(a0),d0
		subq.l	#8,d0
		bra.s	no_more_args

gethunk		bsr.s	get_first_arg
		move.l	SegList(a5),d1
01$		lsl.l	#2,d1
		beq	expr_error		;hunk not found
		tst.l	d0
		beq.s	02$
		move.l	d1,a0
		move.l	(a0),d1
		subq.l	#1,d0
		bra.s	01$
02$		rts

r_abs		bsr.s	get_first_arg
		tst.l	d0
		bpl.s	fcom
		neg.l	d0
fcom		bra.s	no_more_args

r_peek		bsr.s	get_first_arg
		move.l	d0,a0
		moveq	#0,d0
		move.b	(a0)+,d0
		bra.s	no_more_args

r_peekw		bsr.s	get_first_arg
		btst	#0,d0
		bne	expr_error		;odd address
		move.l	d0,a0
		moveq	#0,d0
		move.w	(a0),d0
		bra.s	no_more_args

r_peekl		bsr.s	get_first_arg
		btst	#0,d0
		bne	expr_error		;odd address
		move.l	d0,a0
		move.l	(a0),d0
		bra.s	no_more_args

r_avail		bsr.s	get_first_arg
		move.l	d0,d1
		call	ExecBase,AvailMem
		bra.s	no_more_args

get_first_arg	bsr	skipspaces
		cmp.b	#'(',(a3)+
		bne	expr_error		;left parenhesis expected
		bra	get_expr

;get_arg	bsr	skipspaces
;		cmp.b	#',',(a3)+
;		bne	expr_error		;comma expected
;		bra	get_expr

no_more_args	;D0 not changed!
		bsr	skipspaces
		cmp.b	#')',(a3)+
		bne	expr_error		;right parenthesis expected
		rts

get_num		;radix in D0
		movem.l	d2/d3,-(sp)
		move.l	d0,d2
		moveq	#0,d0
		move.l	a3,a0
getnum1		moveq	#0,d1
		move.b	(a3)+,d1
		cmp.b	#'a',d1
		bcs.s	getnum2
		and.b	#$df,d1
getnum2		sub.b	#'0',d1
		bcs.s	getnum9
		cmp.b	#10,d1
		bcs.s	getnum3
		cmp.b	#17,d1
		bcs.s	getnum9
		subq.b	#7,d1
getnum3		cmp.b	d2,d1
		bcc.s	getnum9
		move.l	d0,d3
		mulu	d2,d3
		swap	d0
		mulu	d2,d0
		swap	d0
		add.l	d3,d0
		add.l	d1,d0
		bra.s	getnum1
getnum9		subq.l	#1,a3
		cmp.l	a0,a3
		beq.s	expr_error		;empty number
		movem.l	(sp)+,d2/d3
		rts

get_strnum	moveq	#0,d0
strnum1		move.b	(a3)+,d1
		beq.s	expr_error		;syntax error
		cmp.b	#'''',d1
		beq.s	strnum2
strnum1a	lsl.l	#8,d0
		move.b	d1,d0
		bra.s	strnum1
strnum2		cmp.b	#'''',(a3)+
		beq.s	strnum1a
		subq.l	#1,a3
strnum9		rts

get_token	movem.l	d2/a2,-(sp)
		lea	tokentable(pc),a0
		moveq	#0,d0

gt1		move.l	a3,a2
		tst.w	(a0)
		beq.s	gt_nf
		move.l	a0,a1
		add.w	(a1),a1
gt2		move.b	(a2)+,d1
		move.b	(a1)+,d2
		or.b	#$20,d1
		cmp.b	#'a',d1
		bcs.s	gt2a
		cmp.b	#'z',d1
		bls.s	gt3
gt2a		tst.b	d2
		bne.s	gt_next
		move.l	a2,a3
		subq.l	#1,a3
		bra.s	gt_ret
gt3		cmp.b	d1,d2
		beq.s	gt2

gt_next		addq.l	#2,a0
		addq.l	#1,d0
		bra.s	gt1

gt_nf		moveq	#-1,d0
gt_ret		movem.l	(sp)+,d2/a2
		rts

expr_error	lea	expr_errtxt(pc),a0
		bra	errcom

tokentable	rw	hunk_t
		rw	hlen_t
		rw	abs_t
		rw	peek_t
		rw	peekw_t
		rw	peekl_t
		rw	avail_t
		dc.w	0

tokfuncs	rw	r_hunk
		rw	r_hlen
		rw	r_abs
		rw	r_peek
		rw	r_peekw
		rw	r_peekl
		rw	r_avail

hunk_t		dc.b	'hunk',0
hlen_t		dc.b	'hlen',0
abs_t		dc.b	'abs',0
peek_t		dc.b	'peek',0
peekw_t		dc.b	'peekw',0
peekl_t		dc.b	'peekl',0
avail_t		dc.b	'avail',0

		ds.w	0

;
; test if character in d0 is alphanumeric
; return carry set if yes, carry clear if no
; does not change d0
;
isalnum		cmp.b	#'0',d0
		bcs.s	no
		cmp.b	#'9',d0
		bls.s	yes
;
; test if character is alphabetic or '_'
;
isalpha		cmp.b	#'A',d0
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

*** Convert char in D0 to lower case ***
tolower		cmp.b	#'A',D0
		bcs.s	low1
		cmp.b	#'Z',D0
		bhi.s	low1
		bset	#5,D0
low1		rts

*** ASSEMBLE ***
* command format:
* a       :assembles at the current address, asks instruction
* a <addr>:assembles at <addr>, asks instruction
* a <addr> <instruction> : assembles <instruction> at <addr>
* in all cases, if no errors occurred, prompts '<next_addr>:'
* and asks a new instruction. use <CR> to exit this mode
* Ctrl-E can be used to edit the disassembled instruction at this location
assemble	bsr	skipspaces
		tst.b	(A3)
		bne.s	assem_01
		move.l	Addr(A5),D0
		bra.s	assem_02
assem_01	bsr	get_expr
		btst	#0,D0
		bne	error	;assembling to odd address is illegal
		move.l	D0,Addr(A5)
assem_02	move.l	D0,EndAddr(A5)
		bsr	skipspaces
		tst.b	(A3)
		bne.s	assem1a

assem1		move.l	Addr(A5),D0
		move.l	D0,EndAddr(A5)
		lea	assemfmt(pc),a0
		bsr	printf_window
		moveq	#1,D0

assem1a0	bsr	GetInput
		tst.w	D0		;check for Ctrl-E (GetInput returns -1)
		bpl.s	assem1a1	;branch if not Ctrl-E
*** disassemble at current address and put result in input buffer ***
		move.l	Addr(A5),A4
		startline
		bsr	disassemble
		lea	InputBuffer(a5),a1
		moveq	#LF,D0
300$		move.b	(A0)+,(A1)+	copy instruction to input buffer
		cmp.b	(A0),D0 	until LF found
		bne.s	300$
		clr.b	(A1)		mark end of string
		moveq	#2,D0		GetInput mode: edit existing line
		bra.s	assem1a0
assem1a1	tst.b	(A3)
		beq	mainloop

assem1a		move.l	A3,A1
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
asmlow1		move.b	D0,(A1)+
		bra.s	lowerloop

asmlow9		lea	instradrs(pc),A2
		moveq	#-1,D1
find_instr_loop	;find the instruction opcode from the opcode table
		addq.w	#1,D1
		move.l	A3,A1
		move.l	A2,A0
		tst.w	(A2)+
		beq.s	try_branch
		add.w	(A0),A0

comp_str	tst.b	(A0)
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
br_err		bra	error

branch_1	lsl.w	#8,D1
		or.w	#$6000,D1
		cmp.b	#'.',(A3)
		bne.s	long_branch
		addq.l	#1,A3
		cmp.b	#'l',(A3)+
		beq.s	long_branch
		cmp.b	#'s',-1(A3)
		bne.s	br_err
		bsr	get_expr
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

long_branch	bsr	get_expr
		sub.l	Addr(A5),D0
		subq.l	#2,D0
		move.w	D0,D2
		ext.l	D2
		cmp.l	D0,D2
		bne.s	out_of_range
		exg	D0,D1
		bra	two_words_instr

instr_found	move.l	A1,A3
		add.w	D1,D1
		lea	instrjumps(pc),A0
		add.w	D1,A0
		add.w	(A0),A0
		jmp	(A0)

out_of_range	lea	outrangetxt(pc),a0
		bra	errcom

try_Scc		cmp.b	#'s',(A3)
		bne.s	try_DBcc
		addq.l	#1,A3
		cmp.b	#'f',(A3)+
		bne.s	Scc_1
		moveq	#1,D1
		bra.s	Scc_3
Scc_1		cmp.b	#'t',-1(A3)
		bne.s	Scc_2
		moveq	#0,D1
		bra.s	Scc_3
Scc_2		subq.l	#1,A3
		bsr	check_cond
		tst.w	D1
		bmi.s	cc_err
		cmp.w	#2,D1
		bcs.s	cc_err
Scc_3		move.w	D1,D3
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

cc_err		bra	error

try_DBcc	cmp.b	#'d',(A3)+
		bne.s	cc_err
		cmp.b	#'b',(A3)+
		bne.s	try_dc
		cmp.b	#'t',(A3)+
		bne.s	DBcc_1
		moveq	#0,D1
		bra.s	DBcc_3
DBcc_1		cmp.b	#'f',-1(A3)
		bne.s	DBcc_2
DBcc_1a		moveq	#1,D1
		bra.s	DBcc_3
DBcc_2		subq.l	#1,A3
		bsr	check_cond
		tst.w	D1
		beq.s	DBcc_1a
		bmi.s	cc_err
		cmp.w	#1,D1
		beq.s	cc_err
DBcc_3		move.w	D1,D3
		bsr	skipspaces
		bsr	getdreg
		move.w	d1,d0
		lsl.w	#8,D3
		or.w	D0,D3
		or.w	#$50C8,D3
		bsr	SkipComma
		bsr	get_expr	
		sub.l	Addr(A5),D0
		subq.l	#2,D0
		move.l	D0,D1
		ext.l	D0
		cmp.l	D0,D1
		bne	out_of_range
		move.w	D3,D0
		bra	two_words_instr
try_dc		cmp.b	#'c',-1(A3)
		bne.s	cc_err
		bsr	GetSize	;*** DC.B, DC.W, DC.L ***
		bsr	skipspaces
		move.l	Addr(A5),A0
		move.w	size(A5),D0
		beq.s	dc_byte
		subq.w	#1,D0
		beq.s	dc_word
dc_long		bsr	get_expr
		move.l	D0,(A0)+
		bsr	skipspaces
		cmp.b	#COMMA,(a3)+
		beq.s	dc_long
		bra.s	dc_exit
dc_word		bsr	get_expr
		move.w	D0,(A0)+
		bsr	skipspaces
		cmp.b	#COMMA,(a3)+
		beq.s	dc_word
		bra.s	dc_exit
dc_byte		bsr	getstring
		move.l	A1,A0
dc_exit		move.l	A0,D0	;align address to word boundary
		btst	#0,D0
		beq.s	dc_exit1
		addq.l	#1,D0

dc_exit1	move.l	D0,Addr(A5)
		bra.s	dc_exit2

assem9		bsr	skipspaces
		tst.b	(a3)
		bne	error

		lea	UpAndClearEol(pc),A0	;move cursor to previous line and clear the line
		bsr	printstring_a0_window
		move.l	EndAddr(A5),A4
		startline
		bsr	disassemble
		bsr	printstring_window
dc_exit2	bra	assem1

; check if the chars at A3 form a condition code
; result returned in D1, condition number or -1 if not found
; now unserstands hs and lo-conditions (same as cc and cs)
check_cond	move.b	(A3)+,D0
		lsl.w	#8,D0
		move.b	(A3)+,D0
		lea	ccodes(pc),A0
		moveq	#0,D1

chk_cond1	cmp.w	(A0)+,D0
		beq.s	cond9
		addq.w	#1,D1
		cmp.w	#18,D1
		bcs.s	chk_cond1
		moveq	#-1,D1
cond9		cmp.w	#16,d1
		bcs.s	cond10
		sub.w	#12,d1
cond10		rts

*** SHIFT INSTRUCTIONS ***
as_asm		move.w	#$E000,D3
		bra.s	shift_instr

ls_asm		move.w	#$E008,D3
		bra.s	shift_instr

rox_asm		move.w	#$E010,D3
		bra.s	shift_instr

rot_asm		move.w	#$E018,D3

shift_instr	cmp.b	#'r',(A3)+		get direction
		beq.s	shift_1
		cmp.b	#'l',-1(A3)
		bne.s	sh_err
		bset	#8,D3
shift_1		cmp.b	#'.',(A3)
		bne.s	shift_mem
		bsr	GetSize
		move.w	size(A5),D0
		lsl.w	#6,D0
		or.w	D0,D3
		bsr	skipspaces
		cmp.b	#'#',(A3)
		bne.s	count_in_reg
		addq.l	#1,A3
		bsr	get_expr_1_8
		bra.s	shift_2

count_in_reg	bsr	getdreg
		move.w	d1,d0
		bset	#5,D3
shift_2		lsl.w	#8,D0
		lsl.w	#1,D0
		or.w	D0,D3
		bsr	SkipComma
		bsr	getdreg
		move.w	d1,d0
		or.w	D3,D0
		bra	one_word_instr

sh_err		bra	error

shift_mem	addq.l	#2,Addr(A5)
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
add_asm		move.w	#$D000,D3
		bra.s	a_s_asm

sub_asm		move.w	#$9000,D3

a_s_asm		cmp.b	#'x',(A3)
		bne.s	no_as_ext
		addq.l	#1,A3
		bsr	GetSize
		bset	#8,D3
		bra	ext_as_asm

no_as_ext	addq.l	#2,Addr(A5)
		cmp.b	#'q',(A3)
		bne.s	no_as_quick
		addq.l	#1,A3
		bsr	GetSize
		bsr	skipspaces
		cmp.b	#'#',(A3)+
		bne.s	as_err
		bsr	get_expr_1_8
		lsl.w	#8,D0
		lsl.w	#1,D0
		cmp.w	#$9000,D3
		bne.s	as_quick_1
		bset	#8,D0

as_quick_1	or.w	#$5000,D0
		move.w	size(A5),D1
		lsl.w	#6,D1
		or.w	D1,D0
		move.w	D0,D3
		bsr	SkipComma
		bsr	GetEA
		tst.w	size(A5)
		bne.s	as_quick_z1
		cmp.w	#1,D1
		beq.s	as_err

as_quick_z1	lsl.w	#3,D1
		or.w	D1,D0
;#
;# fixed addq/subq addressing mode checking 1989-08-28
;#
		cmp.w	#$39,d0
		bhi.s	as_err
		or.w	D3,D0
zcom_1		bra	zzcom

as_err		bra	error

no_as_quick	cmp.b	#'i',(A3)
		bne.s	no_as_imm
		addq.l	#1,A3
		bsr	GetSize
		bsr	skipspaces
		cmp.b	#'#',(A3)+
		bne.s	as_err

as_imm_1	bsr	PutImm
		bsr	SkipComma
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

as_addr_imm_1	lsl.w	#8,D0
		lsl.w	#1,D0
		move.w	size(A5),D1
		beq.s	as_err
		subq.w	#1,D1
		lsl.w	#8,D1
		or.w	D1,D0
		or.w	#$FC,D0
		or.w	D3,D0
		bra.s	zcom_1

no_as_imm	moveq	#0,D5
		cmp.b	#'a',(A3)
		bne.s	as_norm_1
		moveq	#-1,D5
		addq.l	#1,A3

as_norm_1	bsr	GetSize
		bsr	skipspaces
		tst.l	D5
		bne.s	as_norm_2
		cmp.b	#'#',(A3)+
		beq.s	as_imm_1
		subq.l	#1,A3

as_norm_2	cmp.b	#'d',(A3)
		beq.s	as_data_reg_source
		bsr	GetEA
		tst.w	size(A5)
		bne.s	as_norm_3
		cmp.w	#1,D1		address register diret can't be used
		beq.s	as_err2		with byte size

as_norm_3	lsl.w	#3,D1
		or.w	D1,D0
		or.w	D0,D3
		bsr	SkipComma
		cmp.b	#'d',(A3)
		bne.s	try_as_addr
		tst.l	D5
		bne.s	as_err2
		bsr	getdreg
		move.w	d1,d0
		lsl.w	#8,D0
		lsl.w	#1,D0
		or.w	D3,D0
		move.w	size(A5),D1
		lsl.w	#6,D1
		or.w	D1,D0
		bra.s	zcom_2
as_err2		bra	error

try_as_addr	bsr	getareg
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
zcom_2		bra	zzcom

as_data_reg_source
		bsr	getdreg
		lsl.w	#8,D1
		lsl.w	#1,D1
		or.w	D1,D3
		bsr	SkipComma
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

as_datasource_1	cmp.w	#1,D1
		beq.s	as_special
		tst.l	d5
		bne.s	as_err2
		lsl.w	#3,D1
		or.w	D1,D0
		cmp.w	#$39,D0
		bhi.s	as_err2
		or.w	D3,D0
		bset	#8,D0
as_ds_1		move.w	size(A5),D1
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
cmp_asm		cmp.b	#'m',(A3)
		bne.s	no_mem_cmp
		addq.l	#1,A3
		bsr	GetSize	;CMPM
		bsr	skipspaces
		cmp.b	#'(',(A3)+
		bne.s	cmp_err
		bsr	getareg
		move.w	d1,d3
		cmp.b	#')',(A3)+
		bne.s	cmp_err
		cmp.b	#'+',(A3)+
		bne.s	cmp_err
		bsr	SkipComma
		cmp.b	#'(',(A3)+
		bne.s	cmp_err
		bsr	getareg
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

cmp_err		bra	error

no_mem_cmp	addq.l	#2,Addr(A5)
		cmp.b	#'i',(A3)
		bne.s	no_cmp_imm
		addq.l	#1,A3
		bsr	GetSize
		bsr	skipspaces
		cmp.b	#'#',(A3)+
		bne.s	cmp_err

cmp_imm_1	bsr	PutImm
		bsr	SkipComma
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

cmp_addr_imm1	lsl.w	#8,D0
		lsl.w	#1,D0
		move.w	size(A5),D1
		beq.s	cmp_err
		subq.w	#1,D1
		lsl.w	#8,D1
		or.w	D1,D0
		or.w	#$B0FC,D0
		bra	zzcom

no_cmp_imm	moveq	#0,D5
		cmp.b	#'a',(A3)
		bne.s	no_cmp_addr
		addq.l	#1,A3
		moveq	#-1,D5

no_cmp_addr	bsr	GetSize
		bsr	skipspaces
		tst.l	D5
		bne.s	cmp_1
		cmp.b	#'#',(A3)+
		beq.s	cmp_imm_1
		subq.l	#1,A3
cmp_1		bsr	GetEA
		cmp.w	#1,D1
		bne.s	cmp_2
		tst.w	size(A5)
		beq.s	cmp_err2
cmp_2		lsl.w	#3,D1
		or.w	D1,D0
		move.w	D0,D3
		bsr	SkipComma
		bsr	getreg
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

cmp_err2	bra	error

cmp_addr	move.w	size(A5),D0
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
and_asm		move.w	#$C000,D3
		bra.s	a_o_asm

or_asm		move.w	#$8000,D3

a_o_asm		addq.l	#2,Addr(A5)
		cmp.b	#'i',(A3)
		bne.s	a_o_2
		addq.l	#1,A3
		cmp.b	#'.',(A3)
		beq.s	a_o_0
		lsr.w	#5,D3
		and.w	#$0200,D3
		bra	logical_status

a_o_0		bsr	GetSize
		bsr	skipspaces
		cmp.b	#'#',(A3)+
		bne.s	a_o_err
a_o_imm	bsr	PutImm
		bsr	SkipComma
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

a_o_err		bra	error

a_o_2		bsr	GetSize
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
		lsl.w	#8,D1
		lsl.w	#1,D1
		or.w	D1,D3
		bsr	SkipComma
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
zcom_3		bra	zzcom

a_o_3a		move.w	D0,D7
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

reg_dest	bsr	GetEA
		cmp.w	#1,D1
		beq.s	a_o_err2
		lsl.w	#3,D1
		or.w	D1,D0
;# this check prevented the use of pcrelative addressing modes
;# with and/or - fixed 1989-08-27
;#		cmp.w	#$39,D0
;#		bhi.s	a_o_err2
		or.w	D0,D3
		bsr	SkipComma
		bsr	getdreg
		move.w	d1,d0
		lsl.w	#8,D0
		lsl.w	#1,D0
		or.w	D3,D0
		bra.s	zcom_3

a_o_err2	bra	error

*** ABCD & SBCD & ADDX & SUBX ***
abcd_asm	move.w	#$C100,D3
		bra.s	bcd_asm

sbcd_asm	move.w	#$8100,D3

bcd_asm		clr.w	size(A5)
ext_as_asm	bsr	skipspaces
		cmp.b	#'d',(A3)
		bne.s	bcd_mem
		bsr	getdreg
		or.w	D1,D3
		bsr	SkipComma
		bsr	getdreg
bcd_1		lsl.w	#8,D1
		lsl.w	#1,D1
		or.w	D3,D1
		move.w	size(A5),D0
		lsl.w	#6,D0
		or.w	D1,D0
		bra	one_word_instr

bcd_mem		cmp.b	#'-',(A3)
		bne.s	err_bcd
		addq.l	#1,a3
		cmp.b	#'(',(A3)+
		bne.s	err_bcd
		bsr	getareg
		or.w	d1,d3
		bset	#3,d3
		cmp.b	#')',(A3)+
		bne.s	err_bcd
		bsr	SkipComma
		cmp.b	#'-',(A3)+
		bne.s	err_bcd
		cmp.b	#'(',(A3)+
		bne.s	err_bcd
		bsr	getareg
		cmp.b	#')',(A3)+
		beq.s	bcd_1
err_bcd		bra	error

*** EOR ***
eor_asm		addq.l	#2,Addr(A5)
		moveq	#0,D5
		cmp.b	#'i',(A3)
		bne.s	eor_1
		addq.l	#1,A3
		cmp.b	#'.',(A3)
		beq.s	eor_0
		move.w	#$0A00,D3
		bra.s	logical_status

eor_0		moveq	#-1,D5
eor_1		bsr	GetSize
		bsr	skipspaces
		cmp.b	#'#',(A3)
		bne.s	no_eor_imm
		addq.l	#1,A3
		bsr	PutImm
		bsr	SkipComma
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

no_eor_imm	tst.l	D5
		bne.s	err_bcd
		bsr	getdreg
		move.w	D1,D3
		bsr	SkipComma
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
zcom_4		bra	zzcom

*** AND & OR & EOR SR & CCR ***
logical_status	or.w	#$3C,D3
		bsr	skipspaces
		cmp.b	#'#',(A3)+
		bne.s	move_err
		move.w	#WSIZE,size(A5)
		bsr	PutImm
		bsr	SkipComma
		cmp.b	#'s',(A3)
		bne.s	no_log_sr
		addq.l	#1,A3
		cmp.b	#'r',(A3)+
		bne.s	move_err
		or.w	#$40,D3
		bra.s	log_st1

no_log_sr	moveq	#'c',D0
		cmp.b	(A3)+,D0
		bne.s	move_err
		cmp.b	(A3)+,D0
		bne.s	move_err
		cmp.b	#'r',(a3)+
		bne.s	move_err

log_st1		move.w	D3,D0
		bra.s	zcom_4

*** MOVE ***
move_asm	cmp.b	#'q',(A3)
		bne.s	no_move_quick
		addq.l	#1,A3
		bsr	skipspaces
		cmp.b	#'#',(A3)+
		bne.s	move_err
		bsr	get_expr
		move.b	D0,D2
		bsr	SkipComma
		bsr	getdreg
		move.w	d1,d0
		lsl.w	#8,D0
		lsl.w	#1,D0
		or.w	#$7000,D0
		move.b	D2,D0
		bra	one_word_instr

move_err	bra	error

no_move_quick	cmp.b	#'p',(A3)
		beq	move_peripheral
		cmp.b	#'a',(A3)
		bne.s	no_move_addr
		moveq	#-1,D5
		addq.l	#1,A3
		bra	normal_move_2

no_move_addr	cmp.b	#'m',(A3)
		beq	movem_asm
		cmp.b	#'.',(A3)
		beq	normal_move
		bsr	skipspaces
;#
;# move sp,usp didn't work because conflict with 's' in 'sr'
;# fixed in 1989-11-30
;#
		cmp.b	#'s',(A3)
		bne.s	01$
		cmp.b	#'p',1(a3)
		bne.s	move_status
		bra.s	move_to_usp
01$		cmp.b	#'u',(A3)
		beq.s	move_from_usp
		cmp.b	#'a',(A3)
		bne.s	move_to_status_or_ccr

move_to_usp	bsr	getareg
		move.w	d1,d3
		bsr	SkipComma
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
		bsr	getareg
		move.w	d1,d0
		or.w	#$4E68,D0
		bra	one_word_instr

move_status	;MOVE sr,EA
		addq.l	#2,Addr(A5)
		addq.l	#1,A3
		cmp.b	#'r',(A3)+
		bne.s	move_err2
		bsr	SkipComma
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
		cmp.b	#'s',(A3)
		bne.s	try_move_ccr
		addq.l	#1,A3
		cmp.b	#'r',(A3)+
		bne.s	move_err2
		move.w	D1,D0
		or.w	#$46C0,D0
		bra.s	zcom_5
move_err2	bra	error

try_move_ccr	moveq	#'c',D0
		cmp.b	(A3)+,D0
		bne.s	move_err2
		cmp.b	(A3)+,D0
		bne.s	move_err2
		cmp.b	#'r',(A3)+
		bne.s	move_err2
		move.w	D1,D0
		or.w	#$44C0,D0
zcom_5		bra	zzcom

normal_move	moveq	#0,D5
normal_move_2	addq.l	#2,Addr(A5)
		bsr	GetSize
		bsr	GetEA
		tst.w	size(A5)
		bne.s	nm_1
		cmp.w	#1,D1
		beq.s	move_err2
nm_1		lsl.w	#3,D1
		or.w	D1,D0
		move.w	D0,D3
		bsr	SkipComma
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
movea01		cmp.w	#1,D1
		bne.s	move_err2	;MOVEA destination must be addr reg
movea02		tst.w	size(A5)
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

not_byte_move	addq.w	#1,D1
		bchg	#0,D1
		lsl.w	#8,D1
		lsl.w	#4,D1
		or.w	D1,D0
zump1		bra	zzcom

*** MOVEP ***
move_peripheral	addq.l	#1,A3
		addq.l	#2,Addr(A5)
		bsr	GetSize
		tst.w	size(A5)
		beq.s	move_err3
		bsr	skipspaces
		cmp.b	#'d',(A3)
		bne.s	move_from_peripheral
		bsr	getdreg
		lsl.w	#8,D1
		lsl.w	#1,D1
		move.w	D1,D3
		bsr	SkipComma
		bsr	GetEA
		cmp.w	#5,D1
		bne.s	move_err3
		or.w	D3,D0
		or.w	#$0188,D0

move_per_1	move.w	size(A5),D1
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
		bsr	getdreg
		move.w	d1,d0
		lsl.w	#8,D0
		lsl.w	#1,D0
		or.w	D3,D0
		or.w	#$0108,D0
		bra.s	move_per_1

move_err3	bra	error

*** MOVEM ***
movem_asm	addq.l	#1,A3
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

regs_to_mem	bsr	getreglist
		move.w	D2,D3
		bsr	SkipComma
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

juzumps		lsr.w	#1,D3	;reverse bit order
		roxl.w	#1,D4
		dbf	D2,juzumps
		move.w	D4,D3

regs_to_mem_1	lsl.w	#3,D1
		or.w	D1,D0
		or.w	#$4880,D0
zumpsis		move.w	size(A5),D1
		subq.w	#1,D1
		lsl.w	#6,D1
		or.w	D1,D0
		move.l	EndAddr(A5),A0
		move.w	D0,(A0)+
		move.w	D3,(A0)
		bra	assem9

move_err4	bra	error

regs_from_mem	bsr	GetEA
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
		bsr	getreglist
		move.w	D3,D0
		move.w	D2,D3
		or.w	#$4C80,D0
		bra.s	zumpsis

*** BTST & BCHG & BCLR & BSET ***
btst_asm	moveq	#0,D3
		bra.s	bit_ins_1

bchg_asm	moveq	#1,D3
		bra.s	bit_ins_1

bclr_asm	moveq	#2,D3
		bra.s	bit_ins_1

bset_asm	moveq	#3,D3

bit_ins_1	clr.w	size(a5)		for btst Dn,#imm
		addq.l	#2,Addr(A5)
		lsl.w	#6,D3
		bsr	skipspaces
		cmp.b	#'#',(A3)+
		bne.s	bit_reg_mode
		bsr	get_expr
		move.l	Addr(A5),A0
		move.w	D0,(A0)+
		move.l	A0,Addr(A5)
		bset	#11,D3
		bra.s	bit_get_ea
;#
;# btst Dn,#imm is now accepted
;#
bit_reg_mode	subq.l	#1,a3
		bsr	getdreg
		lsl.w	#8,D1
		lsl.w	#1,D1
		or.w	D1,D3
		bset	#8,D3

bit_get_ea	bsr	SkipComma
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

bit_ea_1	cmp.w	#$3c,D0
		bhi.s	bits_err
		or.w	D3,D0
		cmp.w	#$083c,d0
		beq.s	bits_err
		bra	zzcom

bits_err	bra	error

*** CHK ***
chk_asm		move.w	#$4180,D3
		bra.s	mul_div1

*** MUL & DIV ***
mul_asm		move.w	#$C0C0,D3
		bra.s	mul_div

div_asm		move.w	#$80C0,D3

mul_div		bsr	skipspaces
		cmp.b	#'u',(A3)+
		beq.s	mul_div1
		cmp.b	#'s',-1(A3)
		bne.s	bits_err
		bset	#8,D3

mul_div1	move.w	#WSIZE,size(A5)
		addq.l	#2,Addr(A5)
		bsr	GetEA
		cmp.w	#1,D1
		beq.s	bits_err
		lsl.w	#3,D1
		or.w	D1,D0
		or.w	D0,D3
		bsr	SkipComma
		bsr	getdreg
		move.w	d1,d0
		lsl.w	#8,D0
		lsl.w	#1,D0
		or.w	D3,D0
		bra.s	zzcom

*** TAS ***
tas_asm		addq.l	#2,Addr(A5)
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
pea_asm		move.w	#$4840,D3

pp_1		addq.l	#2,Addr(A5)
		bsr	GetEA
		cmp.w	#2,D1
		beq.s	pea_A_ok
		cmp.w	#5,D1
		bcs.s	zz_err

pea_A_ok	lsl.w	#3,D1
		or.w	D1,D0
		cmp.w	#$3C,D0
		bcc.s	zz_err
		or.w	D3,D0
		bra.s	zzcom

*** LEA ***
lea_asm		addq.l	#2,Addr(A5)
		bsr	GetEA
		cmp.w	#2,D1
		beq.s	lea_A_ok
		cmp.w	#5,D1
		bcs.s	zz_err

lea_A_ok	lsl.w	#3,D1
		or.w	D0,D1
		cmp.w	#$3C,D1
		bcc.s	zz_err
		move.w	d1,d2
		bsr	SkipComma
		bsr	getareg
		move.w	d1,d0
		lsl.w	#8,D0
		lsl.w	#1,D0
		or.w	d2,d0
		or.w	#$41C0,D0

zzcom		move.l	EndAddr(A5),A0
		move.w	D0,(A0)
		bra	assem9

zz_err		bra	error

*** EXT ***
ext_asm		bsr	GetSize
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
		move.w	d1,d0
		or.w	D2,D0
		or.w	#$4800,D0
		bra	one_word_instr

*** JMP ***
jmp_asm		move.w	#$4EC0,D3
		bra	pp_1

*** JSR ***
jsr_asm		move.w	#$4E80,D3
		bra	pp_1

*** NBCD ***
nbcd_asm	addq.l	#2,Addr(A5)
		clr.w	size(A5)
		move.w	#$4800,D3
		bra.s	one_arg_2

*** CLR ***
clr_asm		move.w	#$4200,D3

one_arg_com	addq.l	#2,Addr(A5)
		bsr	GetSize

one_arg_2	bsr	GetEA
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
neg_asm		cmp.b	#'x',(A3)
		bne.s	no_negx
		addq.l	#1,A3
		move.w	#$4000,D3
		bra.s	one_arg_com

qlumps_err	bra	error

no_negx		move.w	#$4400,D3
		bra.s	one_arg_com

*** NOT ***
not_asm		move.w	#$4600,D3
		bra.s	one_arg_com

*** TST ***
tst_asm		move.w	#$4A00,D3
		bra.s	one_arg_com

*** SWAP ***
swap_asm	bsr	skipspaces
		bsr	getdreg
		move.w	d1,d0
		or.w	#$4840,D0
		bra.s	one_word_instr

*** RESET ***
reset_asm	move.w	#$4E70,D0
		bra.s	one_word_instr

*** NOP ***
nop_asm		move.w	#$4E71,D0
		bra.s	one_word_instr

*** RTE ***
rte_asm		move.w	#$4E73,D0
		bra.s	one_word_instr

*** RTS ***
rts_asm		move.w	#$4E75,D0
		bra.s	one_word_instr

*** TRAPV ***
trapv_asm	move.w	#$4E76,D0
		bra.s	one_word_instr

*** RTR ***
rtr_asm		move.w	#$4E77,D0

one_word_instr	move.l	Addr(A5),A0
		move.w	D0,(A0)+
		move.l	A0,Addr(A5)
		bra	assem9

*** ILLEGAL ***
illegal_asm	move.w	#ILLEGAL,D0
		bra.s	one_word_instr

*** TRAP ***
trap_asm	bsr	skipspaces
		cmp.b	#'#',(A3)+
		bne.s	zump_err
		bsr	get_expr
		and.w	#$0f,D0
		or.w	#$4E40,D0
		bra.s	one_word_instr

*** UNLK ***
unlk_asm	bsr	skipspaces
		bsr	getareg
		move.w	d1,d0
		or.w	#$4E58,D0
		bra.s	one_word_instr

*** LINK ***
link_asm	bsr	skipspaces
		bsr	getareg
		move.w	d1,d3
		bsr	SkipComma
		cmp.b	#'#',(A3)+
		bne.s	zump_err
		bsr	get_expr
		move.w	D0,D1
		move.w	D3,D0
		or.w	#$4E50,D0
		bra.s	two_words_instr

*** STOP ***
stop_asm	bsr	skipspaces
		cmp.b	#'#',(A3)+
		bne.s	zump_err
		bsr	get_expr
		move.w	D0,D1
		move.w	#$4E72,D0
two_words_instr	move.l	Addr(A5),A0
		move.w	D0,(A0)+
		move.w	D1,(A0)+
		move.l	A0,Addr(A5)
		bra	assem9

zump_err	bra	error

*** EXG ***
exg_asm		bsr	getreg
		bsr	SkipComma
		swap	D1
		bsr	getreg
		swap	D1
		btst	#16+3,D1
		beq.s	no_exg_1
		btst	#3,D1
		bne.s	no_exg_1
		swap	D1

no_exg_1	move.l	D1,D0
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

data_and_addr	or.w	#$88,D0
exg_9		bra	one_word_instr

*** GET REGISTER LIST (for MOVEM) ***
; result is returned in d2
getreglist	clr.w	D2

reglistx	bsr.s	getreg
		bset	D1,D2
		cmp.b	#'/',(A3)+
		beq.s	reglistx
		cmp.b	#'-',-1(A3)
		bne.s	reglist9
		move.w	D1,D7
		bsr.s	getreg
		move.w	D1,D0
		eor.w	D7,D0
		btst	#3,D0
		bne	error		;register in range are different types
		cmp.w	D1,D7
		bls.s	regr_01
		exg	d1,d7		;register range out of order
		bra.s	regrange
regr_01		beq.s	reg_r1

regrange	bset	D7,D2
		addq.w	#1,D7
		cmp.w	D1,D7
		bls.s	regrange
reg_r1		cmp.b	#'/',(A3)+
		beq.s	reglistx

reglist9	subq.l	#1,A3
		moveq	#0,D0
		rts			;register mask returned in D2

*** GET ONE REGISTER, number in D1, 0=D0..15=sp ***
;#
;# must not change the high word of d1
;# (it zeroed the high word if 'sp' was given and
;#  that caused exg Rx,sp to work incorrectly)
;#
getreg		clr.w	D1
		bsr	skipspaces
		cmp.b	#'s',(a3)
		beq.s	check_sp
		cmp.b	#'d',(A3)+
		beq.s	g_r_1
		cmp.b	#'a',-1(A3)
		bne	error
		bset	#3,D1
g_r_1		move.b	(A3)+,D0
		sub.b	#'0',D0
		bcs.s	greg_err
		cmp.b	#8,D0
		bcc.s	greg_err
		ext.w	D0
		or.b	D0,D1
g_r_2		rts	;register returned in D1

check_sp	cmp.b	#'p',1(a3)
		bne.s	greg_err
		move.w	#$0f,d1
		addq.l	#2,a3
		rts

greg_err	bra	error
;
; get address register, return in d1
;
getareg		bsr	getreg
		bmi.s	01$
		bclr	#3,d1
		beq.s	greg_err
01$		rts

;
; get data register, return in d1
;
getdreg		bsr	getreg
		bmi.s	01$
		btst	#3,d1
		bne.s	greg_err
01$		rts

*** Get a number in range 1..8, used by 'quick' instructions and shifts ***
* return -1 if error
get_expr_1_8	bsr	get_expr
		swap	D0
		tst.w	D0
		bne.s	greg_err
		swap	D0
		tst.w	D0
		beq.s	greg_err
		cmp.w	#8,D0
		bhi.s	greg_err
		and.w	#7,D0
		rts

*** SKIP COMMA & SPACES AROUND IT ***
; error if no comma found
SkipComma	bsr	skipspaces
		cmp.b	#',',(A3)+
		bne.s	greg_err
		bsr	skipspaces
		moveq	#0,D0
		rts

GetEA: ;get effective address, mode=D1,reg=D0
* put displacements etc. in memory at address pointed by Addr(A5)
* and increment Addr(A5)
		bsr	skipspaces
		cmp.b	#'d',(a3)
		beq.s	reg_direct
		cmp.b	#'a',(a3)
		beq.s	reg_direct
		cmp.b	#'s',(a3)
		bne.s	no_reg_direct

reg_direct	bsr	getreg
		move.w	d1,d0
		moveq	#0,d1
		bclr	#3,d0
		beq.s	reg_09
		moveq	#1,d1
reg_09		rts

no_reg_direct	cmp.b	#'(',(a3)
		bne.s	no_indirect_or_postincrement
		addq.l	#1,A3
		bsr	getareg
		move.w	d1,d0
		cmp.b	#')',(A3)+
		bne	error
		cmp.b	#'+',(A3)
		beq.s	postincrement
		moveq	#2,D1
		rts

postincrement	addq.l	#1,A3
		moveq	#3,D1
		rts

no_indirect_or_postincrement
		cmp.b	#'-',(a3)
		bne.s	no_predecrement
		cmp.b	#'(',1(A3)
		bne.s	no_predecrement
		addq.l	#2,A3
		bsr	getareg
		move.w	d1,d0
		cmp.b	#')',(A3)+
		bne	error
		moveq	#4,D1
		rts

no_predecrement	cmp.b	#'#',(a3)
		bne.s	no_immediate
		addq.l	#1,A3

PutImm		bsr	get_expr
		move.l	Addr(A5),A0
		move.w	size(A5),D1
		bne.s	sz1
		and.w	#$FF,D0
		move.w	D0,(A0)+
		bra.s	sz9
sz1		cmp.w	#1,D1
		bne.s	sz2
		move.w	D0,(A0)+
		bra.s	sz9
sz2		move.l	D0,(A0)+
sz9		move.l	A0,Addr(A5)
		moveq	#7,D1
		moveq	#4,D0
		rts

no_immediate	bsr	get_expr
		move.l	D0,D2
		cmp.b	#'(',(a3)
		bne	absolute_mode
		addq.l	#1,A3
		cmp.b	#'a',(a3)
		beq.s	01$
		cmp.b	#'s',(a3)
		bne.s	no_displacement_or_index
01$		bsr	getareg
		move.w	d1,d0
		cmp.b	#',',(A3)+
		beq.s	indirect_with_index
		cmp.b	#')',-1(A3)
		bne	error
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
		cmp.b	#'p',(a3)
		bne.s	pcrel_indx_2
		addq.l	#1,a3
		cmp.b	#'c',(A3)+
		bne	error
		cmp.b	#')',(A3)+
		bne.s	pcrel_indx_1
		sub.l	Addr(A5),D2
		move.l	Addr(A5),A0
		move.w	D2,(A0)+
		move.l	A0,Addr(A5)
		move.w	d2,d0
		ext.l	d0
		cmp.l	d0,d2
		bne	out_of_range

		moveq	#7,D1
		moveq	#2,D0
		rts
;
; pc-relative indexed syntax can now be in the correct format '(pc,Rn.w)'
;
pcrel_indx_1	cmp.b	#',',-1(a3)
		bne	error
pcrel_indx_2	sub.l	Addr(A5),D2
		move.b	d2,d0
		ext.w	d0
		ext.l	d0
		cmp.l	d0,d2
		bne	out_of_range
		bsr.s	GetIndex
		moveq	#7,D1
		moveq	#3,D0
		rts

absolute_mode	move.w	D2,D1
		ext.l	D1
		cmp.l	D2,D1
		beq.s	abs_short_mode
		move.l	Addr(A5),A0
		move.l	D2,(A0)+
		move.l	A0,Addr(A5)
		moveq	#7,D1
		moveq	#1,D0
		rts

abs_short_mode	move.l	Addr(A5),A0
		move.w	D2,(A0)+
		move.l	A0,Addr(A5)
		moveq	#7,D1
		moveq	#0,D0
		rts

*** GET SIZE (put it in size(A5), 0=B, 1=W, 2=L) ***
GetSize		cmp.b	#'.',(A3)+
		bne	error
		move.b	(A3)+,D0
		cmp.b	#'b',D0
		bne.s	siz1
		moveq	#BSIZE,d0
		bra.s	siz3
siz1		cmp.b	#'w',D0
		bne.s	siz2
		moveq	#WSIZE,d0
		bra.s	siz3
siz2		cmp.b	#'l',D0
		bne	error
		moveq	#LSIZE,d0
siz3		move.w	d0,size(a5)
		rts

GetIndex	;displacement value in d2
		bsr	getreg
		bmi.s	index_error
		moveq	#0,d0
		bclr	#3,d1
		beq.s	01$
		bset	#15,d0
01$		lsl.w	#8,d1
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
index_2		cmp.b	#')',(A3)+
		bne.s	index_error
		move.l	Addr(A5),A0
		move.w	D1,(A0)+
		move.l	A0,Addr(A5)
		rts
index_error
;#		addq.l	#8,sp
		bra	error

*** DISPLAY & CHANGE REGISTERS ***
regs		bsr	skipspaces
		tst.b	(A3)
		bne.s	changeregs

displayregs_d	bsr	displayregs
		move.l	RegPC(a5),D0
		beq.s	rr9
		btst	#0,D0
		bne.s	rr9
		move.l	D0,A4
		tst.w	(a4)
		beq.s	rr9
		startline
		bsr	disassemble
		tst.w	d0
		bne.s	rr9
		bsr	printstring
rr9		bra.s	regs_mainloop_jmp

skipequal ;the syntax of the register change command can include a '=' sign
		cmp.b	#'=',(A3)	;but it is not necessary
		bne.s	01$
		addq.l	#1,A3
01$		rts

changeregs	move.b	(A3)+,D0	;** CHANGE REGISTERS **
		bsr	tolower
		lsl.w	#8,D0
		move.b	(A3)+,D0	;get possibly unaligned word
		bsr	tolower
		cmp.w	#'pc',D0	;program counter
		bne.s	nopc
		bsr.s	skipequal
		bsr	get_expr
		move.l	D0,RegPC(a5)
		bra.s	regs_mainloop_jmp

;#
;# now 'ccr' can be used instead of 'cc' -- 1989-12-09
;#
nopc		cmp.w	#'cc',D0	;condition code register
		bne.s	nocc
		move.b	(a3),d0
		bsr	tolower
		cmp.b	#'r',d0
		bne.s	01$
		addq.l	#1,a3
01$		bsr.s	skipequal
		bsr	get_expr
		move.b	D0,RegCC(a5)
regs_mainloop_jmp
		bra	mainloop

nocc		cmp.w	#'d0',D0
		bcs.s	nodr
		cmp.w	#'d7',D0
		bhi.s	nodr
		sub.w	#'d0',D0
		lsl.w	#2,D0
		move.w	D0,D2
		bsr	skipequal
		bsr	get_expr
		lea	DataRegs(a5),A0
		move.l	D0,0(A0,D2.W)
		bra.s	regs_mainloop_jmp

nodr		cmp.w	#'sp',d0
		bne.s	nosp
		moveq	#7,d0
		bra.s	setareg
nosp		cmp.w	#'a0',D0
		bcs	error
		cmp.w	#'a7',D0
		bhi	error
		sub.w	#'a0',D0
setareg		lsl.w	#2,D0
		move.w	D0,D2
		bsr	skipequal
		bsr	get_expr
		lea	AddrRegs(a5),A0
		move.l	D0,0(A0,D2.W)
		bra.s	regs_mainloop_jmp

*** DISPLAY REGISTERS ***
;#
;# condition code register is now displayed as ccr, not cc
;#
displayregs	startline
		move.l	#' PC=',(A3)+
		move.l	RegPC(a5),D0
		bsr	phex1_8
		move.l	#' CCR',(A3)+
		putchr	<'='>
		move.b	RegCC(a5),D0
		move.b	D0,D2
		moveq	#2,d1
		bsr	put_hexnum1
		moveq	#4,D1

flagloop	putchr	SPACE
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
flag1		move.b	D0,(A3)+
		subq.l	#1,D1
		bpl.s	flagloop
		endline
		bsr	printstring
		lea	DataRegs(a5),A2
		move.l	#' D0=',D2
		bsr.s	PrintRegLine
		bsr.s	PrintRegLine
		move.l	#' A0=',D2
		bsr.s	PrintRegLine
		nop

PrintRegLine	startline
		moveq	#4-1,D3
regl1		move.l	D2,(A3)+
		move.l	(A2)+,D0
		bsr	phex1_8
		add.w	#$0100,D2
		dbf	D3,regl1
		endline
		bra	printstring

flagstring	dc.b	'CVZNX'
		even

;
; display/change default number base
;
xbase		bsr	skipspaces
		tst.b	(a3)
		beq.s	showbase
		moveq	#10,d0
		bsr	get_num
		moveq	#2,d1
		cmp.b	d1,d0
		bcs.s	base_err
		moveq	#36,d1
		cmp.b	d1,d0
		bhi.s	base_err
		move.b	d0,defbase(a5)
mloop_bx	bra	mainloop

base_err	bra	error

showbase	moveq	#0,d0
		move.b	defbase(a5),d0
		lea	basefmt(pc),a0
		bsr	printf_window
		bra.s	mloop_bx

*** BREAKPOINTS ***
breaks		bsr	skipspaces
		move.b	(A3)+,D0
		bsr	tolower
		cmp.b	#'a',d0
		beq.s	xbase
		cmp.b	#'l',D0
		beq	break_list
		cmp.b	#'r',D0
		beq.s	break_remove

*** SET BREAKPOINT ***
		subq.l	#1,A3
		bsr	get_expr
		btst	#0,D0
		bne.s	brk_err
		move.l	D0,A4
		bsr	find_break
		bpl.s	brk_err
		move.l	A1,D3
		moveq	#brk_SIZE,D0
		move.l	#MEMF_CLEAR!MEMF_PUBLIC,D1
		call	ExecBase,AllocMem
		tst.l	D0
		beq	OutOfMem
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
brset9		bra	mainloop
brk_err		bra	error

*** REMOVE BREAKPOINT ***
break_remove	bsr	skipspaces
		cmp.b	#'a',(A3)	;check 'all'
		bne.s	break_rem1
		moveq	#'l',D0
		cmp.b	1(A3),D0
		bne.s	break_rem1
		cmp.b	2(A3),D0
		bne.s	break_rem1
		bsr.s	remove_all_breaks
		bra.s	brset9

break_rem1	bsr	get_expr
		bsr.s	find_break
		bmi.s	brk_err
		move.l	A1,D0
		bne.s	no_remove_from_start_of_list
		move.l	(A0),BreakList(A5)
		bra.s	break_remove_1
no_remove_from_start_of_list
		move.l	(A0),(A1)
break_remove_1	move.l	A0,A1
		moveq	#brk_SIZE,D0
		call	ExecBase,FreeMem
		bra.s	brset9

*** LIST BREAKPOINTS ***
* note: the list is automatically in order
break_list	move.l	BreakList(A5),D2
		bne.s	break_list_1
		lea	noBrkTxt(pc),A0
		bsr	printstring_a0_window
		bra	mainloop
break_list_1	lea	brklistTx(pc),A0
		bsr	printstring_a0
break_list_loop	tst.l	D2
		beq.s	brset9
		move.l	D2,A4
		move.l	brk_Address(A4),D0
		lea	hexfmt(pc),a0
		bsr	printf
		bsr	CheckKeys
		bne	brset9
		move.l	brk_Next(A4),D2
		bra.s	break_list_loop

*** FIND BREAKPOINT FROM LINKED LIST ***
* address in D0
* if N=1 then not found, A1 points to where new node should be inserted
*  if A1=0 then insert to start of the list
* if N=0 then found, A0 points to the node, A1 points to predecessor
find_break	sub.l	A1,A1	;A1=0
		move.l	BreakList(A5),D1
find_br_1	beq.s	break_not_found
		move.l	D1,A0
		cmp.l	brk_Address(A0),D0
		beq.s	break_found
		bcs.s	break_not_found
		move.l	A0,A1
		move.l	brk_Next(A0),D1
		bra.s	find_br_1
break_not_found	moveq	#-1,D0
		rts
break_found	moveq	#0,D0
brk_ret		rts

remove_all_breaks
* executed before exit of when the 'br all' command is given
; this changes d2 and a6
		move.l	ExecBase,a6
		move.l	BreakList(A5),D2

all_breaks_loop	tst.l	D2
		beq.s	brk_ret2
		move.l	D2,A1
		move.l	brk_Next(A1),D2
		moveq	#brk_SIZE,D0
		call	FreeMem
		bra.s	all_breaks_loop

brk_ret2	clr.l	BreakList(A5)
		rts

*** PUT THE ILLEGAL ($4AFC) INSTRUCTION TO BREAKPOINTS ***
* (first save original contents in the BreakPoint structure)
;
; don't set a breakpoint if the current PC is pointing to it
; but save the address of the breakpoint structure so the breakpoint
; can be activated from the trap handler...
;
SetBreaks	bset	#MONB_BRKACTIVE,flags(a5)
		move.l	BreakList(A5),D2
SetBr1		tst.l	D2
		beq.s	brk_ret
		move.l	D2,A1
		move.l	brk_Address(A1),A0
		cmp.l	RegPC(a5),a0
		bne.s	01$
		move.l	a1,GoBrkPtr(a5)
		bra.s	SetBr2
01$		move.w	(A0),brk_Content(A1)
		move.w	#ILLEGAL,(A0)
SetBr2		move.l	(A1),D2
		bra.s	SetBr1

*** RESTORE ORIGINAL CONTENTS OF BREAKPOINTS ***
RemBreaks	bclr	#MONB_BRKACTIVE,flags(a5)
		beq.s	brk_ret
		move.l	BreakList(A5),D2
RemBr1		tst.l	D2
		beq.s	brk_ret
		move.l	D2,A1
		move.l	brk_Address(A1),A0
		move.w	brk_Content(A1),(A0)
		clr.w	brk_Content(a1)		this helps debugging...
RemBr2		move.l	(A1),D2
		bra.s	RemBr1

**** SINGLE STEP (WALK) ****
* NOTE: This ignores breakpoints
walk		bsr.s	getpc
		move.l	AddrRegs+7*4(a5),sp
		or	#2,CCR		;set overflow flag
		trapv			;let the trap handler do the rest...
walk_here	;a label so we can reference it in the handler routine

getpc		bsr	skipspaces
		tst.b	(a3)
		beq.s	01$
		bsr	get_expr
		move.l	d0,RegPC(a5)
01$		rts

**** EXECUTE MACHINE CODE (GO) ****
go		bsr	getpc
		bsr	SetBreaks
		move.l	AddrRegs+7*4(a5),sp
		bra.s	go_com

**** JUMP TO SUBROUTINE (RETURN WITH RTS) ***
jumpsr		bsr	getpc
		bsr	SetBreaks
		move.l	sp,A0	;set stack ptr
		sub.w	#$100,A0
		move.l	a0,d0
		and.b	#$fc,d0		;long word align
		move.l	d0,AddrRegs+4*7(a5)
		move.l	A0,sp
		lea	returncode(pc),A0
		move.l	A0,(sp)		;put return address in stack

go_com		move.l	RegPC(a5),d0
		bsr	find_break
		bpl.s	go_special

		move.l	RegPC(a5),-(sp)
		move.b	RegCC(a5),ccr
		movem.l	DataRegs(a5),D0-D7/A0-A6
		rts	;this really jumps to the user program

go_special	or	#2,CCR		;set overflow flag
		trapv			;let the trap handler do the rest...
special_go_here

*** CONTROLS RETURNS HERE AFTER THE Jsr COMMAND ***
returncode	movem.l	d0-d2/a0-a1/a5-a6,-(sp)
		call	ExecBase,GetCC
		move.l	d0,d2
		suba.l	a1,a1
		call	FindTask
		move.l	d0,a1
		move.l	TC_TRAPDATA(a1),a5
		move.b	d2,RegCC(a5)
		movem.l	(sp)+,d0-d2/a0-a1
		movem.l	d0-d7/a0-a4,DataRegs(a5)
		move.l	(sp)+,AddrRegs+4*5(a5)		;a5
		move.l	(sp)+,AddrRegs+4*6(a5)		;a6
		addq.l	#4,sp
		move.l	sp,AddrRegs+4*7(a5)		;sp

		move.l	StackPtr(a5),sp

		bsr	RemBreaks

		lea	rettx(pc),A0
		bsr	printstring_a0
		bra	displayregs_d

*** TASK TRAP CODE ***
trapreturn	;Note! We are in supervisor mode!
		movem.l	d0/a0/a5/a6,-(sp)
		move.l	ExecBase,a6
		move.l	ThisTask(a6),a5
		move.l	TC_TRAPDATA(a5),a5

		cmp.l	#9,4*4(sp)		trace?
		bne.s	noskipbrk
		bclr	#MONB_BRKSKIP,flags(a5)
		beq.s	noskipbrk
;
; we have just skipped a breakpoint in trace mode
;
		move.l	GoBrkPtr(a5),a5
		move.l	brk_Address(a5),a0
		move.w	(a0),brk_Content(a5)
		move.w	#ILLEGAL,(a0)
		movem.l	(sp)+,d0/a0/a5/a6
		addq.l	#4,sp			remove exception number from stack
		bclr	#7,(sp)			clear trace bit
		rte				continue at full speed

noskipbrk	cmp.l	#7,4*4(sp)	;is this a TRAPV-trap (possibly by the walk routine)
		bne.s	normtrap_1
		lea	walk_here(pc),a0
		cmp.l	4*4+6(sp),a0		;check program counter
		beq.s	walk_trap
		lea	special_go_here(pc),a0
		cmp.l	4*4+6(sp),a0
		bne.s	normtrap_1

; this is the special go-routine if the address we are going to enter
; the code contains a breakpoint. in that case we trace over the
; instruction and then continue at full speed
		bset	#MONB_BRKSKIP,flags(a5)
; fall to walk trap routine
walk_trap	move.l	RegPC(a5),4*4+6(sp)
		move	sr,d0
		move.b	RegCC(a5),d0
		bclr	#13,d0			supervisor mode off
		bset	#15,d0			trace mode on
		move.w	d0,4*4+4(sp)

		lea	4*4+4(sp),sp
		movem.l	DataRegs(a5),d0-d7/a0-a6
		rte

normtrap_1	movem.l	(sp)+,d0/a0
		movem.l	d0-d7/a0-a4,DataRegs(a5)
		move.l	(sp)+,AddrRegs+5*4(a5)		;a5
		move.l	(sp)+,AddrRegs+6*4(a5)		;a6
		move	usp,a0
		move.l	a0,AddrRegs+7*4(a5)		;sp

		move.w	AttnFlags(A6),D1
		move.l	(sp)+,D5
		cmp.w	#3,D5
		bhi.s	pop_SR_and_PC	jump if not bus error or address error
		btst	#AFB_68010,D1	we must check the type of the processor!
		bne.s	pop_SR_and_PC
		addq.l	#8,sp		;clean stack info left by bus/addr errors
					;by the 68000

pop_SR_and_PC	move.w	(sp)+,D0	status register
		move.b	D0,RegCC(a5)
		move.l	(sp)+,a4
		cmp.w	#ILLEGAL,(a4)
		seq	d7
		move.l	a4,RegPC(a5)

		btst	#AFB_68010,d1
		beq.s	go_user_mode

;
; here we handle 68010/20 stack frames
;
		move.w	(sp)+,d3	get stack frame type/exception number word
		and.w	#$f000,d3	extract frame type bits
		beq.s	go_user_mode	just continue if it is 4-word format
		moveq	#4,d2
		cmp.w	#$2000,d3	instruction error format
		beq.s	drop_stack
		moveq	#12,d2
		cmp.w	#$9000,d3	coprocessor mid-instruction format
		beq.s	drop_stack
		moveq	#50,d2
		cmp.w	#$8000,d3	68010 long format
		beq.s	drop_stack
		moveq	#24,d2
		cmp.w	#$a000,d3	68020 short format
		beq.s	drop_stack
		moveq	#84,d2
		cmp.w	#$b000,d3	68020 long format
		bne.s	go_user_mode

drop_stack	add.w	d2,sp

go_user_mode	and.w	#$5FFF,D0	clear supervisor & trace bits
		move	D0,SR		back to the user mode!

		movea.l	StackPtr(a5),sp		restore monitor (user) stack pointer
;
; if SkipBreakFlag is set when we enter here, some other exception has
; occurred before the trace-exception that should have cleared the
; flag and activated the breakpoint. in that case we must activate the
; breakpoint here (RemBreaks tries to remove it anyway, and that could
; cause problems...)
;
		bclr	#MONB_BRKSKIP,flags(a5)
		beq.s	01$
		move.l	GoBrkPtr(a5),a1		activate breakpoint
		move.l	brk_Address(a1),a3
		move.w	(a3),brk_Content(a1)
		move.w	#ILLEGAL,(a3)
01$
		btst	#MONB_BRKACTIVE,flags(a5)
		bne.s	02$
		clr.b	d7
02$
		ifd	DEBUG
		btst	#AFB_68010,d1
		beq.s	099$
		lsl.l	#4,d3
		swap	d3
		moveq	#0,d0
		move.w	d3,d0
		lea	frametypefmt(pc),a0
		bsr	printf_window
099$
		endc

		bsr	RemBreaks
		move.l	RegPC(a5),D0
		cmp.w	#4,D5
		bne.s	normal_trap
		bsr	find_break
		bne.s	normal_trap
		tst.b	d7
		beq.s	normal_trap
		lea	brkPtTxt(pc),A0
		bsr	printstring_a0
		bra.s	trap_dregs

normal_trap	bsr.s	show_trap_name
trap_dregs	bsr	displayregs
		move.l	RegPC(a5),D0
		btst	#0,D0
		bne.s	tr99
		move.l	D0,Addr(A5)
		move.l	D0,A4
		startline
		bsr	disassemble
		bsr	printstring
tr99		bra	mainloop

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
unknown_trap	lea	unknowntrap(pc),a1
		bsr	putstring
		move.l	d5,d0
		bsr	put_signed_hexnum
		bra.s	notraps
traps		moveq	#10,D0
txout1		add.w	D0,D0
		add.w	D0,A1
		add.w	(A1),A1
		bsr	putstring
		cmp.w	#$20,D5
		bcs.s	notraps
		move.w	D5,D0
		sub.w	#$20,D0
		bsr	put_signed_hexnum
notraps	putchr	SPACE
		moveq	#'*',D0
		move.b	D0,(A3)+
		move.b	D0,(A3)+
		move.b	D0,(A3)+
		endline
		bra	printstring

*** SHOW TRAP NAME (USAGE: ^ num) ***
;#
;# ->1.21  1990-01-06
;# also debug command. ^^ shows monitor data area pointer
;#
showtrap	cmp.b	(a3),d0
		beq.s	dbug
		bsr	get_expr
		move.l	D0,D5
		bsr	show_trap_name
		bra.s	mloop_jx
dbug		lea	hexfmt(pc),a0
		move.l	a5,d0
		bsr	printf
		bra.s	mloop_jx

*** enter command line ***
cmdline		bsr	skipspaces
		tst.b	(a3)
		bne.s	01$
		lea	cmdlinetxt(pc),a0
		bsr	printstring_a0_window
		moveq	#0,d0
		bsr	GetInput
01$		lea	CmdLineBuf(a5),a1
		move.l	a1,a2
02$		move.b	(a3)+,(a1)+
		bne.s	02$
		move.b	#LF,-1(a1)
		clr.b	(a1)
		sub.l	a2,a1
		move.l	a1,DataRegs(a5)
		move.l	a2,AddrRegs(a5)
mloop_jx	bra	mainloop

calculator	bsr	skipspaces
		tst.b	(a3)
		bne.s	01$
		lea	calctxt(pc),a0
		bsr	printstring_a0_window
		moveq	#0,d0
		bsr	GetInput
01$		bsr	get_expr

*** DISPLAY NUMBER IN HEX, DECIMAL, OCTAL AND BINARY ***
		move.l	D0,D5
		move.l	#$00070010,d4
		move.l	#'$hex',d6
		bsr.s	numzump
		move.l	#$ffff000a,d4
		move.l	#'dec',d6
		bsr.s	numzump
		move.l	#$ffff0008,d4
		move.l	#'@oct',d6
		bsr.s	numzump
		move.l	#$001f0002,d4
		move.l	#'%bin',d6
		bsr.s	numzump
		bra.s	mloop_jx

numzump ;print number in one base (both signed & unsigned if negative)
		startline
		tst.l	D5
		bpl.s	num_A1
		lea	signtxt(pc),A1
		bsr	putstring
num_A1		swap	d6
		move.w	d6,d3
		and.w	#$00ff,d6
		or.w	#SPACE<<8,d6
		swap	d6
		lsr.w	#8,d3
		move.l	d6,D0
		bsr	PutLong
		putchr	<':'>
		putchr	SPACE
		tst.b	d3
		beq.s	170$
		move.b	d3,(a3)+
170$		move.l	D5,D0
		move.w	d4,d2
		move.l	d4,d1
		swap	d1
		bsr.s	PutNum
		endline
		bsr	printstring
		tst.l	D5
		bpl.s	num_A2
		startline
		moveq	#SPACE,D0
		move.b	D0,(A3)+
		move.b	D0,(A3)+
		lea	signtxt+2(pc),A1
		bsr	putstring
		move.l	d6,D0
		bsr	PutLong
		putchr	<':'>
		putchr	SPACE
		move.l	D5,D0
		neg.l	D0
		putchr	<'-'>
		tst.b	d3
		beq.s	180$
		move.b	d3,(a3)+
180$		move.w	d4,d2
		move.l	d4,d1
		swap	d1
		bsr.s	PutNum
		endline
		bsr.s	printstring
num_A2		rts

;
; number output routine d0:number d1:length d2:base
;
PutNum		movem.l	D2-d4,-(sp)
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

;
; output routines
;

printf	bsr.s	fmtstring
; fall to printstring
printstring	lea	OutputBuffer(a5),a0
; fall to printstring_a0
*** Output a string (possibly redirected output) ***
printstring_a0	move.l	OutputFile(a5),d1
;
; print text, line at time
; inputs: a0 - pointer to zero-terminated string
; 	  d1 - filehandle
;
print_text	movem.l	d2-d4/a2/a6,-(sp)
		move.l	d1,d4
		move.l	DosBase(a5),a6
		move.l	a0,a2

print_loop	move.l	a2,d2
000$		move.b	(a2)+,d0
		beq.s	001$
		cmp.b	#LF,d0
		bne.s	000$
001$		move.l	a2,d3
		sub.l	d2,d3
		move.l	d4,d1
		tst.b	d0
		bne.s	002$
		subq.l	#1,d3
		beq.s	003$
002$		call	Write
		tst.b	-1(a2)
		bne.s	print_loop
003$		movem.l	(sp)+,d2-d4/a2/a6
		rts

printf_window	bsr.s	fmtstring
printstring_window
		lea	OutputBuffer(a5),a0
*** Output a string to the window ***
* used in error messages etc.
printstring_a0_window
		move.l	WinFile(a5),d1
		bra.s	print_text

;
; format a string in output buffer using RawDoFmt
; arguments in registers d0-d3
;
fmtstring	movem.l	a2-a3/a6,-(sp)
		movem.l	d0-d3,-(sp)
		move.l	sp,a1
		lea	putch(pc),a2
		lea	OutputBuffer(a5),a3
		call	ExecBase,RawDoFmt
		lea	16(sp),sp
		movem.l	(sp)+,a2-a3/a6
		rts

; character output routine for RawDoFmt
putch		move.b	d0,(a3)+
		rts

; Output a character to the window
ChrOutWin	move.l	WinFile(A5),D1
		bra.s	xChrOut
; output a character to current output
ChrOut		move.l	OutputFile(a5),d1
xChrOut		movem.l	D2-D3/a6,-(sp)
		move.b	d0,-(sp)
		move.l	sp,d2
		moveq	#1,D3
		call	DosBase(a5),Write
		addq.l	#2,sp
		movem.l	(sp)+,D2-D3/a6
		rts

**** Get a character ****
GetChar		movem.l	D2-D3/a6,-(sp)
		move.l	WinFile(A5),D1
		clr.b	-(sp)
		move.l	sp,d2
		moveq	#1,d3
		call	DosBase(a5),Read
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
key1		cmp.b	#'S',D0
		bne.s	key2
		move.w	#SHIFT_CURSOR_DOWN,D0
		rts
key2		cmp.b	#'T',D0
		bne.s	key3
		move.w	#SHIFT_CURSOR_UP,D0
		rts
key3		cmp.b	#SPACE,D0
		bne.s	key8
		bsr.s	GetChar
		cmp.b	#'A',D0
		bne.s	key4
		move.w	#SHIFT_CURSOR_LEFT,D0
		rts
key4		cmp.b	#'@',D0
		bne.s	key9
		move.w	#SHIFT_CURSOR_RIGHT,D0
		rts
key8		cmp.b	#'?',D0
		bne.s	key9
		bsr	GetChar
		move.w	#'??',D0	;THE HELP KEY
		rts
key9		cmp.b	#'~',D0
		beq.s	key99
		bsr	GetChar
		bra.s	key9
key99		moveq	#-1,D0	;unknown key
ret9		rts

**** THE INPUT ROUTINE ****
* special code in D0
* 0 = normal operation
* 1 = respond ctrl-e (assembler)
* 2 = edit existing line (created by disassembler)
* returns the special code or -1 if Ctrl-E pressed
*********
; this may modify any register...
GetInput	move.w	D0,inpspecial(A5)
		moveq	#0,D4
		lea	InputBuffer(a5),a4
		moveq	#0,D5
		moveq	#0,D7	;length
		cmp.w	#2,D0
		bne.s	inp0
		lea	ClearEol(pc),a0		erase to end of line
		bsr	printstring_a0_window
		move.l	A4,A0
		bsr	printstring_a0_window
		move.l	A4,A0
inp0len		tst.b	(A0)+
		bne.s	inp0len
		sub.l	A4,A0
		subq.l	#1,A0
		move.l	A0,D7
inp0		move.l	D7,D6	;current position
inp1		bsr	GetKey
		cmp.w	#CR,D0	;return
		beq	inp9
		cmp.w	#CtrlE,D0
		bne.s	noCtrlE
		tst.w	inpspecial(A5)
		beq.s	noCtrlE
		bsr	eraseline
		move.w	#-1,inpspecial(A5)
		bra	inp9

noCtrlE		cmp.w	#CtrlX,D0	;Ctrl-x clears the input line
		bne.s	noCtrlX
		bsr	eraseline
		bra.s	inp1

noCtrlX		cmp.w	#CURSOR_RIGHT,D0
		beq	moveright
		cmp.w	#CURSOR_LEFT,D0	;cursor left
		beq	moveleft
		cmp.w	#SHIFT_CURSOR_LEFT,D0
		bne.s	noleftedge
		bsr	gotoleftedge
		bra.s	inp1

noleftedge	cmp.w	#'??',D0
		bne.s	nohelp
		moveq	#-1,D5
		moveq	#'h',d0
		bra.s	put_char_to_input

nohelp		cmp.w	#SHIFT_CURSOR_UP,D0
		bne.s	no_do_prev
		moveq	#-1,D5
		bra	previousline

no_do_prev	cmp.w	#SHIFT_CURSOR_RIGHT,D0
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
		tst.w	d0
		bmi	inp1

put_char_to_input
		cmp.w	#64,D7		;is input line full?
		bcc	inp1
		cmp.l	D7,D6
		bhi	inp1
		beq.s	iputchr
		move.l	D7,D2

; make room for a new character
080$		move.b	-1(A4,D2.L),0(A4,D2.L)
		subq.l	#1,D2
		cmp.l	D2,D6
		bne.s	080$

iputchr		move.b	D0,D2
		lea	insch(pc),a0
		bsr	printstring_a0_window
		move.b	D2,D0
		bsr	ChrOutWin
		move.b	D2,0(A4,D6.L)
		addq.l	#1,D6
		addq.l	#1,D7
		tst.l	D5	;auto-CR flag
		beq	inp1

inp9		clr.b	0(A4,D7.L)
		move.l	A4,A3
		bsr	skipspaces
		tst.b	(A3)
		beq.s	inp99

		lea	History(a5),A0
		move.w	#(NLINES-1)*LEN/4-1,D0
099$		move.l	LEN(A0),(A0)+
		dbf	D0,099$		;scroll command line history to make space for current line

100$		move.b	(A4)+,(a0)+	;add current line to command line history
		bne.s	100$

inp99		tst.w	inpspecial(A5)
		bmi.s	inp99a
		emitwin	LF
inp99a		move.w	inpspecial(A5),D0
		rts
rightedge	;Shift-Cursor right
		cmp.l	D6,D7
		beq	inp1
		moveq	#0,d0
		move.b	D7,D0
		sub.b	D6,D0
		move.l	D7,D6
right01		lea	go_right_fmt(pc),a0
		bsr	printf_window
		bra	inp1

moveleft	tst.l	D6
		beq	inp1
		subq.l	#1,D6
		emitwin	BS
		bra	inp1

moveright	cmp.l	D6,D7
		beq	inp1
		addq.l	#1,D6
		moveq	#0,d0
		bra.s	right01

backspace	tst.l	D6
		beq	inp1
		subq.l	#1,D6
		emitwin	BS
delchar		cmp.l	D6,D7
		beq	inp1
		lea	delch(pc),a0
		bsr	printstring_a0_window
		subq.l	#1,D7
		cmp.l	D6,D7
		beq	inp1
		move.l	D6,D0

105$		move.b	1(A4,D0.L),0(A4,D0.L)
		addq.l	#1,D0
		cmp.l	D0,D7
		bne.s	105$

		bra	inp1

*** COMMAND LINE HISTORY ***
nextline	bsr.s	eraseline
		moveq	#1,D1
		bra.s	prnxl1

previousline	bsr.s	eraseline
		moveq	#-1,D1
prnxl1		add.b	D1,D4
		bmi.s	linct_neg
		cmp.b	#NLINES,D4
		bcs.s	linct_ok
		moveq	#0,D4
		bra.s	linct_ok

linct_neg	move.b	#NLINES-1,D4

linct_ok	move.b	D4,D0
		lea	History(a5),A0
		ext.w	D0
		mulu	#LEN,D0
		add.l	D0,A0

prevloop	move.b	(A0)+,D0
		beq.s	prev2
		move.b	D0,0(A4,D7.L)
		addq.l	#1,D7
		bra.s	prevloop
prev2		clr.b	0(A4,D7.L)
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
lef9		moveq	#0,D6
		rts

eraseline	bsr.s	gotoleftedge
		moveq	#0,D7
		lea	ClearEol(pc),a0
		bra	printstring_a0_window

*** HEX OUTPUT ROUTINES ***
;
; output a 6 or 8 digit hex number (6 digits if high byte is zero)
;  added checking of 60 or 80 column default font
;  now checks the window width from console unit
;
puthex1_68
		move.l	ConsoleUnit(a5),a0
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

*** PUT LONGWORD (to possibly odd address) ***
PutLong		swap	D0
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
MyCreatePort	movem.l	D2/A6,-(sp)
		moveq	#-1,D0
		call	ExecBase,AllocSignal
		moveq	#-1,D1
		cmp.l	D0,D1
		beq.s	CrepFail
		move.l	D0,D2
		moveq	#MP_SIZE,D0
		move.l	#MEMF_PUBLIC!MEMF_CLEAR,D1
		call	AllocMem
		tst.l	D0
		bne.s	Crep1
		move.b	D2,D0
		call	FreeSignal
		bra.s	CrepFail
Crep1		move.l	D0,-(sp)
		sub.l	A1,A1
		call	FindTask
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
Crep9		movem.l	(sp)+,D2/A6	;port addr in D0 or zero if failed
		rts

*** DELETE MESSAGE PORT ***
* no name (not a public port)
MyDeletePort	;port addr in A1
		movem.l	A2/A6,-(sp)
		moveq	#0,D0
		move.b	MP_SIGBIT(A1),D0
		move.l	A1,A2
		call	ExecBase,FreeSignal
		move.l	A2,A1
		moveq	#MP_SIZE,D0
		call	FreeMem
		movem.l	(sp)+,A2/A6
		rts

*** CREATE IO REQUEST ***
MyCreateIO	;port addr in A1, size in D0
		movem.l	D0/A1/a6,-(sp)
		move.l	#MEMF_PUBLIC!MEMF_CLEAR,D1
		call	ExecBase,AllocMem
		movem.l	(sp)+,D1/A1/a6
		tst.l	D0
		beq.s	CreIO9	;no memory
		move.l	D0,A0
		move.l	A1,MN_REPLYPORT(A0)
		move.b	#NT_MESSAGE,LN_TYPE(A0)
		move.w	D1,MN_LENGTH(A0)
CreIO9		rts

*** DELETE IO REQUEST ***
MyDeleteIO	;IoRequest In A1
		moveq	#0,D0
		move.w	MN_LENGTH(A1),D0
		move.l	a6,-(sp)
		call	ExecBase,FreeMem
		move.l	(sp)+,a6
		rts

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
sendpacket	movem.l	a2-a3/a6,-(sp)
		move.l	ExecBase,a6		only exec will be called here
		move.l	d0,a2			save packet type temporarily
		move.l	a0,d0			check if handler port is zero
		beq	sp9			if it is, just return zero
		move.l	a0,a3			save handler port pointer

		move.l	d1,-(sp)
		moveq	#sp_SIZEOF+MP_SIZE,d0
		move.l	#MEMF_CLEAR!MEMF_PUBLIC,d1
		call	AllocMem
		move.l	(sp)+,d1
		tst.l	d0
		beq.s	sp9			branch if allocmem failed

		move.l	d0,a1
		move.l	a2,sp_Pkt+dp_Type(a1)
		move.l	d0,a2
		movem.l	d1-d7,sp_Pkt+dp_Arg1(a2) save args in DosPacket structure

		moveq	#-1,d0
		call	AllocSignal
		move.b	d0,sp_SIZEOF+MP_SIGBIT(a2)
		bmi.s	sp8			branch if allocsignal failed

; this was missing in original code....
		suba.l	a1,a1
		call	FindTask		find this task
		move.l	d0,sp_SIZEOF+MP_SIGTASK(a2)

		lea	sp_SIZEOF+MP_MSGLIST(a2),a0
		NEWLIST	a0

		lea	sp_Pkt(a2),a0		link StandardPacket and message
		move.l	a0,LN_NAME(a2)		to each other
		move.l	a2,sp_Pkt+dp_Link(a2)
		lea	sp_SIZEOF(a2),a0	pointer to replyport
		move.l	a0,sp_Pkt+dp_Port(a2)

		move.l	a3,a0
		move.l	a2,a1			sp_Msg
		call	PutMsg
		lea	sp_SIZEOF(a2),a0
		call	WaitPort
		lea	sp_SIZEOF(a2),a0
		call	GetMsg

		moveq	#0,d0
		move.b	sp_SIZEOF+MP_SIGBIT(a2),d0
		call	FreeSignal			free signal bit

sp8		move.l	sp_Pkt+dp_Res2(a2),-(sp)	save result codes to stack
		move.l	sp_Pkt+dp_Res1(a2),-(sp)	in reverse order

		move.l	a2,a1
		moveq	#sp_SIZEOF+MP_SIZE,d0
		call	FreeMem				free StandardPacket & MsgPort

		movem.l	(sp)+,d0-d1			get result codes in d0/d1

sp9		movem.l	(sp)+,a2-a3/a6
		rts


**** Command & address tables ****
comtable
		dc.b	'ioxmfthc:arbgjwlsud()&[]<>!=#\^@?',$FF
		even

comadrs	rw	info
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
		rw	calculator

**** HELP TEXT ****
helptext	dc.b	CLS,TAB,TAB,'-- Amiga Monitor Help (version '
		VERSION
		dc.b	') --',LF,LF
		dc.b	'h',TAB,TAB,': help (this)',TAB
		dc.b	'| i',TAB,TAB,TAB,': some information...',LF
		dc.b	'x',TAB,TAB,': exit',TAB,TAB
		dc.b	'| [ addr name',TAB,TAB,': load absolute',LF
		dc.b	'o [name]',TAB,':redirect output'
		dc.b	'| ] addr length name',TAB,': save absolute',LF
		dc.b	'dir [name]',TAB,': directory',TAB
		dc.b	'| < addr dr block cnt',TAB,': read disk blocks',LF
		dc.b	'cd [name]',TAB,': current dir',TAB
		dc.b	'| > addr dr block cnt',TAB,': write disk blocks',LF
		dc.b	'l name',TAB,TAB,': load segment',TAB
		dc.b	'| \',TAB,TAB,TAB,': new CLI',LF
		dc.b	'sl',TAB,TAB,': segment list',TAB
		dc.b	'| ! addr len per [cnt]',TAB,': play digisound',LF
		dc.b	'u',TAB,TAB,': unload segment'
		dc.b	'| = addr',TAB,TAB,': disk block checksum',LF
		dc.b	'r [reg=num]',TAB,': set/show regs',TAB
		dc.b	'| # addr',TAB,TAB,': bootblock checksum',LF
		dc.b	'a addr',TAB,TAB,': assemble',TAB
		dc.b	'| g [addr]',TAB,TAB,': execute (go)',LF
		dc.b	'd addr1 addr2',TAB,': disassemble',TAB
		dc.b	'| j [addr]',TAB,TAB,': jump to subroutine',LF
		dc.b	'm addr1 addr2',TAB,': display memory'
		dc.b	'| w [addr]',TAB,TAB,': single step (walk)',LF
		dc.b	': addr bytes',TAB,': modify memory',TAB
		dc.b	'| ( length',TAB,TAB,': allocate memory',LF
		dc.b	'b addr',TAB,TAB,': set breakpoint'
		dc.b	'| & addr length',TAB,TAB,': allocate absolute',LF
		dc.b	'bl',TAB,TAB,': list brkpoints'
		dc.b	'| ) addr/all',TAB,TAB,': free memory',LF
		dc.b	'br addr/all',TAB,':remove brkpoint'
		dc.b	'| sm',TAB,TAB,TAB,': show allocated mem',LF
		dc.b	'f addr1 addr2 bytes: fill mem',TAB
		dc.b	'| c addr1 addr2 dest',TAB,': compare memory',LF
		dc.b	'@ [line]',TAB,': enter cmd line'
		dc.b	'| t addr1 addr2 dest',TAB,': transfer memory',LF
		dc.b	'ba [decnum]',TAB,': set/show base',TAB
		dc.b	'| h addr1 addr2 bytes',TAB,': hunt memory',LF
		dc.b	'? [expr]',TAB,': calculator',TAB
		dc.b	'| set [var=expr]',TAB,': set/show variables',LF
		dc.b	'mi addr',TAB,TAB,': memory info',TAB
		dc.b	'| cv',TAB,TAB,TAB,': clear variables',LF,0

;

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
t_\1		equ	TokCount
InstrCount	set	InstrCount+1
TokCount	set	TokCount+1
		endm

InstrCount	set	0
TokCount	set	TOKSTART

instradrs	instruction	as		;0
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

;
; disassembler instruction code table
;
;
; structure of the table
;  dc.w mask,value  specifies what bit pattern identifies this instruction
;  dc.b flags       addressing mode and size flags
;  str <string>     instruction "format string" (or special case routine
;		    address)
;
DisAsmTable
; abcd Dn,Dn
		dc.w	$f1f8,$c100
		dc.b	BYTE
		str	<t_abcd,TAB,DR2,',',DRB>
		even
; abcd -(An),-(An)
		dc.w	$f1f8,$c108
		dc.b	BYTE
		str	<t_abcd,TAB,'-(',AR2,'),-(',ARB,')'>
		even
; sbcd Dn,Dn
		dc.w	$f1f8,$8100
		dc.b	BYTE
		str	<t_sbcd,TAB,DR2,',',DRB>
		even
; sbcd -(An),-(An)
		dc.w	$f1f8,$8108
		dc.b	BYTE
		str	<t_sbcd,TAB,'-(',AR2,'),-(',ARB,')'>
		even
; adda.w ea,An
		dc.w	$f1c0,$d0c0
		dc.b	WORD		; all addressing modes allowed
		str	<t_add,'a.w',TAB,EA,',',ARB>
		even
; adda.l ea,An
		dc.w	$f1c0,$d1c0
		dc.b	LONG		; all addressing modes allowed
		str	<t_add,'a.l',TAB,EA,',',ARB>
		even
; suba.w ea,An
		dc.w	$f1c0,$90c0
		dc.b	WORD		; all addressing modes allowed
		str	<t_sub,'a.w',TAB,EA,',',ARB>
		even
; suba.l ea,An
		dc.w	$f1c0,$91c0
		dc.b	LONG		; all addressing modes allowed
		str	<t_sub,'a.l',TAB,EA,',',ARB>
		even
; addx Dn,Dn
		dc.w	$f138,$d100
		dc.b	0
		str	<t_add,'x',SZ,TAB,DR2,',',DRB>
		even
; addx -(An),-(An)
		dc.w	$f138,$d108
		dc.b	0
		str	<t_add,'x',SZ,TAB,'-(',AR2,'),-(',ARB,')'>
		even
; subx Dn,Dn
		dc.w	$f138,$9100
		dc.b	0
		str	<t_sub,'x',SZ,TAB,DR2,',',DRB>
		even
; subx -(An),-(An)
		dc.w	$f138,$9108
		dc.b	0
		str	<t_sub,'x',SZ,TAB,'-(',AR2,'),-(',ARB,')'>
		even
; add ea,Dn
		dc.w	$f100,$d000
		dc.b	0	; all addressing modes are legal
		str	<t_add,SZ,TAB,EA,',',DRB>
		even
; add Dn,ea
		dc.w	$f100,$d100
		dc.b	DATA!ALTER
		str	<t_add,SZ,TAB,DRB,',',EA>
		even
; sub ea,Dn
		dc.w	$f100,$9000
		dc.b	0	; all addressing modes are legal
		str	<t_sub,SZ,TAB,EA,',',DRB>
		even
; sub Dn,ea
		dc.w	$f100,$9100
		dc.b	DATA!ALTER
		str	<t_sub,SZ,TAB,DRB,',',EA>
		even
; addi #data,ea
		dc.w	$ff00,$0600
		dc.b	DATA!ALTER
		str	<t_add,'i',SZ,TAB,IMM,',',EA>
		even
; subi #data,ea
		dc.w	$ff00,$0400
		dc.b	DATA!ALTER
		str	<t_sub,'i',SZ,TAB,IMM,',',EA>
		even
; Dbcc Dn,offs
		dc.w	$f0f8,$50c8
		dc.b	WORD
		str	<'db',CC,TAB,DR2,',',OFFS>
		even
; Scc
		dc.w	$f0c0,$50c0
		dc.b	BYTE!DATA!ALTER
		str	<'s',CC,TAB,EA>
		even
; addq #data,ea
		dc.w	$f100,$5000
		dc.b	ALTER
		str	<t_add,'q',SZ,TAB,IMMQ,',',EA>
		even
; subq #data,ea
		dc.w	$f100,$5100
		dc.b	ALTER
		str	<t_sub,'q',SZ,TAB,IMMQ,',',EA>
		even
; cmpa.w ea,An
		dc.w	$f1c0,$b0c0
		dc.b	WORD	; all addressing modes are legal
		str	<t_cmp,'a.w',TAB,EA,',',ARB>
		even
; cmpa.l ea,An
		dc.w	$f1c0,$b1c0
		dc.b	LONG	; all addressing modes are legal
		str	<t_cmp,'a.l',TAB,EA,',',ARB>
		even
; cmpm (An)+,(An)+
		dc.w	$f138,$b108
		dc.b	0
		str	<t_cmp,'m',SZ,TAB,'(',AR2,')+,(',ARB,')+'>
		even
; cmp ea,Dn
		dc.w	$f100,$b000
		dc.b	0	; all addressing modes are legal
		str	<t_cmp,SZ,TAB,EA,',',DRB>
		even
; cmpi #imm,ea
		dc.w	$ff00,$0c00
		dc.b	DATA!ALTER		;%%68020?
		str	<t_cmp,'i',SZ,TAB,IMM,',',EA>
		even
; divs
		dc.w	$f1c0,$81c0
		dc.b	DATA!WORD
		str	<t_div,'s',TAB,EA,',',DRB>
		even
; divu
		dc.w	$f1c0,$80c0
		dc.b	DATA!WORD
		str	<t_div,'u',TAB,EA,',',DRB>
		even
; muls
		dc.w	$f1c0,$c1c0
		dc.b	DATA!WORD
		str	<t_mul,'s',TAB,EA,',',DRB>
		even
; mulu
		dc.w	$f1c0,$c0c0
		dc.b	DATA!WORD
		str	<t_mul,'u',TAB,EA,',',DRB>
		even
; exg Dn,Dn
		dc.w	$f1f8,$c140
		dc.b	0
		str	<t_exg,TAB,DR2,',',DRB>
		even
;
; my documentation has a bug here. thanks to John van Dijk for informing
; me about it...
;
; exg Dn,An
		dc.w	$f1f8,$c188
		dc.b	LONG
		str	<t_exg,TAB,DRB,',',AR2>
		even
; exg An,An
		dc.w	$f1f8,$c148
		dc.b	LONG
		str	<t_exg,TAB,AR2,',',ARB>
		even
; and ea,Dn
		dc.w	$f100,$c000
		dc.b	DATA
		str	<t_and,SZ,TAB,EA,',',DRB>
		even
; and Dn,ea
		dc.w	$f100,$c100
		dc.b	DATA!MEMORY!ALTER
		str	<t_and,SZ,TAB,DRB,',',EA>
		even
; or ea,Dn
		dc.w	$f100,$8000
		dc.b	DATA
		str	<t_or,SZ,TAB,EA,',',DRB>
		even
; or Dn,ea
		dc.w	$f100,$8100
		dc.b	DATA!MEMORY!ALTER
		str	<t_or,SZ,TAB,DRB,',',EA>
		even
; eor Dn,ea
		dc.w	$f100,$b100
		dc.b	DATA!ALTER
		str	<t_eor,SZ,TAB,DRB,',',EA>
		even
; andi #data,sr/ccr
		dc.w	$ff3f,$023c
		dc.b	0
		str	<t_and,'i',TAB,IMM,',',SCCR>
		even
; ori #data,sr/ccr
		dc.w	$ff3f,$003c
		dc.b	0
		str	<t_or,'i',TAB,IMM,',',SCCR>
		even
; eori #data,sr/ccr
		dc.w	$ff3f,$0a3c
		dc.b	0
		str	<t_eor,'i',TAB,IMM,',',SCCR>
		even
; andi #data,ea
		dc.w	$ff00,$0200
		dc.b	DATA!ALTER
		str	<t_and,'i',SZ,TAB,IMM,',',EA>
		even
; ori #data,ea
		dc.w	$ff00,$0000
		dc.b	DATA!ALTER
		str	<t_or,'i',SZ,TAB,IMM,',',EA>
		even
; eori #data,ea
		dc.w	$ff00,$0a00
		dc.b	DATA!ALTER
		str	<t_eor,'i',SZ,TAB,IMM,',',EA>
		even
; shift? ea
		dc.w	$f8c0,$e0c0
		dc.b	DATA!MEMORY!ALTER!WORD
		str	<STP2,SD,TAB,EA>
		even
; shift? Dn,Dn
		dc.w	$f020,$e020
		dc.b	0
		str	<STP1,SD,SZ,TAB,DRB,',',DR2>
		even
; shift? #imm,Dn
		dc.w	$f020,$e000
		dc.b	0
		str	<STP1,SD,SZ,TAB,IMMQ,',',DR2>
		even
; Bcc addr
		dc.w	$f0ff,$6000
		dc.b	WORD
		str	<'b',CC,TAB,OFFS>
		even
; Bcc.s addr
		dc.w	$f000,$6000
		dc.b	BYTE
		str	<'b',CC,'.s',TAB,SOFFS>
		even
; movep.w offs(An),Dn
		dc.w	$f1f8,$0108
		dc.b	WORD
		str	<t_move,'p.w',TAB,IMMOFFS,'(',AR2,'),',DRB>
		even
; movep.l offs(An),Dn
		dc.w	$f1f8,$0148
		dc.b	LONG
		str	<t_move,'p.l',TAB,IMMOFFS,'(',AR2,'),',DRB>
		even
; movep.w Dn,offs(An)
		dc.w	$f1f8,$0188
		dc.b	WORD
		str	<t_move,'p.w',TAB,DRB,',',IMMOFFS,'(',AR2,')'>
		even
; movep.l Dn,offs(An)
		dc.w	$f1f8,$01c8
		dc.b	LONG
		str	<t_move,'p.l',TAB,DRB,',',IMMOFFS,'(',AR2,')'>
		even
;
; btst has more legal addressing modes than other b??? instructions
; %%note that even btst Dn,#imm is legal (with size of BYTE).
;
; btst Dn,ea
		dc.w	$f1c0,$0100
		dc.b	DATA!BYTE
		str	<'b',BIT,TAB,DRB,',',EA>
		even
; btst #imm,ea
		dc.w	$ffc0,$0800
		dc.b	DATA!NIMM
		str	<'b',BIT,TAB,'#',IMMOFFS,',',EA>
		even
; b??? Dn,ea
		dc.w	$f100,$0100
		dc.b	DATA!ALTER
		str	<'b',BIT,TAB,DRB,',',EA>
		even
; b??? #imm,ea
		dc.w	$ff00,$0800
		dc.b	DATA!ALTER
		str	<'b',BIT,TAB,'#',IMMOFFS,',',EA>
		even
; chk ea,Dn
		dc.w	$f1c0,$4180
		dc.b	DATA!WORD
		str	<t_chk,TAB,EA,',',DRB>
		even
; clr ea
		dc.w	$ff00,$4200
		dc.b	DATA!ALTER
		str	<t_clr,SZ,TAB,EA>
		even
; nbcd ea
		dc.w	$ffc0,$4800
		dc.b	DATA!ALTER!BYTE
		str	<t_nbcd,TAB,EA>
		even
; ext.w Dn
		dc.w	$fff8,$4880
		dc.b	WORD
		str	<t_ext,'.w',TAB,DR2>
		even
; ext.l Dn
		dc.w	$fff8,$48c0
		dc.b	LONG
		str	<t_ext,'.l',TAB,DR2>
		even
; jmp ea
		dc.w	$ffc0,$4ec0
		dc.b	CONTROL
		str	<t_jmp,TAB,EA>
		even
; jsr ea
		dc.w	$ffc0,$4e80
		dc.b	CONTROL
		str	<t_jsr,TAB,EA>
		even
; lea ea,An
		dc.w	$f1c0,$41c0
		dc.b	CONTROL
		str	<t_lea,TAB,EA,',',ARB>
		even
; link An,#imm
		dc.w	$fff8,$4e50
		dc.b	WORD
		str	<t_link,TAB,AR2,',#',IMMOFFS>
		even
; unlk An
		dc.w	$fff8,$4e58
		dc.b	0
		str	<t_unlk,TAB,AR2>
		even
; illegal
		dc.w	$ffff,$4afc
		dc.b	0
		str	<t_illegal>
		even
; tas ea
		dc.w	$ffc0,$4ac0
		dc.b	DATA!ALTER!BYTE
		str	<t_tas,TAB,EA>
		even
; move usp,An
		dc.w	$fff8,$4e68
		dc.b	LONG
		str	<t_move,TAB,'usp,',AR2>
		even
; move An,usp
		dc.w	$fff8,$4e60
		dc.b	LONG
		str	<t_move,TAB,AR2,',usp'>
		even
; movea.w ea,An
		dc.w	$f1c0,$3040
		dc.b	WORD
		str	<t_move,'a.w',TAB,EA,',',ARB>
		even
; movea.l ea,An
		dc.w	$f1c0,$2040
		dc.b	LONG
		str	<t_move,'a.l',TAB,EA,',',ARB>
		even
; move.b ea,ea
		dc.w	$f000,$1000
		dc.b	BYTE!SPECIAL_CASE
		dc.b	6
		rw	handle_move
		dc.b	t_move,'.b',TAB
		even
; move.w ea,ea
		dc.w	$f000,$3000
		dc.b	WORD!SPECIAL_CASE
		dc.b	6
		rw	handle_move
		dc.b	t_move,'.w',TAB
		even
; move.l ea,ea
		dc.w	$f000,$2000
		dc.b	LONG!SPECIAL_CASE
		dc.b	6
		rw	handle_move
		dc.b	t_move,'.l',TAB
		even
; movem.w from memory
		dc.w	$ffc0,$4c80
		dc.b	MEMORY!NIMM!WORD!SPECIAL_CASE
		dc.b	7
		rw	handle_movem
		dc.b	t_move,'m.w',TAB
		even
; movem.w to memory
		dc.w	$ffc0,$4880
		dc.b	MEMORY!ALTER!WORD!SPECIAL_CASE
		dc.b	7
		rw	handle_movem
		dc.b	t_move,'m.w',TAB
		even
; movem.l from memory
		dc.w	$ffc0,$4cc0
		dc.b	MEMORY!NIMM!LONG!SPECIAL_CASE
		dc.b	7
		rw	handle_movem
		dc.b	t_move,'m.l',TAB
		even
; movem.l to memory
		dc.w	$ffc0,$48c0
		dc.b	MEMORY!ALTER!LONG!SPECIAL_CASE
		dc.b	7
		rw	handle_movem
		dc.b	t_move,'m.l',TAB
		even
; moveq #imm,Dn
		dc.w	$f100,$7000
		dc.b	LONG
		str	<t_move,'q',TAB,IMVQ,',',DRB>
		even
; move ea,ccr
		dc.w	$ffc0,$44c0
		dc.b	DATA!BYTE
		str	<t_move,TAB,EA,',ccr'>
		even
; move ea,SR
		dc.w	$ffc0,$46c0
		dc.b	DATA!WORD
		str	<t_move,TAB,EA,',sr'>
		even
; move sr,ea
		dc.w	$ffc0,$40c0
		dc.b	DATA!ALTER!WORD
		str	<t_move,TAB,'sr,',EA>
		even
; neg ea
		dc.w	$ff00,$4400
		dc.b	DATA!ALTER
		str	<t_neg,SZ,TAB,EA>
		even
; negx ea
		dc.w	$ff00,$4000
		dc.b	DATA!ALTER
		str	<t_neg,'x',SZ,TAB,EA>
		even
; not ea
		dc.w	$ff00,$4600
		dc.b	DATA!ALTER
		str	<t_not,SZ,TAB,EA>
		even
; swap Dn
		dc.w	$fff8,$4840
		dc.b	0
		str	<t_swap,TAB,DR2>
		even
; pea ea
		dc.w	$ffc0,$4840
		dc.b	CONTROL
		str	<t_pea,TAB,EA>
		even
; reset
		dc.w	$ffff,$4e70
		dc.b	0
		str	<t_reset>
		even
; nop
		dc.w	$ffff,$4e71
		dc.b	0
		str	<t_nop>
		even
; stop
		dc.w	$ffff,$4e72
		dc.b	WORD
		str	<t_stop,TAB,IMM>
		even
; rte
		dc.w	$ffff,$4e73
		dc.b	0
		str	<t_rte>
		even
; rts
		dc.w	$ffff,$4e75
		dc.b	0
		str	<t_rts>
		even
; trapv
		dc.w	$ffff,$4e76
		dc.b	0
		str	<t_trap,'v'>
		even
; rtr
		dc.w	$ffff,$4e77
		dc.b	0
		str	<t_rtr>
		even
; trap
		dc.w	$fff0,$4e40
		dc.b	0
		str	<t_trap,TAB,TRAPN>
		even
; tst ea
		dc.w	$ff00,$4a00
		dc.b	DATA!ALTER	;%%68020?
		str	<t_tst,SZ,TAB,EA>
		even
; line-a
		dc.w	$f000,$a000
		dc.b	0
		str	<'line-a'>
		even
; line-f
		dc.w	$f000,$f000
		dc.b	0
		str	<'line-f'>
		even
; end of table marker
		dc.w	0,0		matches anything
		dc.b	SPECIAL_CASE
		dc.b	3
		rw	invalid
		dc.b	0

; why don't asemblers warn if you put word data in odd address ???
		even

;
; instruction jump table for assembler
;
instrjumps
		rw	as_asm,ls_asm,rox_asm,rot_asm		;0-3
		rw	move_asm,add_asm,sub_asm
		rw	and_asm,or_asm
		rw	abcd_asm,sbcd_asm
		rw	mul_asm,div_asm
		rw	exg_asm,eor_asm,cmp_asm
		rw	btst_asm,bchg_asm,bclr_asm,bset_asm
		rw	chk_asm,lea_asm,ext_asm,clr_asm
		rw	neg_asm,not_asm,tst_asm,nbcd_asm
		rw	swap_asm,pea_asm,link_asm,unlk_asm
		rw	reset_asm,nop_asm,stop_asm,rte_asm,tas_asm
		rw	rts_asm,trapv_asm,rtr_asm
		rw	jsr_asm,jmp_asm,trap_asm
		rw	illegal_asm

**** INFO TEXT ****
infotext dc.b CLS,LF
		dc.b	TAB,TAB,"Monitor info (version "
		VERSION
		dc.b	')',LF
		dc.b	TAB,TAB,'---------------------------',LF
		dc.b	TAB,TAB,'by Timo Rossi (c) 1987-1989',LF,LF
		dc.b	"   This is a machine code monitor for the Amiga.",LF
		dc.b	" Pressing the HELP-key displays a list of commands.",LF,LF
		dc.b	" Note1: Some of the assembler commands require the",LF
		dc.b	" size specifier (.B, .W or .L), but it can't be used by some others.",LF,LF
		dc.b	" Note2: the default number base is again hex, use '_' as prefix",LF
		dc.b	" for decimal or change the base with the ba-command.",LF
		dc.b	" You can now use expressions in most places where",LF
		dc.b	" numbers are needed.",LF,LF
		dc.b	" This program can be freely distributed for non-commercial purposes.",LF
		dc.b	" I hope you find this program useful, but if you find any bugs in this",LF
		dc.b	" program, please let me know.",LF,LF
		dc.b	" Read the 'mon.doc'-file for more information. (My address is also there)",LF,0

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

welcometxt	dc.b	CLS,LF,TAB,TAB,TAB,' --- Amiga Monitor ---',LF,LF
		dc.b	TAB,TAB,'by Timo Rossi (c) 1987-1989,  version '
		VERSION
		dc.b	LF,LF,0

prompt		dc.b	'-> ',0
cmdlinetxt	dc.b	'Cmdline> ',0
calctxt		dc.b	'Calc> ',0

breaktx		dc.b	'*** Break ***',LF,0
errtx		dc.b	'???',0
outrangetxt	dc.b	'Out of range',0
expr_errtxt	dc.b	'expr error',0
memerr		dc.b	'Out of memory',0
doserrfmt	dc.b	'DOS error '
numfmt		dc.b	'%ld',LF,0
td_errfmt	dc.b	'Trackdisk error %ld',0
nodisktxt	dc.b	'No disk in drive',0
wrprotxt	dc.b	'Disk write protected',0

NewCliCom	dc.b	'NewCLI',0

comhfmt		dc.b	'%08lx ',0
sumfmt		dc.b	'Old: $%08lx New: $%08lx',LF,0
noBrkTxt	dc.b	'No Breakpoints set',LF,0
brklistTx	dc.b	'Breakpoints:',LF,0
audiotxt	dc.b	'Press Ctrl-C to stop...',LF,0
ulserr		dc.b	'Unload old segment first',LF,0

segadrmes	dc.b	'First segment at '
hexfmt		dc.b	'$%08lx',LF,0

allocfmt	dc.b	'Allocated from $%08lx to $%08lx',LF,0
rangefmt	dc.b	'%ld bytes read from $%08lx to $%08lx',LF,0

allocfail	dc.b	'Allocation failed',LF,0
noalloctx	dc.b	'Not allocated that',LF,0

no_alloc	dc.b	'No memory '
allotxt		dc.b	'allocated',LF,0
memlisttx	dc.b	'Allocated memory:',LF,0

seglistfmt	dc.b	'%3ld  '
memlistfmt	dc.b	'$%08lx  $%08lx  %ld',LF,0

nosegmes	dc.b	'No segment loaded',LF,0

novartxt	dc.b	'No variables defined',LF,0
varhead		dc.b	'Variables:',LF,0
varfmt		dc.b	'%s = $%08lx (%ld)',LF,0
clvartxt	dc.b	'Clear vars (y/n)? ',0
basefmt		dc.b	'Base is %ld',LF,0

notmemtxt	dc.b	'Not in MemList',LF,0
inhunkfmt	dc.b	'(in hunk %ld)',LF,0

seghead		dc.b	'Segment list:',LF,'  # '
loctext		dc.b	' startloc   endloc    length',LF,0

assemfmt	dc.b	'%08lx: ',0

dnam		dc.b	'(dir)',0
freeblkfmt	dc.b	'%ld Blocks free.',LF,0

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
unknowntrap	dc.b	'Exception #',0

		ifd	DEBUG
frametypefmt	dc.b	'Frame type #$%02lx',LF,0
		endc

		ds.w 0

trapnamtabl	rw	buserrnam	;0
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

monitor_code_end

		END
