;
; monitor.i
;

;
; include file for Amiga Monitor
; version 1.44 -- 1991-10-27
; Copyright © 1991 by Timo Rossi
;


		include	"exec/types.i"
		include	"exec/nodes.i"
		include	"exec/lists.i"
		include	"exec/memory.i"
		include	"exec/tasks.i"
		include	"exec/errors.i"
		include	"exec/devices.i"
		include	"exec/io.i"
		include	"exec/execbase.i"
		include	"libraries/dos.i"
		include	"libraries/dosextens.i"
		include	"graphics/gfx.i"
		include	"graphics/gfxbase.i"
		include	"devices/trackdisk.i"
		include	"devices/console.i"
		include	"devices/conunit.i"
		include	"devices/audio.i"
		include	"workbench/startup.i"

		include	"offsets.i"


*** This macro is an easy way to update the version number ***

BETA		equ	0	;special 'Beta version' flag

VERSION		macro
		dc.b	'1.46'
		ifne	BETA
		dc.b	'b'
		endc
		endm

;;;;;
;
; clear carry
clc		macro
		and	#%11111110,CCR
		endm

; set carry
sec		macro
		or	#%00000001,CCR
		endm

; force even alignment
even		macro
		ds.w	0
		endm

; clear address register
clra		macro	;An
		suba.l	\1,\1
		endm

; define a string with length byte in beginning
; call: str <string>
str		macro
		dc.b	stryy\@-strxx\@
strxx\@		dc.b	\1
stryy\@
		endm

; define a word (or words) relative to itself
; call: rw label1,label2...
rw		macro
		dc.w	\1-*
		ifnc	'\2',''
		rw	\2,\3,\4,\5,\6,\7,\8,\9
		endc
		endm

; the only absolute address in the Amiga operating system
_ExecBase	equ	4

;
; new library call macros
;
getbase		macro
		ifc	'\1','Exec'
		  ifeq	NARG-2
		    move.l _ExecBase,\2
		  endc
		  ifeq	NARG-1
		    move.l _ExecBase,a6
		  endc
		endc
		ifnc	'\1','Exec'
		  ifeq	NARG-2
		    move.l _\1Base(a4),\2
		  endc
		  ifeq	NARG-1
		    move.l _\1Base(a4),a6
		  endc
		endc
		endm

lib		macro
		ifeq	NARG-2
		  getbase	\1
		  lib	\2
		endc
		ifeq	NARG-1
		  jsr	_LVO\1(a6)
		endc
		endm

jlib		macro
		ifeq	NARG-2
		  getbase  \1
		  jlib	\2
		endc
		ifeq	NARG-1
		  jmp	_LVO\1(a6)
		endc
		endm

slib		macro
		move.l  a6,-(sp)
		lib	\1,\2
		move.l  (sp)+,a6
		endm

*** macro to display a single character ***

emit		macro
		moveq	#\1,D0
		call	ChrOut
		endm

emitwin		macro
		moveq	#\1,d0
		call	ChrOutWin
		endm

*** start output line ***
startline	macro
		lea	OutputBuf(a4),a3
		endm

*** end output line (line feed+NULL) ***
endline		macro
		putchr	LF
		clr.b	(A3)
		endm

putchr		macro
		move.b	#\1,(a3)+
		endm

;
; define a command label
;
cmd		macro
		xdef	\1_cmd
\1_cmd		equ	*
		endm

;
; define a public routine
;
pub		macro
		xdef	\1_routine
\1_routine	equ	*
		endm
;
; call a public routine
;
call		macro
		ifnc	'\1','JUMP'
		  ifnd	\1_routine
		     xref  \1_routine
		  endc
		  bsr	\1_routine
		endc
		ifc	'\1','JUMP'
		  ifnd	\2_routine
		    xref  \2_routine
		  endc
		  bra	\2_routine
		endc
		endm

; definitions of the instruction size variable
BSIZE		equ	0
WSIZE		equ	1
LSIZE		equ	2


; some common ASCII control characters
BS		equ	8
TAB		equ	9
LF		equ	$0a
FF		equ	$0c
CR		equ	$0d
DEL		equ	$7f
ESC		equ	$1b
CSI		equ	$9b

*** SOME SPECIAL CHARACTERS ***
CtrlC	equ	3	;control-c, break
CtrlE	equ	5	;control-e, edit existing assembler instruction
CtrlQ	equ	17	;control-q, xon
CtrlS	equ	19	;control-s, xoff
CtrlX	equ	24	;control-x, clear input line

SPACE	equ	32
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


LEN		equ	100	;length of input & output buffers
NLINES		equ	10	;number of lines of command line history

DNBUFSIZE	equ	50	;length of disk device name buffer

DISKBLOCKSIZE	equ	512	; 9
DISKBLOCKSHIFT	equ	9	;2 = 512
BOOTBLOCKSIZE	equ	2*DISKBLOCKSIZE

MONSTACK	equ	2000

ILLEGAL_INSTR	equ	$4AFC	;illegal instruction (used by breakpoints)

*** BREAKPOINT STRUCTURE ***

		STRUCTURE BrkPoint,0
		 APTR	brk_Next	;linked list
		 APTR	brk_Address	;address of breakpoint
		 UWORD	brk_Count	;count required for activation
		 UWORD	brk_ActCount	;actual count when active
		 UWORD	brk_Content	;word contents of that location
		LABEL brk_SIZE	;because it is temporarily replaced by ILLEGAL

*** Variable structure ***
; the size of this structure depends on the length of the variable name

		STRUCTURE Variable,0
		 APTR	var_Next	;linked list
		 LONG	var_Value	;32-bit interger value of variable
		 WORD	var_Length	;size of structure for FreeMem()
		LABEL	var_Name	;null-terminated string

** Data structure containing all monitor internal variables **

		STRUCTURE MonitorData,0
; first the long word variables
		 APTR	_DosBase	;DOS library base
		 APTR	WBenchMsg	;Workbench startup message pointer
		 APTR	MyTask		;pointer to TCB of monitor process
		 LONG	StackSize	;monitor stack size
		 APTR	OldRetAddr	;monitor process pr_ReturnAddr when monitor was started
		 APTR	OldTrapCode	;task trap code when monitor was started
		 APTR	OldUserData	;task user data when monitor was started

		 LONG	WinFile		;main window file handle
		 LONG	OutputFile	;current output file handle

		 LONG	OldCD		;current dir when monitor was started

		 APTR	ConsoleUnit	;console device unit pointer for main window
		 APTR	StackPtr	;saved monitor stack pointer

		 APTR	InitialFileName	;file name given in command line
		 LONG	SegList		;currently loaded segment

		 APTR	MemoryList	;linked list of allocated memory
		 APTR	BreakList	;linked list of breakpoints
		 APTR	VarList		;linked list of variables

		 APTR	Addr		;current address
		 APTR	EndAddr		;for disassembler & memdisplay
		 APTR	instrad		;instruction address for disassembler

		 APTR	StackHigh	;high limit of stack used
					;by g- and j-commands

		 APTR	TmpBrkAddr	;address of temporary breakpoint

		 APTR	GoBrkPtr	;address of skipped brkpoint struct

; the output buffer must be long word aligned
		 STRUCT	OutputBuf,LEN
		 STRUCT	InputBuf,LEN
		 STRUCT	History,LEN*NLINES
		 STRUCT	CmdLineBuf,LEN
		 STRUCT DevNameBuf,DNBUFSIZE

; processor register storage
		 STRUCT	DataRegs,8*4	;data registers
		 STRUCT	AddrRegs,8*4	;address registers
RegSP		equ	AddrRegs+7*4
		 APTR	RegPC		;program counter
;
; note: the following is because move ea,ccr moves a word and ignores
; the high byte of it...
;
		 UBYTE	RegCCR_W
		 UBYTE	RegCCR_B	;condition code register

; now word and byte variables
		 UWORD	TmpBrkSave	;storage for original contents
					;of a temporary breakpoint

		 UWORD	opcode		;opcode for disassembler

		 UWORD	size		;current instruction size
		 UBYTE	flags		;flags, see below for bitdefs...
		 UBYTE	defbase		;current default number base for input
		 UBYTE	MonOptions	;option flags
		 UBYTE	__pad__
		LABEL MonitorData_SIZE

;
; monitor flags
;

		BITDEF	MON,BRKACTIVE,0
		BITDEF	MON,BRKSKIP,1
		BITDEF	MON,TMPBRK,2
		BITDEF	MON,QTRACE,3
		BITDEF	MON,OWNWINDOW,4
		BITDEF	MON,FIRSTCD,5
		BITDEF	MON,TASKSET,6

;
; Monitor option flags (note: they now start from zero, not one...1.42)
;
		BITDEF	OPT,NARROWDIS,0
		BITDEF	OPT,EXTPRTCHR,1
		BITDEF	OPT,DUMBTERM,2

