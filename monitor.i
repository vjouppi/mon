;
; monitor.i
;

;
; include file for Amiga Monitor
; version 1.59 -- 1993-05-05
; Copyright © 1993 by Timo Rossi
;
		ifnd	EXEC_TYPES_I
		include	"exec/types.i"
		endc

		include	"mon_macros.i"
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
		lea	mon_OutputBuf(a4),a3
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
\1_cmd		;equ	*
		endm


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

LEN		equ	128	;length of input & output buffers
NLINES		equ	10	;number of lines of command line history

DNBUFSIZE	equ	50	;length of disk device name buffer

DISKBLOCKSIZE	equ	512	; 9
DISKBLOCKSHIFT	equ	9	;2 = 512
BOOTBLOCKSIZE	equ	2*DISKBLOCKSIZE

MONSTACK	equ	2000

ILLEGAL_INSTR	equ	$4AFC	;illegal instruction (used by breakpoints)

** Data structure containing all monitor internal variables **

		STRUCTURE MonitorData,0
; first the long word variables
		 APTR	_ExecBase
		 APTR	_DosBase	;DOS library base
		 APTR	mon_WBenchMsg	;Workbench startup message pointer
		 APTR	mon_Task	;pointer to TCB of monitor process
		 LONG	mon_StackSize	;monitor stack size

		 APTR	mon_OrigRetAddr		;monitor process pr_ReturnAddr
						;when monitor was started
		 APTR	mon_OrigTrapCode	;original task trap code ptr
		 APTR	mon_OrigTrapData	;original task user data ptr
		 LONG	mon_OrigCD		;original current dir (BPTR)
						;(-1 if no change)
		 LONG	mon_OrigConTask		;original pr_ConsoleTask

		 LONG	mon_WinFile	;main window file handle
		 LONG	mon_OutputFile	;current output file handle

		 APTR	mon_ConsoleUnit	;console device unit pointer for main window
		 APTR	mon_StackPtr	;saved monitor stack pointer

		 APTR	mon_InitialFileName	;file name given in command line
		 APTR	mon_InitialScriptName	;file name given in -s option

		 LONG	mon_SegList		;currently loaded
						;exefile seglist (BPTR)
		 LONG	mon_NumHunks		;number of hunks in currently
						;loaded exefile segment list
		 APTR	mon_HunkTypeTable	;pointer to array of hunk type
						;values, used only if '+'-option
						;of the 'l'-command has been used

		 APTR	mon_MemoryList	;linked list of allocated memory
		 APTR	mon_BreakList	;linked list of breakpoints
		 APTR	mon_VarList	;linked list of variables

		 APTR	mon_ScriptList	;linked list of currently active scripts

		 APTR	mon_CurrentAddr	;current address
		 APTR	mon_EndAddr	;for disassembler & memdisplay

		 APTR	mon_StackHigh	;high limit of stack used
					;by g- and j-commands

		 APTR	mon_TempBreakAddr	;address of temporary breakpoint
		 APTR	mon_GoBreakPtr	;address of skipped brkpoint struct

		 APTR	mon_RelBaseAddr	;address pointed by a4 or other base reg
;
; the following two variables are for the disassemble routine
; (to avoid some kludges...)
;
		 APTR	mon_dis_StackStore
		 APTR	mon_dis_Addr1

; the output buffer must be long word aligned
		 STRUCT	mon_OutputBuf,LEN
		 STRUCT	mon_InputBuf,LEN
		 STRUCT	mon_History,LEN*NLINES
		 STRUCT	mon_CmdLineBuf,LEN
		 STRUCT mon_DevNameBuf,DNBUFSIZE

; processor register storage
		 STRUCT	mon_DataRegs,8*4	;data registers
		 STRUCT	mon_AddrRegs,8*4	;address registers
mon_RegSP	equ	mon_AddrRegs+7*4
		 APTR	mon_RegPC		;program counter
;
; note: the following is because move ea,ccr moves a word and ignores
; the high byte of it...
;
		 UBYTE	mon_RegCCR_W
		 UBYTE	mon_RegCCR_B	;condition code register

; now word and byte variables
		 UWORD	mon_TempBreakSave	;storage for original contents
						;of a temporary breakpoint

		 UBYTE	mon_Flags	;flags, see below for bitdefs...
		 UBYTE	mon_DefNumBase	;current default number base for input
		 UBYTE	mon_Options	;option flags
		 UBYTE	mon_RelBaseReg	;base register, works with RelBaseAddr
					;-1 if not in use
		LABEL MonitorData_SIZE
;
; Hmm.. I think that I put this check here for a good reason, but I can't
; remember what is was... probably a bug in assembler or something...
;
		ifne	MonitorData_SIZE-$77a
		fail	Panic! MonitorData wrong size!
		endc

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
; Monitor option flags
;
		BITDEF	OPT,NARROWDIS,0
		BITDEF	OPT,EXTPRTCHR,1
		BITDEF	OPT,DUMBTERM,2
		BITDEF	OPT,CMDECHO,3
		BITDEF	OPT,STACKRESET,4

MON_NUM_OPTIONS	equ	5
