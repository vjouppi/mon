************************************
*                                  *
*    Amiga machine code monitor    *
*      Timo Rossi   1987-1988      *
*                                  *
*     last modified 1988-05-02     *
*                                  *
************************************

 include 'exec/types.i'
 include 'exec/nodes.i'
 include 'exec/lists.i'
 include 'exec/tasks.i'
 include 'exec/ports.i'
 include 'exec/memory.i'
 include 'exec/libraries.i'
 include 'exec/devices.i'
 include 'exec/io.i'
 include 'exec/interrupts.i'
 include 'exec/execbase.i'
 include 'devices/trackdisk.i'
 include 'devices/audio.i'
 include 'libraries/dos.i'
 include 'libraries/dosextens.i'
 include 'graphics/gfx.i'
 include 'graphics/gfxbase.i'

*** This macro is an easy way to update the version number ***
VERSION macro
 dc.b '1.06-p'
 endm

*** macro to generate 16-bit self-relative addresses ***
relword macro
belymt set \1-*
 dc.w belymt
 endm

*** macro to generate external references to library vector offsets ***
xlib macro
 xref _LVO\1
 endm

*** macro to call system routines ***
callsys macro
 jsr _LVO\1(A6)
 endm

linksys macro
 move.l A6,-(sp)
 move.l \2,A6
 callsys \1
 move.l (sp)+,A6
 endm

linkexec macro
 linksys \1,_AbsExecBase
 endm

*** macro to display a single character ***
emit macro
 moveq #\1,D0
 bsr ChrOut
 endm

*** start input line (for StrOut) ***
startline macro
 move.l outbufadr(A5),A3
 move.l A3,A0
 endm

*** end output line (line feed+NULL) ***
endline macro
 move.b #LF,(A3)+
 clr.b (A3)
 endm

* because of this definition, the assembler can use absoluse short addr mode
_AbsExecBase equ 4

*** the following system routines are used ***
 xlib OpenLibrary
 xlib CloseLibrary
 xlib AllocMem
 xlib AllocAbs
 xlib FreeMem
 xlib GetCC
 xlib FindTask
 xlib Forbid
 xlib WaitPort
 xlib Wait
 xlib SetSignal
 xlib GetMsg
 xlib ReplyMsg
 xlib AllocSignal
 xlib FreeSignal
 xlib OpenDevice
 xlib CloseDevice
 xlib DoIO
 xlib Open
 xlib Close
 xlib Read
 xlib Write
 xlib Input
 xlib Output
 xlib WaitForChar
 xlib Info
 xlib CurrentDir
 xlib DeleteFile
 xlib Examine
 xlib ExNext
 xlib Lock
 xlib UnLock
 xlib IoErr
 xlib LoadSeg
 xlib UnLoadSeg
 xlib Execute

*** SOME SPECIAL CHARACTERS ***
CtrlC  equ   3 ;control-c, break
CtrlE  equ   5 ;control-key to edit existing assembler instruction
BS     equ   8 ;backspace
TAB    equ   9 ;tabulator
LF     equ  10 ;line feed (new line)
CtrlK  equ  11 ;cursor up
CLS    equ  12 ;clear screen (form feed)
CR     equ  13 ;carriage return (moves cursor to start to the current line)
CtrlX  equ  24 ;control-x, clear input line
SPACE  equ  32
DEL    equ 127 ;delete current char
CSI    equ $9B ;control sequence introducer for ANSI-sequences
NCSI   equ CSI-$100 ;this one can be used with MOVEQ

*** SOME SPECIAL KEY CODES (returned by GetKey)
CURSOR_UP          equ $0100
CURSOR_DOWN        equ $0200
CURSOR_RIGHT       equ $0300
CURSOR_LEFT        equ $0400
SHIFT_CURSOR_UP    equ $0500
SHIFT_CURSOR_DOWN  equ $0600
SHIFT_CURSOR_LEFT  equ $0700
SHIFT_CURSOR_RIGHT equ $0800

LEN    equ  80 ;length of input & output buffers
NLINES equ  10 ;number of lines of command line history

ILLEGAL equ $4AFC ;illegal instruction (used by breakpoints)

*** BREAKPOINT STRUCTURE ***
* struct BreakPoint { 
*  struct BreakPoint *NextBreak; /* linked list */
*  CPTR   BreakAddress;  /* address of breakpoint */
*  UWORD  BreakContents; /* word contents of that location */
* };      /* because it is temporarily replaced by ILLEGAL */

brk_Address equ  4
brk_Content equ  8
brk_SIZE    equ 10

** local variables **
winfile     equ -4   ;file handle for main window
Addr        equ -8   ;current address, used by many commands
EndAddr     equ -12  ;end address for disassembler & memdisplay
temp        equ -16  ;temporary storage
segment     equ -20  ;BPTR to currently loaded segment, zero if none
instrad     equ -24  ;address of the instruction (disassembler)
opcode      equ -26  ;operation code of instruction (disassembler)
outbufadr   equ -30  ;address of output buffer (avoids absolute addr mode)
memlist     equ -34  ;pointer to linked list of allocated memory
OldTrapCode equ -38  ;TC_TRAPCODE, when monitor is started
WBenchMsg   equ -42  ;workbench startup message
thistask    equ -46  ;pointer to this tasks task control block
OutputFile  equ -50  ;output file handle (redirection)
BreakList   equ -54  ;pointer to linked list of breakpoints
size        equ -56  ;instruction size(0=B,1=W,2=L),used by assembler
flags       equ -58  ;only bit 0 used: breakpoints set
inpspecial  equ -60  ;input special mode, see GetInput:

 CODE
*** THIS IS THE WORKBENCH/CLI STARTUP CODE ***
startup:
 link A5,#-60 ;allocate space for local variables
 clr.l WBenchMsg(A5)
 move.l _AbsExecBase,A6
 suba.l A1,A1          ;A1:=0
 callsys FindTask      ;find our task
 move.l D0,thistask(A5)
 move.l D0,A4
 tst.l pr_CLI(A4)      ;started from CLI ?
 bne.s main            ;branch if yes
 lea pr_MsgPort(A4),A0 ;started from workbench
 callsys WaitPort      ;wait for WB startup message
 lea pr_MsgPort(A4),A0
 callsys GetMsg        ;get it
 move.l D0,WBenchMsg(A5)
main:
*** Find the display height from GfxBase, so this program
*** works on both PAL and NTSC machines
 lea gfxname(pc),A1
 moveq #33,D0 ;version 33: kickstart 1.2 or later
 callsys OpenLibrary
 tst.l D0
 beq exit9
 move.l D0,A1
 moveq #0,D2
 move.w gb_NormalDisplayRows(A1),D2
 callsys CloseLibrary
 sub.w #16,D2 ;the actual window height is 16 less
 lea winheight+3(pc),A0
 moveq #3-1,D3 ;convert window height to ASCII string
winheightloop: ;for the Open("CON:....",MODE_OLDFILE)... 
 divu #10,D2
 swap D2
 add.b #'0',D2
 move.b D2,-(A0)
 clr.w D2
 swap D2
 dbf D3,winheightloop

 moveq #0,D0
 move.l D0,Addr(A5)    ;clear some variables
 move.l D0,segment(A5)
 move.l D0,memlist(A5)
 move.l D0,BreakList(A5)
 move.w D0,flags(A5)
 lea dosname(pc),A1
 callsys OpenLibrary   ;Open the DOS library...
 tst.l D0
 beq exit9
 move.l D0,A6
 lea window(pc),A0     ;Open the Window
 move.l A0,D1
 move.l #MODE_OLDFILE,D2
 callsys Open
 move.l D0,winfile(A5)
 beq exit8
 move.l D0,OutputFile(A5) ;default output is monitor window
 move.l #OutBuf,outbufadr(A5)
 lea welcometxt(pc),A0
 bsr StrOutWindow             ;display welcome message
 move.l A7,A0
 sub.w #$100,A0         ;a safe area from this task's stack
 move.l A0,AddrRegs+4*7 ;set stack pointer
 lea trapreturn(pc),A1
 move.l thistask(A5),A0
 move.l TC_TRAPCODE(A0),OldTrapCode(A5) ;save old TrapCode
 move.l A1,TC_TRAPCODE(A0)              ;and set a new one

*** JUMP HERE AFTER EXECUTION OF A COMMAND ***
mainloop:
 moveq #0,D0 ;clear CTRL-C/D/E/F flags
 move.l #SIGBREAKF_CTRL_C!SIGBREAKF_CTRL_D!SIGBREAKF_CTRL_E!SIGBREAKF_CTRL_F,D1
 move.l A6,A4
 move.l _AbsExecBase,A6
 callsys SetSignal
 move.l A4,A6
 lea prompt(pc),A0
 bsr StrOutWindow       ;display prompt
 moveq #0,D0
 bsr GetInput
 move.b (A3)+,D0
 bne.s ml1
 move.l winfile(A5),D0  ;empty command line
 cmp.l OutputFile(A5),D0
 beq.s mainloop
 move.l outbufadr(A5),A0
 move.w #$0A00,(A0)
 bsr StrOutRedirect     ;print line feed (useful with printer)
 bra.s mainloop
ml1:
 bsr tolower
 lea comtable(pc),A0
 moveq #0,D1         ;find command...
findcom:
 cmp.b (A0),D0
 beq.s foundcom
 addq.l #1,D1
 cmp.b #$FF,(A0)+    ;end of command table ??
 bne.s findcom
error:
 lea errtx(pc),A0    ;command not found, print error message
 bsr StrOutWindow
 emit LF
 bra.s mainloop
foundcom:            ;command found, execute it
 add.l D1,D1         ;D1 = D1 * 2
 lea comadrs(pc),A0
 add.l D1,A0
 add.w (A0),A0       ;get command address
 jmp (A0)            ;jump to command

*** THE HELP COMMAND ***
help:
 lea helptext(pc),A0
 bsr StrOutRedirect
 bra.s mainloop

*** INFO ***
info:
 lea infotext(pc),A0
 bsr StrOutRedirect
 bra mainloop

*** EXIT FROM MONITOR ***
exit:
 move.l thistask(A5),A0
 move.l OldTrapCode(A5),TC_TRAPCODE(A0) ;restore old TrapCode
 bsr FreeAllMem           ;free all memory allocated by commands & and (
 bsr remove_all_breaks    ;remove all breakpoints (free memory)
 move.l segment(A5),D1    ;if a segment is loaded, unload it
 beq.s exit7
 callsys UnLoadSeg
exit7:
 move.l OutputFile(A5),D1 ;if output is redirected, close output file
 cmp.l winfile(A5),D1
 beq.s exit7a
 callsys Close
exit7a:
 move.l winfile(A5),D1
 callsys Close  ;close window file
exit8:
 move.l A6,A1
 move.l _AbsExecBase,A6
 callsys CloseLibrary    ;close dos library
exit9:
 move.l WBenchMsg(A5),D3 ;started from workbench??
 beq.s exit99
 callsys Forbid   ;forbid, so WB can't UnloadSeg before exit
 move.l D3,A1
 callsys ReplyMsg ;reply the WB startup message
exit99:
 unlk A5          ;clean up stack
 moveq #0,D0      ;error return code
 rts              ;return control to the system...

*** DIRECTORY ***
dir:
 addq.l #2,A3
 moveq #0,D7  ;clear lock storage so the cleanup knows if it should UnLock
 move.l #fib_SIZEOF,D0 ;allocate FileInfoBlock
*** we use the same memory space for the InfoData structure
 move.l #MEMF_PUBLIC!MEMF_CLEAR,D1
 linkexec AllocMem
 tst.l D0
 beq OutOfMem
 move.l D0,A4 ;we keep the fib-pointer in A4
 bsr SkipSpc
 move.l A3,D1 ;use rest of command line as a name
 moveq #SHARED_LOCK,D2
 callsys Lock
 move.l D0,D7 ;we keep the lock pointer in D7
 beq.s dir7 ;if i/o error
 move.l D7,D1
 move.l A4,D2
 callsys Examine
 tst.l D0
 beq.s dir7 ;branch if Examine failed
 tst.l fib_DirEntryType(A4)
 bpl.s dir1
 bsr DisplayDirLine ;if Examine found a file, display information about it
 bra.s dir7b        ;then number of free blocks
dir1:          ;Examine found a directory, ExNext it
 move.l D7,D1
 move.l A4,D2
 callsys ExNext
 tst.l D0
 bne.s dir2
 callsys IoErr
 cmp.l #ERROR_NO_MORE_ENTRIES,D0
 bne.s dir7a ;branch if a real dos error
 bra.s dir7b ;no more entries, the display free blocks
dir2:
 bsr.s DisplayDirLine
 bne.s dir7b ;branch if user pressed break (control-c)
 bra.s dir1
dir7:
 callsys IoErr
dir7a:
 tst.l D0
 beq.s dir7c
 bsr DOSErr
 bra.s dir7c
dir7b: ;directory ready, now display number of free blocks
 move.l D7,D1
 move.l A4,D2
 callsys Info ;get info about the disk
 tst.l D0
 beq.s dir7
 move.l id_NumBlocks(A4),D0
 sub.l id_NumBlocksUsed(A4),D0
 startline ;print number of free blocks on disk
 bsr PutNum
 lea freetxt(pc),A1
 bsr PutStr
 clr.b (A3)
 bsr StrOutRedirect
dir7c:
 move.l D7,D1
 beq.s dir8 ;not really necessary, because it is safe ot pass NULL to UnLock
 callsys UnLock
dir8:
 move.l A4,A1
 move.l #fib_SIZEOF,D0
 linkexec FreeMem ;free FileInfoBlock
dir9:
 bra mainloop
OutOfMem: ;print error message 'out of memory'
 lea memerr(pc),A0
 bsr StrOutWindow
 bra.s dir9
*** PRINT ONE LINE OF DIREECTORY ***
DisplayDirLine: ;fib-pointer in A4
 startline
 lea fib_FileName(A4),A1
 moveq #0,D1
txmovloop:       ;copy file name to output buffer
 move.b (A1)+,D0
 beq.s txmov2
 move.b D0,(A3)+
 addq.w #1,D1
 cmp.w #30,D1    ;max 30 chars
 bcs.s txmovloop
txfloop:
 move.b #SPACE,(A3)+
 addq.w #1,D1
txmov2:
 cmp.w #24,D1
 bcs.s txfloop
 move.b #SPACE,(A3)+
 tst.l fib_DirEntryType(A4) ;positive=dir, negative=file
 bmi.s txputlen
 moveq #4,D0
 lea dnam(pc),A1 ;'(dir)'
 bsr PutStr
 bra.s txmov9
txputlen:
 move.l fib_Size(A4),D0
 bsr PutNum
txmov9:
 endline
 bsr StrOutRedirect
 bra CheckKeys

*** PRINT DOS ERROR NUMBER ***
DOSErr: ;error number in D0
 move.l D0,-(sp)
 startline
 lea doserrtx(pc),A1
 bsr PutStr ; 'DOS error '
 move.l (sp)+,D0
 bsr PutNum
 endline
 bra StrOutWindow

**** LOAD SEGMENT ****
loadseg:
 tst.l segment(A5)
 bne.s oldseg ;can't do this before old segment is unloaded
 bsr SkipSpc
 move.l A3,D1
 callsys LoadSeg
 move.l D0,segment(A5)
 beq.s segerr ;branch if LoadSeg failed
 startline
 lea segadrmes(pc),A1
 bsr PutStr
 move.l segment(A5),D0
 lsl.l #2,D0
 addq.l #4,D0
 move.l D0,regPC
 move.l D0,Addr(A5)
 bsr PutHexLong
 endline
 bsr StrOutRedirect
 bra.s mjump
oldseg: ;message 'unload old segment first'
 lea ulserr(pc),A0
 bsr StrOutWindow
 bra.s mjump
segerr: ;come here if LoadSeg failed
 callsys IoErr
 bsr.s DOSErr
mjump:
 bra mainloop
nosegerr: ;error 'no segment loaded'
 lea nosegmes(pc),A0
 bsr StrOutWindow
 bra.s mjump

**** UNLOAD SEGMENT ****
unloadseg:
 move.l segment(A5),D1
 beq.s nosegerr    ;branch if no segment
 callsys UnLoadSeg
 clr.l segment(A5) ;remember to clear the segment pointer
 bra.s mjump

**** SEGMENT LIST ****
seglist:
 move.l segment(A5),D3
 beq.s nosegerr    ;branch if no segment
 lea seghead(pc),A0
 bsr StrOutRedirect
 lea loctext(pc),A0
 bsr StrOutRedirect
segloop:
 startline
 lsl.l #2,D3       ;BCPL pointer conversion
 move.l D3,A1
 move.l A1,D0
 addq.l #4,D0
 move.l D0,-(sp)
 bsr PutHexLong1   ;segment start address
 moveq #SPACE,D0
 move.b D0,(A3)+
 move.b D0,(A3)+
 move.l -4(A1),D0
 subq.l #8,D0
 subq.l #1,D0
 add.l (sp)+,D0
 bsr PutHexLong1   ;segment end address
 moveq #SPACE,D0
 move.b D0,(A3)+
 move.b D0,(A3)+
 move.l -4(A1),D0
 subq.l #8,D0
 bsr PutNum        ;segment length
 endline
 move.l (A1),D3    ;get next segment pointer
 bsr StrOutRedirect
 bsr CheckKeys
 bne.s mjump2
 tst.l D3
 bne.s segloop
mjump2:
 bra mainloop

*** SHOW SEGMENT LIST & ALLOCATED MEMORY ***
show:
 bsr SkipSpc
 move.b (A3),D0
 bsr tolower
 cmp.b #'l',D0
 beq.s seglist
 cmp.b #'m',D0
 bne error
*** MEMORY LIST ***
showmemlist:
 move.l memlist(A5),D5
 bne.s showm2
not_at_all_allocated:
 lea no_alloc(pc),A0
 bsr StrOutWindow        ;error 'no memory allocated'
 bra mainloop
showm2:
 lea memlisttx(pc),A0
 bsr StrOutRedirect
 lea loctext(pc),A0
 bsr StrOutRedirect
 move.l D5,D0
showmemloop:
 startline
 move.l D0,A4
 addq.l #8,D0
 bsr PutHexLong1   ;memory block start address
 moveq #SPACE,D0
 move.b D0,(A3)+
 move.b D0,(A3)+
 move.l 4(A4),D0   ;get length
 add.l A4,D0
 addq.l #7,D0
 bsr PutHexLong1   ;end address 
 moveq #SPACE,D0
 move.b D0,(A3)+
 move.b D0,(A3)+
 move.l 4(A4),D0
 bsr PutNum        ;length
 endline
 bsr StrOutRedirect
 bsr CheckKeys
 bne.s showmem_end
 move.l (A4),D0
 bne.s showmemloop
showmem_end:
 bra mainloop

*** FREE ALL MEMORY ALLOCATED WITH THE ( AND & COMMANDS ***
FreeAllMem:
 move.l memlist(A5),D5
 beq.s freeall9
 move.l A6,-(sp)
 move.l _AbsExecBase,A6
freeall_loop:
 move.l D5,A1
 move.l 4(A1),D0
 addq.l #8,D0
 move.l (A1),D5
 callsys FreeMem
 tst.l D5
 bne.s freeall_loop
 move.l (sp)+,A6
 clr.l memlist(A5)
freeall9:
 rts

*** ALLOCATE MEMORY ***
allocate_mem:
 bsr GetNum
 tst.l D0
 beq error
 move.l D0,D5
 bsr GetNum
 moveq #0,D1
 cmp.l #'CHIP',D0 ;check if chip memory required
 bne.s alloc_1
 moveq #MEMF_CHIP,D1
alloc_1:
 move.l D5,D0
 addq.l #8,D0
 or.l #MEMF_CLEAR!MEMF_PUBLIC,D1
 linkexec AllocMem
 tst.l D0
 beq.s allocfailed
 move.l D0,A0
alloc_mem_1: ;add allocated memory to linked list of memory blocks
** A0 points to memory block, D5 is length
 move.l D5,4(A0)
 moveq #0,D0
 move.l memlist(A5),D1
alloc_find1:
 beq.s alloc_find2  ;keep the linked list in order, lowest address first
 move.l D1,A1
 cmp.l A1,A0
 bcs.s alloc_find2
 move.l D1,D0
 move.l (A1),D1
 bra.s alloc_find1
alloc_find2:
 tst.l D0
 bne.s alloc_do2
 move.l memlist(A5),(A0) ;add new memory node to start of list
 move.l A0,memlist(A5)
 bra.s alloc_display
alloc_do2:
 move.l D1,(A0)  ;add new memory node to middle or end of list
 move.l D0,A1
 move.l A0,(A1)
alloc_display:
 move.l A0,A4
 startline ;write message: 'allocated from xxx to xxx'
 lea allocmes(pc),A1
 bsr PutStr
 move.l A4,D0
 addq.l #8,D0
 bsr PutHexLong
 lea totxt(pc),A1
 bsr PutStr
 move.l A4,D0
 add.l 4(A4),D0
 addq.l #7,D0
 bsr PutHexLong
 endline
 bsr StrOutRedirect
mzump:
 bra mainloop
allocfailed: ;error 'allocation failed'
 lea allocfail(pc),A0
 bsr StrOutWindow
 bra.s mzump

*** ALLOCATE ABSOLUTE ***
alloc_abs:
 bsr GetNum
 subq.l #8,D0 ;remember to subtract 8 from the starting address
 move.l D0,D7 ;(next block pointer & length)
 bsr GetNum
 move.l D0,D5
 beq error
 addq.l #8,D0 ;add 8 to length
 move.l D7,A1
 linkexec AllocAbs
 tst.l D0
 beq.s allocfailed
 move.l D7,A0
 bra alloc_mem_1

*** FREE MEMORY ***
free_mem:
 bsr SkipSpc
 cmp.b #'a',(A3) ;check 'all'
 bne.s free_norm
 moveq #'l',D0
 cmp.b 1(A3),D0
 bne.s free_norm
 cmp.b 2(A3),D0
 bne.s free_norm
 bsr FreeAllMem
 bra mainloop
free_norm:
 bsr GetNum
 subq.l #8,D0
 move.l memlist(A5),D1
 beq not_at_all_allocated
 moveq #0,D2
find_mem_to_free_loop:
 move.l D1,A1
 cmp.l D0,D1    ;is this the block we want to free
 beq.s found_do_free_mem
 move.l D1,D2
 move.l (A1),D1 ;get next block pointer
 bne.s find_mem_to_free_loop
 lea noalloctx(pc),A0 ;error 'not allocated'
 bsr StrOutWindow
 bra mainloop
found_do_free_mem: ;remove memory block from linked list
 tst.l D2
 bne.s notfirst
 move.l (A1),memlist(A5)
 bra.s do_free
notfirst:
 move.l D2,A0
 move.l (A1),(A0)
do_free:
 move.l 4(A1),D0
 addq.l #8,D0
 linkexec FreeMem
 bra mainloop

*** SAVE ABSOLUTE (using DOS Write)****
abs_save:
 bsr GetNum
 move.l D0,A4
 bsr GetNum
 move.l D0,D6
 bsr SkipSpc
 move.l A3,D1
 move.l #MODE_NEWFILE,D2
 callsys Open
 move.l D0,D7
 bne.s abs_save_1
dos_err:
 callsys IoErr
 bsr DOSErr
 bra mainloop
abs_save_1:
 move.l D7,D1
 move.l A4,D2
 move.l D6,D3
 callsys Write
 move.l D7,D1
 callsys Close
 bra mainloop

*** LOAD ABSOLUTE (using DOS Read) ***
abs_load:
 bsr GetNum
 move.l D0,A4
 bsr SkipSpc
 move.l A3,D1
 move.l #MODE_OLDFILE,D2
 callsys Open   ;open file
 move.l D0,D7
 beq.s dos_err
 move.l D7,D1
 move.l A4,D2
 move.l #$7FFFFFFF,D3 ;MaxInt (the file can't be longer than this)
 callsys Read         ;read from file, until EOF, return actual length
 tst.l D0
 ble.s abs_load_1
 move.l A4,D5
 move.l D0,D6
 bsr showrange
abs_load_1:
 move.l D7,D1
 callsys Close ;close the file
 bra mainloop

*** REDIRECT OUTPUT ***
redirect:
 move.l OutputFile(A5),D1 ;is output currently redirected
 cmp.l winfile(A5),D1
 beq.s redir1
 callsys Close            ;if so, then close redirection file
 move.l winfile(A5),OutputFile(A5) ;standard output
redir1:
 bsr SkipSpc
 tst.b (A3)
 beq.s redir9
 move.l A3,D1
 move.l #MODE_NEWFILE,D2
 callsys Open ;open redirection file
 tst.l D0
 beq dos_err
 move.l D0,OutputFile(A5)
redir9:
 bra mainloop

*** NEW CLI ***
new_cli:        ; Execute("NewCLI CON:....",0,winfile)
 lea NewCliCom(pc),A0
 move.l A0,D1
 moveq #0,D2
 move.l winfile(A5),D3
 callsys Execute
 bra.s redir9 

showrange: ;start addr in D5, length in D6
 startline
 move.l D6,D0
 bsr PutNum
 lea readtxt(pc),A1
 bsr PutStr
 move.l D5,D0
 bsr PutHexLong
 lea totxt(pc),A1
 bsr PutStr
 move.l D5,D0
 add.l D6,D0
 subq.l #1,D0
 bsr PutHexLong
 endline
 bra StrOutRedirect

*** READ & WRITE DISK ***
disk_read:
 moveq #0,D7 ;D7 is read/write flags
 bra.s disk_rw
disk_write:
 moveq #-1,D7
disk_rw:
 bsr GetNum
 btst #0,D0
 bne error ;error: odd address
 move.l D0,Addr(A5)
 beq error
 bsr GetNum
 move.l D0,D3 ;Unit number (drive)
 bsr GetNum
 move.l D0,D4 ;starting sector
 bsr GetNum
 move.l D0,D5 ;length
 beq error    ;error: zero length
 bsr MyCreatePort
 move.l D0,D6
 beq disk_io_9 ;branch if CreatePort failed
 move.l D0,A1
 moveq #IOSTD_SIZE,D0
 bsr MyCreateIO
 tst.l D0
 beq disk_io_8 ;branch if CreateIO failed
 move.l D0,A2
 move.l D3,D0
 lea tdname(pc),A0
 move.l A2,A1
 moveq #0,D1
 linkexec OpenDevice
 tst.l D0
 bne disk_io_7 ;branch if OpenDevice failed
 move.l Addr(A5),IO_DATA(A2)
 moveq #9,D0
 lsl.l D0,D4    ;multiply by 512
 move.l D4,IO_OFFSET(A2)
 lsl.l D0,D5    ;multiply by 512
 move.l D5,IO_LENGTH(A2)
 tst.l D7
 bne.s disk_wr
 move.w #CMD_READ,IO_COMMAND(A2)    ;read from disk
 move.l A2,A1
 linkexec DoIO 
 tst.l D0
 bne.s disk_io_err
 move.l D6,-(sp)
 move.l IO_DATA(A2),D5
 move.l IO_ACTUAL(A2),D6
 bsr showrange
 move.l (sp)+,D6
 bra.s disk_io_5
disk_wr:
 move.w #CMD_WRITE,IO_COMMAND(A2)   ;write to disk
 move.l A2,A1
 linkexec DoIO
 tst.l D0
 bne.s disk_io_err
 move.w #CMD_UPDATE,IO_COMMAND(A2)  ;make sure that the buffer is written
 move.l A2,A1
 linkexec DoIO
 tst.l D0
 beq.s disk_io_5
disk_io_err:    ;Print TrackDisk error number
 startline
 lea diskerr(pc),A1
 bsr PutStr
 move.b IO_ERROR(A2),D0
 ext.w D0
 ext.l D0
 bsr PutNumSign ;error number is signed
 endline
 bsr StrOutWindow
disk_io_5:      ;stop drive motor
 move.w #TD_MOTOR,IO_COMMAND(A2)
 clr.l IO_LENGTH(A2)
 move.l A2,A1
 linkexec DoIO
disk_io_6:
 move.l A2,A1
 linkexec CloseDevice
disk_io_7:
 move.l A2,A1
 bsr MyDeleteIO
disk_io_8:
 move.l D6,A1
 bsr MyDeletePort
disk_io_9:
 bra mainloop

*** PLAY DIGITIZED SOUND ***
digisound:
 bsr GetNum
 btst #0,D0     ;test: if the address if odd, then error
 beq.s digi1
digi_err:
 bra error
digi1:
 move.l D0,D5
 bsr GetNum
 tst.l D0
 beq.s digi_err ;error: zero length
 btst #0,D0
 bne.s digi_err
 move.l D0,D6
 bsr GetNum     ;period (speed)
 move.w D0,D7
 bsr MyCreatePort
 move.l D0,EndAddr(A5) ;use this for message port addr
 beq digi9
 move.l D0,A1
 moveq #ioa_SIZEOF,D0
 bsr MyCreateIO
 tst.l D0
 beq digi8
 move.l D0,A2
 move.b #127,LN_PRI(A2) ;maximum priority (so nobody can steal the channel)
 lea allocmap(pc),A0    ;channel allocation map (any channel)
 move.l A0,ioa_Data(A2)
 moveq #4,D0            ;size of the allocation map
 move.l D0,ioa_Length(A2)
 move.l A2,A1
 lea audioname(pc),A0
 moveq #0,D0
 moveq #0,D1
 linkexec OpenDevice    ;open audio.device
 tst.l D0
 bne.s digi7
 move.l D5,ioa_Data(A2)
 move.l D6,ioa_Length(A2)
 move.w D7,ioa_Period(A2)
 clr.w ioa_Cycles(A2)              ;repeat mode (zero cycles)
 move.w #64,ioa_Volume(A2)         ;maximum volume
 move.b #ADIOF_PERVOL,IO_FLAGS(A2) ;flag to set the volume & period
 move.w #CMD_WRITE,IO_COMMAND(A2)  ;audio output=CMD_WRITE
 move.l A2,A1
 BEGINIO ;can't use SendIO, because it clears the ADIO_PERVOL flag
 lea audiotxt(pc),A0
 bsr StrOutWindow ;message: 'press Ctrl-C...'
 moveq #0,D0
 move.l #SIGBREAKF_CTRL_C,D1
 linkexec SetSignal ;clear CTRL-C signal
 move.l #SIGBREAKF_CTRL_C,D0
 linkexec Wait      ;wait until the user presses Ctrl-C
 move.l A2,A1
 linkexec CloseDevice
digi7:
 move.l A2,A1
 bsr MyDeleteIO
digi8:
 move.l EndAddr(A5),A1
 bsr MyDeletePort
digi9:
 bra mainloop

*** DISK BLOCK CHECKSUM ***
block_check:
 bsr GetNum
 tst.l D0   ;just checking...
 beq error
 move.l D0,A0
 move.l D0,A1
 moveq #0,D0
 moveq #512/4-1,D1 ;disk block size 512 bytes
bl_check:
 add.l (A0)+,D0
 dbf D1,bl_check
 add.w #5*4,A1     ;checksum located at longword #5 in block
 move.l (A1),D6
 sub.l D0,(A1)
 move.l (A1),D7
 bra.s ShowSum

*** BOOTBLOCK CHECKSUM ***
boot_check:
 bsr GetNum
 tst.l D0   ;just checking...
 beq error
 move.l D0,A0
 move.l D0,A1
 moveq #0,D0
 move.w #1024/4-1,D1 ;bootblock size 1024 bytes
boot_ch:
 add.l (A0)+,D0
 bcc.s boot_1
 addq.l #1,D0        ;remember to add the carry
boot_1:
 dbf D1,boot_ch
 addq.l #4,A1        ;checksum in second longword
 move.l (A1),D6
 sub.l D0,(A1)
 bcc.s boot_2
 subq.l #1,(A1)
boot_2:
 move.l (A1),D7

*** SHOW OLD AND NEW CHECKSUM (jump from the previous two commands)
ShowSum: ;old sum in D6, new sum in D7
 startline
 lea oldTx(pc),A1
 bsr PutStr
 move.l D6,D0
 bsr PutHexLong
 lea newTx(pc),A1
 bsr PutStr
 move.l D7,D0
 bsr PutHexLong
 endline
 bsr StrOutRedirect
 bra mainloop

*** DISPLAY NUMBER IN HEX, DECIMAL, OCTAL AND BINARY ***
number:
 bsr GetNum
 move.l D0,D5
 lea PutHexLong1(pc),A4 ;output routine address
 move.l #' hex',D2
 bsr.s numzump
 lea putdec1(pc),A4
 move.l #' dec',D2
 bsr.s numzump
 lea octnum(pc),A4
 move.l #' oct',D2
 bsr.s numzump
 lea binnum(pc),A4
 move.l #' bin',D2
 bsr.s numzump
 bra mainloop

numzump: ;print number in one base (both signed & unsigned if negative)
 startline
 tst.l D5
 bpl.s num_A1
 lea signtxt(pc),A1
 bsr PutStr
num_A1:
 move.l D2,D0
 bsr PutLong
 move.b #':',(A3)+
 move.b #SPACE,(A3)+
 cmp.w #'ec',D2
 beq.s num_z1
 move.b #SPACE,(A3)+
num_z1:
 move.l D5,D0
 jsr (A4)
 move.b #LF,(A3)+
 tst.l D5
 bpl.s num_A2
 moveq #SPACE,D0
 move.b D0,(A3)+
 move.b D0,(A3)+
 lea signtxt+2(pc),A1
 bsr PutStr
 move.l D2,D0
 bsr PutLong
 move.b #':',(A3)+
 move.b #SPACE,(A3)+
 move.l D5,D0
 neg.l D0
 move.b #'-',(A3)+
 jsr (A4)
 move.b #LF,(A3)+
num_A2:
 clr.b (A3)
 bra StrOutRedirect

*** OCTAL NUMBER OUTPUT ***
octnum:
 move.l D2,-(sp)
 move.b #'0',(A3)+
 moveq #-1,D2
 tst.l D0
 beq.s oct3
oct1:
 move.b D0,D1
 lsr.l #3,D0 
 and.b #7,D1
 add.b #'0',D1
 move.b D1,-(sp)
 addq.w #1,D2
 tst.l D0
 bne.s oct1
oct2:
 move.b (sp)+,(A3)+
 dbf D2,oct2
oct3:
 move.l (sp)+,D2
 rts

*** BINARY NUMBER OUTPUT ***
binnum:
 move.l D2,-(sp)
 move.b #'%',(A3)+
 moveq #31,D2
bin1:
 lsl.l #1,D0
 move.b #'0'/2,D1
 roxl.b #1,D1
 move.b D1,(A3)+
 dbf D2,bin1
 move.l (sp)+,D2
 rts 

*** DECIMAL OUTPUT WITH '+' IF NECESSARY ***
putdec1:
 cmp.b #'-',-1(A3)
 beq.s putdec2
 move.b #'+',(A3)+
putdec2:
 bra PutNum

*** MODIFY MEMORY ***
modifymem:
 bsr GetNum
 move.l D0,A0
 bsr.s getstring
 bra mainloop

*** GET STRING FROM INPUT LINE TO ADDR IN A0, LENGTH IN D2 ***
* NOTE: this version requires commas between numbers and strings
getstring: ;note: this routine changes the A1-register
 move.l A0,A1
gstr1:
 bsr SkipSpc
 move.b (A3)+,D0
 beq.s gstr9    ;branch if end of input line
 cmp.b #'''',D0 ;if single quote, then string follows
 beq.s stringi
 subq.l #1,A3
 bsr GetNum     ;else get number
 move.b D0,(A1)+
gstr1a:
 bsr SkipComma
 bmi.s gstr9    ;if comma not found, then end
 bra.s gstr1
stringi:
 move.b (A3)+,D0
 beq.s gstr9
 cmp.b #'''',D0
 beq.s gstr2
string1:
 move.b D0,(A1)+
 bra.s stringi
gstr2:
 cmp.b #'''',(A3)+ ;test for double-single-quote
 beq.s string1
 subq.l #1,A3
 bra.s gstr1a
gstr9:
 move.l A1,D2
 sub.l A0,D2  ;calculate length of the string in D2
 rts

*** DELETE A FILE ***
Deletefile:
 addq.l #2,A3
 bsr SkipSpc
 move.l A3,D1
 callsys DeleteFile
 tst.l D0
 beq dos_err
 bra mainloop

*** CHANGE CURRENT DIRECTORY (CD) ***
CurrentDirectory:
 addq.l #1,A3
 bsr SkipSpc
 tst.b (A3)
 bne.s cd_1
 moveq #0,D1 ;if no name given set lock to zero (initial boot device root dir)
 bra.s cd_2
cd_1:
 move.l A3,D1
 moveq #SHARED_LOCK,D2
 callsys Lock
 tst.l D0
 beq dos_err
 move.l D0,D1
cd_2:
 callsys CurrentDir
 move.l D0,D1
 callsys UnLock ;unlock previous current directory
 bra mainloop

memcomp:
*** compare memory, set current directory or clear screen ***
 cmp.b #'d',(A3)
 beq.s CurrentDirectory
 cmp.b #'l',(A3)
 bne.s no_cls
 cmp.b #'s',1(A3)
 bne.s no_cls
*** THE CLS COMMAND ***
 emit CLS ;Clear the Screen
 bra mainloop
no_cls:
**** COMPARE MEMORY ****
 moveq #8,D7
 bsr GetNum
 move.l D0,A0
 bsr GetNum
 move.l D0,A1
 bsr GetNum
 move.l D0,A2
comp1:
 cmp.l A1,A0
 bhi.s comp99
 cmpm.b (A0)+,(A2)+
 beq.s comp1
 movem.l A0-A1,-(sp)
 move.l A0,D0
 subq.l #1,D0
 startline
 bsr PutHexLong
 move.w #$2000,(A3) ;space+null
 bsr StrOutRedirect
 subq.l #1,D7
 bne.s compf1
 emit LF
 moveq #8,D7
compf1:
 bsr CheckKeys
 bne.s compbreak
 movem.l (sp)+,A0-A1
 bra.s comp1
compbreak:
 addq.l #8,sp
comp99:
 emit LF
 bra mainloop

**** TRANSFER MEMORY ****
memtransfer:
 bsr GetNum
 move.l D0,A0
 bsr GetNum
 move.l D0,A1
 bsr GetNum
 move.l D0,A2
 cmp.l A2,A0
 bcs.s backwards  ;if destination > source, transfer backwards
trf1:
 cmp.l A1,A0
 bhi.s huntfilltrf2
 move.b (A0)+,(A2)+
 bra.s trf1
backwards:
 add.l A1,A2
 sub.l A0,A2
trf2:
 cmp.l A0,A1
 bcs.s huntfilltrf2
 move.b (A1),(A2)
 subq.l #1,A1
 subq.l #1,A2
 bra.s trf2

**** FILL MEMORY ****
* This version can fill memory with a pattern
memfill:
 bsr GetNum
 move.l D0,-(sp)
 bsr GetNum
 move.l D0,A2
 move.l outbufadr(A5),A0
 lea (InpBuf-OutBuf)(A0),A0
 bsr getstring
 move.l (sp)+,A1
 tst.l D2
 beq.s huntfilltrf2
fill0:
 moveq #0,D0
fill1:
 cmp.l A2,A1
 bhi.s huntfilltrf2
 move.b 0(A0,D0.L),(A1)+
 addq.l #1,D0
 cmp.l D2,D0
 bcs.s fill1
 bra.s fill0

huntfilltrf2:
 bra mainloop

**** HUNT MEMORY ****
memhunt:
 moveq #8,D7 ;how many numbers/line
 bsr GetNum
 move.l D0,-(sp)
 bsr GetNum
 move.l D0,A2
 move.l outbufadr(A5),A0
 lea (InpBuf-OutBuf)(A0),A0
 bsr getstring
 move.l (sp)+,A1
 tst.l D2   ;string length
 beq.s huntfilltrf2
hunt0:
 move.b (A0),D0
hunt1:
 cmp.l A2,A1
 bhi.s hunt99
 cmp.b (A1)+,D0
 bne.s hunt1
 moveq #0,D1
hunt2:
 addq.l #1,D1
 cmp.l D2,D1
 bcc.s huntfound
 move.b -1(A1,D1.L),D0
 cmp.b 0(A0,D1.L),D0
 beq.s hunt2
 bra.s hunt0
huntfound:
 movem.l A0-A1,-(sp)
 move.l A1,D0
 subq.l #1,D0
 startline      ;found, print address
 bsr PutHexLong
 move.w #$2000,(A3)
 bsr StrOutRedirect
 subq.l #1,D7
 bne.s huntf1
 emit LF
 moveq #8,D7
huntf1:
 bsr.s CheckKeys
 bne.s huntbreak
 movem.l (sp)+,A0-A1
 bra.s hunt0
huntbreak:
 addq.l #8,sp
hunt99:
 emit LF
 bra mainloop

**** WAIT IF SPACE PRESSED, BREAK IF CTRL-C (status non-zero) ****
CheckKeys:
 move.l winfile(A5),D1
 move.l D2,-(sp)
 moveq #0,D2 ;timeout=0
 callsys WaitForChar
 move.l (sp)+,D2
 tst.l D0
 beq.s nobreak ;branch if no key pressed
 bsr GetKey
 cmp.w #SPACE,D0
 bne.s nospc
waitspc:       ;space pressed, wait for another space or Ctrl-C
 bsr GetKey
 cmp.w #CtrlC,D0
 beq.s break
 cmp.w #SPACE,D0
 bne.s waitspc
 bra.s nobreak
nospc:
 cmp.w #CtrlC,D0
 bne.s nobreak
break:
 lea breaktx(pc),A0 ;message '*** break ***'
 bsr StrOutWindow
 moveq #-1,D0
 rts
nobreak:
 moveq #0,D0
 rts

**** PARAMETERS FOR DISPLAY AND DISASSEMBLE ****
* get Addr and EndAddr & number of lines to display in D7
* if D7 is zero then display from Addr to EndAddr
* if D7 is non-zero EndAddr is ignored
getparams:
 bsr SkipSpc
 tst.b (A3)
 beq.s param9
 bsr GetNum
 bclr #0,D0
 move.l D0,Addr(A5)
 bsr SkipSpc
 tst.b (A3)
 beq.s param9
 bsr GetNum
 move.l D0,EndAddr(A5)
 moveq #0,D7
 rts
param9:
 moveq #20,D7  ;number of lines to output, if zero then use EndAddr
 rts

**** CHECK IF WE SHOULD STOP MEMDISPLAY OR DISASSEMBLE ****
* if D7 is non-zero decrement it, if it becomes zero then stop
* else if Addr>EndAddr then stop
* return Z-flag set if we should stop
CheckEnd: ;returns Z=1 if stop
 bsr CheckKeys
 bne.s stop1
 tst.l D7
 beq.s cmpadrs
 subq.l #1,D7
 rts
cmpadrs:
 cmp.l EndAddr(A5),A4
 bls.s cont
stop1:
 moveq #0,D0
 rts
cont:
 moveq #-1,D0
 rts

disassem:
*** Disassemble, delete file or directory **
 cmp.b #'e',(A3)
 bne.s nodel
 cmp.b #'l',1(A3)
 beq Deletefile
nodel: 
 cmp.b #'i',(A3)
 bne.s nodir
 cmp.b #'r',1(A3)
 beq dir
nodir:
**** DISASSEMBLE ****
 bsr.s getparams
 move.l Addr(A5),A4
disasmloop:
 bsr.s disasmline
 bsr StrOutRedirect
 bsr CheckEnd
 bne.s disasmloop
 move.l A4,Addr(A5)
 bra mainloop

*** DISASSEMBLE ONE INSTRUCTION ***
* address in A4
disasmline:
 startline
 move.l A4,D0
 bsr PutHexLong ;output instruction address
 move.b #SPACE,(A3)+
 move.l A3,A2
 move.l A0,A3
 add.w #36,A3
 move.w (A4),opcode(A5) ;get opcode
 bsr DGetWord
 move.l A4,instrad(A5)
 move.w D2,D0
 moveq #12,D1
 lsr.w D1,D0  ;jump according to the first four bits (first hex digit)
 add.w D0,D0
 lea disadrtabl(pc),A1
 add.w D0,A1
 add.w (A1),A1
 jmp (A1)
*** $4xxx ***
handlefour:
 cmp.w #ILLEGAL,D2 ;illegal instruction $4AFC
 bne.s no_illegal
 moveq #43,D0
 bsr putopcode
 bra dis9
no_illegal:
 btst #8,D2
 beq.s no_lea_or_chk
 btst #7,D2
 beq.s derr_4
 btst #6,D2
 bne.s lea_instr
 and.w #$38,D2
 cmp.w #$08,D2 ;An addr mode illegal
 beq.s derr_4
 moveq #20,D0  ;chk instruction
 bsr putopcode
 bsr padd
 bsr NormEA
 move.b #',',(A3)+
 bsr putdreg
 bra.s chk_lea_zap9
derr_4:
 bra diserr
lea_instr:
 bsr check_jump_adrmode
 bne.s derr_4
 moveq #21,D0
 bsr putopcode
 bsr padd
 bsr NormEA
 move.b #',',(A3)+
 bsr putAreg
chk_lea_zap9:
 bra dis9
no_lea_or_chk:
 move.w D2,D0
 and.w #$0E00,D0
 bne.s no_negx_or_move_sr
negate:
 bsr check_dest_adrmode
 bne.s derr_4
 move.w D2,D0
 and.w #$C0,D0
 cmp.w #$C0,D0
 beq.s move_from_sr
 moveq #24,D0
 bsr putopcode
 btst #10,D2
 bne.s noextneg
 move.b #'x',(A3)+
noextneg:
 bsr putsize
 bsr padd
 bsr NormEA
 bra.s mngxzap9
move_from_sr: ;MOVE SR,Ea
 moveq #4,D0
 bsr putopcode
 bsr padd
 move.b #'S',(A3)+
 move.b #'R',(A3)+
 move.b #',',(A3)+
 bsr NormEA
mngxzap9:
 bra dis9
no_negx_or_move_sr:
 cmp.w #$0200,D0
 bne.s noclr
 move.w D2,D0 ;clr instruction
 and.w #$C0,D0
 cmp.w #$C0,D0
 beq.s derr_zap
 bsr check_dest_adrmode
 bne.s derr_zap
 moveq #23,D0
 bsr putopcode
 bsr putsize
 bsr padd
 bsr NormEA
 bra.s mngxzap9
derr_zap:
 bra diserr
noclr:
 cmp.w #$0400,D0
 bne.s no_neg_or_move_ccr
 move.w D2,D0
 and.w #$C0,D0
 cmp.w #$C0,D0
 bne negate
 and.w #$38,D2
 cmp.w #$08,D2 ;An addr mode is illegal
 beq.s derr_zap
 moveq #4,D0 ;move to CCR
 bsr putopcode
 bsr padd
 bsr NormEA
 move.l #',CCR',D0
 bsr PutLong
 bra.s nzapp99
no_neg_or_move_ccr:
 cmp.w #$0600,D0
 bne.s no_not_or_move_sr
 move.w D2,D0
 and.w #$C0,D0
 cmp.w #$C0,D0
 beq.s move_to_sr
 bsr check_dest_adrmode ;not-insturuction
 bne.s derr_zap
 moveq #25,D0
 bsr putopcode
 bsr putsize
 bsr padd
 bsr NormEA
nzapp99:
 bra dis9
move_to_sr:
 and.w #$38,D2
 cmp.w #$08,D2 ;An addr mode illegal
 beq.s derr_zump
 moveq #4,D0 ;move to SR
 bsr putopcode
 bsr padd
 bsr NormEA
 move.l #',SR ',D0
 bsr PutLong
 subq.l #1,A3
 bra.s nzapp99
derr_zump:
 bra diserr
no_not_or_move_sr:
 cmp.w #$0A00,D0
 bne.s no_tst_or_tas
 bsr check_dest_adrmode
 bne.s derr_zump
 move.w D2,D0
 and.w #$C0,D0
 cmp.w #$C0,D0
 beq.s tas_instr
 moveq #26,D0 ;tst-instruction
 bsr putopcode
 bsr putsize
 bsr padd
 bsr NormEA
 bra.s ttzap9
tas_instr:
 moveq #36,D0
 bsr putopcode
 bsr padd
 bsr NormEA
ttzap9:
 bra dis9
no_tst_or_tas:
 cmp.w #$0800,D0
 bne no_48
 btst #7,D2
 bne.s movem_or_ext
 btst #6,D2
 beq.s negate_bcd
 move.w D2,D0
 and.w #$38,D0
 beq.s swap_instr
 bsr check_jump_adrmode ;pea-instruction
 bne.s derr_zip
 moveq #29,D0
 bsr putopcode
 bsr padd
 bsr NormEA
 bra.s swapzap9
swap_instr:
 moveq #28,D0
 bsr putopcode
 bsr padd
 bsr putdreg0
swapzap9:
 bra dis9
derr_zip:
 bra diserr
negate_bcd:
 bsr check_dest_adrmode
 bne.s derr_zip
 moveq #27,D0
 bsr putopcode
 bsr padd
 bsr NormEA
 bra.s swapzap9
movem_or_ext:
 move.w D2,D0
 and.w #$38,D0
 bne.s movem_to_mem
 moveq #22,D0 ;ext
 bsr putopcode
 bsr put_wl_size
 bsr padd
 bsr putdreg0
 bra dis9
movem_to_mem:
 bsr check_dest_adrmode
 bne.s derr_zip
 and.w #$38,D2
 cmp.w #$18,D2
 beq.s derr_zip
 moveq #4,D0
 bsr putopcode
 move.b #'m',(A3)+
 bsr put_wl_size
 bsr padd
 bsr DGetWord
 bsr RegisterList
 move.b #',',(A3)+
 bsr NormEA
 bra.s dazap9
no_48:
 cmp.w #$0C00,D0
 bne.s special_4e
 and.w #$3F,D2 ;lots of testing for illegal addressing modes
 cmp.w #$3B,D2
 bhi.s derr_4e
 and.w #$38,D2
 cmp.w #$20,D2
 beq.s derr_4e
 cmp.w #$10,D2
 bcs.s derr_4e
 moveq #4,D0 ;movem from mem
 bsr putopcode
 move.b #'m',(A3)+
 bsr put_wl_size
 bsr padd
 bsr DGetWord
 move.w D2,D5
 bsr NormEA
 move.b #',',(A3)+
 move.w D5,D2
 bsr RegisterList
dazap9:
 bra dis9
derr_4e:
 bra diserr
*** $4Exx ***
special_4e:
 cmp.w #$4E40,D2
 bcs.s derr_4e
 cmp.w #$4e50,D2
 bcc.s notrapinst
 moveq #42,D0  ;TRAP #n
 bsr putopcode
 bsr padd
 move.b #'#',(A3)+
 move.w D2,D0
 and.b #$0F,D0
 bsr PutHexByte1
 bra.s dazap9
notrapinst:
 cmp.w #$4E58,D2
 bcc.s nolink
 moveq #30,D0
 bsr putopcode ;LINK An,#cc
 bsr padd
 bsr putAreg0
 move.b #',',(A3)+
 move.b #'#',(A3)+
 bsr DGetWord
 move.w D2,D0
 bsr PutHexWord1
 bra.s dazap9
nolink:
 cmp.w #$4E60,D2
 bcc.s nounlk
 moveq #31,D0 ;UNLK An
 bsr putopcode
 bsr padd
 bsr putAreg0
 bra.s dazap9
nounlk:
 cmp.w #$4E68,D2
 bcc.s no_move_to_usp
 moveq #4,D0 
 bsr putopcode ;MOVE An,USP
 bsr padd
 bsr putAreg0
 move.b #',',(A3)+
 lea USPnam(pc),A1
 bsr PutStr
uspzap9:
 bra dis9
no_move_to_usp:
 cmp.w #$4E70,D2
 bcc.s no_move_from_usp
 moveq #4,D0
 bsr putopcode ;MOVE USP,An
 bsr padd
 lea USPnam(pc),A1
 bsr PutStr
 move.b #',',(A3)+
 bsr putAreg0
 bra.s uspzap9
no_move_from_usp:
 cmp.w #$4E78,D2
 bcc.s jumps
 cmp.w #$4E74,D2
 beq.s derr_zero ;$4E74 is illegal
 move.w D2,D0
 and.w #7,D0
 add.w #32,D0
 bsr putopcode  ;RTS, NOP, RTE, STOP, RESET, RTR...
 cmp.w #$4E72,D2 ;STOP??
 bne.s uspzap9
 bsr padd
 bsr DGetWord  ;if it is, print '#num'
 move.w D2,D0
 move.b #'#',(A3)+
 bsr PutHexWord1
 bra.s jumpzap9
jumps:
 bsr check_jump_adrmode
 bne.s derr_zero
 move.w D2,D0
 lsr.w #6,D0
 and.w #1,D0
 add.w #40,D0
 bsr putopcode ; JMP & JSR
 bsr padd
 bsr NormEA
jumpzap9:
 bra dis9
derr_zero:
 bra diserr
*** $0xxx ***
handlezero:
 btst #8,D2
 bne movep_etc
 move.w D2,D0
 and.w #$0E00,D0
 beq.s or_imm
 cmp.w #$0200,D0
 beq.s and_imm
 cmp.w #$0400,D0
 beq.s sub_imm
 cmp.w #$0600,D0
 beq.s add_imm
 cmp.w #$0800,D0
 beq bits_imm
 cmp.w #$0A00,D0
 beq.s eor_imm
 cmp.w #$0C00,D0
 bne.s derr_zero
* cmpi *
 moveq #15,D0
 bra.s imm_com
add_imm:
 moveq #5,D0
 bra.s imm_com
sub_imm:
 moveq #6,D0
 bra.s imm_com
or_imm:
 moveq #8,D0
 bra.s logic_imm
and_imm:
 moveq #7,D0
 bra.s logic_imm
eor_imm:
 moveq #14,D0
logic_imm:
 move.w D2,D1
 and.w #$3F,D1
 cmp.w #$3C,D1
 bne.s imm_com
 bsr putopcode ;mode=SR or CCR
 move.b #'i',(A3)+
 bsr padd
 bsr Immediate
 move.b #',',(A3)+
 move.w opcode(A5),D0
 and.w #$C0,D0
 beq.s condreg
 cmp.w #$40,D0
 bne.s derr_imm
 moveq #'S',D0
 bra.s sr_zap9
condreg:
 moveq #'C',D0
 move.b D0,(A3)+
sr_zap9:
 move.b D0,(A3)+
 move.b #'R',(A3)+
 bra dis9
imm_com:
 bsr putopcode
 move.w D2,D0
 and.w #$C0,D0
 cmp.w #$C0,D0
 beq.s derr_imm
 move.w D2,D0
 and.w #$38,D0
 cmp.w #$08,D0
 beq.s derr_imm
 bsr check_dest_adrmode
 bne.s derr_imm
 move.b #'i',(A3)+
 bsr putsize
 bsr padd
 bsr Immediate
 move.b #',',(A3)+
 bsr NormEA
 bra dis9
derr_imm:
 bra diserr
*** Bxxx #num,EA ***
bits_imm:
 move.w D2,D0
 and.w #$3F,D0
 cmp.w #$3B,D0
 bhi.s derr_imm
 and.w #$38,D0
 cmp.w #$08,D0
 beq.s derr_imm
 move.w D2,D0
 lsr.w #6,D0
 and.w #3,D0
 beq.s bits_imm_ok2
 and.w #$3F,D2
 cmp.w #$39,D2
 bhi.s derr_imm
bits_imm_ok2:
 add.w #16,D0
 bsr putopcode
 bsr padd
 bsr DGetWord
 move.b #'#',(A3)+
 move.w D2,D0
 bsr PutHexWord1
 move.b #',',(A3)+
 bsr NormEA
 bra dis9
*** MOVEP, bit manipulations ***
movep_etc:
 move.w D2,D0
 and.w #$38,D0
 cmp.w #$08,D0
 beq.s move_p
*** Bxxx Dn,EA ***
 move.w D2,D0
 and.w #$3F,D0
 cmp.w #$3B,D0
 bhi.s derr_imm
 and.w #$38,D0
 cmp.w #$08,D0
 beq.s derr_imm
 move.w D2,D0
 lsr.w #6,D0
 and.w #3,D0
 beq.s bits_dreg_ok2
 and.w #$3F,D2
 cmp.w #$39,D2
 bhi diserr
bits_dreg_ok2:
 add.w #16,D0
 bsr putopcode
 bsr padd
 bsr putdreg
 move.b #',',(A3)+
 bsr NormEA
 bra dis9
*** MOVEP ***
move_p:
 moveq #4,D0
 bsr putopcode
 move.b #'p',(A3)+
 bsr put_wl_size
 bsr padd
 btst #7,D2
 beq.s noregsource
 bsr putdreg
 move.b #',',(A3)+
noregsource:
 bsr DGetWord
 move.w D2,D0
 bsr PutHexWord1
 move.b #'(',(A3)+
 move.w opcode(A5),D2
 bsr putAreg0
 move.b #')',(A3)+
 btst #7,D2
 bne.s noregdest
 move.b #',',(A3)+
 bsr putdreg
noregdest:
 bra dis9
*** CMP & CMPM & EOR ***
handle_11:
 btst #8,D2
 beq.s compare
 move.w D2,D0
 and.w #$C0,D0
 cmp.w #$C0,D0
 beq.s compare
 and.w #$38,D2
 cmp.w #$08,D2
 beq.s compmem
*** EOR ***
 and.w #$3F,D2
 cmp.w #$39,D2
 bhi diserr
 moveq #14,D0
 bsr putopcode
 bsr putsize
 bsr padd
 bsr putdreg
 move.b #',',(A3)+
 bsr NormEA
 bra dis9
*** CMPM ***
compmem:
 moveq #15,D0
 bsr putopcode
 move.b #'m',(A3)+
 bsr putsize
 bsr padd
 move.b #'(',(A3)+
 bsr putAreg0
 move.l #')+,(',D0
 bsr PutLong
 bsr putAreg
 move.b #')',(A3)+
 move.b #'+',(A3)+
 bra.s cmpzap9
*** CMP ***
compare:
 moveq #15,D0
 bsr putopcode
 move.w D2,D0
 and.w #$C0,D0
 cmp.w #$C0,D0
 bne.s cmp1
 move.b #'a',(A3)+ ;cmpa
cmp1:
 bsr putsize
 cmp.b #'b',-1(A3)
 bne.s cmp2
 and.w #$38,D2
 cmp.w #$08,D2
 beq diserr ;byte size not allowed if addr reg
cmp2:
 bsr padd
 bsr NormEA
 move.b #',',(A3)+
 move.w opcode(A5),D0
 and.w #$C0,D0
 cmp.w #$C0,D0
 bne.s datacmp
 bsr putAreg
 bra.s cmpzap9
datacmp:
 bsr putdreg
cmpzap9:
 bra dis9
*** AND & MUL & ABCD & EXG ***
handle_12: 
 move.w D2,D0
 and.w #$0130,D0
 cmp.w #$0100,D0
 bne.s noexg
 move.w D2,D0
 and.w #$C0,D0
 beq.s noexg
 cmp.w #$C0,D0
 beq.s noexg
 move.w D2,D0
 and.w #$F8,D0
 cmp.w #$80,D0
 beq.s noexg
 and.w #$30,D0
 bne.s noexg
*** EXG ***
 moveq #13,D0
 bsr putopcode
 bsr padd
 move.w D2,D1
 and.w #$F8,D1
 cmp.w #$48,D1
 beq.s both_adr
 moveq #'D',D0
 bra.s putfirstreg 
both_adr:
 moveq #'A',D0
putfirstreg:
 bsr reg0com
 move.b #',',(A3)+
 cmp.w #$40,D1
 beq.s bothdata
 bsr putAreg
 bra.s dzujump9
bothdata:
 bsr putdreg
dzujump9:
 bra dis9
noexg:
 move.w D2,D0
 and.w #$C0,D0
 cmp.w #$C0,D0
 beq.s mul1
 moveq #7,D0
 bra.s and_or
mul1:
 moveq #11,D0
 bra.s divmul
*** OR & DIV & SBCD ***
handle_eight: 
 move.w D2,D0
 and.w #$C0,D0
 cmp.w #$C0,D0 
 bne.s do_or
 moveq #12,D0 ;disivion
divmul:
 bsr putopcode
 btst #8,D2
 beq.s unsigned_divm
 moveq #'s',D0
 bra.s divmul1
unsigned_divm:
 moveq #'u',D0
divmul1:
 move.b D0,(A3)+
 bsr padd
 bsr NormEA
 move.b #',',(A3)+
 bsr putdreg
 bra dis9
do_or:
 moveq #8,D0
*** AND & OR **
and_or: ;opcode number in D0
 move.w D2,D1
 and.w #$01F0,D1
 cmp.w #$0100,D1
 beq.s bcd_oper
 move.w D2,D1
 and.w #$013F,D1
 cmp.w #$0100,D1
 beq.s derr_abc
 bsr putopcode
 bsr putsize
 bsr padd
 btst #8,D2
 bne.s regsource1
 move.w D2,D0 ;AND/OR EA,Dn
 and.w #$38,D0
 cmp.w #$08,D0
 beq.s derr_abc
 bsr NormEA
 move.b #',',(A3)+
 bsr putdreg
 bra.s ddzap9
regsource1:  ;AND/OR Dn,EA
 bsr check_dest_adrmode
 bne.s derr_abc
 bsr putdreg
 move.b #',',(A3)+
 bsr NormEA
ddzap9:
 bra dis9
derr_abc:
 bra diserr
*** ABCD & SBCD ***
bcd_oper:
 addq.w #2,D0
 bsr putopcode
ext_addsub:
 bsr padd
 btst #3,D2
 bne.s mem_to_mem
 bsr putdreg0
 move.b #',',(A3)+
 bsr putdreg
 bra.s dzap9
mem_to_mem:
 move.b #'-',(A3)+
 move.b #'(',(A3)+
 bsr putAreg0
 move.l #'),-(',D0
 bsr PutLong
 bsr putAreg
 move.b #')',(A3)+
dzap9:
 bra dis9
*** MOVE ***
movebyte:
 move.w D2,D0
 and.w #$01C0,D0
 cmp.w #$40,D0
 beq.s derr_move
 move.w D2,D0
 and.w #$38,D0
 cmp.w #$08,D0 ;An allowed if byte size
 beq.s derr_move
movelong:
moveword:
 move.w D2,D0
 and.w #$01C0,D0
 cmp.w #$01C0,D0
 bne.s move_ok1
 move.w D2,D0
 and.w #$0E00,D0
 cmp.w #$0400,D0
 bcc.s derr_move
move_ok1:
 moveq #4,D0
 bsr putopcode
 and.w #$01C0,D2
 cmp.w #$40,D2
 bne.s move1
 move.b #'a',(A3)+ ;move address
move1:
 bsr putsize
 bsr padd
 bsr NormEA
 move.b #',',(A3)+
 move.w opcode(A5),D1
 move.w D1,D0
 lsr.w #8,D0
 lsr.w #1,D0
 lsr.w #6,D1
 and.b #7,D0
 and.b #7,D1
 bsr HandleEA
 bra.s d9jzump
derr_move:
 bra diserr
*** MOVEQ ***
movequick:
 btst #8,D2
 bne.s derr_move
 moveq #4,D0
 bsr putopcode
 move.b #'q',(A3)+
 bsr padd
 move.b #'#',(A3)+
 move.b D2,D0
 bsr PutHexByte1
 move.b #',',(A3)+
 bsr putdreg
d9jzump:
 bra dis9
*** BRANCH INSTRUCTIONS ***
branch:
 tst.b D2
 beq.s disp16bit
 ext.w D2
 bra.s bracom
disp16bit:
 bsr DGetWord
bracom:
 move.b #'b',(A3)+
 bsr putcond
 move.w opcode(A5),D0
 tst.b D0
 beq.s branch1
 move.b #'.',(A3)+
 move.b #'s',(A3)+
branch1:
 bsr padd
 move.l D2,D0
 ext.l D0
 add.l instrad(A5),D0
 bsr PutHexLong1
 bra dis9
**** ADD & SUB ****
subtract:
 moveq #6,D0
 bra.s addsubcom
addinst:
 moveq #5,D0
addsubcom:
 bsr putopcode
 move.w D2,D0
 and.w #$C0,D0
 cmp.w #$C0,D0
 beq.s addsubadr
 bsr putsize
 btst #8,D2
 bne.s regsource
 move.w D2,D0
 and.w #$38,D0
 cmp.w #$08,D0
 bne.s add_sub_1 
 move.w D2,D0
 and.w #$C0,D0
 beq diserr
add_sub_1:
 bsr padd
 bsr NormEA
 move.b #',',(A3)+
 bsr putdreg
 bra.s d9jmp
ext_addsub1:
 move.b -1(A3),(A3)
 move.b -2(A3),-1(A3)
 move.b #'x',-2(A3)
 addq.l #1,A3
 bsr padd
 bra ext_addsub
regsource:
 move.w D2,D1
 and.w #$38,D1
 beq.s ext_addsub1
 cmp.w #$08,D1
 beq.s ext_addsub1
 bsr padd
 bsr check_dest_adrmode
 bne diserr
 bsr putdreg
 move.b #',',(A3)+
 bsr NormEA
 bra.s d9jmp
addsubadr: ;*** ADDA & SUBA ***
 move.b #'a',(A3)+
 bsr putsize
 bsr padd
 bsr NormEA
 move.b #',',(A3)+
 bsr putAreg
d9jmp: 
 bra dis9
**** $5XXX ****
handle_five:
 move.w D2,D0
 and.w #$3F,D0
 cmp.w #$39,D0
 bhi diserr
 move.w D2,D0
 and.w #$C0,D0
 cmp.w #$C0,D0
 bne.s addsubquick
 move.w D2,D0
 and.w #$38,D0
 cmp.w #$08,D0
 bne.s SccInst
*** DBcc (decrement, test condition and branch) instruction ***
 bsr DGetWord
 move.w D2,-(sp)
 move.w opcode(A5),D2
 move.b #'d',(A3)+
 move.b #'b',(A3)+
 bsr putcond
 bsr padd
 bsr putdreg0
 move.b #',',(A3)+
 move.w (sp)+,D0
 ext.l D0
 add.l instrad(A5),D0
 bsr PutHexLong1
 bra dis9
*** Scc (Set according to condition) instruction ***
SccInst:
 move.b #'s',(A3)+
 bsr putcond
 bsr padd
 bsr NormEA
 bra.s d9jzwumps
*** ADDQ, SUBQ ***
addsubquick:
 btst #8,D2
 beq.s addquick
 moveq #6,D0
 bra.s adsubq2
addquick:
 moveq #5,D0
adsubq2:
 bsr putopcode
 move.b #'q',(A3)+
 bsr putsize
 cmp.b #'b',-1(A3)
 bne.s adsubqz1
 move.w opcode(A5),D0
 and.w #$38,D0
 cmp.w #$08,D0
 beq.s derr_sf
adsubqz1:
 bsr padd
 bsr putqnum
 move.b #',',(A3)+
 bsr NormEA
 bra.s d9jzwumps
*** SHIFT INSTRUCTIONS ***
derr_sf:
 bra.s diserr
shifts:
 move.w D2,D0
 and.w #$C0,D0
 cmp.w #$C0,D0
 beq.s shiftmemory
*** shift register ***
 move.w D2,D0
 lsr.w #3,D0
 bsr shiftopcode
 bsr putsize
 bsr.s padd
 btst #5,D2
 bne.s cntreg
 bsr putqnum
 bra.s putdestreg
cntreg:
 bsr putdreg
putdestreg:
 move.b #',',(A3)+
 bsr putdreg0
d9jzwumps:
 bra.s dis9
shiftmemory:
 moveq #11,D0
 btst D0,D2
 bne.s derr_sf
 move.w D2,D0
 and.w #$3F,D0
 cmp.w #$10,D0
 bcs.s derr_sf
 cmp.w #$39,D0
 bhi.s derr_sf
 move.w D2,D0
 lsr.w #8,D0
 lsr.w #1,D0
 bsr.s shiftopcode
 bsr.s padd
 bsr NormEA
 bra.s dis9
*** LINE-A & LINE-F unimplemented codes ***
lineA:
 moveq #'A',D3
 bra.s lines
lineF:
 moveq #'F',D3
lines:
 lea linenam(pc),A1
 bsr PutStr
 move.b D3,(A3)+
 bra.s dis9
*** UNKNOWN OPCODE ***
diserr:
 move.l A0,A3
 add.w #36,A3
 lea errtx(pc),A1
 bsr PutStr
dis9:
 move.l A0,A1
 add.w #36,A1
dis9a:
 cmp.l A1,A2
 bcc.s dis9b
 move.b #SPACE,(A2)+
 bra.s dis9a
dis9b:
 endline
 rts

padd:
 move.l A0,A1
 add.w #44,A1
pad1:
 move.b #SPACE,(A3)+
 cmp.l A1,A3
 bcs.s pad1
 rts

*** DISPLAY OPCODE ***
putopcode: ;code in D0
 add.w D0,D0
 lea instradrs(pc),A1
 add.w D0,A1
 add.w (A1),A1
 bra PutStr

shiftopcode:
 and.w #3,D0
 bsr.s putopcode
 btst #8,D2
 bne.s leftshift
 move.b #'r',(A3)+
 rts
leftshift:
 move.b #'l',(A3)+
 rts

putqnum:
 move.b #'#',(A3)+
 move.w opcode(A5),D0
 lsr.w #8,D0
 lsr.w #1,D0
 and.b #7,D0
 bne.s zero8a
 moveq #8,D0
zero8a:
 add.b #'0',D0
 move.b D0,(A3)+
 rts

putAreg:
 moveq #'A',D0
 bra.s putregcom
putdreg:
 moveq #'D',D0
putregcom:
 move.b D0,(A3)+
 move.w opcode(A5),D0
 lsr.w #8,D0
 lsr.w #1,D0
 and.b #7,D0
 add.b #'0',D0
 move.b D0,(A3)+
 rts

putAreg0:
 moveq #'A',D0
 bra.s reg0com
putdreg0:
 moveq #'D',D0
reg0com:
 move.b D0,(A3)+
 move.w opcode(A5),D0
 and.b #7,D0
 add.b #'0',D0
 move.b D0,(A3)+
 rts

put_wl_size: ;size:word or long
 move.w opcode(A5),D0
 btst #6,D0
 beq.s wl_word
 moveq #'l',D0
 bra.s wl_com
wl_word:
 moveq #'w',D0
wl_com:
 move.b #'.',(A3)+
 move.b D0,(A3)+
 rts

check_dest_adrmode: ;returns false (not equal) if illegal mode for dest.
;allowed modes Dn, (An), (An)+, -(An), disp(An), disp(An,Rx), absolute
;note: An not allowed
 move.w opcode(A5),D0
 and.w #$3F,D0
 cmp.w #$39,D0
 bhi.s illegal_1
 and.w #$38,D0
 cmp.w #$08,D0
 beq.s illegal_1
 moveq #0,D0
 rts
illegal_1:
 moveq #-1,D0
 rts

check_jump_adrmode: ;(An),disp(An),disp(An,Rx),absolute,PC rel,PC rel(Rx)
 move.w opcode(A5),D0
 and.w #$3F,D0
 cmp.w #$3B,D0
 bhi.s illegal_1
 and.w #$38,D0
 cmp.w #$10,D0
 beq.s legal_1
 cmp.w #$28,D0
 bcs.s illegal_1
legal_1:
 moveq #0,D0
 rts

*** DISPLAY REGISTER LIST (for MOVEM) ***
RegisterList: ;mask word in D2
 move.w opcode(A5),D0
 btst #10,D0
 bne.s normlist
 and.w #$38,D0
 cmp.w #$20,D0
 bne.s normlist
 move.w D2,D0
 moveq #15,D1
bitloop: ;reverse bit order
 lsr.w #1,D0
 roxl.w #1,D2
 dbf D1,bitloop
normlist:
 moveq #0,D4  ;flag:registers found
 moveq #'D',D5 ;register type
 bsr.s regset
 moveq #'A',D5
regset:
 moveq #0,D3  ;flag:in range
 moveq #0,D1  ;bit counter
regloop1:
 lsr.w #1,D2
 bcc.s nlist1
 tst.l D3
 bne.s inrange
 cmp.w #7,D1
 beq.s nostartrange
 btst #0,D2
 beq.s nostartrange
 tst.l D3
 bne.s nostartrange ;already in range
 tst.l D4
 beq.s strng1
 move.b #'/',(A3)+
strng1:
 bsr.s putD1reg
 move.b #'-',(A3)+
 moveq #-1,D3 ;now in range
 bra.s nlist1
inrange:
 cmp.w #7,D1
 beq.s endrange
 btst #0,D2
 bne.s nlist1
endrange:
 bsr.s putD1reg
 moveq #0,D3 ;not in range
 moveq #-1,D4 ;regs found
 bra.s nlist1
nostartrange:
 tst.l D4
 beq.s no_found
 move.b #'/',(A3)+
no_found:
 bsr.s putD1reg
 moveq #-1,D4
nlist1:
 addq.w #1,D1
 cmp.w #8,D1
 bcs.s regloop1
 rts
putD1reg:
 move.b D5,(A3)+
 move.b D1,D0
 and.b #7,D0
 add.b #'0',D0
 move.b D0,(A3)+
 rts

*** put condition code ***
* handles correctly bra, bsr & dbf, dbt & scf, sct
putcond:
 move.w opcode(A5),D0
 move.w D0,D1
 lsr #7,D0
 and.w #$1E,D0
 and.w #$F000,D1
 cmp.w #$6000,D1
 beq.s putcnd1
 cmp.b #2,D0
 bhi.s putcnd1
 beq.s condf
 move.b #'t',(A3)+
 rts
condf:
 move.b #'f',(A3)+
 rts
putcnd1:
 lea condcodes(pc),A1
 add.w D0,A1
 move.b (A1)+,(A3)+
 move.b (A1)+,(A3)+
retu:
 rts

*** OUTPUT NORMAL EFFECTIVE ADDRESS (mode=bits 5-3, reg=bits 2-0)
NormEA:
 move.w opcode(A5),D0
 move.w D0,D1
 and.w #7,D0
 lsr.w #3,D1
 and.w #7,D1
HandleEA:   ;D1 = mode, D0 = register
 tst.b D1
 bne.s no_zero
 moveq #'D',D1 ;data register direct
rega:
 move.b D1,(A3)+
 add.b #'0',D0
 move.b D0,(A3)+
 rts
no_zero:
 cmp.b #1,D1
 bne.s no_one
 moveq #'A',D1
 bra.s rega  ;address register direct
no_one:
 cmp.b #3,D1
 bhi.s more_than_three
twocase:
 move.b #'(',(A3)+ ;(An) & (An)+
 move.b #'A',(A3)+
 add.b #'0',D0
 move.b D0,(A3)+
 move.b #')',(A3)+
 cmp.b #3,D1
 bne.s Ej
 move.b #'+',(A3)+
Ej:
 rts
more_than_three:
 cmp.b #4,D1
 bne.s no_four  ;-(An)
 move.b #'-',(A3)+
 bra.s twocase
no_four:
 cmp.b #5,D1
 bne.s no_five
 move.b D0,-(sp) ;disp(An)
 bsr DGetWord
 move.w D2,D0
 bsr PutHexWord1
 move.b (sp)+,D0
 bra.s twocase
no_five:
 cmp.b #6,D1
 bne.s must_be_seven
 move.b D0,-(sp) ;disp(An,index)
 bsr DGetWord
 move.b D2,D0
 bsr PutHexByte1
 move.b #'(',(A3)+
 move.b #'A',(A3)+
 move.b (sp)+,D0
 add.b #'0',D0
 move.b D0,(A3)+
 move.b #',',(A3)+
 bsr HandleIndex
 move.b #')',(A3)+
 rts
must_be_seven:
 dbf D0,no_abs_short ;jump if reg<>0
 bsr DGetWord
 move.w D2,D0
 bra PutHexWord1 ;absolute short
no_abs_short:
 dbf D0,no_abs_long ;jump if reg<>1
 bsr DGetLong
 move.l D2,D0
 bra PutHexLong1  ;absolute long
no_abs_long:
 dbf D0,no_pc_rel ;jump if reg<>2
 bsr DGetWord
 move.w D2,D0
 ext.l D0
 add.l A4,D0
 subq.l #2,D0
 bsr PutHexLong1  ;pc relative
 lea pcnam(pc),A1
 bra PutStr
no_pc_rel:
 dbf D0,no_pc_ind ;jump if reg<>3
 bsr DGetWord
 move.b D2,D0
 ext.w D0
 ext.l D0
 add.l A4,D0
 subq.l #2,D0
 bsr PutHexLong1 ;pc relative with index
 move.b #'(',(A3)+
 bsr HandleIndex
 move.b #')',(A3)+
 rts
no_pc_ind:
 dbf D0,illegal_mode
Immediate:
 move.b #'#',(A3)+
 move.w opcode(A5),D0 ;immediate
 move.w D0,D1
 and.w #$B0C0,D0 ;lots of work to find out the right size
 cmp.w #$80C0,D0
 beq.s wordsize
 move.w D1,D0
 and.w #$F1C0,D0
 cmp.w #$4180,D0
 beq.s wordsize
 move.w D1,D0
 and.w #$C000,D0
 bne.s normsize
 move.w D1,D0
 and.w #$3000,D0
 beq.s normsize
 cmp.w #$1000,D0
 beq.s bytesize
 cmp.w #$2000,D0
 beq.s longsize
 bra.s wordsize
normsize:
 move.w D1,D0
 and.w #$C0,D0
 beq.s bytesize
 cmp.w #$40,D0
 beq.s wordsize
 cmp.w #$80,D0
 beq.s longsize
 btst #8,D1
 beq.s wordsize
longsize:
 bsr DGetLong
 move.l D2,D0
 bra PutHexLong1
wordsize:
 bsr DGetWord
 move.w D2,D0
 bra PutHexWord1
bytesize:
 bsr DGetWord
 move.b D2,D0
 bra PutHexByte1
illegal_mode:
 addq.l #4,sp ;pop return address
 bra diserr

HandleIndex: ;index byte in D2
 move.w D2,D0
 bmi.s adreg
 moveq #'D',D1
 bra.s Ind1
adreg:
 moveq #'A',D1
Ind1:
 move.b D1,(A3)+
 lsr.w #8,D0
 lsr.w #4,D0
 and.b #7,D0
 add.b #'0',D0
 move.b D0,(A3)+
 move.b #'.',(A3)+
 btst #11,D2
 bne.s IndLong
 move.b #'w',(A3)+
 rts
IndLong:
 move.b #'l',(A3)+
 rts

putsize:
 move.b #'.',(A3)+
 move.w opcode(A5),D0
 move.w D0,D1
 and.w #$C000,D0
 bne.s normsize1
 move.w D1,D0
 and.w #$3000,D0
 beq.s normsize1
 cmp.w #$1000,D0
 beq.s bytesize1
 cmp.w #$2000,D0
 beq.s longsize1
 bra.s wordsize1
normsize1:
 move.w D1,D0
 and.w #$C0,D0
 beq.s bytesize1
 cmp.w #$40,D0
 beq.s wordsize1
 cmp.w #$80,D0
 beq.s longsize1
 and.w #$0100,D1
 beq.s wordsize1
longsize1:
 move.b #'l',(A3)+
 rts
wordsize1:
 move.b #'w',(A3)+
 rts
bytesize1:
 move.b #'b',(A3)+
 rts

*** GET WORD FOR DISASSEMBLER ***
DGetWord:
 move.w (A4)+,D0
 move.w D0,D2
 move.l A3,-(sp)
 move.l A2,A3
 bsr PutHexWord
 move.l A3,A2
 move.l (sp)+,A3
 move.b #SPACE,(A2)+
 rts

*** GET LONGWORD FOR DISASSEMBLER ***
DGetLong:
 move.l (A4)+,D0
 move.l D0,D2
 move.l A3,-(sp)
 move.l A2,A3
 bsr PutHexLong
 move.l A3,A2
 move.l (sp)+,A3
 move.b #SPACE,(A2)+
 rts

**** DISPLAY MEMORY ****
memdisplay:
 bsr getparams
 move.l Addr(A5),A4
disploop:
 startline
 move.l A4,D0
 bsr PutHexLong
 move.w #': ',(A3)+
 moveq #3,D1
disp1:
 move.l (A4)+,D0
 bsr PutHexLong
 move.b #SPACE,(A3)+
 dbf D1,disp1
 sub.w #16,A4
 move.w #' ''',(A3)+
 moveq #15,D1
disp2:
 move.b (A4)+,D0  ;printable codes are $20-$7F and $A0-$FF
 cmp.b #SPACE+$80,D0
 bcc.s disp3
 cmp.b #SPACE,D0
 bge.s disp3 ;note: signed comparison handles correctly codes >= $80
 move.b #'.',D0
disp3:
 move.b D0,(A3)+
 dbf D1,disp2
 move.w #$270A,(A3)+
 clr.b (A3)
 bsr StrOutRedirect
 bsr CheckEnd
 bne.s disploop
 move.l A4,Addr(A5)
 bra mainloop

**** SKIP SPACES ****
SkipSpc:
 cmp.b #SPACE,(A3)+
 beq.s SkipSpc
 subq.l #1,A3
 rts

*** PART OF GETNUM (Get Ascii number in single quotes) ***
AsciiNum:
 addq.l #1,A3
Ascii_loop:
 move.b (A3)+,D1
 beq.s Ascii_2
 cmp.b #'''',D1
 beq.s Ascii_1
Ascii_0:
 lsl.l #8,D0
 move.b D1,D0
 bra.s Ascii_loop
Ascii_1:
 cmp.b #'''',(A3)+ ;check for double single quote
 beq.s Ascii_0
 subq.l #1,A3
Ascii_2:
 move.l (sp)+,D2
 rts

**** GET A HEX OR DECIMAL NUMBER ****
* (no check for overflow!)
* number formats:
*  'abcd'    : ascii string representing a longword
*  nnn, $nnn : positive hex number
*  -$nnn     : negative hex number
*  +nnn      : positive decimal number
*  -nnn      : negative decimal numbber
GetNum:
 move.l D2,-(sp)
 bsr.s SkipSpc
 moveq #0,D2
 moveq #0,D0
 cmp.b #'''',(A3)
 beq.s AsciiNum
 cmp.b #'-',(A3)
 bne.s posnum
 moveq #-1,D2
 addq.l #1,A3
 cmp.b #'$',(A3)+
 beq.s gethex1
 subq.l #1,A3
 bra.s getdec
posnum:
 cmp.b #'+',(A3)
 bne.s GetHex
 addq.l #1,A3
getdec:
 moveq #0,D1 ;get decimal number
numloop:
 move.b (A3)+,D1
 sub.b #'0',D1
 blt.s num9
 cmp.b #9,D1
 bgt.s num9
 move.l D0,-(sp) ;multiply by 10 ...
 asl.l #2,D0
 add.l (sp)+,D0
 asl.l #1,D0
 add.l D1,D0 ;add the digit
 bra.s numloop
GetHex: ;get hexadecimal number
 cmp.b #'$',(A3)+
 beq.s gethex1
 subq.l #1,A3
gethex1:
 move.b (A3)+,D1 ;get hex number
 cmp.b #'0',D1
 bcs.s num9
 cmp.b #'9',D1
 bls.s hxnum
 cmp.b #'A',D1
 bcs.s num9
 and.b #$DF,D1
 cmp.b #'A',D1
 bcs.s num9
 cmp.b #'F',D1
 bhi.s num9
 sub.b #55,D1
 bra.s hxl1
hxnum:
 and.b #$0F,D1
hxl1:
 asl.l #4,D0
 or.b D1,D0
 bra.s gethex1
num9:
 subq.l #1,A3
 tst.l D2
 beq.s num99
 neg.l D0
num99:
 move.l (sp)+,D2
 rts

*** Convert char in D0 to lower case ***
tolower:
 cmp.b #'A',D0
 bcs.s low1
 cmp.b #'Z',D0
 bhi.s low1
 bset #5,D0
low1:
 rts

*** ASSEMBLE ***
* command format:
* a       :assembles at the current address, asks instruction
* a <addr>:assembler at <addr>, asks instruction
* a <addr> <instruction> : assembler <instruction> at <addr>
* in all cases, if no errors occurred, prompts '<next_addr>:'
* and asks a new instruction. use <CR> to exit this mode
* Ctrl-E can be used to edit the disassembled instruction at this location
assemble:
 bsr SkipSpc
 tst.b (A3)
 bne.s assem_01
 move.l Addr(A5),D0
 bra.s assem_02
assem_01:
 bsr GetNum
 btst #0,D0
 bne error ;assembling to odd address is illegal
 move.l D0,Addr(A5)
assem_02:
 move.l D0,EndAddr(A5)
 bsr SkipSpc
 tst.b (A3)
 bne.s assem1a
assem1:
 move.l Addr(A5),D0
 move.l D0,EndAddr(A5)
 startline
 bsr PutHexLong
 move.w #': ',(A3)+
 clr.b (A3)
 bsr StrOutWindow
 moveq #1,D0
assem1a0:
 bsr GetInput
 tst.w D0       ;check for Ctrl-E (GetInput returns -1)
 bpl.s assem1a1 ;branch if not Ctrl-E
*** disassemble at current address and put result in input buffer ***
 move.l Addr(A5),A4
 bsr disasmline
 move.l outbufadr(A5),A0
 lea (InpBuf-OutBuf)(A0),A1
 lea 36(A0),A0
 moveq #LF,D0
assem1cploop:        ;copy instruction to input buffer
 move.b (A0)+,(A1)+  ;until LF found
 cmp.b (A0),D0
 bne.s assem1cploop
 clr.b (A1)          ;mark end of string
 moveq #2,D0     ;GetInput mode: edit existing line
 bra.s assem1a0
assem1a1:
 tst.b (A3)
 beq mainloop
assem1a:
 move.l A3,A1
 moveq #0,D1 ;quote mode flag
lowerloop:   ;convert to lower case when not in quotes
 move.b (A1),D0
 beq.s asmlow9
 cmp.b #'''',D0
 bne.s noquote
 eor.b #$FF,D1
noquote:
 tst.b D1
 bne.s asmlow1
 bsr tolower
asmlow1:
 move.b D0,(A1)+
 bra.s lowerloop
asmlow9:
 lea instradrs(pc),A2
 moveq #-1,D1
find_instr_loop: ;find the instruction opcode from the opcode table
 addq.w #1,D1
 move.l A3,A1
 move.l A2,A0
 tst.w (A2)+
 beq.s try_branch
 add.w (A0),A0
comp_str:
 tst.b (A0)
 beq.s instr_found
 cmp.b (A0)+,(A1)+
 beq.s comp_str
 bra.s find_instr_loop
try_branch:      ;if instruction not found in table, it can be
 cmp.b #'b',(A3) ;a branch, DBcc, Scc or dc.?
 bne try_Scc
 addq.l #1,A3
 bsr check_cond
 tst.w D1
 bpl.s branch_1
br_err:
 bra error
branch_1:
 lsl.w #8,D1
 or.w #$6000,D1
 cmp.b #'.',(A3)
 bne.s long_branch
 addq.l #1,A3
 cmp.b #'l',(A3)+
 beq.s long_branch
 cmp.b #'s',-1(A3)
 bne.s br_err
 bsr GetNum
 sub.l Addr(A5),D0
 subq.l #2,D0
 move.b D0,D2
 ext.w D2
 ext.l D2
 cmp.l D0,D2
 bne.s br_err
 and.w #$FF,D0
 beq.s br_err
 or.w D1,D0
 bra one_word_instr
long_branch:
 bsr GetNum
 sub.l Addr(A5),D0
 subq.l #2,D0
 move.w D0,D2
 ext.l D2
 cmp.l D0,D2
 bne.s br_err
 exg D0,D1
 bra two_words_instr
instr_found:
 move.l A1,A3
 add.w D1,D1
 lea instrjumps(pc),A0
 add.w D1,A0
 add.w (A0),A0
 jmp (A0)
try_Scc:
 cmp.b #'s',(A3)
 bne try_DBcc
 addq.l #1,A3
 cmp.b #'f',(A3)+
 bne.s Scc_1
 moveq #1,D1
 bra.s Scc_3
Scc_1:
 cmp.b #'t',-1(A3)
 bne.s Scc_2
 moveq #0,D1
 bra.s Scc_3
Scc_2:
 subq.l #1,A3
 bsr check_cond
 tst.w D1
 bmi.s cc_err
 cmp.w #2,D1
 bcs.s cc_err
Scc_3:
 move.w D1,D3
 addq.l #2,Addr(A5)
 bsr GetEA
 cmp.w #1,D1
 beq.s cc_err
 lsl.w #8,D3
 lsl.w #3,D1
 or.w D1,D0
 cmp.w #$39,D0
 bhi.s cc_err
 or.w D3,D0
 or.w #$50C0,D0
 bra zzcom
cc_err:
 bra error
try_DBcc:
 cmp.b #'d',(A3)+
 bne.s cc_err
 cmp.b #'b',(A3)+
 bne.s try_dc
 cmp.b #'t',(A3)+
 bne.s DBcc_1
 moveq #0,D1
 bra.s DBcc_3
DBcc_1:
 cmp.b #'f',-1(A3)
 bne.s DBcc_2
DBcc_1a:
 moveq #1,D1
 bra.s DBcc_3
DBcc_2:
 subq.l #1,A3
 bsr check_cond
 tst.w D1
 beq.s DBcc_1a
 bmi.s cc_err
 cmp.w #1,D1
 beq.s cc_err
DBcc_3:
 move.w D1,D3
 bsr SkipSpc
 cmp.b #'d',(A3)+
 bne.s cc_err
 bsr GetNum_0_7
 bmi.s cc_err
 lsl.w #8,D3
 or.w D0,D3
 or.w #$50C8,D3
 bsr SkipComma
 bmi.s cc_err
 bsr GetNum 
 sub.l Addr(A5),D0
 subq.l #2,D0
 move.l D0,D1
 ext.l D0
 cmp.l D0,D1
 bne.s cc_err
 move.w D3,D0
 bra two_words_instr
try_dc:
 cmp.b #'c',-1(A3)
 bne.s cc_err
 bsr GetSize  ;*** DC.B, DC.W, DC.L ***
 bsr SkipSpc
 move.l Addr(A5),A0
 move.w size(A5),D0
 beq.s dc_byte
 subq.w #1,D0
 beq.s dc_word
dc_long:
 bsr GetNum
 move.l D0,(A0)+
 bsr SkipComma
 bpl.s dc_long
 bra.s dc_exit
dc_word:
 bsr GetNum
 move.w D0,(A0)+
 bsr SkipComma
 bpl.s dc_word
 bra.s dc_exit
dc_byte:
 bsr getstring
 move.l A1,A0
dc_exit:
 move.l A0,D0 ;align address to word boundary
 btst #0,D0
 beq.s dc_exit1
 addq.l #1,D0
dc_exit1:
 move.l D0,Addr(A5)
 bra.s dc_exit2
assem9:
 lea UpAndClearEol(pc),A0 ;move cursor to previous line and clear the line
 bsr StrOutWindow
 move.l EndAddr(A5),A4
 bsr disasmline
 bsr StrOutWindow
dc_exit2:
 bra assem1

check_cond: ;check if the chars at A3 form a condition code
 move.b (A3)+,D0
 lsl.w #8,D0
 move.b (A3)+,D0
 lea condcodes(pc),A0
 moveq #0,D1
chk_cond1:
 cmp.w (A0)+,D0
 beq.s cond9
 addq.w #1,D1
 cmp.w #16,D1
 bcs.s chk_cond1
 moveq #-1,D1
cond9:
 rts

*** SHIFT INSTRUCTIONS ***
as_asm:
 move.w #$E000,D3
 bra.s shift_instr
ls_asm:
 move.w #$E008,D3
 bra.s shift_instr
rox_asm:
 move.w #$E010,D3
 bra.s shift_instr
rot_asm:
 move.w #$E018,D3
shift_instr:
 cmp.b #'r',(A3)+
 beq.s shift_1
 cmp.b #'l',-1(A3)
 bne.s sh_err
 bset #8,D3
shift_1:
 cmp.b #'.',(A3)
 bne.s shift_mem
 bsr GetSize
 move.w size(A5),D0
 lsl.w #6,D0
 or.w D0,D3
 bsr SkipSpc
 cmp.b #'#',(A3)
 bne.s count_in_reg
 addq.l #1,A3
 bsr GetNum_1_8
 bmi.s sh_err
 bra.s shift_2
count_in_reg:
 cmp.b #'d',(A3)+
 bne.s sh_err
 bsr GetNum_0_7
 bmi.s sh_err
 bset #5,D3
shift_2:
 lsl.w #8,D0
 lsl.w #1,D0
 or.w D0,D3
 bsr SkipComma
 bmi.s sh_err
 cmp.b #'d',(A3)+
 bne.s sh_err
 bsr GetNum_0_7
 bmi.s sh_err
 or.w D3,D0
 bra one_word_instr
sh_err:
 bra error
shift_mem:
 addq.l #2,Addr(A5)
 moveq #0,D0
 move.b D3,D0
 lsl.w #6,D0
 or.w D0,D3
 and.w #$F7C0,D3
 or.w #$C0,D3
 bsr GetEA
 cmp.w #2,D1
 bcs.s sh_err
 lsl.w #3,D1
 or.w D1,D0
 cmp.w #$39,D0
 bhi.s sh_err
 or.w D3,D0
 bra.s zcom_1
*** ADD & SUB ***
add_asm:
 move.w #$D000,D3
 bra.s a_s_asm
sub_asm:
 move.w #$9000,D3
a_s_asm:
 cmp.b #'x',(A3)
 bne.s no_as_ext
 addq.l #1,A3
 bsr GetSize
 bset #8,D3
 bra ext_as_asm
no_as_ext:
 addq.l #2,Addr(A5)
 cmp.b #'q',(A3)
 bne.s no_as_quick
 addq.l #1,A3
 bsr GetSize
 bsr SkipSpc
 cmp.b #'#',(A3)+
 bne.s as_err
 bsr GetNum_1_8
 bmi.s as_err
 lsl.w #8,D0
 lsl.w #1,D0
 cmp.w #$9000,D3
 bne.s as_quick_1
 bset #8,D0
as_quick_1:
 or.w #$5000,D0
 move.w size(A5),D1
 lsl.w #6,D1
 or.w D1,D0
 move.w D0,D3
 bsr SkipComma
 bmi.s as_err
 bsr GetEA
 tst.w size(A5)
 bne.s as_quick_z1
 cmp.w #1,D1
 beq.s as_err
as_quick_z1:
 lsl.w #3,D1
 or.w D1,D0
 or.w D3,D0
zcom_1:
 bra zzcom
as_err:
 bra error
no_as_quick:
 cmp.b #'i',(A3)
 bne.s no_as_imm
 addq.l #1,A3
 bsr GetSize
 bsr SkipSpc
 cmp.b #'#',(A3)+
 bne.s as_err
as_imm_1:
 bsr PutImm
 bsr SkipComma
 bmi.s as_err
 bsr GetEA
 cmp.w #1,D1
 beq.s as_addr_imm_1
 lsl.w #3,D1
 or.w D1,D0
 move.w size(A5),D1
 lsl.w #6,D1
 or.w D1,D0
 bset #10,D0
 cmp.w #$9000,D3
 beq.s zcom_1
 bset #9,D0
 bra.s zcom_1
as_addr_imm_1:
 lsl.w #8,D0
 lsl.w #1,D0
 move.w size(A5),D1
 beq.s as_err
 subq.w #1,D1
 lsl.w #8,D1
 or.w D1,D0
 or.w #$FC,D0
 or.w D3,D0
 bra.s zcom_1
no_as_imm:
 moveq #0,D5
 cmp.b #'a',(A3)
 bne.s as_norm_1
 moveq #-1,D5
 addq.l #1,A3
as_norm_1:
 bsr GetSize
 bsr SkipSpc
 tst.l D5
 bne.s as_norm_2
 cmp.b #'#',(A3)+
 beq.s as_imm_1
 subq.l #1,A3
as_norm_2:
 cmp.b #'d',(A3)
 beq as_data_reg_source
 bsr GetEA
 tst.w size(A5)
 bne.s as_norm_3
 cmp.w #1,D1
 beq.s as_err2
as_norm_3:
 lsl.w #3,D1
 or.w D1,D0
 or.w D0,D3
 bsr SkipComma
 bmi.s as_err2
 cmp.b #'d',(A3)
 bne.s try_as_addr
 tst.l D5
 bne.s as_err2
 addq.l #1,A3
 bsr GetNum_0_7
 bmi.s as_err2
 lsl.w #8,D0
 lsl.w #1,D0
 or.w D3,D0
 move.w size(A5),D1
 lsl.w #6,D1
 or.w D1,D0
 bra.s zcom_2
as_err2:
 bra error
try_as_addr:
 cmp.b #'a',(A3)+
 bne.s as_err2
 bsr GetNum_0_7
 bmi.s as_err2
 lsl.w #8,D0
 lsl.w #1,D0
 or.w D3,D0
 or.w #$C0,D0
 move.w size(A5),D1
 beq.s as_err2
 subq.w #1,D1
 lsl.w #8,D1
 or.w D1,D0
zcom_2:
 bra zzcom
as_data_reg_source:
 addq.l #1,A3
 bsr GetNum_0_7
 lsl.w #8,D0
 lsl.w #1,D0
 or.w D0,D3
 bsr SkipComma
 bmi.s as_err2
 bsr GetEA
 tst.w D1
 bne.s as_datasource_1
 lsl.w #8,D0
 lsl.w #1,D0
 move.w D3,D1
 and.w #$F000,D3
 lsr.w #8,D1
 lsr.w #1,D1
 and.w #7,D1
 or.w D3,D0
 or.w D1,D0
 bra.s as_ds_1
as_datasource_1:
 cmp.w #1,D1
 beq.s as_special
 lsl.w #3,D1
 or.w D1,D0
 cmp.w #$39,D0
 bhi.s as_err2
 or.w D3,D0
 bset #8,D0
as_ds_1:
 move.w size(A5),D1
 lsl.w #6,D1
 or.w D1,D0
 bra.s zcom_2
as_special:
 lsl.w #8,D0
 lsl.w #1,D0
 move.w D3,D1
 and.w #$F000,D3
 or.w D3,D0
 lsr.w #8,D1
 lsr.w #1,D1
 and.w #7,D1
 or.w D1,D0
 move.w size(A5),D1
 beq.s cmp_err
 subq.w #1,D1
 lsl.w #8,D1
 or.w D1,D0
 or.w #$C0,D0
 bra zzcom
*** CMP ***
cmp_asm:
 cmp.b #'m',(A3)
 bne.s no_mem_cmp
 addq.l #1,A3
 bsr GetSize    ;CMPM
 bsr SkipSpc
 cmp.b #'(',(A3)+
 bne.s cmp_err
 cmp.b #'a',(A3)+
 bne.s cmp_err
 bsr GetNum_0_7
 bmi.s cmp_err
 move.w D0,D3
 cmp.b #')',(A3)+
 bne.s cmp_err
 cmp.b #'+',(A3)+
 bne.s cmp_err
 bsr SkipComma
 bmi.s cmp_err
 cmp.b #'(',(A3)+
 bne.s cmp_err
 cmp.b #'a',(A3)+
 bne.s cmp_err
 bsr GetNum_0_7
 bmi.s cmp_err
 cmp.b #')',(A3)+
 bne.s cmp_err
 cmp.b #'+',(A3)+
 bne.s cmp_err
 lsl.w #8,D0
 lsl.w #1,D0
 or.w D3,D0
 or.w #$B108,D0
 bra one_word_instr
cmp_err:
 bra error
no_mem_cmp:
 addq.l #2,Addr(A5)
 cmp.b #'i',(A3)
 bne.s no_cmp_imm
 addq.l #1,A3
 bsr GetSize
 bsr SkipSpc
 cmp.b #'#',(A3)+
 bne.s cmp_err
cmp_imm_1:
 bsr PutImm
 bsr SkipComma
 bmi.s cmp_err
 bsr GetEA
 cmp.w #1,D1
 beq.s cmp_addr_imm1
 lsl.w #3,D1
 or.w D1,D0
 cmp.w #$39,D0
 bhi.s cmp_err
 move.w size(A5),D1
 lsl.w #6,D1
 or.w D1,D0
 or.w #$0C00,D0
 bra zzcom
cmp_addr_imm1:
 lsl.w #8,D0
 lsl.w #1,D0
 move.w size(A5),D1
 beq.s cmp_err
 subq.w #1,D1
 lsl.w #8,D1
 or.w D1,D0
 or.w #$B0FC,D0
 bra zzcom
no_cmp_imm:
 moveq #0,D5
 cmp.b #'a',(A3)
 bne.s no_cmp_addr
 addq.l #1,A3
 moveq #-1,D5
no_cmp_addr:
 bsr GetSize
 bsr SkipSpc
 tst.l D5
 bne.s cmp_1
 cmp.b #'#',(A3)+
 beq.s cmp_imm_1
 subq.l #1,A3
cmp_1:
 bsr GetEA
 cmp.w #1,D1
 bne.s cmp_2
 tst.w size(A5)
 beq.s cmp_err2
cmp_2:
 lsl.w #3,D1
 or.w D1,D0
 move.w D0,D3
 bsr SkipComma
 bmi.s cmp_err2
 bsr getreg
 bmi.s cmp_err2
 btst #3,D1
 bne.s cmp_addr
 tst.l D5
 bne.s cmp_err2
 move.w size(A5),D0
 lsl.w #6,D0
 or.w D3,D0
 and.w #7,D1
 lsl.w #8,D1
 lsl.w #1,D1
 or.w D1,D0
 or.w #$B000,D0
 bra zzcom
cmp_err2:
 bra error
cmp_addr:
 move.w size(A5),D0
 beq.s cmp_err2
 subq.w #1,D0
 lsl.w #8,D0
 or.w D3,D0
 and.w #7,D1
 lsl.w #8,D1
 lsl.w #1,D1
 or.w D1,D0
 or.w #$B0C0,D0
 bra zzcom
*** AND & OR ***
and_asm:
 move.w #$C000,D3
 bra.s a_o_asm
or_asm:
 move.w #$8000,D3
a_o_asm:
 addq.l #2,Addr(A5)
 cmp.b #'i',(A3)
 bne.s a_o_2
 addq.l #1,A3
 cmp.b #'.',(A3)
 beq.s a_o_0
 lsr.w #5,D3
 and.w #$0200,D3
 bra logical_status
a_o_0:
 bsr GetSize
 bsr SkipSpc
 cmp.b #'#',(A3)+
 bne.s a_o_err
a_o_imm:
 bsr PutImm
 bsr SkipComma
 bmi.s a_o_err
 bsr GetEA
 cmp.w #1,D1
 beq.s a_o_err
 lsl.w #3,D1
 or.w D1,D0
 cmp.w #$39,D0
 bhi.s a_o_err
 move.w size(A5),D1
 lsl.w #6,D1
 or.w D1,D0
 cmp.w #$C000,D3
 bne.s zcom_3
 bset #9,D0
 bra.s zcom_3
a_o_err:
 bra error
a_o_2:
 bsr GetSize
 bsr SkipSpc
 cmp.b #'#',(A3)+
 beq.s a_o_imm
 move.w size(A5),D0
 lsl.w #6,D0
 or.w D0,D3
 subq.l #1,A3
 cmp.b #'d',(A3)
 bne.s reg_dest
 addq.l #1,A3
 bsr GetNum_0_7
 bmi.s a_o_err
 lsl.w #8,D0
 lsl.w #1,D0
 or.w D0,D3
 bsr SkipComma
 bmi.s a_o_err
 bsr GetEA
 tst.w D1
 beq.s a_o_3a
 cmp.w #1,D1
 beq.s a_o_err
 lsl.w #3,D1
 or.w D1,D0
 cmp.w #$39,D0
 bhi.s a_o_err
 bset #8,D0
 or.w D3,D0
zcom_3:
 bra zzcom
a_o_3a:
 move.w D0,D7
 move.w D3,D0
 lsr.w #8,D0
 lsr.w #1,D0
 and.w #7,D0
 and.w #$F0C0,D3
 lsl.w #8,D7
 lsl.w #1,D7
 or.w D7,D3
 lsl.w #3,D1
 or.w D1,D0
 or.w D3,D0
 bra.s zcom_3
reg_dest:
 bsr GetEA
 cmp.w #1,D1
 beq.s a_o_err2
 lsl.w #3,D1
 or.w D1,D0
 cmp.w #$39,D0
 bhi.s a_o_err2
 or.w D0,D3
 bsr SkipComma
 bmi.s a_o_err2
 cmp.b #'d',(A3)+
 bne.s a_o_err2
 bsr GetNum_0_7
 bmi.s a_o_err2
 lsl.w #8,D0
 lsl.w #1,D0
 or.w D3,D0
 bra.s zcom_3
a_o_err2:
 bra error
*** ABCD & SBCD & ADDX & SUBX ***
abcd_asm:
 move.w #$C100,D3
 bra.s bcd_asm
sbcd_asm:
 move.w #$8100,D3
bcd_asm:
 clr.w size(A5)
ext_as_asm:
 bsr SkipSpc
 cmp.b #'d',(A3)+
 bne.s bcd_mem
 bsr GetNum_0_7
 bmi.s err_bcd
 or.w D0,D3
 bsr SkipComma
 bmi.s err_bcd
 cmp.b #'d',(A3)+
 bne.s err_bcd
 bsr GetNum_0_7
bcd_1:
 lsl.w #8,D0
 lsl.w #1,D0
 or.w D3,D0
 move.w size(A5),D1
 lsl.w #6,D1
 or.w D1,D0
 bra one_word_instr
bcd_mem:
 cmp.b #'-',-1(A3)
 bne.s err_bcd
 cmp.b #'(',(A3)+
 bne.s err_bcd
 cmp.b #'a',(A3)+
 bne.s err_bcd
 bsr GetNum_0_7
 bmi.s err_bcd
 or.w D0,D3
 bset #3,D3
 cmp.b #')',(A3)+
 bne.s err_bcd
 bsr SkipComma
 bmi.s err_bcd
 cmp.b #'-',(A3)+
 bne.s err_bcd
 cmp.b #'(',(A3)+
 bne.s err_bcd
 cmp.b #'a',(A3)+
 bne.s err_bcd
 bsr GetNum_0_7
 bmi.s err_bcd
 cmp.b #')',(A3)+
 beq.s bcd_1
err_bcd:
 bra error
*** EOR ***
eor_asm:
 addq.l #2,Addr(A5)
 moveq #0,D5
 cmp.b #'i',(A3)
 bne.s eor_1
 addq.l #1,A3
 cmp.b #'.',(A3)
 beq.s eor_0
 move.w #$0A00,D3
 bra logical_status
eor_0:
 moveq #-1,D5
eor_1:
 bsr GetSize
 bsr SkipSpc
 cmp.b #'#',(A3)
 bne.s no_eor_imm
 addq.l #1,A3
 bsr PutImm
 bsr SkipComma
 bmi.s err_bcd
 bsr GetEA
 cmp.w #1,D1
 beq.s err_bcd
 lsl.w #3,D1
 or.w D1,D0
 cmp.w #$39,D0
 bhi.s err_bcd
 move.w size(A5),D1
 lsl.w #6,D1
 or.w D1,D0
 or.w #$0A00,D0
 bra.s zcom_4
no_eor_imm:
 tst.l D5
 bne.s err_bcd
 cmp.b #'d',(A3)+
 bne.s err_bcd
 bsr GetNum_0_7
 bmi.s err_bcd
 move.w D0,D3
 bsr SkipComma
 bmi.s err_bcd 
 bsr GetEA
 cmp.w #1,D1
 beq error
 lsl.w #3,D1
 or.w D1,D0
 cmp.w #$39,D0
 bhi error
 lsl.w #8,D3
 lsl.w #1,D3
 or.w D3,D0
 move.w size(A5),D1
 lsl.w #6,D1
 or.w D1,D0
 or.w #$B100,D0
zcom_4:
 bra zzcom
*** AND & OR & EOR  SR & CCR ***
logical_status:
 or.w #$3C,D3
 bsr SkipSpc
 cmp.b #'#',(A3)+
 bne.s move_err
 move.w #1,size(A5)
 bsr PutImm
 bsr SkipComma
 bmi.s move_err
 cmp.b #'s',(A3)
 bne.s no_log_sr
 addq.l #1,A3
 cmp.b #'r',(A3)+
 bne.s move_err
 or.w #$40,D3
 bra.s log_st1
no_log_sr:
 moveq #'c',D0
 cmp.b (A3)+,D0
 bne.s move_err
 cmp.b (A3)+,D0
 bne.s move_err
log_st1:
 move.w D3,D0
 bra.s zcom_4
*** MOVE ***
move_asm:
 cmp.b #'q',(A3)
 bne.s no_move_quick
 addq.l #1,A3
 bsr SkipSpc
 cmp.b #'#',(A3)+
 bne.s move_err
 bsr GetNum
 move.b D0,D1
 bsr SkipComma
 bmi.s move_err
 cmp.b #'d',(A3)+
 bne.s move_err
 bsr GetNum_0_7
 bmi.s move_err
 lsl.w #8,D0
 lsl.w #1,D0
 or.w #$7000,D0
 move.b D1,D0
 bra one_word_instr
move_err:
 bra error
no_move_quick:
 cmp.b #'p',(A3)
 beq move_peripheral
 cmp.b #'a',(A3)
 bne.s no_move_addr
 moveq #-1,D5
 addq.l #1,A3
 bra normal_move_2
no_move_addr:
 cmp.b #'m',(A3)
 beq movem_asm
 cmp.b #'.',(A3)
 beq normal_move
 bsr SkipSpc
 cmp.b #'s',(A3)
 beq.s move_status
 cmp.b #'u',(A3)
 beq.s move_from_usp
 cmp.b #'a',(A3)
 bne move_to_status_or_ccr
 addq.l #1,A3
 bsr GetNum_0_7
 move.w D0,D3
 bsr SkipComma
 bmi.s move_err
 cmp.b #'u',(A3)+
 bne.s move_err
 cmp.b #'s',(A3)+
 bne.s move_err
 cmp.b #'p',(A3)+
 bne.s move_err
 move.w D3,D0
 or.w #$4E60,D0
 bra one_word_instr
move_from_usp:  ;MOVE USP,An
 addq.l #1,A3
 cmp.b #'s',(A3)+
 bne.s move_err2
 cmp.b #'p',(A3)+
 bne.s move_err2
 bsr SkipComma
 bmi.s move_err2
 cmp.b #'a',(A3)+
 bne.s move_err2
 bsr GetNum_0_7
 bmi.s move_err2
 or.w #$4E68,D0
 bra one_word_instr
move_status:   ;MOVE SR,EA
 addq.l #2,Addr(A5)
 addq.l #1,A3
 cmp.b #'r',(A3)+
 bne.s move_err2
 bsr SkipComma
 bmi.s move_err2
 bsr GetEA
 cmp.w #1,D1
 beq.s move_err2
 lsl.w #3,D1
 or.w D1,D0
 cmp.w #$39,D0
 bhi.s move_err2
 or.w #$40C0,D0
 bra.s zcom_5
move_to_status_or_ccr: ;MOVE EA,SR, MOVE EA,CCR
 move.w #1,size(A5)
 addq.l #2,Addr(A5)
 bsr GetEA
 lsl.w #3,D1
 or.w D0,D1
 bsr SkipComma
 bmi.s move_err2
 cmp.b #'s',(A3)
 bne.s try_move_ccr
 addq.l #1,A3
 cmp.b #'r',(A3)+
 bne.s move_err2
 move.w D1,D0
 or.w #$46C0,D0
 bra.s zcom_5
move_err2:
 bra error
try_move_ccr:
 moveq #'c',D0
 cmp.b (A3)+,D0
 bne.s move_err2
 cmp.b (A3)+,D0
 bne.s move_err2
 cmp.b #'r',(A3)+
 bne.s move_err2
 move.w D1,D0
 or.w #$44C0,D0
zcom_5:
 bra zzcom
normal_move:
 moveq #0,D5
normal_move_2:
 addq.l #2,Addr(A5)
 bsr GetSize
 bsr GetEA
 tst.w size(A5)
 bne.s nm_1
 cmp.w #1,D1
 beq.s move_err2
nm_1:
 lsl.w #3,D1
 or.w D1,D0
 move.w D0,D3
 bsr SkipComma
 bmi.s move_err2
 bsr GetEA
 move.l D1,D2
 lsl.w #3,D2
 or.w D0,D2
 cmp.w #$39,D2
 bhi.s move_err2
 tst.l D5
 beq.s norm_move_2
 cmp.w #1,D1
 bne.s move_err2 ;MOVEA destination must be addr reg
 tst.w size(A5)
 beq.s move_err2 ;MOVEA size can't be BYTE
norm_move_2:
 lsl.w #6,D1
 lsl.w #8,D0
 lsl.w #1,D0
 or.w D1,D0
 or.w D3,D0
 move.w size(A5),D1
 bne.s not_byte_move
 or.w #$1000,D0
 bra.s zump1
not_byte_move:
 addq.w #1,D1
 bchg #0,D1
 lsl.w #8,D1
 lsl.w #4,D1
 or.w D1,D0
zump1:
 bra zzcom
*** MOVEP ***
move_peripheral:
 addq.l #1,A3
 addq.l #2,Addr(A5)
 bsr GetSize
 tst.w size(A5)
 beq.s move_err3
 bsr SkipSpc
 cmp.b #'d',(A3)
 bne.s move_from_peripheral
 addq.l #1,A3
 bsr GetNum_0_7
 bmi.s move_err3
 lsl.w #8,D0
 lsl.w #1,D0
 move.w D0,D3
 bsr SkipComma
 bmi.s move_err3
 bsr GetEA
 cmp.w #5,D1
 bne.s move_err3
 or.w D3,D0
 or.w #$0188,D0
move_per_1:
 move.w size(A5),D1
 subq.w #1,D1
 lsl.w #6,D1
 or.w D1,D0
 bra zzcom
move_from_peripheral:
 bsr GetEA
 cmp.w #5,D1
 bne.s move_err3
 move.w D0,D3
 bsr SkipComma
 bmi.s move_err3
 cmp.b #'d',(A3)+
 bne.s move_err3
 bsr GetNum_0_7
 bmi.s move_err3
 lsl.w #8,D0
 lsl.w #1,D0
 or.w D3,D0
 or.w #$0108,D0
 bra.s move_per_1
move_err3:
 bra error
*** MOVEM ***
movem_asm:
 addq.l #1,A3
 addq.l #4,Addr(A5)
 bsr GetSize
 tst.w size(A5)
 beq.s move_err3
 bsr SkipSpc
 cmp.b #'d',(A3)
 beq.s regs_to_mem
 cmp.b #'a',(A3)
 bne.s regs_from_mem
regs_to_mem:
 bsr getreglist
 bmi.s move_err3
 move.w D2,D3
 bsr SkipComma
 bmi.s move_err3
 bsr GetEA
 cmp.w #2,D1
 bcs.s move_err3
 cmp.w #3,D1
 beq.s move_err3
 move.w D1,D2
 lsl.w #3,D2
 or.w D0,D2
 cmp.w #$39,D2
 bhi.s move_err3
 cmp.w #4,D1
 bne.s regs_to_mem_1
 moveq #15,D2
juzumps:
 lsr.w #1,D3 ;reverse bit order
 roxl.w #1,D4
 dbf D2,juzumps
 move.w D4,D3
regs_to_mem_1:
 lsl.w #3,D1
 or.w D1,D0
 or.w #$4880,D0
zumpsis:
 move.w size(A5),D1
 subq.w #1,D1
 lsl.w #6,D1
 or.w D1,D0
 move.l EndAddr(A5),A0
 move.w D0,(A0)+
 move.w D3,(A0)
 bra assem9
move_err4:
 bra error
regs_from_mem:
 bsr GetEA
 cmp.w #2,D1
 bcs.s move_err4
 cmp.w #4,D1
 beq.s move_err4
 lsl.w #3,D1
 or.w D1,D0
 cmp.w #$3B,D0
 bhi.s move_err4
 move.w D0,D3
 bsr SkipComma
 bmi.s move_err4
 bsr getreglist
 bmi.s move_err4
 move.w D3,D0
 move.w D2,D3
 or.w #$4C80,D0
 bra.s zumpsis 
*** BTST & BCHG & BCLR & BSET ***
btst_asm:
 moveq #0,D3
 bra.s bit_ins_1
bchg_asm:
 moveq #1,D3
 bra.s bit_ins_1
bclr_asm:
 moveq #2,D3
 bra.s bit_ins_1
bset_asm:
 moveq #3,D3
bit_ins_1:
 addq.l #2,Addr(A5)
 lsl.w #6,D3
 bsr SkipSpc
 cmp.b #'#',(A3)+
 bne.s bit_reg_mode
 bsr GetNum
 move.l Addr(A5),A0
 move.w D0,(A0)+
 move.l A0,Addr(A5)
 bset #11,D3
 bra.s bit_get_ea
bit_reg_mode:
 cmp.b #'d',-1(A3)
 bne.s bits_err
 bsr GetNum_0_7
 bmi.s bits_err
 lsl.w #8,D0
 lsl.w #1,D0
 or.w D0,D3
 bset #8,D3
bit_get_ea:
 bsr SkipComma
 bmi.s bits_err
 bsr GetEA
 cmp.w #1,D1
 beq.s bits_err
 lsl.w #3,D1
 or.w D1,D0
 move.w D3,D1
 and.w #$C0,D1
 beq.s bit_ea_1
 cmp.w #$39,D0
 bhi.s bits_err
bit_ea_1:
 cmp.w #$3B,D0
 bhi.s bits_err
 or.w D3,D0
 bra zzcom
bits_err:
 bra error
*** CHK ***
chk_asm:
 move.w #$4180,D3
 bra.s mul_div1
*** MUL & DIV ***
mul_asm:
 move.w #$C0C0,D3
 bra.s mul_div
div_asm:
 move.w #$80C0,D3
mul_div:
 bsr SkipSpc
 cmp.b #'u',(A3)+
 beq.s mul_div1
 cmp.b #'s',-1(A3)
 bne.s bits_err
 bset #8,D3
mul_div1:
 move.w #1,size(A5)
 addq.l #2,Addr(A5)
 bsr GetEA
 cmp.w #1,D1
 beq.s bits_err
 lsl.w #3,D1
 or.w D1,D0
 or.w D0,D3
 bsr SkipComma
 bmi.s bits_err 
 cmp.b #'d',(A3)+
 bne.s bits_err
 bsr GetNum_0_7
 bmi.s bits_err
 lsl.w #8,D0
 lsl.w #1,D0
 or.w D3,D0
 bra.s zzcom
*** TAS ***
tas_asm:
 addq.l #2,Addr(A5)
 bsr GetEA
 cmp.w #1,D1
 beq.s zz_err
 lsl.w #3,D1
 or.w D1,D0
 cmp.w #$39,D0
 bhi.s zz_err
 or.w #$4AC0,D0
 bra.s zzcom
*** PEA ***
pea_asm:
 move.w #$4840,D3
pp_1:
 addq.l #2,Addr(A5)
 bsr GetEA
 cmp.w #2,D1
 beq.s pea_A_ok
 cmp.w #5,D1
 bcs.s zz_err
pea_A_ok: 
 lsl.w #3,D1
 or.w D1,D0
 cmp.w #$3C,D0
 bcc.s zz_err
 or.w D3,D0
 bra.s zzcom
*** LEA ***
lea_asm:
 addq.l #2,Addr(A5)
 bsr GetEA
 cmp.w #2,D1
 beq.s lea_A_ok
 cmp.w #5,D1
 bcs.s zz_err
lea_A_ok:
 lsl.w #3,D1
 or.w D0,D1
 cmp.w #$3C,D1
 bcc.s zz_err
 bsr SkipComma
 bmi.s zz_err
 cmp.b #'a',(A3)+
 bne.s zz_err
 bsr GetNum_0_7
 bmi.s zz_err
 lsl.w #8,D0
 lsl.w #1,D0
 or.w D1,D0
 or.w #$41C0,D0
zzcom:
 move.l EndAddr(A5),A0
 move.w D0,(A0)
 bra assem9
zz_err:
 bra error
*** EXT ***
ext_asm:
 bsr GetSize
 move.w size(A5),D1
 beq.s zz_err
 addq.w #1,D1
 lsl.w #6,D1
 bsr SkipSpc
 cmp.b #'d',(A3)+
 bne.s zz_err
 bsr GetNum_0_7
 bmi.s zz_err
 or.w D1,D0
 or.w #$4800,D0
 bra one_word_instr
*** JMP ***
jmp_asm:
 move.w #$4EC0,D3
 bra pp_1
*** JSR ***
jsr_asm:
 move.w #$4E80,D3
 bra pp_1
*** NBCD ***
nbcd_asm:
 addq.l #2,Addr(A5)
 clr.w size(A5)
 move.w #$4800,D3
 bra.s one_arg_2
*** CLR ***
clr_asm:
 move.w #$4200,D3
one_arg_com:
 addq.l #2,Addr(A5)
 bsr GetSize
one_arg_2:
 bsr GetEA
 cmp.w #1,D1
 beq.s qlumps_err
 lsl.w #3,D1
 or.w D1,D0
 cmp.w #$39,D0 ;check dest addr mode (no pcrel)
 bhi.s qlumps_err
 move.w size(A5),D1
 lsl.w #6,D1
 or.w D1,D0
 or.w D3,D0
 move.l EndAddr(A5),A0
 move.w D0,(A0)
 bra assem9
*** NEG & NEGX ***
neg_asm:
 cmp.b #'x',(A3)
 bne.s no_negx
 addq.l #1,A3
 move.w #$4000,D3
 bra.s one_arg_com
qlumps_err:
 bra error
no_negx:
 move.w #$4400,D3
 bra.s one_arg_com
*** NOT ***
not_asm:
 move.w #$4600,D3
 bra.s one_arg_com
*** TST ***
tst_asm:
 move.w #$4A00,D3
 bra.s one_arg_com
*** SWAP ***
swap_asm:
 bsr SkipSpc
 cmp.b #'d',(A3)+
 bne.s qlumps_err
 bsr GetNum_0_7
 bmi.s qlumps_err
 or.w #$4840,D0
 bra.s one_word_instr
*** RESET ***
reset_asm:
 move.w #$4E70,D0
 bra.s one_word_instr
*** NOP ***
nop_asm:
 move.w #$4E71,D0
 bra.s one_word_instr
*** RTE ***
rte_asm:
 move.w #$4E73,D0
 bra.s one_word_instr
*** RTS ***
rts_asm:
 move.w #$4E75,D0
 bra.s one_word_instr
*** TRAPV ***
trapv_asm:
 move.w #$4E76,D0
 bra.s one_word_instr
*** RTR ***
rtr_asm:
 move.w #$4E77,D0
one_word_instr:
 move.l Addr(A5),A0
 move.w D0,(A0)+
 move.l A0,Addr(A5)
 bra assem9
*** ILLEGAL ***
illegal_asm:
 move.w #ILLEGAL,D0
 bra.s one_word_instr
*** TRAP ***
trap_asm:
 bsr SkipSpc
 cmp.b #'#',(A3)+
 beq.s trap_asm_1
 subq.l #1,A3
trap_asm_1:
 bsr GetNum
 and.w #$0f,D0
 or.w #$4E40,D0
 bra.s one_word_instr
*** UNLK ***
unlk_asm:
 bsr SkipSpc
 cmp.b #'a',(A3)+
 bne.s zump_err
 bsr GetNum_0_7
 bmi.s zump_err
 or.w #$4E58,D0
 bra.s one_word_instr
*** LINK ***
link_asm:
 bsr SkipSpc
 cmp.b #'a',(A3)+
 bne.s zump_err
 bsr GetNum_0_7
 bmi.s zump_err
 move.w D0,D3
 bsr SkipComma
 bne.s zump_err
 cmp.b #'#',(A3)+
 bne.s zump_err
 bsr GetNum
 move.w D0,D1
 move.w D3,D0
 or.w #$4E50,D0
 bra.s two_words_instr
*** STOP ***
stop_asm:
 bsr SkipSpc
 cmp.b #'#',(A3)+
 bne.s zump_err
 bsr GetNum
 move.w D0,D1
 move.w #$4E72,D0
two_words_instr:
 move.l Addr(A5),A0
 move.w D0,(A0)+
 move.w D1,(A0)+
 move.l A0,Addr(A5)
 bra assem9
zump_err:
 bra error
*** EXG ***
exg_asm:
 bsr getreg
 bmi.s zump_err
 bsr SkipComma
 bmi.s zump_err
 swap D1
 bsr getreg
 bmi.s zump_err
 swap D1
 btst #16+3,D1
 bne.s no_exg_1
 btst #3,D1
 beq.s no_exg_1
 swap D1
no_exg_1:
 move.l D1,D0
 move.l D1,D3
 lsr.l #7,D0
 and.w #$0E00,D0
 and.w #$07,D1
 or.w D1,D0
 or.w #$C100,D0
 move.l D3,D2
 swap D2
 eor.w D3,D2
 btst #3,D2
 bne.s data_and_addr
 bset #6,D0
 btst #3,D3
 beq.s exg_9
 bset #3,D0
 bra.s exg_9
data_and_addr:
 or.w #$88,D0
exg_9:
 bra one_word_instr

*** GET REGISTER LIST (for MOVEM) ***
getreglist:
 clr.w D2
reglist1:
 bsr.s getreg
 bmi.s reglist9
 bset D1,D2
 cmp.b #'/',(A3)+
 beq.s reglist1
 cmp.b #'-',-1(A3)
 bne.s reglist9
 move.w D1,D7
 bsr.s getreg
 bmi Num_0_7_err
 move.w D1,D0
 eor.w D7,D0
 btst #3,D0
 bne.s Num_0_7_err ;register in range are different types
 cmp.w D1,D7
 bhi.s Num_0_7_err ;register range out of order
 beq.s reg_r1
regrange:
 addq.w #1,D7
 bset D7,D2
 cmp.w D1,D7
 bne.s regrange
reg_r1:
 cmp.b #'/',(A3)+
 beq.s reglist1
reglist9:
 subq.l #1,A3
 moveq #0,D0
 rts ;register mask returned in D2

*** GET ONE REGISTER, number in D1, 0=D0..15=A7 ***
* return -1 if error
getreg:
 clr.w D1
 bsr SkipSpc
 cmp.b #'d',(A3)+
 beq.s g_r_1
 cmp.b #'a',-1(A3)
 bne.s Num_0_7_err
 bset #3,D1
g_r_1:
 bsr.s GetNum_0_7
 bmi.s g_r_2
 or.b D0,D1
g_r_2:
 rts ;register returned in D1

*** Get a single digit in range 0..7, used as a register number
* return -1 if error
GetNum_0_7:
 move.b (A3)+,D0
 sub.b #'0',D0
 bcs.s Num_0_7_err
 cmp.b #8,D0
 bcc.s Num_0_7_err
 ext.w D0
 rts

*** Get a number in range 1..8, used by 'quick' instructions ***
* return -1 if error
GetNum_1_8:
 movem.l D1-D2,-(sp)
 bsr GetNum
 movem.l (sp)+,D1-D2
 swap D0
 tst.w D0
 bne.s Num_0_7_err
 swap D0
 tst.w D0
 beq.s Num_0_7_err
 cmp.w #8,D0
 bhi.s Num_0_7_err
 and.w #7,D0
 rts

*** SKIP COMMA & SPACES AROUND IT ***
* if first non-space character is not a comma, return -1 else return 0
SkipComma:
 bsr SkipSpc
 cmp.b #',',(A3)+
 beq.s S_K_1
Num_0_7_err:
 moveq #-1,D0
 rts
S_K_1:
 bsr SkipSpc
 moveq #0,D0
 rts

GetEA: ;get effective address, mode=D1,reg=D0
* put displacements etc. in memory at address pointed by Addr(A5)
* and increment Addr(A5)
 bsr SkipSpc
 move.b (A3),D0
 cmp.b #'d',D0
 bne.s no_data_direct 
 moveq #0,D1
regdir1:
 addq.l #1,A3
 bsr.s GetNum_0_7
 bmi ea_error
 rts 
no_data_direct:
 cmp.b #'a',D0
 bne.s no_addr_direct
 moveq #1,D1
 bra.s regdir1
no_addr_direct:
 cmp.b #'(',D0
 bne.s no_indirect_or_postincrement
 cmp.b #'a',1(A3)
 bne ea_error
 addq.l #2,A3
 bsr GetNum_0_7
 bmi ea_error
 cmp.b #')',(A3)+
 bne ea_error
 cmp.b #'+',(A3)
 beq.s postincrement
 moveq #2,D1
 rts
postincrement:
 addq.l #1,A3
 moveq #3,D1
 rts
no_indirect_or_postincrement:
 cmp.b #'-',D0
 bne.s no_predecrement
 cmp.b #'(',1(A3)
 bne.s no_predecrement
 addq.l #2,A3
 cmp.b #'a',(A3)+
 bne ea_error
 bsr GetNum_0_7
 bmi ea_error
 cmp.b #')',(A3)+
 bne ea_error
 moveq #4,D1
 rts
no_predecrement:
 cmp.b #'#',D0
 bne.s no_immediate
 addq.l #1,A3
PutImm:
 bsr GetNum
 move.l Addr(A5),A0
 move.w size(A5),D1
 bne.s sz1
 and.w #$FF,D0
 move.w D0,(A0)+
 bra.s sz9
sz1:
 cmp.w #1,D1
 bne.s sz2
 move.w D0,(A0)+
 bra.s sz9
sz2:
 move.l D0,(A0)+
sz9:
 move.l A0,Addr(A5)
 moveq #7,D1
 moveq #4,D0
 rts
no_immediate:
 bsr GetNum
 move.l D0,D2
 move.b (A3),D0
 cmp.b #'(',D0
 bne absolute_mode
 addq.l #1,A3
 move.b (A3)+,D0
 cmp.b #'a',D0
 bne.s no_displacement_or_index
 bsr GetNum_0_7
 bmi ea_error
 cmp.b #',',(A3)+
 beq.s indirect_with_index
 cmp.b #')',-1(A3)
 bne ea_error
 move.l Addr(A5),A0
 move.w D2,(A0)+
 move.l A0,Addr(A5)
 moveq #5,D1
 rts
indirect_with_index:
 move.l D0,D7
 bsr GetIndex
 move.l D7,D0
 moveq #6,D1
 rts
no_displacement_or_index:
 cmp.b #'p',D0
 bne.s pc_rel_with_index
 cmp.b #'c',(A3)+
 bne.s ea_error
 cmp.b #')',(A3)+
 bne.s ea_error
 sub.l Addr(A5),D2
 move.l Addr(A5),A0
 move.w D2,(A0)+
 move.l A0,Addr(A5)
 swap D2
 tst.w D2
 beq.s pcrel_ok
 not.w D2
 bne.s ea_error
pcrel_ok:
 moveq #7,D1
 moveq #2,D0
 rts
pc_rel_with_index:
 subq.l #1,A3
 sub.l Addr(A5),D2
 bsr.s GetIndex
 moveq #7,D1
 moveq #3,D0
 rts
absolute_mode:
 move.w D2,D1
 ext.l D1
 cmp.l D2,D1
 beq.s abs_short
 move.l Addr(A5),A0
 move.l D2,(A0)+
 move.l A0,Addr(A5)
 moveq #7,D1
 moveq #1,D0
 rts
abs_short:
 move.l Addr(A5),A0
 move.w D2,(A0)+
 move.l A0,Addr(A5)
 moveq #7,D1
 moveq #0,D0
 rts
ea_error:
 addq.l #4,sp
 bra error

*** GET SIZE (put it in size(A5), 0=B, 1=W, 2=L) ***
GetSize:
 cmp.b #'.',(A3)+
 bne.s ea_error
 move.b (A3)+,D0
 cmp.b #'b',D0
 bne.s siz1
 clr.w size(A5)
 rts
siz1:
 cmp.b #'w',D0
 bne.s siz2
 move.w #1,size(A5)
 rts
siz2:
 cmp.b #'l',D0
 bne.s ea_error
 move.w #2,size(A5)
 rts

GetIndex:
 moveq #0,D1
 cmp.b #'d',(A3)+
 beq.s index_1
 cmp.b #'a',-1(A3)
 bne.s index_error
 bset #15,D1
index_1:
 bsr GetNum_0_7
 bmi.s index_error
 lsl.w #8,D0
 lsl.w #4,D0
 or.w D0,D1
 and.w #$FF,D2
 or.w D2,D1
 cmp.b #'.',(A3)+
 bne.s index_error
 cmp.b #'w',(A3)+
 beq.s index_2
 cmp.b #'l',-1(A3)
 bne.s index_error
 bset #11,D1
index_2:
 cmp.b #')',(A3)+
 bne.s index_error
 move.l Addr(A5),A0
 move.w D1,(A0)+
 move.l A0,Addr(A5)
 rts
index_error:
 addq.l #8,sp
 bra error

skipequal: ;the syntax of the register change command can include a '=' sign
 cmp.b #'=',(A3) ;but it is not necessary
 bne.s skeq1
 addq.l #1,A3
skeq1:
 rts
*** DISPLAY & CHANGE REGISTERS ***
regs:
 bsr SkipSpc
 tst.b (A3)
 bne.s changeregs
 bsr displayregs
 bra mainloop
changeregs:
 move.b (A3)+,D0 ;** CHANGE REGISTERS **
 bsr tolower
 lsl.w #8,D0
 move.b (A3)+,D0 ;get possibly unaligned word
 bsr tolower
 cmp.w #'pc',D0  ;program counter
 bne.s nopc
 bsr.s skipequal
 bsr GetNum
 move.l D0,regPC
 bra mainloop
nopc:
 cmp.w #'cc',D0 ;condition code register
 bne.s nocc
 bsr.s skipequal
 bsr GetNum
 move.b D0,regCC
 bra mainloop
nocc:
 cmp.w #'d0',D0
 bcs.s nodr
 cmp.w #'d7',D0
 bhi.s nodr
 sub.w #'d0',D0
 lsl.w #2,D0
 move.w D0,D2
 bsr.s skipequal
 bsr GetNum
 lea DataRegs,A0
 move.l D0,0(A0,D2.W)
 bra mainloop
nodr:
 cmp.w #'a0',D0
 bcs error
 cmp.w #'a7',D0
 bhi error
 sub.w #'a0',D0
 lsl.w #2,D0
 move.w D0,D2
 bsr skipequal
 bsr GetNum
 lea AddrRegs,A0
 move.l D0,0(A0,D2.W)
 bra mainloop

*** DISPLAY REGISTERS ***
displayregs:
 startline
 move.l #' PC=',(A3)+
 move.l regPC,D0
 bsr PutHexLong
 move.l #' CC=',(A3)+
 move.b regCC,D0
 move.b D0,D2
 bsr PutHexByte
 moveq #4,D1
flagloop:
 move.b #' ',(A3)+
 move.b flagstring(D1.L),(A3)+
 move.b #'=',(A3)+
 moveq #'0',D0
 btst D1,D2
 beq.s flag1
 addq.b #1,D0
flag1:
 move.b D0,(A3)+
 subq.l #1,D1
 bpl.s flagloop
 endline
 move.l A0,A3
 bsr StrOutRedirect
 lea DataRegs,A2
 move.l #' D0=',D2
 bsr.s PrintLineOfRegs
 bsr.s PrintLineOfRegs
 move.l #' A0=',D2
 bsr PrintLineOfRegs ;NOTE: Can't use short branch to branch ZERO bytes
PrintLineOfRegs:
 moveq #3,D1
 move.l A3,A0
regl1:
 move.l D2,(A3)+
 move.l (A2)+,D0
 bsr PutHexLong
 add.w #$0100,D2
 dbf D1,regl1
 move.w #$0A00,(A3)
 move.l A0,A3
 bra StrOutRedirect

flagstring dc.b 'CVZNX'
 ds.w 0

*** BREAKPOINTS ***
breaks:
 bsr SkipSpc
 move.b (A3)+,D0
 bsr tolower
 cmp.b #'l',D0
 beq break_list
 cmp.b #'r',D0
 beq.s break_remove

*** SET BREAKPOINT ***
 subq.l #1,A3
 bsr GetNum
 tst.l D0
 beq.s brk_err
 btst #0,D0
 bne.s brk_err
 move.l D0,A4
 bsr find_break
 bpl.s brk_err
 move.l A1,D3
 moveq #brk_SIZE,D0
 move.l #MEMF_CLEAR,D1
 linkexec AllocMem
 tst.l D0
 beq.s brk_err
 move.l D0,A0
 move.l A4,brk_Address(A0)
 tst.l D3
 bne.s no_start_of_list
 move.l BreakList(A5),(A0)
 move.l A0,BreakList(A5)
 bra.s brset9
no_start_of_list:
 move.l D3,A1
 move.l (A1),(A0)
 move.l A0,(A1)
brset9:
 bra mainloop
brk_err:
 bra error

*** REMOVE BREAKPOINT ***
break_remove:
 bsr SkipSpc
 cmp.b #'a',(A3) ;check 'all'
 bne.s break_rem1
 moveq #'l',D0
 cmp.b 1(A3),D0
 bne.s break_rem1
 cmp.b 2(A3),D0
 bne.s break_rem1
 bsr remove_all_breaks
 bra.s brset9
break_rem1:
 bsr GetNum
 bsr.s find_break
 bmi.s brk_err
 move.l A1,D0
 bne.s no_remove_from_start_of_list
 move.l (A0),BreakList(A5)
 bra.s break_remove_1
no_remove_from_start_of_list:
 move.l (A0),(A1)
break_remove_1:
 move.l A0,A1
 moveq #brk_SIZE,D0
 linkexec FreeMem
 bra.s brset9

*** LIST BREAKPOINTS ***
* note: the list is automatically in order
break_list:
 move.l BreakList(A5),D2
 bne.s break_list_1
 lea noBrkTxt(pc),A0
 bsr StrOutWindow
 bra mainloop
break_list_1:
 lea brklistTx(pc),A0
 bsr StrOutRedirect
break_list_loop:
 tst.l D2
 beq.s brset9
 move.l D2,A4
 startline
 move.b #'$',(A3)+
 move.l brk_Address(A4),D0
 bsr PutHexLong
 endline
 bsr StrOutRedirect
 bsr CheckKeys
 bne brset9
 move.l (A4),D2
 bra.s break_list_loop

*** FIND BREAKPOINT FROM LINKED LIST ***
* address in D0
* if N=1 then not found, A1 points to where new node should be inserted
*  if A1=0 then insert to start of the list
* if N=0 then found, A0 points to the node, A1 points to predecessor
find_break:
 sub.l A1,A1 ;A1=0
 move.l BreakList(A5),D1
find_br_1:
 beq.s break_not_found
 move.l D1,A0
 cmp.l brk_Address(A0),D0
 beq.s break_found
 bcs.s break_not_found
 move.l A0,A1
 move.l (A0),D1
 bra.s find_br_1
break_not_found:
 moveq #-1,D0
 rts
break_found:
 moveq #0,D0
brk_ret:
 rts

remove_all_breaks:
* executed before exit of when the 'br all' command is given
 move.l BreakList(A5),D2
all_breaks_loop:
 tst.l D2
 beq.s brk_ret2
 move.l D2,A1
 move.l (A1),D2
 moveq #brk_SIZE,D0
 linkexec FreeMem
 bra.s all_breaks_loop
brk_ret2:
 clr.l BreakList(A5)
 rts

*** PUT THE ILLEGAL ($4AFC) INSTRUCTION TO BREAKPOINTS ***
* (first save original contents in the BreakPoint structure)
SetBreaks:
 or.w #1,flags(A5)
 move.l BreakList(A5),D2
SetBr1:
 tst.l D2
 beq.s brk_ret
 move.l D2,A1
 move.l brk_Address(A1),A0
 cmp.l oldPC,A0
 beq.s SetBr2
 move.w (A0),brk_Content(A1)
 move.w #ILLEGAL,(A0)
SetBr2:
 move.l (A1),D2
 bra.s SetBr1

*** RESTORE ORIGINAL CONTENTS OF BREAKPOINTS ***
RemBreaks:
 move.w flags(A5),D0
 and.w #$FFFE,flags(A5)
 btst #0,D0
 beq.s brk_ret
 move.l BreakList(A5),D2
RemBr1:
 tst.l D2
 beq.s brk_ret
 move.l D2,A1
 move.l brk_Address(A1),A0
 cmp.l oldPC,A0
 beq.s RemBr2
 move.w brk_Content(A1),(A0)
RemBr2:
 move.l (A1),D2
 bra.s RemBr1

**** SINGLE STEP (WALK) ****
* NOTE: This ignores breakpoints
walk:
 bsr SkipSpc
 tst.b (A3)
 beq.s noaddr
 bsr GetNum
 move.l D0,regPC
noaddr:
 movem.l A5-A6,-(sp)
 move.l sp,StackPtr
 move.l AddrRegs+7*4,sp
 or #2,CCR ;set overflow flag
 trapv ;let the trap handler do the rest...
walk_here: ;a label so we can reference it in the handler routine

**** EXECUTE MACHINE CODE (GO) ****
go:
 bsr SkipSpc
 tst.b (A3)
 beq.s go1
 bsr GetNum
 move.l D0,regPC
go1:
 move.l regPC,oldPC
 bsr SetBreaks
 movem.l A5-A6,-(sp)
 move.l sp,StackPtr
 move.l AddrRegs+7*4,sp
 bra.s go_com

**** JUMP TO SUBROUTINE (RETURN WITH RTS) ***
jumpsr:
 bsr SkipSpc
 tst.b (A3)
 beq.s noaddr1
 bsr GetNum
 move.l D0,regPC
noaddr1:
 move.l regPC,oldPC
 bsr SetBreaks
 move.l A7,A0        ;set stack ptr
 sub.w #$100,A0
 move.l A0,AddrRegs+4*7
 movem.l A5-A6,-(sp) ;save frame pointer & DOSBase
 move.l sp,StackPtr
 move.l A0,sp
 lea returncode(pc),A0
 move.l A0,(sp)      ;put return address in stack
go_com:
 move.l regPC,-(sp)
 move.b regCC,D0
 move D0,CCR
 movem.l DataRegs,D0-D7/A0-A6
 rts   ;this really jumps to the user program

*** CONTROLS RETURNS HERE AFTER THE Jsr COMMAND ***
returncode:
 addq.l #4,sp
 movem.l D0-D7/A0-A7,DataRegs
 move.l StackPtr,sp
 move.l _AbsExecBase,A6
 callsys GetCC
 move.b D0,regCC
 movem.l (sp)+,A5-A6 ;restore frame pointer & DOSBase
 bsr RemBreaks
 lea rettx(pc),A0
 bsr StrOutRedirect
 bsr displayregs
 bra mainloop

*** TASK TRAP CODE ***
trapreturn:     ;Note! We are in supervisor mode!
 cmp.l #7,(sp)          ;is this a TRAPV-trap (possibly by the walk routine)
 bne.s normtrap
 cmp.l #walk_here,6(sp) ;check program counter
 bne.s normtrap
 lea 10(sp),sp          ;yes, it was the walk routine, so clean up the stack
 move.l regPC,-(sp)
 move SR,D0             ;this is legal bacause supervisor mode
 move.b regCC,D0
 bclr #13,D0            ;supervisor mode bit off
 bset #15,D0            ;trace mode bit on
 move.w D0,-(sp)
 movem.l DataRegs,D0-D7/A0-A6
 rte       ;back to user mode and user program...
normtrap:
 movem.l D0-D7/A0-A6,DataRegs
 move USP,A0
 move.l A0,AddrRegs+7*4   ;save stack pointer
 move.l _AbsExecBase,A6
 move.w AttnFlags(A6),D1
 move.l (sp)+,D5
 cmp.w #3,D5
 bhi.s pop_SR_and_PC      ;jump if not bus error or address error
 btst #AFB_68010,D1       ;we must check the type of the processor!
 bne.s pop_SR_and_PC
 addq.l #8,sp
pop_SR_and_PC:
 move.w (sp)+,D0          ;status register
 move.b D0,regCC
 move.l (sp)+,regPC
 cmp.w #3,D5
 bhi.s go_user_mode
 btst #AFB_68020,D1
 beq.s no68020
 lea 82(sp),sp
 bra.s go_user_mode
no68020:
 btst #AFB_68010,D1
 beq.s go_user_mode 
 lea 52(sp),sp
go_user_mode:
 and.w #$5FFF,D0          ;clear supervisor & trace bits
 move D0,SR               ;back to the user mode!
 move.l StackPtr,sp       ;this is user stack pointer
 movem.l (sp)+,A5-A6
 bsr RemBreaks
 cmp.w #4,D5
 bne.s normal_trap
 move.l regPC,D0
 bsr find_break
 bne.s normal_trap
 lea brkPtTxt(pc),A0
 bsr StrOutRedirect
 bra.s trap_dregs
normal_trap:
 bsr.s show_trap_name
trap_dregs:
 bsr displayregs
 move.l regPC,D0
 btst #0,D0
 bne.s tr99
 move.l D0,Addr(A5)
 move.l D0,A4
 bsr disasmline
 bsr StrOutRedirect
tr99:
 bra mainloop

show_trap_name: ;trap number in D5
 startline
 move.l #'*** ',(A3)+
 lea trapnamtabl(pc),A1
 move.w D5,D0
 cmp.w #$30,D0
 bcc.s unknown_trap
 cmp.w #$20,D0
 bcc.s traps
 subq.w #2,D0
 cmp.w #10,D0
 bcs.s txout1
unknown_trap:
 moveq #'?',D0
 move.b D0,(A3)+
 move.b D0,(A3)+
 move.b D0,(A3)+
 bra.s notraps
traps:
 moveq #10,D0
txout1:
 add.w D0,D0
 add.w D0,A1
 add.w (A1),A1
 bsr PutStr
 cmp.w #$20,D5
 bcs.s notraps
 move.w D5,D0
 sub.w #32,D0
 bsr PutHexByte1
notraps:
 move.b #' ',(A3)+
 moveq #'*',D0
 move.b D0,(A3)+
 move.b D0,(A3)+
 move.b D0,(A3)+
 endline
 bra StrOutRedirect

*** SHOW TRAP NAME (USAGE: ^ num) ***
showtrap:
 bsr GetNum
 move.l D0,D5
 bsr show_trap_name
 bra mainloop

**** Output a character to the window ****
ChrOut:
 movem.l D2-D3,-(sp)
 move.l winfile(A5),D1
 lea temp(A5),A0
 move.b D0,(A0)
 move.l A0,D2
 moveq #1,D3
 callsys Write
 movem.l (sp)+,D2-D3
 rts

*** Output a string to the window ***
* used in error messages etc.
StrOutWindow:
 movem.l D2-D3,-(sp)
 move.l A0,D2
 moveq #-1,D3
str1:
 addq.l #1,D3
 tst.b (A0)+
 bne.s str1
 move.l winfile(A5),D1
 callsys Write
 movem.l (sp)+,D2-D3
 rts

*** Output a string (possibly redirected output) ***
StrOutRedirect:
 movem.l D2-D3,-(sp)
 move.l A0,D2
 moveq #-1,D3
str2:
 addq.l #1,D3
 tst.b (A0)+
 bne.s str2
 move.l OutputFile(A5),D1
 callsys Write
 movem.l (sp)+,D2-D3
 rts

**** Get a character ****
GetChar:
 movem.l D2-D3/A2,-(sp)
 move.l winfile(A5),D1
 lea temp(A5),A2
 move.l A2,D2
 moveq #1,D3
 callsys Read
 moveq #0,D0
 move.b (A2),D0
 movem.l (sp)+,D2-D3/A2
 rts

GetKey:
* get a word value describing the key pressed
* if high byte is zero, this is the ASCII-code of the key
* else it is a special code or -1 if key is not recognised
* by this routine (for example the function keys)
 bsr.s GetChar
 cmp.b #CSI,D0
 bne.s ret9
 bsr.s GetChar
 cmp.b #'A',D0
 bcs.s key1
 cmp.b #'D',D0
 bhi.s key1
 sub.b #$40,D0
 asl.w #8,D0
 rts ;codes $0100..$0400 ;  cursor up/down/right/left
key1:
 cmp.b #'S',D0
 bne.s key2
 move.w #SHIFT_CURSOR_DOWN,D0
 rts
key2:
 cmp.b #'T',D0
 bne.s key3
 move.w #SHIFT_CURSOR_UP,D0
 rts
key3:
 cmp.b #' ',D0
 bne.s key8
 bsr.s GetChar
 cmp.b #'A',D0
 bne.s key4
 move.w #SHIFT_CURSOR_LEFT,D0
 rts
key4:
 cmp.b #'@',D0
 bne.s key9
 move.w #SHIFT_CURSOR_RIGHT,D0
 rts
key8:
 cmp.b #'?',D0
 bne.s key9
 bsr GetChar
 move.w #'??',D0 ;THE HELP KEY
 rts
key9:
 cmp.b #'~',D0
 beq.s key99
 bsr GetChar
 bra.s key9
key99:
 moveq #-1,D0 ;unknown key
ret9:
 rts

**** Output a number (range 0..99) ****
* used in cursor moving escape sequences
NumOut:
 lea temp(A5),A0
 divu #10,D0
 add.b #'0',D0
 move.b D0,(A0)
 swap D0
 add.b #'0',D0
 move.b D0,1(A0)
 clr.b 2(A0)
 bra StrOutWindow

**** THE INPUT ROUTINE ****
* special code in D0
* 0 = normal operation
* 1 = respond ctrl-e (assembler)
* 2 = edit existing line (created by disassembler)
* returns the special code or -1 if Ctrl-E pressed
*********
GetInput:
 move.w D0,inpspecial(A5)
 moveq #0,D4
 move.l outbufadr(A5),A4
 lea (InpBuf-OutBuf)(A4),A4
 moveq #0,D5
 moveq #0,D7     ;length
 cmp.w #2,D0
 bne.s inp0
 emit NCSI
 emit 'K' ;erase to end of line
 move.l A4,A0
 bsr StrOutWindow
 move.l A4,A0
inp0len:
 tst.b (A0)+
 bne.s inp0len
 sub.l A4,A0
 subq.l #1,A0
 move.l A0,D7
inp0:
 move.l D7,D6 ;current position
inp1:
 bsr GetKey
 cmp.w #CR,D0    ;return
 beq inp9
 cmp.w #CtrlE,D0
 bne.s noCtrlE
 tst.w inpspecial(A5)
 beq.s noCtrlE
 bsr eraseline
 move.w #-1,inpspecial(A5)
 bra inp9
noCtrlE:
 cmp.w #CtrlX,D0 ;Ctrl-x clears the input line
 bne.s noCtrlX
 bsr eraseline
 bra.s inp1
noCtrlX:
 cmp.w #CURSOR_RIGHT,D0
 beq moveright
 cmp.w #CURSOR_LEFT,D0 ;cursor left
 beq moveleft
 cmp.w #SHIFT_CURSOR_LEFT,D0
 bne.s noleftedge
 bsr gotoleftedge
 bra.s inp1
noleftedge:
 cmp.w #'??',D0
 bne.s nohelp
 moveq #-1,D5
 bra.s put_char_to_input
nohelp:
 cmp.w #SHIFT_CURSOR_UP,D0
 bne.s no_do_prev
 moveq #-1,D5
 bra previousline
no_do_prev: 
 cmp.w #SHIFT_CURSOR_RIGHT,D0
 beq rightedge
 cmp.w #CURSOR_UP,D0 ;cursor up--previous input line
 beq previousline
 cmp.w #CURSOR_DOWN,D0
 beq nextline
 cmp.w #BS,D0    ;backspace
 beq backspace
 cmp.w #DEL,D0   ;delete
 beq delchar
 cmp.w #SPACE,D0
 bcs inp1
 cmp.w #DEL,D0
 bcs.s put_char_to_input
 cmp.w #$A0,D0
 bcs inp1
 cmp.w #-1,D0
 beq inp1
put_char_to_input:
 cmp.w #64,D7
 bcc inp1
 cmp.l D7,D6
 bhi inp1
 beq.s putchr
 move.l D7,D2
insloop:
 move.b -1(A4,D2.L),0(A4,D2.L)
 subq.l #1,D2
 cmp.l D2,D6
 bne.s insloop
putchr:
 move.b D0,D2
 emit NCSI
 emit '@'
 move.b D2,D0
 bsr ChrOut
 move.b D2,0(A4,D6.L)
 addq.l #1,D6
 addq.l #1,D7
 tst.l D5 ;auto-CR flag
 beq inp1
inp9:
 clr.b 0(A4,D7.L)
 move.l A4,A3
 bsr SkipSpc
 tst.b (A3)
 beq.s inp99
 lea oldline,A0
 move.w #(NLINES-1)*LEN/4-1,D0
scrl1:
 move.l LEN(A0),(A0)+
 dbf D0,scrl1 ;scroll command line history to make space for current line
copyloop:
 move.b (A4)+,D0
 move.b D0,(A0)+ ;add current line to command line history
 bne.s copyloop
inp99:
 tst.w inpspecial(A5)
 bmi.s inp99a
 emit LF
inp99a:
 move.w inpspecial(A5),D0
 rts
rightedge: ;Shift-Cursor right
 cmp.l D6,D7
 beq inp1
 emit NCSI
 move.b D7,D0
 sub.b D6,D0
 bsr NumOut
 emit 'C'
 move.l D7,D6
 bra inp1
moveleft:
 tst.l D6
 beq inp1
 subq.l #1,D6
 emit BS
 bra inp1
moveright:
 cmp.l D6,D7
 beq inp1
 addq.l #1,D6
 emit NCSI
 emit 'C'
 bra inp1
backspace:
 tst.l D6
 beq inp1
 subq.l #1,D6
 emit BS
delchar:
 cmp.l D6,D7
 beq inp1
 emit NCSI
 emit 'P'
 subq.l #1,D7
 cmp.l D6,D7
 beq inp1
 move.l D6,D0
del1:
 move.b 1(A4,D0.L),0(A4,D0.L)
 addq.l #1,D0
 cmp.l D0,D7
 bne.s del1
 bra inp1
*** COMMAND LINE HISTORY ***
nextline:
 bsr.s eraseline
 moveq #1,D1
 bra.s prnxl1
previousline:
 bsr.s eraseline
 moveq #-1,D1
prnxl1:
 add.b D1,D4
 bmi.s linct_neg
 cmp.b #NLINES,D4
 bcs.s linct_ok
 moveq #0,D4
 bra.s linct_ok
linct_neg:
 move.b #NLINES-1,D4
linct_ok:
 move.b D4,D0
 lea oldline,A0
 ext.w D0
 mulu #LEN,D0
 add.l D0,A0
prevloop:
 move.b (A0)+,D0
 beq.s prev2
 move.b D0,0(A4,D7.L)
 addq.l #1,D7
 bra.s prevloop
prev2:
 clr.b 0(A4,D7.L)
 move.l A4,A0
 bsr StrOutWindow
 move.l D7,D6
 tst.l D5
 bne inp9
 bra inp1
gotoleftedge: ;Shift-Cursor left
 tst.l D6
 beq.s lef9
 emit NCSI
 move.b D6,D0
 bsr NumOut
 emit 'D' ;move cursor left with 'CSI nn D'
lef9:
 moveq #0,D6
 rts
eraseline:
 bsr.s gotoleftedge
 moveq #0,D7
 emit NCSI
 moveq #'K',D0
 bra ChrOut ;bsr/rts

*** Hexadecimal output with the $ sign ***
PutHexByte1:
 move.b #'$',(A3)+
 bra.s PutHexByte
PutHexWord1:
 move.b #'$',(A3)+
 bra.s PutHexWord
PutHexLong1:
 move.b #'$',(A3)+
**** Hexadecimal output ****
PutHexLong:
 move.w D0,-(sp)
 swap D0
 bsr.s PutHexWord
 move.w (sp)+,D0
PutHexWord:
 move.b D0,-(sp)
 lsr #8,D0
 bsr.s PutHexByte
 move.b (sp)+,D0
PutHexByte:
 move.b D0,-(sp)
 lsr #4,D0
 bsr.s PutHexNibble
 move.b (sp)+,D0
PutHexNibble:
 and.b #$0f,D0
 cmp.b #10,D0
 bcs.s hex1
 add.b #'A'-'0'-10,D0
hex1:
 add.b #'0',D0
 move.b D0,(A3)+
 rts

*** SIGNED DECIMAL NUMBER OUTPUT ***
PutNumSign:
 tst.l D0
 bpl.s PutNum
 neg.l D0
 move.b #'-',(A3)+
*** DECIMAL NUMBER OUTPUT ***
PutNum:
 move.l D2,-(sp)
 moveq #-1,D2
pnl1: ; 32 bit division
 move.w D0,-(sp) ;save low word
 clr.w D0
 swap D0 ;D0.L == high word
 divu #10,D0
 move.w D0,D1 ;D1.W == quotient high
 move.w (sp)+,D0
 divu #10,D0
 swap D1
 move.w D0,D1
 swap D0
 add.b #'0',D0
 move.b D0,-(sp)
 move.l D1,D0
 addq.w #1,D2
 tst.l D0
 bne.s pnl1
pnl2:
 move.b (sp)+,(A3)+
 dbf D2,pnl2
 move.l (sp)+,D2
ret99:
 rts

*** PUT LONGWORD (to possibly odd address) ***
PutLong:
 swap D0
 move.b D0,-(sp)
 lsr.w #8,D0
 move.b D0,(A3)+
 move.b (sp)+,(A3)+
 swap D0
 move.b D0,-(sp)
 lsr.w #8,D0
 move.b D0,(A3)+
 move.b (sp)+,(A3)+
 rts

*** STRING OUTPUT ***
PutStr: ;string pointer in A1
 tst.b (A1)
 beq.s ret99
 move.b (A1)+,(A3)+
 bra.s PutStr

*** CREATE MESSAGE PORT ***
* no name, priority 0
MyCreatePort:
 movem.l D2/A6,-(sp)
 move.l _AbsExecBase,A6
 moveq #-1,D0
 callsys AllocSignal
 moveq #-1,D1
 cmp.l D0,D1
 beq.s CrepFail
 move.l D0,D2
 moveq #MP_SIZE,D0
 move.l #MEMF_PUBLIC!MEMF_CLEAR,D1
 callsys AllocMem
 tst.l D0
 bne.s Crep1
 move.b D2,D0
 callsys FreeSignal
 bra.s CrepFail
Crep1:
 move.l D0,-(sp)
 sub.l A1,A1
 callsys FindTask
 move.l D0,A1
 move.l (sp)+,D0
 move.l D0,A0
 move.l A1,MP_SIGTASK(A0)
 move.b D2,MP_SIGBIT(A0)
 move.b #NT_MSGPORT,LN_TYPE(A0)
 move.b #PA_SIGNAL,MP_FLAGS(A0)
 lea MP_MSGLIST(A0),A0
 NEWLIST A0
 bra.s Crep9
CrepFail:
 moveq #0,D0
Crep9:
 movem.l (sp)+,D2/A6 ;port addr in D0 or zero if failed
 rts

*** DELETE MESSAGE PORT ***
* no name (not a public port)
MyDeletePort: ;port addr in A1
 movem.l A2-A6,-(sp)
 move.l _AbsExecBase,A6
 moveq #0,D0
 move.b MP_SIGBIT(A1),D0
 move.l A1,A2
 callsys FreeSignal
 move.l A2,A1
 moveq #MP_SIZE,D0
 callsys FreeMem
 movem.l (sp)+,A2-A6
 rts

*** CREATE IO REQUEST ***
MyCreateIO: ;port addr in A1, size in D0
 movem.l D0/A1,-(sp)
 move.l #MEMF_PUBLIC!MEMF_CLEAR,D1
 linkexec AllocMem
 movem.l (sp)+,D1/A1
 tst.l D0
 beq.s CreIO9 ;no memory
 move.l D0,A0
 move.l A1,MN_REPLYPORT(A0)
 move.b #NT_MESSAGE,LN_TYPE(A0)
 move.w D1,MN_LENGTH(A0)
CreIO9:
 rts

*** DELETE IO REQUEST ***
MyDeleteIO: ;IoRequest In A1
 moveq #0,D0
 move.w MN_LENGTH(A1),D0
 linkexec FreeMem
 rts

**** Command & address tables ****
comtable dc.b '?ioxmfthc:arbgjwlsnud()&[]<>!=#\^',$FF
 ds.w 0
comadrs:
 relword help
 relword info
 relword redirect
 relword exit
 relword memdisplay
 relword memfill
 relword memtransfer
 relword memhunt
 relword memcomp
 relword modifymem
 relword assemble
 relword regs
 relword breaks
 relword go
 relword jumpsr
 relword walk
 relword loadseg
 relword show
 relword number
 relword unloadseg
 relword disassem
 relword allocate_mem
 relword free_mem
 relword alloc_abs
 relword abs_load
 relword abs_save
 relword disk_read
 relword disk_write
 relword digisound
 relword block_check
 relword boot_check
 relword new_cli
 relword showtrap

**** HELP TEXT ****
helptext dc.b CLS,TAB,TAB,'-- Amiga Monitor Help (version '
 VERSION
 dc.b ') --',LF,LF
 dc.b '?             : help (this)      '
 dc.b '| [ addr name         : load absolute',LF
 dc.b 'x             : exit             '
 dc.b '| ] addr1 length name : save absolute',LF
 dc.b 'o [name]      : redirect output  '
 dc.b '| < addr dr block cnt : read disk blocks',LF
 dc.b 'dir [name]    : directory        '
 dc.b '| > addr dr block cnt : write disk blocks',LF
 dc.b 'cd [name]     : current dir      '
 dc.b '| del name            : delete file',LF
 dc.b 'cls           : clear screen     '
 dc.b '| \                   : new CLI',LF
 dc.b 'l name        : load segment     '
 dc.b '| ! addr length period: play digisound',LF
 dc.b 'sl            : segment list     '
 dc.b '| = addr              : disk block checksum',LF
 dc.b 'u             : unload segment   '
 dc.b '| # addr              : bootblock checksum',LF
 dc.b 'r             : show registers   '
 dc.b '| g [addr]            : execute (go)',LF
 dc.b 'r reg=num     : change register  '
 dc.b '| j [addr]            : jump to subroutine',LF
 dc.b 'a addr        : assemble         '
 dc.b '| w [addr]            : single step (walk)',LF
 dc.b 'd addr1 addr2 : disassemble      '
 dc.b '| ( length            : allocate memory',LF
 dc.b 'm addr1 addr2 : display memory   '
 dc.b '| & addr length       : allocate absolute',LF
 dc.b ': addr bytes  : modify memory    '
 dc.b '| ) addr/all          : free memory',LF
 dc.b 'b addr        : set breakpoint   '
 dc.b '| sm                  : show allocated mem',LF
 dc.b 'bl            : list breakpoints '
 dc.b '| c addr1 addr2 dest  : compare memory',LF
 dc.b 'br addr/all   : remove breakpoint'
 dc.b '| t addr1 addr2 dest  : transfer memory',LF
 dc.b 'f addr1 addr2 bytes : fill mem   '
 dc.b '| h addr1 addr2 bytes : hunt memory',LF
 dc.b 'n num         : display number in hex & decimal & octal & binary',LF,0

**** disassembler data ****
 ds.w 0
disadrtabl:
 relword handlezero   ;0 -- immediate, bit manipulatios, movep
 relword movebyte     ;1 -- move.b
 relword movelong     ;2 -- move.l
 relword moveword     ;3 -- move.w
 relword handlefour   ;4 -- misc.
 relword handle_five  ;5 -- addq & subq , DBcc, Scc
 relword branch       ;6 -- branch instructions
 relword movequick    ;7 -- moveq #x,Dn
 relword handle_eight ;8 -- or, div, sbcd
 relword subtract     ;9 -- sub & suba
 relword lineA        ;10 Line-A -- unimplemented
 relword handle_11    ;11 -- cmp, cmpm, eor
 relword handle_12    ;12 -- and, mul, abcd, exg
 relword addinst      ;13 -- add & adda
 relword shifts       ;14 -- asl, asr, lsl, lsr, rol, ror, roxl, roxr
 relword lineF        ;15 Line-F -- unimplemented

pcnam    dc.b '(pc)',0  ;program counter
USPnam   dc.b 'USP',0   ;user stack pointer for Move An,USP & Move USP,An
linenam  dc.b 'line-',0 ;for Line-A & Line-F
 ds.w 0
condcodes dc.b 'rasrhilscccsneeqvcvsplmigeltgtle'
*** INSTRUCTION NAMES ***
asnam    dc.b 'as',0
lsnam    dc.b 'ls',0
roxnam   dc.b 'rox',0
rotnam   dc.b 'ro',0
movenam  dc.b 'move',0
addnam   dc.b 'add',0
subnam   dc.b 'sub',0
andnam   dc.b 'and',0
ornam    dc.b 'or',0
abcdnam  dc.b 'abcd',0
sbcdnam  dc.b 'sbcd',0
mulnam   dc.b 'mul',0
divnam   dc.b 'div',0
exgnam   dc.b 'exg',0
eornam   dc.b 'eor',0
cmpnam   dc.b 'cmp',0
btstnam  dc.b 'btst',0
bchgnam  dc.b 'bchg',0
bclrnam  dc.b 'bclr',0
bsetnam  dc.b 'bset',0
chknam   dc.b 'chk',0
leanam   dc.b 'lea',0
extnam   dc.b 'ext',0
clrnam   dc.b 'clr',0
negnam   dc.b 'neg',0
notnam   dc.b 'not',0
tstnam   dc.b 'tst',0
nbcdnam  dc.b 'nbcd',0
swapnam  dc.b 'swap',0
peanam   dc.b 'pea',0
linknam  dc.b 'link',0
unlknam  dc.b 'unlk',0
resetnam dc.b 'reset',0
nopnam   dc.b 'nop',0
stopnam  dc.b 'stop',0
rtenam   dc.b 'rte',0
trapnam  dc.b 'trap',0
rtsnam   dc.b 'rts',0
trapvnam dc.b 'trapv',0
rtrnam   dc.b 'rtr',0
jsrnam   dc.b 'jsr',0
jmpnam   dc.b 'jmp',0
tasnam   dc.b 'tas',0
illegal_nam dc.b 'illegal',0

 ds.w 0
instradrs:
 relword asnam    ;0
 relword lsnam    ;1
 relword roxnam   ;2
 relword rotnam   ;3
 relword movenam  ;4 
 relword addnam   ;5
 relword subnam   ;6
 relword andnam   ;7
 relword ornam    ;8
 relword abcdnam  ;9
 relword sbcdnam  ;10
 relword mulnam   ;11
 relword divnam   ;12
 relword exgnam   ;13
 relword eornam   ;14
 relword cmpnam   ;15
 relword btstnam  ;16
 relword bchgnam  ;17
 relword bclrnam  ;18
 relword bsetnam  ;19
 relword chknam   ;20
 relword leanam   ;21
 relword extnam   ;22
 relword clrnam   ;23
 relword negnam   ;24
 relword notnam   ;25
 relword tstnam   ;26
 relword nbcdnam  ;27
 relword swapnam  ;28
 relword peanam   ;29
 relword linknam  ;30
 relword unlknam  ;31
 relword resetnam ;32
 relword nopnam   ;33
 relword stopnam  ;34
 relword rtenam   ;35
 relword tasnam   ;36
 relword rtsnam   ;37
 relword trapvnam ;38
 relword rtrnam   ;39
 relword jsrnam   ;40
 relword jmpnam   ;41
 relword trapnam  ;42
 relword illegal_nam ;43
 dc.w 0 ;end mark

instrjumps:
 relword as_asm    ;0
 relword ls_asm    ;1
 relword rox_asm   ;2
 relword rot_asm   ;3
 relword move_asm  ;4 
 relword add_asm   ;5
 relword sub_asm   ;6
 relword and_asm   ;7
 relword or_asm    ;8
 relword abcd_asm  ;9
 relword sbcd_asm  ;10
 relword mul_asm   ;11
 relword div_asm   ;12
 relword exg_asm   ;13
 relword eor_asm   ;14
 relword cmp_asm   ;15
 relword btst_asm  ;16
 relword bchg_asm  ;17
 relword bclr_asm  ;18
 relword bset_asm  ;19
 relword chk_asm   ;20
 relword lea_asm   ;21
 relword ext_asm   ;22
 relword clr_asm   ;23
 relword neg_asm   ;24
 relword not_asm   ;25
 relword tst_asm   ;26
 relword nbcd_asm  ;27
 relword swap_asm  ;28
 relword pea_asm   ;29
 relword link_asm  ;30
 relword unlk_asm  ;31
 relword reset_asm ;32
 relword nop_asm   ;33
 relword stop_asm  ;34
 relword rte_asm   ;35
 relword tas_asm   ;36
 relword rts_asm   ;37
 relword trapv_asm ;38
 relword rtr_asm   ;39
 relword jsr_asm   ;40
 relword jmp_asm   ;41
 relword trap_asm  ;42
 relword illegal_asm ;43

**** INFO TEXT ****
infotext dc.b CLS,LF,LF
 dc.b '                Monitor info (version '
 VERSION
 dc.b ')',LF
 dc.b '                -----------------------------',LF,LF
 dc.b '   This is a machine code monitor for the Amiga.',LF
 dc.b ' Pressing the HELP-key displays a menu of available commands.',LF,LF
 dc.b ' Note1: Some of the assembler commands require the',LF
 dc.b ' size specifier (.B, .W or .L), but it can''t be used by some others.',LF,LF
 dc.b ' Note2: default number base is hex, use ''+'' prefix for decimal',LF
 dc.b ' (default base for negative numbers is decimal, use ''$'' prefix for hex)',LF
 dc.b ' Many assembler instructions require the ''$'' before a hex number starting',LF
 dc.b ' with ''D'' or ''A''',LF
 dc.b LF
 dc.b ' This version can be freely distributed for non-commercial purposes.',LF
 dc.b ' I hope you find this program useful, but if you find any bugs in this',LF
 dc.b ' program, please let me know.',LF
 dc.b '  Here is my address:',LF
 dc.b '    Timo Rossi',LF
 dc.b '    Kellankoski',LF
 dc.b '    44300 KONNEVESI',LF
 dc.b '    FINLAND',LF,LF,0

**** text data ****
UpAndClearEol dc.b CtrlK,CSI,'K',0
dosname       dc.b 'dos.library',0
gfxname       dc.b 'graphics.library',0
tdname        dc.b 'trackdisk.device',0
audioname     dc.b 'audio.device',0
allocmap      dc.b 1,8,2,4
window        dc.b 'RAW:0/0/640/'
winheight     dc.b 'xxx/Amiga Monitor V'
*** NOTE: window height set according to GfxBase->NormalDisplayRows
 VERSION
 dc.b 0
welcometxt dc.b CLS,LF,TAB,TAB,TAB,' --- Amiga Monitor ---',LF,LF
 dc.b TAB,'  by Timo Rossi (c) 1987-1988, pre-release version '
 VERSION
 dc.b LF,LF,0
prompt    dc.b '-> ',0
errtx     dc.b '???',0
breaktx   dc.b '*** Break ***',LF,0
NewCliCom dc.b 'NewCLI "CON:0/12/640/100/Use EndCLI to close this window"',0
oldTx     dc.b 'Old: $',0
newTx     dc.b ' New: $',0
noBrkTxt  dc.b 'No Breakpoints set',LF,0
brklistTx dc.b 'List of Breakpoints:',LF,0
audiotxt  dc.b 'Press Ctrl-C to stop...',LF,0
ulserr    dc.b 'Unload old segment first',LF,0
segadrmes dc.b 'First segment at $',0
allocmes  dc.b 'Allocated from $',0
readtxt   dc.b ' bytes read from $',0
totxt     dc.b ' to $',0
allocfail dc.b 'Allocation failed',LF,0
noalloctx dc.b 'Not allocated that',LF,0
no_alloc  dc.b 'No memory allocated',LF,0
memlisttx dc.b 'Allocated memory:',LF,0
nosegmes  dc.b 'No segment loaded',LF,0
seghead   dc.b 'Segment list:',LF,0
loctext   dc.b ' startloc   endloc    length',LF,0
memerr    dc.b 'Out of memory',LF,0
doserrtx  dc.b 'DOS error ',0
dnam      dc.b '(dir)',0
freetxt   dc.b ' Blocks free.',LF,0
diskerr   dc.b 'Trackdisk error ',0
rettx     dc.b '*** Returned ***',LF,0
brkPtTxt  dc.b '*** Breakpoint ***',LF,0
signtxt   dc.b 'unsigned',0

*** TRAP NAMES ***
buserrnam dc.b 'Bus error',0
adrerrnam dc.b 'Address error',0
illinstr  dc.b 'Illegal instruction',0
zerodiv   dc.b 'Zero divide',0
CHKnam    dc.b 'CHK instruction trap',0
TRAPVnam  dc.b 'TRAPV instruction trap',0
privilege dc.b 'Privilege violation',0
tracenam  dc.b 'Trace',0
lineAnam  dc.b 'Line-A emulator trap',0
lineFnam  dc.b 'Line-F emulator trap',0
TRAPnam   dc.b 'TRAP instruction #',0
 ds.w 0
trapnamtabl:
 relword buserrnam ;0
 relword adrerrnam ;1
 relword illinstr  ;2
 relword zerodiv   ;3
 relword CHKnam    ;4
 relword TRAPVnam  ;5
 relword privilege ;6
 relword tracenam  ;7
 relword lineAnam  ;8
 relword lineFnam  ;9
 relword TRAPnam   ;10

*** UNINITIALIZED DATA SEGMENT ***
 BSS
DataRegs ds.l 8 ;storage space for processor registers
AddrRegs ds.l 8
regPC    ds.l 1
regCC    ds.b 1
StackPtr ds.l 1
oldPC    ds.l 1
InpBuf   ds.b LEN
OutBuf   ds.b LEN
oldline  ds.b LEN*NLINES
 END
