#
# Makefile for Amiga Monitor
#
# Timo Rossi, 1991
#
# for the A68k assembler and Blink
#
# Note that mon_misc.o must be the last object module on linker command line
#

.SUFFIXES: .asm .o

ASM = a68k
AOPTS = -iasminc: -q500 -f -d

LNK = blink
LNKOPTS = nodebug

all:	mon patchtrace

.asm.o:
	$(ASM) $(AOPTS) $*.asm #-o $*.o 

OBJS = mon_main.o assemble.o disassemble.o disk_io.o eval.o execute.o \
	mem_cmd.o mon_dos.o mon_io.o mon_util.o registers.o sound.o \
	variables.o misc_cmd.o lister.o loadsym.o mon_misc.o \
	stringstuff.o script.o

mon:	$(OBJS)
	$(LNK) $(LNKOPTS) FROM $(OBJS) TO mon MAP mon.map

patchtrace: patchtrace.o
	$(LNK) $(LNKOPTS) FROM patchtrace.o to patchtrace

assemble.o:	assemble.asm monitor.i mon_macros.i instructions.i
disassemble.o:	disassemble.asm monitor.i mon_macros.i instructions.i variables.i
disk_io.o:	disk_io.asm monitor.i mon_macros.i
eval.o:		eval.asm monitor.i mon_macros.i
execute.o:	execute.asm monitor.i mon_macros.i breakpoint.i
mem_cmd.o:	mem_cmd.asm monitor.i mon_macros.i
mon_dos.o:	mon_dos.asm monitor.i mon_macros.i
mon_io.o:	mon_io.asm monitor.i mon_macros.i
mon_main.o:	mon_main.asm monitor.i mon_macros.i script.i mon_version.i
mon_misc.o:	mon_misc.asm monitor.i mon_macros.i
mon_util.o:	mon_util.asm monitor.i mon_macros.i
registers.o:	registers.asm monitor.i mon_macros.i
sound.o:	sound.asm monitor.i mon_macros.i
variables.o:	variables.asm monitor.i mon_macros.i variables.i
lister.o:	lister.asm monitor.i mon_macros.i
loadsym.o:	loadsym.asm monitor.i mon_macros.i
stringstuff.o:	stringstuff.asm mon_macros.i
script.o:	script.asm monitor.i script.i

patchtrace.o:	patchtrace.asm

clean:
		-delete \#?.o mon.map

