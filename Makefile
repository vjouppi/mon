#
# Makefile for Amiga monitor
# created for v1.25 by TR on 1990-05-24
#
# mod. 1990-08-22
#

.SUFFIXES: .asm .o

VER = 1.30
ASM = a68k
AOPTS = -iai: -f -q100
LNK = blink
LOPTS = nodebug verbose

.asm.o:
	$(ASM) $(AOPTS) $*.asm

all:	mon patchtrace

mon:	mon.o
	$(LNK) $(LOPTS) from mon.o to mon

patchtrace:	patchtrace.o
	$(LNK) $(LOPTS) from patchtrace.o to patchtrace

mon$(VER).zoo:	mon mon.doc patchtrace
	zoo a mon$(VER).zoo mon mon.doc patchtrace
