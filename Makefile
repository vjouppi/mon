#
# Makefile for Amiga monitor
# created for v1.25 by TR on 1990-05-24
#

.SUFFIXES: .asm .o

ASM = a68k
AFLAGS = -iai: -f -q100
LNK = blink
LFLAGS = nodebug verbose

.asm.o:
	$(ASM) $(AFLAGS) $*.asm

mon:	mon.o
	$(LNK) $(LFLAGS) from mon.o to mon

