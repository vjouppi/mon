.SUFFIXES: .asm .o

ASM = a68k
AOPTS = -iai: -iasminc: -q100 -f

#ASM = genim2
#AOPTS = -l -iai: o+

#ASM = assem
#AOPTS = -i ai:

LNK = blink
LNKOPTS = verbose nodebug

.asm.o:
	$(ASM) $(AOPTS) $*.asm #-o $*.o 

OBJS = mon_main.o assemble.o disassemble.o disk_io.o eval.o execute.o \
	memory.o mon_dos.o mon_io.o mon_util.o registers.o sound.o \
	variables.o misc_cmd.o mon_misc.o

mon:	$(OBJS)
	$(LNK) $(LNKOPTS) FROM $(OBJS) TO mon MAP mon.map

assemble.o:	assemble.asm monitor.i instructions.i
disassemble.o:	disassemble.asm monitor.i instructions.i
disk_io.o:	disk_io.asm monitor.i
eval.o:		eval.asm monitor.i
execute.o:	execute.asm monitor.i
memory.o:	memory.asm monitor.i
mon_dos.o:	mon_dos.asm monitor.i
mon_io.o:	mon_io.asm monitor.i
mon_main.o:	mon_main.asm monitor.i
mon_misc.o:	mon_misc.asm monitor.i
mon_util.o:	mon_util.asm monitor.i
registers.o:	registers.asm monitor.i
sound.o:	sound.asm monitor.i
variables.o:	variables.asm monitor.i

clean:
		-delete \#?.o

