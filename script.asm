;
; script.asm  for Amiga monitor
;

		include	"exec/types.i"
		include	"exec/memory.i"
		include	"libraries/dos.i"
		include	"libraries/dosextens.i"
		include	"offsets.i"

		include	"monitor.i"
		include	"script.i"

		xref	generic_error
		xref	scriptonly_error
		xref	undeflabel_error
		xref	error_com
		xref	exec_cmd
;
;
		pub	free_all_scripts

		getbase	Exec
		move.l	mon_ScriptList(a4),d2

free_script_loop
		tst.l	d2
		beq.s	all_scripts_free
		move.l	d2,a1
		move.l	sh_Length(a1),d0
		move.l	sh_Next(a1),d2
		lib	FreeMem
		bra.s	free_script_loop

all_scripts_free
		clr.l	mon_ScriptList(a4)
		rts

;
; a0 - filename
;
		pub	get_script

		movem.l	d2/d3/d7/a2/a3,-(sp)
		clra	a2
		move.l	a0,a3
		move.l	a0,d1
		move.l	#MODE_OLDFILE,d2
		lib	Dos,Open
		move.l	d0,d7
		beq	getscript_err

		move.l	d7,d1
		moveq	#0,d2
		moveq	#OFFSET_END,d3
		lib	Seek
		tst.l	d0
		bmi.s	getscript_err
		move.l	d7,d1
		moveq	#0,d2
		moveq	#OFFSET_BEGINNING,d3
		lib	Seek
		move.l	d0,d3
		ble.s	getscript_err
;
; make sure that the script is nul-terminated
;
		moveq	#sh_Buffer+1,d1
		add.l	d1,d0
		move.l	d0,d2
		move.l	#MEMF_CLEAR!MEMF_PUBLIC,d1
		lib	Exec,AllocMem
		tst.l	d0
		beq.s	getscript_err_mem
		move.l	d0,a2
		move.l	d2,sh_Length(a2)

		moveq	#30-1,d1
		lea	sh_Name(a2),a0
1$		move.b	(a3)+,(a0)+
		dbeq	d1,1$

		move.l	d7,d1
		move.l	a2,d2
		moveq	#sh_Buffer,d0
		add.l	d0,d2
		lib	Dos,Read
		tst.l	d0
		bmi.s	getscript_err
		move.l	d7,d1
		lib	Close

		lea	sh_Buffer(a2),a0
		move.l	a0,sh_Ptr(a2)

		move.l	mon_ScriptList(a4),sh_Next(a2)
		move.l	a2,mon_ScriptList(a4)
		move.l	a2,d0

getscript_end	movem.l	(sp)+,d2/d3/d7/a2/a3
		rts

getscript_err_mem
		move.l	mon_Task(a4),a0
		moveq	#ERROR_NO_FREE_STORE,d0
		move.l	d0,pr_Result2(a0)

getscript_err	move.l	a2,d0
		beq.s	1$
		move.l	a2,a1
		move.l	sh_Length(a1),d0
		lib	Exec,FreeMem

1$		move.l	d7,d1
		beq.s	2$
		lib	Dos,Close
2$		bra.s	getscript_end

;
; command to execute a script
;
		cmd	exec_script

		call	GetName
		tst.l	d0
		beq	generic_error
		move.l	d0,a0

		pub	exec_script

		call	get_script
		tst.l	d0
		beq	dos_error
		rts

dos_error	call	DosIoErr
		bra	error_com
;
; quit a script and return to higher level
;
		cmd	quit_script
		pub	quit_script

quit_script	move.l	mon_ScriptList(a4),d0
		beq	scriptonly_error
		move.l	d0,a1
		move.l	sh_Next(a1),mon_ScriptList(a4)
		move.l	sh_Length(a1),d0
		jlib	Exec,FreeMem

;
; read input from script
;
; input: pointer to scripthandle in a0
; this may modify almost any register
;
		pub	get_script_line

		move.l	sh_Ptr(a0),d0
		beq	quit_script
		lea	mon_InputBuf(a4),a2
		clr.b	(a2)
		move.l	d0,a1
		tst.b	(a1)
		beq	quit_script
		moveq	#LEN-2,d1

1$		move.b	(a1),d0
		beq.s	2$
		addq.l	#1,a1
		cmp.b	#LF,d0
		beq.s	2$
		move.b	d0,(a2)+
		dbf	d1,1$

2$		addq.l	#1,sh_LineNum(a0)
		clr.b	(a2)
		move.l	a1,sh_Ptr(a0)
		rts

;
; the script goto command
;
		cmd	goto

		move.l	mon_ScriptList(a4),d0
		beq	scriptonly_error
		move.l	d0,a2
		tst.b	(a3)
		beq	generic_error
		cmp.b	#'.',(a3)
		bne.s	1$
		addq.l	#1,a3			;allow goto .label
1$		lea	sh_Buffer(a2),a1
		move.l	sh_LineNum(a2),d5
		clr.l	sh_LineNum(a2)

goto_loop1	move.b	(a1),d0
		beq.s	goto_undeflabel
		cmp.b	#'.',d0
		bne.s	goto_try_next_line
		addq.l	#1,a1
		move.l	a3,a0
1$		move.b	(a1)+,d0
		beq.s	2$
		cmp.b	#LF,d0
		beq.s	2$
		cmp.b	#SPACE,d0
		beq.s	2$
		cmp.b	#TAB,d0
		beq.s	2$
		cmp.b	#':',d0
		beq.s	2$
		cmp.b	(a0)+,d0
		beq.s	1$
		bra.s	goto_try_next_line

2$		cmp.b	#'.',-2(a1)
		beq.s	goto_try_next_line
		tst.b	(a0)
		bne.s	goto_try_next_line
		cmp.b	#LF,d0
		bne.s	3$
		addq.l	#1,sh_LineNum(a2)
3$		move.l	a1,sh_Ptr(a2)
		rts

goto_try_next_line
1$		move.b	(a1)+,d0
		beq	undeflabel_error
		cmp.b	#LF,d0
		bne.s	1$
		addq.l	#1,sh_LineNum(a2)
		bra.s	goto_loop1

goto_undeflabel	move.l	d5,sh_LineNum(a2)
		bra	undeflabel_error

;
; if-command. this can actually be used outside of script as well
; (block-if/else/endif not currently supported)
;
		cmd	if

		call	GetExpr
		call	skipspaces
;
; one line if statement. execute the command following the if-statement
; if the condition is true
;
		tst.l	d0
		bne	exec_cmd
ret_01		rts

		end
