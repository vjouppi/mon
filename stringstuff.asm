;
; stringstuff.asm -- for Amiga Monitor
;
; created for version 1.52 by TR on 1991-12-02
;
		include	'mon_macros.i'

;
; originally:
;   escapestring.a  --  convert '\'-escape sequences in strings
;   created by Timo Rossi, 1991-01-16
;
; -- moved to monitor source, 1991-12-02
;

*{
;
; EscapeString(sourceString, destString)
;                   a0           a1
;
; copies sourceString to destString while converting escape sequences.
; because the destination string is always shorter than the source, this
; can be used as 'in place' conversion by setting the sourceString and
; destString pointers to the same value.
;
; inputs:
;
;   a0   -   pointer to a null-terminated source string, possibly containing
;            escape sequences.
;
;   a1   -   pointer to destination string buffer, should be at least as long
;            as sourceString. Can be same as a0 (in place conversion).
;
; returns:
;   d0   -   length of the converted string (without the null terminator)
;  (a0 and a1 are preserved)
;
; the following escape sequences are supported
;
;   \n   -  0x0a (newline)
;   \r   -  0x0d (carriage return)
;   \t   -  0x09 (tabulator)
;   \e   -  0x1b (escape)
;   \\   -   '\'  (backslash)
;   \xAA -  value 'AA' in hexadecimal
;
*}

		pub	EscapeString

		movem.l	d1/a0/a1,-(sp)

esc_loop	move.b	(a0)+,d0
		beq	esc_exit

		moveq	#'\',d1
		cmp.b	d0,d1
		bne.s	esc_store
		move.b	(a0)+,d0
		beq	esc_exit
		cmp.b	d0,d1
		beq.s	esc_store

		cmp.b	#'n',d0
		bne.s	1$

		moveq	#10,d0
		bra	esc_store

1$		cmp.b	#'r',d0
		bne.s	2$

		moveq	#13,d0
		bra	esc_store

2$		cmp.b	#'t',d0
		bne.s	3$

		moveq	#9,d0
		bra	esc_store

3$		cmp.b	#'e',d0
		bne.s	4$

		moveq	#27,d0
		bra	esc_store

4$		cmp.b	#'x',d0
		bne	esc_store

		move.b	(a0)+,d1
		beq	esc_exit
		bsr	hexdigit
		bcs.s	esc_store
		move.b	d1,d0

		move.b	(a0)+,d1
		bne.s	5$
		move.b	d0,(a1)+
		bra.s	esc_exit

5$		bsr	hexdigit
		bcc.s	6$

		subq.l	#1,a0
		bra.s	esc_store

6$		lsl.b	#4,d0
		or.b	d1,d0

esc_store	move.b	d0,(a1)+
		bra	esc_loop

esc_exit	clr.b	(a1)
		move.l	a1,d0
		movem.l	(sp)+,d1/a0/a1
		sub.l	a1,d0
		rts

;
; convert hex digit to a binary number, return carry clear if ok
; carry set if not a hex digit.
; input:  d1 - hex digit
; output: d1 - binary number
;  carry flag - error status
;
; does not modify any other registers than d1
;
hexdigit	cmp.b	#'0',d1
		bcs.s	hexdigit9
		cmp.b	#'9',d1
		bls.s	hexdigit_ok
		and.b	#$df,d1
		cmp.b	#'A',d1
		bcs.s	hexdigit9
		cmp.b	#'F',d1
		bhi.s	hexdigit_fail

hexdigit_alpha	subq.b	#7,d1
hexdigit_ok	and.b	#$0f,d1
hexdigit9	rts

hexdigit_fail	sec
		rts

*{
;
; string=GetName(cmdline)
;   d0             a3
;
; Inputs:
;  a3  -  Pointer to string or command line
;
; Returns:
;  d0  -  Pointer to string argument or zero if failed.
;  a3  -  Updated to the position after the end of the argument in the string.
;
; Gets the next string argument from command line pointed by a3 to
; d0. returns zero if no valid string argument found. Arguments are
; separated by spaces and can enclosed in double quotes if they contain
; spaces. After the call, a3 points to the command line to a position
; immediately after the argument.
;
; NOTE: The command line itself is modified as this routine null-terminates
; the strings it returns.
;
; modifies only registers d0 and a3.
;
; no libraries used.
;
*}

		pub	GetName

01$		cmp.b	#' ',(a3)+
		beq.s	01$		;skip spaces
		bcs.s	noname
		subq.l	#1,a3
		cmp.b	#'"',(a3)
		beq.s	quoted
		move.l	a3,D0

getname1	cmp.b	#' ',(a3)+
		bhi.s	getname1
		tst.b	-(a3)
		beq.s	getname_ret
		clr.b	(a3)+
		bra.s	getname_ret

quoted		addq.l	#1,a3
		move.l	a3,D0

getname2	tst.b	(a3)
		beq.s	noname
		cmp.b	#'"',(a3)+
		bne.s	getname2
		clr.b	-1(a3)

getname_ret	move.l	d0,a0
		move.l	d0,a1
		call	EscapeString
		move.l	a0,d0
		rts

noname		moveq	#0,D0
		rts

		end
