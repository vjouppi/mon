;
; variables.i
;
*** Variable structure ***
; the size of this structure depends on the length of the variable name

		STRUCTURE Variable,0
		 APTR	var_Next	;linked list
		 LONG	var_Value	;32-bit interger value of variable
		 WORD	var_HunkNum	;or -1 if none
		 WORD	var_Length	;size of structure for FreeMem()
		LABEL	var_Name	;null-terminated string

