;
; script.i
;

;
; script handle structure -- contains file buffer
; (probably should put the file name somewhere here for error reporting)
;
		STRUCTURE ScriptHandle,0
		 APTR	sh_Next
		 APTR	sh_Length
		 STRUCT	sh_Name,32
		 LONG	sh_LineNum
		 APTR	sh_Ptr
		LABEL	sh_Buffer

