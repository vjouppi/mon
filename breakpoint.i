;
; breakpoint.i
;
*** BREAKPOINT STRUCTURE ***

		STRUCTURE BrkPoint,0
		 APTR	brk_Next	;linked list
		 APTR	brk_Address	;address of breakpoint
		 UWORD	brk_Count	;count required for activation
		 UWORD	brk_ActCount	;actual count when active
		 UWORD	brk_Content	;word contents of that location
		LABEL brk_SIZE	;because it is temporarily replaced by ILLEGAL

