;
; mon_version.i
;
*** This macro is an easy way to update the version number ***

BETA		equ	0	;special 'Beta version' flag

VERSION		macro
		dc.b	'1.54'
		ifne	BETA
		dc.b	'b'
		endc
		endm

DATE		macro
		dc.b	'1992-01-05'
		endm
