;
; mon_version.i
;
*** This macro is an easy way to update the version number ***

BETA		equ	0	;special 'Beta version' flag

VERSION		macro
		dc.b	'1.65'
		ifne	BETA
		dc.b	'b'
		endc
		endm

DATE		macro
		dc.b	'21.05.94'
		endm
