;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 CONVERTS.INC                               ;
;                                 Include File                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the symbolic constants for the CONVERTS.ASM program.
;
; Revision History:
;    10/12/16  Maitreyi Ashok              Added TRUE/FALSE symbolic constants
;										   and ASCII conversion constants
;    10/15/16  Maitreyi Ashok              Removed TRUE/FALSE constants since
;                                          unused by program
;	 11/04/16  Maitreyi Ashok			   Changed definitions to use ASCII chars
;										   instead of numbers


; logical values

NEG_SIGN		EQU		'-'				; ASCII value of negative sign
MAX_PWR_10		EQU		10000			; Maximum power of 10 for a 16 bit value (10^4)
MAX_PWR_16		EQU		1000H			; Maximum power of 10 for a 16 bit value (16^3)