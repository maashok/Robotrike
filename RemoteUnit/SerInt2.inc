;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                SerInt2.INC                                 ;
;                                Serial INT 2                                ;
;                                Include File                                ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the definitions for the Serial Interrupt program (SerInt2).
;
; Revision History:
;    11/16/16   Maitreyi Ashok      Initial Revision
;	 11/18/16	Maitreyi Ashok		Updated comments
;	 12/05/16	Maitreyi Ashok		Removed redundant INT 2 EOI
;
; Addresses
INT2_CTRL       EQU     INT_BASE + 3CH 	; Interrupt 2 Control Register

; Control Register Values

INT_2_CTRL_VAL  EQU     00007H      ; value to write to INT 2 control register
                                    ; 00000000000----- reserved
                                    ; -----------0---- edge triggering
                                    ; ------------0--- enable intterupts from INT 2
                                    ; -------------111 highest priority level
; Interrupt Vector
INT_2_VEC       EQU     14          ;inverrupt vector for INT 2