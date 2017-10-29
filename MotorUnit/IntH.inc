;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   IntH.INC                                 ;
;                               Interrupt Handler                            ;
;                                 Include File                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the definitions for the General Interrupt Handler
; program (GenIH.ASM).
;
; Revision History:
;	  10/28/16 Maitreyi Ashok   Added necessary definitions for RoboTrike event
;								handling and removed unnecessary definitions
;	  10/29/16 Maitreyi Ashok	Added constant for size of vector
;     11/03/16 Maitreyi Ashok   Split from timer include file


; Interrupt Controller Definitions

; Addresses
INT_BASE        EQU     0FF00H			; Starting address of interrupt control
										; registers
INT_CTRLR_EOI   EQU     INT_BASE + 22H  ; address of interrupt controller EOI register

; Register Values
NON_SPEC_EOI    EQU     08000H          ; Non-specific EOI command


; General Definitions

VECTOR_SIZE				EQU 4		; Size of each entry in interrupt vector table 
									; in bytes
HALF_VECTOR_SIZE		EQU	VECTOR_SIZE/2	
									; Size of just IP portion of table vector
FIRST_RESERVED_VEC	    EQU	1		; reserve vectors 1-3
LAST_RESERVED_VEC	    EQU	3
NUM_IRQ_VECTORS         EQU 256     ;number of interrupt vectors