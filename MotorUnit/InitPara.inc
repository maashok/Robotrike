;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   InitPara                                 ;
;                                 Include File                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the symbolic constants for the parallel port initialization.
;
; Revision History:
;    11/09/16  Maitreyi Ashok           Defined constants to initialize parallel port

; Parallel Port Unit Definitions

; Addresses
PARALLEL_PORT_START_ADDR  EQU     0180H     ; starting address of parallel port in I/O
PARA_CNTRL_REG_OFFSET     EQU     3     	; offset of control register from start
PARA_PORT_B_OFFSET		  EQU	  1			; offset of parallel port B

; Control Register Values
PARA_B_OUT_CNTRL_VAL  EQU     80H       ;1-------  Active Mode
                                        ;-00-----  Mode 0 - Basic Input/Output
                                        ;---0----  Port A output
                                        ;----0---  Port C (upper) output
                                        ;-----0--  Mode 0 - Basic Input/Output
                                        ;------0-  Port B output
                                        ;-------0  Port C (lower) output