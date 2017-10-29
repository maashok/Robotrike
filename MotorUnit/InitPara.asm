; Maitreyi Ashok
; Section 1 – Richard Lee

        NAME    InitPara

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   InitPara                                 ;
;                     Parallel Port Initialization Routine                   ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; This file contains the initialization of the 8255 parallel port
;
; Table of Contents:
;
; InitParallelPort - Initialize control register for 8255 parallel port for use
; 
; Revision History:
; 	 11/07/2016		Maitreyi Ashok		Pseudocode/functional specifications
; 	 11/09/2016		Maitreyi Ashok		Wrote code
;	 11/12/2016		Maitreyi Ashok		Updated comments

$INCLUDE(InitPara.inc)
CGROUP  GROUP   CODE


CODE	SEGMENT PUBLIC 'CODE'
        ASSUME  CS:CGROUP

; InitParallelPort
;
; Description:       Initialize the Parallel port B on the 8255.
;
; Operation:         Write the initial values to the parallel port control register.
;
; Arguments:         None.
; Return Value:      None.
;
; Local Variables:   None.
; Shared Variables:  None.
; Global Variables:  None.
;
; Input:             None.
; Output:            None.
;
; Error Handling:    None.
;
; Algorithms:        None.
; Data Structures:   None.
;
; Registers Changed: DX, AX
; Stack Depth:       0 words
;
; Author:            Maitreyi Ashok
; Last Modified:     11/07/2016   Maitreyi Ashok   Pseudocode/Functional specification
;					 11/09/2016	  Maitreyi Ashok   Wrote code
;					 11/12/2016	  Maitreyi Ashok   Updated comments
; Pseudocode
; InitParallelPort()
; 		

InitParallelPort	PROC        NEAR
					PUBLIC      InitParallelPort
		MOV		DX, PARALLEL_PORT_START_ADDR + PARA_CNTRL_REG_OFFSET
										; Write control value to the address of the 8255
		MOV		AX, PARA_B_OUT_CNTRL_VAL; control register address
		OUT		DX, AL
		RET
InitParallelPort	ENDP

CODE 		ENDS
			END