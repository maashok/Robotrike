; Maitreyi Ashok
; Section 1 – Richard Lee

        NAME    SerInt2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  	  SerInt2                                ;
;                             Serial Interrupt Routines                      ;
;                                     EE/CS 51                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; The functions in this file are the basic functions to allow for interrupt driven
; event handling of serial I/O using the external INT 2 interrupt.
;
; Table of Contents:
; InstallInt2Handler - Sets the address of the serial event handler as the
;		address to jump to when handling an interrupt from INT 2
; InitInt2 - Initializes the interrupt INT 2

;
; Revision History:
; 11/16/2016	Maitreyi Ashok	Initial revision
; 11/18/2016	Maitreyi Ashok	Updated comments

CGROUP  GROUP   CODE

$INCLUDE(IntH.inc)			; Contains general interrupt handling definitions
$INCLUDE(SerInt2.inc)		; Contains serial int 2 specific definitions

CODE	SEGMENT PUBLIC 'CODE'


        ASSUME  CS:CGROUP
		EXTRN	SerialEventHandler:NEAR		; Handles all types of serial interrupt
											; events
        
; InstallInt2Handler
;
; Description:       Install the event handler for the INT 2 interrupt for serial
;                    I/O to be used when have error, new data, or need to transmit data.
;
; Operation:         Writes the address of the serial event handler to the
;                    appropriate interrupt vector.
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
; Registers Changed: flags, AX, ES
; Stack Depth:       0 words
;
; Author:            Maitreyi Ashok
; Last Modified:     11/16/2016     Maitreyi Ashok   Wrote code
;					 11/18/2016		Maitreyi Ashok	 Updated comments
;					

InstallInt2Handler  PROC    NEAR
                    PUBLIC      InstallInt2Handler


        XOR     AX, AX          ;clear ES (interrupt vectors are in segment 0)
        MOV     ES, AX
                                ;store the vector with address of serial event handler
		MOV		ES: WORD PTR (VECTOR_SIZE * INT_2_VEC), OFFSET(SerialEventHandler)
		MOV		ES: WORD PTR (VECTOR_SIZE * INT_2_VEC + 2), SEG(SerialEventHandler)

        RET                     ;all done, return

InstallInt2Handler  ENDP

; InitInt2
;
; Description:       This function initializes the 80188 INT 2 interrupt and
;					 turns off any pending interrupts.
; Operation:         The INT 2  external interrupt initialized to generate interrupts
;                    to handle serial errors, receiving of data, and data transmission
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
; Registers Changed: flags, AX, DX
; Stack Depth:       0 words
;
; Author:            Maitreyi Ashok
; Last Modified:     11/16/2016     Maitreyi Ashok      Wrote code
;					 11/18/2016		Maitreyi Ashok		Updated comments

InitInt2        PROC    NEAR
                PUBLIC  InitInt2

InitINT2ForSerial:
		MOV     DX, INT2_CTRL   	;initialize the control register to allow INT 2
        MOV     AX, INT_2_CTRL_VAL  ;interrupts
        OUT     DX, AL
GeneralInt2Initialization:
        MOV     DX, INT_CTRLR_EOI 	;send an EOI to turn off any pending interrupts
        MOV     AX, NON_SPEC_EOI
        OUT     DX, AL

        RET                     ;done so return


InitInt2      ENDP

CODE    ENDS

		END