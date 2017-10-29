; Maitreyi Ashok
; Section 1 – Richard Lee

        NAME    Mtimer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  Motors Timers                             ;
;                               Motor Timer Routines                         ;
;                                     EE/CS 51                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; The functions in this file are the basic functions to allow for event handling
; based on both a regular 4 KHz interrupt that controls the speed and direction of
; the motors of the RoboTrike through pulse width modulation and that also sets or
; turns off the laser based on previous calls to mutators.
;
; Table of Contents:
; InstallTimer0Handler - Sets the address of the motors event handler as the
;		address to jump to when handling a timer interrupt from timer 0
; InitTimer0 - Initializes the timer 0 with frequency 4 KHz

;
; Revision History:
; 11/09/2016	Maitreyi Ashok	Wrote functions for timer 0 initialization and
;								handling for motors
; 11/12/2016    Maitreyi Ashok  Separated from keypad/display timer code

CGROUP  GROUP   CODE

$INCLUDE(Timer.inc)
$INCLUDE(Timer0.inc)
$INCLUDE(Inth.inc)

CODE	SEGMENT PUBLIC 'CODE'


        ASSUME  CS:CGROUP
		EXTRN	MotorsEventHandler:NEAR
        
; InstallTimer0Handler
;
; Description:       Install the event handler for the timer interrupt for motor
;                    pulse width modulation.
;
; Operation:         Writes the address of the timer event handler to the
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
; Author:            Glen George, Maitreyi Ashok
; Last Modified:     Jan. 28, 2002
;                    11/09/2016     Maitreyi Ashok   Wrote code
;					 11/12/2016		Maitreyi Ashok	 Updated comments
;					

InstallTimer0Handler  PROC    NEAR
                    PUBLIC      InstallTimer0Handler


        XOR     AX, AX          ;clear ES (interrupt vectors are in segment 0)
        MOV     ES, AX
                                ;store the vector with address of motors event handler
		MOV		ES: WORD PTR (VECTOR_SIZE * TMR_0_VEC), OFFSET(MotorsEventHandler)
		MOV		ES: WORD PTR (VECTOR_SIZE * TMR_0_VEC + 2), SEG(MotorsEventHandler)

        RET                     ;all done, return

InstallTimer0Handler  ENDP

; InitTimer0
;
; Description:       This function initializes the 80188 timer 0 and its count.
;					 Also sends EOI to clear pending interrupts
;
; Operation:         The 80188 timers are initialized to generate interrupts
;                    at 4 KHz.  The interrupt controller is also initialized
;                    to allow the timer interrupts. The time keeping counters 
;                    and flags are also reset.
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
; Author:            Glen George, Maitreyi Ashok
; Last Modified:     October 11, 1998
;                    11/09/2016     Maitreyi Ashok      Wrote code
;					 11/12/2016		Maitreyi Ashok		Updated comments

InitTimer0      PROC    NEAR
                PUBLIC  InitTimer0

InitTimer0ForMotors:
		MOV     DX, TMR_0_COUNT   	;initialize the count register to 0
        XOR     AX, AX
        OUT     DX, AL

        MOV     DX, TMR_0_MAX_CNT  	;setup max count for regular interrupts
        MOV     AX, MOTORS_COUNT
        OUT     DX, AL

        MOV     DX, TMR_0_CTRL    	;setup the control register
        MOV     AX, TMR_0_CTRL_VAL
        OUT     DX, AL
GeneralTimer0Initialization:
        MOV     DX, INT_CTRLR_CTRL 	;setup the interrupt control register
        MOV     AX, INT_CTRLR_CVAL
        OUT     DX, AL

        MOV     DX, INT_CTRLR_EOI 	;send an EOI to turn off any pending interrupts
        MOV     AX, TIMER_EOI
        OUT     DX, AL

        RET                     ;done so return


InitTimer0      ENDP

CODE    ENDS

		END