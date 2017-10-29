; Maitreyi Ashok
; Section 1 – Richard Lee

        NAME    KDTimer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                               Keypad Display Timers                        ;
;                           Keypad Display Timer Routines	                 ;
;                                     EE/CS 51                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; The functions in this file are the basic functions to allow for event handling
; based on both a regular 1 ms timer interrupt that affect the multiplexing 
; such that the LEDs for each digit are displayed in succession to give the 
; image of all LEDs being displayed at once in the display functions as well
; as scanning of keypad.
;
; Table of Contents:
; InstallTimer2Handler - Sets the address of the timer event handler as the address
;		to jump to when handling a timer interrupt from timer 2
; InitTimer2 - Initializes the timer 2 with frequency 1 KHz
; Timer2EventHandler - handles the 1 ms interval interrupts, calling the
;		multiplexing function accordingly and checking whether the relevant
;		keypad buttons have been pressed down long enough to make an effect on
;		the display
;
;
; Revision History:
; 10/24/2016	Maitreyi Ashok	Wrote basic outlines and functional specifications
;								for all functions.		
; 10/28/2016    Maitreyi Ashok  Changed the timer event handler to push/pop all
;								registers
; 11/02/2016	Maitreyi Ashok	Added calling keypad scanning function in event
;								handler
; 11/03/2016    Maitreyi Ashok	Separated from nonrelated interrupt handling
;								functions
; 11/26/2016	Maitreyi Ashok	Updated comments

CGROUP  GROUP   CODE

$INCLUDE(Timer2.inc)
$INCLUDE(Timer.inc)
$INCLUDE(IntH.inc)

CODE	SEGMENT PUBLIC 'CODE'


        ASSUME  CS:CGROUP
        EXTRN   DigitMux:NEAR		; Use to multiplex LEDs (Not required for HW 5)
		EXTRN	KeypadScan:NEAR		; Scan keypad for key presses
        
; InstallTimer2Handler
;
; Description:       Install the event handler for the timer interrupt for display
;                    and keypad from Timer 2.
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
;                    11/26/2016     Maitreyi Ashok   Updated comments

InstallTimer2Handler  PROC    NEAR
                    PUBLIC      InstallTimer2Handler


        XOR     AX, AX          ;clear ES (interrupt vectors are in segment 0)
        MOV     ES, AX
                                ;store the vector with address of timer event handler
        MOV     ES: WORD PTR (VECTOR_SIZE * TMR_2_VEC), OFFSET(Timer2EventHandler)
        MOV     ES: WORD PTR (VECTOR_SIZE * TMR_2_VEC + 2), SEG(Timer2EventHandler)           

        RET                     ;all done, return

InstallTimer2Handler  ENDP

; InitTimer2
;
; Description:       This function initializes the 80188 timer 2 and the time
;                    keeping variables and flags.
;
; Operation:         The 80188 timers are initialized to generate interrupts
;                    every 1 ms.  The interrupt controller is also initialized
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
;                    11/03/2016     Maitreyi Ashok      Updated comments

InitTimer2      PROC    NEAR
                PUBLIC  InitTimer2

InitTimer2ForDisplayAndKeypad:
        MOV     DX, TMR_2_COUNT    ;initialize the count register to 0
        XOR     AX, AX
        OUT     DX, AL

        MOV     DX, TMR_2_MAX_CNT  ;setup max count for 1 KHz counts
        MOV     AX, ONE_MS_COUNT
        OUT     DX, AL

        MOV     DX, TMR_2_CTRL     ;setup the control register
        MOV     AX, TMR_2_CTRL_VAL
        OUT     DX, AL
GeneralTimer2Initialization:
        MOV     DX, INT_CTRLR_CTRL 	;setup the interrupt control register
        MOV     AX, INT_CTRLR_CVAL
        OUT     DX, AL

        MOV     DX, INT_CTRLR_EOI 	;send an EOI to turn off any pending interrupts
        MOV     AX, TIMER_EOI
        OUT     DX, AL
        RET                     ;done so return


InitTimer2      ENDP

; 
; Timer2EventHandler		
; Description: 		This procedure handles updating the display once every millisecond.
;					An interrupt will be created at a frequency of 1 KHz, and the
;					interrupt vector table stores the address of this event handler
;					for the timer interrupt. Thus, this function calls the function
;					to multiplex the LEDs every time, since the LEDs are also multiplexed
;					at a frequency of 1 KHz. At the end of execution, an EOI
;					is also output, to let others know that this interrupt has
;					finished. In addition, the keypad is scanned every timer interrupt
;                   to see whether any new keys are pressed or if the same keys
;                   as before are pressed, to debounce them.
;
; Operation:  		Every one second, whien this function is called to handle the, timer
;					interrupt the multiplexing function is called so that the next
;					digit can be displayed for the next millisecond. This allows the 
;					general LED display to be used, with less hardware and more 
;					software used to implement the features, as only one set of LED 
;					segments needs to be on at any point in time. Also, the keypad 
;					is scanned for any key presses and debounces any key presses 
;					as well during every interrupt. This allows for any key presses 
;					to be found relatively quickly and handled by enqueuing to a 
;					general event queue. In addition, this handler takes care of 
;					sending an End of Interrupt signal so that the CPU knows to move 
;					on to the next interrupt in its queue. Also, it pushes and pops all 
;					registers prior to execution of the muxing functions so that the 
;					interrupt handler does not mess up any of the foreground code 
;					execution.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	None
; Shared Variables: None
; Global Variables: None
;
; Input:			Keypad keys are pressed by the user
; Output:			Segments are displayed for the digit that we are currently
;					multiplexing at, through the DigitMux function.
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	None
;
; Registers Changed:None
; Stack Depth:		8 words
;
; Limitations:		This implementation only allows the multiplexing to take place
;					at 1 ms intervals.
;
; Author:			Maitreyi Ashok
; Last Modified:	10/24/2016	Maitreyi Ashok		Wrote functional specification
;													and outline
;					10/26/2016  Maitreyi Ashok   	Wrote code for basic muxing
;													without blinking/scrolling
;                   11/05/2016  Maitreyi Ashok      Updated to allow for blinking


Timer2EventHandler		PROC        NEAR
						PUBLIC      Timer2EventHandler
SetUp:
		PUSHA                       ; Save all registers to avoid changes
MuxDigits:
		CALL	DigitMux            ; Multiplex the LED display
ScanKeyPad:
		CALL	KeypadScan          ; Scan the keypad for a key press
EndEventHandler:
		MOV		DX, INT_CTRLR_EOI   ; Send an EOI for the interrupt
		MOV		AX, TIMER_EOI
		OUT		DX, AL
		POPA                        ; Restore all registers to original state
		IRET
Timer2EventHandler	ENDP

CODE    ENDS

		END