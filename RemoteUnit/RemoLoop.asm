; Maitreyi Ashok
; Section 1 – Richard Lee

        NAME    RemoLoop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                    RemoLoop                                ;
;                             Remote Unit Main Loop	                         ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; This file contains the main loop for the remote unit of the RoboTrike that
; handles the keypad user interface, display, and serial interface.
;
; Table of Contents:
; MAIN - main loop of RoboTrike remote unit handling serial interface, keypad,
; and display
;
; Revision History:
;     11/29/2016	Maitreyi Ashok	Basic Functional specification/pseudocode

CGROUP  GROUP   CODE
DGROUP  GROUP   DATA, STACK

$INCLUDE(General.inc)           ; General definitions
$INCLUDE(EvQueue.inc)           ; Use event queue
$INCLUDE(RemoFns.inc)           ; Use remote routines

CODE	SEGMENT PUBLIC 'CODE'


        ASSUME  CS:CGROUP, DS:DGROUP
		
		EXTRN	CheckCriticalFailure:NEAR	; Check whether event queue filled up
											; when trying to enqueue
		EXTRN	InitRemoteLoopParts:NEAR	; Intialize all peripherals, interrupts
											; and other routines for remote unit
		EXTRN	EventTable:WORD				; Table of function to handle for different
											; event codes from DequeueEvent
		EXTRN	DequeueEvent:NEAR			; Dequeue event from EventQueue to process
        EXTRN   SerialPutString:NEAR        ; Output string to serial channel


; MAIN			
; Description: 		This main loop controls the remote unit of the RoboTrike, that
;					handles the serial interface, the keypad, and display. Keys
;					pressed on the keypad must be handled accordingly to perform
;					the correct associated keypad and display functions. In addition,
;					status and error information from the motor unit must be
;					displayed. For further information, see complete functional 
;					specification.
;
; Operation:  		The main loop dequeues an event from EventQueue and decides
;					how to handle it. If the event is a character received from
;					serial data, it will be displayed on the display regardless
;					of whether it is a status or error. If the event is a keypad
;					event the correct function is performed for this key. For further 
;					information, see complete functional specification.
 
; Global Variables: None
;
; Input:			Serial data is passed from the motor unit with status and
;					errors. In addition, keys are pressed on the keypad. For further 
;					information, see complete functional specification.
; Output:			Error and status from the motor unit, keypad entries, and other
;					errors are displayed. For further information, see complete 
;					functional specification.
;
; User Interface: 	The remote unit interfaces with the user through a keypad
;					with keys in a 4x4 configuration as well as a LED display to 
;					show status and errors. For further information, see complete 
;					functional specification.		
;
; Error Handling:	Errors from the motor unit and in this unit are handled by
;					displaying information about them. If an enqueue is attempted 
;					when the EventQueue is already full the system goes into 
;					critical failure mode and is reset. For	more information on 
;					the handling of this error and others see complete functional 
;					specification.
;
; Algorithms:		Algorithms are used to convert decimal and hexadecimal values
;					to strings, as well as for other purposes. For more information
;					see complete functional specification
; Data Structures:	EventQueue - queue of all events to handle
;;
; Limitations:		The remote unit does not account for multiple key presses
;					at the same time and will only see key presses in whichever 
;					row it scans first. For more limitations see complete functional 
;					specification.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/29/16	Maitreyi Ashok	Wrote functional specification/pseudocode
;					12/02/16	Maitreyi Ashok	Wrote code
;					12/04/16	Maitreyi Ashok	Updated comments

START:  

MAIN:
InitializeRemoteUnit:
        MOV     AX, DGROUP          ;initialize the stack pointer
        MOV     SS, AX
        MOV     SP, OFFSET(DGROUP:TopOfStack)

        MOV     AX, DGROUP          ;initialize the data segment
        MOV     DS, AX
        
		CALL	InitRemoteLoopParts	; Initialize all parts of the remote loop
CheckForSystemFailure:
		CALL	CheckCriticalFailure ; Check whether there has been any critical
		CMP		AL, FAILURE			 ; in last iteration of the loop
		JE		InitializeRemoteUnit ; If so, re-initialize the remote unit
		;JNE	LookAtNextEvent		 ; If not, look at the next event in the event
									 ; queue
LookAtNextEvent:		
		CALL	DequeueEvent		; Dequeue an event from event queue to handle
        XOR     BX, BX				; Get the key code for the event in the queue
		MOV		BL, AH				
        SHL     BX, BYTES_PER_WORD_SHIFT	; Shift index to get index into word
											; sized event table
HandleEvent:
        MOV     CL, REMOTE_ERROR	; If the event is a serial error, error function
									; will find that the error is from the remote
									; and not motor unit
		CALL	CS:EventTable[BX]	; addr of struc with events

		JMP		CheckForSystemFailure	; Loop again to continue processing remote
										; unit status and errors		

CODE    ENDS

;the data segment

DATA    SEGMENT PUBLIC  'DATA'
; to set up Dgroup
DATA    ENDS




;the stack

STACK   SEGMENT STACK  'STACK'

                DB      80 DUP ('Stack ')       ;240 words

TopOfStack      LABEL   WORD

STACK   ENDS

        END START