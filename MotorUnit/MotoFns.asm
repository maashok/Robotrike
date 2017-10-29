; Maitreyi Ashok
; Section 1 – Richard Lee

        NAME    MotoFns

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   MotorFns                                 ;
;                              Motor Unit Routines                           ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; This file contains the main loop for the Motor Unit of the RoboTrike that handles
; serial interface and motors.
;
; Table of Contents:
; Routines
; InitMotorLoopParts - Initializes peripherals and routines for motor unit
; Tables
; EventTable - Contains handling function for each type of event in motor unit
;
; Revision History:
;     11/29/2016	Maitreyi Ashok	Basic functional specification/pseudocode
; 	  12/07/2016	Maitreyi Ashok	Wrote code
;	  12/08/2016	Maitreyi Ashok	Updated code


CGROUP  GROUP   CODE


CODE	SEGMENT PUBLIC 'CODE'


        ASSUME  CS:CGROUP
		
        EXTRN   InitEventQueue:NEAR			; Initialize event queue to handle events
        EXTRN   InitCS:NEAR					; Initialize chip selects
        EXTRN   ClrIRQVectors:NEAR			; and clear interrupt vector table entries
        EXTRN   InstallTimer0Handler:NEAR	; Install handler for and initialize
        EXTRN   InitTimer0:NEAR				; timers for motors
        EXTRN   InitInt2:NEAR				; Install handler for and initialize
        EXTRN   InstallInt2Handler:NEAR		; interrupt for serial i/o
        EXTRN   InitSerial:NEAR				; Initialize serial routines
        EXTRN   InitMotors:NEAR				; Initialize motor routines
        EXTRN   InitParallelPort:NEAR		; Initialize parallel port for motors
        EXTRN   InitSerialParse:NEAR		; Intialize serial parser FSM
        EXTRN   doNOP:NEAR					; Function to do nothing for invalid
											; key codes
        EXTRN   ParseSerialChar:NEAR		; Serial character parser
        EXTRN   SerialPutString:NEAR		; Place string on serial channel
        
; InitRemoteLoopParts			
; Description: 		This function sets up all the parts needed for the motor loop
;					to work. This involves initializing the event queue to process
;					and store events to handle. In addition, the chip selects are
;					initialized and interrupt vector table vectors cleared. The
;					timer 0 and int 2 interrutps are set up to allow for motor
;					as well as serial handling, respectively. The motors, parallel
;					port, and serial routines are initialized individually.
;
; Operation:  		This function sets up all the above detailed parts of the motor
;					unit by calling dedicated functions for each of these tasks.
;					During this initialization, interrupts are not allowed as they
;					may cause the initializations to be affected. 
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	None
; Shared Variables: None
; Global Variables: None
;
; Input:			None
; Output:			All interrupts are started
;
; Error Handling:	This function is used to handle a critical failure in the remote
;					unit if the event queue fills up by resetting the entire system.
;
; Algorithms:		None
; Data Structures:	None
;
; Registers Changed: None
; Stack Depth:		1 word
;
; Limitations:		This function does not allow for variable initialization based
;					on any initial conditions.
;
; Author:			Maitreyi Ashok
; Last Modified:	12/07/16	Maitreyi Ashok	Wrote code
;					12/08/16	Maitreyi Ashok	Updated comments

InitMotorLoopParts  PROC        NEAR
                    PUBLIC      InitMotorLoopParts
InitQueue:
        CLI							; Disallow interrupts during initialization
		CALL	InitEventQueue		; Initialize event queue to hold events
Init80188General:
		CALL	InitCS				; Initialize chip selects and clear
		CALL	ClrIRQVectors		; interrupt vector table
InitInterrupts:
		CALL	InstallTimer0Handler	; Set up interrupt vector table to handle
		CALL	InitTimer0				; timer 0 interrupts and set up timer 0
        CALL    InitInt2				; Set up interrupt vector table to handle
        CALL    InstallInt2Handler		; int 2 interrupts and set up int 2
InitPeripherals:
		CALL	InitSerial			; Initialize serial channel routines
		CALL	InitMotors			; Initialize motors
        CALL    InitParallelPort	; Initialize parallel port
DoneInit:
        CALL    InitSerialParse		; Initialize the remote parser
        STI							; Allow interrutps to start happening again
		RET
InitMotorLoopParts	ENDP

; EventTable
; Description:		This table contains the event handler for each type of event
;					in the EventQueue. If the event is of type NOP_EVENT or 
;					KEY_EVENT, the function NOPs and does not do anything. If the 
;					event is of type SERIAL_DATA_RECEIVED, the data	received is 
;					parsed by the remote parser to send commands to the motors. 
;					If the event is of type SERIAL_ERROR, the error is sent back
;					to the remote unit as a string.
;
; Author:			Maitreyi Ashok
; Last Modified:	12/08/16

EventTable	LABEL	WORD
			PUBLIC	EventTable
; 	DW	eventHandler
    DW	doNOP					; Event queue empty
	DW	doNOP					; Key pressed on keypad
	DW	ParseSerialChar			; Serial command from remote unit
	DW	SerialPutString        	; Serial error on motor unit

; This contains the message to be sent as a serial string back to the remote
; unit if a parsing error occurs
ParsingError    LABEL   BYTE
                PUBLIC  ParsingError
    DB  'P', 0
    
CODE    ENDS

        END