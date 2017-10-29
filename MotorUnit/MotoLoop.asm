; Maitreyi Ashok
; Section 1 – Richard Lee

        NAME    MotoLoop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 MotorLoop                                  ;
;                             Motor Unit Main Loop                           ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; This file contains the main loop for the Motor Unit of the RoboTrike that handles
; serial interface and motors.
;
; Table of Contents:
; MAIN - main loop of RoboTrike motor unit handling serial interface and motors
;
; Revision History:
;     11/29/2016	Maitreyi Ashok	Basic functional specification/pseudocode
;	  12/07/2016	Maitreyi Ashok	Wrote code
;	  12/08/2016	Maitreyi Ashok	Updated comments

$INCLUDE(General.inc)
$INCLUDE(EvQueue.inc)
$INCLUDE(Keycodes.inc)

CGROUP  GROUP   CODE
DGROUP  GROUP   DATA, STACK


CODE	SEGMENT PUBLIC 'CODE'
        

        ASSUME  CS:CGROUP, DS:DGROUP
        EXTRN   InitMotorLoopParts:NEAR		; Initialize necessary peripherals
											; and routines for motor unit
        EXTRN   CheckCriticalFailure:NEAR	; Check if critical failure occurred
        EXTRN   DequeueEvent:NEAR			; Dequeue event from the event queue
        EXTRN   EventTable:WORD				; Event table for how to handle 
											; different events
        EXTRN   ParsingError:BYTE			; Error string for any parsing error
        EXTRN   SerialPutString:NEAR		; Put a string onto serial channel
		EXTRN	SerialPutChar:NEAR			; Send motor error character as needed
        
; MAIN			
; Description: 		The motor unit of the RoboTrike consists of a 3 wheeled
;					vehicle with a laser and a motor on each laser to move in
;					any direction and at any speed. The motor unit handles the 
;					serial interface and the motors. Commands sent from the remote 
;					unit are handled and parsed to control various functions of 
;					the motors. Then, any resulting status updates or errors are 
;					sent back through the serial interface to the remote unit to 
;					be displayed and handled accordingly.
; 
; Global Variables: None
;
; Input:			Serial data is passed to the motor unit specifying what commands
;					to execute. This input is based on user input on the keypad
;					to set various speeds, move in various directions, or to fire
;					or turn off the laser.
;					The commands are of the form:
;						To set Speed to a value				S#<return>
;								where # is between 0 and 32767
;						To increment/decrement speed		V#<return>
;								where # is between -32767 and 32767
;						To increment/decrement the angle	D#<return>
;								where # is between -32767 and 32767
;						To turn on laser					F<return>
;						To turn off laser					O<return>
;					For more details on keypad input and serial transmission of it,
;					see complete system functional specification.		
; Output:			Motor commands are executed based on information of what to
;					do. The speed can be set to any value between 0 and 65534,
;					and the keypad allows for setting this speed in increments
;					of ± 2 and ±20 percent as well as a discrete speed value for every
;					10 percent of the maximum speed. The direction is set relative to
;					the current orientation, and can be incremented or decremented
;					in 5° or 50° increments as well as set to be one of 11 discrete
;					direction values. The laser can also be turned on or off from
;					the keypad. In general (from System Description), The DC motor 
;                   drivers are connected to port B of an 8255. This device latches 
;                   the data written to it and holds it on the output lines. Each motor 
;                   may be run clockwise or counter clockwise as determined by one bit 
;                   of Port B for each motor. The laser is connected to a driver 
;                   on port B of an 8255. When activated, the laser is powered.
;					Any status for setting speed/direction as well as any errors
;					that occur in the motor unit are sent back to the remote
;					unit where it is displayed. For more details on this, see
;					system complete functional specification.
;
; User Interface: 	The motor unit interfaces with the user by sending serial data
;					through the serial channel that can be viewed in any serial
;					terminal. In addition, the motor unit sends back status and
;					any errors it runs into that the remote unit can then display
;					for the user to know. Also, based on commands sent from the
;					serial interface, the RoboTrike can turn the laser on or off,
;					move in any direction using holonomic motion, and move at
;					any speed entered by the user. For more details on complete
;					user interface, see system functional specification.
;
; Error Handling:	If an enqueue is attempted when the EventQueue is already full
;					the system goes into critical failure mode and is reset. If
;					an invalid command is passed in to execute, the system detects
;					the error as a parsing error and sends a parsing error message
;					back to the remote unit to display. If there is a serial error
;					in the motor unit, the LSR value that is associated with the error
;					is also sent back to the remote unit to display.
;
; Algorithms:		A holonomic motion algorithm is used to control the motion of
;					the RoboTrike. The speed and direction to move in are used
;					to get the pulse widths for each motor, by using a vector of
;					powers for each motor, as Pulse Width = F • v. Then, for the
;					duration of each motor's pulse width, it is turned on and 
;					outside of that duration, the motor is turned off. Using this
;					and by controlling the direction of the motors based on the
;					sign of the pulse width, the RoboTrike can be made to move 
;					in any direction at any speed without turning.
;
; Data Structures:	An EventQueue is used as a queue of all events to handle in 
;					motor unit. Tables are used to implement the transition tables 
;					as well	as token tables for a Mealy State Machine to parse 
;					commands from the remote unit in order to control the motors.
;
; Limitations:		The motor unit does not handle the turret elevation or
;					rotation and has no option for modifying these. In addition,
;					there is no handshaking between this and the remote unit to
;					ensure that status sent back is actually received or that 
;					the complete commands from the remote unit are received here.
;					Also, the motor unit does not allow for paths to be downloaded
;					to it to be executed in sequence and requires paths to be 
;					entered step by step on the remote unit by the user using
;					the keypad. Also, the motor unit does not have any feedback
;                   of whether it is moving how it is directed to move.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/29/16	Maitreyi Ashok	Wrote functional specification/pseudocode
;                   12/07/16    Maitreyi Ashok  Wrote code
;					12/08/16	Maitreyi Ashok	Updated comments

START:  

MAIN:
InitializeMotorUnit:
        MOV     AX, DGROUP          ;initialize the stack pointer
        MOV     SS, AX
        MOV     SP, OFFSET(DGROUP:TopOfStack)

        MOV     AX, DGROUP          ;initialize the data segment
        MOV     DS, AX
        
		CALL	InitMotorLoopParts	; Initialize peripherals and other routines
									; for motor unit
CheckForSystemFailure:
		CALL	CheckCriticalFailure	; Check if event queue became full
		CMP		AL, FAILURE				; If it did reinitialize the motor unit
		JE		InitializeMotorUnit		; to reset everything
		;JNE	LookAtNextEvent			; If not, then look at the next event
LookAtNextEvent:
        CALL    DequeueEvent			; Dequeue an event to handle from queue
		XOR     BX, BX					; Get the key code of the event
		MOV		BL, AH					; to process
        SHL     BX, BYTES_PER_WORD_SHIFT	; Shift index to get index into word
											; sized event table
		CMP		BX, SERIAL_ERROR * BYTES_PER_WORD	
										; Check if the error was a motor serial error
		JNE		HandleEvent				; If not, go straight to handle event
		;JE		SendMotorError			; If it was, send the prefix of the motor
SendMotorError:							; error
		MOV		AL, 'M'
		CALL	SerialPutChar			; Send first that the error is a motor
		JC		SendMotorError			; error for remote parser to handle 
		;JNC	HandleEvent				; accordingly
HandleEvent:
		CALL	CS:EventTable[BX]	   	; Look in table of how to handle this event
		CMP     AX, FALSE              	; If there was no error in handling, move on
		JE		CheckForSystemFailure	; to next iteration of main loop
		;JNZ	ErrorInParsing
ErrorInParsing:							; If there was an error in the handling
		MOV		AX, OFFSET(ParsingError)	; Get the error message for a 
        PUSH    CS							; parsing error into argument for
        POP     ES							; SerialPutString
		CALL	SerialPutString			; Put the parsing error message into 
										; serial channel
		JMP		CheckForSystemFailure	; Go back to next iteration of loop to
										; handle next event
        
CODE    ENDS		
;the data segment

DATA    SEGMENT PUBLIC  'DATA'
; present to set up DGroup
DATA    ENDS

;the stack

STACK   SEGMENT STACK  'STACK'
                DB      80 DUP ('Stack ')       ;240 words
TopOfStack      LABEL   WORD

STACK   ENDS

        END		START