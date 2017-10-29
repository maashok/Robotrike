 ; Maitreyi Ashok
; Section 1 – Richard Lee

        NAME    SerParse

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 Serial Parser                              ;
;                            Serial Parser Routines	                         ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; This file contains the serial parsing functions for the RoboTrike to parse
; passed input and execute commands it receives and decodes.
;
; Table of Contents:
; Functions:
; Local
; GetToken - Returns token type and value for any input character
; AddDigit - Add digit based on serial character to number for command
; SetError - Set error flag and reset
; DoneError - Reset error flag when receive End of Command
; SetSign - Set sign for value in command
; SetAbsSpeed - Set absolute speed of RoboTrike
; SetRelSpeed - Set speed of RoboTrike compared to current speed
; SetDirection - Set direction of RoboTrike travel
; SendInfo - Sends status for parameter set to remote unit
; RotTurret - Rotate the turret to an absolute angle or relative to current
; ElevTurret - Elevate laser on turret to absolute angle
; FireLaser - Turns laser on
; LaserOff - Turns laser off
; ResetStates - Resets to idle state
;
; Global
; ParseSerialChar - Processes character passed in as part of serial command
; InitSerialParse - Initializes serial parsing shared variables
; 
; Tables:
; StateTable - State transition table of TRANSITION_ENTRY elements
; TokenTypeTable - Contains token types of all ASCII characters
; TokenValueTable - Contains token values of all ASCII characters
; 
; Macros:
; TRANSITION - Builds TRANSITION_ENTRY STRUCs for state table using arguments
; TABLE - creates tables of token types and token values
; TABENT - Stores either token type or value as entry in the table
;
;
; Revision History:
;
;     11/21/2016	Maitreyi Ashok	Functional Specification/Pseudocode
;	  11/23/2016	Maitreyi Ashok	Wrote code
;     11/24/1016    Maitreyi Ashok  Fixed overflow errors and updated comments


CGROUP  GROUP   CODE
DGROUP  GROUP   DATA

$INCLUDE(InitPara.inc)      ; Contains parallel port intiialization
$INCLUDE(SerPars.inc)       ; Contains serial parsing specific definitions
$INCLUDE(General.inc)       ; Contains data size definitions
$INCLUDE(MOTORS.inc)        ; Contains motor speed/angle definitions

CODE	SEGMENT PUBLIC 'CODE'


        ASSUME  CS:CGROUP, DS:DGROUP
		
		EXTRN	SetMotorSpeed:NEAR          ; Used to set speed/direction of movement
		EXTRN	GetMotorSpeed:NEAR          ; Used to get current speed
		EXTRN	GetMotorDirection:NEAR      ; Used to get current direction
		EXTRN	SetLaser:NEAR               ; Sets the laser
		EXTRN	SetTurretAngle:NEAR         ; Sets the absolute turret angle
		EXTRN	SetRelTurretAngle:NEAR      ; Sets the relative turret angle
		EXTRN	SetTurretElevation:NEAR     ; Sets turret elevation
		EXTRN	doNOP:NEAR					; doNOP function
        EXTRN   SerialPutChar:NEAR
        EXTRN   Dec2String:NEAR
        EXTRN   SerialPutString:NEAR

; ParseSerialChar
; Description: 		This function recieves a character from serial input
;					and processes the character as a serial command. The character 
;					inputs follow a format during repeated calls to the function 
;					of a letter representing a command, an optional positive or 
;					negative sign depending on the command, and 16 bits maximum 
;					for a number with decimal digits, with the entire command 
;					delimited by a <Return>. Blank	spaces and white lines are 
;					ignored, and all other characters are considered invalid. The
;					function also returns whether there was a parsing error. Refer
;					to complete functional specification for full format of serial 
;					commands.
;
; Operation:  		This function uses a state machine to parse characters from
;					serial input as a command for the RoboTrike.  This is
;					done using a Mealy Finite State Machine which dictates
;					what state to enter from the current state and what
;					action to perform during this transition based on the
;					serial character input. The function is called repeatedly
;					and performs an action and changes the state during each call,
;					after which it returns and is called again later when the
;					next serial character is available. If there was an error during
;                   the parsing, an error value is returned to the caller function.
;                   If the error was due to overflow, a transition is made to
;                   the error state instead of the normally consecutive state.
;                   However, if there was an error due to the elevation angle being
;                   out of bounds or the absolute speed being negative, these errors
;                   are still reported but the state machine is not set to the
;                   error state since the end of command was already reached with
;                   this state and we want to handle the next command entered
;                   from the idle state. 
;
; Arguments:   		char [AL] - next serial character passed in, only valid 
;							characters are S, s, V, v, D, d, T, t, E, e, F, f, O, 
;                           o, '', ' ', '\n', carriage return, +, - and '0'-'9'
; Return Value:		error [AX] - nonzero value if parsing error due to argument,
;							zero value if no error
;
; Local Variables:	tokenType [DX] - token group character passed in is part of
;					tokenVal [CH/AL]  - value of token referred to by character passed in
;					tableEntry [BX] - offset of table entry referred to by current state
;							and input character
; Shared Variables: error (R/W) - stores whether an error has occurred in parsing
;					state (R/W)- stores current state in the parsing of command
; Global Variables: None
;
; Input:			Characters coming in from serial I/O
; Output:			Motor commands are executed
;
; Error Handling:	If an error occurred in the parsing, an error flag is returned
;					to the main loop.
;
; Algorithms:		A Mealy state machine is used to parse serial commands
; Data Structures:	Table of STRUCs containing action and next state associated
;					with each pair of current state and character input type
;
; Registers Changed: AX, DX, BX, CH, flags
; Stack Depth:		1 word
;
; Limitations:		The function does not send a specific error message to the
;					main loop and just specifies whether there was an error or 
;					not
;
; Author:			Maitreyi Ashok
; Last Modified:	11/21/16	Maitreyi Ashok	Functional Specification/Pseudocode
;                   11/22/16    Maitreyi Ashok  Wrote code, Handle overflow errors
;                                               and elevation errors
;                   11/24/16    Maitreyi Ashok  Updated comments

ParseSerialChar    	PROC        NEAR
					PUBLIC      ParseSerialChar
GetTokenForInput:
		CALL	GetToken                    ; Get token info for the passed in 
		MOV		DL, AH                      ; character
        XOR     DH, DH                      ; Save token type and token value
		MOV		CH, AL
ComputeTransition:
		MOV		AX, NUM_TOKEN_TYPES         ; Get index of current state in
		MUL		state                       ; state table
		ADD		AX, DX                      ; Add in index from start of current
                                            ; state for the current token type
		IMUL	BX, AX, SIZE TRANSITION_ENTRY
                                            ; Get actual offset from index
                                            ; by multiplying by size of each entry
DoAction:
        PUSH    BX                          ; Save offset in state table to be used
                                            ; to find next state to go to
		MOV		AL, CH                      ; Call action routines with the token
		CALL	CS:StateTable[BX].ACTION    ; value as an argument
        MOV     AX, error                   ; See whether error occurred to use
                                            ; as return value
        CMP     CX, OVERFLOW_ERROR          ; If the error was an overflow error
        JE      DoTransitionToError         ; move to error state next instead of
                                            ; the state listed in state table
        ;JNE    DoNormalTransition          ; Otherwise, go to next state as expected
DoNormalTransition:
        MOV     error, NO_ERROR             ; Reset the error flag (will be set again in
                                            ; next iteration if still in error state)
        POP     BX                          ; Get offset in state table and use it
		MOV		BL, CS:StateTable[BX].NEXTSTATE
        MOV     state, BL                   ; to find and save next state to go to
        JMP     EndParsing                  ; Then we are done parsing this character
DoTransitionToError:
        POP     BX                          ; Remove the offset in table even though
                                            ; not using it to preserve stack as it was
        MOV     state, ST_ERROR             ; Move automatically to error state if
                                            ; there was overflow error in AddDigit
        ;JMP    EndParsing
EndParsing:
		RET
ParseSerialChar	ENDP

; InitSerialParse
; Description: 		This function intializes the serial parser routines by
;					setting up the shared variables used by the action routines
;					and serial parsing handler. This is done by resetting
;					the number and sign of the command, as well as starting
;					the state machine from the initial idle state without any
;                   errors
;
; Operation:  		This function uses the ResetStates method to reset the value
;					in the shared variables for the number and sign associated
;					with the command. The sign consists of both whether a positive
;					or negative sign was entered as a character, as well as
;					whether no sign character was entered at all. In addition,
;					the finite state machine is started from the initial idle
;					state from which it will wait for a command letter to be 
;					entered. Also the error flag is reset since there are no
;                   errors when we start the state machine.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	None
; Shared Variables: state (W) - stores current state in the parsing of command
; Global Variables: None
;
; Input:			None
; Output:			None
;
; Error Handling:	None
;
; Algorithms:		A Mealy state machine is used to parse serial commands
; Data Structures:	None
;
; Registers Changed: CX (In ResetStates)
; Stack Depth:		0 words
;
; Limitations:		This function does not allow for starting from various
;					versions of an idle state depending on intial conditions
;
; Author:			Maitreyi Ashok
; Last Modified:	11/21/16	Maitreyi Ashok	Functional specification/pseudocode
;                   11/22/16    Maitreyi Ashok  Wrote code
;                   11/24/16    Maitreyi Ashok  Updated comments


InitSerialParse    	PROC        NEAR
					PUBLIC      InitSerialParse
ResetSharedVariables:
		CALL		ResetStates         ; Reset sign and currNum shared variables
		MOV			error, NO_ERROR     ; Start out with no error
StartIdle:
		MOV			state, ST_IDLE      ; Start in idle state
		RET
InitSerialParse	    ENDP

; GetToken
; Description: 		This function finds the token class and value for the
;					character passed in. The character is truncated to 7 bits
;					since the high bit is unused in standard ASCII code. The
;					possible token classes are Digit, Positive Sign, Negative Sign, 
;                   End of Command, Absolute Speed, Relative Speed, Direction, Turret
;                   Rotation, Turret Elevation, Laser Fire, Laser Off, Ignore, and 
;                   Other. The token values	are dependent on the numerical value 
;                   of the character or how the token value is used in the associated 
;                   character handler function.
;
; Operation:  		The function looks up the passed character in a token type
;					table and a token value table to get these values for
;					any character passed in easily. These types and values can
;					then be returned to the caller (ParseSerialChar) for use
;					in finding the next state to transition to as well as what
;					action to perform during the transition.
;
; Arguments:   		char [AL] - character to find token value/type for
; Return Value:		tokenVal [AL] - token value for character
;					tokenType [AH] - token type for character
;
; Local Variables:	tables [BX] - table pointer pointing to look up tables
; Shared Variables: None
; Global Variables: None
;
; Input:			None
; Output:			None
;
; Error Handling:	None
;
; Algorithms:		Table lookup using the XLAT instruction
; Data Structures:	Uses a table with token values and one with token types
;
; Registers Changed: AX, BX, flags
; Stack Depth:		0 words
;
; Limitations:		This function does not handle non standard ASCII codes or
;					character codes based on any other convention.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/21/16	Maitreyi Ashok	Functional Specification
;                   11/22/16    Maitreyi Ashok  Wrote Code
;                   11/24/16    Maitreyi Ashok  Updated comments

GetToken    	PROC        NEAR

InitGetToken:
		AND			AL, TOKEN_MASK              ; Get only low 7 bits of token
                                                ; since top bit is unused in ASCII
                                                ; characters
		MOV			AH, AL                      ; Save value of ASCII character
TokenTypeLookup:
		MOV			BX, OFFSET(TokenTypeTable)  ; Get token type from token type
		XLAT		CS:TokenTypeTable           ; table
		XCHG		AH, AL                      ; Move token type to return value
                                                ; register and prepare to look up
                                                ; token value
TokenValueLookup:
		MOV			BX, OFFSET(TokenValueTable) ; Get token value from token
		XLAT		CS:TokenValueTable          ; value table into return value register
EndGetToken:
		RET                                     ; Done looking up token information
GetToken		ENDP

; AddDigit
; Description: 		This function adds in a digit from the command to the value
;					portion of the command. This is done by finding the signed
;					value of the new digit based on what sign character was
;					entered in previously. This signed value is then added to
;					the already signed value of the current number times 10 to
;					shift it one digit to the left. If at any point in this operation,
;                   overflow occurs, the error flag is set and an overflow error
;                   is returned to the caller. Otherwise, no error is returned to
;                   the caller.
;
; Operation:  		This function adds an originally ASCII digit charater that
;					is part of a command to control the motors to the numerical
;					decimal representation one character as a time as it is received
;					on the serial channel. The currNum value is kept signed, adding
;					in the signed version of the new digit based on whether the
;					sign has been set positive or negative. The old value is shifted
;					one digit to the left and the new digit is added in to
;					get the decimal representation of the value in the command.
;                   If the addition or multiplication to shift decimal digit
;                   overflows, the error flag is set and appropriate value returned
;                   to the caller
;
; Arguments:   		digit [AL] - next digit in command to add to total value for command
; Return Value:		error [CX] - returns OVERFLOW_ERROR if an overflow error occurred,
;                               returns NO_ERROR otherwise
;
; Local Variables:	digit [BX] - next digit in command to add to total value for command
;                   currNumShifted [AX] - Current argument shifted by DEC_BASE
;                               to accommodate next digit
; Shared Variables: currNum (R/W)- stores number constructed so far in parsing of command
; Global Variables: None
;
; Input:			A digit included as character in serial channel as part of command
; Output:			Digit included as part of command to control motors
;
; Error Handling:	If overflow occurs at any step of adding in next digit,
;					error is set.
;
; Algorithms:		abc...yz = a*10^n + b*10^n-1 + c*10^n-2 + ... + y*10^1 + z*10^0
;					Uses this fact to convert decimal digits in ASCII representation
;					to a decimal number
; Data Structures:	None
;
; Registers Changed: AX, BX, CX, flags
; Stack Depth:		0 words
;
; Limitations:		This function only allows for adding a decimal digit to a
;					value and does not work for digits in any other base.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/21/16	Maitreyi Ashok	Functional specification/pseudocode
;					11/23/16    Maitreyi Ashok  Wrote code, added return value
;                   11/24/16    Maitreyi Ashok  Updated comments

AddDigit    	PROC        NEAR

SignedDigitAndShiftOld:
		IMUL    sign                ; Convert parsed digit to signed value by
                                    ; multiplying by sign character passed in
                                    ; before (or assuming positive if none)
        MOV     BX, AX              ; Save parsed digit to be used later
        MOV     AX, currNum         ; Get the digits passed in previously and
		IMUL	AX, AX, DEC_BASE    ; shift to left by decimal system base value
        MOV     currNum, AX         ; since those are all one place higher when
                                    ; the next digit is added in
		JO      AddDigitError       ; If there was overflow, set error
        ;JNO    AddNextDigit
AddNextDigit:
        ADD     currNum, BX         ; Add in the new digit to the current value
        JNO     NoOverflowAddDigit  ; If there was no overflow, we are done
        ;JO     AddDigitError       ; If there was, set error
AddDigitError:
        CALL    SetError            ; Set error flag and reset values
        MOV     CX, OVERFLOW_ERROR  ; Retun overflow error to caller
        JMP     DoneAddDigit        ; Done with adding digit (unsuccessfully)
NoOverflowAddDigit:
        MOV     CX, NO_ERROR        ; Return no overflow error to caller
        ;JMP    DoneAddDigit        ; Done with adding digit (successfully)
DoneAddDigit:
		RET
AddDigit		ENDP

; SetError
; Description: 		This function is called if there was an error in the
;					serial parsing in any stage in the traversing through the
;					finite state machine. The function resets all shared variables
;					to start from the idle state again and sets the error flag.
;
; Operation:  		This function signifies to the parsing function that an
;					error has occurred by setting the error flag which is also
;					checked in that function to return to main loop. In addition,
;					the currNum and sign variables are reset since the old values
;					will not be used anymore when parsing of the next command
;					is started. Due to the error, the current values and associated
;					command are invalid and not used.
;
; Arguments:   		None
; Return Value:		overflowError [CX] - zero if no overflow error, nonzero if there
;                               was overflow error (Always reset to zero in ResetStates)
;
; Local Variables:	None
; Shared Variables: error (W)- stores whether an error has occurred in parsing
; Global Variables: None
;
; Input:			Some invalid command entered in serial I/O
; Output:			None
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	None
;
; Registers Changed: CX (Reset in ResetStates), flags
; Stack Depth:		0 words
;
; Limitations:		This function does not take into account the cause of the error
;					and just sets a generic error flag.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/21/16	Maitreyi Ashok	Functional specification/pseudocode
;					11/22/16    Maitreyi Ashok  Wrote code
;                   11/24/16    Maitreyi Ashok  Updated comments

SetError    	PROC        NEAR

SetErrorFlag:
		MOV			error, GENERAL_ERROR    ; Sets error flag to any general error
ResetVariables:
		CALL		ResetStates             ; Reset shared variables
		RET
SetError		ENDP

; DoneError
; Description: 		This function is called if there was an error in the serial
;                   parsing at any stage but an end of command character was 
;                   entered later, meaning that the error has ended and that
;                   normal parsing should start taking place again. This function
;                   resets the error flag for this to happen.
;
; Operation:  	    This function signifies to the main loop that the error has
;                   ended once an end of command is received since it resets the 
;                   error flag that is returned to the main loop in every call to 
;                   ParseSerialChar. This way the next non error command will be 
;                   parsed correctly.
;
; Arguments:   		None
; Return Value:		overflowError [CX] - returns 0 since no error to report
;
; Local Variables:	None
; Shared Variables: error (W)- stores whether an error has occurred in parsing
; Global Variables: None
;
; Input:			End of command carriage return character entered.
; Output:			None
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	None
;
; Registers Changed: CX (Reset in ResetStates), flags
; Stack Depth:		0 words
;
; Limitations:		This function does not do a different action based on the cause
;                   of the original error.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/23/16    Maitreyi Ashok      Wrote code
;                   11/24/16    Maitreyi Ashok      Updated comments/func spec

DoneError    	PROC        NEAR

ReSetErrorFlag: 
		MOV			error, NO_ERROR     ; Set that no error anymore
        CALL        ResetStates         ; Reset all shared variables for next use
		RET
DoneError		ENDP

; SetSign
; Description: 		This function is called if there is a positive or negative
;					sign character entered after the command character. The function 
;					sets the sign flag shared variable to positive 1 or negative
;					1 depending on the token value of the sign character used. 
;					It also saves that a sign character has been entered.
;
; Operation:  		The function saves that a sign has been entered to
;					be used in two scenarios. It first saves a +/- 1 as
;					the sign variable, so that the sign can easily be multiplied
;					with the digits passed in to get a positive or negative number. 
;					In addition	it is saved that a sign character was entered for 
;					cases in which there is a distinction in the motor command
;					between no sign character having been entered and a sign character 
;					having been entered.
;
; Arguments:   		signToken [AL] - token value of sign character used (+/- 1)
; Return Value:		overflowError [CX] - returns 0 since no error to report
;
; Local Variables:	None
; Shared Variables: sign (W)- stores sign of value in command, +1 if plus sign or
;							no sign character entered, -1 if negative sign character
;							entered
;					noSign (W)- stores 0 if no sign character entered, 1 if sign
;							character entered
; Global Variables: None
;
; Input:			Positive or negative sign character entered on serial channel
; Output:			None
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	None
;
; Registers Changed: CX, flags
; Stack Depth:		0 words
;
; Limitations:		None
;
; Author:			Maitreyi Ashok
; Last Modified:	11/21/16	Maitreyi Ashok	Functional Specification/Pseudocode
;                   11/22/16    Maitreyi Ashok  Wrote code
;                   11/24/16    Maitreyi Ashok  Updated comments

SetSign    	PROC        NEAR

SetSignVars:
		MOV			sign, AL            ; Sets sign value to +/- 1 depending on which
                                        ; sign character was entered
		MOV			noSign, GOT_SIGN    ; Set noSign variable to GOT_SIGN since
                                        ; a sign character was actually entered and
                                        ; it is not implicit
        XOR         CX, CX              ; Clear return value since no overflow error
		RET                             ; Done setting the sign
SetSign		ENDP

; SetAbsSpeed
; Description: 		This function sets the speed of the motors to an absolute
;					value depending on serial channel input. This is done by
;					checking that the speed value is within 15 bits as expected
;					andd then setting the speed with this value and an argument
;                   of no change for the angle. The shared variables are then reset
;                   since the end of the command has been received. If the argument
;                   to set speed was more than 15 bits (had MSB set), then the
;                   ABS_SPEED error value is returned.
;
; Operation:  		The function operates by checking the sign bit of the word
;					value of currNum. If the sign bit is set, the value is 16 bits
;					which is invalid for this command. Otherwise, SetSpeed is
;					called to set the speed of the RoboTrike by doing PWM 
;					on the motors. In SetSpeed, the speed is set to the
;					passed in value while the angle of travel will not be changed.
;                   The shared variables are reset since the end of command was
;                   reached to prepare for the next command, and the error return
;                   value is set depending on whether there was an error in the
;                   argument or not.
;
; Arguments:   		None
; Return Value:		absSpeedError [CX] - returns ABS_SPEED_ERROR if argument to set
;                               speed at was negative/more than 15 bits. Otherwise
;                               returns NO_ERROR
;
; Local Variables:	None
; Shared Variables: currNum (R)- stores number constructed so far in parsing of command

; Global Variables: None
;
; Input:			Serial command of form S# and carriage return character
; Output:			RoboTrike motors set to modulate pulse widths to move at
;					specified speed
;
; Error Handling:	If the value passed in is a 16 bit value, set the error flag
;					in the process.
;
; Algorithms:		Uses PWM of the motors to get the RoboTrike to move at some speed
; Data Structures:	None
;
; Registers Changed: AX, BX, CX, flags
; Stack Depth:		0 words
;
; Limitations:		This function does not allow for the speed to be absolutely
;					set to its full range of values, restricting the speeds to be
;					15 bits whereas SetSpeed accepts arguments for speed ranging
;					from 0 to 65534.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/21/16	Maitreyi Ashok	Functional Specification/Pseudocode
;					11/23/16    Maitreyi Ashok  Wrote code
;                   11/24/16    Maitreyi Ashok  Updated comments


SetAbsSpeed    	PROC        NEAR

FindParsedSpeed:
		MOV		AX, currNum             ; Get speed passed in to move at
        JS      AbsSpeedNegValue        ; If it was negative, or had more than 15 bits
                                        ; handle the error
        ;JNS    SetParsedSpeed          ; Otherwise set the parsed speed
SetParsedSpeed:     
		MOV		BX, ANGLE_NO_CHANGE     ; Do not change angle of movement when
                                        ; setting speed
		
        PUSH    AX
		CALL	SetMotorSpeed           ; Set speed with parsed argument and angle
                                        ; no change
        POP     AX
        MOV		BL, SPEED_STATUS
        CALL	SendInfo
        CALL    ResetStates             ; Reset shared variables since EOC reached
        JMP     DoneSetAbsSpeed         ; Done setting absolute speed
AbsSpeedNegValue:
        CALL    SetError                ; If error, set error flag and return
        MOV     CX, ABS_SPEED_ERROR     ; error value
        ;JMP    DoneSetAbsSpeed
DoneSetAbsSpeed:
        RET
        
SetAbsSpeed		ENDP

; SetRelSpeed
; Description: 		This function sets the speed of the motors to a relative
;					value depending on serial channel input and the current
;					speed of the RoboTrike. The current speed and the positive/
;					negative relative change are added and then the speed is set
;					with this value. If the speed becomes out of the range of the
;                   arguments to SetSpeed, the value is capped at the maximum
;                   or minimum value depending how it overflowed.
;
; Operation:  		The function operates by finding the current speed of the 
;					RoboTrike. This speed is added to the change in speed just
;					received through the serial channel. If this speed is out of
;					the ranges of the arguments SetSpeed expects, the speed
;					is adjusted to meet this range by either capping at the maximum
;                   value or minimum value. In addition, it is checked that this
;                   addition does not result in the SPEED_NO_CHANGE value. If it
;                   does, the speed is changed to MAX_SPEED. Then, SetSpeed is called
;					to set the net speed of the RoboTrike by doing PWM 
;					on the motors. In SetSpeed, the speed is set to the
;					passed in value while the angle of travel will not be changed.
;
; Arguments:   		None
; Return Value:		overflowError [CX] - returns 0 since no error to report
;
; Local Variables:	speed [AX] - Total speed of RoboTrike, summing current speed and
;							new relative change
;                   oldSpeed [BX] - Speed of RoboTrike before this serial command
; Shared Variables: currNum (R)- stores number constructed so far in parsing of command
; Global Variables: None
;
; Input:			Serial command of form V# and carriage return character
; Output:			RoboTrike motors set to modulate pulse widths to move at
;					current speed plus/minus some relative change
;
; Error Handling:	If the sum of the original speed plus the relative change
;					is out of bounds of the argument SetSpeed expects, the
;					sum is adjusted to meet the range.
;
; Algorithms:		Uses PWM of the motors to get the RoboTrike to move at some speed
; Data Structures:	None
;
; Registers Changed: AX. BX. CX (ResetStates), flags
; Stack Depth:		0 words
;
; Limitations:		This function cannot decrease the speed in a series of finite
;					increments, but instead decreases the speed as a whole in
;					one change, and the RoboTrike does not switch speeds smoothly.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/21/16	Maitreyi Ashok	Functional Specification/Pseudocode
;					11/23/16    Maitreyi Ashok  Wrote code
;                   11/24/16    Maitreyi Ashok  Corrected overflow cases
;                                               Updated comments
SetRelSpeed    	PROC        NEAR

FindNewSpeed:
		CALL	GetMotorSpeed               ; Get the current RoboTrike speed
        MOV     BX, AX                      ; and save it for later overflow checks
		ADD		AX, currNum                 ; Add in the relative speed to increment/
                                            ; decrement by
        JO      CheckIfOveflowMatters       ; If it signed overflowed, the speed became 
                                            ; too large so fix the large speed
        ;JNO    CheckIfUnsignedOverflow     ; Otherwise, check if unsigned overflow
CheckIfUnsignedOverflow:                    ; occurred
        JC      CheckIfOveflowMatters       ; If it did (CF set), fix the large speed
        ;JNC    CheckIfTooSmall             ; If not, check if underflow occurred
CheckIfTooSmall:
        JL      FixSmallSpeed               ; If the value is less than 0, but not
                                            ; fix the small speed
        JGE     CheckIfIgnoreSpeed          ; Otherwise, check if speed is ignore speed
CheckIfOveflowMatters:
        CMP     BX, MAX_16_BIT_POSITIVE     ; If the old speed was unsigned less than
                                            ; the maximum positive number, adding it
                                            ; to the new speed (also less than the
                                            ; maximum positive number), should
        JBE     CheckIfIgnoreSpeed          ; not overflow. If that is the case
                                            ; go to check whether it is the ignore speed
        CMP     currNum, MIN_POSITIVE       ; If the current number is less than
                                            ; the minimum positive number (is negative)
                                            ; then adding it to any old speed should
        JL      CheckIfIgnoreSpeed          ; not overflow. If so go to check whether
                                            ; it is the ignore speed
        ;JGE    FixLargeSpeed               ; Otherwise, it did actually overflow
FixLargeSpeed:                              ; past the range of SetSpeed so fix
		MOV		AX, MAX_SPEED               ; the speed argument by capping it at the  max
		JMP	    SetParsedRelativeSpeed      ; Then set this speed
FixSmallSpeed:
        CMP     BX, MAX_16_BIT_POSITIVE     ; If the old speed is greater than the
                                            ; maximum postive number then adding
                                            ; even the maximum negative number 
        JA     CheckIfIgnoreSpeed           ; should not cause it to underflow
                                            ; If so, go to check whether it is the
                                            ; ignore speed
        MOV     AX, MIN_SPEED               ; Otherwise, cap the speed at the minimum
                                            ; speed since it did underflow
CheckIfIgnoreSpeed:
		CMP		AX, MAX_SPEED               ; If the speed is less than or equal
		JBE	    SetParsedRelativeSpeed      ; to the maximum speed (unsigned) then
                                            ; can set this speed
		MOV     AX, MAX_SPEED               ; Otherwise cap the speed at the max speed
SetParsedRelativeSpeed:       
        PUSH    AX
        MOV     BX, ANGLE_NO_CHANGE         ; Do not change the angle in SetSpeed
        CALL    SetMotorSpeed               ; Set the updated speed for the motors
        POP     AX
        MOV		BL, SPEED_STATUS
		CALL	SendInfo
DoneSetRelSpeed:
        CALL    ResetStates                 ; Reset shared variables since EOC reached
		RET                                 ; Done setting relative speed
SetRelSpeed		ENDP

; SetDirection
; Description: 		This function moves the RoboTrike in the specified direction
;					in degrees relative to the current direction of movement. A 
;					positive angle moves the RoboTrike in a direction in the right, 
;					whereas a negative angle moves the RoboTrike in a direction 
;					to the left. No overflow error should occur as passed in value
;                   is modified to get an angle within the range of a word but
;                   still have an equivalent angle.
;
; Operation:  		The function operates by finding the direction to move
;					the RoboTrike in. SetSpeed is called to set the direction 
;					of the RoboTrike by doing PWM on the motors with the value 
;					entered on the serial channel. In SetSpeed, the direction is 
;					set to the passed in value while the speed of travel will not 
;					be changed. Since SetSpeed accepts any 16 bit speeds and
;					finds the speed mod MAX_ANGLE to get a speed between 0 and
;					MAX_ANGLE - 1 degrees, the angle received from the serial
;					channel can be directly passed in. Any overflow in setting
;                   the new angle as old angle plus relative angle is avoided by
;                   changing the new angle to a value within -MAX_ANGLE to MAX_ANGLE
;                   since all angles +/- 360 degrees are the same.
;
; Arguments:   		None
; Return Value:		overflowError [CX] - returns 0 since no error to report
;
; Local Variables:	correctedAngle [AX] - parsed argument mod MAX_ANGLE which
;                               is equal to the parsed argument in movement
; Shared Variables: currNum (R)- stores number constructed so far in parsing of command
; Global Variables: None
;
; Input:			Serial command of form D# and carriage return character
; Output:			RoboTrike motors set to modulate pulse widths and rotate forwards
;					or backwards to move in	specified direction
;
; Error Handling:	Overflow errors in adding the new angle to the old angle are
;                   avoided by mod-ing the parsed angle by MAX_ANGLE and then adding
;                   it to the old angle. Since the old angle is also within MIN_ANGLE
;                   and MAX_ANGLE the value will not overflow.
;
; Algorithms:		Uses PWM of motors to get RoboTrike to move in some direction
; Data Structures:	None
;
; Registers Changed: AX, BX, DX, flags, CX (reset in ResetStates)
; Stack Depth:		0 words
;
; Limitations:		This function does not have an option to set the direction of
;					the RoboTrike relative to some fixed orientation.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/21/16	Maitreyi Ashok	Functional Specification/Pseudocode
;					11/23/16    Maitreyi Ashok  Wrote code
;                   11/24/16    Maitreyi Ashok  Updated comments


SetDirection    PROC        NEAR

FindNewAbsDirection:
        MOV     AX, currNum         ; Get the parsed angle to move in
        CWD                         ; Convert with sign to double word size
                                    ; to get angle MOD MAX_ANGLE
        MOV     BX, MAX_ANGLE       ; Divide by MAX_ANGLE to get the same angle
        IDIV    BX                  ; within the desired range of -MAX_ANGLE
                                    ; to +MAX_ANGLE as the remainder
AddToOldDirection:
		CALL	GetMotorDirection   ; Get the current direction of movement
		ADD		AX, DX              ; and add the relative direction in
        MOV     BX, AX              ; Use this as the angle argument in SetSpeed
SetParsedDirection:
        PUSH    BX
		MOV		AX, SPEED_NO_CHANGE ; Do not change the speed from the current value
		CALL    SetMotorSpeed       ; Use SetSpeed to set the angle of movement
        POP     AX
		MOV		BL, DIRECTION_STATUS
		CALL	SendInfo
DoneSetDirection:
        CALL    ResetStates         ; Reset shared variables since end of command
        RET                         ; reached and done setting direction
SetDirection	ENDP

; SendInfo
; Description: 		
;
; Operation:  		
;
; Arguments:   		value [AX] - value of status to update with
;					type [BX] - type of status ('S' for speed, 'D' for direction)
; Return Value:		None
;
; Local Variables:	None
; Shared Variables: ValueBuffer (R/W) - holds ASCII value of status value
; Global Variables: None
;
; Input:			Command from remote unit to set speed or direction.
; Output:			Remote unit displays current speed/direction that was updated
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	None
;
; Registers Changed: AX, BX, SI, ES, flags
; Stack Depth:		2 words
;
; Limitations:		This function only allows for sending two types of status,
;					and no status about the laser
;
; Author:			Maitreyi Ashok
; Last Modified:	12/07/16	Maitreyi Ashok	Wrote code
;					12/08/16	Maitreyi Ashok	Updated comments

SendInfo		PROC		NEAR

PrepareForSendingInfo:
        PUSH    AX					; Save value of speed/direction to send
SendType:
        PUSH    BX					; Save type of status to resend if it doesn't
									; go through
        MOV     AX, BX
        XOR     AH, AH				; Send status character back depending on 
        CALL    SerialPutChar		; whether speed or direction was set
        POP     BX					; Restore type of status and 
        JNC     SendValue			; check if it got sent through serial channel
        ;JC     TrySendingAgain		; If it did, send the associated value
TrySendingAgain:					; If it did not, try sending the type
        JMP     SendType			; again
SendValue:
        POP     AX					; Restore value of speed/direction and 
        MOV     SI, OFFSET(ValueBuffer)
									; convert the decimal value
        PUSH    SI					; to an unsigned ASCII representation	
        CALL    Dec2String			; to send through serial channel to remote unit
        POP     SI					; Get the address of the buffer with the string
        MOV     AX, SI				; and put the entire string character
        PUSH    DS					; by character with a carriage return to 
        POP     ES					; signify end of status at the end
        CALL    SerialPutString
        RET							; Done updating remote unit with the status
SendInfo        ENDP
        
; RotTurret
; Description: 		This function rotates the turret on the RoboTrike to an
;					absolute angle or by some relative amount depending on the
;					value in degrees included in the command. The rotation is
;					absolute if no sign character is included, and relative if
;					one is included. Positive rotations are to the right from
;					current position and negative rotations are to the left.
;
; Operation:  		The function operates by finding the angle to rotate the
;					turret through. SetTurretAngle is called to rotate the angle
;					of the turret to an absolute orientation if there is no
;					sign character in the command, and SetRelTurretAngle is called
;					to rotate the angle of the turret relative to its current
;					orientation if there is a sign character in the command. Positive
;					sign characters correspond to rotating to the right, and negative
;					sign characters correspond to rotating to the left. The rotation
;					is controlled by a stepper motor in the turret of the RoboTrike.
;
; Arguments:   		None
; Return Value:		overflowError [CX] - returns 0 since no error to report
;
; Local Variables:	None
; Shared Variables: currNum (R)- stores number constructed so far in parsing of command
;					noSign (R)- stores 0 if no sign character entered, 1 if sign
;							character entered 
; Global Variables: None
;
; Input:			Serial command of form T# and carriage return character
; Output:			RoboTrike stepper motor rotates the turret
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	None
;
; Registers Changed: AX, BX, flags, CX (reset by ResetStates)
; Stack Depth:		0 words
;
; Limitations:		This function does not allow the user to define where the
;					fixed orientation to measure absolute angles against is
;					located.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/21/16	Maitreyi Ashok	Functional Specification/Pseudocode
;					11/23/16    Maitreyi Ashok  Wrote code
;                   11/24/16    Maitreyi Ashok  Updated comments


RotTurret    	PROC        NEAR

NormalizeNewAngle:
        MOV     AX, currNum             ; Get the parsed number for rotating turret
FindAbsOrRelAngle:
		MOV		BX, noSign              ; See if sign character was entered
		OR		BX, BX                  ; before entering a number
		JZ		RotateTurretAbsolute    ; If not, rotate turret by absolute amount
		;JNZ	TurretRelativeAngle     ; If it was, rotate turret by relative amount
TurretRelativeAngle:
        CALL    SetRelTurretAngle       ; Use relative angle setting function to
		JMP     DoneRotating            ; set angle and then done rotating
RotateTurretAbsolute:
		CALL	SetTurretAngle          ; Otherwise use absolute angle setting function
        ;JMP    DoneRotating            ; to set angle and then done rotating
DoneRotating:
        CALL    ResetStates             ; Reset shared variables since reached end
		RET                             ; of command. Done rotating turret
RotTurret		ENDP

; ElevTurret
; Description: 		This function elevates the laser on the turret on the RoboTrike 
;					to a specified absolute angle in degrees. Positive elevations 
;					are above the horizontal and negative elevations are below the 
;					horizontal. The angles given in the command must be between
;					between positive and negative 60 degrees due to the limited
;					range of rotation of the servomotor used in elevating the
;					laser in the turret.
;
; Operation:  		The function operates by finding the angle to elevate the
;					laser on the turret by. SetTurretElevation is called to elevate
;					the laser to an absolute angle. Positive sign characters (or no 
;					sign character) correspond to elevating to above the horizontal, 
;					and negative sign characters correspond to elevating to
;					below the horizontal. The elevation is controlled by a servomotor 
;					in the turret of the RoboTrike. Thus, the range of the angle
;					is between +/- 60 degrees, and angles outside this range
;					will cause an error.
;
; Arguments:   		None
; Return Value:		elevationError [CX] - returns whether elevation error occurred
;                               if passed in argument out of possible elevation range
;
; Local Variables:	elevation [AX] - elevation to set turret at
; Shared Variables: currNum (R)- stores number constructed so far in parsing of command
; Global Variables: None
;
; Input:			Serial command of form E# and carriage return character
; Output:			RoboTrike servomotor elevates the laser on turret
;
; Error Handling:	If angle is outside of range of (-60 degrees, 60 degrees) an error
;					is set.
;
; Algorithms:		None
; Data Structures:	None
;
; Registers Changed: AX, CX
; Stack Depth:		0 words
;
; Limitations:		The servomotor used to elevate the laser does not allow for
;					a wider range of motion of the laser.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/21/16	Maitreyi Ashok	Functional Specification/Pseudocode
;					11/23/16    Maitreyi Ashok  Wrote code
;                   11/24/16    Maitreyi Ashok  Updated comments


ElevTurret    	PROC        NEAR

CheckAngleTooLarge:
		MOV		AX, currNum             ; Get the passed number to elevate turret
		CMP		AX, MAX_ELEVATION       ; to and see if it is more than maximum elevation
		JG		ElevError               ; If it is, there is an error
		;JLE	CheckAngleTooSmall      ; Otherwise check the lower bound
CheckAngleTooSmall:
		CMP		AX, MIN_ELEVATION       ; If the angle is less than minimum elevation
		JL		ElevError               ; there is an error
		;JGE	SetElevation            ; Otherwise set the elevation
SetElevation:
		CALL	SetTurretElevation      ; Set elevation of turret in degrees
        CALL    ResetStates             ; Reset shared variables since reached
		JMP		DoneElevateTurret       ; end of command. Done elevating turret
ElevError:
		CALL	SetError                ; Set error if elevation argument out of
        MOV     CX, ELEVATION_ERROR     ; bounds and return appropriate value
		;JMP	DoneElevateTurret
DoneElevateTurret:		                
		RET                             ; Done elevating turret
ElevTurret		ENDP

; FireLaser
; Description: 		This function turns the laser on in the turret on the RoboTrike.
;					The laser is turned on and kept on until turned off by a
;					laser turn off command.
;
; Operation:  		The function is called when there is a Fire Laser command,
;					at which time it turns the laser on. The laser is kept on
;					by setting a bit in parallel port B of the 8255, until the bit
;					is cleared when turning the laser off.
;
; Arguments:   		None
; Return Value:		overflowError [CX] - returns 0 since no error to report
;
; Local Variables:	None
; Shared Variables: None
; Global Variables: None
;
; Input:			Serial comand of the form F and carriage return character
; Output:			Laser in RoboTrike turned on
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	None
;
; Registers Changed: AX, CX (reset in ResetStates), flags
; Stack Depth:		0 words
;
; Limitations:		There is no way to specify for how long the laser should be
;					on or if there is any pattern to it turning on and off.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/21/16	Maitreyi Ashok	Functional Specification/Pseudocode
;                   11/22/16    Maitreyi Ashok  Wrote code
;                   11/24/16    Maitreyi Ashok  Updated comments

FireLaser    	PROC        NEAR

TurnLaserOn:
		MOV		AX, LASER_ON        ; Set laser with argument as LASER_ON
		CALL	SetLaser            ; since laser firing character entered
        CALL    ResetStates         ; Reset shared variables since end of command
		RET                         ; reached and done firing laser
FireLaser		ENDP

; LaserOff
; Description: 		This function turns the laser off in the turret on the RoboTrike.
;					The laser is turned off and kept off until turned on by a
;					laser fire command.
;
; Operation:  		The function is called when there is a Laser Off command,
;					at which time it turns the laser off. The laser is kept off
;					by clearing a bit in parallel port B of the 8255, until the bit
;					is set when turning the laser on.
;
; Arguments:   		None
; Return Value:		overflowError [CX] - returns 0 since no error to report
;
; Local Variables:	None
; Shared Variables: None
; Global Variables: None
;
; Input:			Serial command of the form O and carriage return character
; Output:			Laser in RoboTrike is turned off
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	None
;
; Registers Changed: AX, CX (reset in ResetStates), flags
; Stack Depth:		0 words
;
; Limitations:		There is no way to specify for how long the laser should be
;					off or what the on/off pattery should be.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/21/16	Maitreyi Ashok	Functional Specification/Pseudocode
;					11/23/16    Maitreyi Ashok  Wrote code
;                   11/24/16    Maitreyi Ashok  Updated comments

LaserOff    	PROC        NEAR

TurnLaserOff:
		MOV		AX, LASER_OFF       ; Set laser with argument as LASER_OFF
		CALL	SetLaser            ; since laser turning off character entered
        CALL    ResetStates         ; Reset shared variables since end of command
		RET                         ; reached and done turning off laser
LaserOff		ENDP

; ResetStates
; Description: 		This function is used to reset all shared variables to the
;					initial state for starting to parse a new command from the
;					Idle state. This function is used if there was an error in
;					any step of the parsing and in initializing the parsing routines.
;					The value that is part of the command is set to zero, while
;					the sign is set to positive, and the noSign variable is set
;					to no sign character.
;
; Operation:  		This function signfies that the state machine is soon entering
;					the Idle state, where it will start processing a new command
;					independent from previous ones. Thus, the values and signs
;					stored in variables from previous commands must be cleared
;					to not interfere with the parsing of the next command. In addition
;                   the register used as return value to signify overflow or other
;                   errors in the action routines is reset.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	None
; Shared Variables: sign (W)- stores sign of value in command, +1 if plus sign or
;							no sign character entered, -1 if negative sign character
;							entered
;					noSign (W)- stores 0 if no sign character entered, 1 if sign
;							character entered
;					currNum (W)- stores number constructed so far in parsing of command
; Global Variables: None
;
; Input:			None
; Output:			None
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	None
;
; Registers Changed: CX
; Stack Depth:		0 words
;
; Limitations:		This function does not allow for the specification of the
;					default sign. Instead, it is assumed to be positive.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/21/16	Maitreyi Ashok	Functional Specification/Pseudocode
;					11/22/16    Maitreyi Ashok  Wrote code
;                   11/24/16    Maitreyi Ashok  Updated comments

ResetStates    	PROC        NEAR
ResetVars:
		MOV		sign, IMPLIED_SIGN
		MOV		noSign, GOT_NO_SIGN
        XOR     CX, CX
		MOV		currNum, 0
		RET
ResetStates		ENDP


; StateTable
;
; Description:      This is the state transition table for the state machine.
;                   Each entry consists of the next state and action for that
;                   transition.  Each set of rows corresponds to the current state
;                   and each row within the set corresponds to a different input
;                   type. This is essentially a 2-D table with rows as current
;                   states and columns as input types. 
;
; Author:           Maitreyi Ashok
; Last Modified:    11/24/2016

; Type of each entry in state table containing next state to enter and action
; to perform during transition
TRANSITION_ENTRY        STRUC           ;structure used to define table
    NEXTSTATE   DB      ?               ;the next state for the transition
    ACTION      DW      ?               ;action for the transition
TRANSITION_ENTRY      ENDS


;define a macro to make table a little more readable
;macro just does an offset of the action routine entries to build the STRUC
%*DEFINE(TRANSITION(nxtst, act))  (
    TRANSITION_ENTRY< %nxtst, OFFSET(%act) >
)


StateTable		LABEL	TRANSITION_ENTRY

	;Current State = ST_IDLE					Input Token Type
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_DIGIT
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_POS
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_NEG
	%TRANSITION(ST_IDLE, doNOP)				    ;TOKEN_EOC
	%TRANSITION(ST_ABS_SPEED, doNOP)			;TOKEN_ABS
	%TRANSITION(ST_REL_SPEED, doNOP)			;TOKEN_REL
	%TRANSITION(ST_DIRECTION, doNOP)			;TOKEN_DIR
	%TRANSITION(ST_ROT_TURR, doNOP)				;TOKEN_ROT
	%TRANSITION(ST_ELEV_TURR, doNOP)			;TOKEN_ELEV
	%TRANSITION(ST_LASER_FIRE, doNOP)			;TOKEN_FIRE
	%TRANSITION(ST_LASER_OFF, doNOP)			;TOKEN_OFF
	%TRANSITION(ST_IDLE, doNOP)					;TOKEN_IGNORE
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_OTHER

	;Current State = ST_ABS_SPEED				Input Token Type
	%TRANSITION(ST_ABS_SPEED_NUM, AddDigit)		;TOKEN_DIGIT
	%TRANSITION(ST_ABS_SPEED_SIGN, SetSign)		;TOKEN_POS
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_NEG
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_EOC
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_ABS
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_REL
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_DIR
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_ROT
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_ELEV
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_FIRE
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_OFF
	%TRANSITION(ST_ABS_SPEED, doNOP)			;TOKEN_IGNORE
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_OTHER
	
	;Current State = ST_ABS_SPEED_SIGN			Input Token Type
	%TRANSITION(ST_ABS_SPEED_NUM, AddDigit)		;TOKEN_DIGIT
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_POS
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_NEG
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_EOC
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_ABS
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_REL
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_DIR
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_ROT
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_ELEV
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_FIRE
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_OFF
	%TRANSITION(ST_ABS_SPEED_SIGN, doNOP)		;TOKEN_IGNORE
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_OTHER
	
	;Current State = ST_ABS_SPEED_NUM			Input Token Type
	%TRANSITION(ST_ABS_SPEED_NUM, AddDigit)		;TOKEN_DIGIT
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_POS
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_NEG
	%TRANSITION(ST_IDLE, SetAbsSpeed)			;TOKEN_EOC
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_ABS
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_REL
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_DIR
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_ROT
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_ELEV
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_FIRE
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_OFF
	%TRANSITION(ST_ABS_SPEED_NUM, doNOP)		;TOKEN_IGNORE
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_OTHER
	
	;Current State = ST_REL_SPEED				Input Token Type
	%TRANSITION(ST_REL_SPEED_NUM, AddDigit)		;TOKEN_DIGIT
	%TRANSITION(ST_REL_SPEED_SIGN, SetSign)		;TOKEN_POS
	%TRANSITION(ST_REL_SPEED_SIGN, SetSign)		;TOKEN_NEG
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_EOC
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_ABS
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_REL
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_DIR
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_ROT
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_ELEV
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_FIRE
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_OFF
	%TRANSITION(ST_REL_SPEED, doNOP)			;TOKEN_IGNORE
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_OTHER
	
	;Current State = ST_REL_SPEED_SIGN			Input Token Type
	%TRANSITION(ST_REL_SPEED_NUM, AddDigit)		;TOKEN_DIGIT
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_POS
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_NEG
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_EOC
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_ABS
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_REL
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_DIR
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_ROT
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_ELEV
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_FIRE
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_OFF
	%TRANSITION(ST_REL_SPEED_SIGN, doNOP)		;TOKEN_IGNORE
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_OTHER
	
    ;Current State = ST_REL_SPEED_NUM			Input Token Type
	%TRANSITION(ST_REL_SPEED_NUM, AddDigit)		;TOKEN_DIGIT
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_POS
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_NEG
	%TRANSITION(ST_IDLE, SetRelSpeed)			;TOKEN_EOC
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_ABS
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_REL
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_DIR
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_ROT
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_ELEV
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_FIRE
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_OFF
	%TRANSITION(ST_REL_SPEED_NUM, doNOP)		;TOKEN_IGNORE
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_OTHER
	
	;Current State = ST_DIRECTION				Input Token Type
	%TRANSITION(ST_DIRECTION_NUM, AddDigit)		;TOKEN_DIGIT
	%TRANSITION(ST_DIRECTION_SIGN, SetSign)		;TOKEN_POS
	%TRANSITION(ST_DIRECTION_SIGN, SetSign)		;TOKEN_NEG
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_EOC
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_ABS
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_REL
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_DIR
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_ROT
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_ELEV
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_FIRE
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_OFF
	%TRANSITION(ST_DIRECTION, doNOP)			;TOKEN_IGNORE
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_OTHER
	
	;Current State = ST_DIRECTION_SIGN			Input Token Type
	%TRANSITION(ST_DIRECTION_NUM, AddDigit)		;TOKEN_DIGIT
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_POS
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_NEG
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_EOC
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_ABS
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_REL
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_DIR
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_ROT
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_ELEV
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_FIRE
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_OFF
	%TRANSITION(ST_DIRECTION_SIGN, doNOP)		;TOKEN_IGNORE
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_OTHER
    
    ;Current State = ST_DIRECTION_NUM			Input Token Type
	%TRANSITION(ST_DIRECTION_NUM, AddDigit)		;TOKEN_DIGIT
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_POS
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_NEG
	%TRANSITION(ST_IDLE, SetDirection)			;TOKEN_EOC
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_ABS
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_REL
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_DIR
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_ROT
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_ELEV
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_FIRE
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_OFF
	%TRANSITION(ST_DIRECTION_NUM, doNOP)		;TOKEN_IGNORE
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_OTHER
	
	;Current State = ST_ROT_TURR				Input Token Type
	%TRANSITION(ST_ROT_TURR_NUM, AddDigit)		;TOKEN_DIGIT
	%TRANSITION(ST_ROT_TURR_SIGN, SetSign)		;TOKEN_POS
	%TRANSITION(ST_ROT_TURR_SIGN, SetSign)		;TOKEN_NEG
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_EOC
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_ABS
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_REL
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_DIR
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_ROT
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_ELEV
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_FIRE
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_OFF
	%TRANSITION(ST_ROT_TURR, doNOP)			    ;TOKEN_IGNORE
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_OTHER
	
	;Current State = ST_ROT_TURR_SIGN			Input Token Type
	%TRANSITION(ST_ROT_TURR_NUM, AddDigit)		;TOKEN_DIGIT
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_POS
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_NEG
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_EOC
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_ABS
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_REL
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_DIR
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_ROT
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_ELEV
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_FIRE
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_OFF
	%TRANSITION(ST_ROT_TURR_SIGN, doNOP)		;TOKEN_IGNORE
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_OTHER
    
    ;Current State = ST_ROT_TURR_NUM			Input Token Type
	%TRANSITION(ST_ROT_TURR_NUM, AddDigit)		;TOKEN_DIGIT
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_POS
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_NEG
	%TRANSITION(ST_IDLE, RotTurret)			    ;TOKEN_EOC
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_ABS
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_REL
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_DIR
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_ROT
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_ELEV
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_FIRE
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_OFF
	%TRANSITION(ST_ROT_TURR_NUM, doNOP)		    ;TOKEN_IGNORE
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_OTHER
	
	;Current State = ST_ELEV_TURR				Input Token Type
	%TRANSITION(ST_ELEV_TURR_NUM, AddDigit)		;TOKEN_DIGIT
	%TRANSITION(ST_ELEV_TURR_SIGN, SetSign)		;TOKEN_POS
	%TRANSITION(ST_ELEV_TURR_SIGN, SetSign)		;TOKEN_NEG
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_EOC
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_ABS
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_REL
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_DIR
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_ROT
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_ELEV
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_FIRE
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_OFF
	%TRANSITION(ST_ELEV_TURR, doNOP)		    ;TOKEN_IGNORE
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_OTHER
	
	;Current State = ST_ELEV_TURR_SIGN			Input Token Type
	%TRANSITION(ST_ELEV_TURR_NUM, AddDigit)		;TOKEN_DIGIT
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_POS
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_NEG
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_EOC
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_ABS
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_REL
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_DIR
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_ROT
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_ELEV
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_FIRE
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_OFF
	%TRANSITION(ST_ELEV_TURR_SIGN, doNOP)		;TOKEN_IGNORE
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_OTHER
    
    ;Current State = ST_ELEV_TURR_NUM			Input Token Type
	%TRANSITION(ST_ELEV_TURR_NUM, AddDigit)		;TOKEN_DIGIT
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_POS
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_NEG
	%TRANSITION(ST_IDLE, ElevTurret)			;TOKEN_EOC
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_ABS
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_REL
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_DIR
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_ROT
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_ELEV
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_FIRE
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_OFF
	%TRANSITION(ST_ELEV_TURR_NUM, doNOP)		;TOKEN_IGNORE
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_OTHER
	
	;Current State = ST_LASER_FIRE				Input Token Type
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_DIGIT
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_POS
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_NEG
	%TRANSITION(ST_IDLE, FireLaser)				;TOKEN_EOC
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_ABS
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_REL
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_DIR
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_ROT
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_ELEV
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_FIRE
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_OFF
	%TRANSITION(ST_LASER_FIRE, doNOP)		    ;TOKEN_IGNORE
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_OTHER
	
	;Current State = ST_LASER_OFF   			Input Token Type
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_DIGIT
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_POS
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_NEG
	%TRANSITION(ST_IDLE, LaserOff)				;TOKEN_EOC
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_ABS
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_REL
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_DIR
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_ROT
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_ELEV
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_FIRE
	%TRANSITION(ST_ERROR, SetError)			    ;TOKEN_OFF
	%TRANSITION(ST_LASER_OFF, doNOP)		    ;TOKEN_IGNORE
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_OTHER
    
    ;Current State = ST_ERROR       			Input Token Type
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_DIGIT
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_POS
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_NEG
	%TRANSITION(ST_IDLE, DoneError)				;TOKEN_EOC
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_ABS
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_REL
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_DIR
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_ROT
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_ELEV
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_FIRE
	%TRANSITION(ST_ERROR, SetError)		    	;TOKEN_OFF
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_IGNORE
	%TRANSITION(ST_ERROR, SetError)				;TOKEN_OTHER
	
; Token Tables
;
; Description:      This creates the tables of token types and token values.
;                   Each entry corresponds to the token type and the token
;                   value for a character.  Macros are used to actually build
;                   two separate tables - TokenTypeTable for token types and
;                   TokenValueTable for token values.
;
; Author:           Maitreyi Ashok
; Last Modified:    11/22/2016

%*DEFINE(TABLE)  (
        %TABENT(TOKEN_OTHER, 0)		;<null>
        %TABENT(TOKEN_OTHER, 1)		;SOH
        %TABENT(TOKEN_OTHER, 2)		;STX
        %TABENT(TOKEN_OTHER, 3)		;ETX
        %TABENT(TOKEN_OTHER, 4)		;EOT
        %TABENT(TOKEN_OTHER, 5)		;ENQ
        %TABENT(TOKEN_OTHER, 6)		;ACK
        %TABENT(TOKEN_OTHER, 7)		;BEL
        %TABENT(TOKEN_OTHER, 8)		;backspace
        %TABENT(TOKEN_IGNORE, 9)	;TAB
        %TABENT(TOKEN_IGNORE, 10)	;new line
        %TABENT(TOKEN_OTHER, 11)	;vertical tab
        %TABENT(TOKEN_OTHER, 12)	;form feed
        %TABENT(TOKEN_EOC, 0)		;carriage return (end of command)
        %TABENT(TOKEN_OTHER, 14)	;SO
        %TABENT(TOKEN_OTHER, 15)	;SI
        %TABENT(TOKEN_OTHER, 16)	;DLE
        %TABENT(TOKEN_OTHER, 17)	;DC1
        %TABENT(TOKEN_OTHER, 18)	;DC2
        %TABENT(TOKEN_OTHER, 19)	;DC3
        %TABENT(TOKEN_OTHER, 20)	;DC4
        %TABENT(TOKEN_OTHER, 21)	;NAK
        %TABENT(TOKEN_OTHER, 22)	;SYN
        %TABENT(TOKEN_OTHER, 23)	;ETB
        %TABENT(TOKEN_OTHER, 24)	;CAN
        %TABENT(TOKEN_OTHER, 25)	;EM
        %TABENT(TOKEN_OTHER, 26)	;SUB
        %TABENT(TOKEN_OTHER, 27)	;escape
        %TABENT(TOKEN_OTHER, 28)	;FS
        %TABENT(TOKEN_OTHER, 29)	;GS
        %TABENT(TOKEN_OTHER, 30)	;AS
        %TABENT(TOKEN_OTHER, 31)	;US
        %TABENT(TOKEN_IGNORE, ' ')	;space
        %TABENT(TOKEN_OTHER, '!')	;!
        %TABENT(TOKEN_OTHER, '"')	;"
        %TABENT(TOKEN_OTHER, '#')	;#
        %TABENT(TOKEN_OTHER, '$')	;$
        %TABENT(TOKEN_OTHER, 37)	;percent
        %TABENT(TOKEN_OTHER, '&')	;&
        %TABENT(TOKEN_OTHER, 39)	;'
        %TABENT(TOKEN_OTHER, 40)	;open paren
        %TABENT(TOKEN_OTHER, 41)	;close paren
        %TABENT(TOKEN_OTHER, '*')	;*
        %TABENT(TOKEN_POS, +1)		;+  (positive sign)
        %TABENT(TOKEN_OTHER, 44)	;,
        %TABENT(TOKEN_NEG, -1)		;-  (negative sign)
        %TABENT(TOKEN_OTHER, 0)		;.
        %TABENT(TOKEN_OTHER, '/')	;/
        %TABENT(TOKEN_DIGIT, 0)		;0  (digit)
        %TABENT(TOKEN_DIGIT, 1)		;1  (digit)
        %TABENT(TOKEN_DIGIT, 2)		;2  (digit)
        %TABENT(TOKEN_DIGIT, 3)		;3  (digit)
        %TABENT(TOKEN_DIGIT, 4)		;4  (digit)
        %TABENT(TOKEN_DIGIT, 5)		;5  (digit)
        %TABENT(TOKEN_DIGIT, 6)		;6  (digit)
        %TABENT(TOKEN_DIGIT, 7)		;7  (digit)
        %TABENT(TOKEN_DIGIT, 8)		;8  (digit)
        %TABENT(TOKEN_DIGIT, 9)		;9  (digit)
        %TABENT(TOKEN_OTHER, ':')	;:
        %TABENT(TOKEN_OTHER, ';')	;;
        %TABENT(TOKEN_OTHER, '<')	;<
        %TABENT(TOKEN_OTHER, '=')	;=
        %TABENT(TOKEN_OTHER, '>')	;>
        %TABENT(TOKEN_OTHER, '?')	;?
        %TABENT(TOKEN_OTHER, '@')	;@
        %TABENT(TOKEN_OTHER, 'A')	;A
        %TABENT(TOKEN_OTHER, 'B')	;B
        %TABENT(TOKEN_OTHER, 'C')	;C
        %TABENT(TOKEN_DIR, 'D')		;D (Set Direction)
        %TABENT(TOKEN_ELEV, 'E')	;E (Set Turret Elevation Angle)
        %TABENT(TOKEN_FIRE, 'F')	;F (Fire Laser)
        %TABENT(TOKEN_OTHER, 'G')	;G
        %TABENT(TOKEN_OTHER, 'H')	;H
        %TABENT(TOKEN_OTHER, 'I')	;I
        %TABENT(TOKEN_OTHER, 'J')	;J
        %TABENT(TOKEN_OTHER, 'K')	;K
        %TABENT(TOKEN_OTHER, 'L')	;L
        %TABENT(TOKEN_OTHER, 'M')	;M
        %TABENT(TOKEN_OTHER, 'N')	;N
        %TABENT(TOKEN_OFF, 'O')		;O (Laser Off)
        %TABENT(TOKEN_OTHER, 'P')	;P
        %TABENT(TOKEN_OTHER, 'Q')	;Q
        %TABENT(TOKEN_OTHER, 'R')	;R
        %TABENT(TOKEN_ABS, 'S')		;S (Set Absolute Speed)
        %TABENT(TOKEN_ROT, 'T')		;T (Rotate Turret Angle)
        %TABENT(TOKEN_OTHER, 'U')	;U
        %TABENT(TOKEN_REL, 'V')		;V (Set Relative Speed)
        %TABENT(TOKEN_OTHER, 'W')	;W
        %TABENT(TOKEN_OTHER, 'X')	;X
        %TABENT(TOKEN_OTHER, 'Y')	;Y
        %TABENT(TOKEN_OTHER, 'Z')	;Z
        %TABENT(TOKEN_OTHER, '[')	;[
        %TABENT(TOKEN_OTHER, '\')	;\
        %TABENT(TOKEN_OTHER, ']')	;]
        %TABENT(TOKEN_OTHER, '^')	;^
        %TABENT(TOKEN_OTHER, '_')	;_
        %TABENT(TOKEN_OTHER, '`')	;`
        %TABENT(TOKEN_OTHER, 'a')	;a
        %TABENT(TOKEN_OTHER, 'b')	;b
        %TABENT(TOKEN_OTHER, 'c')	;c
        %TABENT(TOKEN_DIR, 'd')		;d (Set Direction)
        %TABENT(TOKEN_ELEV, 'e')	;e (Set Elevation Angle)
        %TABENT(TOKEN_FIRE, 'f')	;f (Fire Laser)
        %TABENT(TOKEN_OTHER, 'g')	;g
        %TABENT(TOKEN_OTHER, 'h')	;h
        %TABENT(TOKEN_OTHER, 'i')	;i
        %TABENT(TOKEN_OTHER, 'j')	;j
        %TABENT(TOKEN_OTHER, 'k')	;k
        %TABENT(TOKEN_OTHER, 'l')	;l
        %TABENT(TOKEN_OTHER, 'm')	;m
        %TABENT(TOKEN_OTHER, 'n')	;n
        %TABENT(TOKEN_OFF, 'o')		;o (Laser Off)
        %TABENT(TOKEN_OTHER, 'p')	;p
        %TABENT(TOKEN_OTHER, 'q')	;q
        %TABENT(TOKEN_OTHER, 'r')	;r
        %TABENT(TOKEN_ABS, 's')		;s (Set Absolute Speed)
        %TABENT(TOKEN_ROT, 't')		;t (Rotate Turret Angle)
        %TABENT(TOKEN_OTHER, 'u')	;u
        %TABENT(TOKEN_REL, 'v')		;v (Set Relative Speed)
        %TABENT(TOKEN_OTHER, 'w')	;w
        %TABENT(TOKEN_OTHER, 'x')	;x
        %TABENT(TOKEN_OTHER, 'y')	;y
        %TABENT(TOKEN_OTHER, 'z')	;z
        %TABENT(TOKEN_OTHER, '{')	;{
        %TABENT(TOKEN_OTHER, '|')	;|
        %TABENT(TOKEN_OTHER, '}')	;}
        %TABENT(TOKEN_OTHER, '~')	;~
        %TABENT(TOKEN_OTHER, 127)	;rubout
)

; token type table - uses first byte of macro table entry
%*DEFINE(TABENT(tokentype, tokenvalue))  (
        DB      %tokentype
)

TokenTypeTable	LABEL   BYTE
        %TABLE


; token value table - uses second byte of macro table entry
%*DEFINE(TABENT(tokentype, tokenvalue))  (
        DB      %tokenvalue
)

TokenValueTable	LABEL       BYTE
        %TABLE

CODE    ENDS

;the data segment

DATA    SEGMENT PUBLIC  'DATA'


error       DW      ?   ; stores whether an error has occurred in the step of the FSM
state       DB      ?   ; stores the number of the state currently on
currNum     DW      ?   ; current number associated with sign
sign        DB      ?   ; stores +1 for positive sign value, -1 for negative sign value
noSign      DW      ?   ; stores whether no sign +/- was entered before entering digits
                        ; 0 if no sign was entered, 1 if sign was entered
ValueBuffer DB      ?   ; Buffer used by SerialPutString to output status to remote unit
DATA    ENDS

        END