; Maitreyi Ashok
; Section 1 – Richard Lee

        NAME    StrOps

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   StrOps                                   ;
;                           String Operation Routines	                     ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; This file contains a routine to parse a motor status/error string and create a
; string to display.
;
; Table of Contents:
; Functions
; Global - 
; ParseDisplayString - Parse character received from serial motor output
; SetError - Set that serial error on motor or remote unit occurred
; InitRemoteParser - Initialize remote parser and associated FSM
;
; Local - 
; GetToken - Get token type/value of character received from serial output
; DispCommEr - Display communication error message
; DispStatus - Display updated status of RoboTrike
; AddSpeedChar - Add digit to speed status of RoboTrike
; AddDirChar - Add digit to direction status of RoboTrike
; EndOfSpeedString - Display updated speed of RoboTrike
; EndOfDirString - Display updated direction of RoboTrike
; AddMotoDig - Add digit of LSR error value of motor serial error
; InitDisplayString - Initialize status buffer display string
; AddParse - Add to buffer that parsing error occurred on motor unit
; DispError - Display motor serial error value
;
; Table
; StateTable - State transition table of TRANSITION_ENTRY elements
; TokenTypeTable - Contains token types of all ASCII characters
; TokenValueTable - Contains token values of all ASCII characters
; 
; Macro
; TRANSITION - builds TRANSITION_ENTRY STRUCs for state tabel using arguments
; TABLE - creates tables of token types and token values
; TABENT - stores eitehr token type or value as entry in table
;
; Revision History:
;     11/29/2016	Maitreyi Ashok	Functional Specification/Pseudocode
;	  12/04/2016	Maitreyi Ashok	Wrote code
;	  12/05/2016	Maitreyi Ashok	Updated comments

$INCLUDE(General.inc)		; Include logical value definitions
$INCLUDE(StrOps.inc)		; Include string operation definitions
$INCLUDE(Display.inc)		; Include display definitions
$INCLUDE(RemoFns.inc)		; Include motor error definitions

CGROUP  GROUP   CODE
DGROUP	GROUP	DATA

CODE	SEGMENT PUBLIC 'CODE'


        ASSUME  CS:CGROUP, DS:DGROUP
		
		EXTRN	Display:NEAR		; Display a string once parsed
        EXTRN   DisplayErrors:NEAR	; Display errors based on line status values
        EXTRN   doNOP:NEAR			; Do nothing as action

; ParseDisplayString		
; Description: 		This function recieves a character from the motor serial output
;					and processes the character as a string to display. The string
;					will be prefixed by either a character to indicate speed status,
;					direction status, motor serial error, or a parsing error. Then,
;					the digits related to the error will follow, with the entire
;					string delimited by carriage return. Blank spaces and new lines
;					are ignored and all invalid characters will lead to an error 
;					state which will cause a communication error message to be
;					displayed.
; Operation:  		This function uses a state machine to parse characters from
;					serial output of the motor as a status to display for the RoboTrike.  
;					This is	done using a Mealy Finite State Machine which dictates
;					what state to enter from the current state and what
;					action to perform during this transition based on the
;					serial character input. The function is called repeatedly
;					and performs an action and changes the state during each call,
;					after which it returns and is called again later when the
;					next serial character is available. If there was an error during
;                   the parsing, a communication error message is displayed.
;                   Otherwise, the speed, angle, and parsing error/no error is
;					displayed. for more details, see complete functional specification.				
;
; Arguments:   		char [AL] - next serial character passed in, only valid 
;							characters are S, s, D, d, M, m, P, p, '', ' ', '\n', 
;							carriage return, and '0'-'9'
; Return Value:		None
;
; Local Variables:	tokenType [DX] - token group character passed in is part of
;					tokenVal [CH/AL]  - value of token referred to by character passed in
;					tableEntry [BX] - offset of table entry referred to by current state
;							and input character
; Shared Variables: state (R/W)- stores current state in the parsing of command
; Global Variables: None
;
; Input:			Motor status or error sent through serial channel
; Output:			Appropriate message shown on display
;
; Error Handling:	If an error character is input, a communication error message
;					is displayed.
;
; Algorithms:		None
; Data Structures:	Tables are used to implement a finite state machine
;
; Registers Changed: DX, CH, AX, BX, flags
; Stack Depth:		2 words
;
; Limitations:		The function does not give priority to specific strings to display,
;					for example to errors over general status
;
; Author:			Maitreyi Ashok
; Last Modified:	12/04/16	Maitreyi Ashok	Wrote code
;					12/05/16	Maitreyi Ashok	Updated comments

ParseDisplayString      PROC        NEAR
                        PUBLIC      ParseDisplayString
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
DoTransition:
        POP     BX                          ; Get offset in state table and use it
		MOV		BL, CS:StateTable[BX].NEXTSTATE
        MOV     state, BL                   ; to find and save next state to go to
        RET
ParseDisplayString     ENDP

; GetToken
; Description: 		This function finds the token class and value for the
;					character passed in. The character is truncated to 7 bits
;					since the high bit is unused in standard ASCII code. The
;					possible token classes are End of Command, Ignore, Digit,
;					Direction status, motor error, speed status, parsing error,
;					and other. The token values	are dependent on the numerical value 
;                   of the character or how the token value is used in the associated 
;                   character handler function.
;
; Operation:  		The function looks up the passed character in a token type
;					table and a token value table to get these values for
;					any character passed in easily. These types and values can
;					then be returned to the caller (ParseDisplayString) for use
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
;					12/05/16	Maitreyi Ashok	Updated comments for use in remote
;								parser

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

; DispCommEr		
; Description: 		This function displays that there was a communication error.
;					This function executes if an invalid character was output via
;					the serial channel to parse and display. Thus, after the end 
;					of line character is received, the communication error message
;					is displayed until the next string to display. In addition,
;					the error state is reached if a serial error occurs to prevent
;					other successive input from being displayed until the end
;					of line character. Thus, if the error flag is set, the 
;					communication error message is not displayed since another more
;					specific error message would have already been displayed. 
;
; Operation:  		This function checks if the error flag is set. If so, then
;					a more descriptive error message has already been displayed
;					so it is not necessary to display this messag. Since the end
;					of line character has been received, the error flag is reset
;					and other strings can now be displayed. If there was no error,
;					then an invalid character must be sent and the user can be
;					notified of this by a 'Communication Error' display message.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	None
; Shared Variables: BadCommunication (R) - error message to signify some invalid
;							serial output by motor unit
;					error (R/W) - whether a serial error occurred on either end
;							of the communication. If so, flag is reset.
; Global Variables: None
;
; Input:			Some invalid character output to remote unit through serial
;					channel or some serial error on either end.
; Output:			Communication error message displayed if the error was due to
;					invalid character output.
;
; Error Handling:	If a serial error occurred, this function helps ensure that
;					another junk character does not replace it immediately and that
;					it is only replaced once an end of line character is received
;					so the next status can be displayed.
;
; Algorithms:		None
; Data Structures:	None
;
; Registers Changed: ES, SI, flags
; Stack Depth:		1 word
;
; Limitations:		This function does not allow for a more specific message of what
;					input caused the communication error.
;
; Author:			Maitreyi Ashok
; Last Modified:	12/04/16	Maitreyi Ashok	Wrote code
;					12/05/16	Maitreyi Ashok	Updated comments

DispCommEr          PROC        NEAR
DispBadErrMsg:
        CMP     error, TRUE			; Check if a serial error occurred
        JNE     DisplayErrorMsg		; If not, then the error is due to invalid
									; character, so output that message
        ;JE     NoNeedToDisplay		; Otherwise, the error is due to a serial
									; error
NoNeedToDisplay:					; In that case no need to display another error
        MOV     error, FALSE		; message. Reset error flag so other status can
        JMP     DoneDispCommEr		; display again and return
DisplayErrorMsg:					
        PUSH    CS					; If invalid character was sent in serial channel
        POP     ES					; display BadCommunication string on display
        LEA     SI, BadCommunication	; to let user know
        CALL    Display
DoneDispCommEr:						; Once display message, done with error state
        RET
DispCommEr          ENDP

; DispStatus				
; Description: 		Displays status of motor unit, specifically whether there
;					was a parsing error on that end, and what the current speed
;					and direction of the RoboTrike is. Based on the status buffer
;					filled up during other states, the string is displayed. The
;					format is:
;						NP Speed ..... Direction ...
;					where . is a spot for a digit to be placed for speed and direction
;					status, and NP or PE indicates no parsing error or parsing error.
;					For more details, see complete functional specification. The
;					speed and direction will be displayed as decimal values.
;
; Operation:  		The status is displayed from the status buffer of the RoboTrike's
;					current status found from previous serial input from motor to
;					remote unit. The speed and direction have 5 and 3 characters,
;					respectively. The digits are left justified, so that there are
;					blank LEDs on the right if fewer than the maximum number of digits
;					are present. The buffer is then reset to have no parsing error
;					and the indices to enter speed and direction input are reset
;					to the start.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	None
; Shared Variables: StatusBuffer (R/W) - holds current status of motor unit of
;							RoboTrike, updated by serial input
;					speedIndex (W) - holds current index of entering digit in
;							StatusBuffer for current speed
;					directionIndex (W) - holds current index of entering digit
;							in StatusBuffer for current direction
; Global Variables: None
;
; Input:			A complete serial status string was sent.
; Output:			The parsed serial string is dispalyed to view status
;
; Error Handling:	In the case that a previous error messed up the buffer, the
;					LEDs which should always be blank are reset.
;
; Algorithms:		None
; Data Structures:	Array of characters holds status of RoboTrike to be updated
;					and displayed.
;
; Registers Changed: ES, SI, flags
; Stack Depth:		1 word
;
; Limitations:		The status buffer has a fixed format that can not be changed
;					based on the specific input. For example, even if the string
;					for speed is less than 5 characters, the buffer is fixed and
;					cannot be adjusted in size to remove unnecessary blank LEDs.
;
; Author:			Maitreyi Ashok
; Last Modified:	12/04/16	Maitreyi Ashok	Wrote code
;					12/05/16	Maitreyi Ashok	Updated comments

DispStatus          PROC        NEAR
ClearBadSpots:
        MOV     StatusBuffer[SPEED_START - 1], ' '		; In the case that an error
        MOV     StatusBuffer[SPEED_END + 1], ' '		; messed up the status buffer
        MOV     StatusBuffer[DIRECTION_START - 1], ' '	; clear out LEDs that should
														; be blank
DispStatusMsg:
        PUSH    DS									; Get the address of the 
        POP     ES									; StatusBuffer in ES:SI to be
        LEA     SI, StatusBuffer					; displayed in a scrollable
        CALL    Display								; display
ResetParsingError:
        MOV     StatusBuffer[PARSING_INDEX], 'N'	; The parsing error message
													; reset to no error for the next
        MOV     StatusBuffer[PARSING_INDEX + 1], 'P'
													; status to be displayed
ChangeIndicesToStart:
        MOV     speedIndex, SPEED_START				; Speed and direction current
        MOV     directionIndex, DIRECTION_START		; indices into buffer reset to
													; start index to overwrite old
													; value when next writing to buffer
        RET
DispStatus          ENDP

; AddSpeedChar		
; Description: 		Add digit to StatusBuffer for the speed status. This function
;					is called once a speed status character has been received and
;					any digit characters are then transmitted on the serial channel.
;					These digit characters are then placed in the correct location
;					for speed in the StatusBuffer and the speedIndex updated to
;					allow for the next speed digit to be later added. The speed
;					will be received as a decimal value.
;
; Operation:  		This function first converts the speed digit received to a
;					character and then places it in the StatusBuffer at the index
;					the speed is correctly at. If the end of the part of the buffer
;					for the speed has not been passed, the speed index is incremented
;					to the spot where the next speed digit will be placed at.
;
; Arguments:   		digit [AL] - the numerical digit to add to the status buffer
;							for speed
; Return Value:		None
;
; Local Variables:	None
; Shared Variables: speedIndex (R/W) - index into StatusBuffer of digit currently 
;							adding speed to
; Global Variables: None
;
; Input:			A digit was output through serial channel after a speed status
;					character
; Output:			The status buffer to be displayed is updated with new speed
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	Array of characters holds status of RoboTrike to be updated
;					and displayed.
;
; Registers Changed: SI, AL, flags
; Stack Depth:		0 words
;
; Limitations:		This function does not handle if the speedIndex is past the
;					bounds already due to some other misbehaving function, and
;					instead outputs to whatever index the value is currently at.
;
; Author:			Maitreyi Ashok
; Last Modified:	12/04/16	Maitreyi Ashok	Updated comments
;					12/05/16	Maitreyi Ashok	Wrote code

AddSpeedChar        PROC        NEAR
AddSpeedCharacter:
        MOV     SI, speedIndex			; The index of speed part to output the digit
										; at is found
        ADD     AL, '0'					; Convert numerical digit to ASCII character
WriteToBuffer:
        MOV     StatusBuffer[SI], AL	; Write the digit to the StatusBuffer
        CMP     SI, SPEED_END			; Check if past end of buffer
        JG      DoneAddingSpeedChar		; If so, then done adding the character
        ;JLE    MoveToNextSpeedIndex	
MoveToNextSpeedIndex:					; Otherwise, increment index of speed so can
        INC     speedIndex				; still add another character next time
        ;JMP    DoneAddingSpeedChar		; without overwriting current one
DoneAddingSpeedChar:					; Return to allow next character to be 
        RET								; processed
AddSpeedChar        ENDP

; AddDirChar			
; Description: 		Add digit to StatusBuffer for the direction status. This function
;					is called once a direction status character has been received and
;					any digit characters are then transmitted on the serial channel.
;					These digit characters are then placed in the correct location
;					for direction in the StatusBuffer and the directionIndex updated to
;					allow for the next direction digit to be later added. The
;					direction will be received as a decimal value.
;
; Operation:  		This function first converts the direction digit received to a
;					character and then places it in the StatusBuffer at the index
;					the direction is correctly at. If the end of the part of the buffer
;					for the direction has not been passed, the direction index is 
;					incremented to the spot where the next direction digit will 
;					be placed at.
;
; Arguments:   		digit [AL] - the numerical digit to add to the status buffer
;							for direction
; Return Value:		None
;
; Local Variables:	None
; Shared Variables: directionIndex (R/W) - index into StatusBuffer of digit 
;							currently adding direction to
; Global Variables: None
;
; Input:			A digit was output through serial channel after a direction
;					status character
; Output:			The status buffer to be displayed is updated with new direction
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	Array of characters holds status of RoboTrike to be updated
;					and displayed.
;
; Registers Changed: SI, AL
; Stack Depth:		0 words
;
; Limitations:		This function does not handle if the directionIndex is past the
;					bounds already due to some other misbehaving function, and
;					instead outputs to whatever index the value is currently at.
;
; Author:			Maitreyi Ashok
; Last Modified:	12/04/16	Maitreyi Ashok	Updated comments
;					12/05/16	Maitreyi Ashok	Wrote code

AddDirChar          PROC        NEAR
AddDirCharacter:
        MOV     SI, directionIndex		; The index of direction part to output digit
										; at is found
        ADD     AL, '0'					; Convert numerical digit to ASCII character
        MOV     StatusBuffer[SI], AL	; Write the digit to the StatusBuffer
        CMP     SI, DIRECTION_END		; Check if past end of buffer
        JG      DoneAddingDirChar		; If so, then done adding the character
        ;JLE    MoveToNextDirIndex
MoveToNextDirIndex:						; Otherwise increment index of direction
        INC     directionIndex			; so can still add another character
        ;JMP    DoneAddingDirChar		; next time without overwriting current one
DoneAddingDirChar:						; Return to allow next character to be
        RET								; processed
AddDirChar          ENDP

; EndOfSpeedString		
; Description: 		This function is called when an end of line character was
;					received when updating the speed in the StatusBuffer. The
;					function outputs extra blanks to the right of the speed
;					if not all the maximum number of digits were output to ensure
;					that parts of previous speeds are not displayed. The speeds
;					are left justified in their part of the buffer with extra 
;					blanks as needed to the right to have a speed of maximum 5 
;					characters displayed.
;
; Operation:  		This function first outputs spaces to the Status Buffer for
;					any unused digits to the right of the last input digit of speed. 
;					These will correspond to blank LEDs to overwrite any previous speed
;					data. Once the speed portion of the buffer is filled with 
;					status and blanks as needed, the new status buffer is displayed
;					with the parsing error if present, updated speed, and direction 
;					status.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	None
; Shared Variables: StatusBuffer (R/W) - holds current status of motor unit of
;							RoboTrike, updated by serial input
;					speedIndex (W) - holds current index of entering digit in
;							StatusBuffer for current speed
; Global Variables: None
;
; Input:			Speed status character, numerical digits, and end of line
;					character received.
; Output:			Status Buffer is displayed with updated string
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	Array of characters holds status of RoboTrike to be updated
;					and displayed.
;
; Registers Changed: SI, flags
; Stack Depth:		1 word
;
; Limitations:		This function wastes blanks at the end of speed if the speed
;					is not all of the 5 digits, making it harder to read the display
;					due to intervening blanks.
;
; Author:			Maitreyi Ashok
; Last Modified:	12/04/16	Maitreyi Ashok	Wrote code
;					12/05/16	Maitreyi Ashok	Updated comments

EndOfSpeedString    PROC        NEAR
CheckSpeedIndex:
        CMP     speedIndex, SPEED_END	; Check if end of string portion of buffer
        JG      DisplaySpeedString		; reached. If so, display the string
        ;JLE    AddSpeedBlank			; Otherwise, add blanks to buffer
AddSpeedBlank:
        MOV     SI, speedIndex			; Get the current index of speed into 
        MOV     StatusBuffer[SI], ' '	; buffer and write blank to buffer
        INC     speedIndex				; Increment index into buffer to check
        JMP     CheckSpeedIndex			; if need to add another blank
DisplaySpeedString:
        CALL    DispStatus				; Display the status with updated speed
        RET
EndOfSpeedString    ENDP

; EndOfDirString			
; Description: 		This function is called when an end of line character was
;					received when updating the direction in the StatusBuffer. The
;					function outputs extra blanks to the right of the direction
;					if not all the maximum number of digits were output to ensure
;					that parts of previous direction are not displayed. The directions
;					are left justified in their part of the buffer with extra 
;					blanks as needed to the right to have a direction of maximum 3 
;					characters displayed. 
;
; Operation:  		This function first outputs spaces to the Status Buffer for
;					any unused digits to the right of the last input digit of
;					direction. These will correspond to blank LEDs to overwrite 
;					any previous direction data. Once the direction portion of the 
;					buffer is filled with status and blanks as needed, the new 
;					status buffer is displayedwith the parsing error if present,
;					speed, and updated direction status.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	None
; Shared Variables: StatusBuffer (R/W) - holds current status of motor unit of
;							RoboTrike, updated by serial input
;					directionIndex (R/W) - holds current index of entering digit
;							in StatusBuffer for current direction
; Global Variables: None
;
; Input:			Direction status character, numerical digits, and end of line
;					character received.
; Output:			Status Buffer is displayed with updated string
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	Array of characters holds status of RoboTrike to be updated
;					and displayed.
;
; Registers Changed: SI, flags
; Stack Depth:		1 word
;
; Limitations:		This function wastes blanks at the end of direction if the 
;					direction is not all of the 3 digits, making it harder to read 
;					the display due to intervening blanks.
;
; Author:			Maitreyi Ashok
; Last Modified:	12/04/16	Maitreyi Ashok	Wrote code
;					12/05/16	Maitreyi Ashok	Updated comments

EndOfDirString    PROC        NEAR
CheckDirIndex:
        CMP     directionIndex, DIRECTION_END	; Check if end of direction portion
        JG      DisplayDirString		; of buffer reached. If so, display the string
        ;JLE    AddDirBlank				; Otherwise, add blanks to buffer
AddDirBlank:
        MOV     SI, directionIndex		; Get current index of direction into buffer
        MOV     StatusBuffer[SI], ' '	; and write blank to buffer
        INC     directionIndex			; Increment index into buffer to check if
        JMP     CheckDirIndex			; need to add another blank
DisplayDirString:
        CALL    DispStatus				; Display the status with updated direction
        RET
EndOfDirString    ENDP



; AddMotoDig	
; Description: 		Add a digit to the motor serial error value received on the
;					the serial channel. This digit is a digit of the line status
;					register value for the error that occurred. This value can
;					be added to the current value of the motor error to get the
;					value for the error. This value can then be converted to a
;					exact error message of the serial error. The value will be
;					received as a hexadecimal value.
;
; Operation:  		This function first shifts the previous digit(s) to the left
;					by 16, since the value received is a hexadecimal value. Then,
;					the next digit is added in to the right, to get the hexadecimal 
;					value of the digits received so far, where the digits are 
;					received from left to right. This value will be updated when
;					the next digit is received or displayed with a string of the
;					error message if end of line character received.
;
; Arguments:   		digit [AL] - numerical digit to add to the motor error value
; Return Value:		None
;
; Local Variables:	val [AX] - current value of motor error that digit is added
;							in to
; Shared Variables: motorError (R/W) - Line status register of motor error, updated
;							as more digits received
; Global Variables: None
;
; Input:			Digits input following a motor error character due to an error
;							in the motor unit.
; Output:			Digit added to error value whose exact error string will be
;							displayed once end of line character received
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	None
;
; Registers Changed: AX. BX
; Stack Depth:		0 words
;
; Limitations:		This function does not check the validity of digits passed in,
;							since only certain LSR values are valid. The function
;							instead saves all digits and allows the DisplayError
;							function to handle invalid values.
;
; Author:			Maitreyi Ashok
; Last Modified:	12/04/16	Maitreyi Ashok	Wrote code
;					12/05/16	Maitreyi Ashok	Updated comments

AddMotoDig          PROC        NEAR
ShiftLastDigit:
        XOR     AH, AH			; Clear high byte since may be required in 
        MOV     BX, AX			; calculations of new motor error if value invalid
        MOV     AL, motorError	; Get the current motor error value and shift
        IMUL    AX, AX, 16		; to left (hexadecimal values)
AddNewDigit:
        ADD     AX, BX			; Add in the new digit to the right and save
        MOV     motorError, AL	; this new value of the motor error
        RET
AddMotoDig      ENDP

; DispError		
; Description: 		This function displays a serial error for the motor unit on
;					the display using the DisplayErrors function. The purpose of
;					this function is to specify to the main DisplayErrors function
;					that this is a serial error from the motor and not the reomte
;					unit so that the according error message can be displayed as
;					well as to pass the parsed motor error value to the function.
;
; Operation:  		This function gets the value of the motor error into the argument
;					for the main DisplayErrors function. In addition, the DisplayErrors
;					function is passed an argument that the serial error is from
;					the motor unit. This way the error message can be prefixed
;					accordingly to alert the user. In addition, the motor unit
;					error is set back to the no error value until another
;					error is received.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	None
; Shared Variables: motorError (R/W) - Line status register of motor error, updated
;							as more digits received
; Global Variables: None
;
; Input:			Motor serial error character followed by digits and then
;							end of line character.
; Output:			String error message displayed
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	None
;
; Registers Changed: AL, CL
; Stack Depth:		1 word
;
; Limitations:		The motor error is directly passed to the DisplayErrors function
;					without checking its validity.
;
; Author:			Maitreyi Ashok
; Last Modified:	12/04/16	Maitreyi Ashok	Wrote code
;					12/05/16	Maitreyi Ashok	Updated comments

DispError           PROC        NEAR
CallDispWithLSRArg:
        MOV     AL, motorError				; Get motor error value as argument
        MOV     CL, MOTOR_ERROR				; to display as well as that error is
											; a motor serial one
        CALL    DisplayErrors				; Display error
        MOV     motorError, NO_LSR_ERROR	; Reset motor error value to no error
        RET
DispError           ENDP

; AddParse		
; Description: 		This function adds that there was a parsing error to the
;					StatusBuffer, in the two characcters at PARSING_INDEX in the
;					buffer. This function is called if a parsing error character
;					was passed through the serial channel due to an error in the
;					serial parser of the motor unit. This will then be displayed
;					acccordingly once end of line character received.
;
; Operation:  		This function adds two characters for a Parsing Error at the
;					digits pointed to by PARSING_INDEX in the StatusBuffer to 
;					signify that a parsing error has occurred in the motor unit.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	None
; Shared Variables: StatusBuffer (R/W) - holds current status of motor unit of
;							RoboTrike, updated by serial input 
; Global Variables: None
;
; Input:			A parsing error character indicating that motor unit had
;					parsing error
; Output:			Status Buffer eventually displayed with updated string
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	Array of characters holds status of RoboTrike to be updated
;					and displayed.
;
; Registers Changed: SI
; Stack Depth:		0 words
;
; Limitations:		The exact parsing error is not displayed to the user.
;
; Author:			Maitreyi Ashok
; Last Modified:	12/04/16	Maitreyi Ashok	Wrote code
;					12/05/16	Maitreyi Ashok	Updated comments

AddParse            PROC        NEAR
WriteParseError:
        MOV     SI, PARSING_INDEX		; Get index to write parsing error to
        MOV     StatusBuffer[SI], 'P'	; Write characters to signify parsing
        MOV     StatusBuffer[SI+1], 'E'	; error into buffer
        RET
AddParse            ENDP

; SetError		
; Description: 		This function sets that a serial error has been received on
;					either the motor or serial side. It moves the state machine
;					to the error state and sets the error flag. This is done so
;					other incoming characters are not used to update and display
;					the StatusBuffer until an end of line character is received.
;					This way, the error messages are not flashed and hidden 
;					immediately due to other incoming junk characters as part of
;					the error.
;
; Operation:  		This function moves the current state to the error state, which
;					can only be exited when a end of line character is received
;					after which any incoming status can be displayed.
;					In addition, the error flag is set to let the action routine
;					exiting the error state that an additional error message
;					need not be displayed.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	None
; Shared Variables: state (W)- stores current state in the parsing of command
;					error (W) - whether a serial error occurred on either end
;							of the communication
; Global Variables: None
;
; Input:			A serial error occurred in either motor or remote ends
; Output:			New status is not displayed until end of line character received
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	None
;
; Registers Changed: None
; Stack Depth:		0 words
;
; Limitations:		This function does not allow for critical status to be displayed
;					during the error message display.
;
; Author:			Maitreyi Ashok
; Last Modified:	12/04/16	Maitreyi Ashok	Wrote code
;					12/05/16	Maitreyi Ashok	Updated comments

SetError            PROC        NEAR
                    PUBLIC      SetError
SetErrorFlag:
        MOV     state, ST_ERROR		; Move FSM to error state and
        MOV     error, TRUE			; set the error flag
        RET
SetError            ENDP

; InitRemoteParser			
; Description: 		This function initializes the remote parser by starting from
;					the idle state with no error (so status can be displayed).
;					In addition, the status buffer is initialized to display
;					blank speed and direction as well as no parsing error.
;
; Operation:  		The function resets the error shared variable since there are
;					no errors when we start the state machine. In addition, the
;					FSM is started from the initial idle state from which it will
;					wait for a status/error letter to be entered. The status
;					string buffer to be displayed is also intialized to its
;					initial value.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	None
; Shared Variables: state (W)- stores current state in the parsing of command
;					error (W) - whether a serial error occurred on either end
;							of the communication
; Global Variables: None
;
; Input:			None
; Output:			None
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	Array of characters holds status of RoboTrike to be updated
;					and displayed.
;
; Registers Changed: AL, SI (By InitDisplayString)
; Stack Depth:		1 word
;
; Limitations:		The function does not allow for starting from various versions
;					of an idle state depending on initial conditions.
;
; Author:			Maitreyi Ashok
; Last Modified:	12/04/16	Maitreyi Ashok	Wrote code
;					12/05/16	Maitreyi Ashok	Updated comments

InitRemoteParser        PROC        NEAR
                        PUBLIC      InitRemoteParser
StartParserFromStart:
        MOV     state, ST_IDLE		; Start from idle state
        MOV     error, FALSE		; Start out with no serial error
		MOV     motorError, NO_LSR_ERROR
									; Start out with no serial motor error value
									; as well
        CALL    InitDisplayString	; Initialize the StatusBuffer string
        RET
InitRemoteParser        ENDP

; InitDisplayString		
; Description: 		This function initializes the status buffer display string and
;					its associated shared variables when starting the FSM. The
;					buffer is set up to have no parsing error displayed, as
;					well as 'Speed' and 'Direction' but no speed and direction
;					digits following with blanks replacing any spots where digits
;					can be added. In addition, the indices into the buffer for
;					speed and direction are reset to the start indices to start
;					adding digits from the left.
;
; Operation:  		The functin first resets the indices into the buffer for speed
;					and direction to their start, so that the digits entered will
;					be entered from the start of the area portioned to each status.
;					This way, even if the maximum number of digits is later entered,
;					all the digits can be displayed. Then, the status buffer is
;					filled based on the fixed start string to have no speed or
;					direction digits displayed but to show that there has been
;					no parsing error so far.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	bufferPtr [SI] - index into StatusBuffer and StatBufferString
;							to enter all initial characters of the buffer
; Shared Variables: speedIndex (W) - holds current index of entering digit in
;							StatusBuffer for current speed
;					StatusBuffer (W) - holds current status of motor unit of
;							RoboTrike, updated by serial input
;					directionIndex (W) - index into StatusBuffer of digit 
;							currently adding direction to
; Global Variables: None
;
; Input:			None
; Output:			None
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	Array of characters holds status of RoboTrike to be updated
;					and displayed.
;
; Registers Changed: SI, AL
; Stack Depth:		0 words
;
; Limitations:		This function does not allow for variable initial string
;					start based on input.
;
; Author:			Maitreyi Ashok
; Last Modified:	12/04/16	Maitreyi Ashok	Wrote code
;					12/05/16	Maitreyi Ashok	Updated comments

InitDisplayString       PROC        NEAR
ResetSharedVariables:             
        MOV     speedIndex, SPEED_START			; Start speed/direction indices
        MOV     directionIndex, DIRECTION_START	; from start of their positions
												; in buffer
CreateStatusBuffer:							
		MOV     StatusBuffer[0] , 'N'       ; Add characters to StatusBuffer for
        MOV     StatusBuffer[1], 'P'        ; 'NP Speed       Direction    '
        MOV     StatusBuffer[2], ' '
        MOV     StatusBuffer[3], 'S'        ; Write word 'Speed'
        MOV     StatusBuffer[4], 'p'
        MOV     StatusBuffer[5], 'e'
        MOV     StatusBuffer[6], 'e'
        MOV     StatusBuffer[7], 'd'
        MOV     SI, SPEED_START - 1
WriteSpeedSpaces:
        CMP     SI, SPEED_END + 1           ; Add spaces for digits to be entered for speed
        JG      WriteDirection
        ;JNE    WriteSpace
WriteSSpace:
        MOV     StatusBuffer[SI], ' '
        INC     SI
        JMP     WriteSpeedSpaces
WriteDirection:
        MOV     StatusBuffer[15], 'D'       ; Wrote word 'Direction'
        MOV     StatusBuffer[16], 'i'
        MOV     StatusBuffer[17], 'r'
        MOV     StatusBuffer[18], 'e'
        MOV     StatusBuffer[19], 'c'
        MOV     StatusBuffer[20], 't'
        MOV     StatusBuffer[21], 'i'
        MOV     StatusBuffer[22], 'o'
        MOV     StatusBuffer[23], 'n'
        MOV     SI, DIRECTION_START - 1
WriteDirectionSpaces:
        CMP     SI, DIRECTION_END           ; Add spaces for digits to be entered for direction
        JG      WriteEnd
        ;JNE    WriteSpace
WriteDSpace:
        MOV     StatusBuffer[SI], ' '
        INC     SI
        JMP     WriteDirectionSpaces
WriteEnd:
        MOV     StatusBuffer[28], ASCII_NULL    ; Terminate string of status buffer
DoneStatBuffer:								; Done setting up status buffer to
        RET									; be used to display status of RoboTrike
InitDisplayString       ENDP

; StateTable
;
; Description:      This is the state transition table for the state machine of
;					the remote parser. Each entry consists of the next state and 
;					action for that transition.  Each set of rows corresponds to 
;					the current state and each row within the set corresponds to 
;					a different input type. This is essentially a 2-D table with 
;					rows as current states and columns as input types. 
;
; Author:           Maitreyi Ashok
; Last Modified:    12/05/2016

;define a macro to make table a little more readable
;macro just does an offset of the action routine entries to build the STRUC
%*DEFINE(TRANSITION(nxtst, act))  (
    TRANSITION_ENTRY<%nxtst, OFFSET(%act) >
)

StateTable		LABEL	TRANSITION_ENTRY

	;Current State = ST_IDLE				Input Token Type
	%TRANSITION(ST_IDLE,doNOP)				;TOKEN_EOC
	%TRANSITION(ST_IDLE,doNOP) 				;TOKEN_IGNORE
	%TRANSITION(ST_ERROR,doNOP)		        ;TOKEN_DIGIT
	%TRANSITION(ST_DIR,doNOP)				;TOKEN_DIR
	%TRANSITION(ST_MOTO,doNOP)			    ;TOKEN_MOTO
	%TRANSITION(ST_SPEED,doNOP)			    ;TOKEN_SPEED
	%TRANSITION(ST_PARS,AddParse)		    ;TOKEN_PARS
    %TRANSITION(ST_ERROR,doNOP)             ;TOKEN_OTHER

    ;Current State = ST_ERROR				Input Token Type
    %TRANSITION(ST_IDLE,DispCommEr)			;TOKEN_EOC
	%TRANSITION(ST_ERROR,doNOP) 			;TOKEN_IGNORE
	%TRANSITION(ST_ERROR,doNOP)		        ;TOKEN_DIGIT
	%TRANSITION(ST_ERROR,doNOP)				;TOKEN_DIR
	%TRANSITION(ST_ERROR,doNOP)			    ;TOKEN_MOTO
	%TRANSITION(ST_ERROR,doNOP)			    ;TOKEN_SPEED
	%TRANSITION(ST_ERROR,doNOP)			    ;TOKEN_PARS
    %TRANSITION(ST_ERROR,doNOP)             ;TOKEN_OTHER

    ;Current State = ST_SPEED				Input Token Type
    %TRANSITION(ST_IDLE,EndOfSpeedString)	;TOKEN_EOC
	%TRANSITION(ST_SPEED,doNOP) 			;TOKEN_IGNORE
	%TRANSITION(ST_SPEED,AddSpeedChar)		;TOKEN_DIGIT
	%TRANSITION(ST_ERROR,doNOP)			    ;TOKEN_DIR
	%TRANSITION(ST_ERROR,doNOP)			    ;TOKEN_MOTO
	%TRANSITION(ST_ERROR,doNOP)			    ;TOKEN_SPEED
	%TRANSITION(ST_ERROR,doNOP)			    ;TOKEN_PARS
    %TRANSITION(ST_ERROR,doNOP)             ;TOKEN_OTHER

    ;Current State = ST_DIR					Input Token Type
    %TRANSITION(ST_IDLE,EndOfDirString)		;TOKEN_EOC
	%TRANSITION(ST_DIR,doNOP) 			    ;TOKEN_IGNORE
	%TRANSITION(ST_DIR,AddDirChar)		    ;TOKEN_DIGIT
	%TRANSITION(ST_ERROR,doNOP)				;TOKEN_DIR
	%TRANSITION(ST_ERROR,doNOP)			    ;TOKEN_MOTO
	%TRANSITION(ST_ERROR,doNOP)			    ;TOKEN_SPEED
	%TRANSITION(ST_ERROR,doNOP)			    ;TOKEN_PARS
    %TRANSITION(ST_ERROR,doNOP)             ;TOKEN_OTHER
    
    ;Current State = ST_MOTO				Input Token Type
    %TRANSITION(ST_IDLE,DispError)			;TOKEN_EOC
	%TRANSITION(ST_MOTO,doNOP) 				;TOKEN_IGNORE
	%TRANSITION(ST_MOTO,AddMotoDig)		    ;TOKEN_DIGIT
	%TRANSITION(ST_ERROR,doNOP)				;TOKEN_DIR
	%TRANSITION(ST_ERROR,doNOP)			    ;TOKEN_MOTO
	%TRANSITION(ST_ERROR,doNOP)			    ;TOKEN_SPEED
	%TRANSITION(ST_ERROR,doNOP)			    ;TOKEN_PARS
    %TRANSITION(ST_ERROR,doNOP)             ;TOKEN_OTHER
    
    ;Current State = ST_PARS				Input Token Type
    %TRANSITION(ST_IDLE,DispStatus)			;TOKEN_EOC
	%TRANSITION(ST_PARS,doNOP) 				;TOKEN_IGNORE
	%TRANSITION(ST_ERROR,doNOP)		        ;TOKEN_DIGIT
	%TRANSITION(ST_ERROR,doNOP)				;TOKEN_DIR
	%TRANSITION(ST_ERROR,doNOP)			    ;TOKEN_MOTO
	%TRANSITION(ST_ERROR,doNOP)			    ;TOKEN_SPEED
	%TRANSITION(ST_ERROR,doNOP)			    ;TOKEN_PARS
    %TRANSITION(ST_ERROR,doNOP)             ;TOKEN_OTHER
 
; Token Tables
;
; Description:      This creates the tables of token types and token values for
;					the remote parser. Each entry corresponds to the token type 
;					and the token value for a character.  Macros are used to 
;					actually build two separate tables - TokenTypeTable for token 
;					types and TokenValueTable for token values.
;
; Author:           Maitreyi Ashok
; Last Modified:    12/05/2016 

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
        %TABENT(TOKEN_OTHER, '+')	;+ 
        %TABENT(TOKEN_OTHER, 44)	;,
        %TABENT(TOKEN_OTHER, '-')	;-
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
        %TABENT(TOKEN_DIR, 'D')	    ;D (Direction Status)
        %TABENT(TOKEN_OTHER, 'E')	;E
        %TABENT(TOKEN_OTHER, 'F')	;F
        %TABENT(TOKEN_OTHER, 'G')	;G
        %TABENT(TOKEN_OTHER, 'H')	;H
        %TABENT(TOKEN_OTHER, 'I')	;I
        %TABENT(TOKEN_OTHER, 'J')	;J
        %TABENT(TOKEN_OTHER, 'K')	;K
        %TABENT(TOKEN_OTHER, 'L')	;L
        %TABENT(TOKEN_MOTO, 'M')	;M (Motor Error)
        %TABENT(TOKEN_OTHER, 'N')	;N
        %TABENT(TOKEN_OTHER, 'O')	;O 
        %TABENT(TOKEN_PARS, 'P')	;P (Parsing Error)
        %TABENT(TOKEN_OTHER, 'Q')	;Q
        %TABENT(TOKEN_OTHER, 'R')	;R
        %TABENT(TOKEN_SPEED, 'S')	;S (Speed Status)
        %TABENT(TOKEN_OTHER, 'T')	;T
        %TABENT(TOKEN_OTHER, 'U')	;U
        %TABENT(TOKEN_OTHER, 'V')	;V
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
        %TABENT(TOKEN_OTHER, 10)	;a
        %TABENT(TOKEN_OTHER, 11)	;b
        %TABENT(TOKEN_OTHER, 12)	;c
        %TABENT(TOKEN_DIR, 13)	    ;d (Direction Status)
        %TABENT(TOKEN_OTHER, 14)	;e
        %TABENT(TOKEN_OTHER, 15)	;f
        %TABENT(TOKEN_OTHER, 'g')	;g
        %TABENT(TOKEN_OTHER, 'h')	;h
        %TABENT(TOKEN_OTHER, 'i')	;i
        %TABENT(TOKEN_OTHER, 'j')	;j
        %TABENT(TOKEN_OTHER, 'k')	;k
        %TABENT(TOKEN_OTHER, 'l')	;l
        %TABENT(TOKEN_MOTO, 'm')	;m (Motor Error)
        %TABENT(TOKEN_OTHER, 'n')	;n
        %TABENT(TOKEN_OTHER, 'o')	;o
        %TABENT(TOKEN_PARS, 'p')	;p (Parsing Error)
        %TABENT(TOKEN_OTHER, 'q')	;q
        %TABENT(TOKEN_OTHER, 'r')	;r
        %TABENT(TOKEN_SPEED, 's')	;s (Speed Status)
        %TABENT(TOKEN_OTHER, 't')	;t
        %TABENT(TOKEN_OTHER, 'u')	;u
        %TABENT(TOKEN_OTHER, 'v')	;v
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

; This is the string to be displayed when an invalid character is received
; through serial output from motor
BadCommunication	LABEL	BYTE
        DB      'Communication Error', 0

CODE    ENDS

;the data segment

DATA    SEGMENT PUBLIC  'DATA'
speedIndex      DW      ?			; Index of which digit of speed is being 
									; entered to status buffer
directionIndex  DW      ?			; Index of which digit of direction is being
									; entered to status buffer
StatusBuffer    DB      STAT_BUFF_SIZE  DUP (?)
									; Status buffer of speed, direction, and parsing
									; error status to be updated and displayed
									; based on serial input
state           DB      ?			; stores number of state of FSM currently on
motorError      DB      ?			; Stores LSR value of motor serial error passed
									; in
error           DB      ?			; Flag to indicate whether motor or remote serial
									; error is being displayed
DATA    ENDS
        END
