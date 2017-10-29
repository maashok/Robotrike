; Maitreyi Ashok
; Section 1 – Richard Lee

        NAME    RemoFns

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   RemoFns                                  ;
;                              Remote Unit Routines	                         ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; This file contains  routines used by the remote system main loop
;
; Table of Contents:
; Functions
; InitRemoteLoopParts - Initialize all peripherals/routines/chip select/interrupts
; 					for remote unit
; KeypadHandler - Handle keypad events in EventQueue
; DisplayErrors - Display serial errors from motor or remote units
; 
; Macros
; TABENT - Definitions to create table entry with either UI key information or
;			with error strings

; Tables
; MainKeyTable - Key table for main menu of keypad
; SpeedKeyTable - Key table for speed menu of keypad
; AngleKeyTable - Key table for Angle menu of keypad
; AdditionalKeyTable - Key table for additional menu of keypad
; EventTable - Table for functions to perform for different events in eventqueue
; ErrorTable - Table of strings to display for serial errors
;
; Revision History:
;     11/29/2016	Maitreyi Ashok	Functional Specification/Pseudocode
;	  12/03/2016	Maitreyi Ashok	Wrote code
;	  12/04/2016	Maitreyi Ashok	Updated code
;	  12/05/2016	Maitreyi Ashok	Updated comments

$INCLUDE(RemoFns.inc)		; Include remote function definitions
$INCLUDE(General.inc)		; Include general definitions
$INCLUDE(Keypad.inc)		; Use for key value definitions

CGROUP  GROUP   CODE
DGROUP	GROUP	DATA

CODE	SEGMENT PUBLIC 'CODE'


        ASSUME  CS:CGROUP, DS:DGROUP
		
		EXTRN	doNOP:NEAR					; Perform no action
		EXTRN	SerialPutString:NEAR		; Put string on serial channel
		EXTRN	InitEventQueue:NEAR			; Initialize event queue
		EXTRN	InitCS:NEAR					; Initialize chip selects
		EXTRN	ClrIRQVectors:NEAR			; Clear unused interrupt vectors
		EXTRN	InstallTimer2Handler:NEAR	; Install timer 2 event handler 
		EXTRN	InitTimer2:NEAR				; and initialize timer 2
        EXTRN   InstallInt2Handler:NEAR		; Install int 2 event handler 
        EXTRN   InitInt2:NEAR				; and initialize int 2
		EXTRN	InitDigitMux:NEAR			; Intialize muxing of digits
		EXTRN	InitKeypad:NEAR				; Intialize keypad routines
		EXTRN	InitSerial:NEAR				; Initialize serial routines
        EXTRN   InitRemoteParser:NEAR		; Initialize remote side parser
        EXTRN   Display:NEAR				; Display error strings
        EXTRN   BlinkChange:NEAR			; Change blinking, scrolling 
        EXTRN   ScrollDisplayLeft:NEAR		; based on key input
        EXTRN   ScrollDisplayRight:NEAR
        EXTRN   ParseDisplayString:NEAR		; Parse character received to display
        EXTRN   SetError:NEAR				; Set that serial error occurred
        EXTRN   StartAutoScrolling:NEAR		; Start and stop scrolling based on
        EXTRN   StopAutoScrolling:NEAR		; key input
        

; InitRemoteLoopParts			
; Description: 		This function sets up all the parts needed for the remote loop
;					to work. This involves initializing the event queue to process
;					and store events to handle. In addition, the chip selects are
;					initialized and interrupt vector table vectors cleared. The
;					timer 2 and int 2 interrutps are set up to allow for keypad/display
;					as well as serial handling, respectively. The digit muxing, 
;					keypad, and serial routines are initialized individually. In
;					addition, values used in creating display strings and key
;					value arguments are initialized. The keytables and remote
;					parsers are set up as well.
;
; Operation:  		This function sets up all the above detailed parts of the remote
;					unit by calling dedicated functions for each of these tasks.
;					During this initialization, interrupts are not allowed as they
;					may cause the initializations to be affected. In addition, 
;					initialization is done for the remote function routines themselves
;					so that the values used in creating strings to display and 
;					use as arguments is set. The keypad also starts with the main
;					menu first.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	None
; Shared Variables: KeyTable (W) - which keypad menu using currently
;					EntryNo (W) - numbering of strings in keypad tables
;					ErrorNo (W) - numbering of strings in error string tables
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
; Last Modified:	12/02/16	Maitreyi Ashok	Wrote code
;					12/04/16	Maitreyi Ashok	Added string index setting
;					12/05/16	Maitreyi Ashok	Updated comments

InitRemoteLoopParts  PROC        NEAR
					 PUBLIC		 InitRemoteLoopParts
InitQueue:
        CLI							; Disallow interrupts during initialization
		CALL	InitEventQueue		; Initialize event queue to hold events
Init80188General:
		CALL	InitCS				; Initialize chip selects and clear
		CALL	ClrIRQVectors		; interrupt vector table
InitInterrupts:
		CALL	InstallTimer2Handler	; Set up interrupt vector table to handle
		CALL	InitTimer2				; timer 2 interrupts and set up timer 2
        CALL    InitInt2				; Set up interrupt vector table to handle
        CALL    InstallInt2Handler		; int 2 interrupts and set up int 2
InitPeripherals:
		CALL	InitDigitMux		; Initialize muxing of digits
		CALL	InitKeypad			; Initialize the keypad scanning
		CALL	InitSerial			; Initialize serial channel routines
DoneInit:
		%SET(EntryNo, 0)			; Start indices of strings used in tables	
        %SET(ErrorNum, 0)			; from start - value itself doens't really matter
        MOV     KeyTable, OFFSET(MainKeyTable)
									; Start keypad so keys refer to main menu
        CALL    InitRemoteParser	; Initialize the remote parser
        STI							; Allow interrutps to start happening again
		RET
InitRemoteLoopParts	ENDP


; KeypadHandler	
; Description: 		This function handles any keypad events in the EventQueue. 
;					This is done by finding which key table to refer to based on
;					previous input and then finding the index into the table based
;					on the key value given by the keypad scanning function. This
;					is then used to find the appropriate function to perform along
;					with the argument string used for the function. This function
;					is called, after which the command table to use is switched
;					to reflect the current key press.
;
; Operation:  		The function first finds the row that the key pressed was in
;					by using the KEY_ROW_NUM_MASK to find this value in the key
;					value passed in. This is used to find out which offset from
;					the start that the row for this key is from the start of the
;					key table. The other part of the key value is then used to
;					find out which key in the row was pressed. This is done by
;					checking the bits in part of the key value, each of which refers
;					to one key in the row. If the bit is cleared (active low),
;					meaning the key was pressed, this key is handled. This is
;					done by finding the exacct offset in the key table for this
;					STRUC. Then the associated function is called with the argument
;					specified. Finally, the command table is changed to reflect
;					the key press.
;
; Arguments:   		keyPressed [AL] - key value of key pressed on keypad, contains
;							row number and encoded value of key in row
; Return Value:		None
;
; Local Variables:	keyTableOffset [AX] - offset into key table of key pressed
; Shared Variables: KeyTable (R/W) - which table to use to get meanings of keys
;							pressed
; Global Variables: None
;
; Input:			A key is pressed on the keypad
; Output:			The display is changed based on certain key presses, and serial
;					commands are sent for others. For more details, see complete
;					functional specification.
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	Key tables with function, argument, and command table for each
;					key
;
; Registers Changed: AX, BX, CX, SI, flags
; Stack Depth:		2 words
;
; Limitations:		If multiple keys in the same row were pressed, the rightmost
;					key pressed is the only one handled. Assumes no error would
;					have happened where no key was actually pressed but event
;					enqueued. If that happens, the code breaks
;
; Author:			Maitreyi Ashok
; Last Modified:	12/02/16	Maitreyi Ashok	Wrote code
;					12/03/16	Maitreyi Ashok	Updated code
;					12/05/16	Maitreyi Ashok	Updated comments

KeypadHandler		PROC		NEAR
					PUBLIC		KeypadHandler
GetIndexOfKeypadRow:
        XOR     AH, AH					; Clear high byte since values will overflow
										; into it when multiplying
        MOV     BX, AX					; Save key value to be used later
		AND		AX, KEY_ROW_NUM_MASK	; Get the row number of key pressed
		SHR		AX, ROW_NUMBER_SHIFT	; by masking those bits
		MOV  	CX, (SIZE KeyStruc + KEY_STRING_SIZE) * 4
										; Use row number to get offset of row 
		MUL		CX						; in the key table
GetIndexOfKeypadColumn:		
		TEST	BX, PRESSED_CHECK		; For each key in the row, check whether
		JZ		GetKeyStruc				; it is pressed. If it is, handle it
		;JNZ	TryNextKey				; If not, try the next key
TryNextKey:
		ADD		AX, SIZE KeyStruc + KEY_STRING_SIZE
										; For every key not pressed, add its offset
										; to index into key table
		SHR		BX, 1					; Look at next key in the row to see
		JMP		GetIndexOfKeypadColumn	; whether it was pressed
GetKeyStruc:
		MOV		SI, KeyTable			; Get the offset of the current key table
        ADD     SI, AX					; and add the index into the table
        ADD     SI, KEY_STRING_SIZE		; Also add offset of string declared inside
										; table
CallAssociatedFunction:
        PUSH    SI						; Save value since might be changed by
										; action function
        PUSH    CS
        POP     ES
		MOV		AX, CS:[SI].FunctionArg	; Get the argument to call the function with
		CALL    CS:[SI].Function		; as well as the function itself and call it
        POP     SI						; Restore the value to use again
UpdateNextCommandTable:
		MOV		CX, CS:[SI].CmdTable	; Update the command table based on the
		MOV		KeyTable, CX			; key pressed
		RET
KeypadHandler	ENDP

; DisplayErrors		
; Description: 		This function displays an error message on the display based
;					on the Line Status Register of the motor or remote unit
;					serial error. If there are multiple errors in the LSR value
;					all of these are displayed as part of the same string. If
;					the LSR value is out of bounds, nothing about the error is
;					output to the display. If the error was a motor unit error,
;					then the remote parser will take care of outputting a 
;					communication error message. 
;
; Operation:  		This function first checks whether the error value of the LSR
;					passed in is within the bounds of possible errors. In addition,
;					it is checked that the DataReady bit is not set, since it
;					being set is not an error. If any of these are not met, then
;					an error message is not displayed. If they are met, the value
;					of the LSR is used as an index into the ErrorTable to retrieve
;					the string to output. This string is then output to the
;					Display with a prefix of whether it was due to motor or remote
;					unit serial errors.
;
; Arguments:   		LSRVal [AX] - value of LSR that holds what serial error occurred
; Return Value:		String displays meaning of LSR value
;
; Local Variables:	errorIndex [AX] - index into error table of string to display
; Shared Variables: None
; Global Variables: None
;
; Input:			A serial error occurs on motor or remote unit
; Output:			The error message is displayed detailing the exact error
;
; Error Handling:	If the LSR value is not valid, nothing is output
;
; Algorithms:		None
; Data Structures:	A table contains strings for all the possible serial error values.
;
; Registers Changed: AX, CL, SI, ES, flags
; Stack Depth:		1 word
;
; Limitations:		This function does not allow for any errors other than serial
;					errors to be output
;
; Author:			Maitreyi Ashok
; Last Modified:	12/04/16	Maitreyi Ashok	Wrote code
;					12/05/16	Maitreyi AShok	Updated comments

DisplayErrors       PROC        NEAR
                    PUBLIC      DisplayErrors
CheckErrorValueMin:
		CMP		AL, MIN_ERROR			; Check if the error value is too small
		JL		DoneDisplayError		; If so, done trying to display error
		;JGE	CheckErrorValueMax		; If not check if too large
CheckErrorValueMax:
		CMP		AL, MAX_ERROR			; If error value too large, done
		JG		DoneDisplayError		; trying to display error
		;JLE	CheckDataReadyBitNotSet	; If not, check if data ready bit set
CheckDataReadyBitNotSet:
		TEST	AL, ERROR_CHECK_MASK	; Check if data ready bit set
		JNZ		DoneDisplayError		; If it is, done trying to display error
		;JZ		GetIndexToTable			; If not, get string to display
GetIndexToTable:
        CALL    SetError				; Set that a serial error occurred
        XOR     AH, AH
        IMUL    AX, AX, ERR_STRING_SIZE	; Multiply the LSR value by the 
										; size of each error string to get index
										; to that string of the table
        CMP     CL, 'R'					; If the error was not a remote serial error
        JNE     LookUpValue				; Then output error with motor prefix
        ADD     AX, ERR_STRING_SIZE		; Otherwise find string with remote prefix
LookUpValue:
        MOV     SI, OFFSET(ErrorTable)	; Get offset of error table itself
        ADD     SI, AX					; and add in offset from start of table

DisplayIndexError:
        PUSH    CS						; Output this string by calling display
        POP     ES						; with the string as an argument
        CALL    Display
DoneDisplayError:
        RET
DisplayErrors        ENDP
		
; Description:		This function sets the entries of the Key Table for the
;					different keys. It stores the function to call when the key
;					is pressed, the string argument to call it with, as well as
;					the menu to move to. If there are two string arguments, these
;					arguments are concatenated with a carriage return to allow
;					for multiple commands to be sent as part of one key press.
; Operation:		This function first creates one string in the code segment
;					by concatenating the two strings passed in with a carriage 
;					return if the second string is not blank. Otherwise, the
;					strings are just null terminated. Then, a KeyStruc is created
;					with the action, command table to move to, and string argument
;					to call the function with. In addition, the counter of strings
;					created is incremented to allow for creation of unique strings.
; 
; Arguments:		function - what function to perform when key is pressed
;					table - what menu table to move to when key is pressed
;					string1/string2 - what string(s) to send through serial output
;							when key is pressed (if key does not send anything
;							through serial output, the values are just fillers)
; Registers Changed: None
; Stack Depth:		0 words
;
; Author:			Maitreyi Ashok
; LastModified:		12/05/16

%*DEFINE(TABENT(function, table, string1, string2)) (
String%EntryNo		LABEL	BYTE			; Create a string based on the string(s)
    %IF (%EQS(%string2,)) THEN (			; input
        DB  %string1, 0
    ) ELSE (
    	DB  %string1, 13, %string2, 0
    ) FI
	KeyStruc<%function, OFFSET(String%EntryNo), OFFSET(%table)>
											; Create struc with information in table
	%SET(EntryNo, %EntryNo + 1)				; Update string counter
)

; Description:		This is the table that contains the functions to perform along
;					with the arguments to pass in for each of the keys when the 
;					keypad is in the main menu state. In addition, it contains which
;					menu table to move to based on this key press. Thus, each
;					entry of the table corresponds to the key, with the keys
;					listed horizontally in a row, and each group of keys corresponding
;					to a row.
; Author:			Maitreyi Ashok
; Last Modified:	12/05/16
MainKeyTable	LABEL	KeyStruc
    %TABENT(SerialPutString,MainKeyTable,'F            ',)	; Laser fire key
	%TABENT(SerialPutString,MainKeyTable,'O            ',)	; Lasr off key
	%TABENT(doNOP,AdditionalKeyTable,'0            ',)		; Move to additional menu
	%TABENT(doNOP,SpeedKeyTable,'0            ',)			; Move to speed menu
	 
	%TABENT(SerialPutString,MainKeyTable,'D-45         ',)	; Move forwards left
    %TABENT(SerialPutString,MainKeyTable,'D0           ',)	; Move forward
    %TABENT(SerialPutString,MainKeyTable,'D45          ',)	; Move forwards right
	%TABENT(doNOP,AngleKeyTable,'0            ',)			; Move to angle menu

   	%TABENT(SerialPutString,MainKeyTable,'D-90         ',)	; Move left
	%TABENT(SerialPutString,MainKeyTable,'S0           ',)	; Stop
	%TABENT(SerialPutString,MainKeyTable,'D90          ',)	; Move right
	%TABENT(SerialPutString,MainKeyTable,'S32767       ',)	; Move at half speed
    
   	%TABENT(SerialPutString,MainKeyTable,'D-135        ',)	; Move backwards left
	%TABENT(SerialPutString,MainKeyTable,'D180         ',)	; Move backwards
	%TABENT(SerialPutString,MainKeyTable,'D-135        ',)	; Move backwards right
	%TABENT(SerialPutString,MainKeyTable,'S32767','V32767')	; Move full speed

; Description:		This is the table that contains the functions to perform along
;					with the arguments to pass in for each of the keys when the 
;					keypad is in the speed menu state. In addition, it contains which
;					menu table to move to based on this key press. Thus, each
;					entry of the table corresponds to the key, with the keys
;					listed horizontally in a row, and each group of keys corresponding
;					to a row.
; Author:			Maitreyi Ashok
; Last Modified:	12/05/16

SpeedKeyTable       LABEL   KeyStruc
    %TABENT(SerialPutString,SpeedKeyTable,'S32767','V32767')	; 100 percent speed
    %TABENT(SerialPutString,SpeedKeyTable,'S32767','V26213')	; 90 percent speed	
    %TABENT(SerialPutString,SpeedKeyTable,'S32767','V19660')	; 80 percent speed
	%TABENT(doNOP,MainKeyTable,'0            ',)				; Go to main menu
    
    %TABENT(SerialPutString,SpeedKeyTable,'S32767','V13107')	; 70 percent speed
    %TABENT(SerialPutString,SpeedKeyTable,'S32767','V6553 ')	; 60 percent speed
    %TABENT(SerialPutString,SpeedKeyTable,'S32767       ',)		; 50 percent speed
    %TABENT(SerialPutString,SpeedKeyTable,'S26213       ',)		; 40 percent speed
			
    %TABENT(SerialPutString,SpeedKeyTable,'S19660       ',)		; 30 percent speed
    %TABENT(SerialPutString,SpeedKeyTable,'S13107       ',)		; 20 percent speed
    %TABENT(SerialPutString,SpeedKeyTable,'S6553        ',)		; 10 percent speed
    %TABENT(SerialPutString,SpeedKeyTable,'S0           ',)		; 0 percent speed
    
    %TABENT(SerialPutString,SpeedKeyTable,'V1310        ',)		; Speed up 2 percent
    %TABENT(SerialPutString,SpeedKeyTable,'V-1310       ',)		; Slow down 2 percent
    %TABENT(SerialPutString,SpeedKeyTable,'V13107       ',)		; Speed up 20 percent
    %TABENT(SerialPutString,SpeedKeyTable,'V-13107      ',)		; Slow down 20 percent
 
; Description:		This is the table that contains the functions to perform along
;					with the arguments to pass in for each of the keys when the 
;					keypad is in the angle menu state. In addition, it contains which
;					menu table to move to based on this key press. Thus, each
;					entry of the table corresponds to the key, with the keys
;					listed horizontally in a row, and each group of keys corresponding
;					to a row. All angles listed are from current orientation.
; Author:			Maitreyi Ashok
; Last Modified:	12/05/16 
	
AngleKeyTable	LABEL	KeyStruc
    %TABENT(SerialPutString,AngleKeyTable,'D320         ',)	; Move at 320 degrees
    %TABENT(SerialPutString,AngleKeyTable,'D300         ',)	; Move at 300 degrees
    %TABENT(SerialPutString,AngleKeyTable,'D270         ',)	; Move at 270 degrees
    %TABENT(SerialPutString,AngleKeyTable,'D240         ',)	; Move at 240 degrees

    %TABENT(SerialPutString,AngleKeyTable,'D210         ',)	; Move at 210 degrees
    %TABENT(SerialPutString,AngleKeyTable,'D180         ',)	; Move at 180 degrees
    %TABENT(SerialPutString,AngleKeyTable,'D150         ',)	; Move at 150 degrees
	%TABENT(doNOP,MainKeyTable,'0            ',)			; Go to main menu

    %TABENT(SerialPutString,AngleKeyTable,'D120         ',)	; Move at 120 degrees
    %TABENT(SerialPutString,AngleKeyTable,'D90          ',)	; Move at 90 degrees
    %TABENT(SerialPutString,AngleKeyTable,'D60          ',)	; Move at 60 degrees
    %TABENT(SerialPutString,AngleKeyTable,'D30          ',)	; Move at 30 degrees

    %TABENT(SerialPutString,AngleKeyTable,'D+5          ',)	; Increment +5 degrees
    %TABENT(SerialPutString,AngleKeyTable,'D-5          ',)	; Decrement -5 degrees
    %TABENT(SerialPutString,AngleKeyTable,'D+50         ',)	; Increment +50 degrees
    %TABENT(SerialPutString,AngleKeyTable,'D-50         ',)	; Decrement -50 degrees
	
; Description:		This is the table that contains the functions to perform along
;					with the arguments to pass in for each of the keys when the 
;					keypad is in the additional menu state. In addition, it contains 
;					which menu table to move to based on this key press. Thus, each
;					entry of the table corresponds to the key, with the keys
;					listed horizontally in a row, and each group of keys corresponding
;					to a row.
; Author:			Maitreyi Ashok
; Last Modified:	12/05/16

AdditionalKeyTable	LABEL	KeyStruc
	%TABENT(ScrollDisplayLeft,AdditionalKeyTable,'0            ',)
															; Scroll display left
	%TABENT(ScrollDisplayRight,AdditionalKeyTable,'0            ',)
															; Scroll display right
	%TABENT(doNOP,MainKeyTable,'0            ',)
															; Go to main menu
	%TABENT(BlinkChange,AdditionalKeyTable,'0            ',); Change blinking
	
	%TABENT(StartAutoscrolling,AdditionalKeyTable,'0            ',)
															; Start autoscrolling
	%TABENT(StopAutoscrolling,AdditionalKeyTable,'0            ',)
															; Stop autoscrolling
	%TABENT(doNOP,AdditionalKeyTable,'0            ',)		; Other keys do nothing
	%TABENT(doNOP,AdditionalKeyTable,'0            ',)
	
	%TABENT(doNOP,AdditionalKeyTable,'0            ',)
	%TABENT(doNOP,AdditionalKeyTable,'0            ',)
	%TABENT(doNOP,AdditionalKeyTable,'0            ',)
	%TABENT(doNOP,AdditionalKeyTable,'0            ',)
	
	%TABENT(doNOP,AdditionalKeyTable,'0            ',)
	%TABENT(doNOP,AdditionalKeyTable,'0            ',)
	%TABENT(doNOP,AdditionalKeyTable,'0            ',)
	%TABENT(doNOP,AdditionalKeyTable,'0            ',)

; EventTable
; Description:		This table contains the event handler for each type of event
;					in the EventQueue. If the event is of type NOP_EVENT, the
;					function NOPs and does not do anything. If the event is of
;					type KEY_EVENT, the keypad handler is used to handle the key
;					press. If the event is of type SERIAL_DATA_RECEIVED, the data
;					received is parsed by the remote parser. If the event is
;					of type SERIAL_ERROR, the error is diaplayed.
;
; Author:			Maitreyi Ashok
; Last Modified:	12/05/16
EventTable	LABEL	WORD
			PUBLIC	EventTable
; 	DW	eventHandler
    DW	doNOP					; Event queue empty
	DW	KeypadHandler			; Key pressed on keypad
	DW	ParseDisplayString		; Serial status from motor unit
	DW	DisplayErrors          	; Serial error on remote unit

; This macro defines the strings for each error description in the error table
; by concatenating the string and its type (motor or remote error)
%*DEFINE(TABENT(string, type)) (
Error%ErrorNum		LABEL	BYTE
    DB  %type, %string, 0
	%SET(ErrorNum, %ErrorNum + 1)
)

; ErrorTable
; Description:		This table contains the strings to display for different
;					serial motor or remote errors based on what the value of the
;					line status register is. The LSR value can be used as an index
;					into the table to find the exact string to display
;
; Author:			Maitreyi Ashok
; Last Modified:	12/05/16
ErrorTable  LABEL   WORD
    ; 11 byte entry of error string
    %TABENT(' Nothing ','M')     	 ; LSR = 0, No line status
    %TABENT(' Nothing ','S')         
    %TABENT(' Overrun ','M')         ; LSR = 2, Overrun Error
    %TABENT(' Overrun ','S')         
    %TABENT(' Parity  ','M')         ; LSR = 4, Parity Error
    %TABENT(' Parity  ','S')         
    %TABENT(' Par/Over','M')         ; LSR = 6, Parity Error and Overrun error
    %TABENT(' Par/Over','S')         
    %TABENT(' Framing ','M')         ; LSR = 8, Framing Error
    %TABENT(' Framing ','S')         
    %TABENT(' Fram/Ove','M')         ; LSR = 10, Framing error and overrun error
    %TABENT(' Fram/Ove','S') 
    %TABENT(' Fr/Pari ','M')         ; LSR = 12, Framing and Parity Error
    %TABENT(' Fr/Pari ','S') 
    %TABENT(' Fr/Pa/Ov','M')         ; LSR = 14, Framing and parity and overrun error
    %TABENT(' Fr/Pa/Ov','S') 
    %TABENT(' BreakInt','M')         ; LSR = 16, Break interrupt
    %TABENT(' BreakInt','S') 
    %TABENT(' Break/Ov','M')         ; LSR = 18, Break interrupt and overrun error
    %TABENT(' Break/Ov','S') 
    %TABENT(' Break/Pa','M')         ; LSR = 20, Break interrupt and parity error
    %TABENT(' Break/Pa','S') 
    %TABENT(' Br/Pa/Ov','M')         ; LSR = 22, Break interrupt and parity and 
    %TABENT(' Br/Pa/Ov','S') 		 ; overrun error
    %TABENT(' Bre/Fra ','M')         ; LSR = 24, Break interrupt and framing error
    %TABENT(' Bre/Fra ','S') 
    %TABENT(' Br/Fr/Ov','M')         ; LSR = 26, Break interrupt and framing and 
    %TABENT(' Br/Fr/Ov','S') 		 ; overrun error
    %TABENT(' Br/Fr/Pa','M')         ; LSR = 28, Break interrupt and framing and 
    %TABENT(' Br/Fr/Pa','S') 		 ; parity error
    %TABENT(' B/F/P/O ','M')         ; LSR = 30, Break interrupt and framing and 
    %TABENT(' B/F/P/O ','S') 		 ; parity and overrun error
CODE ENDS

;the data segment

DATA    SEGMENT PUBLIC  'DATA'

KeyTable		DW		?			; This stores the offset of the key table
									; currently being used to find the meanings
									; of any key press

DATA    ENDS

		END