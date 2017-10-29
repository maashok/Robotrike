;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  RemoFns.INC                               ;
;                            Remote Unit Definitions                         ;
;                                 Include File                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the general definitions for remote unit functions.
;
; Revision History:
;    12/02/16	Maitreyi Ashok	Initial revision
;	 12/05/16	Maitreyi Ashok	Updated comments

; Definition of STUC for keypad keys. Each key has a different function to
; perform with a specific argument to it whenver it is pressed, depending on
; which menu of the keypad the user is in. The key can also cause a switch
; to a new keypad menu.
KeyStruc		STRUC
	Function			DW		?	; Function to perform when pressed
	FunctionArg			DW		?	; Argument to pass to function
	CmdTable			DW		?	; Command table to switch to (switch between
									; menus depending on key pressed)
KeyStruc		ENDS

; String definitions for UI
KEY_STRING_SIZE     EQU     14		; Fixed length byte size of string used as 
									; argument to some KeyStruc linked functions
KEY_ROW_NUM_MASK	EQU		0000000011110000B
									; Mask to get row number of key pressed in keypad
PRESSED_CHECK		EQU		0000000000000001B
									; Mask to check whether a key in the row is pressed
ERR_STRING_SIZE     EQU     11		; Fixed length byte size of serial error 
									; messages to display

REMOTE_ERROR		EQU		'R'		; Remote unit serial error signified to 
									; DisplayErrors function by passing remote 
									; argument to display accordingly
MOTOR_ERROR		EQU		'M'			; Motor unit serial error signified to 
									; DisplayErrors function by passing motor 
									; argument to display accordingly
NO_LSR_ERROR	EQU		0			; LSR value of 0 signifies that there is no
									; serial error.
									
MIN_ERROR			EQU		0		; Minimum value of LSR for errors
MAX_ERROR			EQU		30		; Maximum value of LSR for errors
ERROR_CHECK_MASK	EQU		00000001	; Error mask to check whether the error
										; is even (no DataReady bit set)