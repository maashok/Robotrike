; Maitreyi Ashok
; Section 1 – Richard Lee

        NAME   KEYPAD

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                    KEYPAD                                  ;
;                               Keypad Routines	                             ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the keypad scanning and debouncing functions for the keypad
; on the target board. The keypad is scanned regularly using an event handler, 
; after which the key is debounced and event sent to a queue.
;
; Table of Contents:
; KeypadScan - Scans the keypad every millisecond to check whether any keys
; are pressed down and debounce those keys
; InitKeypad - Initialize shared variables for keypad
;
; Revision History:
;     10/31/16	Maitreyi Ashok		Wrote pseudocode/functional specifications
;     11/02/16  Maitreyi Ashok      Wrote code for both functions
;     11/03/16  Maitreyi Ashok      Updated comments
;	  11/09/16	Maitreyi Ashok		Updated comments and optimized code

$INCLUDE(KEYPAD.inc)
$INCLUDE(KEYCODES.inc)

CGROUP  GROUP   CODE
DGROUP  GROUP   DATA

CODE	SEGMENT PUBLIC 'CODE'


        ASSUME  CS:CGROUP, DS:DGROUP
		EXTRN	EnqueueEvent:NEAR
		
; InitKeypad		
; Description: 		Initializes the keypad variables to allow for scanning of the
;					keypad for keypresses and debouncing of the keys on a regular
;					time interval. This function is called by the main loop to 
;					set all the static variables to initial values. More specifically, 
;					the	old value when last scanning the keys is set to the reset value
;					signifying that none of the 16 active low keys have
;					been pressed. In addition, we set our static variable to start
;					from the first row when scanning for key presses, and start
;					the debounce counter at its maximum value, so that we can count
;					down to zero to see if enough time has passed for it to be 
;					decided that the key intentionally pressed. Also, the auto
;					repeat variables are reset, so that the keys start auto-repeating
;					at a slow rate, and that there is the maximum amount of time
;					left before the fast auto-repeat rate is used.
;
; Operation:  		This function is called to set up the keypad scanning routines
;					in the main loop. This function sets the oldValue static variable
;					to oldValueReset. This value means that none of the keys have
;					been pressed yet, which is true when we first initialize
;					the keypad routines. This way, if the ith key is pressed, bit
;					i of oldValue can be cleared, since we are dealing with active
;					low signals. Also, the row number is set to zero so that
;					we start scanning for keypad presses from the 1st row (counted
;					0-3). In addition, the debounce counter is started from the 
;					debounce time, which is the amount of time we wait with a key 
;					pressed	before acting on the key press. Also, the repeat rate
;					of the key pad is set to the slow rate, so that auto repeat
;					starts at a slower rate once the key has been debounced. Thus,
;					the counter until fast rate is also set to its maximum value
;					since no keys have been pressed yet, and once it is, there
;					will be that amount of time left before the fast auto-repeat
;					rate is used for the keys instead of the slower one.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	None
; Shared Variables: oldValue (W)- value of keypad keys at last keypad scan
;					rowNumber (W)- number of row to scan next (0-3)
;					debounce_cntr (W)- amount of time has waited for key to debounce
;					repeat_rate (W)- clock cycles before key is considered to be 
;							pressed again when press has never been removed
;					repeat_cntr (W)  amount of time have waited for fast auto-repeat 
;							rate to be used
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
; Registers Changed: None
; Stack Depth:		
;
; Limitations:		The initialization only allows for a fixed amount of time
;                   until the faster repeat rate is reached.
;
; Author:			Maitreyi Ashok
; Last Modified:	10/31/16	Maitreyi Ashok	Wrote pseudocode/functional spec
;                   11/02/16    Maitreyi Ashok  Wrote code for init function
;                   11/03/16    Maitreyi Ashok  Commented code
;					
InitKeypad      PROC        NEAR
                PUBLIC      InitKeypad

ResetInitValues:
		MOV		oldValue, OLD_VAL_RESET         ; Value from last keypad scan
                                                ; variable reset
		MOV		rowNumber, 0                    ; Start scanning from first row
		MOV		debounceCntr, DEBOUNCE_TIME     ; Start debounce counter at max
        MOV		repeatRate, SLOW_RATE           ; time and repeat rate at slowest rate
		MOV		repeatCntr, TIME_UNTIL_FAST_REPEAT
                                                ; so that have to wait maximum amount
                                                ; of time before reaching fast rate
		RET
InitKeypad	ENDP
		
; KeypadScan		
; Description: 		This function scans the keypad for any key presses and debounces
;					them when the function is called at a frequency of 1 KHz. 
;                   Each call to the function multiplexes between the rows, 
;					only scanning one row during each function call. The value at 
;					the address for the row in the keypad is found and compared to 
;					the old value for that row from the last scan. If the value is 
;					different but there is a key press, the	debounce counter is reset 
;					since we have to see whether this key is pressed for the next 
;					DEBOUNCE_TIME ms. If the key has already been pressed, the 
;					counter is just decremented. If the counter	reaches zero, meaning 
;					it has been pressed for DEBOUNCE_TIME ms, then the key press 
;					is enqueued into the event queue. Also, variable auto-repeat 
;					functionality is included so that there are two repeat rates 
;					- slow and fast. Once the key is pressed long enough to be 
;					considered intentional (debounced), the key auto-repeats at 
;					a slow rate, so that after a relatively	large number of clock 
;					cycles, the key is considered to be pressed again even if there 
;					was no discontinuity in the pressing of the key. However, after 
;					the key is pressed for a much longer amount of time than the 
;					necessary time to be considered pressed, then the repeat rate 
;					increases so that less time needs to be pass for the key to be 
;					considered pressed again. 
;
; Operation:  		This function is called by the event handler at a 1 KHz frequency
;					due to a regular timer interrupt. Starting from the first row,
;					each call to the function scans one row of the keypad. The value
;					at the address is found for this row and compared to the value 
;                   for the row from the last scan, where each row is stored as 
;                   a 4 bit value. If the value is different, we know a key has 
;                   either been newly pressed or released since the last scan. 
;                   If the key was not released to get an empty row, we restart the
;					debounce counter to signify that we need to wait for the key
;					to debounce again. In addition, the repeat rate the keyapd
;					is currently at is reset. If the value is the same as before,
;					we wait for it to be held for DEBOUNCE_TIME amount of time, 
;					after which the debounce counter will reach	zero and the key 
;					press event will be enqueued. In addition, the debounce counter 
;					will be set to the repeat rate (slow or fast), for it to autorepeat 
;					accordingly	based on how much time has passed since the key 
;					was pressed. A counter is also stored to determine when the 
;					faster repeat rate will be used, depending again on how much 
;					time has passed	since the key was pressed. Key presses in the
;					same row will be noted in the same scan, but key presses in 
;					multiple rows will not be handled.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	rowVal [AL] - value of this row read from address of row
;					oldRowNibble [DL] - value of the row being scanned at the last scan
;                   rowValShift [CL] - amount to shift row values by to place in
;                           or retrieve from oldValue 
; Shared Variables: oldValue (R/W)- value of keypad keys at last keypad scan
;					rowNumber (R/W)- number of row to scan next (0-3)
;					debounce_cntr (R/W)- amount of time have waited for key to debounce
;					repeat_rate (R/W)- clock cycles before key is considered to be pressed
;							again when press has never been removed
;					repeat_cntr (R/W)- amount of time have waited for fast auto-repeat rate
;							to be used
; Global Variables: None
;
; Input:			Key presses on the keypad are scanned and enqueued into an
;					event queue to be handled individually
; Output:			Different outputs are done based on the key pressed. For 
;					example, something may be displayed on the LED display, the
;					laser could be fired, or the RoboTrike could move.
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	None
;
; Registers Changed: AX, BX, CX, DX, DI, flags
; Stack Depth:		0 words
;
; Limitations:		The keypad scanning does not allow for 2 key presses in different
;                   rows to be debounced concurrently. Whichever is found first when
;                   scanning will be debounced first and if this key is released before
;                   the other one, the other key will then be debounced.
;                   The function also assumes that the number of rows is a power of 2
;
; Author:			Maitreyi Ashok
; Last Modified:	10/31/16	Maitreyi Ashok	Wrote pseudocode/functional spec
;                   11/02/16    Maitreyi Ashok  Wrote code for keypad scanning
;                   11/03/16    Maitreyi Ashok  Commented code
;					11/04/16	Maitreyi Ashok	Updated comments
;                   11/09/16    Maitreyi Ashok  Updated comments and optimized code
 
KeypadScan      PROC        NEAR
                PUBLIC      KeypadScan
SetUpScanning:
		MOV		DL, rowNumber           ; I/O address to retrieve keypad row value from
		XOR		DH, DH					; port based on current row number
		ADD		DX, KEYPAD_START        
		IN		AL, DX
										; Empty all bits except those which correspond
		AND		AL, ROW_MASK			; to rows in the keypad
		;JMP	RetrieveOldValue
RetrieveOldValue:
		MOV		CL, rowNumber           ; The row in the old value is in the row
		SHL		CL, KEYS_PER_ROW_SHIFT	; number-th group of bits (each group has as
                                        ; many bits as number of columns). Use bit  
		MOV		DX, oldValue            ; operations to multiply by KEYS_PER_ROW
		SHR		DX, CL                  ; Shift the oldValue by this many bits and clear
		AND		DL, ROW_MASK			; out all bits except the row nibble to get the 
CheckIfKeyPressed:                      ; value
		CMP		AL, NoKeyPress			; If no key is pressed, reset debounce counter
		JE		UpdateRowToScan 		; and move on to next row
        ;JNE    CompareToOldValue
CompareToOldValue:
		CMP		DL, AL					; If the same key as before is pressed, debounce it
		JE		DebounceKeyPress		
		;JNE	DebounceValuesForNewKey	; If a key is pressed, but not the same as before
                                        ; reset and then debounce the key
		
DebounceValuesForNewKey:                      ; Reset debounce counter and repeat 
		MOV		debounceCntr, DEBOUNCE_TIME   ; rate/counter to initial values
		MOV		repeatRate, SLOW_RATE
		MOV		repeatCntr, TIME_UNTIL_FAST_REPEAT		
		
DebounceKeyPress:
		DEC		debounceCntr            ; Debounce key by decrementing counter
		JNZ		AutoRepeatUpdate		; If counter is not zero, have not waited for
		;JZ		DebounceTimeEnded       ; required amount of time, so directly go to 
                                        ; update autorepeat
DebounceTimeEnded:                      ; If the counter did reach zero, enqueue the event
		MOV		BL, rowNumber			; Get the key value by placing row number   
		SHL		BL, ROW_NUMBER_SHIFT	; in high nibble and the value read from 
		OR		AL, BL					; the I/O address (which columns pressed)
		MOV		AH, KEYPAD_EVENT        ; in low nibble. Key code is KEYPAD_EVENT
UpdateAndEnqueue:
		CALL	EnqueueEvent			; Enqueue the event
		MOV		DI, repeatRate          ; Make the debounce counter the auto repeat
		MOV		debounceCntr, DI		; rate to wait that amount of time before counting
		;JMP	AutoRepeatUpdate        ; it as a key press again
		
AutoRepeatUpdate:
		DEC		repeatCntr				    ; Decrement counter for fast autorepeat
		JNZ		UpdateOldValueForNextScan	; If it hasn't finished wait time, just 
		;JZ		ChangeRepeatRate            ; update value
ChangeRepeatRate:
		MOV		repeatRate, FAST_RATE		; But if we have finished full wait time, 
		JMP		UpdateOldValueForNextScan	; change repeat rate to a faster one
		
UpdateRowToScan:
		INC		rowNumber					; Move to next row only if no key press 
                                            ; in this row
		AND		rowNumber, NUM_ROWS - 1     ; Wrap row number to stay between 0 
                                            ; and NUM_ROWS - 1
		;JMP	DoneWithKeyPadScan
UpdateOldValueForNextScan:
		MOV		BX, ROW_EMPTY			; Shift empty spots in a word to match up
		ROL		BX, CL					; with the position for this row's info
		AND		BX, oldValue			; Clear that nibble in old value for the row
		AND		AX, ROW_MASK			; Remove the added row number in the key value
		SHL		AX, CL					; and move the nibble to the correct row position
                                        ; in a word sized value
		OR		AX, BX					; Put this row nibble into old value's emptied spot
		MOV		oldValue, AX            ; Store this value as the next old value
		;JMP	UpdateRowToScan         ; for the next keypad scan

DoneWithKeyPadScan:
		RET
KeypadScan	ENDP

CODE    ENDS

;the data segment

DATA    SEGMENT PUBLIC  'DATA'

oldValue 		DW		?		; Value of keypad during last scan
								; Stores KEYS_PER_ROW bits for each of the NUM_ROWS 
								; rows with a bit cleared in a row group if that   
								; corresponding key was pressed when last scanning 
								; that row. This value is updated with the new keys 
								; pressed/not pressed at each scan of the keypad.
rowNumber		DB		?		; The row currently being scanned
debounceCntr	DW		?		; Counter of how much time key has been pressed
repeatRate		DW		? 		; The repeat rate key is currently at
repeatCntr		DW		? 		; Amount of time since key has been debounced
								; to determine auto repeat rate (fast vs. slow)

DATA    ENDS


        END