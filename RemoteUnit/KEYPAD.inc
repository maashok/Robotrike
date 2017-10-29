;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  KEYPAD.INC                                ;
;                                 Include File                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the symbolic constants for the KEYPAD.ASM program.
;
; Revision History:
;    11/02/16  Maitreyi Ashok              Debounce/auto-repeat constant definitions
;                                          and necessary masks
;    11/03/16  Maitreyi Ashok              Added commenting to values
;   


OLD_VAL_RESET 		EQU 0FFFFH	; reset value of keys so that no keys are pressed
ROW_MASK			EQU 0000000000001111B	
								; Extract one row from a word with all the rows
ROW_EMPTY           EQU 0FFF0H  ; Clear the bits of one row from word with all the rows
NoKeyPress          EQU 0FH     ; Value in low nibble in I/O space when no key 
                                ; pressed in rows
KEYS_PER_ROW_SHIFT  EQU 2		; value to shift number by to multiply by number
                                ; of keys per row (multiply by 4 = shifting left 2 bits)
KEYS_PER_ROW		EQU	4		; 4 keys in each row of keypad
NUM_ROWS			EQU 4		; number of rows in keypad
ROW_NUMBER_SHIFT    EQU 4       ; Used to make the key value by shifting row
                                ; number to be in upper nibble
KEYPAD_START		EQU	080H	; Starting address of keypad in I/O space

DEBOUNCE_TIME 		EQU	20		; wait 20 ms before considering key to be pressed
SLOW_RATE			EQU 2000	; Every 2 s, if key is still pressed use slow auto-repeat
FAST_RATE			EQU 1000	; Every 1 s, after TIME_UNTIL_FAST_REPEAT, use fast
                                ; auto-repeat
TIME_UNTIL_FAST_REPEAT EQU 5000	; Use fast auto-repeat after 5 s of pressing key
