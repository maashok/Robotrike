;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  General.INC                               ;
;                                 Include File                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the symbolic constants for any general program.
;
; Revision History:
;    10/28/16  Maitreyi Ashok              Added TRUE/FALSE symbolic constants
;										   and ASCII NULL      
;	 11/04/16  Maitreyi Ashok			   Added data size constants       
;	 11/11/16  Maitreyi Ashok			   Added bit size constants and shift
;										   to convert size
;    11/24/16  Maitreyi Ashok              Added maximum and minimum values
;    12/04/16  Maitreyi Ashok              Updated comments

; logical values
TRUE				EQU		0FFH		; Store values for logical true and false
FALSE				EQU 	00H

; ASCII Values
ASCII_NULL			EQU		00H			; Byte for ASCII character NULL
CARRIAGE_RETURN		EQU		0DH			; Byte for ASCII Carriage Return

; Data Sizes
BYTES_PER_WORD		EQU		2			; Each word has 2 bytes
BYTES_PER_BYTE		EQU		1			; Each byte has 1 byte
BITS_PER_BYTE       EQU     8           ; Each byte has 8 bits
BITS_PER_WORD       EQU     BITS_PER_BYTE * BYTES_PER_WORD
                                        ; Each byte has 16 bits
BYTES_PER_WORD_SHIFT EQU	1			; Shift left by 1 to multiply index by
										; BYTES_PER_WORD and get an index into
										; word sized array or table

; Max and min values
MAX_16_BIT_POSITIVE     EQU     32767       ; Maximum possible positive word value
MIN_POSITIVE            EQU     0           ; Minimum possible positive value