; Maitreyi Ashok
; Section 1 - Richard Lee
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 Keycodes.INC                               ;
;                         	   Keycode Definitions     	                     ;
;                                 Include File                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the definitions for the Keycodes used by the Event Queue.
;
;
; Revision History:
;    11/16/16		Maitreyi Ashok		Initial revision, added KEYPAD_EVENT from
;										Keypad.inc
;	 11/18/16		Maitreyi Ashok		Removed unused key code
;	 12/01/16		Maitreyi Ashok		Added NOP key code
;	 12/04/16	    Maitreyi Ashok		Changed values for codes to be indices
;										to event table rather than ASCII chars

NOP_EVENT			  EQU 	0		; key code used when queue is empty
KEYPAD_EVENT		  EQU 	1		; key code used for all key press events in queue
SERIAL_DATA_RECEIVED  EQU 	2		; key code used for all data received via serial
									; i/o in the queue
SERIAL_ERROR          EQU 	3		; key code used for all serial errors in queue
