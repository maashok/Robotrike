;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 DISPLAY.INC                                ;
;                                 Include File                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the symbolic constants for the DISPLAY.ASM program.
;
; Revision History:
;    10/26/2016     Maitreyi Ashok      Added constants for display and event handler
;    10/28/2016     Maitreyi Ashok		Added constants for adding 14 segment portions
;	 12/02/2016		Maitreyi Ashok		Added blinking constants
;	 12/04/2016		Maitreyi Ashok		Added scrolling constants


HIGH_SEG_BYTE     EQU         08H	; Address of the high byte for segment patterns
DISPLAY_SIZE      EQU         8     ; Number of digits in display
BUFFER_ELEM_SIZE  EQU         2     ; Size of each segment pattern in buffer
SCROLL_SIZE       EQU         100   ; Size of buffer to scroll through
BUFFER_SIZE       EQU         SCROLL_SIZE * BUFFER_ELEM_SIZE
									; Number of bytes buffer can store - number
									; of digits in display times size of each digit
BLANK_LED         EQU         00000000B
                                    ; Segment pattern for no LEDs to be displayed
CONVERTS_SIZE     EQU         7     ; Buffer to hold string for converts functions
									; Maximum size for DisplayNum which has maximum
									; five digits, plus a sign, and null terminations
LED_DISPLAY	      EQU         00H	; The starting position of the LED display
									; peripherals in I/O space
START_MUX		  EQU		  0		; start muxing from leftmost digit
SHOW_DIGITS_TIME  EQU         500   ; Display digit for 1 second as part of blinking
DONT_SHOW_DIGITS_TIME   EQU   500   ; Hide display for 1 second as part of blinking
BLINK_RESET_CNT   EQU         0     ; Value to start blink counter at time 
                                    ; (no time has passed since started blinking)
BLINK_SET         EQU         0FFH  ; Blink digits in LED display

SCROLL_START      EQU         0     ; Start scrolling from leftmost digit
START_SCROLL_COUNT	EQU		  0		; Reset value of count for autoscroll time
SCROLL_TIME		  EQU		500		; Counts to wait to autoscroll right by one digit