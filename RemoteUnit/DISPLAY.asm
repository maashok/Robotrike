; Maitreyi Ashok
; Section 1 – Richard Lee

        NAME    DISPLAY

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   DISPLAY                                  ;
;                               DISPLAY Routines	                         ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
; This file contains the display routines to convert from either numbers or strings
; to various segments being lit up on digits of the LED display accordingly. Supports
; multiplexing the digits of the 14 segment display at a rate of 1 KHz.
;
; Table of Contents:
; Display - Takes NULL terminated ASCII string and writes segment patterns to buffer
; DisplayNum - displays a 16 bit signed value in decimal representation
; DisplayHex - displays a 16 bit unsigned value in hexadecimal representation
; DigitMux - multiplexes, blinks, and scrolls the display
; InitDigitMux - initializes the necessary counters and buffers for display and
;				multiplexing
; ScrollDisplayRight - scrolls display right by one digit if not at end of display
; ScrollDisplayLeft - scrolls display left by one digit if not at end of display
; StartAutoScrolling - Allow for timer based automatic scrolling
; StopAutoScrolling - Do not allow for timer based autoscrolling
; BlinkChange - Switches between blinking and not blinking
;
; Revision History:
; 10/24/2016	Maitreyi Ashok	Wrote basic outlines and functional specifications
;								for all functions.		
; 10/26/2016	Maitreyi Ashok	Wrote code for all functions
; 10/27/2016	Maitreyi Ashok	Fixed all addressing mode errors
; 10/28/2016	Maitreyi Ashok	Fixed 14 segment pattern accessing method  
; 10/29/2016	Maitreyi Ashok	Added documentation to code
; 11/05/2016	Maitreyi Ashok	Updated comments and did minor fixes
; 11/05/2016    Maitreyi Ashok  Added blinking functionality
; 12/03/2016	Maitreyi Ashok	Updated blinking functionality
; 12/04/2016	Maitreyi Ashok	Added scrolling functionality
; 12/05/2016	Maitreyi Ashok	Updated comments

$INCLUDE(DISPLAY.inc)
$INCLUDE(General.inc)

CGROUP  GROUP   CODE
DGROUP  GROUP   DATA


CODE	SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP, DS:DGROUP

        EXTRN   Dec2String:NEAR			; Converts functions to display number
        EXTRN   Hex2String:NEAR			; in decimal or hexadecimal
        EXTRN   ASCIISegTable:BYTE		; Table of segment patterns for ASCII chars
		
; Display		
; Description: 		This function takes a null terminated ASCII format string 
;					and displays it to a 14 segment LED display. Thus, this 
;					string takes each character of the string and finds the 14 
;					segment	pattern from a table. Then the bit patterns for each 
;					of the segments that should be displayed for the digit are 
;					stored in a buffer. Thus, there is no actual output to the 
;					LED display but the buffer is updated so that the DigitMux 
;					routine will output the segments from the buffer periodically. 
;					If the string is too long, this function will add to the buffer 
;					until the buffer is full, so the segment patterns in the buffer
;					might not be for all the characters that were intended to be 
;					in the string. If the string is too short, blank characters
;					are included on the right to fill the 8 digit display.
;					
; Operation:  		The function gets a null terminated string. It goes through
;					the string and looks at each of the ASCII characters until
;					it reaches a NULL character, which signifies the end of the
;					string, or the buffer is full, which means BUFFER_SIZE/2 digits
;					have been displayed. The string contains output for status 
;					updates, error codes, or other messages for the RoboTrike) 
;					For each of the characters, the function converts it to a word
;					size bit pattern for a 14 segment pattern using a table of 14 
;					segment display options. This pattern is kept as an	active high 
;					signal, and is then	stored in the buffer of digits. Then, the 
;					new size of the buffer is stored, and the function adds the 
;					next digit to the buffer.
;
; Arguments:   		str [ES:SI] - null terminated string to output to display
; Return Value:		None
;
; Local Variables:	DoneFlag[CF]- stores whether we have reached the Null character
;					char[DI/AX]- The current character from the string
;					segPattern[DX]- the segment pattern corresponding to the current char
;					bufferPtr[SI]- the pointer to the buffer of segment patterns
;					SegTable[BX] - pointer to the segment pattern table
;					digitCount[BP] - stores the position in the string we are at
; Shared Variables: buffer (R/W)- the buffer with the segment patterns of all digits in
;								in the string
; Global Variables: None
;
; Input:			None
; Output:			The digits in the string passed in is output to a LED display
;								(by DigitMux)
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	The function uses a buffer, which is an array. It also uses
;					a look up table of segment patterns.
;
; Registers Changed: BP, SI, DI, BX, AX, DX, flags
; Stack Depth:		1 words
;
; Limitations:		Not all characters can be displayed using a 14 digit display. 
;					For example, an exclamation mark can not be used with a 14 
;					digit display. 
;
; Author:			Maitreyi Ashok
; Last Modified:	10/24/16  Maitreyi Ashok	Wrote functional specification
;												and pseudocode for function
;					10/26/16  Maitreyi Ashok    Wrote basic code for filing buffer
;					10/27/16  Maitreyi Ashok 	Changed registers to use base/index
;												registers
;					10/28/16  Maitreyi Ashok	Corrected method of getting segment
;												patterns from buffer
;                   11/05/16  Maitreyi Ashok    Simplified null termination logic
;					12/04/16  Maitreyi Ashok	Changed to store actual buffer
;												length
;					
; Operational Notes
; Go through each character of the string until reach NULL or end buffer
; Look up segment pattern for the character
; Add the segment pattern to the buffer
; Increment buffer and move to next digit


Display	        PROC        NEAR
                PUBLIC      Display

SetUpDisplay:
        MOV     actualBufferLen, 0
        MOV     BP, SI			; Save the address of the string to display
		XOR		SI, SI			; Start buffer pointer from start and clear character
		XOR		DI, DI          ; to clear data from last execution of display
		MOV     BX, OFFSET(ASCIISegTable)	; Store location of segment table
		CLC						; Clear the carry flag when starting for checks
		PUSHF					; The carry flag is set when a null character
CheckDoneWithBuffer:			; is found.
        CMP		SI, BUFFER_SIZE	; If the buffer is full, then we are done
		JGE		BufferComplete  
        POPF                    ; Look at the earlier stored carry flag
        JC		ReachedNullTermination	; If null termination has been reached, we 
                                        ; just add empty LEDs
		;JL		FindCharacter	; Otherwise, convert the next character to segment
								; pattern
FindCharacter:
        XOR     AH, AH			; Store the ASCII character, clearing the high byte
        MOV     AL, ES:[BP]		; so that value stored is 1 byte and not word
        MOV     DI, AX
UpdateAndCheck:
		INC		BP				    ; Move to next character in string
		CMP		DI, ASCII_NULL	    ; Check if the character is ASCII NULL
		JNE		GetSegmentPattern	; If it is not, get the segment pattern
		;JE		ReachedNullTermination 
ReachedNullTermination:				; Once null termination has been reached
									; and string completely traversed:
        CMP     actualBufferLen, 0	; If the actual buffer length has been 
        JNE     AddBlankLEDs		; set, then move on
        ;JE     ChangeActualLength	; If not, set the actual length
ChangeActualLength:
        MOV     actualBufferLen, SI	; The current pointer into the buffer is the
        ;JMP    AddBlankLEDs		; actual length of the string to display
AddBlankLEDs:        
        MOV 	buffer[SI], BLANK_LED   ; If NULL character was reached, add blank
        MOV     buffer[SI+1], BLANK_LED ; LEDs to the buffer
        STC                             ; Set the carry flag to signify null was already
        PUSHF                           ; reached to find next digit accordingly
		JMP		MoveToNextCharacter     
		
GetSegmentPattern:
        SHL     DI, 1			        ; Get the character from the segment table (each
		MOV		DX, CS:[BX][DI]         ; entry in the table is 1 word)
        MOV     WORD PTR buffer[SI], DX ; Move the segment pattern to the buffer
		CLC                             ; Clear the carry flag - have not reached NULL
                                        ; character yet
        PUSHF
MoveToNextCharacter:
        ADD		SI, BUFFER_ELEM_SIZE    ; Increment the buffer pointer to the next element
		JMP 	CheckDoneWithBuffer     ; Loop to next character
		
BufferComplete:					        ; Once the buffer is full (with extra NULL chars
		POPF                            ; added), can return from the function
        						        ; Clear flags from stack from last character
        MOV     scrollPos, 0
        RET
Display		ENDP

; DisplayNum	
; Description: 		This function gets a 16 bit signed value that it displays
;					in the decimal ASCII representation with Null termination. The 
;					maximum number of digits in the ASCII string is 5 digits, plus 1 
;					digit for the negative sign if the high bit of the value is 1. 
;					Leading zeroes are included so that there at 5 numerical digits for
;					each 16 bit value. The ASCII sring is then displayed on the 14 
;					segment LED display, by writing the segment patterns for each
;					digit to a buffer. The segment patterns in this buffer will 
;					later be written to the display using multiplexing of the LEDs
;					for the digits. The number is left justfied on the display,
;					so that 5 digits are displayed on the left of the display (if
;					the number is positive), and the right 3 digits are blank on
;					the LED display. If the number is negative, the left 6 LEDs
;					have the negative sign and 5 digits, and the right 2 digits
;					are blank.
;
; Operation:  		The 16 bit signed value is converted to a 5 digit ASCII string
;					(with sign if needed), using the Dec2String routine. This 
;					function divides by successive powers of 10 to get each 
;					individual digit. This is then converted to an ASCII character
;					and stored in a string buffer. When the 16 bit value has been
;					completely converted, the string buffer is passed to the 
;					Display function which finds the segment patterns for each
;					of the characters of the null terminated string representation
;					of the decimal value. Each segment pattern is 2 words, since
;					it has to include the bits for 14 segments of the display.
;					The segment patterns are then added in a buffer for each digit
;					to be displayed by the MuxDigit function. These segment patterns
;					which will be displayed cause the decimal numbers to be
;					left justified, with no LEDs on in the right 2 or 3 digits.
;
; Arguments:   		num [AX] - 16 bit signed value to be output in decimal format
; Return Value:		None
;
; Local Variables:	None
; Shared Variables: stringAddr(R)- The address of the buffer to store the ASCII string
;					actualBufferLen (R/W) - actual length of string to display 
;							ignoring blank LEDs
; Global Variables: None
;
; Input:			None
; Output:			None
;
; Error Handling:	None
;
; Algorithms:		Uses algorithm in Dec2String to divide the number by 
;					successive powers of 10 to get each of the decimal digits
; Data Structures:	None
;
; Registers Changed: SI, ES, AX, BX, DX, DI, flags
; Stack Depth:		1 word
;
; Limitations:		This function can only display 16 bit signed values. Also, it
;					fills up 5 or 6 digits on the display, using leading zeroes
;					which take up space without contributing any meaning to the
;					number
;
; Author:			Maitreyi Ashok
; Last Modified:	10/24/2016	Maitreyi Ashok		Wrote functional specification
;													and outline
;					10/26/2016	Maitreyi Ashok		Wrote code for function
;					10/28/2016	Maitreyi Ashok		Fixed issue of buffer pointer
;													getting moved by convert function
;					
; Operational Notes
; Convert value to decimal string representation
; Add segment patterns of string to buffer

DisplayNum      PROC        NEAR
                PUBLIC      DisplayNum
GetASCIIStringOfNum:
        MOV     SI, OFFSET(stringAddr)	; Store the string returned by Dec2String
        CALL    Dec2String				; in a buffer of characters
        MOV     SI, OFFSET(stringAddr)	; Reload address of buffer of strings
										; (was changed by Dec2String)
DisplayDecString:
        PUSH    DS						; Move the address pointed to by data 
        POP     ES						; segment into extra segment so Display
        CALL    Display					; function can access string stored in DS
										; Display the decimal number
		RET
DisplayNum	ENDP

; DisplayHex		
; Description: 		This function gets a 16 bit unsigned value that it then converts
;					to the hexadecimal ASCII representation with Null termination. The 
;					maximum number of digits in the ASCII string is 4 digits. Leading
;					zeroes are included so that there at 4 numerical digits for
;					each 16 bit value. The ASCII sring is then displayed on the 14 
;					segment LED display, by writing the segment patterns for each
;					digit/letter to a buffer. The segment patterns in this buffer will 
;					later be written to the display using multiplexing of the LEDs
;					for the digits. The number is left justfied on the display,
;					so that 4 digits are displayed on the left of the display, and 
;					the right 4 digits are blank on	the LED display. 
;
; Operation:  		The 16 bit unsigned value is converted to a 4 digit hexadecimal 
;					representation ASCII string, using the Hex2String routine. This 
;					function divides by successive powers of 16 to get each 
;					individual digit. This is then converted to an ASCII character
;					and stored in a string buffer. When the 16 bit value has been
;					completely converted, the string buffer is passed to the 
;					Display function which finds the segment patterns for each
;					of the characters of the null terminated string representation
;					of the hexadecimal value. Each segment pattern is 2 words, since
;					it has to include the bits for 14 segments of the display.
;					The segment patterns are then added in a buffer for each digit/
;					letter to be displayed by the MuxDigit function. These segment 
;					patterns will display the left justified hex number, with no 
;					LEDs on in the right 4 digits.
;
; Arguments:   		num [AX] - 16 bit unsigned value to be output in hexadecimal format
; Return Value:		None
;
; Local Variables:	None
; Shared Variables: addr [SI](R)- The address of the buffer to store the ASCII string at
; Global Variables: None
;
; Input:			None
; Output:			None
;
; Error Handling:	None
;
; Algorithms:		Uses algorithm in Hex2String to divide the number by 
;					successive powers of 16 to get each of the decimal digits
; Data Structures:	None
;
; Registers Changed: SI, ES, AX, BX, DX, DI, flags
; Stack Depth:		1 word
;
; Limitations:		This function can only display 16 bit unsigned values. Also, 
;					it fills up 4 digits on the display, using leading zeroes
;					which take up space without contributing any meaning to the
;					number
;
; Author:			Maitreyi Ashok
; Last Modified:	10/24/2016	Maitreyi Ashok		Wrote functional specification
;													and outline
;					10/27/2016	Maitreyi Ashok		Wrote code for function
;					10/28/2016	Maitreyi Ashok		Fixed issue of buffer pointer
;													getting moved by convert function
;					
; Operational Notes
; Convert value to hexadecimal string representation
; Add segment patterns of string to buffer

DisplayHex      PROC        NEAR
                PUBLIC      DisplayHex
GetASCIIStringOfHex:					; Store the string returned by Hex2String
		MOV     SI, OFFSET(stringAddr)	; in a buffer of characters
        CALL    Hex2String				; Reload address of buffer of strings
        MOV     SI, OFFSET(stringAddr)	; (waschanged by Hex2String)
DisplayHexString:
        PUSH    DS						; Move the address pointed to by data
        POP     ES						; segment into extra segment so Display
        CALL    Display					; function can access string stored in DS
		RET								; Display the hexadecimal number

        DisplayHex	ENDP

; DigitMux		
; Description: 		This routine takes care of multiplexing the digits of the
;					LED display, and is called at a regular interval of once every
;					1 millisecond. At each interval, all the 14 segments of 
;					one of the 8 digits of the LED display are displayed. Then,
;					when the next timer interrupt occurs, the function starts
;					displaying the next digit to the right in the same manner.
;					When the rightmost digit is displayed, the multiplexing wraps
;					around and starts displaying the leftmost digit next. In addition,
;                   if the user decides that the display should blink, then that
;                   flag is set and all of the digits of the display blink at an 
;					interval of SHOW_DIGITS_TIME. Also, the scroll position is 
;					taken into account to display a specific frame of the buffer,
;					where the buffer can be a maximum size of SCROLL_SIZE, but
;					only DISPLAY_SIZE digits can be displayed at a time at some
;					offset from the start of the buffer. If autoscrolling is 
;					enabled, the scroll position is also updated at regular time
;					intervals based on the same timer as the digit muxing.
;
; Operation:  		This function mainly multiplexes the digits, showing all the
;					14 segments of each digit simultaneously, and alternating
;					between the 8 possible digits at a high frequency (~1 KHz) so 
;					that the difference in display is not obvious. To do this, a
;					count of the current digit being displayed is kept. Every time
;					a timer event occurs and this function is called, the current
;					digit is displayed and then this digit counter is incremented
;					to show the next digit at the next timer interrupt. If the end of
;					the display is reached, then the  The current digit is displaying 
;					by storing each byte of the segment pattern at the I/O memory 
;					addresses corresponding to the LED display peripheral. In addition, 
;					if blinking is enabled, the multiplexed LEDs are displayed for a 
;					portion of the total time interval, and for another portion of 
;					the time interval, the display is turned off. The digits displayed
;					are based on the current scroll position, with DISPLAY_SIZE
;					digits shwon at different frames of the entire buffer. Auto-
;					scrolling also happens at fixed times to move the display right.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	pattern [AL] - The segment patterns from the buffer for a digit
;                   currDigit [SI] - store the current digit we are muxing
;					addr [DX] - address we are ouptputting segment patterns to
; Shared Variables: buffer (R)- contains segment patterns of digits to display
;					currMuxDigit (R/W)- to determine which buffer digit to output
;								and then update to display the next digit after 1 ms
;                   blinkCount (R/W)- how much time has passed since started displayin
;                               digits in a blinking cycle
;					autoScroll (R) - whether display should autoscroll to the right
;					scrollCount (R/W) - current time waited to autoscroll display
;								again
;					scrollPos (R) - starting digit of current scroll frame of buffer
;					blink (R) - whether the digits should blink on display
; Global Variables: None
;
; Input:			Keys pressed to scroll/blink display						
; Output:			The next digit is output to the memory mapped LED display
;							
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	A buffer array is used to store segment patterns
;
; Registers Changed: SI, AL, DX, flags
; Stack Depth:		0 words
;
; Limitations:		The function does not allow for the user to adjust the frequency
;					of multiplexing the digits or of how fast to blink the digits.
;								
; Author:			Maitreyi Ashok
; Last Modified:	10/24/2016	Maitreyi Ashok		Wrote functional specification
;													and outline
;					10/27/2016	Maitreyi Ashok		Wrote code for basic muxing
;                   11/05/2016  Maitreyi Ashok      Added code for blinking
;					12/03/2016	Maitreyi Ashok		Updated blinking code
;					12/04/2016	Maitreyi Ashok		Added scrolling code
;					12/05/2016	Maitreyi Ashok		Added comments and optimized code

DigitMux	    PROC        NEAR
                PUBLIC      DigitMux
CheckAutoScroll:
		CMP     autoScroll, TRUE				; Check whether to autoscroll
		JNE     CheckWhetherToBlink				; If not, check whether to blink
		;JE		CheckIfTimeToAutoScroll
CheckIfTimeToAutoScroll:						; If have to autoscroll update the
		INC     scrollCount						; autoscroll count and check whether
		CMP		scrollCount, SCROLL_TIME		; it is time to autoscroll
		JNE		CheckWhetherToBlink				; If not, check whether to blink
		;JE		AutoScrollRight
AutoScrollRight:								; If it is time to autoscroll
		CALL	ScrollDisplayRight				; move display frame to right by one
		MOV		scrollCount, START_SCROLL_COUNT	; digit and restart counting
		;JMP	CheckWhetherToBlink				; for autoscroll time
CheckWhetherToBlink:
        CMP     blink, BLINK_SET                ; Find whether the LEDs should
        JE      MuxDigits                       ; blink. If so, go to mux digits
        ;JNE    NotBlinking						
NotBlinking:									 
        MOV     blinkCount, BLINK_RESET_CNT     ; If they should not blink, reset 
												; blink count to zero
MuxDigits:
        MOV     SI, currMuxDigit                ; Change curr mux digit to refer to 
        SHL     SI, 1							; words instead of bytes
        MOV     BX, scrollPos					; Get the scroll position to refer
        SHL     BX, 1							; to words instead of bytes as well
		
CheckPartOfBlinkingCycle:                       ; Check if in part of the display cycle
        CMP     blinkCount, SHOW_DIGITS_TIME    ; cycle where the digits are displayed
        JGE     DontShowDisplay                 ; If not, don't show anything on the display
        ;JL     ShowHighSeg         			; since it is blank part of blinking
ShowHighSeg:
        MOV     AL, buffer[SI+BX+1]          	; Get high byte of the segment pattern
		MOV     DX, LED_DISPLAY + HIGH_SEG_BYTE	; and output it to the address for
        OUT     DX, AL							; the high byte
FindLowSeg:
        MOV		AL, buffer[SI+BX]               ; Then, get the low byte of the segment
                                                ; pattern
        JMP     ShowLowSeg
DontShowDisplay:
        MOV     AL, BLANK_LED                   ; If digits should not be displayed
        MOV     DX, LED_DISPLAY + HIGH_SEG_BYTE ; output a BLANK_LED pattern first
        OUT     DX, AL                          ; to the upper segment byte
												; Then use this same BLANK_LED pattern
		;JMP	ShowLowSeg						; to output to low segment byte
ShowLowSeg:
		SHR		SI, 1							; Prepare segment offset from start
												; of LED display based on current
												; mux digit
		ADD		SI, LED_DISPLAY					; Write low byte of digit so both high
		MOV		DX, SI							; and low bytes of the digit will
		OUT		DX, AL							; be shown 
		;JMP	SetUpForNextMux
SetUpForNextMux:
		INC		currMuxDigit                    ; The next mux will work with the next
		CMP		currMuxDigit, DISPLAY_SIZE      ; digit to the right in the display
		JL		UpdateBlinking						    
		;JGE	WrapAroundMuxDigit			
WrapAroundMuxDigit:								
		MOV		currMuxDigit, START_MUX			; If we have muxed the right most digit
		;JMP	UpdateBlinking					; then wrap around and display the
                                                ; leftmost digit next time
UpdateBlinking:
        INC     blinkCount                      ; Increment the blink count
        CMP     blinkCount, SHOW_DIGITS_TIME + DONT_SHOW_DIGITS_TIME
        JL      DoneMuxing		                ; If have passed total time of a blink
        ;JGE    WrapAroundBlinkingCounter       ; cycle reset the blink count to restart
WrapAroundBlinkingCounter:                      ; a blink cycle
        MOV     blinkCount, BLINK_RESET_CNT
        ;JMP    DoneMuxing

DoneMuxing:	
		RET
DigitMux	ENDP


; InitDigitMux		
; Description: 		This function sets up the multiplexing operations of the
;					LED display, namely what digit to show at each timer interval. 
;					Also, the buffer that holds segment patterns to display is 
;					cleared, so that no digits are displayed initially. Also,
;					the blinking and scrolling variables are reset to not blink
;					and scroll initially.
;
; Operation:  		The multiplexing functionalities are set up by resetting all
;					the values of the counters involved with multiplexing. Majorly,
;					this is the counter that stores which of the 8 digits we are 
;					currently displaying in the LED display for the 1 ms time
;					interval. This counter is set to 0, so that we start out
;					displaying the leftmost digit. Also, the function
;					goes through every word of the buffer and sets it to the segment
;					pattern for the NULL character. This is so no old values
;					are displayed in the LEDs, and the first value to be shown
;					is one input to the Display function. Also, the blinking and
;					scrolling variables are reset to start from the beginning without
;					old values of scrolling or blinking.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	bufferCount[DI] - position in buffer when setting all segment
;								patterns to zero initially
; Shared Variables: currMuxDigit(W)- to determine which buffer digit to output
;								and then update to display the next digit after 1 ms
;					buffer(W)- contains segment patterns of digits to display. 
;					blinkCount (W)- how much time has passed since started displayin
;                               digits in a blinking cycle
;					autoScroll (W) - whether display should autoscroll to the right
;					scrollCount (W) - current time waited to autoscroll display
;								again
;					scrollPos (W) - starting digit of current scroll frame of buffer
;					blink (W) - whether the digits should blink on display
; Global Variables: None
;
; Input:			None
; Output:			None
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	Clears the array buffer for future use. It also uses
;					a look up table of segment patterns.
;
; Registers Changed: SI, ES, flags
; Stack Depth:		1 word
;
; Limitations:		This way of initializing does not allow for any LED pattern to
;					be displayed when setting up the display system.
;
; Author:			Maitreyi Ashok
; Last Modified:	10/24/2016	Maitreyi Ashok		Wrote functional specification
;													and outline
;					10/26/2016	Maitreyi Ashok		Wrote basic code for function
;					10/28/2016	Maitreyi Ashok		Added chip select definitions
;                   11/05/2016  Maitreyi Ashok      Updated to allow for blinking
;					12/04/2016	Maitreyi Ashok		Updated for scrolling and blinking

InitDigitMux    PROC        NEAR
                PUBLIC      InitDigitMux
				
StartWithFirstLED:
		MOV     currMuxDigit, START_MUX	; The LEDs will be displayed starting from
										; the leftmost
        MOV     blinkCount, BLINK_RESET_CNT	; Start blink count from the start
        MOV     scrollPos, SCROLL_START	; Start scrolling frame from left most position
		MOV		autoScroll, TRUE		; Start out autoscrolling the display
        MOV     scrollCount, START_SCROLL_COUNT		; Start autoscrolling count from
													; start
		MOV     blink, NOT(BLINK_SET)	; Start out not blinking

SetBufferZero:
		MOV		stringAddr, ASCII_NULL	; Store an empty string as argument
        MOV     SI, OFFSET(stringAddr)
		PUSH 	DS						; Move value stored in data segment
		POP		ES						; to extra segment so Display function
		CALL	Display					; can access it, so that can display
DoneInitializing:						; blank LEDs to start off
		RET
InitDigitMux	ENDP

; ScrollDisplayRight		
; Description: 		This function scrolls the display to the right by one digit
;					essentially, moving a frame of DISPLAY_SIZE digits shown
;					to the right, so one less digit from the left is displayed
;					and one more digit from the right is displayed. If no more
;					digits exist on the right to be shown, then the display
;					is not scrolled.
;
; Operation:  		This function allows to scroll the display right by one digit
;					if there are still digits on the right to be shown. If not,
;					then no scrolling occurs and the scroll position remains
;					unchanged. If there are digits on the right, one extra digit
;					on the right is shown while one less digit is shown on 
;					the left.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	buffLength [AX] - length of string displayed in buffer
; Shared Variables: actualBufferLen (R) - actual number of characters to display
;							in display excluding blank spaces
;					scrollPos (R/W) - index of leftmost digit in current scroll frame
; Global Variables: None
;
; Input:			Either the count was reached to autoscroll or the scroll right
;					key was pressed
; Output:			Display is scrolled one digit right if possible
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	None
;
; Registers Changed: AX
; Stack Depth:		0 words
;
; Limitations:		This function does not allow for scrolling right by variable
;					amounts of digits
;
; Author:			Maitreyi Ashok
; Last Modified:	12/04/16	Maitreyi Ashok	Wrote code
;					12/05/16	Maitreyi Ashok	Updated comments

ScrollDisplayRight	PROC		NEAR
					PUBLIC		ScrollDisplayRight
				
CheckIfAtEndOfDisplay:
        MOV     AX, actualBufferLen		; Get number of segment patterns in display 
        SHR     AX, BYTES_PER_WORD_SHIFT					
										; string and convert to a number of digits
        SUB     AX, DISPLAY_SIZE		; Subtract the display size from this number
										; to get index of starting digit in last
										; display frame
		CMP		scrollPos, AX			; Check if the current scroll position is
										; larger than this value
		JGE		EndScrollRight			; If it is, then can't scroll right
		;JLE	ScrollRight				; If not, can scroll right
ScrollRight:
		INC     scrollPos				; Scroll display right by one character
EndScrollRight:							; Done scrolling right
		RET
ScrollDisplayRight	ENDP

; ScrollDisplayRight		
; Description: 		This function scrolls the display to the left by one digit
;					essentially, moving a frame of DISPLAY_SIZE digits shown
;					to the left, so one less digit from the right is displayed
;					and one more digit from the left is displayed. If no more
;					digits exist on the left to be shown, then the display
;					is not scrolled.
;
; Operation:  		This function allows to scroll the display left by one digit
;					if there are still digits on the left to be shown. If not,
;					then no scrolling occurs and the scroll position remains
;					unchanged. If there are digits on the left, one extra digit
;					on the left is shown while one less digit is shown on 
;					the right.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	None
; Shared Variables: scrollPos (R/W) - index of leftmost digit in current scroll frame
; Global Variables: None
;
; Input:			Either the count was reached to autoscroll or the scroll left
;					key was pressed
; Output:			Display is scrolled one digit left if possible
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	None
;
; Registers Changed: None
; Stack Depth:		0 words
;
; Limitations:		This function does not allow for scrolling left by variable
;					amounts of digits
;
; Author:			Maitreyi Ashok
; Last Modified:	12/04/16	Maitreyi Ashok	Wrote code
;					12/05/16	Maitreyi Ashok	Updated comments

ScrollDisplayLeft	PROC		NEAR
					PUBLIC		ScrollDisplayLeft
CheckIfAtStartOfDisplay:
		CMP		scrollPos, SCROLL_START	; Check if the current scroll position
										; is the leftmost possible
		JLE		EndScrollLeft			; If it is, can't scroll left
		;JG		ScrollLeft
ScrollLeft:								; Otherwise, scroll left by one digit
		DEC		scrollPos
EndScrollLeft:							; Done scrolling left
		RET
ScrollDisplayLeft	ENDP

; StartAutoScrolling		
; Description: 		This function sets the autoscroll flag based on keypad input
;					to allow for autoscrolling once every SCROLL_TIME counts of
;					the display timer.
;
; Operation:  		This function sets the autoscroll flag so that the muxing of
;					digits will scroll the display right once every SCROLL_TIME
;					counts of the timer that the muxing uses.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	None
; Shared Variables: autoScroll (W) - Whether the display should be scrolled right
;							at a fixed interval
; Global Variables: None
;
; Input:			Key pressed on keypad to start autoscrolling
; Output:			Display will autoscroll to right at fixed interval
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	None
;
; Registers Changed: None
; Stack Depth:		0 words
;
; Limitations:		This function does not allow the user to select a rate or
;					size of autoscrolling.
;
; Author:			Maitreyi Ashok
; Last Modified:	12/04/16	Maitreyi Ashok	Wrote code
;					12/05/16	Maitreyi Ashok	Updated comments

StartAutoScrolling  PROC        NEAR
                    PUBLIC      StartAutoScrolling
SetAutoScroll:
        MOV     autoScroll, TRUE		; Set autoscroll flag
        RET
StartAutoScrolling  ENDP

; StartAutoScrolling		
; Description: 		This function clears the autoscroll flag based on keypad input
;					to disallow autoscrolling once every SCROLL_TIME counts of
;					the display timer.
;
; Operation:  		This function clears the autoscroll flag so that the muxing of
;					digits will not scroll the display right once every SCROLL_TIME
;					counts of the timer that the muxing uses.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	None
; Shared Variables: autoScroll (W) - Whether the display should be scrolled right
;							at a fixed interval
; Global Variables: None
;
; Input:			Key pressed on keypad to stop autoscrolling
; Output:			Display will stay fixed at current scrollPos scroll frame
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	None
;
; Registers Changed: None
; Stack Depth:		0 words
;
; Limitations:		This function does not allow the user to select where the 
;					display frame should stay when stopping autoscrolling.
;
; Author:			Maitreyi Ashok
; Last Modified:	12/04/16	Maitreyi Ashok	Wrote code
;					12/05/16	Maitreyi Ashok	Updated comments

StopAutoScrolling   PROC        NEAR
                    PUBLIC      StopAutoScrolling
ResetAutoScroll:
        MOV     autoScroll, FALSE		; Clear autoscroll flag
        RET
StopAutoScrolling   ENDP

; BlinkChange		
; Description: 		This function changes the value of the blink flag, so that
;					if the display is currently blinking, it will start blinking.
;					If the display is currently not blinking, it will stop
;					blinking. This is in response to keypad input on the blink
;					key.
;
; Operation:  		This function negates the current value of the blink flag to
;					stop blinking if the display currently blinks and to start
;					blinking if the display is currently still.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	None
; Shared Variables: blink (W) - whether the digits should blink on display
; Global Variables: None
;
; Input:			Key pressed on display to start/stop blinking
; Output:			Digits on display will start or stop blinking
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	None
;
; Registers Changed: None
; Stack Depth:		0 words
;
; Limitations:		The rate of blinking is fixed and cannot be set by user
;
; Author:			Maitreyi Ashok
; Last Modified:	12/04/16	Maitreyi Ashok	Wrote code
;					12/05/16	Maitreyi Ashok	Updated comments
	
BlinkChange	    PROC    NEAR
				PUBLIC  BlinkChange
OppositeBlink:
        NOT     blink		; Negate blink flag. If currently set, will be cleared
        RET					; If currently cleared, will be set
							; Value of BLINK_SET means display will blink. Value
							; of NOT(BLINK_SET) means display will not blink
BlinkChange     ENDP

CODE    ENDS




;the data segment

DATA    SEGMENT PUBLIC  'DATA'

currMuxDigit   DW                   ?      	; which digit we are displaying
											; so we can mux them in series
buffer         DB    BUFFER_SIZE    DUP (?) ; array of BUFFER_SIZE bytes to store segment 
											; patterns of digits to display
actualBufferLen DW                  ?		; Actual number of bytes of segment patterns
											; in buffer which aren't fillers at end
stringAddr     DB    CONVERTS_SIZE  DUP (?)	; buffer to store string for converts
                                            ; functions
blink		   DB					?		; Whether display should blink
blinkCount     DW                   ?       ; Counter of time passed in blink cycle
scrollPos      DW                   ?       ; Offset from leftmost digit have scrolled to
scrollCount    DW                   ?		; Counter of time passed to scroll digits
autoScroll     DB                   ?		; Whether display should be autoscrolled
DATA    ENDS


        END	