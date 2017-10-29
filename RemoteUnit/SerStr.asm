; Maitreyi Ashok
; Section 1 – Richard Lee

        NAME    SerStr

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   SerStr                                   ;
;                          	 Serial String Routines	                         ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; This file contains a routine to output a string to the serial port.
;
; Table of Contents:
; SerialPutString - transmits the passed string character by character via the
; serial channel
;
; Revision History:
;     11/29/2016	Maitreyi Ashok	Functional Specification/Pseudocode
;	  12/02/2016	Maitreyi Ashok	Wrote code
; 	  12/05/2016	Maitreyi Ashok	Updated comments
;	  12/07/2016	Maitreyi Ashok	Changed to use ES:SI as argument

$INCLUDE(General.inc)


CGROUP  GROUP   CODE

CODE	SEGMENT PUBLIC 'CODE'


        ASSUME  CS:CGROUP

		EXTRN	SerialPutChar:NEAR


; SerialPutString		
; Description: 		This function outputs a string to the serial channel by transmitting
;					one character of the string at a time through the channel. This
;					is done by SerialPutChar, which transmits one byte/character
;					of data that it is passed in via serial transmission if it 
;					can be transmitted. If the transmission does not work on the
;					first try for any character, the transmission attempt for that
;					character is repeated until it is successfully transmitted.
;					
; Operation:  		This function outputs a string on the serial channel by 
;					outputting each character on the channel in order. Thus, it
;					is attempted to output each of these characters and if for 
;					some reason it is not transmitted, the attempts are repeated
;					until the transmission is successful. This process is repeated
;					for each of the characters until a NULL is reached, after which
;					carriage return is transmit on the channel to delimit the
;					string.
;
; Arguments:   		stringAddr [ES:AX]- address of string that should be output on the
;							serial channel
; Return Value:		error [AX] - returns that no error occurred always
;
; Local Variables:	char [AL] - character currently outputting to serial channel
; Shared Variables: None
; Global Variables: None
;
; Input:			Some event has occurred for which status/error updates need to
;					be displayed/handled
; Output:			A string is output character by character on the serial channel
;
; Error Handling:	None
;
; Algorithms:		A brute force method is used, outputting characters until they
;					are succesfully sent on the serial channel.
; Data Structures:	A string of characters to output on serial channel
;
; Registers Changed: SI, AL, flags
; Stack Depth:		2 words
;
; Limitations:		This function can become blocking if the serial channel is blocked
;					for some reason and a character cannot be transmitted. This
;					will be attempted again and again with no other activity happening.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/29/16	Maitreyi Ashok	Functional specification/Pseudocode
;					12/02/16	Maitreyi Ashok	Wrote code
;					12/05/16	Maitreyi Ashok	Updated comments
;			        12/07/16    Maitreyi Ashok  Added return value
;					12/07/16	Maitreyi Ashok	Changed to use address at ES:SI

SerialPutString     PROC        NEAR
					PUBLIC      SerialPutString
SerialPutStringSetUp:
        MOV     SI, AX					; Get the address of the string to output
										; to serial channel
CheckCharacter:
        MOV		AL, BYTE PTR ES:[SI]	; Get next character from string to output
        CMP     AL, ASCII_NULL			; and check if it is null
        JE      AddCarriageReturn		; If it is null, the string has ended and
										; the end of command character can be output
        ;JNE    AddCharacter			; Otherwise, the next character needs to
										; be output
AddCharacter:
        PUSH    SI						; Save address of string since SerialPutChar
										; changes it
		CALL	SerialPutChar			; Transmit current character of string to
										; serial channel
        POP     SI						; Restore address of string to be used again
		JC		CheckCharacter			; Check carry flag - If set, character was
										; not output so try again
		;JC		CharOutput				; Otherwise, character was output and can
										; move to next one
CharOutput:
        INC		SI					 	; If character was output increment pointer
		JMP		CheckCharacter			; to next character of string and output it
AddCarriageReturn:
		MOV		AL, CARRIAGE_RETURN		; At end of string add carriage return to
										; signify command has ended
		CALL	SerialPutChar			; Continually try adding this character
		JC		AddCarriageReturn		; to serial channel until we get no error
        ;JNC     DonePutString			; Once this happens, done outputting the
DonePutString:							; string to the serial channel
        XOR     AX, AX                  ; Clear AX to have no error since caller
                                        ; function checks for return value in
                                        ; motor unit
		RET
SerialPutString		ENDP


CODE    ENDS
		END
