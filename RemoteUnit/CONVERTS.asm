; Maitreyi Ashok
; Section 1 – Richard Lee

        NAME    CONVERTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   CONVERTS                                 ;
;                             Conversion Functions                           ;
;                                   EE/CS 51                                 ;
;                                 Maitreyi Ashok                             ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; This file contains the conversion functions from a binary value to a string
; containing the decimal and hexadecimal representations. The ASCII convention
; is used to store the strings. 
;
; Table of Contents:
; Dec2String - converts 16 bit signed value to string with decimal representation
; Hex2String - converts 16 bit unsigned value to string with hex representation
;
; Revision History:
;     10/10/2016  Maitreyi Ashok      Wrote basic outline for convert procedures
;	  10/12/2016  Maitreyi Ashok	  Made corrections to functional specifications
;	  10/13/2016  Maitreyi Ashok	  Wrote initial assembly code for both functions
;	  10/14/2016  Maitreyi Ashok	  Corrected errors in code and tested functions
;	 								  Updated function header to reflect fixed code
;	  10/29/2016  Maitreyi Ashok	  Updated comments based on feedback
;	  12/07/2016  Maitreyi Ashok	  Changed Dec2String to be unsigned decimal
									  
$INCLUDE(Converts.inc)
$INCLUDE(General.inc)


CGROUP  GROUP   CODE

CODE	SEGMENT PUBLIC 'CODE'


        ASSUME  CS:CGROUP




; Dec2String
;
; Description: 		Converts a 16 bit unsigned value to its decimal 
;                   representation. Stores each digit of the decimal value at a 
;                   specified address. All values are treated as positive. Leading 
;                   zeroes are included so that all numbers have 5 digits. The strings 
;					are always null terminated and ASCII convention is used to 
;					represent the strings. The strings will be 6 bytes in length.
;
; Operation:  		The function starts with the largest power of 10 possible 
;                   (10000) and loops dividing the number by that The function 
;                   starts with the largest power of 10 possible (10000) and 
;                   loops dividing the number by that power of 10. The quotient 
;                   is the digit that will be converted to its ASCII value and 
;                   stored at the specified address. The remainder is used to 
;                   be divided by the next lowest power of 10 and get the next 
;			  	 	significant digit. This repeats until the value is 0, and 
;                   all digits of the decimal representation have been stored. 
;                   Then it terminates the string with the NULL character.
;
; Arguments:   		AX   	Binary value to convert to a string with decimal 
;                           representation
;					DS:SI   Address to store the decimal representation string at
; Return Value:		None
;
; Local Variables:	num (BX)  			passed binary value to convert
;					digit (AX)			computed decimal digit
;					pwr10 (DI)			current power of 10 being computed
;					address (SI)	    the address to store the next digit at
; Shared Variables: None
; Global Variables: None
;
; Input:			None
; Output:			None
;
; Error Handling:	None
;
; Algorithms:		Divide the number by successive powers of 10 to get each of 
;                   the decimal digits
; Data Structures:	None
;
; Registers Changed: AX, BX, DX, CX, SI, flags
; Stack Depth:		0 words
;
; Limitations:		Can only handle 16 bit unsigned numbers
;
; Author:			Maitreyi Ashok
; Last Modified:	10/10/2016  Wrote basic functional specification and pseudocode
;					10/12/2016	Added information to description and assigned 
;                               variables to registers
;					10/13/2016	Initial assembly code
;					10/14/2016  Fixed error in storing values at memory address 
;                               and tested code
;								Updated function header for different registers/code
;					12/07/2016	Changed to treat values as unsigned


Dec2String      PROC        NEAR
                PUBLIC      Dec2String

Dec2StringInit:
		MOV		BX, AX					; Store the num to convert into BX
		MOV		CX, MAX_PWR_10			; Store the highest power of 10 possible
		;JMP     ComputeDec
ComputeDec:
		CMP		CX, 0					; Check if the power of 10 is greater 
		JLE		GotAllDigitsDec			; than 0 (we have not gotten all 5 digits)
		;JG 	FindDigitsDec           ; If we have gotten all 5 digits, we are done
										; Otherwise, continue to get the next digit
FindDigitsDec:
		XOR     DX, DX                  ; Setup to get the next digit and update num
		MOV		AX, BX					
		DIV		CX       				; digit (AX) = num / pwr10
		MOV		BX, DX					; num (BX) = num MOD pwr10
		ADD 	AL, '0'	   				; Convert digit to ASCII value and store it at
		MOV		BYTE PTR [SI], AL	    ; next address to write to
		INC		SI						; Increment where we are storing the string
		MOV		AX, CX					; Set up to update pwr10
		XOR     DX, DX					; Clear DX register for 16 bit division
		MOV		CX, 10					; We want to divide current pwr10 by 10 
		DIV 	CX                      ; to get the new pwr10
		MOV		CX, AX					; Place the new pwr10 into the variable
		JMP 	ComputeDec              ; Done getting one digit, move to the next 
										; digit
		
GotAllDigitsDec:
		MOV  	BYTE PTR[SI], ASCII_NULL ; Null terminate the string
Dec2StringEnd:
        RET								; and return

Dec2String	ENDP




; Hex2String
;
; Description: 		Converts a 16 bit unsigned value to its hexadecimal representation.
;			   		Stores each digit/letter (0-9, A-F) of the hexadecimal value 
; 					at a specified address. Leading zeroes are included so that 
;					all numbers have 4 digits/letters. ASCII convention is used 
;                   to represent the strings. Thus, each string is ended with null 
;					termination and is 5 bytes (4 digits + null).
;
; Operation:		The function starts with the largest power of 16 possible (16^3) 
;					and loops dividing the number by that power of 16. The quotient  
;					is the digit that will be converted to its ASCII value and 
;					stored at the specified address. The remainder is used to be 
;					divided by the next lowest power of 16 and get the next significant 
;					digit. This repeats until the value is 0, and all digits of the 
;					hexadecimal representation have been stored. The string is then 
;                   terminated with the NULL character.
;
; Arguments:		AX		binary value to convert to a string with hexadecimal value
;	        		DS:SI	address to store the hexadecimal representation string at
; Return Value:		None
;
; Local Variables:	num (BX)			passed binary value to convert
;					digit (AX)			computed hexadecimal digit
;					pwr16 (DI)			current power of 16 being computed
;					address (SI)		the offset of the address to store the 
;										next digit
; Shared Variables: None
; Global Variables: None
;
; Input:			None
; Output:			None
;
; Error Handling:	None
;
; Algorithms:		Divide the number by successive powers of 16 to get 
;                   each of the hex digits
; Data Structures:	None
;
; Registers Changed: AX, BX, DX, CX, SI, flags
; Stack Depth:		0 words
;
; Limitations:		Can only handle 16 bit unsigned numbers
;
; Author:			Maitreyi Ashok
; Last Modified:	10/10/2016	Wrote basic functional specification and pseudocode
;					10/12/2016	Added information to description and assigned 
;                               variables to registers
;					10/13/2016	Wrote initial code for Hex2String function
;					10/14/2016  Fixed error in storing values at memory locations 
;								and tested code. Updated function headers with 
;                               different registers/code


Hex2String      PROC        NEAR
                PUBLIC      Hex2String

Hex2StringInit:
		MOV		BX, AX					; Store the num to convert into BX
		MOV		CX, MAX_PWR_16			; Store the highest power of 16 possible
		;JMP	ComputeHex
ComputeHex:
		CMP		CX, 0					; Check if the power of 16 is greater than 0
		JLE		GotAllDigitsHex			; (we have not gotten all digits)
		;JG 	FindDigitsHex           ; If we have gotten all digits 
                                        ; (the power of 16 <= 0), we are done
FindDigtsHex:
		XOR     DX, DX     				; Setup to get the next digit and update num
		MOV		AX, BX					
		DIV		CX      				; digit (AX) = num / pwr16
		MOV		BX, DX					; num (BX) = num MOD pwr16
		CMP		AL, 10					; If the digit is less than 10, it is a number. 
		JL		NumberDigit	
		;JGE	LetterDigit				; Otherwise, it is a letter in hex.
LetterDigit:
		ADD		AL, 'A' - 10			; Get the ASCII value of the A-F letter
		JMP		StoreAndUpdateHex		; and then store it in the proper address
NumberDigit:
		ADD		AL, '0'					; Get the ASCII value of the 0-9 digit
		;JMP	StoreAndUpdateHex		; and then store it in the proper address

StoreAndUpdateHex:	
		MOV		BYTE PTR [SI], AL	    ; Store ASCII value at specified address 
                                        ; plus offset of digits written so far
		INC		SI  					; Increment the address where we store the char
		SHR		CX, 4					; Divide pwr16 by 16 to get the next pwr16
		JMP 	ComputeHex				; Done getting one digit, now get the next digit
		
GotAllDigitsHex:
		MOV  	BYTE PTR[SI], ASCII_NULL ; Null terminate the string
Hex2StringEnd:		
        RET								; and return				
        

Hex2String	ENDP



CODE    ENDS



        END