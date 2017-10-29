; Maitreyi Ashok
; Section 1 – Richard Lee

        NAME    SerialIO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  Serial I/O                                ;
;                               Serial I/O Routines	                         ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; This file contains the basic routines for handling of 16C450 serial input and output.
; This is done through interrupt driven code and a event and transmitter
; queue
;
; Table of Contents:
; Routines
; SerialPutChar - Outputs a character to the serial channel
; InitSerial - Sets up the serial channel and TxQueue
; SetSerialBaud - Set baud rate of serial channel
; SetSerialParity - Set parity of serial channel
; SerialEventHandler - Finds out what interrupts have happened
; SerialErrorHandler - Handle serial errors
; SerialDataReceived - Handle data received from serial channel
; SerialTransmitterEmpty - Handle transmitter holding register being empty
; SerialModemStatus - Reset modem status interrupt
;
; Tables
; EHAddr - Call table of specific event handler to handle serial events
; BaudRates - Contains various baud rate divisors for use by serial chip
; ParityTable - Contains various options for parity for use by serial chip
;
; Revision History:
;     11/14/2016		Maitreyi Ashok		Pseudocode/Functional Specification
; 	  11/15/2016		Maitreyi Ashok		Wrote code
;	  11/17/2016		Maitreyi Ashok		Debugging of code
;	  11/18/2016		Maitreyi Ashok		Updated comments




$INCLUDE(SerialIO.inc)		; Contains serial definitions specific to RoboTrike
$INCLUDE(simpmac.inc)		; Definitions for CRITICAL_START and CRITICAL_END macros
$INCLUDE(16C450.inc)		; Contains general 16C450 serial chip definitions
$INCLUDE(IntH.inc)			; General definitions of interrupt controller
$INCLUDE(SerInt2.inc)		; Specific 80x86 definitions of INT 2 interrupt
$INCLUDE(KeyCodes.inc)		; Contain key code definitions for EventQueue
$INCLUDE(Queue.inc)			; Contain queue definitions
$INCLUDE(General.inc)		; Data size definitions

CGROUP  GROUP   CODE
DGROUP  GROUP   DATA

CODE	SEGMENT PUBLIC 'CODE'


        ASSUME  CS:CGROUP, DS:DGROUP
        
        EXTRN   QueueInit:NEAR		; Initialize TxQueue
        EXTRN   Enqueue:NEAR		; Enqueue element to TxQueue
        EXTRN   Dequeue:NEAR		; Dequeue element from TxQueue
        EXTRN   QueueEmpty:NEAR		; Check if TxQueue is empty or
        EXTRN   QueueFull:NEAR		; if it is full to avoid blocking
        EXTRN   EnqueueEvent:NEAR	; Enqueue event to EventQueue
        
; SerialPutChar		
; Description: 		This function outputs a character passed in to the serial 
;					channel. It does this by enqueuing the character it receives
;					onto the transmitter queue. If the queue is full, the character
;					cannot be added to the queue, and the enqueue function is not
;					called to avoid blocking. However, if the queue is not full,
;					the character is enqueued. Either way, the caller function
;					is notified based on the return value. In addition, the function
;					is responsible for kickstarting the transmitter empty interrupt
;					if it has been turned off at any point.
;
; Operation:  		This function first checks whether the transmitter queue
;					is full. If it is, the carry flag is set and returned to
;					the caller function to signify that the character was not
;					even added to the queue yet. If the transmitter queue is not
;					full, the character is enqueued to the transmitter queue, and
;					the carry flag is reset and returned to the caller function
;					that the output to the queue was successful. Also, the function
;					can kickstart the transmitter empty interrupt which can get
;					turned off if the serial channel expects a character but does
;					not receive it due to the transmitter queue being empty. The
;					kickstarting is done by clearing the bit that enables the transmitter
;					holding register empty interrupt in the interrupt enable register
;					and then setting it to force future interrupts from the
;					transmitter holding register becoming empty, since this step
;					causes another transmitter holding register empty interrupt
;					to happen immediately.
;					
;
; Arguments:   		char [AL] - character to output to serial channel
; Return Value:		enqueueFailed [CF] - set if transmit queue is full, reset if put
;							in serial channel queue
;
; Local Variables:	IER_val [AL]- value in the Interrupt Enable Register that is used to 
;							clear and set ETBE when kickstarting
; Shared Variables: TxQueue (R/W) - transmitter queue holding characters to eventually 
;							output to serial channel
;					kickstart (R/W) - value set to 1 if transmitter empty interrupt must
;							be kickstarted again
; Global Variables: None
;
; Input:			Transmitter empty interrupt when lack of characters in TxQueue causes
;					kickstarter to be set
; Output:			Transmitter empty interrupts re-enabled as needed
;
; Error Handling:	If the transmitter queue is already full, the character is not
;					added and the function does not block. Instead, a set carry flag
;					is returned to the user.
;
; Algorithms:		None
; Data Structures:	A queue is used to hold all characters that must be transmitted.
;
; Registers Changed: SI, DX, BX, AL, flags
; Stack Depth:		0 words
;
; Limitations:		This function does not let the user know that the character 
;					has been sent over the serial channel, and resets the return
;					value as long as the character is added to the queue.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/14/16	Maitreyi Ashok	Pseudocode/functional specification
;					11/15/16	Maitreyi Ashok	Wrote code
;					11/17/16	Maitreyi Ashok	Switched order of enqueuing and 
;												kickstarting

SerialPutChar   PROC        NEAR
                PUBLIC      SerialPutChar
CheckIfCanEnqueueChar:
		MOV		SI, OFFSET(TxQueue)			; Check if TxQueue is full
		CALL	QueueFull
		JZ		EnqueueFailed				; If it is, then can't enqueue the character
		;JNZ	EnqueueChar					; Otherwise, enqueue the character
        %CRITICAL_START
EnqueueChar:
		CALL	Enqueue						; Enqueue character into TxQueue
CheckKickstartNecessary:
		CMP		kickstart, KICKSTART_NEEDED	; Check if need to kickstart THRE interrupt
		JNE		FinishEnqueue				; If we don't need to, finish enqueuing
		;JE		KickstartTransInt			; If we do, do the kickstarting
KickstartTransInt:	
		MOV		DX, IER_REG					; Get the value in the interrupt enable
		IN		AL, DX						; register of the serial chip
DisableTransInt:
		AND		AL, NOT(ENABLE_ETBE)			; Disable the transmitter holding register
		OUT		DX, AL						; empty interrupt
RestartTransInt:
		OR		AL, ENABLE_ETBE				; Reenable the transmitter holding register
		OUT		DX, AL						; empty interrupt, causing those interrupts
											; to happen again
        MOV     kickstart, KICKSTART_NOT_NEEDED
		;JMP	FinishEnqueue				; Reset the kickstart flag
FinishEnqueue:
        %CRITICAL_END
        CLC									; If enqueuing was successful, clear return
        JMP    SerialPutCharEnd				; value
EnqueueFailed:
		STC									; If enqueuing was not successful (queue was
		;JMP	SerialPutCharEnd			; full), set return value
SerialPutCharEnd:
		RET									; Whether queuing was successful returned
SerialPutChar	ENDP						; to caller

; InitSerial		
; Description: 		This function sets up the serial transmission for use for
;					transmitting and receiving data. This is done by first initializing
;					the interrupt enable register to enable interrupts of all
;					types. The line control register is initialized to have a word 
;					size of 8 bits, 1 stop bit is used, and the break is cleared. 
;					The transmit queue is initialized to hold values before they 
;					are added to the serial channel. The baud rate and parity setting 
;					functions are called to allow user defined settings.
;
; Operation:  		This function first inializes the control register for serial
;					transmission. This is done by defining the word size, number
;					of stop bits, forcing breaks, and stopping access to the divisor 
;					latch so that the interrupt enable register can next be set. The 
;					interrupt enable register is set to enable all interrupts. The transmit 
;					queue is also initialized since it will be used once SerialPutChar 
;					is called and Transmitter empty interrupts start occurring. In 
;					addition, the transmit empty interrupt will need to be kickstarted 
;					by SerialPutChar since the serial channel may try to transmit
;					a character before having one to transmit as other setup takes
;					place. For this reason, the kickstart shared variable is set
;					when initializing so the kickstarting process can take place
;					once a character is added to the transmit queue. Finally, the
;					baud rate and parity setting functions are called to set these
;					values independently.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	None
; Shared Variables: TxQueue (W) - transmitter queue holding characters to eventually 
;							output to serial channel
;					kickstart (W) - value set to 1 if transmitter empty interrupt must
;							be kickstarted again
; Global Variables: None
;
; Input:			None
; Output:			Interrupts will occur when received data is available, the transmitter
;					holding register is empty, or there is receiver line status
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	A queue is used to hold all characters that must be transmitted.
;
; Registers Changed: DS, AX, SI, BX, DI, CX, flags
; Stack Depth:		0 words
;
; Limitations:		This function does not allow for RTS-CTS handshaking since the
;					modem status interrupt is disabled.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/14/16	Maitreyi Ashok	Pseudocode/functional specification
;					11/15/16	Maitreyi Ashok	Wrote code
;					11/16/16	Maitreyi Ashok	Started passing arguments to Baud/Parity
;												functions
;					11/17/16	Maitreyi Ashok	Enabled modem status interrupt
;					11/18/16	Maitreyi Ashok	Updated comments
 
InitSerial	PROC        NEAR
            PUBLIC      InitSerial
SetLineControl:
		MOV		DX, LCR_REG				; Set line control register with a word size of
										; 8 bits, 1 stop bit per character, no forced break
										; and divisor latch access off 
		MOV		AL, EIGHT_BIT_WORD_SIZE OR ONE_STOP_BIT OR RESET_BREAK OR RESET_ACCESS_DLAB
		OUT		DX, AL
SetIntEnable:
		MOV		DX, IER_REG             ; Enable all types of interrupts in interrupt
										; enable register
		MOV		AL, ENABLE_ERBF OR ENABLE_ETBE OR ENABLE_ELSI OR ENABLE_EDSSI
		OUT		DX, AL
InitTransmitQueue:
		MOV		SI, OFFSET(TxQueue)		; Initialize TxQueue with the maximum
		MOV		BL, BYTE_ELEM_SIZE		; array size QueueStruc can support and 
		MOV		AX, MAX_ARRAY_SIZE		; each element of queue as a byte (character)
		CALL	QueueInit
OtherSerialSettings:
		MOV		kickstart, KICKSTART_NEEDED	
										; Need to kickstart since transmitter empty
										; until first SerialPutChar call
        MOV     BX, DEFAULT_BAUD_RATE		; Set baud rate using indices from BaudRates
		CALL	SetSerialBaud			; table as argument
        MOV     DI, DEFAULT_PARITY		; Set parity using indices from ParityTable
		CALL	SetSerialParity			; as arguments
		RET     
InitSerial	ENDP


; SetSerialBaud		
; Description: 		This function sets the baud rate based on the number in the
;					range from 0 to 11 passed in to the function as the percentage
;					of the total baud rate wanted. Thus, the baud rate is set to
;					the one corresponding to the index passed in from the BaudRates
;					table and the appropriate divisior is used to generate 16xCLOCK. 
;
; Operation:  		The function first sets the DLAB access bit in the line control
;					register to give access to the divisor latch registers. Then,
;					based on the index (baud rate setting) passed in to the function,
;					the value at that index of the table is used to get the baud 
;					rate divisor for the desired rate. This divisor from the table
;					is saved to the divisor latch registers. Finally, the DLAB access 
;					bit will be cleared again to allow future access to the interrupt 
;					enable register as well as the receiver buffer and transmitter 
;					holding registers.
;
; Arguments:   		baudSetting [AX] - value between 0 and NUM_BAUD_RATES - 1 to set 
;							baud rate between BaudRates[0] and BaudRates[11] 
;							(not exact percentage of max but 12 common baud rate 
;							settings supported by serial chip)
; Return Value:		None
;
; Local Variables:	LCR_val [AL] - store value of line control register currently
; Shared Variables: None
; Global Variables: None
;
; Input:			None
; Output:			Bits will be transmitted on the serial channel at the baud
;					rate specified in the function.
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	Uses table of baud rate divisors for NUM_BAUD_RATES common baud 
;					settings
;
; Registers Changed: DX, AX, BX, flags
; Stack Depth:		2 words
;
; Limitations:		This function currently only allows for NUM_BAUD_RATES possible 
;					baud rate settings
;
; Author:			Maitreyi Ashok
; Last Modified:	11/14/16	Maitreyi Ashok	Pseudocode/functional specification
;					11/15/16	Maitreyi Ashok	Wrote code
;					11/16/16	Maitreyi Ashok	Changed to take in baud rate arguments
;					11/18/16	Maitreyi Ashok	Updated comments

SetSerialBaud	PROC        NEAR
				PUBLIC      SetSerialBaud
SetDivisorLatch:
        %CRITICAL_START					; Errors will occur if the interrupt handler
										; attempts to access the reciever buffer 
										; register or transmitter holding register
										; since they will access the divisor latch
										; after DLAB is set, so don't allow interrupts
		MOV		DX, LCR_REG				; Allow access to divisor latch registers
		IN		AL, DX					; by setting the DLAB bit in the line
		OR		AL, ACCESS_DIVISOR_LATCH
										; control register
		PUSH	AX						; Save address of LCR and value just written
		OUT		DX, AL					; to avoid reading again when resetting DLAB
		PUSH	DX						; at end of function
SetBaudDivisor:
		MOV		DX, DLL_LSB_REG			 ; Find the baud rate divisor from the Baud
        SHL     BX, BYTES_PER_WORD_SHIFT ; rates table for index passed in and 
        MOV     AX, WORD PTR CS:BaudRates[BX]
		OUT     DX, AX					 ; save that divisor into divisor latch register
ClearDivisorLatch:				
		POP		DX						; Disallow access to divisor latch registers
		POP		AX						; by clearing DLAB bit in the l ine control 
		AND		AL, NO_ACCESS_DIVISOR_LATCH
		OUT		DX, AL					; register
		%CRITICAL_END					; Allow interrupts to happen once critical
		RET								; code ends and DLAB is reset
SetSerialBaud	ENDP


; SetSerialParity		
; Description: 		This function sets the parity of the serial channel using the
;					line control register. The index of the parity to use from
;					the ParityTable register is passed in and the specified parity
;					is then set in the register. The options for parity are no parity,
;					even parity, odd parity, set parity, and clear parity. 
;
; Operation:  		This function sets the serial channel to use whatever parity
;					is specified in the argument. The user passes in an index 
;					0, 1, 2, 3, or 4 for no parity, even parity, odd parity, set
;					parity or clear parity respecitively. This is done by finding
;					the appropriate parity setting bits in the ParityTable. Then
;					the three bits that relate to parity in the line control 
;					register are cleared and the necessary bits are set based on
;					the table entry.		
;
; Arguments:   		parity - this argument specifies the index into ParityTable
;							to use to define the parity of the serial I/O
;		
; Return Value:		None
;
; Local Variables:	LCR_val [AL]- store value of line control register currently
; Shared Variables: None
; Global Variables: None
;
; Input:			None
; Output:			Parity bits are included depending on the settings of this function
;					when transmitting bits on the serial channel. If included,
;					this function will also specify the even/odd parity and whether
;					to set setick parity.
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	Uses table of parity settings for serial I/O
;
; Registers Changed: AL, DX, flags
; Stack Depth:		0 words
;
; Limitations:		The user can not enter an absolute type of parity as the argument
;					but instead needs to provide an index into the parity table.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/14/16	Maitreyi Ashok	Pseudocode/functional specification
;					11/15/16	Maitreyi Ashok	Wrote code
;					11/16/16	Maitreyi Ashok	Changed code to take in parity argument
;					11/18/16	Maitreyi Ashok	Updated comments

SetSerialParity	PROC        NEAR
				PUBLIC      SetSerialParity
SetNoParity:
		MOV		DX, LCR_REG				; Get the value currently in the line control
		IN		AL, DX					; register
		AND		AL, PARITY_MASK			; Clear the parity related bits in the value
        OR      AL, CS:ParityTable[DI]	; and set the parity bits based on the
		OUT		DX, AL					; parity setting argument passed in
		RET
SetSerialParity	ENDP

; SerialEventHandler			
; Description: 		This function does the general event handling when an INT 2
;					external interrupt occurs. The function examines the 
;					interrupt indentity register. Depending on what bits are set
;					in the low 3 bits of the register, the function decides
;					which specific external interrupts occurred to cause the INT
;					2 interrupt. Then, the specific event handlers for these
;					interrupts are called by the generic event handler using a
;					call table to find the offset of the function based on
;					the type of interrupt.
; Operation:  		This function first finds the bit pattern in the interrupt
;					identity register. This allows the event handler to know
;					what specific interrupts caused the INT 2 interrupt. Then,
;					the bit pattern in the IIR register is compared to the pattern
;					for no interrupt pending. If there is no interrupt pending, then
;					there is no work to be done with the serial event handler.
;					Otherwise, it uses the bit value to index the call table of 
;					event handler addresses. Using the call table address, the 
;					generic event handler can call a more specific event handler 
;					to take care of specific interrupts.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	IIR_val [AL]- value in Interrupt Identity register
; Shared Variables: None
; Global Variables: None
;
; Input:			An error, data received, or transmitter holding register
;					empty in the serial channel
; Output:			If transmitter holding register empty was reason for interrupt
;					another character is transmitted to serial channel.
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	Uses call table with addresses of specific event handlers
;
; Registers Changed: DX, AX, SI, flags
; Stack Depth:		8 words
;
; Limitations:		If some error somehow occurs so that the IIR register does not
;					have all zeroes in the high byte, the code will break due to
;					the accessing of the 4 entry word call table with the value
;					in the IIR register as the index.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/14/16	Maitreyi Ashok	Pseudocode/functional specification
;					11/15/16	Maitreyi Ashok	Wrote code
;					11/17/16	Maitreyi Ashok	Added looping around event handler
;												until no interrupts
;					11/18/16	Maitreyi Ashok	Updated comments
			

SerialEventHandler  PROC        NEAR
					PUBLIC      SerialEventHandler
SetUpEH:
		PUSHA							; Save all registers so interrupted code is
										; not affected
GetIntCode:
		MOV		DX, IIR_REG				; Read what the identity of the interrupt
		IN		AL, DX					; that occurred is
CheckIfIntPending:
		TEST	AL, NO_INT_PENDING		; Check if an interrupt is pending
		JNZ		EndEventHandler			; If not, no interrupts to handle
		;JZ		CallSpecificHandler		; If it is, find out which one
CallSpecificHandler:                    
		XOR     AH, AH					; Use the bits in the register as an index
        MOV     SI, AX					; into the call table by using a 4 bit value
										; that is 0, 2, 4, or 6
        CALL    WORD PTR CS:EHAddr[SI]	; Call the specific event handler for the
										; interrupt
		JMP	    GetIntCode				; Repeat this process in case there are more
										; interrupts since we are using edge level
										; triggering which only calls event handler
										; when signal first goes active
EndEventHandler:
		MOV		DX, INT_CTRLR_EOI   	; Send an EOI for the interrupt when done so 
		MOV		AX, NON_SPEC_EOI		; other processes can now interrupt
		OUT		DX, AL
		POPA							; Restore all registers to old state
		IRET
SerialEventHandler	ENDP

; SerialErrorHandler			
; Description: 		This function handles any errors when it is called by the generic
;					event handler due to a 16C450 receiver line status interrupt.
;					The function checks if the line status register actually 
;					signifies an error by masking out non error bits and examining
;					the remaining bits. Any error bits are then enqueued to the
;					Event Queue using the EnqueueEvent function along with a key
;					code to signify the serial error.
;
; Operation:  		This function is called by SerialEventHandler if the value in
;					the interrupt identity register signified that there was a 
;					Receiver Line Status interrupt. However, since the Line Status
;					register includes information not only about overrun, parity,
;					and framing errors, it also includes information about data ready,
;					break interrupt indicators, THRE indicator, and transmitter
;					empty indicator, the latter of which are not errors. Thus, these
;					bits are masked out to only view the error bits. Then, if there
;					are still error bits set, these bits are enqueued into an EventQueue
;					with a specific serial error keycode used to distinguish from
;					other events in the queue as well as the key value as the value
;					in the line status register with the non-error bits masked out.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	LSR_val [AL]- value in Line Status Register
; Shared Variables: None
; Global Variables: None
;
; Input:			Error in serial channel transmission
; Output:			Serial Error Event in EventQueue to be handled in main loop
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	An event queue is used to store the errors from the serial channel
;					along with all other events (other serial, keypad, etc.)
;
; Registers Changed: AX, DX, flags
; Stack Depth:		0 words	
;
; Limitations:		If the Event Queue is full when the error is handled, it will
;					not be added to the queue since EnqueueEvent does not call the
;					Enqueue function if the queue is full to avoid blocking and
;					instead immediately returns.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/14/16	Maitreyi Ashok	Pseudocode/functional specification
;					11/15/16	Maitreyi Ashok	Wrote code
;					11/18/16	Maitreyi AShok	Updated comments

SerialErrorHandler	PROC        NEAR
					PUBLIC      SerialErrorHandler
FindErrorType:
		MOV		DX, LSR_REG				; Read line status register to find out 
		IN		AL, DX					; what the status is
		AND		AL, NON_ERROR_BITS_MASK	; Check if the status includes any actual
										; errors
		JZ		DoneWithErrorHandler	; If not, nothing to handle
		;JNZ	EnqueueErrors			; If there is an error, store the event
EnqueueErrors:
		MOV		AH, SERIAL_ERROR		; Enqueue the serial error type event
		CALL	EnqueueEvent			; into the event queue for it to be handled
		;JMP	DoneWithErrorHandler	; outside
DoneWithErrorHandler:
		RET
SerialErrorHandler	ENDP

; SerialDataReceived			
; Description: 		This function handles any data when it is called by the generic
;					event handler due to a 16C450 received data interrupt.
;					The function reads the data from the receiver buffer register
;					and enqueues the byte value to the Event Queue using the 
;					EnqueueEvent function along with a key code to signify that
;					the event type is the serial data received.
;
; Operation:  		This function is called by SerialEventHandler if the value in
;					the interrupt identity register signified that there was a 
;					Recived Data interrupt. Based on this, this function reads the
;					receiver buffer register to read the 8 bit value that has been
;					received. These bits are then enqueued into an EventQueue
;					with a specific serial data keycode used to distinguish from
;					other events in the queue as well as the key value as the value
;					in the receiver buffer register.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	RBR_val [AL]- value in receiver buffer register
; Shared Variables: None
; Global Variables: None
;
; Input:			Data received from serial channel
; Output:			Dat Sent Event in EventQueue to be handled in main loop
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	An event queue is used to store the data from the serial channel
;					along with all other events (other serial, keypad, etc.)
;
; Registers Changed: AX, DX
; Stack Depth:		0 words
;
; Limitations:		If the Event Queue is full when the data is handled, it will
;					not be added to the queue since EnqueueEvent does not call the
;					Enqueue function if the queue is full to avoid blocking and
;					instead immediately returns.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/14/16	Maitreyi Ashok	Pseudocode/functional specification
;					11/15/16	Maitreyi Ashok	Wrote code
;					11/18/16	Maitreyi Ashok	Updated comments
;					
; Operational Notes
; Read value from RBR
; Enqueue value with data key type

SerialDataReceived	PROC        NEAR
					PUBLIC     SerialDataReceived
GetValueOfSerialData:
		MOV		DX, RBR_REG				; Read the value in the receiver buffer
		IN		AL, DX					; register
EnqueueDataReceived:
		MOV		AH, SERIAL_DATA_RECEIVED
		CALL	EnqueueEvent			; Enqueue the received data into the event
										; queue using a SERIAL_DATA_RECEIVED key code
										; for it to be handled in the main loop
		RET
SerialDataReceived	ENDP

; SerialTransmitterEmpty			
; Description: 		This function handles any data transmission when it is called 
;					by the generic event handler due to a 16C450 transmitter empty 
;					interrupt. The function dequeues a value from the TxQueue (if
;					there are values in it), and writes the value to the transmitter
;					holding register. If the TxQueue was empty and did not have 
;					any characters to transmit, the kickstart shared variable is 
;					set to indicate	that the SerialPutChar would need to restart 
;					the Transmitter	Empty Interrupts.
;
; Operation:  		This function is called by SerialEventHandler if the value in
;					the interrupt identity register signified that there was a 
;					Transmitter Empty interrupt. Based on this, this function 
;					dequeues a character from the TxQueue. This byte is then written
;					to the tranmitter holding register. However, if the TxQueue is empty
;					a value will not be dequeued. Instead, no value will be added
;					to the EventQueue and a variable will be set to kickstart this
;					interrupt again later when we have data. The kickstarting is
;					necessary since the serial channel expected a character did not 
;					receive a character when it expected to do so, and will thus 
;					never have the Transmitter Empty interrupt since it is assumed 
;					that there will never be data again. However, this interrupt
;					will be kickstarted again by the SerialPutChar if this
;					variable is set when SerialPutChar recieves a character that
;					would make TxQueue nonempty.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	char [AL]- character dequeued from the transmit queue to be stored
;							in transmitter holding register
; Shared Variables: TxQueue (R/W) - transmitter queue holding characters to eventually 
;							output to serial channel
;					kickstart (W) - value set to 1 if transmitter empty interrupt must
;							be kickstarted again
; Global Variables: None
;
; Input:			Transmitter holding register ready to hold data to transmit
;							directly to serial channel
; Output:			Data Transmission Event in EventQueue to be handled in main loop			
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	An event queue is used to store the data from the serial channel
;					along with all other events (other serial, keypad, etc.)
;
; Registers Changed: SI, DX, AX, BX, flags
; Stack Depth:		0 words
;
; Limitations:		If the Event Queue is full when the data is transmitted, it will
;					not be added to the queue since EnqueueEvent does not call the
;					Enqueue function if the queue is full to avoid blocking and
;					instead immediately returns.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/14/16	Maitreyi Ashok	Pseudocode/functional specification
;					11/15/16	Maitreyi Ashok	Wrote code
;					11/18/16	Maitreyi Ashok	Updated comments

SerialTransmitterEmpty	PROC        NEAR
						PUBLIC      SerialTransmitterEmpty
CheckIfTxQueueEmpty:
		MOV		SI, OFFSET(TxQueue)		; Check if the TxQueue is empty
		CALL	QueueEmpty				; If it is, then need to kickstart this
		JZ		NeedToKickstart			; interrupt later since holding register
										; will expect a character but not get one
										; and thus stop this type of interrupt
		;JNZ	TransferValueToSerial	; If not empty then we can transfer a value
TransferValueToSerial:
		CALL	Dequeue					; Dequeue an element from the TxQueue
		MOV		DX, THR_REG				; Write the character to the transmitter
		OUT		DX, AL					; holding register
		JMP		DoneWithTransEmptyHandler
NeedToKickstart:
		MOV		kickstart, KICKSTART_NEEDED
										; Signify that the next successful call to
										; SerialPutChar needs to start up the THRE
										; interrupt by setting a flag
		;JMP	DoneWithTransEmptyHandler
DoneWithTransEmptyHandler:
		RET		
SerialTransmitterEmpty	ENDP

; SerialModemStatus			
; Description: 		This function handles any modem status interrupts when it is 
;					called by the generic event handler due to a 16C450 transmitter 
;					empty interrupt. The interrupt occurs because of a clear to 
;					send, data set ready, ring indicator, or data carrier detect.
;					The interrupt is reset by just reading the modem status register.
;
; Operation:  		This function is called by SerialEventHandler if the value in
;					the interrupt identity register signified that there was a 
;					Modem status interrupt. To reset the modem status interrupt
;					the modem status register is just read. However, the value
;					read is just ignored as modem status interrupts are not used
;					in the implementation of serial I/O for any purpose.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	None
; Shared Variables: None
; Global Variables: None
;
; Input:			Some modem status change causing INT 2 interrupt
; Output:			None
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	None
;
; Registers Changed: DX, AL
; Stack Depth:		0 words
;
; Limitations:		This function does not actually use the modem status for
;					handshaking to ensure that the data transfer goes smoothly
;					and instead just ignores the modem status
;
; Author:			Maitreyi Ashok
; Last Modified:	11/16/16	Maitreyi Ashok	Initial Revision
;					11/18/16	Maitreyi Ashok	Updated comments
;					

SerialModemStatus	PROC        NEAR
					PUBLIC      SerialModemStatus
ReadStatus:
        MOV     DX, MSR_REG		; Read value in modem status register
        IN      AL, DX			; to reset interrupt
		RET		
SerialModemStatus	ENDP

; EHAddr
; Description:		Call table that contains addresses of specific event handlers
;					to handle the 4 types of interrupts that happen in 16C450.
;					These interrupts are the modem status interrupt, transmitter
;					holding register empty interrupt, serial data received interrupt,
;					and the receiver line status interrupt. These are indexed in
;					the table according to the value in the interrupt identification
;					register for each of the interrupts
;
; Author:			Maitreyi Ashok
; Last Modified:	11/16/16	Maitreyi Ashok		Created table
;					11/18/16	Maitreyi Ashok		Updated comments
;
EHAddr	LABEL	WORD
; 	DW		Address Of Event Handler
    DW      SerialModemStatus			; Modem status interrupt
	DW		SerialTransmitterEmpty		; Transmitter holding register empty interrupt
	DW		SerialDataReceived			; Received data available interrupt
	DW		SerialErrorHandler			; Receiver line status interrupt

; BaudRates
; Description:      Contains baud rate divisors for 12 different baud rates supported
;					by the 16C450 serial chip. The baud rate divisors are found through
;					the following calculation:
;						Divisor = Clock Frequency / (Desired baud rate * 16)
;					In our case, the frequency of the processor clock is 18.342 MHZ
;					but we have to divide the clock frequency by 2 since the timers
;					clock is at half the rate of the processor. These values are used
;					in setting the serial baud rate by indexing the table using
;					one of the baud rates to be used.
;
; Author            Maitreyi Ashok
; Last Modified:    11/16/16    Maitreyi Ashok      Created table
;					11/17/16	Maitreyi Ashok		Removed baud rates too slow
;													for RealTerm
;					11/18/16	Maitreyi Ashok		Updated comments

BaudRates   LABEL   WORD
;   DW      Baud Rate Divisor     Baud Rate
    DW      1920                ; 300
    DW      960                 ; 600
    DW      480                 ; 1200
    DW      320                 ; 1800
    DW      288                 ; 2000
    DW      240                 ; 2400
    DW      160                 ; 3600
    DW      120                 ; 4800
    DW      80                  ; 7200
    DW      60                  ; 9600
    DW      30                  ; 19200
    DW      15                  ; 38400
	
; ParityTable
; Description:      Contains the various bit patterns for the different options
;					of parity with different combinations of bits 3-5 set. These
;					options are no parity, even parity, odd parity, set parity, 
;					and clear parity. These values are used in the set parity function
;					by accessing some index of the table to get the bits for
;					a specific parity. These bits can then be OR-ed with a value
;					that has bits 3-5 cleared to get the value to be written to
;					the line control register to transmit data with some parity.
;
; Author            Maitreyi Ashok
; Last Modified:    11/16/16    Maitreyi Ashok      Created Table
;					11/18/16 	Maitreyi Ashok		Updated comments

ParityTable  LABEL   BYTE
;   DB      Parity Bits to Set
    DB      NO_PARITY       ; No parity
    DB      EVEN_PARITY     ; Even Parity
    DB      ODD_PARITY      ; Odd Parity
    DB      SET_PARITY      ; Set Parity
    DB      CLEAR_PARITY    ; Clear Parity

CODE    ENDS

;the data segment

DATA    SEGMENT PUBLIC  'DATA'

TxQueue     QueueStruc      <>		; Holds the characters to transfer to the 
									; transmitter holding register before 
									; outputting to serial channel
kickstart   DB          	?		; whether transmitter empty interrupt needs to
									; be forced to occur again when enqueuing the
									; next character

DATA    ENDS

        END