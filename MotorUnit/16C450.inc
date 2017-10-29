; Maitreyi Ashok
; Section 1 - Richard Lee
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  16C450.INC                                ;
;                         	 16C450 Serial Definitions                       ;
;                                 Include File                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the definitions for the 16C450 Serial Chip.
;
;
; Revision History:
;    11/16/16		Maitreyi Ashok		Initial revision
;	 11/18/16		Maitreyi Ashok		Updated comments
;	 12/01/16		Maitreyi Ashok		Removed redundant interrupt enable definitions

; Register Addresses
RBR_REG			EQU		SERIAL_BASE + 0		; Receiver Buffer register (read only)
THR_REG			EQU		SERIAL_BASE + 0		; Transmitter holding register (write only)
IER_REG			EQU		SERIAL_BASE + 1		; Interrupt Enable register
IIR_REG			EQU		SERIAL_BASE + 2		; Interrupt Identity Register
LCR_REG			EQU		SERIAL_BASE + 3		; Line Control register
MCR_REG			EQU		SERIAL_BASE + 4		; Modem control register
LSR_REG 		EQU		SERIAL_BASE + 5		; Line status register	
MSR_REG			EQU		SERIAL_BASE + 6		; Modem status register
SCR_REG			EQU		SERIAL_BASE + 7		; Scratch register
DLL_LSB_REG		EQU		SERIAL_BASE + 0		; Least significant byte of divisor latch
DLL_MSB_REG		EQU		SERIAL_BASE + 1		; Most significant byte of divisor latch

; Transmitter Holding Register 
CLEAR_THR		EQU		00000000B			; Clear transmitter holding register

; Interrupt Enable Register
ENABLE_ERBF		EQU		00000001B			; Enable received data available interrupt
ENABLE_ETBE		EQU		00000010B			; Enable disable transmitter holding
ENABLE_ELSI		EQU		00000100B			; Enable disable receiver line
ENABLE_EDSSI	EQU		00001000B			; Enable disable modem status interrupt

; Interrupt Identity Register
NO_INT_PENDING		EQU		00000001B		; No interrupt pending to be handled
REC_LINE_STAT_INT	EQU		00000110B		; Receiver line status interrupt pending
REC_DATA_AVAIL_INT	EQU		00000100B		; Received data interrupt pending
TRANS_REG_EMPTY_INT	EQU		00000010B		; Transmitter holding register empty 
											; interrupt pending
MODEM_STAT_INT		EQU		00000000B		; Modem status interrupt pending

; Line Control Register
FIVE_BIT_WORD_SIZE		EQU		00000000B	; 5 bit word size used for each character
SIX_BIT_WORD_SIZE		EQU		00000001B	; 6 bit word size used for each character
SEVEN_BIT_WORD_SIZE		EQU		00000010B	; 7 bit word size used for each character
EIGHT_BIT_WORD_SIZE		EQU		00000011B	; 8 bit word size used for each character

ONE_STOP_BIT			EQU		00000000B	; 1 stop bit used to end character
ONE_AND_HALF_STOP_BIT	EQU		00000100B	; 1.5 stop bits used to end character
TWO_STOP_BITS			EQU		00000100B	; 2 stop bits used to end character

NO_PARITY   		EQU		00000000B		; No parity is generated or checked
EVEN_PARITY			EQU		00011000B		; Even or odd number of logic 1s in 
ODD_PARITY			EQU		00001000B		; data and parity bits
SET_PARITY			EQU		00101000B		; Parity bit transmitted and checked as 
											; set
CLEAR_PARITY		EQU		00111000B		; Parity bit transmitted and checked as
											; cleared
PARITY_MASK             EQU 11000111B		; Mask out parity bits to allow or'ing 
											; of desired parity type

FORCE_BREAK			EQU		01000000B		; Force break condition
RESET_BREAK			EQU		00000000B		; Don't set break condition
DISABLE_BREAK		EQU		NOT(FORCE_BREAK)
											; Disable break condition

ACCESS_DIVISOR_LATCH 	EQU		10000000B	; Allow divisor latch access of baud
											; generator
RESET_ACCESS_DLAB       EQU     00000000B	; Don't allow divisor latch access
NO_ACCESS_DIVISOR_LATCH EQU     NOT(ACCESS_DIVISOR_LATCH)
											; Disable divisor latch access
; Modem Control Register
DATA_TERM_READY		EQU		00000001B	; Force !(DTR) output to active state
DATA_TERM_NOT_READY	EQU		00000000B	; Force !(DTR) output to inactive state
REQUEST_TO_SEND		EQU		00000010B	; Force !(RTS) output to active state
NO_REQUEST_TO_SEND	EQU		00000000B	; Force !(RTS) output to inactive state
MODEM_OUT_1			EQU		00000100B	; Force !(OUT1) signal to active state
MODEM_NOT_OUT_1		EQU		00000000B	; Force !(OUT1) signal to inactive state
MODEM_OUT_2			EQU		00001000B	; Force !(OUT2) signal to active state
MODEM_NOT_OUT_2		EQU		00000000B	; Force !(OUT2) signal to inactive state
MODEM_LOOP			EQU		00010000B	; Loopback feature for diagnosting testing
NO_MODEM_LOOP		EQU		00000000B	; of asynchronous communications element
										; set or reset

; Line Status Register
LSR_DATA_READY		EQU		00000001B	; complete incoming character received and
										; transferred into receiver buffer register
LSR_OVERRUN_ERROR	EQU		00000010B	; character in RBR was overwritten by next
										; one before being read
LSR_PARITY_ERROR	EQU		00000100B	; Parity of received data character doesn't
										; match parity in line control register
LSR_FRAMING_ERROR	EQU		00001000B	; recieved character doesn't have valid
										; stop bit
LSR_BREAK_INT		EQU		00010000B	; Received data input held clear for longer
										; than full word transmission time
LSR_HOLD_REG_EMPT	EQU		00100000B	; Transmitter holding register empty indicator
LSR_TRANS_EMPTY		EQU		01000000B	; Transmitter holding register and transmitter
										; shift register both empty
NON_ERROR_BITS_MASK	EQU		00011110B	; Mask to show values in only bits that signify
										; an actual error and not status

; Modem Status Register
DELTA_CLEAR_TO_SEND		EQU		00000001B	; clear to send input changed, generate
											; modem status interrupt
DELTA_DATA_SET_READY	EQU		00000010B	; data set ready input changed, generate
											; modem status interrupt
TRAILING_EDGE_RING_IND	EQU		00000100B	; ring indicator input changed to high
											; state, generate modem status interrupt
DELTA_DATA_CARR_DETECT	EQU		00001000B	; data carrier detect input changed,
											; generate modem status interrupt
CLEAR_TO_SEND			EQU		00010000B	; active low clear to send input
DATA_SET_READY			EQU		00100000B	; active low data set ready input
RING_INDICATOR			EQU		01000000B	; active low ring indicator input
DATA_CARR_DETECT		EQU		10000000B	; active low data carrier detect input
