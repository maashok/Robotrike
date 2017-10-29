;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 SerialIO.INC                               ;
;                                  Serial I/O                                ;
;                                 Include File                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the definitions for the Serial I/O routines (SerialIO.ASM).
;
; Revision History:
;	  11/16/16		Maitreyi Ashok		Initial revision
;	  11/17/16		Maitreyi Ashok		Added baud rate index
;	  11/18/16		Maitreyi Ashok		Updated comments


KICKSTART_NEEDED        EQU 1			; whether transmitter empty interrupt 
KICKSTART_NOT_NEEDED    EQU 0			; needs to be kickstarted after the register
										; expected a value but didn't get one due
										; to transmit queue being empty

SERIAL_BASE             EQU 100H		; Address of serial chip on board

DEFAULT_BAUD_RATE       EQU 9			; Starting baud rate index used by 
										; initialization function
NUM_BAUD_RATES			EQU	12			; Number of baud rate settings - table 
										; indexed 0 to NUM_BAUD_RATES - 1
DEFAULT_PARITY			EQU 0			; Starting parity used by initialization
										; function
MAX_PARITY_INDEX		EQU 4			; Last index of parity table, maximum
										; index passed into SetSerialParity
BYTE_ELEM_SIZE    EQU   0				; Byte size argument used for QueueInit
										; function - signifies that all elements
										; of TxQueue will be 1 byte
