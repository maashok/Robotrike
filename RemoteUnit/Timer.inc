;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 Timer.INC                                  ;
;                                   Timers                                   ;
;                                 Include File                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the definitions for the Timers program (Timers.ASM).
;
; Revision History:
;     10/28/16 Maitreyi Ashok   Added necessary definitions for RoboTrike event
;								handling and removed unnecessary definitions
;	  10/29/16 Maitreyi Ashok	Added constant for size of vector
;     11/03/16 Maitreyi Ashok   Isolated timer handling definitions
;     11/09/16 Maitreyi Ashok   Addded timer 0 definitions for motors
;	  11/26/16 Maitreyi Ashok	Moved timer 2 and timer 0 specific deifnitions 
;								to separate files



; Interrupt Controller Definitions

; Addresses
INT_CTRLR_CTRL  EQU     0FF32H          ;address of interrupt controller for timer

; Register Values
INT_CTRLR_CVAL  EQU     00001H          ;set priority for timers to 1 and enable
                                        ;000000000000----  reserved
                                        ;------------0---  enable timer interrupt
                                        ;-------------001  timer priority
TIMER_EOI        EQU     00008H         ;Timer EOI command (same for all timers)