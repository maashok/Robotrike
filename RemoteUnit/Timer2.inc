;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 Timer2.INC                                 ;
;                                   Timers                                   ;
;                                 Include File                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the definitions for the Keypad and Display Timer program 
; (KDTimer.asm)
;
; Revision History:
;     11/26/16 Maitreyi Ashok   Separated timer 2 definitions from main timers file


; Timer 2 Definitions

; Addresses

TMR_2_CTRL      EQU     0FF66H          ;address of Timer 2 Control Register
TMR_2_MAX_CNT   EQU     0FF62H          ;address of Timer 2 Max Count A Register
TMR_2_COUNT     EQU     0FF60H          ;address of Timer 2 Count Register




; Control Register Values

TMR_2_CTRL_VAL  EQU     0E001H          ;value to write to Timer 2 Control Register
                                        ;1---------------  enable timer
                                        ;-1--------------  write to control
                                        ;--1-------------  no interrupts
                                        ;----000000-0000-  reserved
                                        ;---0------0-----  read only
                                        ;---------------1  continuous mode
                                        


; Interrupt Vectors
TMR_2_VEC       EQU     19               ;interrupt vector for Timer 2

; Timing Definitions

ONE_MS_COUNT    EQU     2304            ;Timers get 1/8 of 18.432 MHz clock = 2.304 MHz
                                        ;2.304 MHz = 2304 counts per millisecond 
                                        ;Want period of 1 ms, so 2304 counts

