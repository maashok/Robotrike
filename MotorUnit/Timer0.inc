;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 Timer0.INC                                 ;
;                                   Timers                                   ;
;                                 Include File                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the definitions for the Motor timers program (Mtimer.ASM).
;
; Revision History:
;     11/26/16 Maitreyi Ashok   Separated constants in Timer.inc from Timer 2
;								definitions

; Timer 0 Definitions

; Addresses
TMR_0_CTRL      EQU     0FF56H          ;address of Timer 0 Control Register
TMR_0_MAX_CNT   EQU     0FF52H          ;address of Timer 0 Max Count A Register
TMR_0_COUNT     EQU     0FF50H          ;address of Timer 0 Count Register

; Control Register Values

TMR_0_CTRL_VAL  EQU     0E001H          ;value to write to Timer 0 Control Register
                                        ;1---------------  enable timer
                                        ;-1--------------  write to control
                                        ;--1-------------  enable interrupts
                                        ;---0------------  Maxcount CMP A in use
                                        ;----000000------  reserved
                                        ;----------0-----  Max_Count not reached
                                        ;-----------0----  Enable Counting when low->high
                                        ;------------0---  No prescaling
                                        ;-------------0--  Use internal clock
                                        ;--------------0-  Single maximum mode
                                        ;---------------1  continuous mode

; Interrupt Vectors
										
TMR_0_VEC       EQU     8                ;interrupt vector for Timer 0

; Timing Definitions

MOTORS_COUNT    EQU     576             ;Timers are at 2.304 MHz = 2304 counts/millisecond
                                        ;Want 128 counts per period of 32 ms, so
                                        ;interrupt 4 times as fast, or 2304/4 
                                        ;= 576 counts/32 milliseconds