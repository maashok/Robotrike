;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  MOTORS.INC                                ;
;                                 Include File                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the symbolic constants for the MOTORS.ASM program.
;
; Revision History:
;    11/07/16   Maitreyi Ashok      Defined basic constants
;    11/09/16   Maitreyi Ashok      Included constants in include file
; 	 11/12/16	Maitreyi Ashok		Updated comments

MIN_SPEED         EQU  0        ; minimum speed setting for RoboTrike
MAX_SPEED         EQU  65534	; maximum speed setting for RoboTrike
SPEED_NO_CHANGE   EQU  65535 	; if this value of speed is passed in, doesn't change speed
NEG_TO_POS_SPEED  EQU  1		; Shift value of speed right by 1 to make all
								; speeds positive
								
MIN_ANGLE         EQU  0		; minimum direction setting for RoboTrike
MAX_ANGLE         EQU  360		; maximum direction setting for RoboTrike
ANGLE_NO_CHANGE   EQU  -32768	; if this value of angle is passed in, doesn't 
								; change direction

NUM_DIRECTIONS    EQU  2       	; Each motor has x and y components to movement
MAX_PWM_COUNTER   EQU  127 	   	; Duty cycle is 128 counts, or 0 -> 127, for 
								; END_PRECISION bits of precision
OFF_PULSE_WIDTH	  EQU  0		; Pulse width reset to zero so motor will always
								; be turned off
NUM_DC_MOTORS     EQU  3 		; 3 DC motors in RoboTrike, one for each wheel
				
PARALLEL_PORT_B   EQU  PARALLEL_PORT_START_ADDR + PARA_PORT_B_OFFSET
									; address of port B of 8255
LASER_ON	      EQU  10000000B	; Bits for turning laser on or off in parallel
LASER_OFF         EQU  00000000B	; port B

NUM_EXTRA_SIGN_BITS EQU 2 		; Number of extra sign bits when multiplying 3 
								; Q0.15 signed numbers together
SIGN_BITS_NEEDED  EQU  1		; Number of sign bits needed for final value
END_PRECISION     EQU  7		; Number of bits of precision desired at end to
								; to control RoboTrike speed
EXTRA_PRECISION   EQU  BITS_PER_WORD - SIGN_BITS_NEEDED - END_PRECISION
								; Extra precision to get rid of to get END_PRECISON
								; bits of precision plus one sign bit
FORCE_MOTOR_SHIFT EQU  2		; Shift motor number by 2 (multiply by 4) to access
								; the force vector for each motor (x and y components
								; for each force, each 2 bytes)