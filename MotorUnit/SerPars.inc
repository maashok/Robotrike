;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 SerParse.INC                               ;
;                                 Include File                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the symbolic constants for the SerPars.ASM program.
;
; Revision History:
;    11/23/16  Maitreyi Ashok              Initial Revision
;    11/24/16  Maitreyi Ashok              Updated comments
;	 12/08/16  Maitreyi Ashok			   Added status definitions

; Sending Back to Remote Unit
SPEED_STATUS        EQU     'S'         ; RoboTrike speed has been changed
DIRECTION_STATUS    EQU     'D'         ; Direction of travel has been changed

; Serial Parsing Values
NO_ERROR            EQU     0           ; Error value for no error in action routine
GENERAL_ERROR       EQU     1           ; Error value for an invalid character
OVERFLOW_ERROR      EQU     1           ; Error value for overflow in AddDigit
ELEVATION_ERROR     EQU     2           ; Error value for out of bounds value
                                        ; for turret elevation
ABS_SPEED_ERROR     EQU     3           ; Error value for negative/out of bounds
                                        ; value for absolute speed
TOKEN_MASK          EQU     01111111B   ; mask high bit of token (ASCII character)
GOT_SIGN            EQU     1           ; Received sign character
GOT_NO_SIGN         EQU     0           ; Did not receive sign character
IMPLIED_SIGN        EQU     +1          ; Implied sign value - positive
DEC_BASE            EQU     10          ; Base of decimal system to shift by
                                        ; for each digit
MAX_ELEVATION       EQU     60          ; Maximum turret elevation
MIN_ELEVATION       EQU     -60         ; Minimum turret elevation

; State Definitions
ST_IDLE             EQU     0           ; Idle state, waiting for input
ST_ABS_SPEED        EQU     1           ; Have absolute speed command character
ST_ABS_SPEED_SIGN   EQU     2           ; Have a sign when setting absolute speed
ST_ABS_SPEED_NUM    EQU     3           ; Have a digit when setting absolute speed
ST_REL_SPEED        EQU     4           ; Have relative speed command character
ST_REL_SPEED_SIGN   EQU     5           ; Have a sign when setting relative speed
ST_REL_SPEED_NUM    EQU     6           ; Have a digit when setting relative speed
ST_DIRECTION        EQU     7           ; Have a direction command character
ST_DIRECTION_SIGN   EQU     8           ; Have a sign when setting direction
ST_DIRECTION_NUM    EQU     9           ; Have a digit when setting direction
ST_ROT_TURR         EQU     10          ; Have a turret rotation command
ST_ROT_TURR_SIGN    EQU     11          ; Have a sign when rotating turret
ST_ROT_TURR_NUM     EQU     12          ; Have a digit when rotating turret
ST_ELEV_TURR        EQU     13          ; Have a turret elevation command
ST_ELEV_TURR_SIGN   EQU     14          ; Have a sign when elevating turret
ST_ELEV_TURR_NUM    EQU     15          ; Have a digit when elevating turret
ST_LASER_FIRE       EQU     16          ; Have a fire laser command
ST_LASER_OFF        EQU     17          ; Have a laser off command
ST_ERROR            EQU     18          ; Got an error of some kind

NUM_STATES          EQU     19          ; Number of states

; Token Type Definitions
TOKEN_DIGIT         EQU     0           ; token is a digit: 0 to 9
TOKEN_POS           EQU     1           ; token is a positive sign: +
TOKEN_NEG           EQU     2           ; token is a negative sign: -
TOKEN_EOC           EQU     3           ; token is end of command: <return>
TOKEN_ABS           EQU     4           ; token is absolute speed: S or s
TOKEN_REL           EQU     5           ; token is relative speed: V or v
TOKEN_DIR           EQU     6           ; token is direction: D or d
TOKEN_ROT           EQU     7           ; token is turret rotation: T or t
TOKEN_ELEV          EQU     8           ; token is turret elevation: E or e
TOKEN_FIRE          EQU     9           ; token is fire laser: F or f
TOKEN_OFF           EQU     10          ; token is laser off: O or o
TOKEN_IGNORE        EQU     11          ; token is ignore character: ' ', '', '\n'
TOKEN_OTHER         EQU     12          ; anything else 
    
NUM_TOKEN_TYPES     EQU     13          ; number of token types
