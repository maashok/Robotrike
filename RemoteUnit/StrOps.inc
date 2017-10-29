;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  StrOps.INC                                ;
;                                 Include File                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the symbolic constants for the string routines.
;
; Revision History:
;    12/02/16	Maitreyi Ashok	Initial Revision
;	 12/04/16	Maitreyi Ashok	Added FSM definitions and comments

; Remote parser definitions
TOKEN_MASK          EQU     01111111B   ; mask high bit of token (ASCII character)

; Token types
TOKEN_EOC       EQU     0		; Token is end of command: <return>
TOKEN_IGNORE    EQU     1		; Token is ignore character: ' ', '', '\n'
TOKEN_DIGIT     EQU     2		; Token is a digit: 0 to 9
TOKEN_DIR       EQU     3		; Token is direction status: D or d
TOKEN_MOTO      EQU     4		; Token is motor error: M or m
TOKEN_SPEED     EQU     5		; Token is speed status: S or s
TOKEN_PARS      EQU     6		; Token is parsing error: P or p
TOKEN_OTHER     EQU     7		; Anything else: error

NUM_TOKEN_TYPES EQU     8		; Number of token types

; String parsing states
ST_IDLE         EQU     0		; Idle state, waiting for input
ST_ERROR        EQU     1		; Got an error of some kind
ST_SPEED        EQU     2		; Got speed status character
ST_DIR          EQU     3		; Got direction status character
ST_MOTO         EQU     4		; Got motor error character
ST_PARS         EQU     5		; Got parsing error character

NUM_STATES      EQU     6       ; Number of states

; Status buffer definitions
STAT_BUFF_SIZE  EQU     29		; Fixed size of status buffer
SPEED_START     EQU     9 		; Index of start of speed digits in status buffer
SPEED_END       EQU     13		; Index of end of speed digits in status buffer
DIRECTION_START EQU     25		; Index of start of direction digits in status
								; buffer
DIRECTION_END   EQU     27		; Index of end of direction digits in status buffer
PARSING_INDEX   EQU     0		; Index of parsing error display in status buffer

; Type of each entry in state table containing next state to enter and action
; to perform during transition
TRANSITION_ENTRY        STRUC           ;structure used to define table
    NEXTSTATE   DB      ?               ;the next state for the transition
    ACTION      DW      ?               ;action for the transition
TRANSITION_ENTRY      ENDS
