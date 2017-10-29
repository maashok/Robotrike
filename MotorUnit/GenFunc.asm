; Maitreyi Ashok
; Section 1 – Richard Lee

        NAME    GenFunc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  GenFunc                                   ;
;                          	   General Routines	                             ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; This file contains a routine to NOP.
;
; Table of Contents:
; doNOP	- performs no action
;
; Revision History:
;     11/19/2016	Maitreyi Ashok	Functional Specification/Pseudocode
;	  12/01/2016	Maitreyi Ashok 	Moved to separate file


CGROUP  GROUP   CODE


CODE	SEGMENT PUBLIC 'CODE'


        ASSUME  CS:CGROUP

; doNOP
; Description: 		This function is used to perform no action when a transition
;                   does not require any action routine to be performed in a state
;					machine or to perform nothing in general for some specific
;					entries in a call table. It does nothing and immediately returns
;
; Operation:  		This function performs no operation and is used when the 
;                   action routine corresponding to a transition or the entry for
;					an entry in a call table is a NOP, so that no operation needs 
;					to be performed in the transition
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	None
; Shared Variables: None
; Global Variables: None
;
; Input:			None
; Output:			None
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	None
;
; Registers Changed: None
; Stack Depth:		0 words
;
; Limitations:		None
;
; Author:			Maitreyi Ashok
; Last Modified:	11/22/16    Maitreyi Ashok  Wrote code
;                   11/24/16    Maitreyi Ashok  Updated comments
;					12/04/16	Maitreyi Ashok	Updated comments to make more
;												general for use in remote unit

doNOP           PROC        NEAR
				PUBLIC		doNOP
        RET             ; Perform no operation and return immediately
doNOP           ENDP


CODE    ENDS

        END