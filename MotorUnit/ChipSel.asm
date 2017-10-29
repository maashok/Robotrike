; Maitreyi Ashok
; Section 1 – Richard Lee

        NAME    ChipSel

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   Chip Select                              ;
;                             Chip Select Init Routines	                     ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; The function in this file allows for access of I/O and memory space by initializing
; the chip selects
;
; Table of Contents:
; InitCS - Intializes the chip select logic
;
; Revision History:
; 10/24/2016	Maitreyi Ashok	Wrote basic outlines and functional specifications
;								for all functions.		
; 10/28/2016    Maitreyi Ashok  Changed the timer event handler to push/pop all
;								registers
; 11/02/2016	Maitreyi Ashok	Added calling keypad scanning function in event
;								handler
; 11/03/2016    Maitreyi Ashok  Moved chip select initialization to separate file.


CGROUP  GROUP   CODE

$INCLUDE(chipSel.inc)

CODE	SEGMENT PUBLIC 'CODE'


        ASSUME  CS:CGROUP     
; InitCS
;
; Description:       Initialize the Peripheral Chip Selects on the 80188.
;
; Operation:         Write the initial values to the PACS and MPCS registers.
;
; Arguments:         None.
; Return Value:      None.
;
; Local Variables:   None.
; Shared Variables:  None.
; Global Variables:  None.
;
; Input:             None.
; Output:            None.
;
; Error Handling:    None.
;
; Algorithms:        None.
; Data Structures:   None.
;
; Registers Changed: AX, DX
; Stack Depth:       0 words
;
; Author:            Glen George, Maitreyi Ashok
; Last Modified:     Oct. 29, 1997
;                    11/03/2016     Maitreyi Ashok   Updated comments

InitCS  PROC    NEAR
                PUBLIC      InitCS        

        MOV     DX, PACS_REG    ;setup to write to PACS register
        MOV     AX, PACS_VAL
        OUT     DX, AL          ;write PACSval to PACS

        MOV     DX, MPCS_REG    ;setup to write to MPCS register
        MOV     AX, MPCS_VAL
        OUT     DX, AL          ;write MPCSval to MPCS


        RET                     ;done so return

InitCS  ENDP

CODE    ENDS

        END