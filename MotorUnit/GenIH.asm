; Maitreyi Ashok
; Section 1 – Richard Lee

        NAME    GenIH

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                               Interrupt Handling                           ;
;                      General Interrupt Handler Routines	                 ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; The functions in this file set up general event handling by setting up the 
; interrupt vector table and setting up an illegal event handler
;
; Table of Contents:
; ClrIRQvectors - Sets up the interrupt vector table
; IllegalEventHandler - Handles illegal events (not timer interrupts)
;
; Revision History:
; 10/24/2016	Maitreyi Ashok	Wrote basic outlines and functional specifications
;								for all functions.		
; 10/28/2016    Maitreyi Ashok  Changed the timer event handler to push/pop all
;								registers
; 11/02/2016	Maitreyi Ashok	Added calling keypad scanning function in event
;								handler
; 11/03/2016    Maitreyi Ashok  Moved ClrIRQvectors and IllegalEventHandler to
;                               separate file


CGROUP  GROUP   CODE

$INCLUDE(IntH.inc)

CODE	SEGMENT PUBLIC 'CODE'


        ASSUME  CS:CGROUP   

; ClrIRQVectors
;
; Description:      This functions installs the IllegalEventHandler for all
;                   interrupt vectors in the interrupt vector table.  Note
;                   that all 256 vectors are initialized so the code must be
;                   located above 400H.  The initialization skips  (does not
;                   initialize vectors) from vectors FIRST_RESERVED_VEC to
;                   LAST_RESERVED_VEC.
;
; Arguments:        None.
; Return Value:     None.
;
; Local Variables:  CX    - vector counter.
;                   ES:SI - pointer to vector table.
; Shared Variables: None.
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
;
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Used:   flags, AX, CX, SI, ES
; Stack Depth:      0 words
;
; Author:           Glen George, Maitreyi Ashok
; Last Modified:    Feb. 8, 2002
;                   11/03/2016      Maitreyi Ashok      Updated comments

ClrIRQVectors   PROC    NEAR
                PUBLIC      ClrIRQVectors


InitClrVectorLoop:              ;setup to store the same handler 256 times

        XOR     AX, AX          ;clear ES (interrupt vectors are in segment 0)
        MOV     ES, AX
        MOV     SI, 0           ;initialize SI to start at beginning of vector table

        MOV     CX, 256         ;up to 256 vectors to initialize


ClrVectorLoop:                  ;loop clearing each vector
                                ;check if should store the vector
        CMP     SI, VECTOR_SIZE * FIRST_RESERVED_VEC
        JB	    DoStore		        ;if before start of reserved field - store it
        CMP	    SI, VECTOR_SIZE * LAST_RESERVED_VEC
        JBE	    DoneStore	        ;if in the reserved vectors - don't store it
        ;JA	DoStore		        ;otherwise past them - so do the store

DoStore:                        ;store the vector
        MOV     ES: WORD PTR [SI], OFFSET(IllegalEventHandler)
        MOV     ES: WORD PTR [SI + HALF_VECTOR_SIZE], SEG(IllegalEventHandler)

DoneStore:			            ;done storing the vector
        ADD     SI, VECTOR_SIZE ;update pointer to next vector

        LOOP    ClrVectorLoop   ;loop until have cleared all vectors
        ;JMP    EndClrIRQVectors;and all done


EndClrIRQVectors:               ;all done, return
        RET


ClrIRQVectors   ENDP


; IllegalEventHandler
;
; Description:       This procedure is the event handler for illegal
;                    (uninitialized) interrupts.  It does nothing - it just
;                    returns after sending a non-specific EOI.
;
; Operation:         Send a non-specific EOI and return.
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
; Registers Changed: None
; Stack Depth:       2 words
;
; Author:            Glen George, Maitreyi Ashok
; Last Modified:     Dec. 25, 2000
;                    11/03/2016     Maitreyi Ashok      Updated comments

IllegalEventHandler     PROC    NEAR

        NOP                             ;do nothing (can set breakpoint here)

        PUSH    AX                      ;save the registers
        PUSH    DX

        MOV     DX, INT_CTRLR_EOI       ; send a EOI to the
        MOV     AX, NON_SPEC_EOI        ; interrupt controller to clear out
        OUT     DX, AL                  ; the interrupt that got us here

        POP     DX                      ;restore the registers
        POP     AX

        IRET                            ;and return


IllegalEventHandler     ENDP


CODE    ENDS

        END