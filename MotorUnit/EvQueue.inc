;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  EvQueue.INC                               ;
;                                 Include File                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the symbolic constants for the event queue.
;
; Revision History:
;    12/02/16	Maitreyi Ashok		Initial Revision
;	 12/04/16	Maitreyi Ashok		Updated comments


FAILURE			EQU		1			; Value of criticalFailure flag if the event
									; queue was full when an enqueue was attempted
NO_FAILURE		EQU 	0			; Value of criticalFailure flag if the event
									; queue has not been full when an enqueue was
									; attempted since the last reset of the system
WORD_SIZE_ARG	EQU		TRUE		; Argument used to QueueInit to specify that
									; the elements of the EventQueue should be
									; words (to hold event code in high byte and
									; event value in low byte)