; Maitreyi Ashok
; Section 1 – Richard Lee

        NAME    EvQueue

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   EvQueue                                  ;
;                              Event Queue Routines	                         ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; This file contains the routines to set up and enqueue/dequeue from the event
; queue for both the remote and motor boards.
;
; Table of Contents:
; Global
; EnqueueEvent - Enqueues an event to the queue
; DequeueEvent - Dequeues an event from the queue
; InitEventQueue - Initializes an event queue
; CheckSystemFailure - Returns whether system failure has occurred
;
; Local
; SetSystemFailure - Sets that system failure has occurred
;
; Revision History:
;     11/28/2016	Maitreyi Ashok	Functional specifications & pseudocode
;	  12/02/2016	Maitreyi Ashok	Wrote Code
;	  12/03/2016	Maitreyi Ashok	Updated to handle interrupts during routines
;	  12/04/2016	Maitreyi Ashok	Updated commands


CGROUP  GROUP   CODE
DGROUP  GROUP   DATA

$INCLUDE(General.inc)		; Include general definitions for logical values
$INCLUDE(EvQueue.inc)		; Include event queue definitions
$INCLUDE(KEYCODES.inc)		; Include keycode definitions
$INCLUDE(Queue.inc)			; Include general queue definitions
$INCLUDE(simpmac.inc)		; Include critical code macro

CODE	SEGMENT PUBLIC 'CODE'


        ASSUME  CS:CGROUP, DS:DGROUP
		
		EXTRN	Enqueue:NEAR		; General queue routines to enqueue
		EXTRN	Dequeue:NEAR		; and dequeue elements
		EXTRN	QueueFull:NEAR		; General queue routines to check whether
		EXTRN	QueueEmpty:NEAR		; queue is full or empty as well
		EXTRN	QueueInit:NEAR		; as to initialize it
;
; EnqueueEvent			
; Description: 		This function enqueues a passed event into the event queue.
;					This is done by storing both the event type as well as the 
;					value for the event in a word sized queue. This way, when
;					there are large bursts of incoming data, they can all be 
;					enqueued to the event queue and processed later with the
;					context of what the event is from and the exact event itself
;					when there is less data coming in. If the queue is already full
;					when there is another event coming in, the system goes into
;					a critical failure mode and resets the system, thus losing all
;					events enqueued thus far.
;
; Operation:  		This function first checks whether the event queue is already
;					full. If so, there is no more space to store events and the
;					rate of incoming data is clearly greater than the rate it
;					can be processed at. Thus, the system will be set to critical
;					failure mode and reset. If there is still space in the queue,
;					the key value and code of the event passed in is enqueued
;					to this system's EventQueue, where it can later be processed.
;					The key code contains information about the type of event occurring
;					and the key value contains the value pertaining to the event.
;
; Arguments:   		keyEvent [AH] - type of event that has occurred
;					keyValue [AL] - value pertaining to event that happened
; Return Value:		None
;
; Local Variables:	None
; Shared Variables: EventQueue (W) - queue of events to process
; Global Variables: None
;
; Input:			Either keypad or serial event has occurred
; Output:			None
;
; Error Handling:	If the queue is full when the enqueue is attempted, instead of
;					blocking in this function and preventing other functionality
;					from occurring, a critical failure flag is set and this will
;					cause the system to be reset in the next iteration of the main
;					loop.
;
; Algorithms:		None
; Data Structures:	A queue of type QueueStruc containing events to handle
;
; Registers Changed: SI, BX (By Enqueue & QueueFull)
; Stack Depth:		3 words
;
; Limitations:		This function does not give different priority to different
;					types of events depending on the order they should be handled
;					in.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/29/16	Maitreyi Ashok	Pseudocode/functional specification
;					12/02/16	Maitreyi Ashok	Wrote Code
;					12/03/16	Maitreyi Ashok	Added critical code macros
;					12/04/16	Maitreyi Ashok	Updated comments


EnqueueEvent    PROC        NEAR
                PUBLIC      EnqueueEvent
CheckIfQueueFull:
        %CRITICAL_START				; Prevent other interrupts from happening to avoid
									; any critical code in event queue handling being
									; interrupted
		LEA		SI, EventQueue		; Get address of event queue and find out whether
		CALL	QueueFull			; it is full
		JZ		EventQueueFull		; Handle the queue accordingly based on whether
		;JNZ	EventQueueNotFull	; the queue is full or not
EventQueueNotFull:
		CALL	Enqueue				; If the event queue is not full then enqueue
        JMP     DoneEnqueueEvent	; an event
EventQueueFull:						; If the event queue is full then set a critical
		CALL	SetCriticalFailure	; failure since the queue has been filled up
									; faster than it can be processed
DoneEnqueueEvent:					; Once done enqueuing an event allow other 
        %CRITICAL_END				; interrupts to happen again to register and 
		RET							; process other events
		
EnqueueEvent	ENDP

; DequeueEvent		
; Description: 		This function removes an event from the event queue to be
;					processed. It retrieves both the event type and specific 
;					value pertaining to the event from the queue. This will be
;					done during lulls in the large bursts of data so that events
;					that happened previously will be taken care of. If the queue
;					is empty when an attempt is made to remove an element to
;					process, the function returns an empty NOP_EVENT element.
;
; Operation:  		This function first checks whether the event queue is already
;					empty. If so, there are no more events to handle so nothing
;					is done to the queue and an empty event is returned to the
;					caller. However, if there are still events in the queue,
;					the earliest stored key value and code of an event is dequeued
;					from this system's EventQueue, to be processed. The key code 
;					contains information about how to generally handle this event
;					and the key value contains the exact value to handle pertaining 
;					to the event.
;
; Arguments:   		None
; Return Value:		keyEvent [AH] - type of event to process
;					keyValue [AL] - value pertaining to event to process
;					If no events left in queue, keyEvent = NOP_EVENT
;												keyValue is ignored
;
; Local Variables:	eventVal [AX] - value of event to return
; Shared Variables: EventQueue (R/W)- queue of events to process
; Global Variables: None
;
; Input:			None
; Output:			Display event depending on specific type of event.
;
; Error Handling:	If the Event Queue is already empty, return an EmptyEvent
;					element to the caller to process accordingly.
;
; Algorithms:		None
; Data Structures:	A queue of type QueueStruc containing events to handle
;
; Registers Changed: SI, AH
; Stack Depth:		3 words
;
; Limitations:		This function does not give different priority to different
;					types of events depending on the order they should be handled
;					in.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/28/16	Maitreyi Ashok	Functional specification/pseudocode
;					12/02/16	Maitreyi Ashok	Wrote Code
;					12/03/16	Maitreyi Ashok	Added critical code macros
;					12/04/16	Maitreyi Ashok	Updated comments

DequeueEvent    PROC        NEAR
                PUBLIC      DequeueEvent
CheckIfQueueEmpty:
        %CRITICAL_START				; Prevent other interrupts from happening to avoid
									; any critical code in event queue handling being
									; interrupted
		LEA		SI, EventQueue		; Get address of event queue and find out whether
		CALL	QueueEmpty			; it is empty
		JZ		EventQueueEmpty		; Handle the queue accordingly based on whether
		;JNZ	EventQueueNotEmpty	; if is empty or not
EventQueueNotEmpty:
		CALL	Dequeue				; If the event queue is not empty then dequeue
        JMP     DoneEnqueueEvent	; an event
EventQueueEmpty:
		MOV		AH, NOP_EVENT		; If the event queue is empty return a NOP_EVENT
									; which will signal to the main loop that there
									; is no event to process
        ;JMP    DoneDequeueEvent	
DoneDequeueEvent:					; Once done dequeuing an event allow other interrupts
        %CRITICAL_END				; to happen to register and process other events
		RET
DequeueEvent	ENDP

; InitEventQueue			
; Description: 		This function initializes the EventQueue of type QueueStruc.
;					The event queue contains word size elements so that the
;					high byte contains the event code and the low byte contains
;					the event value. The size of the event queue is MAX_ARRAY_SIZE
;					and has a fixed length. The event queue is also stored at a
;					fixed memory address defined in the data segment of this file.
;					This function also resets the criticalFailure flag since there
;					can not be any critical failure when the queue has not been
;					used yet and is empty.
;
; Operation:  		This function initializes an EventQueue to store information
;					about events to process as time permits later. This EventQueue
;					is of type QueueStruc. It is a word sized queue to hold both
;					event code and event value in the same elements of the queue
;					so that they will be associated with each other. In addition,
;					the queue is set to the size MAX_ARRAY_SIZE, which is large
;					enough to avoid filling up the queue during normal usage. Also
;					the function starts the criticalFailure flag so that there
;					is not critical failure when the queue is first empty.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	None
; Shared Variables: EventQueue (W) - queue of events to process
;					criticalFailure (W) - whether queue was full when enqueue attempted
;							causing system to be reset later
; Global Variables: None
;
; Input:			None
; Output:			None
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	A queue of type QueueStruc containing events to handle
;
; Registers Changed: SI, AX, BX
; Stack Depth:		1 word
;
; Limitations:		This function does not create queues that would give different 
;					priority to different types of events depending on the order 
;					they should be handled in.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/29/16	Maitreyi Ashok	Functional specification/pseudocode
;					12/02/16	Maitreyi Ashok	Wrote Code
;					12/04/16	Maitreyi Ashok	Updated comments

InitEventQueue  PROC        NEAR
                PUBLIC      InitEventQueue
SetUpForInit:
		MOV		SI, OFFSET(EventQueue)		; Get the address of the event queue
		MOV		AX, MAX_ARRAY_SIZE			; as well as the maximum size of the 
											; queue to set as the queue size and
		MOV		BX, WORD_SIZE_ARG			; the argument for word size events
CallInitFunctionAndReset:
		CALL	QueueInit					; Initialize the queue with these arguments
		MOV		criticalFailure, NO_FAILURE
											; Start out with no critical failures
		RET
InitEventQueue	ENDP

; SetCriticalFailure			
; Description: 		This function sets that a critical system failure has occurred
;					due to the event queue being full when an enqueue was attempted.
;					This signifies that the rate of incoming data exceeds the rate
;					of data processing, so the queue of events to handle is building
;					up at an unsustainable rate. To take care of this issue,
;					the system the oversized queue is for (motor/remote) is reset,
;					reinitializing the EventQueue such that it is empty and there
;					are no pending events to process anymore.
;
; Operation:  		This function sets the critical failure flag when an enqueue
;					is attempted on an already full queue. Thus, this function
;					is called by the EnqueueEvent function in this case to set
;					the flag that will be checked in every iteration of the main
;					loop to be handled with high priority if there is a critical
;					failure. The failure is handled by resetting the system and
;					the EventQueue.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	None
; Shared Variables: criticalFailure (W) - whether queue was full when enqueue attempted
;							causing system to be reset later
; Global Variables: None
;
; Input:			More events are happening than are processed.
; Output:			System is reset later when the flag is checked
;
; Error Handling:	If the EventQueue is already full when an enqueue is attempted
;					this function is called to set a critical failure flag and
;					reset the system to get rid of this issue.
;
; Algorithms:		None
; Data Structures:	None
;
; Registers Changed: BL
; Stack Depth:		0 words
;
; Limitations:		This function does not account for unimportant events
;					that can be ignored and just not enqueued to a full queue
;					rather than resetting the entire system for such an event.
;					For example, this could be a status update that does not
;					affect the functioning of the system and only changes the UI.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/28/16	Maitreyi Ashok	Functional specification/pseudocode
;					12/02/16	Maitreyi Ashok	Wrote Code
;					12/04/16	Maitreyi Ashok	Updated comments

SetCriticalFailure	PROC        NEAR

SetFailureValue:
		MOV		criticalFailure, FAILURE	; Set critical failure flag at failure
											; to be checked later by main loop
		RET
SetCriticalFailure	ENDP

; CheckCriticalFailure			
; Description: 		This function checks whether a critical failure error has
;					occurred due to the EventQueue filling up faster than it is
;					emptied such that an Enqueue is attempted when the queue
;					is already full. If this is the case, the set flag is returned
;					to the caller, which can then reset the system by reinitializing
;					all components including the EventQueue. This way, the queue
;					will be empty again and events can begin being processed from
;					the start again. In addition, this function does not reset the 
;					critical failure flag as well, since it is assumed that this system
;					failure has high priority in the main loop and will be handled
;					immediately after it is checked if there is in fact a critical
;					failure (and thus call InitEventQueue again).
;
; Operation:  		This function returns the value of the criticalFailure flag to
;					the caller and resets the criticalFailure flag. This allows
;					the caller to decide how to handle this critical error (in
;					this case, reset the entire system including the EventQueue).
;
; Arguments:   		None
; Return Value:		criticalFailure [AL] - returns whether a critical failure with the
;							queue filling up has occurred. FAILURE if critical failure
;							NO_FAILURE if no critical failure.
;
; Local Variables:	None
; Shared Variables: criticalFailure (R/W) - whether queue was full when enqueue attempted
;							causing system to be reset later
; Global Variables: None
;
; Input:			None
; Output:			If criticalFailure is set to FAILURE, system will be reset.
;
; Error Handling:	If the EventQueue is already full when an enqueue is attempted
;					this function returns a set critical failure flag to signal to
;					reset the system to get rid of this issue.
;
; Algorithms:		None
; Data Structures:	None
;
; Registers Changed: AL
; Stack Depth:		0 words
;
; Limitations:		This function does not ensure that the system is reset instantly
;					and instead assumes that it will be, thus waiting to reset the
;					criticalFailure flag when initializing the new queue.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/29/16	Maitreyi Ashok	Functional specification/pseudocode
;					12/02/16	Maitreyi Ashok	Wrote Code
;					12/04/16	Maitreyi Ashok	Updated comments
; 

CheckCriticalFailure	PROC        NEAR
						PUBLIC      CheckCriticalFailure
CheckFailureValue:
		MOV		AL, criticalFailure		; Return value of critical failure flag
		RET								; for main loop to know whether critical
										; error occurred
CheckCriticalFailure	ENDP

CODE    ENDS

;the data segment
DATA    SEGMENT PUBLIC  'DATA'
criticalFailure		DB		?		; Whether critical failure has occurred due
									; to EventQueue filling up. If occurred, value
									; is FAILURE. Otherwise value is NO_FAILURE
EventQueue			QueueStruc	<>	; EventQueue of type QueueStruc that holds
									; events of various types for the RoboTrike to 
									; be filled during bursts of data and 
									; processed during lulls in data transmission
									; Contains serial errors, data received from
									; other unit, as well as keypad events in
									; remote unit


DATA    ENDS

        END