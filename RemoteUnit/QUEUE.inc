;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   QUEUE.INC                                ;
;                                 Include File                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the symbolic constants for the QUEUE.ASM and hw3Loop.asm programs.
;
; Table of Contents
; 	QueueStruc definition - the queue structure used to hold the elements
;	maxArraySize - max bytes in the queue
;
; Revision History:
;    10/19/16  Maitreyi Ashok              Added struct definition
;	 10/20/16  Maitreyi Ashok			   Added constant to store maxArraySize
;	 10/22/16  Maitreyi Ashok			   Documented elements of the queue
;	 12/04/16  Maitreyi Ashok			   Updated comments

MAX_ARRAY_SIZE    EQU     1024		; Number of bytes that can be stored in the queue

; QueueStruc - The QueueStruc holds information about a queue, with the pointers
; to enqueue and dequeue values, as well as the size and the size and maximum number
; of the elements, as well as all the elements in a circular array format themselves.
; 
; 	numElem - 2 bytes (number of possible elements in queue)
;	sizeElem - 1 byte (length of each element)
;	head - 2 bytes (first element - stores index of array)
; 	tail - 2 bytes (last element - stores index of array)
;	currSize - 2 bytes (number of elements currently in queue)
;	elems - arrary of maxArraySize bytes with queue elements

QueueStruc 		STRUC
	numElem 	DW 	?				; The number of elements that are in the queue
	sizeElem 	DB 	?				; If storing bytes, numElem is maxArraySize, 
									; sizeElem is BYTES_PER_BYTE. If storing words,
									; is BYTES_PER_WORD
	head		DW 	?				; The head points to the first element of the queue
	tail		DW	?				; The tail points to the next free slot of the queue
	currSize	DW 	?				; Current size stored to know whether queue is 
									; full/empty
	elems		DB  MAX_ARRAY_SIZE DUP (?)	
                                    ; Array can store maxArraySize bytes or 
									; maxArraySize/2 words
QueueStruc      ENDS 