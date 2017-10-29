; Maitreyi Ashok
; Section 1 – Richard Lee

        NAME    QUEUE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                    QUEUE                                   ;
;                               QUEUE Routines	                             ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; This file contains the queue functions for the RoboTrike. These allow the RoboTrike
; to process large bursts of data over a longer period of time, as there are instants
; where the amount of data is more than the RoboTrike can process, but the average amount
; of data is lower than the RoboTrike's capabilities. The queue must be of a size
; that is a power of 2.
;
; Table of Contents:
; QueueInit  - Initializes the queue
; QueueEmpty - Returns whether the queue is empty
; QueueFull  - Returns whether the queue is full
; Dequeue    - Removes and returns value at head of queue
; Enqueue    - Adds value to tail of queue
;
; Revision History:
;     10/15/2016  Maitreyi Ashok      Wrote basic outline for queue routines		
;	  10/17/2016  Maitreyi Ashok   	  Wrote a description for QueueStruc		
;	  10/19/2016  Maitreyi Ashok	  Wrote basic code for functions
;	  10/20/2016  Maitreyi Ashok	  Corrected syntax errors
;	  10/21/2016  Maitreyi Ashok	  Changed method of wrapping head and tail 
;									  pointers, Further debugging of code
;	  10/22/2016  Maitreyi Ashok	  Documented code
;	  11/04/2016  Maitreyi Ashok	  Updated comments and optimized code
;	  11/26/2016  Maitreyi Ashok	  Updated comments and optimized code
;	  12/04/2016  Maitreyi Ashok	  Updated comments

$INCLUDE(QUEUE.inc)
$INCLUDE(General.inc)

CGROUP  GROUP   CODE

CODE	SEGMENT PUBLIC 'CODE'


        ASSUME  CS:CGROUP


; QueueInit			
; Description: 		Initializes a queue of the type QueueStruc at a given address. 
;					The queue structure stores the size of each of the elements. 
;					The number of elements entered by the user is ignored, and a 
;					fixed length queue is used. Thus, if the size entered is a word, 
;					the queue has MAX_ARRAY_SIZE/2 elements, and if the size entered 
;					is a byte, the queue has MAX_ARRAY_SIZE elements. It also sets up 
;					the structure for future use by initializing the head and tail 
;                   pointers to zero as well as the current size. 
;
; Operation:  		The function starts by finding the size of the elements. If the 
;					size entered as an argument is zero, which means each entry is 
;					a byte, 1 is stored in the  sizeElem field. Otherwise, 2 is stored 
;					in the sizeElem field, since each entry is 2 bytes. The length 
;					entered by the user for the queue is ignored, and is set to 
;					MAX_ARRAY_SIZE if the size was determined to be bytes. If it was 
;					determined to be words, then the length for the queue is 
;					MAX_ARRAY_SIZE/2. Since there are no elements in the queue yet, 
;					set the head and tail pointers to zero (offset from the start 
;					of the array). The current size of the queuewill also be zero 
;					until elements are added. 
;
; Arguments:   		address [DS:SI] - address at which the queue is stored
;					length [AX] - maximum number of items that can be stored in the 
;							queue; ignored and fixed length queue is used
;					size [BL] - size of the elements in the queue (0 - byte, nonzero 
;							- word)
; Return Value:		None
;
; Local Variables:	arraySize [CX] - the number of elements that will be in the array
; Shared Variables: None
; Global Variables: None
;
; Input:			None
; Output:			None
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	Structure to hold the information of the queue. Stores number 
;					of elements, size of each element, the current size, head and 
;					tail indices, and a circular array containing all the elements.
;					Circular array containing all the elements in the queue.
;
; Registers Changed: CX, flags
; Stack Depth:		0 words
;
; Limitations:		This function initializes a fixed length array of pre-determined 
;					size which cannot be increased in size depending on later needs. 
;					Also, the elements of the array can only be all bytes or all words.
;
; Author:			Maitreyi Ashok
; Last Modified:	10/16/2016  Wrote functional spec and outline for queue init
;					10/19/2016	Wrote code for initializing queue
;					10/21/2016	Changed procedure to use fixed length queues
;					10/22/2016	Documented code


QueueInit       PROC        NEAR
                PUBLIC      QueueInit

SetSize:
		CMP		BL, 0					; Check if the size entered is zero
		JZ		ByteSize				; If it is, the size is a byte. If not the 
		;JNZ 	WordSize                ; size is a word
WordSize:
		MOV		[SI].sizeElem, BYTES_PER_WORD	; Size of each element is 2 bytes
		MOV		[SI].numElem, MAX_ARRAY_SIZE/2	; Can store 1/2 as many elements in
        JMP     SetStartPos						; queue since size of each twice as big
ByteSize:
		MOV 	[SI].sizeElem, BYTES_PER_BYTE	; Size of each element is 1 byte
		MOV		[SI].numElem, MAX_ARRAY_SIZE	; Can store as many elements as
        ;JMP    SetStartPos						; number of bytes
SetStartPos:
		MOV		[SI].head, 0			; The head, tail and current size pointers 
		MOV		[SI].tail, 0			; are all zero (pointing to the first slot 
		MOV     [SI].currSize, 0		; of the array) until elements are added
        RET
QueueInit	ENDP


; QueueEmpty			
; Description: 		Checks if the queue at the address passed in is empty. This is 
;					when no elements have been stored in the array of elements that 
;					is part of the queue structure, and the currentSize entry is zero. 
;					If the queue is empty, the zero flag is set.
;
; Operation:  		This function checks whether the array stored in the queue 
;					structure is empty, or has zero elements. To do this, the 
;					function first clears the zero flag. Then, if the currSize 
;					field of the structure is zero (meaning	there are no elements 
;					in the queue's list of elems), it sets the ZF flag.
;
; Arguments:   		address [DS:SI] - address at which the queue is stored
; Return Value:		empty [ZF] - whether queue is empty (ZF set) or not (ZF cleared)
;
; Local Variables:	empty [ZF] - stores whether the queue is empty (set) or not (clear)
; Shared Variables: None
; Global Variables: None
;
; Input:			None
; Output:			None
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	Structure to hold the information of the queue. Stores number 
;					of elements, size of each element, the current size, head and 
;					tail indices, and a circular array containing all the elements.
;
; Registers Changed: flags
; Stack Depth:		0 words
;
; Limitations:		None
;
; Author:			Maitreyi Ashok
; Last Modified:	10/16/2016  Wrote functional spec and outline
;					10/19/2016	Wrote code to check if queue empty by checking size
;					10/22/2016  Documented code
;					

QueueEmpty      PROC        NEAR
                PUBLIC      QueueEmpty

CheckSizeEmpty:
		CMP 	[SI].currSize, 0		; Compare current size to zero to see if empty
        RET								; If the current size is zero, sets zero 
										; flag indicating queue is empty
QueueEmpty	ENDP


; QueueFull			
; Description: 		Checks if the queue at the address passed in is full. This is 
;					when the number of elements that have been stored in the array 
;					that is part of the data structure is equal to the total number 
;					of elements that can be stored in the array, and the currentSize 
;					entry equals the numElems entry. If the queue is full, the zero 
;					flag is set.
;
; Operation:  		This function checks whether the array stored in the queue structure
;					is full, or has the maximum number of elements. To do this, the 
;					function first clears the zero flag. Then, if the currSize field 
;					of the structure equal to the numElems field of the structure 
;					(meaning that all the elements in the queue's list of elems 
;					have been filled up), it sets the ZF flag.
;
; Arguments:   		address [DS:SI] - address at which the queue is stored
; Return Value:		full [ZF] - whether queue is full (1) or not (0)
;
; Local Variables:	full [ZF] - stores whether the queue is full (set) or not (clear)
; Shared Variables: None
; Global Variables: None
;
; Input:			None
; Output:			None
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	Structure to hold the information of the queue. Stores number 
;					of elements, size of each element, the current size, head and 
;					tail indices, and a circular array containing all the elements.
;
; Registers Changed: BX, flags
; Stack Depth:		0 words
;
; Limitations:		None
;
; Author:			Maitreyi Ashok
; Last Modified:	10/16/2016  Wrote functional spec and outline 
;					10/19/2016 	Wrote code to check if queue is full by checking size
;					10/21/2016	Changed storage of numElem in different register, 
;								since changes value to enqueue otherwise
;					10/22/2016	Documented code


QueueFull     	PROC        NEAR
                PUBLIC      QueueFull
				
CheckSizeFull:
		MOV 	BX, [SI].numElem		; Compares maximum number of elements in
		CMP		[SI].currSize, BX 		; queue and current size. If equal, every
										; spot in queue is filled by elements
        RET								; If so, zero flag is set. Otherwise, 
										; zero flag cleared

QueueFull	ENDP


; Dequeue			
; Description: 		Dequeues a value from the head of the queue at the address passed 
;					in, in FIFO	order. The element is removed from the queue and 
;					then returned to the user. If the queue is empty when this 
;					function is called, it waits until the queue gets a value to 
;					dequeue, and is thus a blocking function. The element removed	
;					is either a byte or word, depending on whether the queue 
;					was initialized with word or byte size elements.
;
; Operation:  		This function dequeues a 8 bit or 16 bit value from the queue, 
;					depending on which type of value was specified in initialization. 
;					The element is removed from the head, or front of the queue, 
;					depending on how early it was entered into the queue. If the 
;					queue is empty, then the function waits/blocks until the condition 
;					becomes false (the queue has some element), after which it 
;					can dequeue it. To remove the element, the head pointer is 
;					incremented to point to the next element of the queue (if the 
;					queue becomes empty, then it just points to the position where 
;					the next element will be added to, or where the tail pointer 
;					currently is). Thus, the head pointer will wrap around after it 
;					reaches MAX_ARRAY_SIZE in the array back to the front of the array 
;					(but it will never cross the tail pointer, because then we will 
;					be dequeueing with an empty queue). Then, the element is returned 
;					to the user in a register. However, the array position where the 
;					element was at is not cleared since it will just be overwritten 
;					whenever another enqueue operation adds an element to that 
;					position.
;
; Arguments:   		address [DS:SI] - address at which the queue is stored
; Return Value:		elem [AX/AL] - the element removed from the head of the queue
;
; Local Variables:	elem [AX/AL] - element that was at the head of the queue
;					head [BX]	 - index of head pointer of elements in queue
;					numBytes [DX] - maximum number of bytes in the QueueStruc
; Shared Variables: None
; Global Variables: None
;
; Input:			None
; Output:			None
;
; Error Handling:	If the queue is empty when this function is called, the function
;					waits until the queue has any value to be removed and returned, 
;					causing the function to be blocking.
;
; Algorithms:		When an element is removed from the head of the queue, the head 
;					pointer	is incremented so that the head of the queue becomes the 
;					previously second element. If the head pointer is at the end of 
;					the array but the array is not full, restarts the head pointer 
;					at 0 (where the next element of	the queue has been stored)
; Data Structures:	Structure to hold the information of the queue. Stores number 
;					of elements, size of each element, the current size, head and 
;					tail indices, and a circular array containing all the elements.
;
; Registers Changed: AX, BX, flags
; Stack Depth:		0 words
;
; Limitations:		The function blocks if there are no elements in the queue, 
;					which might waste CPU clock cycles. Also, this function can 
;					only return 8 or 16 bit values.	Also, the function assumes the 
;					queue is of size 2^n.
;
; Author:			Maitreyi Ashok
; Last Modified:	10/16/2016  Wrote functional spec and outline for dequeueing
;					10/17/2016  Fixed condition for blocking function if queue empty
;					10/19/2016	Wrote code to dequeue element
;								Changed wrapping of head pointer to use modulus 
;								operation instead of comparisons
;					10/21/2016	Changed wrapping of head pointer to use formula that
;								s mod 2^n = s & (2^n)-1
;					10/22/2016	Documented code


Dequeue		    PROC        NEAR
                PUBLIC      Dequeue
				
BlockIfEmpty:
		CALL 	QueueEmpty					; Check if the queue is empty
		JZ		BlockIfEmpty				; If the queue is empty, just loop around 
		;JNZ	SetUpDequeue				; until an element is added
SetUpDequeue:
		MOV		BX, [SI].head				; Store the head pointer
		
CheckSizeDequeue:
		CMP		[SI].sizeElem, BYTES_PER_BYTE
		JE		DequeueByte					; If the size of elements is 1 byte, 
											; dequeue 1 byte			
		;JNE	DequeueWord					; Otherwise, dequeue 2 bytes
		
DequeueWord:								; If the elements are words
		MOV		AX, WORD PTR [SI].elems[BX]	; Store the earliest added element of 
											; the queue
		ADD 	BX, BYTES_PER_WORD			; Increment head pointer to remove element
DequeueWordWrapAround:
        AND     BX, MAX_ARRAY_SIZE - 1		; numBytes is a power of 2, this will 
											; wrap the head
        MOV     [SI].head, BX				; Store the head pointer, and go update 
		JMP		UpdateSizeDequeue			; the size
DequeueByte:								; If the elements are bytes
		MOV		AL, BYTE PTR [SI].elems[BX]	; Store the earliest added element of 
											; the queue
		ADD 	BX, BYTES_PER_BYTE			; Increment the head pointer to remove 
											; the element
DequeueByteWrapAround:
        AND     BX, MAX_ARRAY_SIZE - 1		; numBytes is a power of 2, this will 
											; wrap the head
        MOV     [SI].head, BX				; Store the head pointer, and go update 
											; the size
		;JMP	UpdateSizeDequeue
		
UpdateSizeDequeue:
		DEC		[SI].currSize				; Decrement the size by 1, since removed 
        RET									; 1 element
		
Dequeue	ENDP


; Enqueue			
; Description: 		Enqueues a specified value to the tail of the queue at the 
;					address passed in, in FIFO	order. This means the element that 
;					was enqueued last will be the last to be removed. If the queue 
;					is full when this function is called (size = MAX_ARRAY_SIZE), 
;					it waits until the queue has dequeued a value to provide space 
;					for this new value, and is thus a blocking function. The element 
;					added is either a byte or word, depending on whether the queue 
;					was intialized with word or byte size elements.
;
; Operation:  		This function enqueues a 8 bit or 16 bit value to the queue, 
;					depending on which type of value was specified in initialization. 
;					The element is added to the tail, or back of the queue. If the 
;					queue is full, then the function waits/blocks until the condition 
;					becomes false (the queue has some empty space), after which it 
;					can use that extra space for this new value to enqueue. To add 
;					an element, the value at the current tail pointer is set 
;					to the specified value. Then, the tail pointer is incremented 
;					to point to the position where the next element will be added 
;					to. (If the queue becomes full, the tail pointer points to the 
;					current head of the queue). Thus, the tail pointer will wrap 
;					around after it reaches the maximum size of the array back 
;					to the front of the array (but it will never cross the head 
;					pointer, because then we will be enqueuing to a full queue). 
;					No value is returned, but the operation is successful if control 
;					is returned to the user (some open space became available for 
;					this element)
;
; Arguments:   		address [DS:SI] - address queue is stored at
; Return Value:		val [AX/AL] - the value to be added to the tail of the queue
;
; Local Variables:	tail [BX] - index of tail pointer of elements in queue
;					numBytes [DX] - maximum number of bytes in the QueueStruc
; Shared Variables: None
; Global Variables: None
;
; Input:			None
; Output:			None
;
; Error Handling:	If the queue is full when this function is called, the function
;					waits until the queue has any space to add another value to the
;					queue, causing the function to be blocking.
;
; Algorithms:		When an element is added to the tail of the queue, the tail 
;					pointer	is incremented (to point to the next available spot 
;					to add an element) If the tail pointer is at the end of the 
;					array but the array is not full, wraps the tail pointer around 
;					to 0 (where the next element of	the queue can be stored)
; Data Structures:	Structure to hold the information of the queue. Stores number 
;					of elements, size of each element, the current size, head and 
;					tail indices, and a circular array containing all the elements.
;
; Registers Changed: BX, flags
; Stack Depth:		0 words
;
; Limitations:		The function blocks if the queue is full, which might waste
;					CPU clock cycles. Also, this function can only add 8 or 16 bit 
;					values to the queue. The function also requires queue size to
;					be a power of 2.
;
; Author:			Maitreyi Ashok
; Last Modified:	10/16/2016  Wrote functional spec and outline for enqueuing
;					10/17/2016  Fixed condition for blocking function if queue is full
;					10/20/2016	Wrote code to enqueue element
;								Changed wrapping of tail pointer to use modulus operation
;								instead of comparisons
;					10/21/2016	Changed wrapping of head pointer to use formula that
;								s mod 2^n = s & (2^n)-1
;					10/22/2016	Documented code



Enqueue		    PROC        NEAR
                PUBLIC      Enqueue

BlockIfFull:
		CALL 	QueueFull					; Checks if the queue is full. If it 
		JZ		BlockIfFull					; loops just loops around until element
		;JNZ	SetUpEnqueue				; is removed and queue has empty slot
SetUpEnqueue:
		MOV		BX, [SI].tail				; Store the tail of the array
CheckSizeEnqueue:
		CMP		[SI].sizeElem, BYTES_PER_BYTE
		JE		EnqueueByte					; If the size of elements is 1 byte, 
		;JNE	EnqueueWord					; enqueue 1 byte. Otherwise, enqueue 2 bytes
EnqueueWord:
		MOV		WORD PTR [SI].elems[BX], AX	; Store the value to enqeuue in the tail slot
		ADD 	BX, BYTES_PER_WORD			; Increment the tail pointer
EnqueueWordWrapAround:
        AND     BX, MAX_ARRAY_SIZE - 1		; numBytes is a power of 2, this will 
											; wrap the tail
        MOV     [SI].tail, BX				; Store the tail pointer, and go update 
		JMP		UpdateSizeEnqueue			; the size
EnqueueByte:
		MOV		BYTE PTR [SI].elems[BX], AL ; Store the value to enqeuue in the 
		ADD 	BX, BYTES_PER_BYTE			; tail slot. Increment the tail pointer
EnqueueByteWrapAround:
        AND     BX, MAX_ARRAY_SIZE - 1		; numBytes is a power of 2, this will 
											; wrap the tail
        MOV     [SI].tail, BX				; Store the tail pointer, and go update 
											; the size
        
		;JMP	UpdateSizeEnqueue
UpdateSizeEnqueue:
        INC     [SI].currSize				; Increment the current size since we 
											; added one element
Check:
        RET
				
Enqueue	ENDP


CODE    ENDS

        END