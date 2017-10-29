; Maitreyi Ashok
; Section 1 – Richard Lee

        NAME    Motors

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                    Motors                                  ;
;                                Motor Routines	                             ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; This file contains the motor routines for controlling the speed and direction
; of the motor as well as the laser.
;
; Table of Contents:
; SetMotorSpeed - sets the speeds and directions of the motors based on
;				  the given arguments
; GetMotorSpeed - returns the current speed setting for RoboTrike
; GetMotorDirection - returns current direction of movement for RoboTrikie
; SetLaser - turns the laser on the RoboTrike on or off
; GetLaser - return status for RoboTrike laser
; PulseWidthModulator - use PWM to control whether motors are on or off
; MotorsEventHandler - Use PWM to control DC motors, and control laser
; InitMotors - Initialize shared variables for use by motors
; 
; Tables
; ForceVectorTable - x and y components of force vectors of motors
; MotorOnOff - Motor on bits to set for each motor
; MotorForward - Motor forward bits to set/clear for each motor
; MotorBackward - Motor reverse bits to set/clear for each motor

; Revision History:
;    11/06/2016		Maitreyi Ashok		Wrote pseudocode/functional specification
; 	 11/07/2016		Maitreyi Ashok		Updated pseudocode, added event handlers
;	 11/09/2016		Maitreyi Ashok		Wrote basic code for mutators/acccessors
;	 11/10/2016		Maitreyi Ashok		Wrote event handler code
;	 11/11/2016		Maitreyi Ashok		Debugged code
;	 11/12/2016		Maitreyi Ashok		Updated comments


CGROUP  GROUP   CODE
DGROUP  GROUP   DATA

$INCLUDE(General.inc)		; Contains data size definitions
$INCLUDE(InitPara.inc)		; Contains parallel port definitions
$INCLUDE(Motors.inc)		; Contains definitions for motor speed/direction
$INCLUDE(Timer.inc)			; Contains definitions for EOI
$INCLUDE(IntH.inc)

CODE	SEGMENT PUBLIC 'CODE'


        ASSUME  CS:CGROUP, DS:DGROUP
		EXTRN	Sin_Table:WORD		; Sine Q0.15 values of integer angles in one rotation
		EXTRN	Cos_Table:WORD		; Cosine Q0.15 values of integer angles in one rotation
;
; SetMotorSpeed			
; Description: 		Sets the speed of the motors based on the speed and angle passed
;					in as arguments. If the speed is SPEED_NO_CHANGE, the value
;					is ignored and the current speed is still used. If the angle is
;					ANGLE_NO_CHANGE, the value is ignored and the current direction
;					is still used. Otherwise, the speeds are between MIN_SPEED and
;					MAX_SPEED as a percentage of the max speed and the angles should 
;                   be normalized to be between MIN_ANGLE and MAX_ANGLE degrees. 
;                   The holonomic motion calculations are done to find the displacement 
;                   for each motor. This is used to calculate whether the motor should 
;                   move forwards or backwards and is saved to be used by the event 
;                   handler in controlling speed using pulse width modulation.
; Operation:  		The function takes in the speed and angle to move at, or that
;					either of these should not be changed from the current value.
;					Using the old speed/angle or the newly passed in values, the
;					pulse width for each of the motors is calculated. The speed is also
;					saved as a value between MIN_SPEED and MAX_SPEED and the angle
;					is saved as value between MIN_ANGLE and MAX_ANGLE degrees. This is done
;					by using the equation that pulse width for each motor is
;					the dot product of the force vector for that wheel and the
;					speed to move at. For this, the cosine and sines of the angles
;					to move at or used along with a table of all sines and cosines.
;					This calculation is done to get a pulse width value from -MAX_PWM_COUNTER
;					to MAX_PWM_COUNTER, or from full reverse to full speed forward 
;                   for each motor. The value of pulse width is also saved as a 
;                   shared variable so	the event handler can control when each 
;                   motor is on and off using pulse width modulation.
;
; Arguments:   		speed [AX] - speed for RoboTrike to move at, or no change in speed
;							as a percentage of the max speed from MIN_SPEED and
;							MAX_SPEED. Input of SPEED_NO_CHANGE	specifies that 
;							speed value should not be changed when setting angle
;					angle [BX] - angle for RoboTrike to move at, or no change in angle
;							in degrees from MIN_ANGLE to MAX_ANGLE. Input of 
;							ANGLE_NO_CHANGE specifies that angle value should not 
;							be changed when setting speed
; Return Value:		None
;
; Local Variables:	speed [AX] - speed to find pulse widths for each motor
;					angle [BX] - angle used to find components of speed
;					coordCounter [DI/BX] - counter for which component calculation
;							being done for - x or y
;					forceTablePtr [SI] - holds a pointer to force vector table
;					motorCounter [DI] - counter for which motor PWM calculations being
;							done for
;					pulseWidth [CX] - stores pulse width for a motor
; Shared Variables: velocity (R/W)- stores speed of RoboTrike or reads old setting
; 					direction (R/W)- stores direction of RoboTrike or reads old setting
; 					pulseWidth (W)- stores pulse width of motors to be used for PWM
;					speedCoords (R/W) - store and read values of speed components
;					trigTableAddress(R) - holds values of addresses for trig tables
; Global Variables: None
;
; Input:			None
; Output:			Motors of RoboTrike move forward or backward and are modulated at
;					some pulse width based on holonomic motion calculations to get
;					the RoboTrike to move in a particular direction at some speed
;
; Error Handling:	None		
;
; Algorithms:		Holonomic Motion algorithm - pulse width of each wheel = 
;					(force vector for that wheel)dot(velocity) 
;					= Fix * vx * cos(theta) + Fiy * vy * sin(theta)
; Data Structures:	Table with values of sine and cosine for MIN_ANGLE to MAX_ANGLE 
;					degrees. Table with force vectors for each motor as a pair of
;					x and y coordinates as separate entries in the table for
;					each motor.
;					Array of pulse widths specifying how many counts each motor
;					should be on for in a duty cycle (with 7 bit precision)
;
; Registers Changed: BX, AX, DX, DI, SI, CX, flags
; Stack Depth:		2 words
;
; Limitations:		This method of controlling speed and direction does not allow
;					for any feedback, to know whether the speed and angle specified
;					are actually implemented in the RoboTrike's movement.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/06/16	Maitreyi Ashok	Pseudocode/functional specification
;					11/07/16	Maitreyi Ashok	Corrected holonomic motion calculation
;					11/09/16	Maitreyi Ashok	Wrote code, changed force vectors
;												to table representation
;					11/10/16	Maitreyi Ashok	Made fixes to implementation of 
;												algorithm in debugging
;					11/11/16	Maitreyi Ashok	Further debugging, how to treat
;												negative angles
;					11/12/16	Maitreyi Ashok	Commented code

SetMotorSpeed   PROC        NEAR
                PUBLIC      SetMotorSpeed

CheckIfIgnoreAngle:
		CMP		BX, ANGLE_NO_CHANGE		; Check if the angle is the value that
		JE		OldAngleStays			; is specified to continue using the old
		;JNE	NormalizeAngle			; angle. If not, normalize the angle
NormalizeAngle:							; to be between MIN_ANGLE and MAX_ANGLE
		PUSH	AX
		MOV		AX, BX					; Prepare to divide the signed angle argument
		CWD								; passed in by the MAX_ANGLE to get a 
		MOV		BX, MAX_ANGLE			; remainder between MIN_ANGLE and MAX_ANGLE
		IDIV	BX
CheckVal:
		POP		AX
        OR      DX, DX					; Check the sign of the remainder
		JNS		DoneNormalizing			; If it is positive, normalizing is over
		;JS		NegateAngleValue		; If not, need to make the mod positive
NegateAngleValue:						
		ADD     DX, MAX_ANGLE			; Adding MAX_ANGLE to the negative 
		;JMP    DoneNormalizing			; remainder found gives a positive angle
DoneNormalizing:			
		MOV		BX, DX
SaveDirection:							; Save the angle to move at for use by
		MOV		direction, BX			; other functions and move to check the value
		JMP		CheckIfIgnoreSpeed		; of speed passed in
OldAngleStays:							; If just using the value of the old angle
		MOV		BX, direction			; move that value into the local variable
		;JMP	CheckIfIgnoreSpeed		; for use by this function
		
CheckIfIgnoreSpeed:
		CMP		AX, SPEED_NO_CHANGE		; Check if the speed is the value that
		JE		OldVelocityStays		; is specified to continue using the old angle
		;JNE	NewVelocitySave			; If it is not, save the new speed argument
NewVelocitySave:
		MOV		velocity, AX			; Save the speed to move at for use by other
		JMP		FindSpeedComponents		; functions and then find the x and y components
OldVelocityStays:
		MOV		AX, velocity			; If just using the value of the old speed
		;JMP	NextSpeedComponent		; move that value into the local variable
										; for use by this function
FindSpeedComponents:					
		XOR		DI, DI					; Clear counter for which component of speed
										; is being calculated
		SHR		AX, NEG_TO_POS_SPEED	; Convert all values of speed passed in to
										; Q0.15[0, 1) from INT[0, 65534]
        SHL     BX, BYTES_PER_WORD_SHIFT	
										; Shift angle to use as counter into a table
										; of words (wanted value is twice as many 
										; bytes into the table as the current angle value)
NextSpeedComponent:
		CMP		DI, NUM_DIRECTIONS * BYTES_PER_WORD
										; Compare the current speed coordinate to
										; total number of speed coordinates
		JGE		SetUpPulseWidthCalculation
										; If computed all speed coordinates move on
		;JL		CalculateSpeedComponent	; to calculate pulse widths
CalculateSpeedComponent:				; Otherwise calculate next speed component
		PUSH	AX
		MOV		SI, trigTableAddress[DI]; Find the address of trig table for this
                                        ; coordinate
		MOV		DX, CS:[SI][BX]			; Find the value of cos/sin of angle between
									    ; -1 and 1 in Q0.15 notation
		IMUL	DX						; Multiply the angle by the speed
		MOV		speedCoords[DI], DX		; In Q0.15 the high word is all we need
		POP		AX						; since it gives 16 bits of precision
		ADD		DI, BYTES_PER_WORD		; Move on to the next coordinate and check
		JMP		NextSpeedComponent		; if any coordinates are left
		
SetUpPulseWidthCalculation:
		XOR		DI, DI          		; Start calculating pulse widths from the 
		MOV		SI, OFFSET(ForceVectorTable)	; first motor using the force vector
												; table
CalculatePulseWidths:					
		XOR		CX, CX					; Clear variable to store the pulse width
        XOR		BX, BX           		; Start from first coordinate (x)
NextPulseWidthComponent:
		CMP		BX, NUM_DIRECTIONS * BYTES_PER_WORD
										; If computed pulse width using both directions
		JGE		UpdateMotorPulse		; of a motor, update the total pulse width
		;JL		CalculatePulseWidthComponent
CalculatePulseWidthComponent:			; Otherwise calculate the next component
        PUSH    DI	
        SHL     DI, FORCE_MOTOR_SHIFT	; Change motor number to access the force
        PUSH    BX						; component in the force table for that
        ADD     BX, DI					; motor and coordinate number
		MOV		AX, CS:[SI][BX]			; Find the force vector component
        POP     BX						; and restore old value of motor number
										; and coordinates
		IMUL	WORD PTR speedCoords[BX]	; Multiply the force component by the
										; corresponding speed component
		ADD		CX, DX					; Add the resulting pulse width to the
		ADD     BX, BYTES_PER_WORD		; value for this motor and move to the next
        POP     DI						; coordinate
		JMP		NextPulseWidthComponent
UpdateMotorPulse:
		SHL		CX, NUM_EXTRA_SIGN_BITS	; Remove the extra sign bits from multiplying
										; 3 Q0.15 values together
		SAR		CX, EXTRA_PRECISION		; Remove unneeded precision in pulse width
		MOV		pulseWidth[DI], CL		; to get END_PRECISION bits of precision
		INC     DI						; Move to calculations for next motor
CheckIfDone:							
		CMP		DI, NUM_DC_MOTORS		; If not done with all motors, calculate
		JL		CalculatePulseWidths	; pulse width for next motor. Otherwise
		;JGE	DoneWithSetSpeed		; done with setting the speed
DoneWithSetSpeed:
		RET
SetMotorSpeed	ENDP


; GetMotorSpeed			
; Description: 		This function returns the current speed for the RoboTrike.
;					The absolute value of the speed ranges between MIN_SPEED and 
;                   MAX_SPEED, inclusively as a percentage of the MAX_SPEED value. 
;                   The minimum	speed indicates that the RoboTrike is not moving, 
;                   and has stopped, whereas the maximum speed indicates that all 
;                   the motors are moving at their fastest speed possible. Negative
;                   values of the speed indicate that the motors are moving in 
;                   reverse.
; Operation:  		This function takes no arguments, and returns the current speed
;					setting. It does this by accessing a shared variable storing
;					the last speed that was passed in to SetMotorSpeed, which is the
;					speed the RoboTrike is now moving at. This absolute value of the
;                   speed will be between MIN_SPEED and MAX_SPEED, inclusively 
;                   and ranges from the RoboTrike being stopped to moving at full speed.
;                   A negative value of the speed indicates the RoboTrike motors
;                   are moving mostly in reverse.
;
; Arguments:   		None
; Return Value:		velocity [AX] - current speed setting of RoboTrike from MIN_SPEED
;							to MAX_SPEED as percentage of maximum speed setting
;
; Local Variables:	None
; Shared Variables: velocity (R) - current speed setting of RoboTrike
; Global Variables: None
;
; Input:			RoboTrike is moving at some speed due to previous settings	
; Output:			None
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	None
;
; Registers Changed: AX
; Stack Depth:		0 words
;
; Limitations:		This function only returns speed settings with a resolution
;					of 1 from MIN_SPEED to MAX_SPEED. Thus this only allows for
;					MAX_SPEED number of speed settings.
;
; Author:			Maitreyi Ashok
; Last Modified:  	11/06/16	Maitreyi Ashok		Pseudocode/functional specification
;					11/09/16	Maitreyi Ashok		Wrote code
;					11/12/16	Maitreyi Ashok		Updated comments
;					

GetMotorSpeed   PROC        NEAR
                PUBLIC      GetMotorSpeed
ReturnSpeed:
		MOV		AX, velocity		; Return the velocity RoboTrike has been
		RET							; set to move at
GetMotorSpeed	ENDP

; GetMotorDirection			
; Description: 		This function returns the current direction of the RoboTrike
;					as an angle in degrees.	The direction ranges from MIN_ANGLE 
;					to MAX_ANGLE, inclusively. The minimum direction indicates that 
;					the RoboTrike is moving straight forward compared to the orientation
;					and all other directions are relative to this angle, rotating
;					clockwise. (MAX_ANGLE is 1 degree northwest of MIN_ANGLE)
; Operation:  		This function takes no arguments, and returns the current direction
;					setting. It does this by accessing a shared variable storing
;					the last direction that was passed in to SetMotorSpeed, which 
;					is the direction the RoboTrike is now moving in. This direction 
;					will be between	MIN_ANGLE and MAX_ANGLE, inclusively and 
;					ranges from moving straight ahead to all other angles moving
;					clockwise.
;
; Arguments:   		None
; Return Value:		direction [AX] - current direction RoboTrike is moving in from
;							MIN_ANGLE to MAX_ANGLE in degrees
;
; Local Variables:	None
; Shared Variables: direction (R) - current direction RoboTrike is moving in
; Global Variables: None
;
; Input:			RoboTrike is moving in some direction due to previous settings
; Output:			None
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	None
;
; Registers Changed: AX
; Stack Depth:		0 words
;
; Limitations:		The directions are all measured relative to the RoboTrike's
;					current orientation and not some fixed reference point. Thus,
;					the user must think of the return value direction compared to
;					the RoboTrike's current position.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/06/16	Maitreyi Ashok		Pseudocode/functional specification
;					11/09/16	Maitreyi Ashok		Wrote code
;					11/12/16	Maitreyi Ashok		Updated comments

GetMotorDirection   PROC        NEAR
					PUBLIC      GetMotorDirection
ReturnDirection:
		MOV		AX, direction		; Return the direction the RoboTrike has been
		RET							; moving in
GetMotorDirection	ENDP

; SetLaser			
; Description: 		Turns the laser on or off depending on the argument passed in.
;					The laser is active high, meaning the laser  is	turned on later	
;					in the event handler when a nonzero onoff value is passed in 
;					to the function. When a zero onoff value is passed in to the 
;					function, the laser is turned off by the event handler.
; Operation:  		The function receives an argument that is nonzero if the laser
;					should be turned on and zero if the laser should be turned off.
;					This is done by saving the argument passed in to a shared variable
;					for future reference in the event handler to be written into
;					the memory location for port B of the 8255 by the motor event
;					handler at a 33.3 Hz frequency. The same value that will be 
;					combined with other	ports to set or clear the laser bit in 
;					the parallel port is saved.
;
; Arguments:   		onoff [AX] - zero to turn laser off, nonzero to turn laser on
; Return Value:		None
;
; Local Variables:	None
; Shared Variables: laser (W) - zero if laser is off, nonzero if laser is on
; Global Variables: None
;
; Input:			None
; Output:			Laser is turned on or off depending on argument later in the event
;					handler
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	None
;
; Registers Changed: flags
; Stack Depth:		0 words
;
; Limitations:		The amount of time for the laser to be on cannot be set, and
;					the laser is only turned off when the function is called again
;					with zero as the value for onoff.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/06/16	Maitreyi Ashok	Pseudocode/functional specification
;					11/09/16	Maitreyi Ashok	Wrote code
;					11/10/16	Maitreyi Ashok	Updated laser value to use value for
;												setting the bit in port B
;					11/12/16	Maitreyi Ashok	Updated comments
;					

SetLaser        PROC        NEAR
                PUBLIC      SetLaser
SaveLaserVal:
		OR		AX, AX				; Find whether value of laser passed is
		JZ		SetLaserOff			; zero or nonzero
		;JNZ	SetLaserOn
SetLaserOn:							; If nonzero, the laser is on and save the
		MOV		laser, LASER_ON		; value accordingly into shared variable
		JMP		DoneSettingLaser
SetLaserOff:
		MOV		laser, LASER_OFF	; If zero, the laser is off and save the value
		;JMP	DoneSettingLaser	; accordingly
DoneSettingLaser:
		RET
SetLaser		ENDP

; GetLaser			
; Description: 		Return the status of the RoboTrike motor unit's laser. The value
;					returned is zero of the laser is off and non zero if the laser
;					is on.
; Operation:  		This function has no arguments and returns whether the laser
;					is on or off, based on previous calls to the SetLaser function.
;					The value returned is from a shared variable of the laser status,
;					and should also reflect the value in the memory address for 
;					the laser in the 8255. If the laser is on, the value returned
;					is non zero (value used to set/clear parallel port bit), and 
;					if the laser is off, zero is returned.
;
; Arguments:   		None
; Return Value:		laser [AX] - nonzero if laser on, zero if laser off
;
; Local Variables:	None
; Shared Variables: laser (R) - nonzero if laser on, zero if laser off
; Global Variables: None
;
; Input:			RoboTrike laser is on or off due to previous settings
; Output:			None
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	None
;
; Registers Changed: AX
; Stack Depth:		0 words
;
; Limitations:		The function provides no information of how long the laser
;					has been on or off for, merely providing whether the laser is
;					currently on or off.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/06/16	Maitreyi Ashok		Pseudocode/functional specification
;					11/09/16	Maitreyi Ashok		Wrote code
;					11/12/16	Maitreyi Ashok		Updated comments

GetLaser	    PROC        NEAR
                PUBLIC      GetLaser
ReturnLaserVal:
		MOV		AL, laser		; Move the laser bit value to the lower
		XOR		AH, AH			; byte of return value and clear the high byte
		RET						; (in case laser is off and high byte is nonzero)
GetLaser		ENDP

; PulseWidthModulator			
; Description: 		Using pulse width modulation based on the pulse widths calculated
;					from the force vectors and velocity, each of the 3 DC motors
;					are turned on and off for specified amounts of time. This allows
;					the RoboTrike to move at a whole range of speeds, as each motor
;					can run backwards or forwards and at different pulse widths
;					to get a wide range of combinations of total movement.
; Operation:  		First, each of the motors is found to  move forward or backward.
;					This depends on the sign of the pulse width calculated (positive
;					pulse widths rotate forward, negative pulse widths rotate in reverse).
;					Then the pulse widths are converted to positive values to
;					use pulse width modulation appropriately. For PWM, the pulse
;					widths of each motor is compared to a total counter which
;					starts out at the size of the duty cycle. For each call to the
;					pulse width modulator, the counter is decremented. Once the
;					pulse width exceeds this counter, the motor is turned on
;					(so the motor is on for pulse width increments of time).
;					When the counter exceeds pulse width, the motor is off.
;
; Arguments:   		None
; Return Value:		parallelPortByte [AL] - byte value to write to port B of 8255
;
; Local Variables:	motorCounter [SI] - counter to go through all motors
;					pulseWidth [CL] - stores pulse width for each motor
;					parallelPortByte [AL] - byte value to write to port B of 8255
; Shared Variables: motorWidth[i] (R)- stores pulse width of motors to be used for PWM
;					pwmCounter (R/W) - counter for duty cycle of pulse width modulation
; Global Variables: None
;
; Input:			4 KHz Timer interrupt calls MotorsEventHandler 
; Output:			3 motors are turned on or off and rotated at different speeds.
;
; Error Handling:	None
;
; Algorithms:		Pulse width modulation used to control speed by turning
;	 				motors on and off for specific amounts of time.
; Data Structures:	Table that stores bits for each motor moving forward, backward,
;					and being on or off. This allows table values to be OR'ed together
;					based on current setting
;
; Registers Changed: SI, AL, CL, BX, flags
; Stack Depth:		0 words
;
; Limitations:		This function only changes speed/direction of motors
;					at regular intervals but not immediately when the event is
;					initiated by the keypad presses.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/07/16	Maitreyi Ashok		Pseudocode/functional specification
;					11/09/16	Maitreyi Ashok		Wrote code
;					11/12/16	Maitreyi Ashok		Updated comments
;		
PulseWidthModulator	PROC        NEAR
					PUBLIC      PulseWidthModulator

SetUpPWM:		
		XOR		SI, SI					; Start finding pulse width for first motor
		XOR		AL, AL					; Clear parallel port byte to add bits
LoopThroughMotors:						; based on pulse widths and laser
		CMP		SI, NUM_DC_MOTORS		; Check if we have added corresponding bits
		JGE		UpdatePWMCounter        ; for all motors. If we have, update pwm counter
		;JL		FindDirection			; Otherwise work on the next motor
FindDirection:
        MOV     CL, pulseWidth[SI]		; Find pulse width for the motor
		CMP		CL, 0       			; If the pulse width is negative, motor
		JL		NegateAndReverse        ; must rotate in reverse
		;JGE	ForwardDirection		; If positive, the  motor rotates forwards
ForwardDirection:						; If moving in forward direction, find the
		OR		AL, CS:MotorForward[SI]	; the bits for that motor moving forward
		JMP		FindOnOffState			; and add to the byte to be written
NegateAndReverse:						; If moving in reverse, get the positive
		NEG		CL            			; value of the pulse width and find the
		OR		AL, CS:MotorBackward[SI]; bits for that motor moving reverse and add
		;JMP	FindOnOffState			; to the byte to be written
FindOnOffState:         
		CMP		pwmCounter, CL			; Compare the pulse width to the counter for
		JAE		DoneWithThisMotor		; the duty cycle. If the counter is less than
		;JB	MotorOnState				; the pulse width the motor on. Otherwise off
										; If pulse width is zero, the motor will
										; never be turned on
		
MotorOnState:							; Find the bits for this motor being on
		OR		AL, CS:MotorOnOff[SI]	; and add to the byte to be written
		;JMP	DoneWithThisMotor		; If the motor is off, the bits are zero
DoneWithThisMotor:						; and nothing needs to be added
		INC		SI						; Move to adding pulse width bits for next
		JMP		LoopThroughMotors		; motor
UpdatePWMCounter:
		DEC		pwmCounter				; Decrement pulse width/duty cycle counter)
		JGE		DoneWithPulseWidthModulation
											; If counter reaches zero, wrap it back around
		MOV		pwmCounter, MAX_PWM_COUNTER	; to maximum value of counter
DoneWithPulseWidthModulation:
		RET		
PulseWidthModulator	ENDP

; MotorsEventHandler
; Description: 		This function finds the value to write to port B of the 8255 
;					based on PWM of the motors and whether the laser should be on
;					or off and is called at a 4 KHz frequency. Whether the motors 
;					should be on/off and moving forward	or in reverse is found by 
;					the pulse width modulator. The function	turns the laser on or 
;					off depending on the value of the shared variable from previous 
;					calls to SetLaser. If the laser value passed in was non zero, 
;					the laser is turned	on by setting LASER_BIT in port B of the 
;					8255. If the laser	value passed in to SetLaser was zero, the 
;					laser is turned off by clearing LASER_BIT in port B of the 8255. 
;					An end of interrupt is also sent to allow other	interrupts to take 
;					place.
; Operation:  		The motors are physically turned on or off and moved in forward
;					or reverse and the laser is physically turned on or off based 
;					on this function which is called due to a regular timer interrupt 
;					by the motor event handler. The pulse widths calculated in SetSpeed
;					are used to set the motor bits. If the motor should move in 
;					reverse, the pulse width is negative and if the motor should
;					move forward, the pulse width is positve. Also, if value of the 
;					counter of the duty cycle is less than the pulse width for the 
;					motor the motor is turned on. Otherwise, the motor is turned off.
;					This way, the motor will be turned on for pulseWidth counts of
;					the MAX_PWM_COUNTER counts of the duty cycle. The value passed 
;					into SetLaser was saved into a shared variable. This variable 
;					is accessed here and the bit in port B of the 8255 for the laser 
;					is set if the laser variable is non zero and the bit is cleared 
;					if the laser variable is zero.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	parallelPortByte [AL] - value to write to port B of the 8255
; Shared Variables: laser (R)- stores nonzero value if laser is on, zero if laser is off
; Global Variables: None
;
; Input:			None
; Output:			Laser/motors are turned on or off and moved in reverse/forward
;
; Error Handling:	None
;
; Algorithms:		None
; Data Structures:	None
;
; Registers Changed: None
; Stack Depth:		8 words
;
; Limitations:		This function does not turn the laser/motors on instantaneously 
;					after the key press since this function is only called by the 
;					event handler by a timer interrupt.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/07/16	Maitreyi Ashok		Pseudocode/functional specification
;					11/09/16	Maitreyi Ashok		Wrote code
;					11/12/16	Maitreyi Ashok		Updated comments

MotorsEventHandler	PROC        NEAR
					PUBLIC      MotorsEventHandler

SaveRegisters:
		PUSHA							; Save all registers so interrupted code
GetMotorAndLaser:						; is not affected
		CALL	PulseWidthModulator		; Find bits to set for controlling motors
		OR		AL, laser				; Add laser bit to control laser
WriteToParallelPort:
		MOV		DX, PARALLEL_PORT_B		; Write this byte to control the motors
		OUT		DX, AL					; to port B of the 8255 for parallel I/O
EndMotorsHandler:
		MOV		DX, INT_CTRLR_EOI   	; Send an EOI for the interrupt so other
		MOV		AX, TIMER_EOI			; processes can now interrupt
		OUT		DX, AL
		POPA                        	; Restore all registers to original state
		IRET
MotorsEventHandler	ENDP

; InitMotors
; Description: 		This function sets up the pulse width modulation operations 
;					of the DC motors. This is done by resetting the values of all
;					the pulse widths of the motors as well as the general counter.
;					The values of velocity, direction, and laser on/off are also 
;					reset.
; Operation:  		The pulse width modulation is set up by resetting the values
;					of all the counter used to measure a duty cycle as well as the
;					pulse widths of all the motors. Also, the velocity, direction,
;					and laser on/off are reset so the motor is at rest and facing
;					forward with laser off when the motor routines are started.
;
; Arguments:   		None
; Return Value:		None
;
; Local Variables:	motorCounter [DI] - counts through all motors to reset pulse widths
;							so that all motors will be off before setting a speed
;					coordCounter [SI] - counter of all coordinates to set addresses
;							of trig tables
; Shared Variables: velocity (W)- stores speed of RoboTrike
; 					direction (W)- stores direction of RoboTrike
; 					motorWidth[i] (W)- stores pulse width of motors to be used for PWM
;					pwmCounter (W) - counter for pulse width modulation
;					laser (W) - nonzero if laser on, zero if laser off
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
; Registers Changed: DI, SI, flags
; Stack Depth:		0 words
;
; Limitations:		This function has a fixed length duty cycle which cannot be
;					changed with user input. Also, the RoboTrike is also started
;					in a specific direction and can not be pre-set.
;
; Author:			Maitreyi Ashok
; Last Modified:	11/07/16	Maitreyi Ashok		Pseudocode/functional specification
;					11/09/16	Maitreyi Ashok		Wrote code
;					11/12/16	Maitreyi Ashok		Updated comments

InitMotors	PROC        NEAR
			PUBLIC      InitMotors
ResetAllStatus:
		MOV		pwmCounter, MAX_PWM_COUNTER	; Set duty cycle counter to max to count down
		MOV		laser, LASER_OFF			; Reset values of laser, velocity, and
		XOR		AX, AX						; and direction so that RoboTrike 
		XOR		BX, BX						; motors/laser are off when started
		CALL	SetMotorSpeed				
SetTrigTableAddresses:
		XOR		SI, SI						; Set trig table for x coordinate as 						
		MOV		trigTableAddress[SI], OFFSET(Cos_Table)	; Cos_Table
		ADD     SI, BYTES_PER_WORD			; Set trig table for y coordinate as
		MOV		trigTableAddress[SI], OFFSET(Sin_Table)	; Sin_Table
DoneInitMotors:
		RET
InitMotors	ENDP

SetRelTurretAngle   PROC    NEAR   
                    PUBLIC  SetRelTurretAngle
        RET
SetRelTurretAngle   ENDP

SetTurretAngle   PROC    NEAR
                 PUBLIC  SetTurretAngle
        RET
SetTurretAngle      ENDP

SetTurretElevation  PROC    NEAR
                    PUBLIC  SetTurretElevation
        RET
SetTurretElevation  ENDP

; ForceVectorTable
; Description:		This table contains the x and y components of the force vectors
;					in Q0.15 notation. Each pair of two entries correspond to 
;					a motor, each element of the pair is the x or y component
;					of the force vector. For example, the force vector for the
;					first vector is (7FFFH, 0000H) or (1, 0). The force vector
;					for the second vector is (C000H, 9127H) or (-1/2, -sqrt(3)/2).
;					The force vector for the third vector is (C000H, 6ED9H) or
;					(-1/2, sqrt(3)/2).
; Author:			Maitreyi Ashok
; Last Modified:	11/09/16	Maitreyi Ashok		Created table
;					11/12/16	Maitreyi Ashok		Updated comments
;					11/26/16	Maitreyi Ashok		Updated comments

ForceVectorTable	LABEL	WORD

;	DW		Force Value
	DW		7FFFH	; Force vector x component motor 1, 1 in Q0.15
	DW		0000H	; Force vector y component motor 1, 0 in Q0.15
	DW		0C000H	; Force vector x component motor 2, -1/2 in Q0.15
	DW		9127H 	; Force vector y component motor 2, -sqrt(3)/2 in Q0.15
	DW		0C000H	; Force vector x component motor 3, -1/2 in Q0.15
	DW		6ED9H	; Force vector y component motor 3, sqrt(3)/2 in Q0.15

	
; MotorOnOff
; Description:		This table contains the motor on bits to set for each of the 
;					motors. The 1st, 3rd, or 5th bits are used to set the motors 
;					1, 2, and 3. These values can be OR'ed with other values
;					for the bits for other aspects of the motors to get the value
;					to write to the parallel I/O port B
; Author:			Maitreyi Ashok
; Last Modified:	11/09/16	Maitreyi Ashok		Created table
;					11/12/16	Maitreyi Ashok		Updated comments

MotorOnOff	LABEL	BYTE

;	DW		Motor On Bits
	DB		00000010B	; Motor 1 On
	DB		00001000B	; Motor 2 On
	DB		00100000B	; Motor 3 On
	
	
; MotorForward
; Description:		This table contains the motor forward bits to set/clear for 
;					each of the motors. These values can be OR'ed with other values
;					for the bits for other aspects of the motors to get the value
;					to write to the parallel I/O port B. The 0th, 2nd, or 4th bits
;					are set to make motors 1, 2, and 3 to move backwards, 
;					respectively.
; Author:			Maitreyi Ashok
; Last Modified:	11/09/16	Maitreyi Ashok		Created table
;					11/12/16	Maitreyi Ashok		Updated comments

MotorForward	LABEL	BYTE

;	DW		Motor Forward Bits
	DB		00000000B	; Motor 1 Forward
	DB		00000000B	; Motor 2 Forward
	DB		00000000B	; Motor 3 Forward
	
; MotorBackward
; Description:		This table contains the motor reverse bits to set/clear for 
;					each of the motors. These values can be OR'ed with other values
;					for the bits for other aspects of the motors to get the value
;					to write to the parallel I/O port B. For each of the motors,
;					bit 0.
; Author:			Maitreyi Ashok
; Last Modified:	11/09/16	Maitreyi Ashok		Created table
;					11/12/16	Maitreyi Ashok		Updated comments

MotorBackward	LABEL	BYTE

;	DW		Motor Backward Bits
	DB		00000001B	; Motor 1 Backward
	DB		00000100B	; Motor 2 Backward
	DB		00010000B	; Motor 3 Backward
	
	
CODE    ENDS

;the data segment

DATA    SEGMENT PUBLIC  'DATA'

laser			DB		?	; stores bit value to write to parallel port for the laser bit
							; - either LASER_ON or LASER_OFF
velocity		DW		?	; stores total speed from MIN_SPEED to MAX_SPEED of RoboTrike
direction		DW		?	; stores direction of RoboTrike from MIN_ANGLE to MAX_ANGLE
pulseWidth		DB	NUM_DC_MOTORS 	DUP (?)   
							; stores pulse width of motors - or how many counts they 
							; should be on for per cycle
pwmCounter		DB		? 	; counter for a duty cycle of all the motors, motors
							; turned on through PWM relative to this counter
speedCoords		DW	NUM_DIRECTIONS	DUP (?)
							; Contains the values of v*cos(theta) and v*sin(theta)
trigTableAddress DW	NUM_DIRECTIONS	DUP	(?)
							; Contains the addresses of Cos_Table and Sin_Table
							
DATA    ENDS

        END