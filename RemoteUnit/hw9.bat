rem Compile the serial handling files
asm86 SerialIO.asm m1 db ep
asm86 SerInt2.asm m1 db ep
rem Compile general interrupt and chip select functions
asm86 GenIH.asm m1 db ep
asm86 ChipSel.asm m1 db ep
rem Compile queues
asm86 Queue.asm m1 db ep
rem Compile keypad and display routines
asm86 Keypad.asm m1 db ep
asm86 Display.asm m1 db ep
rem Compile event queue
asm86 EvQueue.asm m1 db ep
rem Compile any general functions
asm86 GenFunc.asm m1 db ep
rem Compile keypad and display timer
asm86 KDTimer.asm m1 db ep
rem Compile segment table for display
asm86 segtab14.asm m1 db ep
rem Compile remote parser
asm86 StrOps.asm m1 db ep
rem Compile remote main loop and functions
asm86 RemoLoop.asm m1 db ep
asm86 RemoFns.asm m1 db ep
rem Compile serial string routine and string conversion
asm86 SerStr.asm m1 db ep
asm86 Converts.asm m1 db ep

rem Link all above compiled files together
link86 SerialIO.obj, GenIH.obj, SerInt2.obj, ChipSel.obj, Queue.obj, Keypad.obj, EvQueue.obj, GenFunc.obj TO part1.lnk
link86 part1.lnk, segtab14.obj, Converts.obj, Display.obj, SerStr.obj TO part2.lnk
link86 RemoFns.obj, RemoLoop.obj, part2.lnk, KDTimer.obj, StrOps.obj TO RemoLoop.lnk

rem Locate memory segments for all files
loc86 RemoLoop.lnk AD(SM(CODE(4000H), DATA(400H), STACK(7000H))) NOIC
