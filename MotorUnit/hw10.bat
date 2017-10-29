rem Compile chip select function
asm86 ChipSel.asm m1 db ep
rem Compile number to string conversions
asm86 Converts.asm m1 db ep
rem Compile event queue and general functions
asm86 EvQueue.asm m1 db ep
asm86 GenFunc.asm m1 db ep
rem Compile general interrupt handler
asm86 GenIH.asm m1 db ep
rem Initialize parallel port
asm86 InitPara.asm m1 db ep
rem Compile motor loop general functions and motor main loop
asm86 MotoFns.asm m1 db ep
asm86 MotoLoop.asm m1 db ep
rem Compile motor routines and timer
asm86 Motors.asm m1 db ep
asm86 Mtimer.asm m1 db ep
rem Compile general queue routines
asm86 Queue.asm m1 db ep
rem Compile serial routines
asm86 SerialIO.asm m1 db ep
asm86 SerInt2.asm m1 db ep
rem Compile motor side parser
asm86 Serparse.asm m1 db ep
rem Compile serial string routines
asm86 SerStr.asm m1 db ep
rem Compile table of trig values
asm86 trigtbl.asm m1 db ep

rem Link together above compiled files
link86 ChipSel.obj, Converts.obj, Queue.obj, EvQueue.obj, GenFunc.obj, GenIH.obj, InitPara.obj TO part1.lnk
link86 part1.lnk, trigtbl.obj, Motors.obj, Mtimer.obj TO part2.lnk
link86 part2.lnk, SerialIO.obj, SerStr.obj, SerInt2.obj, Serparse.obj, MotoFns.obj, MotoLoop.obj TO MotoLoop.lnk

rem Locate memory segments for all the files
loc86 MotoLoop.lnk AD(SM(CODE(4000H), DATA(400H), STACK(7000H))) NOIC
