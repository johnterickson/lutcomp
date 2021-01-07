loadimm32 r00 <- :hello_world
!call :puts
halt

:puts
load8 *r00 -> r04
orimm8 r04 <- $00
jzimm :puts_done
ttyout r04
addimm32ignorecarry r00 <- $00000001
jmpimm :puts
:puts_done
!return

:hello_world
LITERAL_STRING Hello, World!
LITERAL8 $0A
LITERAL8 $00
