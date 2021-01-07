loadimm8 r01 <- 'B'
ttyout r01
loadimm8 r01 <- 'O'
ttyout r01
loadimm8 r01 <- 'O'
ttyout r01
loadimm8 r01 <- 'T'
ttyout r01
loadimm8 r01 <- $0A
ttyout r01
:prompt
loadimm8 r01 <- ':'
ttyout r01
:read
ttyin r01
copy8 r01 -> r02
andimm8 r02 <- $80
jzimm :read
andimm8 r01 <- $7f
ttyout r01
copy8 r01 -> r02
xorimm8 r02 <- 'q'
jzimm :halt
copy8 r01 -> r02
xorimm8 r02 <- $0A
jzimm :prompt
jmpimm :read
:halt
halt
