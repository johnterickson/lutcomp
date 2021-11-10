loadimm8 r00 <- $01
and8 r00 r00 -> r00
loadimm8 r01 <- 'H'
ttyout r01
loadimm8 r01 <- 'i'
ttyout r01
loadimm8 r01 <- '!'
ttyout r01
loadimm8 r01 <- $0A
ttyout r01
:prompt
loadimm8 r01 <- '>'
ttyout r01
loadimm8 r01 <- ':'
ttyout r01
:read
ttyin r01
and8 r01 r01 -> r02
andimm8 r02 <- $80
jzimm :read
andimm8 r01 <- $7f
ttyout r01
and8 r01 r01 -> r02
xorimm8 r02 <- 'q'
jzimm :halt
and8 r01 r01 -> r02
xorimm8 r02 <- $0A
jzimm :prompt
jmpimm :read
:halt
halt
