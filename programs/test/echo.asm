#!image_base_address=(00080400)
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
io_ready_to_read r02
andimm8 r02 <- $01
jzimm :read
ttyin r01
ttyout r01
and8 r01 r01 -> r02
xorimm8 r02 <- 'q'
jzimm :halt
and8 r01 r01 -> r02
xorimm8 r02 <- $0A
jzimm :prompt
jmpimm :read
:halt
halt $00000000
