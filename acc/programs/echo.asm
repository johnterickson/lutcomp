loadimm8 r01 <- '>'
ttyout r01
loadimm8 r01 <- ':'
ttyout r01
:loop
ttyin r01
copy8 r01 -> r02
andimm8 r02 <- $80
jz :loop
andimm8 r01 <- $7f
ttyout r01
xorimm8 r01 <- 'q'
jz :halt
jmp :loop
:halt
halt
