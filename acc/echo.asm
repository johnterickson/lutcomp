:start
# read *tty to b
or b serialin serialin
or a b b
and a $80
# begin !skip_if_zero
equals a $80
multiply a $02
add a $01
add pc a pc
# end !skip_if_zero
loadimm pc :halt
or serialin b b
or a b b
and a $7f
# output to tty
or serialout a a
loadimm pc :start
