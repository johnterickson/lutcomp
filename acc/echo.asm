# set c to tty
loadimm c :tty
:start
# copy *tty to a
or a &c $0
# save a copy in b
or b a $0
and a $80
# begin !skip_if_zero
equals a $00
add pc a pc
# end !skip_if_zero
loadimm pc :halt
# restore copy to a
or a b $0
and a $7f
# output to tty
or &c a $0
loadimm pc :start
