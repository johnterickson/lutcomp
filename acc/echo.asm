:start
# read *tty to b
!copy b serialin
!copy a b
and a $80
equals a $80
!skip_if_zero
loadimm pc :halt
!copy serialin b
!copy a b
and a $7f
!copy serialout a
loadimm pc :start
