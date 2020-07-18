# set c to tty
loadimm a 'H'
!copy serialout a
loadimm a 'i'
!copy serialout a
loadimm a $a
!copy serialout a
loadimm pc :halt
