# set c to tty
loadimm a 'H'
or serialout a $0
loadimm a 'i'
or serialout a $0
loadimm a $a
or serialout a $0
loadimm pc :halt
