# set c to tty
loadimm c :tty
loadimm a 'H'
or &c a a
loadimm a 'i'
or &c a a
loadimm a $a
or &c a a
loadimm pc :halt
