# set c to tty_in
xor c c c
or c :tty
:start
# copy *tty to a
or a &c $0
# copy to b
or b a $0
and c $80
## begin !skip_if_zero
#equals acc imm $00
#add pc pc
## end !skip_if_zero
#copy pc imm :halt
#copy acc memimm $ff
#and acc imm $7f
#copy memimm acc $00
#copy pc imm :start
