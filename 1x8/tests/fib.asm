# set sp to fe
copy imm addr ff 
copy imm mem fd
copy imm addr fe
copy imm mem 0
copy imm addr fd
copy imm mem 6
copy imm pc ff


copy imm addr ff
load imm acc 1
add imm mem 1