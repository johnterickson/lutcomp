copy imm addr 01
copy imm mem '>'
copy imm addr 00
copy mem acc
and imm acc 80
rotleft imm acc -7
add imm acc 1
add pc pc
copy imm pc 2
copy mem acc
copy imm mem ff
and imm acc 7f
copy imm addr 1
copy acc mem
compare imm acc '.'
and imm acc 01
xor imm acc 01
add imm acc 01
add pc pc
copy imm pc 2
copy imm pc ff
