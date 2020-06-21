# add dword 1081 @ 2 to dword 4080 @ 4 and store @ 6

#init
copy imm addr 2
copy imm mem 81
copy imm addr 3
copy imm mem 10
copy imm addr 4
copy imm mem 80
copy imm addr 5
copy imm mem 40

# add lo byte
copy imm addr 2
copy mem acc
copy imm addr 4
add mem acc
copy imm addr 6
copy acc mem

# add hi byte
copy imm addr 3
copy mem acc
copy imm addr 5
add mem acc
copy imm addr 7
copy acc mem

# if (sum + (-a) < 0) { carry = 1 }
# negate a
copy imm addr 2
copy mem acc
xor imm acc ff
add imm acc 1
# add sum
copy imm addr 6
add mem acc
# move the sign bit to bit0 - it is the sign bit
rotleft imm acc -7
# isolate it
and imm acc 01

copy imm addr 07
add mem acc
copy acc mem
copy imm pc -1
