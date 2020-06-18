# add dword 1081 @ 0 to dword 4080 @ 2 and store @ 4

#init
copy imm addr 0
copy imm mem 81
copy imm addr 1
copy imm mem 10
copy imm addr 2
copy imm mem 80
copy imm addr 3
copy imm mem 40

# add lo byte
copy imm addr 0
copy mem acc
copy imm addr 2
add mem acc
copy imm addr 4
copy acc mem

# add hi byte
copy imm addr 1
copy mem acc
copy imm addr 3
add mem acc
copy imm addr 5
copy acc mem

# if (sum + (-a) < 0) { carry = 1 }
# negate a
copy imm addr 0
copy mem acc
xor imm acc ff
add imm acc 1
# add sum
copy imm addr 4
add mem acc
rotate imm acc f9
and imm acc 01
copy imm addr 05
add mem acc
copy acc mem
