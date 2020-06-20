# lutcomp

Hi! I like tinkering with logisim.  

I'm currently looking at making a dumb computer that minimizes the numbers of wires and circuits by replacing the ALU with one giant ROM LUT

My thought is that the painstaking part of making a physcial homebrew computer is the ALU
So instead, i would just get a big ROM and program it with the answers
address:[3 bits opcode][8 bits operand1][8 bits operand2]
so if opcode 0b001 is "add" then at address 0x010304 would be the byte 0x07 because 3+4=7
