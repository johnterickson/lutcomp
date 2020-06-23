# lutcomp

Hi! I like tinkering with logisim.  

I'm currently looking at making a dumb computer that minimizes the numbers of wires and circuits by replacing the ALU with one giant ROM LUT

My thought is that the painstaking part of making a physcial homebrew computer is the ALU
So instead, i would just get a big ROM and program it with the answers
address:[3 bits opcode][4 bits flags][4 bits operand1][4 bits operand2]
so if opcode 0b001 is "add" then at address 0x010304 would be the byte 0x07 because 3+4=7


LUT-ALU computer

8-bit instuctions
4-bit math
4-bit flags
24-bit addressing

instruction
[3 bit opcode][1 bit immediate=0/register=1][4 bits [immediate value][register index]]

register indicies:
0 X
1 Y
2 MEMORY
3 ?
4 ADDR0 .. ADDR5
a PC0 .. PC5

instructions:
0 0 0 ? ? ? ? ? LOAD IMM/[REG] -> X
0 0 1 0 ? ? ? ? 
0 0 1 1 ? ? ? ? STORE X -> IMM/[REG]
0 1 0 ? ? ? ? ? ADC X + FLAGS[CARRY] + IMM/[REG] -> X, FLAGS[CARRY]
0 1 1 ? ? ? ? ? 