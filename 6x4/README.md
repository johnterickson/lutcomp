# lutcomp

Hi! I like tinkering with logisim.  

I'm currently looking at making a dumb computer that minimizes the numbers of wires and circuits by replacing the ALU with one giant ROM LUT

My thought is that the painstaking part of making a physcial homebrew computer is the ALU
So instead, i would just get a big ROM and program it with the answers
address: `[3 bits opcode][4 bits flags][4 bits operand1][4 bits operand2]`
so if opcode 0b001 is "add" then at address 0x1034 would be the byte 0x07 because 3+4=7


LUT-ALU computer

* 8-bit instuctions
* 4-bit math
* 4-bit flags
* 24-bit addressing via 6 4-bit registers

### instruction format:
```
[3 bit opcode][1 bit immediate=0/register=1][4 bits [immediate value][register index]]
```

### register indicies:
| index | name |
| ----: | :--- |
| 0..=5 | ADDR0 .. ADDR5 |
| 6 | X |
| 7 | Y |
| 8..=d | PC0 .. PC5 |
| e | ? |
| f | MEMORY |



instructions:
```
0 0 0 ? ? ? ? ? LOAD IMM/[REG] -> X
0 0 1 0 ? ? ? ? (available) 
0 0 1 1 ? ? ? ? STORE X -> [REG]
0 1 0 ? ? ? ? ? ADC X + FLAGS[CARRY] + IMM/[REG] -> X, FLAGS[CARRY]
0 1 1 ? ? ? ? ? OR
1 0 0 ? ? ? ? ? XOR
1 0 1 ? ? ? ? ? AND
1 1 0 ? ? ? ? ? ROT
1 1 1 ? ? ? ? ? CMP
```