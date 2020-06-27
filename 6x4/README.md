# lutcomp

Hi! I like tinkering with logisim.  

I'm currently looking at making a dumb computer that minimizes the numbers of wires and circuits by replacing the ALU with one giant ROM LUT

My thought is that the painstaking part of making a physcial homebrew computer is the ALU
So instead, i would just get a big ROM and program it with the answers
address: `[3 bits opcode][4 bits flags][4 bits operand1][4 bits operand2]`
so if opcode 0b001 is "add" then at address 0x1034 would be the byte 0x07 because 3+4=7


LUT-ALU computer

* 16-bit instuctions
* 4-bit math
* 4-bit flags
* 24-bit addressing via 6 4-bit registers

### instruction format:
```
[4 bit opcode]
[1 bit src immediate=0/register=1]
[4 bits input2 [immediate value][register index]]
[4 bits output register index]
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


https://www.digikey.com/products/en/integrated-circuits-ics/logic-flip-flops/706?k=74LS377&k=&pkeyword=74LS377&sv=0&pv69=411897&sf=0&FV=1989%7C0%2C-8%7C706%2C41%7C380371&quantity=&ColumnSort=0&page=1&pageSize=25