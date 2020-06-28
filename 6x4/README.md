# lutcomp

Hi! I like tinkering with logisim.  

I'm currently looking at making a dumb computer that minimizes the numbers of wires and circuits by replacing the ALU with one giant ROM LUT

My thought is that the painstaking part of making a physcial homebrew computer is the ALU
So instead, i would just get a big ROM and program it with the answers
address: `[4 bits opcode][4 bits flags CZ??][4 bits operand1][4 bits operand2]`
so if opcode `0b001` is "add" then at address `0x1034` would be the byte `0x07` because 3+4=7


LUT-ALU computer

* 16-bit instuctions
* 8-bit logic (via 2x 4-bit ALU lookups)
* 4-bit flags
* 24-bit addressing RAM/ROM via 3 8-bit registers
* 8-bit addressing uRAM via 1 8-bit register uADDR

### instruction format:
```
[4 bit opcode]
[1 bit src immediate=1/register=0]
[2 bits input2 register index]
[2 bits output register index]
```

### input2 indicies:
| index | name |
| ----: | :--- |
| 0 | IMM |
| 1 | uPC |
| 2 | uADDR |
| 3 | uMEM |

### extended register indicies:

| 3 | X |
| 7 | Y |
| 8..=d | PC0 .. PC5 |
| e | uPC |
| f | MEMORY |

| reg/imm bit | input 2 | out register | 
| --- | --- | --- |
|  0  | register indexed by bits 2,3 | register indexed by bits 0,1 |
|  1  | immediate in bits 0-3 | ACC |

instructions:
```
x x x 1 [ immediate ] 
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

```
// emulate 'add rd, rx, ry'

rd_addr: $90
rx_addr: $91
ry_addr: $92

// grab d
copy addr, $80
copy acc, mem
// multiply by 4 to get address in memory
rotl acc, 2
copy addr, rd_addr
copy mem, acc

// grab x
copy addr, $81
copy acc, mem
// multiply by 4 to get address in memory
rotl acc, 2
copy addr, rx_addr
copy mem, acc

// grab y
copy addr, $82
copy acc, mem
// multiply by 4 to get address in memory
rotl acc, 2
copy addr, ry_addr
copy mem, acc


copy addr, rx_addr
// get address of LSB of rx
copy addr, mem
// read LSB of rx
copy acc, mem

copy addr, ry_addr
copy addr, mem
adc acc, mem

copy addr, rd_addr
copy addr, mem
copy mem, acc



// store at t0=$90
copy addr, $90 
copy mem, acc

// grab y
copy addr, $82
copy acc, mem
// multiply by 4 to get address in memory
rotl acc, 2
copy addr, acc
// read LSB of ry
copy acc, mem
// add LSB of rx
copy addr, $90 
adc acc, mem
// store at t0=$90 = rx[0] + ry[1]
copy mem, acc

```
https://www.digikey.com/products/en/integrated-circuits-ics/logic-flip-flops/706?k=74LS377&k=&pkeyword=74LS377&sv=0&pv69=411897&sf=0&FV=1989%7C0%2C-8%7C706%2C41%7C380371&quantity=&ColumnSort=0&page=1&pageSize=25