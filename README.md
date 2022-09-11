


# computer  

## todo
- how to handle interrupts not splitting multi-opcode instructions
  - [x] leverage CARRY_PENDING?
  - [ ] add test
- PS2 -> I/O board
  - [x] I/O mux
  - [x] move tty behind I/O mux
    - [x] software
    - [x] circuit
  - [x] make I/O ports bidirectional
- RTC
- hlt command to wait for an interrupt

## instruction format
variable length

## registers
Each is 8-bit
```
r0
r1
r2
r3
```

## flags
```
bit 0: carry
bit 1: zero
bit 2: negative
bit 3: it's complicated
```

# uComputer

## ALU lookup
```
0-7: in2
8-15: in1
16-18: opcode
```

## ucode instruction
```
2 byte instructions
byte 0:
      bits 0-2: data bus output select bits
      bit 3: ?
      bits 4-6: ALU opcode
      bit 7: address bus output select
byte 1:
      bits 0-3: data bus load select bits
      bits 4-7: sign-extended
```
## ucode address
```
7 bits 0-6 for uPC
[8 bits IR0 register]
[4 bits flags ]
```

## Control lines

### Address bus

| Register | Select |
| -------  | ------ |
|  ADDR*    | 0 |
|  PC*      | 1 |

### Data bus

| Index  | Output | Load (*_CP) |
| -----  | ---- | -----  |
| 0    |  Next   |   PC_INC    |
| 1    |  Halt   | IR0       |
| 2    |  IMM    |   IN1    |
| 3    |  ALU    |    ALU   |
| 4    |  MEM    |   MEM    |
| 5    |  TTYIN  |   TTYIN    |
| 6    |  PC   |   PCR    |
| 7    |     |     FLAGS  |
| 8    |   W  |   W    |
| 9    |    X |    X   |
| 10/a    |  Y   |   Y    |
| 11/b    |  Z   |    Z   |
| 12/c    |     |   ADDR0    |
| 13/d    |     |    ADDR1   |
| 14/e    |     |  ADDR2    |
| 15/f    |     |   TTYOUT    |


 

# Thoughts on optimizing ALU:
---------
# Most operations can be split in half:
8-bit XOR 8-bit == 4-bit XOR 4-bit | 4-bit XOR 4-bit
This would save *lots* of space... but it would mean more registers...
Can we make it *mostly* 8-bit but with a *mostly* 4-bit ALU?

Could even do add with carry flag
Shifts: 
4-bit value, 4-bit shift => (8-bit result)
Multiplication is four 8-bit with 16-bit adds
ab *cd = bd + 16ad + 16bc + 256ac

e.g. alu lookup
0000*: Parallel, independent 4-bit operations e.g. ADD, AND, OR, XOR, COPY, ...
[3 bit opcode]
[4 flags]
[4 bit in1]
[4 bit in2]

001*: two output e.g. shift, rotate
[1 bit lo/hi result]
[3 bit opcode]
[4 flags]
[4 bit in1]
[4 bit in2]

010*

Most operations are symmetrical:
x XOR y == y XOR x


### acc <- acc ^ *80000000
```
out pc, out mem, *, load ir0, *
*, out imm, *, load a2, sx(-4)
*, out imm, *, load a1, sx(0)

*, out imm, *, load a0, sx(0)
*, out w, *, load in1, *
out addr, out mem, xor, load alu, *
*, out alu, *, load w, *

*, out imm, *, load a0, sx(1)
*, out x, *, load in1, *
out addr, out mem, xor, load alu, *
*, out alu, *, load x, *

*, out imm, *, load a0, sx(2)
*, out y, *, load in1, *
out addr, out mem, xor, load alu, *
*, out alu, *, load y, *

*, out imm, *, load a0, sx(3)
*, out z, *, load in1, *
out addr, out mem, xor, load alu, *
*, out alu, *, load z, *
```

### r1 <- r2 ^ r3
```
out pc, out mem, *, load ir0, *

*, out imm, *, load a2, sx(-8)
*, out imm, *, load a1, sx(0)

*, out imm, *, load in1, sx(2) // r2
*, out imm, multiplylo, load alu, sx(4)
*, out alu, *, load x, *

*, out imm, *, load a0, sx(3) // r3
*, out imm, multiplylo, load alu, sx(4)
*, out alu, *, load y, *

*, out imm, *, load a0, sx(1) // r1
*, out imm, multiplylo, load alu, sx(4)
*, out alu, *, load z, *

 / repeat 4x
*, out x, *, load a0, *
out addr, out mem, *, load in1, *
*, out y, *, load a0, *
out addr, out mem, xor, load alu, *
*, out z, *, load a0, *
out addr, out alu, *, load mem, *


TODO increment 
/
```

### r1 <- r2 + r3
```
out pc, out mem, *, load ir0, *

*, out imm, *, load a2, sx(-8)
*, out imm, *, load a1, sx(0)

*, out imm, *, load a0, sx(-8) // r2[0]
out addr, out mem, *, load in1, *]

 / repeat 4x
 *, out imm, *, load a0, sx(-8+i) // r2[i]
out addr, out mem, *, load in1, *
if (flags.CARRY) {
      *, out imm, addhi, load alu, sx(1)
      *, out alu, *, load flags, *
      *, out imm, addlo, load alu, sx(1)
      *, out alu, *, load in1, *
}
*, out imm, *, load a0, sx(-4+i) // r3[i]
out addr, out mem, addhi, load alu, *
*, out alu, *, load flags, *
out addr, out mem, addlo, load alu, *
*, out alu, *, load x, *
*, out imm, *, load a0, sx(4+i) // r1[0]
*, out x, *, load mem, *
```