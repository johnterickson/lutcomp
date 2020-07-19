


# computer  

## instruction format
variable length

## flags
```
bit 0: carry
bit 1: zero
bit 2: negative
```

## ALU lookup
```
0-7: in2
8-15: in1
16-18: opcode
```

## ALU opcodes:
```
0 0 0 addlo: (in1 + in2) & 0xFF
0 0 1 addhi: Flags from (in1 + in2)
0 1 0 Or: in1 | in2
0 1 1 Xor: in1 ^ in2
1 0 0 And: in1 & in2
1 0 1 Special: mode:[in2][6..=7]
      mode 00: shifts/roates
            shift_mode:[in2][4..=5] amount:[in2][0..=3] as signed int
                  shift_mode:00 ROTATE(ACC, amount) -> [out]
                  shift_mode:01 LOGICAL_SHIFT(ACC, amount) -> [out]
                  shift_mode:10 ARTHIMETIC_SHIFT(ACC, amount) -> [out]
                  shift_mode:11 reserved (not?)
      mode 01:
      mode 10:
      mode 11:
1 1 0 MultiplyLo: (in1 * in2) & 0xFF
1 1 1 MultiplyHi: ((in1 * in2)>>8) & 0xFF
```

# uComputer

## ucode instruction
2 byte instructions
byte 0:
      bits 0-2: data bus output select bits
      bit 3: ?
      bits 4-6: ALU opcode
      bit 7: address bus output select
byte 1:
      bits 0-3: data bus load select bits
      bits 4-7: sign-extended

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
| 1    |  Halt   | PC       |
| 2    |  IMM   |   PCR    |
| 3    |  ALU   |    ALU   |
| 4    |  MEM   |   MEM    |
| 5    |  TTYIN   |   TTYIN    |
| 6    |  X   |   X    |
| 7    |  Y   |   Y    |
| 8    |     |    ADDR0   |
| 9    |     |   ADDR1    |
| 10/a    |     |   ADDR2    |
| 11/b    |     |   TTYOUT    |
| 12/c    |     |   IN1    |
| 13/d    |     |   IR0    |
| 14/e    |     |   FLAGS   |
| 15/f    |     |   ?    |


 

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


acc <- acc ^ *80000000
----------------------
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

r1 <- r2 ^ r3
----------------------
out pc, out mem, *, load ir0, *

*, out imm, *, load a2, sx(-8)
*, out imm, *, load a1, sx(0)

 / repeat 4x
*, out imm, *, load a0, sx(-8) // r2
out addr, out mem, *, load in1, *
*, out imm, *, load a0, sx(-4) // r3
out addr, out mem, xor, load alu, *
*, out imm, *, load a0, sx(4) // r1
out addr, out alu, *, load mem, *
/

r1 <- r2 + r3
----------------------
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




*, out alu, *, load x, *
out addr, out mem, addhi, load alu, *
out addr, out alu, *, load mem, *
/

