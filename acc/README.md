


# computer  

# instruction format
[3 bit opcode]
[2 bit mode]
[3 bits output register index]

mode 00: out <- fn(a,in2)
[8 bits in2 immediate]

mode 01: out <- fn(&in1,&in2)
[1 bit in1 direct/indirect]
[3 bits in1 register index]
[1 bit in2 direct/indirect]
[3 bits in2 register index]

mode 10: out <- fn(&in1, &signext(imm))
[1 bit in1 direct/indirect]
[3 bits in1 register index]
[1 bit in2 direct/indirect]
[3 bits in2 sign-ext immediate]

mode 11: &out <- fn(&in1, &signext(imm))
[1 bit in1 direct/indirect]
[3 bits in1 register index]
[1 bit in2 direct/indirect]
[3 bits in2 sign-ext immediate]
```

opcodes:
0 0 0 Copy: [in2] -> [out]
0 0 1 Add: ACC + [in2] -> [out]
0 1 0 Or: ACC | [in2] -> [out]
0 1 1 Xor: ACC ^ [in2] -> [out]
1 0 0 And: ACC & [in2] -> [out]
1 0 1 Shift: shift_mode:[in2][6..=7] amount:[in2][0..=3] as signed int
      shift_mode:00 ROTATE(ACC, amount) -> [out]
      shift_mode:01 LOGICAL_SHIFT(ACC, amount) -> [out]
      shift_mode:10 ARTHIMETIC_SHIFT(ACC, amount) -> [out]
      shift_mode:11 reserved
1 1 0 Equals: ACC == [in2] ? 1 : 0 -> [out]
1 1 1 Multiply: ACC * [in2] -> [out]

# uComputer

# ucode lookup
6 bits 0-6 for uPC
[second byte of instruction]
[bottom 5 bits of first byte of instruction]

# ALU lookup
0-7: in2
8-15: in1
16-18: opcode

# Control lines:
|Register| Load | Output |
| -----  | ---- | -----  |
| IRL    |  0   |   0    |
| IRH    |  1   |        |
| PCINC  |      |   1    |
| ALU    |  2   |   2    |
| TMP    |  3   |   3    |
| RAM    |  4   |   4    |
| PC     |  5   |   5    |
| IN1    |  6   |        |
| ADDR   |  7   |        |
| RSV?   |      |   6    |
| A      |  8   |   8    |
| B      |  9   |   9    |
| C      |  10  |   10   |
| IRLL   |  11  |   11   |
| ttyin  |  13  |   13   |
| ttyout |  14  |        |
| next   |  15  |        |
