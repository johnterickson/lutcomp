


# computer  

# instruction format
3 bits opcode
2 bits output is 0:ACC 1:? 2:MEM[IMM] 3:MEM[MEM[IMM]]
2 bits in2 is 0:? 1:IMM 2:MEM[IMM] 3:MEM[MEM[IMM]]
1 bit ?

opcodes:
0 0 0 Copy: [in2] -> [out]
0 0 1 Add: ACC + [in2] -> [out]
0 1 0 Or: ACC | [in2] -> [out]
0 1 1 Xor: ACC ^ [in2] -> [out]
1 0 0 And: ACC & [in2] -> [out]


# uComputer

# ucode lookup
7 its 0-6 for uPC
[top 4 bits of second byte of instruction]
[1st byte of instruction]

# ALU lookup
0-7: in2
8-15: in1
16-18: opcode

# Control lines:
|Register| Load | Output |
| -----  | ---- | -----  |
| IRL    |  0   |   0    |
| IRH    |  1   |        |
| ALU    |  2   |   2    |
| TMP    |  3   |   3    |
| RAM    |  4   |   4    |
| PC     |  5   |   5    |
| PCINC  |      |   1    |
| ACC    |  6   |        |
| ADDR   |  7   |        |
| xADDR0 |  8   |        |
| xADDR1 |  9   |        |
| xADDR2 |  10  |        |
| xMEM   |  11  |  6     |