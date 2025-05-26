Minimizing wiring by making data bus a serial line

## Key Components 

| Function | Notes | 
| - | - |
| [CD74HC299E Shift Register](https://www.digikey.com/en/products/detail/texas-instruments/CD74HC299E/376835) | Rotate by feeding output back to input |
| [SRAM](https://www.digikey.com/en/products/detail/microchip-technology/23LCV04M-I-P/22611769) | |
| [Flash](https://www.digikey.com/en/products/detail/microchip-technology/SST25VF080B-50-4C-PAE/4740884)| |

| Chip | Needed Control Lines (excluding global RST) |
| --- | --- |
| ROM | CE_ |
| IR | |

### SDATA selector ###
| Index | Chip |
| - | - |
| 0 | ROM (R) |
| 1 | X |
| 2 | Y |
| 3 | Z | 
| 4 | RAM |
| 5 | ALU |
| 6 | CONSTANT (R) |
| 7 | ? |

### Instruction Register ###
| Bits | Meaning |
| - | - |
| 0-2 | READ sdata sel |
| 3 | ROM CS_ |
| 4-6 | WRITE sdata sel |
| 7 | IR Read |



<!-- | 9 | 8 | 7 | 6 | 5 4 3 | 2 1 0 |
| - | - | - | - | --- | --- |
| EOI_ | RAM !CS_ | ALU !CS_ | ALU !HOLD_ | OE_ ROM,X,Y,Z,RAM,ALU |  | -->

### bootup / jmp: IR == 0 --> ###
| Byte | Operation | Effective IR |
| - | - | - |
| 0 | ROM CS_=1 | 0x0F | 
| 1 | ROM CS_=0, 0x3 -> ROM | 0x60 | 
| 2 | ROM CS_=0, X -> ROM | 0x10 |
| 3 | ROM CS_=0, Y -> ROM | 0x20 |
| 4 | ROM CS_=0, Z -> ROM | 0x30 |
| 5 | ROM CS_=0, ROM -> IR | 0x87 |


### Interesting chips ###
| Model | Description |
| - | - |
| 74138 | inverting decoder |
| 74238 | non-inverting decoder |
| 74574 | 8-bit register, dedicated in, dedicated out |
| 74161 | 4-bit counter, dedicated in, dedicated out |
| 74299 | 8-bit shift register, tri-state |