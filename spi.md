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

### Instruction Register ###
| 7 | 6 | 5 | 4 | 3 2 | 1 0 |
| - | - | - | - | --- | --- |
| EOI_ | RAM !CS_ | ALU !CS_ | ALU !HOLD_ | OE_ -,X,Y,Z | CP -,X,Y,Z |

### bootup / jmp: IR == 0 --> ###
| Byte | Operation |
| - | - |
| 0 | ROM CS_=1 |
| 1 | ROM CS_=0, 0x3 -> ROM | 
| 2 | ROM CS_=0, X -> ROM |
| 3 | ROM CS_=0, Y -> ROM | 
| 4 | ROM CS_=0, Z -> ROM | 
| 5 | ROM CS_=0, ROM -> IR |


### Interesting chips ###
| Model | Description |
| - | - |
| 74138 | inverting decoder |
| 74238 | non-inverting decoder |
| 74574 | 8-bit register, dedicated in, dedicated out |
| 74161 | 4-bit counter, dedicated in, dedicated out |
| 74299 | 8-bit shift register, tri-state |