extern crate strum;

extern crate packed_struct;
#[macro_use]
extern crate packed_struct_codegen;
use packed_struct::prelude::*;

use common::*;

use lazy_static::lazy_static;
lazy_static! {
    pub static ref ALU: Vec<u8> = alu(false);
}

#[derive(Debug, PackedStruct)]
#[packed_struct(size_bytes = "3", endian = "lsb", bit_numbering = "lsb0")]
pub struct LutEntry {
    #[packed_field(bits = "0..=7")]
    pub in2: u8,
    #[packed_field(bits = "8..=15")]
    pub in1: u8,
    #[packed_field(bits = "16..=18", ty = "enum")]
    pub op: AluOpcode,
}

impl LutEntry {
    pub fn pack_lsb(&self) -> [u8; 3] {
        let bytes = self.pack();
        [bytes[2],bytes[1],bytes[0]]
    }

    pub fn unpack_lsb(index: u32) -> LutEntry {
        let bytes = index.to_le_bytes();
        assert_eq!(0, bytes[3]);
        let bytes = [bytes[2], bytes[1], bytes[0]];
        LutEntry::unpack(&bytes).unwrap()
    }
}

pub fn alu(print: bool) -> Vec<u8> {
    if print {
        println!("v2.0 raw");
    }
    let mut out_bytes = Vec::new();

    for encoded_entry in 0u32..=0x7FFFF {
        let bytes = encoded_entry.to_le_bytes();
        assert_eq!(0, bytes[3]);
        let entry = LutEntry::unpack_lsb(encoded_entry);
        if print {
            println!("# {:05x} {:?} {:?}", encoded_entry, &bytes, &entry);
        }

        let out = match entry.op {
            AluOpcode::AddLo => entry.in1.wrapping_add(entry.in2),
            AluOpcode::AddHi => {
                let sum = (entry.in1 as u16) + (entry.in2 as u16);

                let mut flags = Flags::from_bits_truncate(0);
                if sum > 0xFF {
                    flags.insert(Flags::CARRY);
                }
                if sum == 0 {
                    flags.insert(Flags::ZERO);
                }
                if sum & 0x80 == 0x80 {
                    flags.insert(Flags::NEG);
                }
                flags.bits()
            }
            AluOpcode::Or => entry.in1 | entry.in2,
            AluOpcode::Xor => entry.in1 ^ entry.in2,
            AluOpcode::And => entry.in1 & entry.in2,
            AluOpcode::Special => {
                let special_mode = SpecialArgs::unpack(&[entry.in2]).unwrap();
                match special_mode.op {
                    SpecialOpcode::Shift => {
                        let args = ShiftArgs::unpack(&[entry.in2]).unwrap();
                        if print {
                            println!("# {:?}", &args);
                        }
                        let amount = *args.amount;
                        let abs_amount = amount.abs();
                        if abs_amount >= 8 {
                            0xFF
                        } else if abs_amount == 0 {
                            entry.in1
                        } else {
                            match args.mode {
                                ShiftMode::Rotate => {
                                    if amount > 0 {
                                        (entry.in1 << abs_amount) | (entry.in1 >> (8 - abs_amount))
                                    } else {
                                        (entry.in1 >> abs_amount) | (entry.in1 << (8 - abs_amount))
                                    }
                                }
                                ShiftMode::Arithmetic => {
                                    if amount > 0 {
                                        entry.in1 << abs_amount
                                    } else {
                                        entry.in1 >> abs_amount
                                    }
                                }
                                ShiftMode::Logical => {
                                    let signed = entry.in1 as i8;
                                    (if amount > 0 {
                                        signed << abs_amount
                                    } else {
                                        signed >> abs_amount
                                    }) as u8
                                }
                                ShiftMode::Reserved3 => 0xFF,
                            }
                        }
                    }
                    _ => 0xFF,
                }
            }
            AluOpcode::MultiplyLo => {
                ((((entry.in1 as u16) * (entry.in2 as u16)) >> 0) & 0xFF) as u8
            }
            AluOpcode::MultiplyHi => {
                ((((entry.in1 as u16) * (entry.in2 as u16)) >> 8) & 0xFF) as u8
            }
        };

        if print {
            println!("{:02x}", out);
        }
        out_bytes.push(out);
    }

    out_bytes
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pack() {
        let lut_entry = LutEntry {
            in1: 15,
            in2: 240,
            op: AluOpcode::Or,
        };

        assert_eq!([240, 15, 2], lut_entry.pack_lsb());
    }
}