extern crate strum;

extern crate packed_struct;
#[macro_use]
extern crate packed_struct_codegen;
use packed_struct::prelude::*;

use common::*;

#[derive(Debug, PackedStruct)]
#[packed_struct(size_bytes = "4", endian = "lsb", bit_numbering = "lsb0")]
pub struct LutEntry {
    #[packed_field(bits = "0..=7")]
    pub in2: u8,
    #[packed_field(bits = "8..=15")]
    pub in1: u8,
    #[packed_field(bits = "16..=18", ty = "enum")]
    pub op: AluOpcode,
}

pub fn alu() {
    println!("v2.0 raw");

    for encoded_entry in 0u32..=0x7FFFF {
        let bytes = encoded_entry.to_le_bytes();
        assert_eq!(0, bytes[3]);
        let entry = LutEntry::unpack(&bytes).unwrap();
        println!("# {:05x} {:?} {:?}", encoded_entry, &bytes, &entry);

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
                        println!("# {:?}", &args);
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
                    _ => 0xFF
                }
            }
            AluOpcode::MultiplyLo => ((((entry.in1 as u16) * (entry.in2 as u16)) >> 0) & 0xFF) as u8,
            AluOpcode::MultiplyHi => ((((entry.in1 as u16) * (entry.in2 as u16)) >> 8) & 0xFF) as u8,
        };

        println!("{:02x}", out);
    }
}
