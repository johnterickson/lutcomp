extern crate strum;
use strum::IntoEnumIterator;

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
        let bytes = self.pack().unwrap();
        [bytes[2], bytes[1], bytes[0]]
    }

    pub fn to_index(&self) -> u32 {
        let lut_entry_bytes = self.pack_lsb();
        let lut_entry_bytes = [
            lut_entry_bytes[0],
            lut_entry_bytes[1],
            lut_entry_bytes[2],
            0,
        ];
        u32::from_le_bytes(lut_entry_bytes)
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

        let carry_in = entry.op as u8 % 2;
        let out = match entry.op {
            AluOpcode::AddLoNoCarry | AluOpcode::AddLoCarry => {
                entry.in1.wrapping_add(entry.in2).wrapping_add(carry_in)
            }
            AluOpcode::AddHiNoCarry | AluOpcode::AddHiCarry => {
                let sum = (entry.in1 as u16) + (entry.in2 as u16) + (carry_in as u16);

                let mut flags = Flags::from_bits_truncate(0);
                if sum > 0xFF {
                    flags.insert(Flags::CARRY);
                }
                if (sum & 0xFF) == 0 {
                    flags.insert(Flags::ZERO);
                }
                if sum & 0x80 == 0x80 {
                    flags.insert(Flags::NEG);
                }
                flags.bits()
            }
            AluOpcode::Or => entry.in1 | entry.in2,
            AluOpcode::Reserved5 => 0xFF,
            AluOpcode::And => entry.in1 & entry.in2,
            AluOpcode::Special => {
                let special_mode = SpecialArgs::unpack(&[entry.in2]).unwrap();
                match special_mode.op {
                    SpecialOpcode::MicroHelper => match SpecialMicroHelper::iter()
                        .filter(|o| *o as u8 == *special_mode.mode_args)
                        .next()
                    {
                        Some(helper) => {
                            match helper {
                                SpecialMicroHelper::AllBitsIfOdd => {
                                    if entry.in1 & 0x1 == 0 {
                                        0x00
                                    } else {
                                        0xFF
                                    }
                                }
                                SpecialMicroHelper::LeftShiftByOne => entry.in1 << 1,
                                SpecialMicroHelper::RightShiftByOne => entry.in1 >> 1,
                                SpecialMicroHelper::SwapNibbles => {
                                    (entry.in1 >> 4) | (entry.in1 << 4)
                                }
                                SpecialMicroHelper::Decrement => entry.in1.wrapping_add(0xFF),
                                SpecialMicroHelper::Negate => (entry.in1 ^ 0xFF).wrapping_add(0x01),
                                SpecialMicroHelper::Pow2Mask => {
                                    ((1u64 << (entry.in1 & 0x1F)) - 1) as u8
                                }
                                SpecialMicroHelper::Invert => (entry.in1 ^ 0xFF),
                                SpecialMicroHelper::Max => {
                                    panic!();
                                }
                            }
                        }
                        None => 0xFF,
                    },
                    SpecialOpcode::Shift => {
                        let args = ShiftArgs::unpack(&[*special_mode.mode_args]).unwrap();
                        let left_amount = args.left_amount();
                        if print {
                            println!("# {:?} left_amount_signed:{}", &args, &left_amount);
                        }
                        let abs_amount = left_amount.abs();
                        if left_amount < -8 || left_amount > 8 {
                            0xFF
                        } else if abs_amount == 0 || abs_amount == 8 {
                            entry.in1
                        } else {
                            assert!(abs_amount <= 7);
                            match args.mode {
                                ShiftMode::Rotate => {
                                    if left_amount > 0 {
                                        (entry.in1 << abs_amount) | (entry.in1 >> (8 - abs_amount))
                                    } else {
                                        (entry.in1 >> abs_amount) | (entry.in1 << (8 - abs_amount))
                                    }
                                }
                                ShiftMode::Logical => {
                                    if left_amount > 0 {
                                        entry.in1 << abs_amount
                                    } else {
                                        entry.in1 >> abs_amount
                                    }
                                }
                                ShiftMode::Arithmetic => {
                                    let signed = entry.in1 as i8;
                                    (if left_amount > 0 {
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

        assert_eq!([240, 15, AluOpcode::Or as u8], lut_entry.pack_lsb());
    }

    fn test_special_micro(op: SpecialMicroHelper, in1: u8, expected: u8) {
        let lut_entry = LutEntry {
            in1,
            in2: (SpecialArgs {
                op: SpecialOpcode::MicroHelper,
                mode_args: (op as u8).into(),
            })
            .pack().unwrap()[0],
            op: AluOpcode::Special,
        };

        assert_eq!(expected, ALU[lut_entry.to_index() as usize]);
    }

    #[test]
    fn pow2_mask() {
        test_special_micro(SpecialMicroHelper::Pow2Mask, 3, 0x7);
        test_special_micro(SpecialMicroHelper::Pow2Mask, 2, 0x3);
        test_special_micro(SpecialMicroHelper::Pow2Mask, 1, 0x1);
        test_special_micro(SpecialMicroHelper::Pow2Mask, 0, 0x0);
    }

    #[test]
    fn all_bits_if_odd() {
        test_special_micro(SpecialMicroHelper::AllBitsIfOdd, 0x7, 0xFF);
        test_special_micro(SpecialMicroHelper::AllBitsIfOdd, 0x6, 0x00);
    }

    #[test]
    fn swap_nibbles() {
        test_special_micro(SpecialMicroHelper::SwapNibbles, 0x34, 0x43);
    }

    #[test]
    fn decrement() {
        test_special_micro(SpecialMicroHelper::Decrement, 0x17, 0x16);
        test_special_micro(SpecialMicroHelper::Decrement, 0x00, 0xFF);
    }

    #[test]
    fn negate() {
        test_special_micro(SpecialMicroHelper::Negate, 0x01, 0xFF);
        test_special_micro(SpecialMicroHelper::Negate, 0xFF, 0x01);
    }

    fn test_shift(mode: ShiftMode, left_amount: i8, in1: u8, expected: u8) {
        let shift_args = ShiftArgs {
            mode,
            unsigned_left_amount: left_amount.into()
        };

        let args = SpecialArgs {
            op: SpecialOpcode::Shift,
            mode_args: shift_args.pack().unwrap()[0].into(),
        };

        let lut_entry = LutEntry {
            in1,
            in2: args.pack().unwrap()[0],
            op: AluOpcode::Special,
        };

        let index = lut_entry.to_index() as usize;
        let actual = ALU[index];
        assert_eq!(
            expected, actual,
            "{:?} {} {:02x} -> Expected {:02x} but found {:02x} at ALU[{:05x}]",
            mode, left_amount, in1, expected, actual, index);
    }

    #[test]
    fn rotate() {
        test_shift(ShiftMode::Rotate, 0, 0x0F, 0x0F);
        test_shift(ShiftMode::Rotate, 1, 0x0F, 0x1E);
        test_shift(ShiftMode::Rotate, 2, 0x0F, 0x3C);
        test_shift(ShiftMode::Rotate, 3, 0x0F, 0x78);
        test_shift(ShiftMode::Rotate, 4, 0x0F, 0xF0);
        test_shift(ShiftMode::Rotate, 5, 0x0F, 0xE1);
        test_shift(ShiftMode::Rotate, 6, 0x0F, 0xC3);
        test_shift(ShiftMode::Rotate, 7, 0x0F, 0x87);
        test_shift(ShiftMode::Rotate, 8, 0x0F, 0x0F);

        test_shift(ShiftMode::Rotate, -8, 0x0F, 0x0F);
        test_shift(ShiftMode::Rotate, -7, 0x0F, 0x1E);
        test_shift(ShiftMode::Rotate, -6, 0x0F, 0x3C);
        test_shift(ShiftMode::Rotate, -5, 0x0F, 0x78);
        test_shift(ShiftMode::Rotate, -4, 0x0F, 0xF0);
        test_shift(ShiftMode::Rotate, -3, 0x0F, 0xE1);
        test_shift(ShiftMode::Rotate, -2, 0x0F, 0xC3);
        test_shift(ShiftMode::Rotate, -1, 0x0F, 0x87);
        test_shift(ShiftMode::Rotate,  0, 0x0F, 0x0F);

        test_shift(ShiftMode::Rotate, 4, 0x23, 0x32);
        test_shift(ShiftMode::Rotate, -4, 0x23, 0x32);
    }

    #[test]
    fn shift() {
        test_shift(ShiftMode::Arithmetic, -1, 0x84, 0xC2);
        test_shift(ShiftMode::Logical,-1, 0x84, 0x42);
        test_shift(ShiftMode::Arithmetic, 1, 0x42, 0x84);
        test_shift(ShiftMode::Logical,1, 0x42, 0x84);
    }
}
