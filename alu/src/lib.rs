extern crate strum;
use std::{collections::hash_map::DefaultHasher, hash::Hasher, convert::TryInto};

use strum::IntoEnumIterator;

extern crate packed_struct;
extern crate packed_struct_codegen;
use packed_struct::prelude::*;

use common::*;

use lazy_static::lazy_static;
lazy_static! {
    static ref ALU_AND_HASH: (Vec<u8>, u32) = alu(false);
    pub static ref ALU: Vec<u8> = ALU_AND_HASH.0.clone();
    pub static ref ALU_HASH: u32 = ALU_AND_HASH.1;
}

pub const MAJOR_VERSION: u8 = 1;
pub const MINOR_VERSION: u8 = 2;
pub const PATCH_VERSION: u8 = 0;

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

pub fn alu(print: bool) -> (Vec<u8>,u32) {
    if print {
        println!("v2.0 raw");
    }
    let mut out_bytes = Vec::new();

    let mut hash_indices = Vec::new();

    for encoded_entry in 0u32..=0x7FFFF {
        let bytes = encoded_entry.to_le_bytes();
        assert_eq!(0, bytes[3]);
        let entry = LutEntry::unpack_lsb(encoded_entry);
        if print {
            println!("# {:05x} {:?} {:?}", encoded_entry, &bytes, &entry);
        }

        let carry_in = entry.op as u8 % 2;
        let out = match entry.op {
            AluOpcode::AddLo => {
                entry.in1.wrapping_add(entry.in2)
            },
            AluOpcode::MulLoHi => {
                if entry.in1 >= entry.in2 {
                    entry.in1.wrapping_mul(entry.in2)
                } else {
                    (((entry.in1 as u16 + 1) * entry.in2 as u16) / 256).try_into().unwrap()
                }
            },
            AluOpcode::AddHiNoCarry | AluOpcode::AddHiCarry => {
                let sum = (entry.in1 as u16) + (entry.in2 as u16) + (carry_in as u16);

                let mut flags = Flags::from_bits_truncate(0);
                if carry_in != 0 {
                    flags.insert(Flags::CARRY_PENDING)
                }
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
            AluOpcode::Divide => {
                if entry.in2 == 0 {
                    0xFF
                } else {
                    entry.in1 / entry.in2
                }
            }
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
                                SpecialMicroHelper::GetInfo => {
                                    let info = SpecialMicroHelperInfo::iter().filter(|o| *o as u8 == entry.in1).next();
                                    if let Some(info) = info {
                                        match info {
                                            SpecialMicroHelperInfo::Hash0 => {
                                                assert_eq!(0, hash_indices.len());
                                                hash_indices.push(out_bytes.len());
                                                0x00
                                            }
                                            SpecialMicroHelperInfo::Hash1 => {
                                                assert_eq!(1, hash_indices.len());
                                                hash_indices.push(out_bytes.len());
                                                0x01
                                            }
                                            SpecialMicroHelperInfo::Hash2 => {
                                                assert_eq!(2, hash_indices.len());
                                                hash_indices.push(out_bytes.len());
                                                0x02
                                            }
                                            SpecialMicroHelperInfo::Hash3 => {
                                                assert_eq!(3, hash_indices.len());
                                                hash_indices.push(out_bytes.len());
                                                0x03
                                            }
                                            SpecialMicroHelperInfo::VersionMajor => MAJOR_VERSION,
                                            SpecialMicroHelperInfo::VersionMinor => MINOR_VERSION,
                                            SpecialMicroHelperInfo::VersionPatch => PATCH_VERSION,
                                        }
                                    } else {
                                        0xFF
                                    }
                                }
                                SpecialMicroHelper::Max => {
                                    panic!();
                                }
                            }
                        }
                        None => 0xFF,
                    },
                    SpecialOpcode::Shift => {
                        let args = ShiftArgs::unpack(&[*special_mode.mode_args]).unwrap();
                        let left_amount = *args.left_amount;
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

    assert_eq!(4, hash_indices.len());

    let hash = {
        let mut hasher = DefaultHasher::new();
        hasher.write(&out_bytes);
        let hash = hasher.finish();
        let hash: u32 = (hash % 0x1_0000_0000).try_into().unwrap();
        hash
    };

    {
        let hash = hash.to_le_bytes();
        for (hash_byte_index, alu_byte_index) in hash_indices.iter().enumerate() {
            assert_eq!(out_bytes[*alu_byte_index], hash_byte_index.try_into().unwrap());
            out_bytes[*alu_byte_index] = hash[hash_byte_index];
        }
    }

    (out_bytes, hash)
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

    fn eval_special_micro(op: SpecialMicroHelper, in1: u8) -> u8 {
        let lut_entry = LutEntry {
            in1,
            in2: (SpecialArgs {
                op: SpecialOpcode::MicroHelper,
                mode_args: (op as u8).into(),
            })
            .pack().unwrap()[0],
            op: AluOpcode::Special,
        };

        ALU[lut_entry.to_index() as usize]
    }

    fn test_special_micro(op: SpecialMicroHelper, in1: u8, expected: u8) {
        let actual = eval_special_micro(op, in1);
        assert_eq!(expected, actual, "{:?} 0x{:02x}",op,in1);
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
            left_amount: left_amount.into()
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

    #[test]
    fn version_bump() {

        let version = [
            eval_special_micro(SpecialMicroHelper::GetInfo, SpecialMicroHelperInfo::VersionMajor as u8),
            eval_special_micro(SpecialMicroHelper::GetInfo, SpecialMicroHelperInfo::VersionMinor as u8),
            eval_special_micro(SpecialMicroHelper::GetInfo, SpecialMicroHelperInfo::VersionPatch as u8),
        ];
        assert_eq!([MAJOR_VERSION, MINOR_VERSION, PATCH_VERSION], version);

        // if you have to change this, also change the version above
        let hash = u32::from_le_bytes([
            eval_special_micro(SpecialMicroHelper::GetInfo, SpecialMicroHelperInfo::Hash0 as u8),
            eval_special_micro(SpecialMicroHelper::GetInfo, SpecialMicroHelperInfo::Hash1 as u8),
            eval_special_micro(SpecialMicroHelper::GetInfo, SpecialMicroHelperInfo::Hash2 as u8),
            eval_special_micro(SpecialMicroHelper::GetInfo, SpecialMicroHelperInfo::Hash3 as u8),
        ]);

        assert_eq!(1822093141, hash); // if you have to change this, also change the version

        let mut hasher = DefaultHasher::new();
        hasher.write(&ALU);
        let whole_hash = hasher.finish();
        assert_eq!(4006551507680735044, whole_hash); // if you have to change this, but not the above then something is screwy
    }

    #[test]
    fn mul() {
        for x in 0u8..=0xFF {
            for y in 0u8..=0xFF {
                let product = (x as u16) * (y as u16);

                let mullo = |x, y| {
                    let (in1, in2) = if x >= y {
                        (x,y)
                    } else {
                        (y,x)
                    };

                    assert!(in1 >= in2);

                    let index = LutEntry { in1, in2, op: AluOpcode::MulLoHi }.to_index() as usize;
                    ALU[index]
                };

                assert_eq!(
                    mullo(x,y),
                    (product % 256).try_into().unwrap()
                    );

                let mulhi = |x: u8, y: u8| {
                    
                    let (in1, in2) = match (x,y) {
                        (0u8, _) | (_, 0u8) => return 0,
                        (a, b) if a < b => (a.checked_sub(1u8).unwrap(), b),
                        (a, b) if a >= b => (b.checked_sub(1u8).unwrap(), a),
                        _ => unreachable!(),
                    };

                    assert!(in1 < in2, "For mulhi({}*{}) Expected {} < {}", x, y, in1, in2);

                    let index = LutEntry { in1, in2, op: AluOpcode::MulLoHi }.to_index() as usize;
                    ALU[index]
                };

                assert_eq!(
                    mulhi(x,y),
                    (product / 256).try_into().unwrap()
                    );
            }
        }
    }
}
