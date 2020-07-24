extern crate strum;
#[macro_use]
extern crate strum_macros;

extern crate packed_struct;
#[macro_use]
extern crate packed_struct_codegen;
use packed_struct::prelude::*;

#[macro_use]
extern crate bitflags;

bitflags! {
    pub struct Flags: u8 {
        const CARRY = 0b0001;
        const ZERO = 0b0010;
        const NEG = 0b0100;
    }
}

#[derive(Clone, Copy, Display, Debug, PartialEq)]
#[derive(EnumCount, EnumIter, EnumString)]
#[derive(PrimitiveEnum_u8)]
#[strum(serialize_all = "lowercase")]
pub enum ShiftMode {
    Rotate = 0,
    Logical = 1,
    Arithmetic = 2,
    Reserved3 = 3,
}

#[derive(Clone, Copy, Display, Debug, PartialEq)]
#[derive(EnumCount, EnumIter, EnumString)]
#[derive(PrimitiveEnum_u8)]
pub enum AluOpcode {
    AddLoNoCarry = 0,
    AddLoCarry = 1,
    AddHiNoCarry = 2,
    AddHiCarry = 3,
    Or = 4,
    Xor = 5,
    And = 6,
    Special = 7,
}

impl AluOpcode {
    pub fn addlo(f: Flags) -> AluOpcode {
        if f.contains(Flags::CARRY) {
            AluOpcode::AddLoCarry
        } else {
            AluOpcode::AddLoNoCarry
        }
    }

    pub fn addhi(f: Flags) -> AluOpcode {
        if f.contains(Flags::CARRY) {
            AluOpcode::AddHiCarry
        } else {
            AluOpcode::AddHiNoCarry
        }
    }
}

#[derive(Clone, Copy, Display, Debug, PartialEq)]
#[derive(EnumCount, EnumIter, EnumString)]
#[derive(PrimitiveEnum_u8)]
#[strum(serialize_all = "lowercase")]
pub enum RwRegister {
    W = 0,
    X = 1,
    Y = 2,
    Z = 3,
}

#[derive(Debug, PackedStruct)]
#[packed_struct(size_bytes = "1", endian = "lsb", bit_numbering = "lsb0")]
pub struct ShiftArgs {
    #[packed_field(bits = "0..=3")]
    pub amount: Integer<i8, packed_bits::Bits4>,
    #[packed_field(bits = "4..=5", ty = "enum")]
    pub mode: ShiftMode,
}

#[derive(Clone, Copy, Display, Debug, PartialEq)]
#[derive(EnumCount, EnumIter, EnumString)]
#[derive(PrimitiveEnum_u8)]
pub enum SpecialMicroHelper {
    AllBitsIfOdd = 0,
    LeftShiftByOne = 1,
    RightShiftByOne = 2,
}

#[derive(Clone, Copy, Display, Debug, PartialEq)]
#[derive(EnumCount, EnumIter, EnumString)]
#[derive(PrimitiveEnum_u8)]
pub enum SpecialOpcode {
    MicroHelper = 0,
    Shift = 1,
    Reserved2 = 2,
    Reserved3 = 3,
}

#[derive(Debug, PackedStruct)]
#[packed_struct(size_bytes = "1", endian = "lsb", bit_numbering = "lsb0")]
pub struct SpecialArgs {
    #[packed_field(bits = "0..=5")]
    pub mode_args: Integer<u8, packed_bits::Bits6>,
    #[packed_field(bits = "6..=7", ty = "enum")]
    pub op: SpecialOpcode,
}

// #[derive(Debug, PackedStruct)]
// #[packed_struct(size_bytes = "2", endian = "lsb", bit_numbering = "lsb0")]
// pub struct Instruction {
//     #[packed_field(bits = "0..=7")]
//     pub mode_specific: u8,
//     #[packed_field(bits = "8..=10", ty = "enum")]
//     pub out_reg: Register,
//     #[packed_field(bits = "11..=12", ty = "enum")]
//     pub mode: InstructionModeDiscriminants,
//     #[packed_field(bits = "13..=15", ty = "enum")]
//     pub op: Opcode,
// }

#[derive(Clone, Copy, Display, Debug, PartialEq)]
#[derive(EnumCount, EnumIter, EnumString)]
#[derive(PrimitiveEnum_u8)]
#[strum(serialize_all = "lowercase")]
pub enum Opcode {
    LoadImm = 0, // regA <- [32-bit constant BCDE]
    Add = 1,
    Or = 2, // reg? | acc -> acc
    Xor = 3,
    And = 4,
    Load = 5,
    Store = 6,
    Halt = 7,
    Jmp = 8,
    Jz = 9,
    RegsOr = 10,        // regA | regB -> regC
    FetchAbsToReg = 11, // mem[24-bit address ABCD] -> regE
    Multiply = 12,
    FetchRegToReg = 13, // mem[regA] -> regB
    RegsAddPart1 = 14,  // carry + regA + regB -> regC + carry
    RegsAddPart2 = 15,  // [none]
}
