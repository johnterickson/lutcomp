extern crate strum;
#[macro_use]
extern crate strum_macros;

extern crate packed_struct;
#[macro_use]
extern crate packed_struct_codegen;
use packed_struct::prelude::*;
use std::collections::BTreeMap;

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
    AddLo = 0,
    AddHi = 1,
    Or = 2,
    Xor = 3,
    And = 4,
    Special = 5,
    MultiplyLo = 6,
    MultiplyHi = 7,
}

#[derive(Clone, Copy, Display, Debug, PartialEq)]
#[derive(EnumCount, EnumIter, EnumString)]
#[derive(PrimitiveEnum_u8)]
#[strum(serialize_all = "lowercase")]
pub enum Register {
    A = 0,
    B = 1,
    C = 2,
    Pc = 3,
    SerialIn = 4,
    SerialOut = 5,
    Reserved6 = 6,
    Reserved7 = 7,
}

#[derive(Clone, Copy, Display, Debug, EnumDiscriminants)]
pub enum MaybeDirect<T> {
    Value(T),
    Address(T),
}

impl<T: Copy> MaybeDirect<T> {
    pub fn split(&self) -> (bool, T) {
        match self {
            MaybeDirect::Value(r) => (false, *r),
            MaybeDirect::Address(r) => (true, *r),
        }
    }
}

#[derive(Clone, Display, Debug)]
pub enum Value {
    Constant(u8),
    Label(String, Option<u8>),
}

impl Value {
    pub fn resolve(&mut self, labels: &BTreeMap<String, u8>) {
        match self {
            Value::Constant(_) => {}
            Value::Label(label, resolved_value) => {
                *resolved_value = Some(
                    *labels
                        .get(label)
                        .expect(&format!("Could not find label '{}'", label)),
                );
            }
        }
    }
    pub fn unwrap_constant(&self) -> u8 {
        match self {
            Value::Constant(c) => *c,
            Value::Label(label, resolved_value) => {
                resolved_value.expect(&format!("Label '{}' has not been resolved", label))
            }
        }
    }
}

#[derive(Clone, Display, Debug, EnumDiscriminants)]
#[strum_discriminants(derive(Display, EnumCount, EnumIter, EnumString, PrimitiveEnum_u8))]
pub enum InstructionMode {
    Imm8(Register, Value),
    Regs(Register, MaybeDirect<Register>, MaybeDirect<Register>),
    Imm4(Register, MaybeDirect<Register>, Value),
    MemOutRegs(Register, MaybeDirect<Register>, MaybeDirect<Register>),
}

#[derive(Debug, PackedStruct)]
#[packed_struct(size_bytes = "1", endian = "lsb", bit_numbering = "lsb0")]
pub struct PackedInstructionMode1or3 {
    #[packed_field(bits = "0..=2", ty = "enum")]
    pub in1_reg: Register,
    #[packed_field(bits = "3")]
    pub in1_indirect: bool,
    #[packed_field(bits = "4..=6", ty = "enum")]
    pub in2_reg: Register,
    #[packed_field(bits = "7")]
    pub in2_indirect: bool,
}

#[derive(Debug, PackedStruct)]
#[packed_struct(size_bytes = "1", endian = "lsb", bit_numbering = "lsb0")]
pub struct PackedInstructionMode2 {
    #[packed_field(bits = "0..=3")]
    pub imm: Integer<u8, packed_bits::Bits4>,
    #[packed_field(bits = "4..=6", ty = "enum")]
    pub in1_reg: Register,
    #[packed_field(bits = "7")]
    pub in1_indirect: bool,
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
pub enum SpecialOpcode {
    Shift = 0,
    Reserved1 = 1,
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
    Jmp = 0,
    Jz = 1,
}
