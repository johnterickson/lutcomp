extern crate strum;
#[macro_use]
extern crate strum_macros;

extern crate packed_struct;
extern crate packed_struct_codegen;
use std::collections::BTreeMap;

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
pub enum AluOpcode {
    AddLoNoCarry = 0,
    AddLoCarry = 1,
    AddHiNoCarry = 2,
    AddHiCarry = 3,
    Or = 4,
    Reserved5 = 5,
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

#[derive(Debug, PackedStruct)]
#[packed_struct(size_bytes = "1", endian = "lsb", bit_numbering = "lsb0")]
pub struct ShiftArgs {
    #[packed_field(bits = "0..=3")]
    pub left_amount: Integer<i8, packed_bits::Bits::<4>>,
    #[packed_field(bits = "4..=5", ty = "enum")]
    pub mode: ShiftMode,
}

#[derive(Clone, Copy, Display, Debug, PartialEq, PartialOrd)]
#[derive(EnumCount, EnumIter, EnumString)]
#[derive(PrimitiveEnum_u8)]
pub enum SpecialMicroHelper {
    AllBitsIfOdd = 0,
    LeftShiftByOne = 1,
    RightShiftByOne = 2,
    SwapNibbles = 3,
    Decrement = 4,
    Negate = 5,
    Pow2Mask = 6,
    Invert = 7,
    GetInfo = 0x39,
    Max=0x40,
}

#[derive(Clone, Copy, Display, Debug, PartialEq, PartialOrd)]
#[derive(EnumCount, EnumIter, EnumString)]
#[derive(PrimitiveEnum_u8)]
pub enum SpecialMicroHelperInfo {
    VersionHi = 1,
    VersionLo = 2,
    VersionPatch = 3,
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
    pub mode_args: Integer<u8, packed_bits::Bits::<6>>,
    #[packed_field(bits = "6..=7", ty = "enum")]
    pub op: SpecialOpcode,
}

#[derive(Clone, Copy, Display, Debug, PartialEq)]
#[derive(EnumCount, EnumIter, EnumString)]
#[derive(PrimitiveEnum_u8)]
#[strum(serialize_all = "lowercase")]
pub enum Opcode {
    LoadImm8 = 0, // regA <- [8-bit constant B]
    Invert8 = 3, // ~regA -> regA
    Negate8 = 4, // (~regA + 1) -> regA + FLAGS

    Load8 = 0x10,     // 8-bit MEM[24-bit RegA] -> RegB
    Store8 = 0x11,    // Reg A -> 8-bit MEM[24-bit RegB]
    TtyIn = 0x12,  // TTY -> RegA
    TtyOut = 0x13, // RegA -> TTY
    Push8 = 0x14, // Reg_SP -= 1; RegA -> 8-bit MEM[Reg_SP]
    Pop8 = 0x15, // 8-bit MEM[Reg_SP] -> RegA;  Reg_SP += 1; 

    Mul8Part1 = 0x20, // 8-bit LSB RegA * 8-bit LSB RegB -> 16-bit LSB R0R1
    Mul8Part2 = 0x21,
    Add8 = 0x22, // carry + 8bit regA + 8bit regB -> 8bit regC + carry
    Add8NoCarry = 0x23, // 8bit regA + 8bit regB -> 8bit regC
    Add8NoCarryIn = 0x24, // 8bit regA + 8bit regB -> 8bit regC
    Cmp8 = 0x25, // 8bit regB - 8bit regA -> FLAGS
    Cmp8IfZero = 0x26, // if Flags & ZERO { 8bit regB - 8bit regA } else { Flags } -> Flags

    AndImm8 = 0x30, // regA &= [8-bit constant B]
    OrImm8 = 0x31, // regA |= [8-bit constant B]
    XorImm8 = 0x32, // regA ^= [8-bit constant B]
    And8 = 0x33, // regA & regB -> regC + FLAGS
    Or8 = 0x34, // regA | regB -> regC + FLAGS
    Xor8 = 0x35, // regA ^ regB -> regC + FLAGS
    ShiftImm8 = 0x36,  // shift((ShiftMode)regA, (left_amount)regB, regC)

    JmpImm = 0x40, // pc <- [24-bit constant ABC]
    JcImm = 0x41,  // if Flags & CARRY { pc <- [24-bit constant ABC] }
    JzImm = 0x42,  // if Flags & ZERO { px <- [24-bit constant ABC] }
    JnImm = 0x43,  // if Flags & NEG { pc <- [24 LSB of Reg A] }
    JmpReg = 0x48, // pc <- 24 LSB of Reg A
    JmpMem = 0x49, // pc <- MEM[24 LSB of Reg A]

    LoadImm32 = 0x80, // regA <- [32-bit constant BCDE]
    Copy32 = 0x81,    // regA -> regB

    Load32 = 0x90,       // 32-bit MEM[24-bit RegA] -> RegB
    Store32Part1 = 0x92, // RegA -> 32-bit MEM[24-bit RegB]
    Store32Part2 = 0x93, // [none] must follow Part1
    StoreImm32,          // 32-bit MEM[24-bit RegA] <- [32-bit constant BCDE]

    Add32NoCarryIn = 0xA0, // regA + regB -> regC [carry out undefined]
    Add32Part1 = 0xA1,      // 32-bit carry + regA + regB -> regC + carry
    Add32Part2 = 0xA2,      // [none] must follow Part1
    AddImm32IgnoreCarry = 0xA3, // RegA += [32-bit constant BCDE] [carry out unchanged]

    Or32 = 0xB0,  // regA | regB -> regC
    And32 = 0xB1, // regA & regB -> regC
    OrImm32 = 0xB2,  // regA |= [32-bit constant BCDE]
    AndImm32 = 0xB3, // regA &= [32-bit constant BCDE]

    HaltRAM = 0xCC,
    Halt = 0xFF,
}

pub enum RegOperation {
    Read,
    Write,
}

pub struct RegUsage {
    pub pc_offset: u32,
    pub reg_count: u32,
    pub op: RegOperation,
}

impl Opcode {
    pub fn reg_usages(&self) -> &[RegUsage] {
        match &self {
            Opcode::LoadImm8 => &[RegUsage {
                pc_offset: 1,
                reg_count: 1,
                op: RegOperation::Write,
            }],
            Opcode::Invert8 => todo!(),
            Opcode::Negate8 => todo!(),
            Opcode::Load8 => todo!(),
            Opcode::Store8 => todo!(),
            Opcode::TtyIn => todo!(),
            Opcode::TtyOut => todo!(),
            Opcode::Push8 => todo!(),
            Opcode::Pop8 => todo!(),
            Opcode::Mul8Part1 => todo!(),
            Opcode::Mul8Part2 => todo!(),
            Opcode::Add8 => todo!(),
            Opcode::Add8NoCarry => todo!(),
            Opcode::Add8NoCarryIn => todo!(),
            Opcode::Cmp8 => todo!(),
            Opcode::Cmp8IfZero => todo!(),
            Opcode::AndImm8 => todo!(),
            Opcode::OrImm8 => todo!(),
            Opcode::XorImm8 => todo!(),
            Opcode::And8 => todo!(),
            Opcode::Or8 => todo!(),
            Opcode::Xor8 => todo!(),
            Opcode::ShiftImm8 => todo!(),
            Opcode::JmpImm => todo!(),
            Opcode::JcImm => todo!(),
            Opcode::JzImm => todo!(),
            Opcode::JnImm => todo!(),
            Opcode::JmpReg => todo!(),
            Opcode::JmpMem => todo!(),
            Opcode::LoadImm32 => todo!(),
            Opcode::Copy32 => todo!(),
            Opcode::Load32 => todo!(),
            Opcode::Store32Part1 => todo!(),
            Opcode::Store32Part2 => todo!(),
            Opcode::StoreImm32 => todo!(),
            Opcode::Add32NoCarryIn => todo!(),
            Opcode::Add32Part1 => todo!(),
            Opcode::Add32Part2 => todo!(),
            Opcode::AddImm32IgnoreCarry => todo!(),
            Opcode::Or32 => todo!(),
            Opcode::And32 => todo!(),
            Opcode::OrImm32 => todo!(),
            Opcode::AndImm32 => todo!(),
            Opcode::HaltRAM => todo!(),
            Opcode::Halt => todo!(),
        }
    }
    pub fn expected_arg_sizes(&self) -> &[u32] {
        match &self {
            Opcode::LoadImm8 => &[1,1],
            Opcode::Invert8 => &[1],
            Opcode::Negate8 => &[1],
            Opcode::Load8 => &[1,1],
            Opcode::Store8 => &[1,1],
            Opcode::TtyIn => &[1],
            Opcode::TtyOut => &[1],
            Opcode::Push8 => &[1],
            Opcode::Pop8 => &[1],
            Opcode::Mul8Part1 => &[1,1],
            Opcode::Mul8Part2 => &[],
            Opcode::Add8 => &[1,1,1],
            Opcode::Add8NoCarry => &[1,1,1],
            Opcode::Add8NoCarryIn => &[1,1,1],
            Opcode::Cmp8 => &[1,1],
            Opcode::Cmp8IfZero => &[1,1],
            Opcode::AndImm8 => &[1,1],
            Opcode::OrImm8 => &[1,1],
            Opcode::XorImm8 => &[1,1],
            Opcode::And8 => &[1,1,1],
            Opcode::Or8 => &[1,1,1],
            Opcode::Xor8 => &[1,1,1],
            Opcode::ShiftImm8 => &[1,1,1],
            Opcode::JmpImm => &[3],
            Opcode::JcImm => &[3],
            Opcode::JzImm => &[3],
            Opcode::JnImm => &[3],
            Opcode::JmpReg => &[1],
            Opcode::JmpMem => &[1],
            Opcode::LoadImm32 => &[1,4],
            Opcode::Copy32 => &[1,1],
            Opcode::Load32 => &[1,1],
            Opcode::Store32Part1 => &[1,1],
            Opcode::Store32Part2 => &[],
            Opcode::StoreImm32 => &[1,4],
            Opcode::Add32NoCarryIn => &[1,1,1],
            Opcode::Add32Part1 => &[1,1,1],
            Opcode::Add32Part2 => &[],
            Opcode::AddImm32IgnoreCarry => &[1,4],
            Opcode::Or32 => &[1,1,1],
            Opcode::And32 => &[1,1,1],
            Opcode::OrImm32 => &[1,4],
            Opcode::AndImm32 => &[1,4],
            Opcode::Halt => &[],
            Opcode::HaltRAM => &[],
        }
    }
}

pub const REG_SP: u8 = 0xC;
pub const REGISTER_COUNT: u32 = 256;

#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub struct Register(pub u8);

impl std::fmt::Debug for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "r0x{:02x}", self.0)
    }
}

pub const MEM_BITS_PER_CHIP: u32 = 19;
pub const CHIP_ADDRESS_MASK: u32 = (1 << MEM_BITS_PER_CHIP) - 1;
pub const ADDRESS_BITS: u32 = 20;

pub const ROM_MIN: u32 = 0;
pub const ROM_SIZE: u32 = 1 << MEM_BITS_PER_CHIP;
pub const ROM_MAX: u32 = ROM_MIN + ROM_SIZE - 1;

pub const RAM_MIN: u32 = ROM_MAX + 1;
pub const RAM_SIZE: u32 = 1 << MEM_BITS_PER_CHIP;
pub const RAM_MAX: u32 = RAM_MIN + RAM_SIZE - 1;

#[derive(Clone,Debug,Default)]
pub struct Symbol {
    pub notes: Vec<String>,
}

#[derive(Clone,Debug,Default)]
pub struct Image {
    pub start_addr: u32,
    pub bytes: Vec<u8>,
    pub symbols: BTreeMap<u32,Symbol>,
}