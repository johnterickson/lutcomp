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
        const CARRY_PENDING = 0b1000;
    }
}

#[derive(Clone, Copy, Display, Debug, PartialEq)]
#[derive(EnumCount, EnumIter, EnumString)]
#[derive(PrimitiveEnum_u8)]
pub enum AluOpcode {
    AddLo = 0,
    MulLoHi = 1,
    AddHiNoCarry = 2,
    AddHiCarry = 3,
    Or = 4,
    Divide = 5,
    And = 6,
    Special = 7,
}

impl AluOpcode {
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
    GetInfo = 0x0F,
    Max=0x40,
}

#[derive(Clone, Copy, Display, Debug, PartialEq, PartialOrd)]
#[derive(EnumCount, EnumIter, EnumString)]
#[derive(PrimitiveEnum_u8)]
pub enum SpecialMicroHelperInfo {
    Hash0 = 0,
    Hash1 = 1,
    Hash2 = 2,
    Hash3 = 3,
    VersionMajor = 4,
    VersionMinor = 5,
    VersionPatch = 6,
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
pub enum ShiftDirection {
    Left = 0,
    Right = 1,
}

#[derive(Debug, PackedStruct)]
#[packed_struct(size_bytes = "1", endian = "lsb", bit_numbering = "lsb0")]
pub struct ShiftCommand {
    #[packed_field(bits = "0..=1", ty = "enum")]
    pub mode: ShiftMode,
    #[packed_field(bits = "2..=2", ty = "enum")]
    pub dir: ShiftDirection,
}

#[derive(Clone, Copy, Display, Debug, PartialEq)]
#[derive(EnumCount, EnumIter, EnumString)]
#[derive(PrimitiveEnum_u8)]
#[strum(serialize_all = "lowercase")]
pub enum Opcode {
    LoadImm8 = 0, // regA <- [8-bit constant B]
    Invert8 = 3, // ~regA -> regA
    Negate8 = 4, // (~regA + 1) -> regA + FLAGS
    ClearCarry = 0xE, // FLAGS & ~CARRY -> FLAGS
    Init = 0xF, // Flags and all internal regs to zero

    Load8 = 0x10,     // 8-bit MEM[24-bit RegA] -> RegB
    Store8 = 0x11,    // Reg A -> 8-bit MEM[24-bit RegB]
    TtyIn = 0x12,  // TTY -> RegA
    TtyOut = 0x13, // RegA -> TTY
    Push8 = 0x14, // Reg_SP -= 1; RegA -> 8-bit MEM[Reg_SP]
    Pop8 = 0x15, // 8-bit MEM[Reg_SP] -> RegA;  Reg_SP += 1;
    Copy8 = 0x16, // regA -> regB

    Mul8_8 = 0x20, // 8-bit LSB RegA * 8-bit LSB RegB -> 8-bit LSB RegC
    Mul8_16 = 0x21, // 8-bit LSB RegA * 8-bit LSB RegB -> 16-bit LSB RegC
    AddCarry8 = 0x22, // carry + 8bit regA + 8bit regB -> 8bit regC + carry
    Add8NoCarry = 0x23, // 8bit regA + 8bit regB -> 8bit regC
    Add8NoCarryIn = 0x24, // 8bit regA + 8bit regB -> 8bit regC
    Cmp8 = 0x25, // 8bit regB - 8bit regA -> FLAGS
    Cmp8IfZero = 0x26, // if Flags & ZERO { 8bit regB - 8bit regA } else { Flags } -> Flags
    Divide8 = 0x27, // carry + 8bit regA / 8bit regB -> 8 bit regC + ZERO

    AndImm8 = 0x30, // regA &= [8-bit constant B]
    OrImm8 = 0x31, // regA |= [8-bit constant B]
    XorImm8 = 0x32, // regA ^= [8-bit constant B]
    And8 = 0x33, // regA & regB -> regC + FLAGS
    Or8 = 0x34, // regA | regB -> regC + FLAGS
    Xor8 = 0x35, // regA ^ regB -> regC + FLAGS
    Shift8 = 0x36,  // shift((ShiftCommand)regA, (amount)regA, (value) regB, (dest) regC)

    JmpImm = 0x40, // pc <- [24-bit constant ABC]
    JcImm = 0x41,  // if Flags & CARRY { pc <- [24-bit constant ABC] }
    JzImm = 0x42,  // if Flags & ZERO { px <- [24-bit constant ABC] }
    JnImm = 0x43,  // if Flags & NEG { pc <- [24 LSB of Reg A] }
    JmpReg = 0x48, // pc <- 24 LSB of Reg A
    JmpMem = 0x49, // pc <- MEM[24 LSB of Reg A]

    LoadImm32 = 0x80, // regA <- [32-bit constant BCDE]
    Copy32 = 0x81,    // regA -> regB

    Load32 = 0x90,       // 32-bit MEM[24-bit RegA] -> RegB
    Store32_1 = 0x92, // RegA -> 32-bit MEM[24-bit RegB]
    Store32_2 = 0x93, // [none] must follow Part1
    StoreImm32 = 0x94,          // 32-bit MEM[24-bit RegA] <- [32-bit constant BCDE]

    AddCarry32_1 = 0xA2,      // 32-bit carry + regA + regB -> regC + carry
    AddCarry32_2 = 0xA3,      // [none] must follow Part1
    AddImm32IgnoreCarry = 0xA4, // RegA += [32-bit constant BCDE] [carry out unchanged]

    Or32 = 0xB0,  // regA | regB -> regC
    And32 = 0xB1, // regA & regB -> regC
    OrImm32 = 0xB2,  // regA |= [32-bit constant BCDE]
    AndImm32 = 0xB3, // regA &= [32-bit constant BCDE]

    HaltRAM = 0xCC, // imm32 halt code
    GetUcodeInfo = 0xFD, // major byte -> regA, minor byte -> regB, patch -> regC, 32-bit hash regD
    GetAluInfo = 0xFE, // SpecialMicroHelperInfo(regA), 8-bit value regB
    Halt = 0xFF, // imm32 halt code
}

#[derive(Clone, Copy, Display, Debug, PartialEq)]
#[derive(EnumCount, EnumIter, EnumString)]
#[derive(PrimitiveEnum_u32)]
pub enum HaltCode {
    Success = 0x0000_0000,
    TestFail = 0x0000_0001,
    CompilerUnreachable = 0x1000_0000,
    BadInstructionRAM = 0xCCCC_CCCC,
    BadInstructionROM = 0xFFFF_FFFF,
}

pub enum RegOperation {
    Read,
    Write,
}

impl Opcode {
    pub fn expected_arg_sizes(&self) -> &[u32] {
        match &self {
            Opcode::LoadImm8 => &[1,1],
            Opcode::Invert8 => &[1],
            Opcode::Negate8 => &[1],
            Opcode::ClearCarry => &[],
            Opcode::Init => &[],
            Opcode::Load8 => &[1,1],
            Opcode::Store8 => &[1,1],
            Opcode::TtyIn => &[1],
            Opcode::TtyOut => &[1],
            Opcode::Push8 => &[1],
            Opcode::Pop8 => &[1],
            Opcode::Copy8 => &[1,1],
            Opcode::Mul8_8 => &[1,1,1],
            Opcode::Mul8_16 => &[1,1,1],
            Opcode::AddCarry8 => &[1,1,1],
            Opcode::Add8NoCarry => &[1,1,1],
            Opcode::Add8NoCarryIn => &[1,1,1],
            Opcode::Cmp8 => &[1,1],
            Opcode::Cmp8IfZero => &[1,1],
            Opcode::Divide8 => &[1,1,1],
            Opcode::AndImm8 => &[1,1],
            Opcode::OrImm8 => &[1,1],
            Opcode::XorImm8 => &[1,1],
            Opcode::And8 => &[1,1,1],
            Opcode::Or8 => &[1,1,1],
            Opcode::Xor8 => &[1,1,1],
            Opcode::Shift8 => &[1,1,1,1],
            Opcode::JmpImm => &[3],
            Opcode::JcImm => &[3],
            Opcode::JzImm => &[3],
            Opcode::JnImm => &[3],
            Opcode::JmpReg => &[1],
            Opcode::JmpMem => &[1],
            Opcode::LoadImm32 => &[1,4],
            Opcode::Copy32 => &[1,1],
            Opcode::Load32 => &[1,1],
            Opcode::Store32_1 => &[1,1],
            Opcode::Store32_2 => &[],
            Opcode::StoreImm32 => &[1,4],
            Opcode::AddCarry32_1 => &[1,1,1],
            Opcode::AddCarry32_2 => &[],
            Opcode::AddImm32IgnoreCarry => &[1,4],
            Opcode::Or32 => &[1,1,1],
            Opcode::And32 => &[1,1,1],
            Opcode::OrImm32 => &[1,4],
            Opcode::AndImm32 => &[1,4],
            Opcode::GetUcodeInfo => &[1,1,1,4],
            Opcode::GetAluInfo => &[1,1,1,4],
            Opcode::Halt => &[4],
            Opcode::HaltRAM => &[4],
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
    pub functions: BTreeMap<u32, (u32, String)>,
}

impl Image {
    pub fn find_containing_function(&self, addr: u32) -> Option<(u32, u32, &String)> {
        self.functions.iter()
            .find_map(|(start,(end, name))| {
                if *start <= addr && addr <= *end {
                    Some((*start, *end, name))
                } else {
                    None
                }
            })
    }
}