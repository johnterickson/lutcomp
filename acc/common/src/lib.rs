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
    pub left_amount: Integer<i8, packed_bits::Bits4>,
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
    SwapNibbles = 3,
    Decrement = 4,
    Negate = 5,
    Pow2Mask = 6,
    Invert = 7,
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

pub const REG_SP: u8 = 0x0C;

#[derive(Clone, Copy, Display, Debug, PartialEq)]
#[derive(EnumCount, EnumIter, EnumString)]
#[derive(PrimitiveEnum_u8)]
#[strum(serialize_all = "lowercase")]
pub enum Opcode {
    LoadImm8 = 0, // regA <- [8-bit constant B]
    JmpImm = 1,      // pc <- [24-bit constant ABC]
    JzImm = 2,       // if Flags & ZERO { px <- [24-bit constant ABC]}
    Invert8 = 3, // ~regA -> regA
    Negate8 = 4, // (~regA + 1) -> regA
    JmpReg = 5, // pc <- 24 LSB of Reg A
    JnImm = 6, // pc <- [24 LSB of Reg A]
    JmpMem = 7, // pc <- MEM[24 LSB of Reg A]
    Copy8 = 0xF, // regA -> regB

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
    // Sub8NoCarry = 0x24, // 8bit regA - 8bit regB -> 8bit regC

    AndImm8 = 0x30, // regA &= [8-bit constant B]
    OrImm8 = 0x31, // regA |= [8-bit constant B]
    XorImm8 = 0x32, // regA ^= [8-bit constant B]
    And8 = 0x33, // regA & regB -> regC
    Or8 = 0x34, // regA | regB -> regC
    Xor8 = 0x35, // regA ^ regB -> regC

    LoadImm32 = 0x80, // regA <- [32-bit constant BCDE]
    Copy32 = 0x81,    // regA -> regB

    Load32 = 0x90,       // 32-bit MEM[24-bit RegA] -> RegB
    Store32Part1 = 0x92, // RegA -> 32-bit MEM[24-bit RegB]
    Store32Part2 = 0x93, // RegA -> 32-bit MEM[24-bit RegB]
    StoreImm32,          // 32-bit MEM[24-bit RegA] <- [32-bit constant BCDE]

    Add32NoCarryIn = 0xA0, // regA + regB -> regC [carry out undefined]
    Add32Part1 = 0xA1,      // 32-bit carry + regA + regB -> regC + carry
    Add32Part2 = 0xA2,      // [none] must follow Part1
    AddImm32IgnoreCarry = 0xA3, // RegA += [32-bit constant BCDE] [carry out unchanged]

    Or32 = 0xB0,  // regA | regB -> regC
    And32 = 0xB1, // regA & regB -> regC
    OrImm32 = 0xB2,  // regA |= [32-bit constant BCDE]
    AndImm32 = 0xB3, // regA &= [32-bit constant BCDE]

    Halt = 255,
}
