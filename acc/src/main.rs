extern crate pest;
#[macro_use]
extern crate pest_derive;
use pest::error::Error;
use pest::Parser;

extern crate strum;
#[macro_use]
extern crate strum_macros;
use strum::IntoEnumIterator;

extern crate packed_struct;
#[macro_use]
extern crate packed_struct_codegen;
use packed_struct::prelude::*;

use std::collections::BTreeMap;
use std::{
    io::{BufWriter, Read},
    str::FromStr,
};

#[derive(Clone, Copy, Display, Debug, PartialEq)]
#[derive(EnumCount, EnumIter, EnumString)]
#[derive(PrimitiveEnum_u8)]
#[strum(serialize_all = "lowercase")]
enum ShiftMode {
    Rotate = 0,
    Logical = 1,
    Arithmetic = 2,
    Reserved3 = 3,
}

#[derive(Clone, Copy, Display, Debug, PartialEq)]
#[derive(EnumCount, EnumIter, EnumString)]
#[derive(PrimitiveEnum_u8)]
#[strum(serialize_all = "lowercase")]
enum Opcode {
    LoadImm = 0,
    Add = 1,
    Or = 2,
    Xor = 3,
    And = 4,
    Shift = 5,
    Equals = 6,
    Multiply = 7,
}

#[derive(Clone, Copy, Display, Debug, PartialEq)]
#[derive(EnumCount, EnumIter, EnumString)]
#[derive(PrimitiveEnum_u8)]
#[strum(serialize_all = "lowercase")]
enum Register {
    A = 0,
    B = 1,
    C = 2,
    Pc = 3,
    SerialIn = 4,
    SerialOut = 5,
    Reserved6 = 6,
    Reserved7 = 7,
}

impl Register {
    fn to_output_level(&self) -> OutputLevel {
        match self {
            Register::A => OutputLevel::A,
            Register::B => OutputLevel::B,
            Register::C => OutputLevel::C,
            Register::Pc => OutputLevel::Pc,
            Register::SerialIn => OutputLevel::TTYin,
            Register::SerialOut => OutputLevel::Halt,
            Register::Reserved6 => OutputLevel::Halt,
            Register::Reserved7 => OutputLevel::Halt,
        }
    }

    fn to_load_edge(&self) -> LoadEdge {
        match self {
            Register::A => LoadEdge::A,
            Register::B => LoadEdge::B,
            Register::C => LoadEdge::C,
            Register::Pc => panic!(),
            Register::SerialIn => LoadEdge::TTYin,
            Register::SerialOut => LoadEdge::TTYout,
            _ => unimplemented!(),
        }
    }
}

#[derive(Clone, Copy, Display, Debug)]
#[derive(EnumDiscriminants)]
enum MaybeDirect<T> {
    Value(T),
    Address(T),
}

impl<T: Copy> MaybeDirect<T> {
    fn split(&self) -> (bool, T) {
        match self {
            MaybeDirect::Value(r) => (false, *r),
            MaybeDirect::Address(r) => (true, *r),
        }
    }
}

#[derive(Clone, Display, Debug, EnumDiscriminants)]
#[strum_discriminants(derive(Display, EnumCount, EnumIter, EnumString, PrimitiveEnum_u8))]
enum InstructionMode {
    Imm8(Register, Value),
    Regs(Register, MaybeDirect<Register>, MaybeDirect<Register>),
    Imm4(Register, MaybeDirect<Register>, Value),
    MemOutRegs(Register, MaybeDirect<Register>, MaybeDirect<Register>),
}

#[derive(Debug, PackedStruct)]
#[packed_struct(size_bytes = "1", endian = "lsb", bit_numbering = "lsb0")]
pub struct PackedInstructionMode1or3 {
    #[packed_field(bits = "0..=2", ty = "enum")]
    in1_reg: Register,
    #[packed_field(bits = "3")]
    in1_indirect: bool,
    #[packed_field(bits = "4..=6", ty = "enum")]
    in2_reg: Register,
    #[packed_field(bits = "7")]
    in2_indirect: bool,
}

#[derive(Debug, PackedStruct)]
#[packed_struct(size_bytes = "1", endian = "lsb", bit_numbering = "lsb0")]
pub struct PackedInstructionMode2 {
    #[packed_field(bits = "0..=3")]
    imm: Integer<u8, packed_bits::Bits4>,
    #[packed_field(bits = "4..=6", ty = "enum")]
    in1_reg: Register,
    #[packed_field(bits = "7")]
    in1_indirect: bool,
}

#[derive(Debug, PackedStruct)]
#[packed_struct(size_bytes = "1", endian = "lsb", bit_numbering = "lsb0")]
pub struct ShiftArgs {
    #[packed_field(bits = "0..=3")]
    amount: Integer<i8, packed_bits::Bits4>,
    #[packed_field(bits = "4..=5")]
    reserved: Integer<u8, packed_bits::Bits2>,
    #[packed_field(bits = "6..=7", ty = "enum")]
    mode: ShiftMode,
}

#[derive(Debug, PackedStruct)]
#[packed_struct(size_bytes = "2", endian = "lsb", bit_numbering = "lsb0")]
pub struct Instruction {
    #[packed_field(bits = "0..=7")]
    mode_specific: u8,
    #[packed_field(bits = "8..=10", ty = "enum")]
    out_reg: Register,
    #[packed_field(bits = "11..=12", ty = "enum")]
    mode: InstructionModeDiscriminants,
    #[packed_field(bits = "13..=15", ty = "enum")]
    op: Opcode,
}

#[derive(Debug, PackedStruct)]
#[packed_struct(size_bytes = "3", endian = "lsb", bit_numbering = "lsb0")]
pub struct LutEntry {
    #[packed_field(bits = "0..=7")]
    in2: u8,
    #[packed_field(bits = "8..=15")]
    in1: u8,
    #[packed_field(bits = "16..=18", ty = "enum")]
    op: Opcode,
    #[packed_field(bits = "19..=23")]
    unused: Integer<u8, packed_bits::Bits5>,
}

fn alu() {
    println!("v2.0 raw");

    for encoded_entry in 0u32..=0x7FFFF {
        let bytes = encoded_entry.to_le_bytes();
        assert_eq!(0, bytes[3]);
        let bytes = [bytes[2], bytes[1], bytes[0]];
        let entry = LutEntry::unpack(&bytes).unwrap();
        println!("# {:05x} {:?} {:?}", encoded_entry, &bytes, &entry);
        assert_eq!(0, *entry.unused);

        let out = match entry.op {
            Opcode::LoadImm => entry.in2,
            Opcode::Add => entry.in1.wrapping_add(entry.in2),
            Opcode::Or => entry.in1 | entry.in2,
            Opcode::Xor => entry.in1 ^ entry.in2,
            Opcode::And => entry.in1 & entry.in2,
            Opcode::Shift => {
                let args = ShiftArgs::unpack(&[entry.in2]).unwrap();
                println!("# rotate mode:{:?}", &args);
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
            Opcode::Equals => {
                if entry.in1 == entry.in2 {
                    1u8
                } else {
                    0u8
                }
            }
            Opcode::Multiply => entry.in1.wrapping_mul(entry.in2),
        };

        println!("{:02x}", out);
    }
}

#[derive(Clone, Copy, Display, Debug, EnumCount, EnumIter, EnumString)]
#[strum(serialize_all = "lowercase")]
enum LoadEdge {
    Irl = 0,
    Irh = 1,
    Alu = 2,
    Tmp = 3,
    Ram = 4,
    PcR = 5,
    In1 = 6,
    Addr = 7,
    A = 8,
    B = 9,
    C = 10,
    PcFromPcr = 11,
    PcClk = 12,
    TTYin = 13,
    TTYout = 14,
    Next = 15,
}

#[derive(Clone, Copy, Display, Debug, EnumCount, EnumIter, EnumString)]
#[strum(serialize_all = "lowercase")]
enum OutputLevel {
    Irl = 0,
    PcClkEn = 1,
    Alu = 2,
    Tmp = 3,
    Ram = 4,
    Reserved5 = 5,
    Pc = 6,
    Reserved7 = 7,
    A = 8,
    B = 9,
    C = 10,
    Reserved11 = 11,
    Reserved12 = 12,
    TTYin = 13,
    Reserved14 = 14,
    Halt = 15,
}

const MAX_UOPS: usize = 64;

#[derive(Clone, Copy, Debug)]
struct MicroOp {
    out: OutputLevel,
    load: LoadEdge,
}

impl MicroOp {
    fn emit(&self) -> u8 {
        ((self.out as u8) << 4) | (self.load as u8)
    }

    fn print(&self) {
        println!("# OUT {:?}, LOAD {:?}", self.out, self.load);
        println!("{:02x}", self.emit());
    }
}

#[derive(Debug, PackedStruct)]
#[packed_struct(size_bytes = "2", endian = "lsb", bit_numbering = "lsb0")]
pub struct MicroEntry {
    #[packed_field(bits = "0..=7")]
    mode_specific: u8,
    #[packed_field(bits = "8..=10", ty = "enum")]
    out_reg: Register,
    #[packed_field(bits = "11..=12")]
    mode: Integer<u8, packed_bits::Bits2>,
}

fn ucode() {
    println!("v2.0 raw");

    let halt = MicroOp {
        out: OutputLevel::Halt,
        load: LoadEdge::Tmp,
    };

    // let mut uops = Vec::new();
    for encoded_inst in 0u16..(1 << 13) {
        let bytes: &[u8; 2] = &encoded_inst.to_be_bytes();
        let inst = MicroEntry::unpack(bytes).expect(&format!(
            "Could not decode {:02x}={:?}.",
            encoded_inst, bytes
        ));

        let base_address = encoded_inst as usize * MAX_UOPS;

        println!("# @{:04x} {:?} {:?}", base_address, &inst, &bytes);

        let mode = InstructionModeDiscriminants::iter()
            .nth(*inst.mode as usize)
            .unwrap();
        assert_eq!(
            *inst.mode,
            match mode {
                InstructionModeDiscriminants::Imm8 => 0,
                InstructionModeDiscriminants::Regs => 1,
                InstructionModeDiscriminants::Imm4 => 2,
                InstructionModeDiscriminants::MemOutRegs => 3,
            }
        );

        let mut uop_count = 0;

        println!("# common prelude");
        for u in &[
            MicroOp {
                out: OutputLevel::Pc,
                load: LoadEdge::Irl,
            },
            MicroOp {
                out: OutputLevel::PcClkEn,
                load: LoadEdge::PcClk,
            },
            MicroOp {
                out: OutputLevel::Pc,
                load: LoadEdge::Irh,
            },
        ] {
            u.print();
            uop_count += 1;
        }

        println!("# prep second input and drive alu");
        let mut in_ops = Vec::new();
        match mode {
            InstructionModeDiscriminants::Imm8 => {
                in_ops.push(MicroOp {
                    out: OutputLevel::A,
                    load: LoadEdge::In1,
                });
                in_ops.push(MicroOp {
                    out: OutputLevel::Irl,
                    load: LoadEdge::Alu,
                });
            }
            InstructionModeDiscriminants::Imm4 => {
                let mode2 = PackedInstructionMode2::unpack(&[inst.mode_specific]).unwrap();
                let in1_level = mode2.in1_reg.to_output_level();
                if mode2.in1_indirect {
                    in_ops.push(MicroOp {
                        out: in1_level,
                        load: LoadEdge::Addr,
                    });
                    in_ops.push(MicroOp {
                        out: OutputLevel::Ram,
                        load: LoadEdge::In1,
                    });
                } else {
                    in_ops.push(MicroOp {
                        out: in1_level,
                        load: LoadEdge::In1,
                    });
                }

                in_ops.push(halt); //todo
            }
            InstructionModeDiscriminants::Regs | InstructionModeDiscriminants::MemOutRegs => {
                let mode1 = PackedInstructionMode1or3::unpack(&[inst.mode_specific]).unwrap();
                {
                    let in1_level = mode1.in1_reg.to_output_level();
                    if mode1.in1_indirect {
                        in_ops.push(MicroOp {
                            out: in1_level,
                            load: LoadEdge::Addr,
                        });
                        in_ops.push(MicroOp {
                            out: OutputLevel::Ram,
                            load: LoadEdge::In1,
                        });
                    } else {
                        in_ops.push(MicroOp {
                            out: in1_level,
                            load: LoadEdge::In1,
                        });
                    }
                }
                {
                    let in2_level = mode1.in2_reg.to_output_level();
                    if mode1.in1_indirect {
                        in_ops.push(MicroOp {
                            out: in2_level,
                            load: LoadEdge::Addr,
                        });
                        in_ops.push(MicroOp {
                            out: OutputLevel::Ram,
                            load: LoadEdge::Alu,
                        });
                    } else {
                        in_ops.push(MicroOp {
                            out: in2_level,
                            load: LoadEdge::Alu,
                        });
                    }
                }
            }
        };
        for u in in_ops {
            u.print();
            uop_count += 1;
        }

        println!("# store output");
        let mut out_ops = Vec::new();
        match mode {
            InstructionModeDiscriminants::MemOutRegs => {
                out_ops.push(MicroOp {
                    out: inst.out_reg.to_output_level(),
                    load: LoadEdge::Addr,
                });
                out_ops.push(MicroOp {
                    out: OutputLevel::Alu,
                    load: LoadEdge::Ram,
                });
            }
            _ => match inst.out_reg {
                Register::Pc => {
                    out_ops.push(MicroOp {
                        out: OutputLevel::Alu,
                        load: LoadEdge::PcR,
                    });
                    out_ops.push(MicroOp {
                        out: OutputLevel::Tmp,
                        load: LoadEdge::PcFromPcr,
                    });
                }
                Register::Reserved6 | Register::Reserved7 => {
                    out_ops.push(halt);
                }
                r => {
                    out_ops.push(MicroOp {
                        out: OutputLevel::Alu,
                        load: r.to_load_edge(),
                    });
                    out_ops.push(MicroOp {
                        out: OutputLevel::PcClkEn,
                        load: LoadEdge::PcClk,
                    });
                }
            },
        };
        for u in out_ops {
            u.print();
            uop_count += 1;
        }

        println!("# common exit");
        for u in &[MicroOp {
            out: OutputLevel::Tmp,
            load: LoadEdge::Next,
        }] {
            u.print();
            uop_count += 1;
        }

        assert!(uop_count < MAX_UOPS);

        println!("{}*{:02x}", MAX_UOPS - uop_count, halt.emit());
    }
}

#[derive(Parser)]
#[grammar = "assembly.pest"]
pub struct AssemblyParser;

#[derive(Clone, Display, Debug)]
enum Value {
    Constant(u8),
    Label(String, Option<u8>),
}

impl Value {
    fn resolve(&mut self, labels: &BTreeMap<String, u8>) {
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
    fn unwrap_constant(&self) -> u8 {
        match self {
            Value::Constant(c) => *c,
            Value::Label(label, resolved_value) => {
                resolved_value.expect(&format!("Label '{}' has not been resolved", label))
            }
        }
    }
}

#[derive(Clone, Debug)]
struct AssemblyInstruction {
    source: String,
    opcode: Opcode,
    mode: InstructionMode,
}

impl AssemblyInstruction {
    fn resolve(&mut self, labels: &BTreeMap<String, u8>) {
        match &mut self.mode {
            InstructionMode::Regs(_, _, _) => {}
            InstructionMode::Imm8(_, in2) => in2.resolve(labels),
            InstructionMode::Imm4(_, _, in2) => in2.resolve(labels),
            InstructionMode::MemOutRegs(_, _, _) => {}
        }
    }

    fn emit(&self) -> Instruction {
        let (out_reg, mode, mode_specific) = match &self.mode {
            InstructionMode::Regs(out_reg, in1, in2) => {
                let (in1_indirect, in1_reg) = in1.split();
                let (in2_indirect, in2_reg) = in2.split();
                (
                    out_reg,
                    InstructionModeDiscriminants::Regs,
                    PackedInstructionMode1or3 {
                        in1_indirect,
                        in1_reg,
                        in2_indirect,
                        in2_reg,
                    }
                    .pack()[0],
                )
            }
            InstructionMode::Imm8(out_reg, in2) => (
                out_reg,
                InstructionModeDiscriminants::Imm8,
                in2.unwrap_constant(),
            ),
            InstructionMode::Imm4(out_reg, in1, in2) => {
                let (in1_indirect, in1_reg) = in1.split();

                (
                    out_reg,
                    InstructionModeDiscriminants::Imm4,
                    PackedInstructionMode2 {
                        in1_indirect,
                        in1_reg,
                        imm: in2.unwrap_constant().into(),
                    }
                    .pack()[0],
                )
            }
            InstructionMode::MemOutRegs(out_reg, in1, in2) => {
                let (in1_indirect, in1_reg) = in1.split();
                let (in2_indirect, in2_reg) = in2.split();
                (
                    out_reg,
                    InstructionModeDiscriminants::MemOutRegs,
                    PackedInstructionMode1or3 {
                        in1_indirect,
                        in1_reg,
                        in2_indirect,
                        in2_reg,
                    }
                    .pack()[0],
                )
            }
        };

        Instruction {
            op: self.opcode,
            mode: mode,
            mode_specific,
            out_reg: *out_reg,
        }
    }
}

fn assemble() {
    let input = {
        let mut input = String::new();
        std::io::stdin().lock().read_to_string(&mut input).unwrap();
        input
    };

    use pest::iterators::Pair;

    println!("v2.0 raw");

    {
        use pest::iterators::Pairs;
        fn dump_tree(pairs: Pairs<Rule>, indent: usize) {
            for pair in pairs {
                if pair.as_rule() != Rule::program {
                    print!("# ");
                    for _ in 0..indent {
                        print!(" ");
                    }
                    println!("{:?}: '{}'", pair.as_rule(), pair.as_str());
                }
                dump_tree(pair.into_inner(), indent + 1);
            }
        };

        let assembly = AssemblyParser::parse(Rule::program, &input).unwrap();
        dump_tree(assembly, 0);
    }

    fn parse_maybe_direct(maybe: Pair<Rule>) -> MaybeDirect<Register> {
        match maybe.as_rule() {
            Rule::deref_register => {
                let mut register = maybe.into_inner();
                let reg = std::str::FromStr::from_str(register.next().unwrap().as_str()).unwrap();
                MaybeDirect::Address(reg)
            }
            Rule::direct_register => {
                let mut register = maybe.into_inner();
                let reg = std::str::FromStr::from_str(register.next().unwrap().as_str()).unwrap();
                MaybeDirect::Value(reg)
            }
            r => panic!("unexpexted {:?}", r),
        }
    }

    fn parse_constant(constant: Pair<Rule>) -> Value {
        match constant.as_rule() {
            Rule::hex_constant => {
                let mut chars = constant.as_str().chars();
                Value::Constant(if chars.next().unwrap() == '-' {
                    let positive =
                        i8::from_str_radix(constant.as_str().trim_start_matches('-'), 16).unwrap();
                    (-1 * positive) as u8
                } else {
                    u8::from_str_radix(constant.as_str(), 16).unwrap()
                })
            }
            Rule::char_constant => {
                let mut chars = constant.as_str().chars();
                assert_eq!('\'', chars.next().unwrap());
                let c = chars.next().unwrap();
                assert_eq!('\'', chars.next().unwrap());
                assert_eq!(None, chars.next());
                Value::Constant(c as u8)
            }
            Rule::label => Value::Label(constant.as_str().to_owned(), None),
            r => panic!("unexpexted {:?}", r),
        }
    }

    enum SourceLine {
        Instruction(AssemblyInstruction),
        Comment(String),
    }

    let mut lines = Vec::new();
    let mut labels = BTreeMap::new();

    labels.insert(":tty".to_owned(), 0);
    labels.insert(":halt".to_owned(), 0xff);

    let mut assembly = AssemblyParser::parse(Rule::program, &input).unwrap();
    let assembly = assembly.next().unwrap();
    let assembly = assembly.into_inner();
    for line in assembly {
        match line.as_rule() {
            Rule::instruction => {
                let instruction = line;
                let source = instruction.as_span().as_str().to_owned();
                let mut tokens = instruction.into_inner();

                let opcode: Opcode = {
                    let opcode = tokens.next().unwrap();
                    assert_eq!(Rule::opcode, opcode.as_rule());
                    std::str::FromStr::from_str(opcode.as_str()).unwrap()
                };

                let mode = {
                    let inputs = tokens.next().unwrap();
                    match inputs.as_rule() {
                        Rule::mode_imm8 => {
                            let mut args = inputs.into_inner();
                            let out =
                                std::str::FromStr::from_str(args.next().unwrap().as_str()).unwrap();
                            let in2 = parse_constant(args.next().unwrap());
                            InstructionMode::Imm8(out, in2)
                        }
                        Rule::mode_regs => {
                            let mut args = inputs.into_inner();
                            let out =
                                std::str::FromStr::from_str(args.next().unwrap().as_str()).unwrap();
                            let in1 = parse_maybe_direct(args.next().unwrap());
                            let in2 = parse_maybe_direct(args.next().unwrap());
                            InstructionMode::Regs(out, in1, in2)
                        }
                        Rule::mode_imm4 => {
                            let mut args = inputs.into_inner();
                            let out =
                                std::str::FromStr::from_str(args.next().unwrap().as_str()).unwrap();
                            let in1 = parse_maybe_direct(args.next().unwrap());
                            let in2 = parse_constant(args.next().unwrap());
                            InstructionMode::Imm4(out, in1, in2)
                        }
                        Rule::mode_memoutregs => {
                            let mut args = inputs.into_inner();
                            let out =
                                std::str::FromStr::from_str(args.next().unwrap().as_str()).unwrap();
                            let in1 = parse_maybe_direct(args.next().unwrap());
                            let in2 = parse_maybe_direct(args.next().unwrap());
                            InstructionMode::MemOutRegs(out, in1, in2)
                        }
                        r => panic!("Unexpected: {:?}", r),
                    }
                };

                let inst = AssemblyInstruction {
                    source,
                    opcode,
                    mode,
                };

                println!("# {:?}", &inst);
                lines.push(SourceLine::Instruction(inst));
            }
            Rule::pseudo_copy => {
                let source = line.as_str().to_owned();
                let mut args = line.into_inner();
                let dst = std::str::FromStr::from_str(args.next().unwrap().as_str()).unwrap();
                let src = parse_maybe_direct(args.next().unwrap());

                let inst = AssemblyInstruction {
                    source,
                    opcode: Opcode::Or,
                    mode: InstructionMode::Regs(dst, src, src),
                };

                println!("# {:?}", &inst);
                lines.push(SourceLine::Instruction(inst));
            }
            Rule::pseudo_skip_if_zero => {
                let source = line.as_str().to_owned();
                let mut insts = vec![
                    AssemblyInstruction {
                        source: source.clone(),
                        opcode: Opcode::Shift,
                        mode: InstructionMode::Imm8(Register::A, Value::Constant(1)),
                    },
                    AssemblyInstruction {
                        source: source.clone(),
                        opcode: Opcode::Add,
                        mode: InstructionMode::Imm8(Register::A, Value::Constant(1)),
                    },
                    AssemblyInstruction {
                        source: source.clone(),
                        opcode: Opcode::Add,
                        mode: InstructionMode::Regs(
                            Register::Pc,
                            MaybeDirect::Value(Register::A),
                            MaybeDirect::Value(Register::Pc),
                        ),
                    },
                ];

                for inst in insts.drain(..) {
                    println!("# {:?}", &inst);
                    lines.push(SourceLine::Instruction(inst));
                }
            }
            Rule::label => {
                labels.insert(line.as_str().to_owned(), lines.len() as u8);
                lines.push(SourceLine::Comment(line.as_str().to_owned()));
            }
            Rule::comment => {
                lines.push(SourceLine::Comment(line.as_str().to_owned()));
            }
            Rule::EOI => {}
            r => panic!("Unexpected: {:?}", r),
        }
    }

    let mut pc = 0;
    for line in lines.as_mut_slice() {
        match line {
            SourceLine::Comment(comment) => {
                println!("# {}", &comment);
            }
            SourceLine::Instruction(i) => {
                i.resolve(&labels);
                let packed = i.emit();
                println!("# {:02x} {:?}", pc, &i);
                println!("# {:02x} {:?}", pc, &packed);
                let bytes = packed.pack();
                println!("{:02x} {:02x}", bytes[1], bytes[0]);
                pc += 2;
            }
        }
    }
}

fn main() {
    use std::fs::File;
    use std::io::prelude::*;

    let args: Vec<_> = std::env::args().collect();
    match args.get(1).map(|s| s.as_str()) {
        Some("ucode") => {
            ucode();
        }
        Some("alu") => {
            alu();
        }
        Some("assemble") => {
            assemble();
        }
        Some(unknown) => eprintln!("Unknown arg '{}'", unknown),
        None => eprintln!("no arg provided"),
    }
}
