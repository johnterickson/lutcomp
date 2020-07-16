extern crate pest;
#[macro_use]
extern crate pest_derive;
use pest::Parser;

extern crate strum;

extern crate packed_struct;
use packed_struct::prelude::*;

use common::*;
use std::{collections::BTreeMap, io::Read};

#[derive(Parser)]
#[grammar = "assembly.pest"]
pub struct AssemblyParser;

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

pub fn assemble() {
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

