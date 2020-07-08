extern crate pest;
#[macro_use]
extern crate pest_derive;
use pest::error::Error;
use pest::Parser;

extern crate strum;
#[macro_use]
extern crate strum_macros;
use strum::IntoEnumIterator;

#[macro_use]
extern crate static_assertions;

use std::{io::BufWriter, str::FromStr};
use std::collections::BTreeMap;

#[derive(Parser)]
#[grammar = "assembly.pest"]
pub struct AssemblyParser;

#[derive(Clone, Copy, Display, Debug, EnumCount, EnumIter, EnumString, PrimitiveEnum_u8)]
#[strum(serialize_all = "lowercase")]
enum Opcode {
    Copy,
    AddIfZero,
    Add,
    Or,
    Xor,
    And
}

#[derive(Clone, Copy, Display, Debug, EnumCount, EnumIter, EnumString, PartialEq)]
#[strum(serialize_all = "lowercase")]
enum Register {
    Acc,
    Pc,
    Addr,
    Mem,
    ExtAddr0,
    ExtAddr1,
    ExtAddr2,
    ExtMem,
}

#[derive(Clone, Display, Debug)]
enum Value {
    Constant(u8),
    Label(String),
}

impl Value {
    fn unwrap_constant(&self) -> u8 {
        match self {
            Value::Constant(c) => *c,
            _ => panic!()
        }
    }
}

#[derive(Clone, Copy, Display, Debug)]
#[strum(serialize_all = "lowercase")]
enum Input {
    Imm(u8),
    Reg(Register),
}

#[derive(Clone, Display, Debug)]
enum InstructionMode {
    Imm8(Value),
    Regs(Register, Register),
    Imm4(Register, u8),
}

#[derive(Clone, Debug)]
struct Instruction {
    source: String,
    opcode: Opcode,
    mode: InstructionMode,
    output: Register,
}

const EXT_MEMORY_ADDR_BITS: usize = 24;
const MEMORY_SIZE: usize = 1 << EXT_MEMORY_ADDR_BITS;

const REGISTER_SELECT_BITS: usize = 3;
const REGISTER_MAX_COUNT: usize = 1 << REGISTER_SELECT_BITS;
const_assert!(REGISTER_COUNT <= REGISTER_MAX_COUNT);

const OPCODE_SELECT_BITS: usize = 3;
const OPCODE_MAX_COUNT: usize = 1 << OPCODE_SELECT_BITS;
const_assert!(OPCODE_COUNT <= OPCODE_MAX_COUNT);

type InstructionInt = u16;

impl Instruction {
    fn emit(&self) -> InstructionInt {
        let mut inst = 0;
        inst |= (self.opcode as InstructionInt) << (16 - OPCODE_SELECT_BITS);
        inst |= (self.output as InstructionInt) << 8;
        let mode_num =  match &self.mode {
            InstructionMode::Imm8(imm) => {
                inst |= imm.unwrap_constant() as InstructionInt;
                0b00
            }
            InstructionMode::Regs(in1, in2) => {
                inst |= (*in1 as InstructionInt) << 4;
                inst |= *in2 as InstructionInt;
                0b01
            }
            InstructionMode::Imm4(in1, imm) => {
                inst |= (*in1 as InstructionInt) << 4;
                inst |= (*imm & 0xF) as InstructionInt;
                0b10
            }
        };
        inst |= mode_num << 11;
        inst
    }
}

extern crate packed_struct;
#[macro_use]
extern crate packed_struct_codegen;

use packed_struct::prelude::*;

#[derive(Debug, PackedStruct)]
#[packed_struct(size_bytes="4", endian="msb", bit_numbering = "lsb0")]
pub struct LutEntry {
    #[packed_field(bits = "19..=31")]
    unused: u16,
    #[packed_field(bits = "16..=18", ty = "enum")]
    op: Opcode,
    #[packed_field(bits = "8..=15")]
    in1: u8,
    #[packed_field(bits = "0..=7")]
    in2: u8,
}

#[derive(Debug)]
struct Machine {
    lut: Vec<u8>,
    ext_mem: Vec<u8>, //[u8; MEMORY_SIZE],
    mem: Vec<u8>,     //[u8; 256],
    regs: [u8; REGISTER_MAX_COUNT],
    rom: Vec<Instruction>,
}

impl Machine {
    fn new(rom: Vec<Instruction>, lut: Vec<u8>) -> Machine {
        Machine {
            lut,
            ext_mem: vec![0u8; MEMORY_SIZE],
            mem: vec![0u8; 256],
            regs: [0u8; REGISTER_MAX_COUNT],
            rom,
        }
    }

    fn ext_addr(&self) -> usize {
        (self.read_reg(Register::ExtAddr0) as usize)
            | ((self.read_reg(Register::ExtAddr1) as usize) << 8)
            | ((self.read_reg(Register::ExtAddr2) as usize) << 16)
    }

    fn read_reg(&self, r: Register) -> u8 {
        match r {
            Register::Mem => self.read_mem(self.read_reg(Register::Addr)),
            Register::ExtMem => self.ext_mem[self.ext_addr()],
            _ => self.regs[r as usize],
        }
    }

    fn read_mem(&self, addr: u8) -> u8 {
        
        let v = match addr {
            0x00 => 0xFF, //unimplemented!(),
            0x01 => 0xFF, //unimplemented!(),
            _ => self.mem[addr as usize],
        };
        // println!("Read ${:02x} from memory address ${:02x}", v, addr);
        v
    }

    fn write_reg(&mut self, r: Register, v: u8) {
        println!("Writing ${:02x} to {}", v, r);
        match r {
            Register::Mem => self.write_mem(self.read_reg(Register::Addr), v),
            Register::ExtMem => {
                let addr = self.ext_addr();
                println!("Writing ${:02x} to {:06x}", v, addr);
                self.ext_mem[addr] = v;
            }
            _ => self.regs[r as usize] = v,
        }
    }

    fn write_mem(&mut self, addr: u8, v: u8) {
        println!("Writing ${:02x} to memory address ${:02x}", v, addr);
        match addr {
            0x00 => unimplemented!(),
            0x01 => self.tty_out(v as char),
            _ => self.mem[addr as usize] = v,
        }
    }

    fn tty_out(&mut self, c: char) {
        // eprintln!("\tTTY: {}", c);
        eprint!("{}", c);
    }

    fn run(&mut self) {
        while self.read_reg(Register::Pc) != 0xFF {
            self.run_step();
        }
    }

    fn run_alu_lut(&mut self, lut_entry: &LutEntry) -> u8 {
        let lut_entry_index = u32::from_be_bytes(lut_entry.pack());

        let lut_out = self.lut[lut_entry_index as usize];

        println!("{:?}=={:04x} -> {:02x}", lut_entry, lut_entry_index, lut_out);
        lut_out
    }

    fn run_step(&mut self) {
        println!("Start instruction...");
        for r in Register::iter() {
            print!("{}: {:02x}  ", r, self.read_reg(r));
        }
        println!();

        let inst = self.rom[self.read_reg(Register::Pc) as usize].clone();

        let (in1, in2) = match &inst.mode {
            InstructionMode::Imm8(in2) => (self.read_reg(Register::Acc), in2.unwrap_constant()),
            InstructionMode::Regs(in1, in2) => (self.read_reg(*in1), self.read_reg(*in2)),
            InstructionMode::Imm4(in1, imm) => {
                (self.read_reg(*in1), (((*imm << 4) as i8) >> 4) as u8)
            }
        };

        let out = inst.output as Register;

        println!("{:?} ({:02x},{:02x}) -> {}", &inst, in1, in2, out);


        let lut_entry = LutEntry {
            unused: 0,
            op: inst.opcode,
            in1,
            in2,
        };

        let val = self.run_alu_lut(&lut_entry);

        self.write_reg(out, val);
        self.write_reg(Register::Pc, self.read_reg(Register::Pc) + 1);
    }
}



fn parse_assembly_file(input: &str) -> Result<Vec<Instruction>, Error<Rule>> {
    use pest::iterators::Pair;

    // {
    //     use pest::iterators::Pairs;
    //     fn dump_tree(pairs: Pairs<Rule>, indent: usize) {
    //         for pair in pairs {
    //             if pair.as_rule() != Rule::program {
    //                 for _ in 0..indent {
    //                     print!(" ");
    //                 }
    //                 println!("{:?}: '{}'", pair.as_rule(), pair.as_str());
    //             }
    //             dump_tree(pair.into_inner(), indent + 1);
    //         }
    //     };

    //     let assembly = AssemblyParser::parse(Rule::program, input).unwrap();
    //     dump_tree(assembly, 0);
    // }

    fn parse_constant(constant: Pair<Rule>) -> Value {
        assert_eq!(Rule::constant, constant.as_rule());
        let constant = constant.into_inner().next().unwrap();
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
            Rule::label => {
                Value::Label(constant.as_str().to_owned())
            }
            r => panic!("unexpexted {:?}", r),
        }
    }

    fn parse_instruction(instruction: Pair<Rule>) -> Instruction {
        assert_eq!(Rule::instruction, instruction.as_rule());
        let source = instruction.as_span().as_str().to_owned();
        let mut tokens = instruction.into_inner();

        let opcode = {
            let opcode = tokens.next().unwrap();
            assert_eq!(Rule::opcode, opcode.as_rule());
            std::str::FromStr::from_str(opcode.as_str()).unwrap()
        };

        let output = {
            let output = tokens.next().unwrap();
            assert_eq!(Rule::register, output.as_rule());
            Register::from_str(output.as_str()).unwrap()
        };

        let mode = {
            let inputs = tokens.next().unwrap();
            assert_eq!(Rule::inputs, inputs.as_rule());
            let inputs = inputs.into_inner().next().unwrap();
            match inputs.as_rule() {
                Rule::mode0 => {
                    let mut regs = inputs.into_inner();
                    let in1 = Register::from_str(regs.next().unwrap().as_str()).unwrap();
                    let in2 = Register::from_str(regs.next().unwrap().as_str()).unwrap();
                    InstructionMode::Regs(in1, in2)
                }
                Rule::mode1 => {
                    let constant = inputs.into_inner().next().unwrap();
                    InstructionMode::Imm8(parse_constant(constant))
                }
                Rule::mode2 => {
                    let mut inputs = inputs.into_inner();
                    let in1 = Register::from_str(inputs.next().unwrap().as_str()).unwrap();
                    let constant = parse_constant(inputs.next().unwrap()).unwrap_constant();
                    InstructionMode::Imm4(in1, constant & 0xF)
                }
                r => panic!("unexpexted {:?}", r),
            }
        };

        Instruction {
            source,
            opcode,
            mode,
            output,
        }
    }

    let mut instructions = Vec::new();
    let mut labels = BTreeMap::new();
    let mut assembly = AssemblyParser::parse(Rule::program, input).unwrap();

    let assembly = assembly.next().unwrap();
    assert_eq!(assembly.as_rule(), Rule::program);
    let assembly = assembly.into_inner();
    for line in assembly {
        match line.as_rule() {
            Rule::line => {
                let line = line.into_inner().next().unwrap();
                match line.as_rule() {
                    Rule::comment => {}
                    Rule::instruction => {
                        let i = parse_instruction(line);
                        println!("{:?}", &i);
                        instructions.push(i);
                    }
                    Rule::label => {
                        labels.insert(line.as_str().to_owned(), instructions.len() as u8);
                    }
                    Rule::pseudo_call => {
                        let source = line.as_str().to_owned();
                        let label = line.into_inner().next().unwrap();
                        assert_eq!(label.as_rule(), Rule::label);

                        let insts = [
                            Instruction {
                                source: source.clone(),
                                opcode: Opcode::Copy,
                                output: Register::Addr,
                                mode: InstructionMode::Imm8(Value::Constant(0xff))
                            },
                            Instruction {
                                source: source.clone(),
                                opcode: Opcode::Add,
                                output: Register::Mem,
                                mode: InstructionMode::Imm4(Register::Mem, 0xF)
                            },
                            Instruction {
                                source: source.clone(),
                                opcode: Opcode::Or,
                                output: Register::Addr,
                                mode: InstructionMode::Imm4(Register::Mem, 0)
                            },
                            Instruction {
                                source: source.clone(),
                                opcode: Opcode::Add,
                                output: Register::Mem,
                                mode: InstructionMode::Imm4(Register::Pc, 1)
                            },
                            Instruction {
                                source: source.clone(),
                                opcode: Opcode::Copy,
                                output: Register::Pc,
                                mode: InstructionMode::Imm8(Value::Label(label.as_str().to_owned()))
                            }
                        ];

                        for i in &insts {
                            println!("{:?}", &i);
                            instructions.push(i.clone());
                        }
                    }
                    Rule::pseudo_return => {
                        let source = line.as_str().to_owned();

                        let insts = [
                            Instruction {
                                source: source.clone(),
                                opcode: Opcode::Copy,
                                output: Register::Addr,
                                mode: InstructionMode::Imm8(Value::Constant(0xff))
                            },
                            Instruction {
                                source: source.clone(),
                                opcode: Opcode::Or,
                                output: Register::Addr,
                                mode: InstructionMode::Imm4(Register::Mem, 0)
                            },
                            Instruction {
                                source: source.clone(),
                                opcode: Opcode::Or,
                                output: Register::Acc,
                                mode: InstructionMode::Imm4(Register::Mem, 0)
                            },
                            Instruction {
                                source: source.clone(),
                                opcode: Opcode::Copy,
                                output: Register::Addr,
                                mode: InstructionMode::Imm8(Value::Constant(0xff))
                            },
                            Instruction {
                                source: source.clone(),
                                opcode: Opcode::Add,
                                output: Register::Mem,
                                mode: InstructionMode::Imm4(Register::Mem, 1)
                            },
                            Instruction {
                                source: source.clone(),
                                opcode: Opcode::Or,
                                output: Register::Pc,
                                mode: InstructionMode::Imm4(Register::Acc, 0)
                            },
                        ];

                        for i in &insts {
                            println!("{:?}", &i);
                            instructions.push(i.clone());
                        }
                    }
                    Rule::pseudo_initstack => {
                        let source = line.as_str().to_owned();

                        let insts = [
                            Instruction {
                                source: source.clone(),
                                opcode: Opcode::Copy,
                                output: Register::Addr,
                                mode: InstructionMode::Imm8(Value::Constant(0xff))
                            },
                            Instruction {
                                source: source.clone(),
                                opcode: Opcode::Copy,
                                output: Register::Mem,
                                mode: InstructionMode::Imm8(Value::Constant(0xfe))
                            },
                            Instruction {
                                source: source.clone(),
                                opcode: Opcode::Copy,
                                output: Register::Addr,
                                mode: InstructionMode::Imm8(Value::Constant(0xfe))
                            },
                            Instruction {
                                source: source.clone(),
                                opcode: Opcode::Copy,
                                output: Register::Mem,
                                mode: InstructionMode::Imm8(Value::Constant(0xfe))
                            },
                        ];

                        for i in &insts {
                            println!("{:?}", &i);
                            instructions.push(i.clone());
                        }
                    }
                    _ => panic!("Unexpected: {:?}", line.as_rule()),
                }
            }
            Rule::EOI => {}
            r => panic!("unexpexted {:?}", r),
        }
    }

    for i in instructions.iter_mut() {
        if let InstructionMode::Imm8(v) = &mut i.mode {
            *v = Value::Constant(match v {
                Value::Constant(c) => *c,
                Value::Label(label) => {
                    let mut resolved = *labels
                        .get(label.as_str())
                        .expect(&format!("Could not find label '{}", &label));
                    if i.output == Register::Pc {
                        resolved -= 1;
                    }
                    resolved
                }
            });
        }
    }

    Ok(instructions)
}

fn ucode(print: bool) -> Vec<u8> {
    if print {
        println!("v2.0 raw");
    }

    let mut i: u32  = 0;
    let mut lut = Vec::new();
    for op in Opcode::iter() {
        for in1 in 0..=0xFFu8 {
            for in2 in 0..=0xFFu8 {
                let lut_out = match op {
                    Opcode::Copy => in2,
                    Opcode::AddIfZero => {
                        if in1 == 0 {
                            in1.wrapping_add(in2)
                        } else {
                            in1
                        }
                    }
                    Opcode::Add => in1.wrapping_add(in2),
                    Opcode::Or => in1 | in2,
                    Opcode::Xor => in1 ^ in2,
                    Opcode::And => in1 & in2,
                };
                
                lut.push(lut_out);

                if print {
                    println!("# {:08x} {} in1={:02x} in2={:02x} -> val={:02x}", 
                        i, op, in1, in2, lut_out);
                    println!("{:02x}", lut_out);
                }

                i += 1;
            }
        }
    }

    lut
}

fn main() {
    use std::fs::File;
    use std::io::prelude::*;

    let args: Vec<_> = std::env::args().collect();
    match args.get(1).map(|s| s.as_str()) {
        Some("ucode") => {
            ucode(true);
        }
        Some("assemble") => {
            let instructions = {
                let path = args[2].as_str();
                let mut f = File::open(path).unwrap();
                let mut contents = String::new();
                f.read_to_string(&mut contents).unwrap();
                parse_assembly_file(&contents).unwrap()
            };

            {
                let path = args[3].as_str();
                let f = File::create(path).unwrap();
                let mut f = BufWriter::new(f);
                writeln!(f, "v2.0 raw").unwrap();

                let write_inst = |f: &mut BufWriter<_>, i: &Instruction| {
                    writeln!(f, "{:?}", &i).unwrap();
                    writeln!(f, "{:04x}", i.emit()).unwrap();
                };

                let mut pc = 0;
                for i in &instructions {
                    write!(f, "# {:02x} ", pc).unwrap();
                    write_inst(&mut f, i);
                    pc += 1;
                }

                println!("Simulating...");
                let lut = ucode(false);
                let mut m = Machine::new(instructions, lut);
                m.run();
            }
        }
        Some(unknown) => eprintln!("Unknown arg '{}'", unknown),
        None => eprintln!("no arg provided"),
    }
}
