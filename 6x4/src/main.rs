extern crate pest;
#[macro_use]
extern crate pest_derive;

extern crate strum;
#[macro_use]
extern crate strum_macros;

#[macro_use]
extern crate static_assertions;

use std::{io::BufWriter, str::FromStr};

use pest::error::Error;
use pest::Parser;

#[derive(Parser)]
#[grammar = "assembly.pest"]
pub struct AssemblyParser;

#[derive(Clone, Copy, Display, Debug, EnumCount, EnumString)]
#[strum(serialize_all = "lowercase")]
enum Opcode {
    Copy,
    Adc,
    Or,
    Xor,
    And,
    RotLeft,
    Compare,
}

#[derive(Clone, Copy, Display, Debug, EnumCount, EnumString, PartialEq)]
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

#[derive(Clone, Copy, Display, Debug)]
#[strum(serialize_all = "lowercase")]
enum Input {
    Imm(u8),
    Reg(Register),
}

#[derive(Clone, Copy, Display, Debug)]
enum InstructionMode {
    Regs(Register, Register),
    Imm8(u8),
    Imm4(Register, u8),
}

#[derive(Clone, Debug)]
struct Instruction {
    source: String,
    opcode: Opcode,
    mode: InstructionMode,
    output: Register,
}

#[derive(Clone, Copy, Display, Debug, EnumCount, EnumString)]
enum Flags {
    Carry,
    Zero,
}

const EXT_MEMORY_ADDR_BITS: usize = 24;
const MEMORY_SIZE: usize = 1 << EXT_MEMORY_ADDR_BITS;

const REGISTER_SELECT_BITS: usize = 3;
const REGISTER_MAX_COUNT: usize = 1 << REGISTER_SELECT_BITS;
const_assert!(REGISTER_COUNT <= REGISTER_MAX_COUNT);

const OPCODE_SELECT_BITS: usize = 4;
const OPCODE_MAX_COUNT: usize = 1 << OPCODE_SELECT_BITS;
const_assert!(OPCODE_COUNT <= OPCODE_MAX_COUNT);

type InstructionInt = u16;

impl Instruction {
    fn emit(&self) -> InstructionInt {
        let mut inst = 0;
        inst |= (self.opcode as InstructionInt) << 12;
        inst |= (self.output as InstructionInt) << 9;
        match self.mode {
            InstructionMode::Regs(in1, in2) => {
                inst |= 0x180;
                inst |= (in1 as InstructionInt) << 4;
                inst |= in2 as InstructionInt;
            }
            InstructionMode::Imm8(imm) => {
                inst |= imm as InstructionInt;
            }
            InstructionMode::Imm4(in1, imm) => {
                inst |= 0x100;
                inst |= (in1 as InstructionInt) << 4;
                inst |= (imm & 0xF) as InstructionInt;
            }
        }
        inst
    }
}

#[derive(Debug)]
struct Machine {
    ext_mem: Vec<u8>,//[u8; MEMORY_SIZE],
    mem: Vec<u8>, //[u8; 256],
    regs: [u8; REGISTER_MAX_COUNT],
    flags: [bool; 2],
    rom: Vec<Instruction>,
}

impl Machine {
    fn new(rom: Vec<Instruction>) -> Machine {
        Machine {
            ext_mem: vec![0u8; MEMORY_SIZE],
            mem: vec![0u8; 256],
            regs: [0u8; REGISTER_MAX_COUNT],
            flags: [false; 2],
            rom
        }
    }

    fn ext_addr(&self) -> usize {
        (self.read_reg(Register::ExtAddr0) as usize)
            | ((self.read_reg(Register::ExtAddr1) as usize) << 8)
            | ((self.read_reg(Register::ExtAddr2) as usize) << 16)
    }

    fn read_reg(&self, r: Register) -> u8 {
        match r {
            Register::Mem => self.mem[Register::Addr as usize],
            Register::ExtMem => self.ext_mem[self.ext_addr()],
            _ => self.regs[r as usize],
        }
    }

    fn write_reg(&mut self, r: Register, v: u8) {
        println!("Writing ${:02x} to {}", v, r);
        match r {
            Register::Mem => self.write_mem(self.read_reg(Register::Addr), v),
            Register::ExtMem => {
                let addr = self.ext_addr();
                self.ext_mem[addr] = v;
            },
            _ => self.regs[r as usize] = v,
        }
    }

    fn write_mem(&mut self, addr: u8, v: u8) {
        println!("Writing ${:02x} to ${:02x}", v, addr);
        match addr {
            0x01 => self.tty_out(v as char),
            _ => self.mem[addr as usize] = v,
        }
    }

    fn tty_out(&mut self, c: char) {
        eprint!("{}", c);
    }

    fn run(&mut self) {
        while self.read_reg(Register::Pc) != 0xFF {
            self.run_step();
        }
    }

    fn run_step(&mut self) {
        println!("{:?}",&self.regs);
        let inst = &self.rom[self.read_reg(Register::Pc) as usize];
        println!("{:?}",&inst);

        let (in1, in2) = match inst.mode {
            InstructionMode::Imm8(in2) => (self.read_reg(Register::Acc), in2),
            InstructionMode::Regs(in1, in2) => (self.read_reg(in1), self.read_reg(in2)),
            InstructionMode::Imm4(in1, imm) => {
                (self.read_reg(in1), (((imm << 4) as i8) >> 4) as u8)
            }
        };

        let out = inst.output as Register;

        match inst.opcode {
            Opcode::Copy => {
                self.write_reg(out, in2);
            }
            _ => unimplemented!(),
        }

        if out != Register::Pc {
            self.write_reg(Register::Pc, self.read_reg(Register::Pc) + 1);
        }
    }
}

fn parse_assembly_file(input: &str) -> Result<Vec<Instruction>, Error<Rule>> {
    use pest::iterators::Pair;

    {
        use pest::iterators::Pairs;
        fn dump_tree(pairs: Pairs<Rule>, indent: usize) {
            for pair in pairs {
                if pair.as_rule() != Rule::program {
                    for _ in 0..indent {
                        print!(" ");
                    }
                    println!("{:?}: '{}'", pair.as_rule(), pair.as_str());
                }
                dump_tree(pair.into_inner(), indent + 1);
            }
        };

        let assembly = AssemblyParser::parse(Rule::program, input).unwrap();
        dump_tree(assembly, 0);
    }

    fn parse_constant(constant: Pair<Rule>) -> u8 {
        assert_eq!(Rule::constant, constant.as_rule());
        let constant = constant.into_inner().next().unwrap();
        match constant.as_rule() {
            Rule::hex_constant => {
                let mut chars = constant.as_str().chars();
                if chars.next().unwrap() == '-' {
                    let positive =
                        i8::from_str_radix(constant.as_str().trim_start_matches('-'), 16).unwrap();
                    (-1 * positive) as u8
                } else {
                    u8::from_str_radix(constant.as_str(), 16).unwrap()
                }
            }
            Rule::char_constant => {
                let mut chars = constant.as_str().chars();
                assert_eq!('\'', chars.next().unwrap());
                let c = chars.next().unwrap();
                assert_eq!('\'', chars.next().unwrap());
                assert_eq!(None, chars.next());
                c as u8
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
            Opcode::from_str(opcode.as_str()).unwrap()
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
                    let constant = parse_constant(inputs.next().unwrap());
                    InstructionMode::Imm4(in1, constant)
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
                        instructions.push(parse_instruction(line));
                    }
                    _ => panic!("Unexpected: {:?}", line.as_rule()),
                }
            }
            Rule::EOI => {}
            r => panic!("unexpexted {:?}", r),
        }
    }

    Ok(instructions)
}

// fn write(name: &str, out: u8) {
//     println!("{}", name);
//     print!("{:02x}", out);
// }

// fn ucode() {
//     println!("v2.0 raw");

//     // let inputs = ["IMM","ADDR","PC","MEM"];
//     // let outputs = ["ACC", "ADDR", "PC", "MEM"];

//     let instructions = 8u8;
//     for i in 0..instructions {
//         for in2 in 0..=255u8 {
//             for acc in 0..=255u8 {
//                 print!("# {:01x}{:02x}{:02x} ", i, in2, acc);

//                 match i {
//                     0 => {
//                         write("COPY", in2);
//                     }
//                     1 => {
//                         write("OR", acc | in2);
//                     }
//                     2 => {
//                         write("ADD", acc.wrapping_add(in2));
//                     }
//                     3 => {
//                         write(&format!("AND {:02x} {:02x}", acc, in2), acc & in2);
//                     },
//                     4 => {
//                         write("XOR", acc ^ in2);
//                     },
//                     5 => {
//                         let left_shift = in2 as i8;
//                         if 0 < left_shift && left_shift <= 7 {
//                             let shifted = acc << left_shift;
//                             let shifted = shifted | (acc >> (8 - left_shift));
//                             write("ROTLEFT", shifted);
//                         } else if -7 <= left_shift && left_shift < 0 {
//                             let right_shift = -1 * left_shift;
//                             let shifted = acc >> right_shift;
//                             let shifted = shifted | (acc << (8 - right_shift));
//                             write("ROTLEFT", shifted);
//                         } else {
//                             write("INVALID ROTLEFT", 0xFF);
//                         }
//                     },
//                     6 => {
//                         write("COMPARE", if acc < in2 { 0xff } else if acc == in2 { 0 } else { 1 });
//                     }
//                     _ => {
//                         write("UNUSED", 0xFF);
//                     }
//                 }
//                 println!();
//             }
//         }
//     }
// }

fn main() {
    use std::fs::File;
    use std::io::prelude::*;

    let args: Vec<_> = std::env::args().collect();
    match args.get(1).map(|s| s.as_str()) {
        // Some("ucode") => ucode(),
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
                    writeln!(f, "# {:?}", &i).unwrap();
                    writeln!(f, "{:04x}", i.emit()).unwrap();
                };

                for i in &instructions {
                    write_inst(&mut f, i);
                }

                println!("Simulating...");
                let mut m = Machine::new(instructions);
                m.run();
            }
        }
        Some(unknown) => eprintln!("Unknown arg '{}'", unknown),
        None => eprintln!("no arg provided"),
    }
}
