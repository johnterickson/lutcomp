extern crate pest;
#[macro_use]
extern crate pest_derive;

extern crate strum;
#[macro_use]
extern crate strum_macros;
use std::{io::BufWriter, str::FromStr};

use pest::error::Error;
use pest::Parser;

#[derive(Parser)]
#[grammar = "assembly.pest"]
pub struct AssemblyParser;

#[derive(Clone, Copy, Display, Debug, EnumString)]
#[strum(serialize_all = "lowercase")]
enum Opcode {
    Copy = 0,
    Or,
    Add,
    Add3IfAcc0Else1,
    And,
    Xor,
    Rotate,
}

#[derive(Clone, Copy, Display, Debug, EnumString)]
#[strum(serialize_all = "lowercase")]
enum InputSelect {
    Imm = 0,
    Addr,
    Pc,
    Mem,
    Acc,
}

#[derive(Clone, Copy, Display, Debug, EnumString)]
#[strum(serialize_all = "lowercase")]
enum Output {
    Acc = 0,
    Addr,
    Pc,
    Mem,
}

#[derive(Debug)]
struct Instruction {
    source: String,
    opcode: Opcode,
    input: InputSelect,
    output: Output,
    immediate: Option<u8>,
}

fn parse_assembly_file(input: &str) -> Result<Vec<Instruction>, Error<Rule>> {
    use pest::iterators::{Pair, Pairs};

    {
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

    fn parse_instruction(pair: Pair<Rule>) -> Instruction {
        assert_eq!(Rule::instruction, pair.as_rule());
        let source = pair.as_span().as_str().to_owned();
        let mut tokens = pair.into_inner();

        let opcode = {
            let opcode = tokens.next().unwrap();
            assert_eq!(Rule::opcode, opcode.as_rule());
            Opcode::from_str(opcode.as_str()).unwrap()
        };

        let input = {
            let input_select = tokens.next().unwrap();
            assert_eq!(Rule::input_select, input_select.as_rule());
            InputSelect::from_str(input_select.as_str()).unwrap()
        };

        let output = {
            let output_select = tokens.next().unwrap();
            assert_eq!(Rule::output_select, output_select.as_rule());
            Output::from_str(output_select.as_str()).unwrap()
        };

        let immediate = tokens.next().map(|immediate| {
            assert_eq!(Rule::constant, immediate.as_rule());
            let immediate = immediate.into_inner().next().unwrap();
            match immediate.as_rule() {
                Rule::hex_constant => {
                    u8::from_str_radix(immediate.as_str(), 16).unwrap()
                },
                Rule::char_constant => {
                    let mut chars = immediate.as_str().chars();
                    assert_eq!('\'', chars.next().unwrap());
                    let c = chars.next().unwrap();
                    assert_eq!('\'', chars.next().unwrap());
                    assert_eq!(None, chars.next());
                    c as u8
                }
                _ => panic!()
            }
        });

        assert_eq!(
            immediate.is_some(),
            match input {
                InputSelect::Imm => true,
                _ => false
            }
        );

        Instruction {
            source,
            opcode,
            input,
            output,
            immediate,
        }
    }

    let mut instructions = Vec::new();
    let mut assembly = AssemblyParser::parse(Rule::program, input).unwrap();

    let assembly = assembly.next().unwrap();
    assert_eq!(assembly.as_rule(), Rule::program);
    let assembly = assembly.into_inner();
    for line in assembly {
        assert_eq!(line.as_rule(), Rule::line);
        let line = line.into_inner().next().unwrap();
        match line.as_rule() {
            Rule::comment => {}
            Rule::instruction => {
                instructions.push(parse_instruction(line));
            }
            _ => panic!("Unexpected: {:?}", line.as_rule()),
        }
    }

    Ok(instructions)
}

fn write(name: &str, out: u8, carry: bool, halt: bool) {
    println!("{}", name);

    let mut flags = 0;
    if out == 0 {
        flags |= 0b0000_0001;
    }
    if carry {
        flags |= 0b0000_0010;
    }
    if halt {
        flags |= 0b1000_0000;
    }

    print!("{:02x}{:02x}", flags, out);
}

fn ucode() {
    println!("v2.0 raw");

    // let inputs = ["IMM","ADDR","PC","MEM"];
    // let outputs = ["ACC", "ADDR", "PC", "MEM"];

    let instructions = 8u8;
    for i in 0..instructions {
        for in2 in 0..=255u8 {
            for acc in 0..=255u8 {
                print!("# {:01x}{:02x}{:02x} ", i, in2, acc);

                match i {
                    0 => {
                        write("COPY", in2, false, false);
                    }
                    1 => {
                        write("OR", acc | in2, false, false);
                    }
                    2 => {
                        let sum = (in2 as u16) + (acc as u16);
                        write("ADD", sum as u8, (sum & 0x100) != 0, false);
                    }
                    3 => {
                        let sum = (in2 as u16) + (if acc == 0 { 3 } else { 1 });
                        write("ADD3_IF_ACC_0_ELSE_1", sum as u8, (sum & 0x100) != 0, false);
                    },
                    4 => {
                        write("AND", acc & in2, false, false);
                    },
                    5 => {
                        write("XOR", acc ^ in2, false, false);
                    },
                    6 => {
                        let left_shift = in2 as i8;
                        if 0 < left_shift && left_shift <= 7 {
                            let shifted = acc << left_shift;
                            let shifted = shifted | (acc >> (8 - left_shift));
                            write("ROTATE", shifted, false, false);
                        } else if -7 <= left_shift && left_shift < 0 {
                            let right_shift = -1 * left_shift;
                            let shifted = acc >> right_shift;
                            let shifted = shifted | (acc << (8 - right_shift));
                            write("ROTATE", shifted, false, false);
                        } else {
                            write("INVALID ROTATE", 0xFF, false, true);
                        }
                    },
                    _ => {
                        write("UNUSED", 0xFF, false, true);
                    }
                }
                println!();
            }
        }
    }
}

fn main() {
    use std::fs::File;
    use std::io::prelude::*;

    let args: Vec<_> = std::env::args().collect();
    match args.get(1).map(|s| s.as_str()) {
        Some("ucode") => ucode(),
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
                    writeln!(f, "{:01x}{:01x}{:02x}", 
                        ((i.input as usize) << 2) | (i.output as usize),
                        (i.opcode) as usize,
                        i.immediate.unwrap_or(0xff)).unwrap();
                };

                for i in &instructions {
                    match (i.opcode, i.input) {
                        (Opcode::Copy, InputSelect::Acc) => {
                            write_inst(&mut f, &Instruction {
                                source: i.source.clone(),
                                input: InputSelect::Imm,
                                output: i.output,
                                opcode: Opcode::Or,
                                immediate: Some(0x00)
                            });
                        }
                        _ => {
                            write_inst(&mut f, i);
                        }
                    }
                }
                writeln!(f, "# eof").unwrap();
                writeln!(f, "ffff").unwrap();
            }
        }
        Some(unknown) => eprintln!("Unknown arg '{}'", unknown),
        None => eprintln!("no arg provided"),
    }
}
