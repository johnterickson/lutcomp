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
    Load = 0,
    Store,
    Adc,
    Or,
    Xor,
    And,
    RotLeft,
    Compare
}

#[derive(Clone, Copy, Display, Debug, EnumString)]
#[strum(serialize_all = "lowercase")]
enum Register {
    Addr0, Addr1, Addr2, Addr3, Addr4, Addr5, 
    X,
    Y,
    Pc0, Pc1, Pc2, Pc3, Pc4, Pc5,
    Reserved,
    Mem,
}


#[derive(Clone, Copy, Display, Debug)]
#[strum(serialize_all = "lowercase")]
enum Input {
    Imm(u8),
    Reg(Register)
}

#[derive(Clone, Debug)]
struct Instruction {
    source: String,
    opcode: Opcode,
    input: Input,
}

impl Instruction {
    fn emit(&self) -> u8 {
        let mut byte = 0;
        byte |= (self.opcode as u8) << 5;
        match self.input {
            Input::Imm(i) => {
                byte |= i;
            }
            Input::Reg(r) => {
                byte |= 0x10;
                byte |= r as u8;
            }
        }

        byte
    }
}

fn parse_assembly_file(input: &str) -> Result<Vec<Instruction>, Error<Rule>> {
    use pest::iterators::Pair;

    // {
    //     use pest::iterators::{Pair, Pairs};
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
            let input = tokens.next().unwrap();
            assert_eq!(Rule::input, input.as_rule());
            let input = input.into_inner().next().unwrap();
            match input.as_rule() {
                Rule::immediate_input => {
                    let constant = input.into_inner().next().unwrap();
                    assert_eq!(Rule::constant, constant.as_rule());
                    let constant = constant.into_inner().next().unwrap();
                    Input::Imm(match constant.as_rule() {
                        Rule::hex_constant => {
                            let mut chars = constant.as_str().chars();
                            if chars.next().unwrap() == '-' {
                                let positive = i8::from_str_radix(constant.as_str().trim_start_matches('-'), 16).unwrap();
                                (-1 * positive) as u8
                            } else {
                                u8::from_str_radix(constant.as_str(), 16).unwrap()
                            }
                        },
                        Rule::char_constant => {
                            let mut chars = constant.as_str().chars();
                            assert_eq!('\'', chars.next().unwrap());
                            let c = chars.next().unwrap();
                            assert_eq!('\'', chars.next().unwrap());
                            assert_eq!(None, chars.next());
                            c as u8
                        }
                        r => panic!("unexpexted {:?}", r)
                    })
                }
                Rule::register_input => {
                    let register = input.into_inner().next().unwrap();
                    assert_eq!(Rule::register, register.as_rule());
                    Input::Reg(Register::from_str(register.as_str()).unwrap())
                }
                r => panic!("unexpexted {:?}", r)
            }
        };

        // let output = {
        //     let output_select = tokens.next().unwrap();
        //     assert_eq!(Rule::output_select, output_select.as_rule());
        //     Output::from_str(output_select.as_str()).unwrap()
        // };

        // let immediate = tokens.next().map(|immediate| {
        //     assert_eq!(Rule::constant, immediate.as_rule());
        //     let immediate = immediate.into_inner().next().unwrap();
        //     match immediate.as_rule() {

        //         _ => panic!()
        //     }
        // });

        Instruction {
            source,
            opcode,
            input,
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
            },
            Rule::EOI => {},
            r => panic!("unexpexted {:?}", r)
        }
    }

    Ok(instructions)
}

fn write(name: &str, out: u8) {
    println!("{}", name);
    print!("{:02x}", out);
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
                        write("COPY", in2);
                    }
                    1 => {
                        write("OR", acc | in2);
                    }
                    2 => {
                        write("ADD", acc.wrapping_add(in2));
                    }
                    3 => {
                        write(&format!("AND {:02x} {:02x}", acc, in2), acc & in2);
                    },
                    4 => {
                        write("XOR", acc ^ in2);
                    },
                    5 => {
                        let left_shift = in2 as i8;
                        if 0 < left_shift && left_shift <= 7 {
                            let shifted = acc << left_shift;
                            let shifted = shifted | (acc >> (8 - left_shift));
                            write("ROTLEFT", shifted);
                        } else if -7 <= left_shift && left_shift < 0 {
                            let right_shift = -1 * left_shift;
                            let shifted = acc >> right_shift;
                            let shifted = shifted | (acc << (8 - right_shift));
                            write("ROTLEFT", shifted);
                        } else {
                            write("INVALID ROTLEFT", 0xFF);
                        }
                    },
                    6 => {
                        write("COMPARE", if acc < in2 { 0xff } else if acc == in2 { 0 } else { 1 });
                    }
                    _ => {
                        write("UNUSED", 0xFF);
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
                    writeln!(f, "{:02x}", i.emit()).unwrap();
                };

                for i in &instructions {
                    write_inst(&mut f, i);
                }
            }
        }
        Some(unknown) => eprintln!("Unknown arg '{}'", unknown),
        None => eprintln!("no arg provided"),
    }
}
