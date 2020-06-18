extern crate pest;
#[macro_use]
extern crate pest_derive;

extern crate strum;
#[macro_use]
extern crate strum_macros;

use pest::Parser;
use pest::error::Error;

#[derive(Parser)]
#[grammar = "assembly.pest"]
pub struct AssemblyParser;

enum Opcode { 
    Copy,
    CopyAcc,
    Add,
    Add3IfAcc0Else1,
}

#[derive(Display, Debug, EnumString)]
enum SecondInput {
    Imm,
    Addr,
    Pc,
    Mem
}

#[derive(Display, Debug, EnumString)]
enum Output {
    Acc,
    Addr,
    Pc,
    Mem
}

struct Instruction {
    opcode: Opcode,
    input: SecondInput,
    output: Output,
    immediate: u8,
}

fn parse_assembly_file(input: &str) -> Result<Vec<Instruction>, Error<Rule>> {
    let mut instructions = Vec::new();
    println!("INPUT\n{}\nINPUT\n", input);
    let mut assembly = AssemblyParser::parse(Rule::program, input).unwrap();

    use pest::iterators::Pairs;

    fn parse_values(pairs: Pairs<Rule>) {
        for pair in pairs {
            println!("{:?}: '{}'", pair.as_rule(), pair.as_str().trim());
            parse_values(pair.into_inner());
        }
        // match pair.as_rule() {
        //     // Rule::file => {
        //     //     for i in pair.into_inner() {
        //     //         parse_value(i);
        //     //     }
        //     // }
        //     // Rule::comment => {},
        //     _ => {}
        // }
    }
    parse_values(assembly);

    // for p in assembly.next().unwrap().into_inner() {
    //     parse_value(p);
    // }
    // let instruction_tokens = assembly.into_inner();
    // for i in instruction_tokens {
    //     match i.as_rule() {
    //         Rule::
    //     }
    // }

    // match assembly.as_rule() {
    //     Rule::file => {},
    //     _ => {}
    // }

    // ...

    Ok(instructions)
}



// enum Instruction {
//     CopyImmTo()
// }

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

    let instructions = 16u8;
    for i in 0..instructions {
        for in2 in 0..=255u8 {
            for acc in 0..=255u8 {
                print!("# {:01x}{:02x}{:02x} ", i, in2, acc);

                match i {
                    0 => {
                        write("COPY", in2, false, false);
                    }
                    1 => {
                        write("COPY_ACC", acc, false, false);
                    }
                    2 => {
                        let sum = (in2 as u16) + (acc as u16);
                        write("ADD", sum as u8, (sum & 0x100) != 0, false);
                    }
                    3 => {
                        let sum = (in2 as u16) + (if acc == 0 { 3 } else { 1 });
                        write("ADD3_F_ACC_0_ELSE_1", sum as u8, (sum & 0x100) != 0, false);
                    }
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
            let path = args[2].as_str();
            let mut f = File::open(path).unwrap();
            let mut contents = String::new();
            f.read_to_string(&mut contents).unwrap();
            parse_assembly_file(&contents).unwrap();
        },
        Some(unknown) => eprintln!("Unknown arg '{}'", unknown),
        None => eprintln!("no arg provided"),
    }
}
