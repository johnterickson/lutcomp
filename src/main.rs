use strum_macros::{Display, EnumString};

extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;

#[derive(Parser)]
#[grammar = "assembly.pest"]
pub struct AssemblyParser;

// #[derive(Display, Debug, EnumString)]
// enum SecondInput {
//     Imm,
//     Addr,
//     Pc,
//     Mem
// }

// #[derive(Display, Debug, EnumString)]
// enum Output {
//     Acc,
//     Addr,
//     Pc,
//     Mem
// }

// enum Instruction {
//     CopyImmTo()
// }





fn ucode() {
    println!("v2.0 raw");

    // let inputs = ["IMM","ADDR","PC","MEM"];
    let outputs = ["ACC","ADDR","PC","MEM"];

    let instructions = 16u8;
    for i in 0..instructions {
        for in2 in 0..=255u8 {
            for acc in 0..=255u8 {
                print!("# {:01x}{:02x}{:02x} ", i, in2, acc);

                match i {
                    0 => {
                        println!("COPY");
                        print!("{:02x}{:02x}",  0,  in2);
                    },
                    1 => {
                        println!("COPY_ACC");
                        print!("{:02x}{:02x}",  0,  acc);
                    },
                    2 => {
                        let sum = (in2 as u16) + (acc as u16);
                        let flags = ((sum >> 8) & 0x1) as u8;
                        println!("ADD");
                        print!("{:02x}{:02x}",  flags,  sum as u8);
                    },
                    3 => {
                        let sum = (in2 as u16) + (if acc == 0 { 3 } else {  1 });
                        let flags = ((sum >> 8) & 0x1) as u8;
                        println!("ADD3_F_ACC_0_ELSE_1");
                        print!("{:02x}{:02x}",  flags,  sum as u8);
                    },
                    _ => {
                        println!("UNUSED");
                        print!("80FF");
                    }
                }
                println!();
            }
        }
    }
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    match args[1].as_str(){
        "ucode" => ucode(),
        unknown => { eprintln!("Unknown arg '{}'", unknown); }
    }
}
