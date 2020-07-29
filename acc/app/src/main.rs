use alu::*;
use assemble::*;
use sim::*;
use ucode::*;
use std::{io::Read, fs::File};

fn main() {
    let args: Vec<_> = std::env::args().collect();
    match args.get(1).map(|s| s.as_str()) {
        Some("ucode") => {
            ucode(true);
        }
        Some("alu") => {
            alu(true);
        }
        Some("assemble") => {
            let input = {
                let mut input = String::new();
                let mut file = File::open(args.get(2).unwrap()).unwrap();
                file.read_to_string(&mut input).unwrap();
                input
            };
            assemble(input);
        }
        Some("assemble_sim") => {
            let input = {
                let mut input = String::new();
                let mut file = File::open(args.get(2).unwrap()).unwrap();
                file.read_to_string(&mut input).unwrap();
                input
            };
            let rom = assemble(input);
            let mut c = Computer::with_print(rom, true);
            while c.step() { }
        }
        Some(unknown) => eprintln!("Unknown arg '{}'", unknown),
        None => eprintln!("no arg provided"),
    }
}
