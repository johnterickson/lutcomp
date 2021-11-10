use alu::*;
use assemble::*;
use compiler::*;
use sim::*;
use ucode::*;
use std::{borrow::Cow, fs::File, io::Read, path::Path};

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
            assemble_from_str(&input);
        }
        Some("assemble_sim") => {
            let input = {
                let mut input = String::new();
                let mut file = File::open(args.get(2).unwrap()).unwrap();
                file.read_to_string(&mut input).unwrap();
                input
            };
            let rom = assemble_from_str(&input);
            let mut c = Computer::from_image(Cow::Borrowed(&rom), true);
            while c.step() { }
        }
        Some("compile") => {
            let file_path: &Path = Path::new(args.get(2).unwrap());
            let input = {
                let mut input = String::new();
                let mut file = File::open(file_path).unwrap();
                file.read_to_string(&mut input).unwrap();
                input
            };
            let (_program, assembly) = compile(
                "main", &input, file_path.parent().unwrap());
            let _rom = assemble::assemble(assembly);
        }
        Some("compile_sim") => {
            let file_path: &Path = Path::new(args.get(2).unwrap());
            let input = {
                let mut input = String::new();
                let mut file = File::open(file_path).unwrap();
                file.read_to_string(&mut input).unwrap();
                input
            };
            let (_program, assembly) = compile(
                "main", &input, file_path.parent().unwrap());
            let rom = assemble::assemble(assembly);
            let mut c = Computer::from_image(Cow::Borrowed(&rom), false);
            c.stdin_out = true;
            while c.step() { }
            // let mut last_ir0 = None;
            // while c.step() { 
            //     if last_ir0 != Some(c.ir0) {
            //         print_state(&c);
            //     }
            //     last_ir0 = Some(c.ir0);
            // }
            // print_state(&c);
        }
        Some(unknown) => eprintln!("Unknown arg '{}'", unknown),
        None => eprintln!("no arg provided"),
    }
}
