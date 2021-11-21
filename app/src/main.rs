use alu::*;
use assemble::*;
use common::*;
use compiler::*;
use compiler::il::IlProgram;
use sim::*;
use ucode::*;
use std::{borrow::Cow, collections::HashMap, fs::File, io::Read, path::Path};

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let arg1 = args.get(1).map(|s| s.as_str());
    let arg2 = args.get(2).map(|s| s.as_str());

    let params = {
        let mut params: HashMap<&str,_> = HashMap::new();
        for arg in args.iter().skip(2) {
            if arg.starts_with("--") {
                if let Some((key,value)) = arg.split_once('=') {
                    let key = key.trim_start_matches('-');
                    params.insert(key, value.to_owned());
                }
            }
        }
        params
    };

    let get_param = |k| params.get(k).map(|s| s.as_str());

    //let arg3 = args.get(3).map(|s| s.as_str());
    match arg1 {
        Some("ucode") => {
            ucode(true);
        }
        Some("alu") => {
            alu(true);
        }
        Some("assemble") => {
            let input = {
                let mut input = String::new();
                let mut file = File::open(arg2.unwrap()).unwrap();
                file.read_to_string(&mut input).unwrap();
                input
            };
            let rom = assemble_from_str(&input);
            if let Some("true") = get_param("sim") {
                let mut c = Computer::from_image(Cow::Borrowed(&rom), false);
                c.stdin_out = true;
                while c.step() { }
            }
        }
        Some("il") => {
            let file_path: &Path = Path::new(args.get(2).unwrap());
            let input = {
                let mut input = String::new();
                let mut file = File::open(file_path).unwrap();
                file.read_to_string(&mut input).unwrap();
                input
            };
            let entry = get_param("entry").unwrap_or("main");
            let ctxt = create_program(entry, &input, file_path.parent().unwrap());
            let il = IlProgram::from_program(&ctxt);
            for (name, f) in &il.functions {
                print!("// {:?}(", name);
                for a in &f.args {
                    print!("{:?},", a);
                }
                println!(")");
                for s in &f.body {
                    println!("{:?}", s);
                }
                println!();
            }
            if let Some("true") = get_param("sim") {
                il.simulate(&[]);
            }
        }
        Some("compile") => {
            let file_path: &Path = Path::new(args.get(2).unwrap());
            let input = {
                let mut input = String::new();
                let mut file = File::open(file_path).unwrap();
                file.read_to_string(&mut input).unwrap();
                input
            };
            let entry = get_param("entry").unwrap_or("main");
            let (_program, assembly) = compile(
                entry, &input, file_path.parent().unwrap());
            let rom = assemble::assemble(assembly);

            if let Some("true") = get_param("sim") {
                let mut c = Computer::from_image(Cow::Borrowed(&rom), false);
                c.stdin_out = true;
                let mut last_ir0 = None;
                while c.step() { 
                    if last_ir0 != Some(c.ir0) {
                        // print_state(&c);
                    }
                    last_ir0 = Some(c.ir0);
                }
                // print_state(&c);
            }
        }
        Some("make_test") => {
            let rom = make_test();
            if let Some("true") = get_param("sim") {
                let mut c = Computer::from_image(Cow::Borrowed(&rom), false);
                c.stdin_out = true;
                while c.step() { }
            }
        }
        Some(unknown) => eprintln!("Unknown arg '{}'", unknown),
        None => eprintln!("no arg provided"),
    }
}

macro_rules! add {
    ($lines:expr, $op:expr, $args:expr) => {
        $lines.push(AssemblyInputLine::Instruction(Instruction {
            opcode: $op,
            args:$args, 
            source: format!("{}:{}", file!(), line!()),
            resolved: None,
        }))
    };
}

macro_rules! text {
    ($lines:expr, $s:expr) => {
        for c in $s.chars() {
            if c == '\0' { continue; }
            add!($lines, Opcode::LoadImm8, vec![Value::Register(0xF), Value::Constant8(c as u8)]);
            add!($lines, Opcode::TtyOut, vec![Value::Register(0xF)])
        }
    };
}

fn make_test() -> Image {
    let fail_label = "FAIL";
    let r = |r| {Value::Register(r)};
    let c8 = |v| {Value::Constant8(v)};

    let mut lines = Vec::new();

    text!(lines, "Hex values START:\n");
    text!(lines, "0123456789abcdef\n");
    text!(lines, "Hex values COMPELTE.\n");

    let (ra, rb, rtest, ractual) = (0x10, 0x20, 0x30, 0x40);
    text!(lines, format!("Add8NoCarry(r{:02x},r{:02x},r{:02x}) START:\n", ra, rb, rtest));
    for a in 0..0x10 {
        let a = a as u8;
        for b in 0..0x10 {
            let b = b as u8;
            let sum = a.wrapping_add(b);
            let case_name = format!("{:02x}+{:02x}={:02x}\n", a, b, sum);
            text!(lines, &case_name);
            add!(lines, Opcode::LoadImm8, vec![r(ra), c8(a)]);
            add!(lines, Opcode::LoadImm8, vec![r(rb), c8(b)]);
            add!(lines, Opcode::Add8NoCarry, vec![r(ra), r(rb), r(rtest)]);
            add!(lines, Opcode::LoadImm8, vec![r(ractual), c8(sum)]);
            add!(lines, Opcode::Cmp8, vec![r(ractual), r(rtest)]);
            add!(lines, Opcode::JzImm, vec![Value::Label24(case_name.to_owned())]);
            add!(lines, Opcode::Add8NoCarry, vec![r(rtest), r(rtest), r(0)]); //catpure to W
            add!(lines, Opcode::JmpImm, vec![Value::Label24(fail_label.to_owned())]);
            lines.push(AssemblyInputLine::Label(case_name.to_owned()));
        }
    }

    text!(lines, "TESTS COMPLETE\n");
    add!(lines, Opcode::Halt, vec![]);
    lines.push(AssemblyInputLine::Label(fail_label.to_owned()));
    text!(lines, "FAIL\n");
    add!(lines, Opcode::Halt, vec![]);

    assemble::assemble(lines)
}