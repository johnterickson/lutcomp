use alu::*;
use assemble::*;
use common::*;
use compiler::*;
use compiler::backend::emit_assembly;
use compiler::il::*;
use sim::*;
use ucode::*;
use std::{borrow::Cow, collections::BTreeMap, fs::File, io::Read, path::Path};

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let arg1 = args.get(1).map(|s| s.as_str());
    let arg2 = args.get(2).map(|s| s.as_str());

    let params = {
        let mut params: BTreeMap<&str,_> = BTreeMap::new();
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

    let get_param = |k: &str| params.get(k).map(|s| s.as_str());

    let mut sim_args: Vec<IlNumber> = Vec::new();
    for i in 1..=3 {
        let param_name = format!("arg{}",i);
        if let Some(v) = get_param(&param_name) {
            if let Ok(v) = v.parse() {
                sim_args.push(IlNumber::U8(v))
            } else {
                sim_args.push(IlNumber::U32(v.parse().unwrap()))
            }
        }
    }

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
                println!("{:?}", il.simulate(&sim_args));
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
            let root = file_path.parent().unwrap();

            let (ctxt, il) = emit_il(entry, &input, root);

            let (_backend, mut assembly) = emit_assembly(&ctxt, &il);

            if let Some(image_base_address) = get_param("image_base_address") {
                let image_base_address = u32::from_str_radix(image_base_address, 16).unwrap();

                if let Some(existing_base) = assembly.iter_mut().filter_map(|line| match line {
                    AssemblyInputLine::ImageBaseAddress(existing_base) => Some(existing_base),
                    _ => None
                }).next() {
                    *existing_base = image_base_address;
                } else {
                    assembly.insert(0, AssemblyInputLine::ImageBaseAddress(image_base_address));
                }
            }
            let rom = assemble::assemble(assembly);

            if let Some("true") = get_param("sim") {
                if !sim_args.is_empty() {
                    todo!();
                }

                let trace_ucode = Some("true") == get_param("trace_ucode");
                let trace_pc = Some("true") == get_param("trace_pc");

                let mut c = Computer::from_image(Cow::Borrowed(&rom), trace_ucode);
                if let Some("true") = get_param("profile") {
                    c.pc_hit_count = Some(BTreeMap::new());
                }
                if let Some(n) = get_param("stack_dump_rate") {
                    c.stack_dump_rate = n.parse::<u64>().unwrap();
                }
                if let Some("true") = get_param("block_for_stdin") {
                    c.block_for_stdin = true;
                }
                c.stdin_out = true;

                let mut last_ir0_pc = None;
                while c.step() { 
                    if last_ir0_pc != c.ir0_pc && trace_pc {
                        print_state(&c);
                    }
                    last_ir0_pc = c.ir0_pc;
                }
                // print_state(&c);

                if let Some(pc_hit_count) = c.pc_hit_count {
                    let mut hits : Vec<_> = pc_hit_count.iter().collect();
                    hits.sort_by(|a,b| a.1.cmp(b.1).reverse().then_with(|| a.0.cmp(b.0)));
                    hits.truncate(10);
                    for h in &hits {
                        print!("hits:{}", h.1);

                        if let Some(f) = rom.find_containing_function(*h.0) {
                            println!(" pc:{:05x}={}+0x{:x}", h.0, f.2, *h.0 - f.0);
                        } else {
                            println!(" pc:{:05x}=??", h.0);
                        }

                        if let Some(sym) = rom.symbols.get(h.0) {
                            for line in &sym.notes {
                                println!(" {}", line);
                            }
                        }
                    }
                }
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
        Some("ascii_to_ps2") => {
            println!("v2.0 raw");
            for (c, codes) in ASCII_TO_PS2_SCAN_CODES.iter().enumerate() {
                if (0xA..=0xD).contains(&c) {
                    println!("# ASCII 0x{:02x}", c);
                } else {
                    println!("# ASCII 0x{:02x} == `{}`", c, (c as u8) as char);
                }

                for code in codes {
                    print!("{:02x} ", *code);
                }

                println!();
            }
        }
        Some("ps2_to_ascii") => {
            let style = get_param("style");
            let is_hex = match style {
                None | Some("hex") => true,
                Some("j") => false,
                Some(other) => panic!("unknown style: {}", other),
            };

            println!("# Table Format");
            println!("# ASCII WITH_SHIFT_ASCII IS_SHIFT IS_RELEASE");

            for (code, mapping) in PS2_SCAN_CODE_TO_ASCII.iter().enumerate() {
                let code = code as u8;
                println!("# PS2 CODE 0x{:02x}", code);
                println!("# {:?}", mapping);
                if let Some(key_name) = mapping.key_name {
                    println!("# \"{}\"", key_name);
                }
                if mapping.ascii != 0 {
                    if 0xA <= mapping.ascii && mapping.ascii <= 0xD {
                        println!("# NORMAL ASCII  0x{:02x}", mapping.ascii);
                    } else {
                        println!("#  NORMAL ASCII 0x{:02x} == `{}`", mapping.ascii, mapping.ascii as char);
                    }
                }

                if let Some(shifted_key_name) = mapping.shift_key_name {
                    println!("# \"{}\"", shifted_key_name);
                }
                if mapping.shift_ascii != 0 {
                    println!("# SHIFTED ASCII 0x{:02x} == `{}`", mapping.shift_ascii, mapping.shift_ascii as char);
                }

                if is_hex {
                    println!("{:02x} {:02x} {:02x} {:02x}", 
                        mapping.ascii, mapping.shift_ascii, 
                        if mapping.is_shift {1} else {0},
                        if mapping.is_release {1} else {0}  );
                } else {
                    println!("# {},{},{},{},", 
                        mapping.ascii, mapping.shift_ascii, 
                        if mapping.is_shift {1} else {0},
                        if mapping.is_release {1} else {0}  );
                }
            }

            if !is_hex {
                print!("const PS2_SCAN_CODE_TO_ASCII: u8[] = [");
                for (_, mapping) in PS2_SCAN_CODE_TO_ASCII.iter().enumerate() {
                    print!("{},{},{},{},", mapping.ascii, mapping.shift_ascii, 
                        if mapping.is_shift {1} else {0}, if mapping.is_release {1} else {0} );
                }
                println!("255];");
            }
        }
        Some("program_ram") => {

            let hex_path = get_param("hex_path").expect("hex path not specified.");
            let file = File::open(hex_path).unwrap();
            let hex = HexFile::read(file).unwrap();

            let mut base_address = None;
            for line in &hex.lines {
                match line {
                    HexFileLine::Data(_) => {},
                    HexFileLine::Comment(line) => {
                        if line.starts_with(" ImageBaseAddress=0x") {
                            let mut parts = line.split("0x");
                            let _ = parts.next().unwrap();
                            base_address = Some(u32::from_str_radix(parts.next().unwrap(), 16).unwrap());
                        }
                    },
                }
            }

            let base_address = base_address.expect("Did not find a base address");
            assert!(base_address >= RAM_MIN);

            println!("s{:08x}", base_address);
    
            for b in &hex.bytes() {
                println!("w{:02x}\nn", b);
            }
    
            // overwrite the return address
            println!("s{:08x}", INITIAL_STACK-4);
    
            for b in base_address.to_le_bytes() {
                println!("w{:02x}\nn", b);
            }
    
            println!("q");
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
            add!($lines, IoPort::Tty.out_opcode(), vec![Value::Register(0xF)])
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
    add!(lines, Opcode::Halt, vec![Value::Constant32(HaltCode::Success as u32)]);
    lines.push(AssemblyInputLine::Label(fail_label.to_owned()));
    text!(lines, "FAIL\n");
    add!(lines, Opcode::Halt, vec![Value::Constant32(HaltCode::TestFail as u32)]);

    assemble::assemble(lines)
}