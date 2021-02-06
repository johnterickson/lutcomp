extern crate pest;
#[macro_use]
extern crate pest_derive;
use pest::Parser;

extern crate strum;
use strum::IntoEnumIterator;

use std::{collections::BTreeSet, env, fs::File, io, path::PathBuf, unimplemented};
use std::io::Read;
use std::collections::BTreeMap;
use std::{convert::TryInto};

mod call;
use call::*;
mod ctxt;
use ctxt::*;
mod expression;
use expression::*;
mod func;
use func::*;
mod optimize;
use optimize::*;
mod stmt;
use stmt::*;
mod storage;
use storage::*;
mod parse;
use parse::*;
mod struct_def;
use struct_def::*;
mod types;
use types::*;

use assemble::{assemble,AssemblyInputLine, Instruction, Value};
use common::*;
use sim::*;

#[derive(Clone, Copy, Debug)]
enum Declaration {
    Local,
    Arg,
    Result,
    ReturnAddress,
}
#[derive(Debug)]
pub struct Variable {
    var_type: Type,
    decl: Declaration,
    storage: Storage,
}


fn print_state(c: &mut Computer) {
    let pc = u32::from_le_bytes(c.pc);
    //let pc_byte = *c.mem_byte_mut(pc);
    let sp = u32::from_le_bytes(*c.mem_word_mut(0x8000C));
    println!(
        "pc:{:05x} sp:{:08x} flags:{:01x} | mem[sp]:{:08x} mem[sp+4]:{:08x} mem[sp+8]:{:08x} mem[sp+c]:{:08x} mem[sp+10]:{:08x} r0:{:08x} r4:{:08x} r8:{:08x} ir0:{:?} ",
        //"pc:{:05x}={:02x} sp:{:08x} flags:{:01x} | mem[sp]:{:08x} mem[sp+4]:{:08x} mem[sp+8]:{:08x} mem[sp+c]:{:08x}| r0:{:08x} r4:{:08x} r8:{:08x} ir0:{:?} ",
        pc,
        //pc_byte,
        sp,
        c.flags.bits(),
        u32::from_le_bytes(*c.mem_word_mut(sp)),
        u32::from_le_bytes(*c.mem_word_mut(sp+4)),
        u32::from_le_bytes(*c.mem_word_mut(sp+8)),
        u32::from_le_bytes(*c.mem_word_mut(sp+0xC)),
        u32::from_le_bytes(*c.mem_word_mut(sp+0x10)),
        u32::from_le_bytes(*c.mem_word_mut(0x80000)),
        u32::from_le_bytes(*c.mem_word_mut(0x80004)),
        u32::from_le_bytes(*c.mem_word_mut(0x80008)),
        Opcode::iter().filter(|o| *o as u8 == c.ir0).next(),
    );
}

fn main() -> Result<(), std::io::Error> {
    let input = {
        let mut s = String::new();
        let stdin = io::stdin();
        stdin.lock().read_to_string(&mut s)?;
        s
    };
    let assembly = compile(&input, "main", &env::current_dir().unwrap());

    let rom = assemble::assemble(assembly);

    let args : Vec<_> = std::env::args().skip(1).map(|arg| u8::from_str_radix(&arg, 16).unwrap()).collect();

    let mut c = Computer::with_print(rom, false);

    *c.mem_word_mut(0x80000) = u32::to_le_bytes(0xAABBCCDD);

    if args.len() > 3 {
        panic!("main can take a max of three args.");
    }

    for (i,arg) in args.iter().enumerate() {
        *c.mem_byte_mut(0x80002 - i as u32) = *arg;
    }

    let mut last_ir0 = None;
    let mut running: bool = true;
    while running {
        running = c.step();
        // let pc = u32::from_le_bytes(c.pc);

        if last_ir0 != Some(c.ir0) {
            print_state(&mut c);
        }

        last_ir0 = Some(c.ir0);
    }

    eprintln!("Final R0:{:02x}", u32::from_le_bytes(*c.mem_word_mut(0x80000)));

    Ok(())
}

fn emit(ctxt: ProgramContext) -> Vec<AssemblyInputLine> {
    let main = ctxt.function_impls.get(&ctxt.entry)
        .expect(&format!("entry '{}' not found.", &ctxt.entry));

    let mut program = Vec::new();

    program.push(AssemblyInputLine::Comment("Types:".to_owned()));
    for (name, t) in &ctxt.types {
        program.push(AssemblyInputLine::Comment(format!("{} {:?}", name, t)));
    }

    program.push(AssemblyInputLine::Comment(format!("set up stack and call entry {}", ctxt.entry)));
    let initial_stack = 0x8FFF0;
    program.push(AssemblyInputLine::Instruction(Instruction {
        opcode: Opcode::LoadImm32,
        source: format!("init stack to 0x{:x}", initial_stack),
        args: vec![Value::Register(REG_SP), Value::Constant32(initial_stack)],
        resolved: None
    }));

    if Type::Void != main.def.return_type {
        assert!(main.def.return_type.byte_count(&ctxt) <= 4);
        program.push(AssemblyInputLine::Instruction(Instruction {
            opcode: Opcode::LoadImm8,
            source: format!("load RESULT placeholder byte for main"),
            args: vec![Value::Register(0x10), Value::Constant8(0xCC)],
            resolved: None
        }));
        for b in (0..4).rev() {
            program.push(AssemblyInputLine::Instruction(Instruction {
                opcode: Opcode::Push8,
                source: format!("push RESULT placeholder byte {} for main", b),
                args: vec![Value::Register(0x10)],
                resolved: None
            }));
        }
    }

    let main_args = main.def.args.len() as u8;
    for i in (0..main_args).rev() {
        for b in (0..4).rev() {
            program.push(AssemblyInputLine::Instruction(Instruction {
                opcode: Opcode::Push8,
                source: format!("push arg {}, byte {} for main", i, b),
                args: vec![Value::Register(4*i+b)],
                resolved: None
            }));
        }
    }
    program.push(AssemblyInputLine::from_str(&format!("!call :{}", ctxt.entry)));

    program.push(AssemblyInputLine::Instruction(Instruction {
        source: format!("discard args from main"),
        opcode: Opcode::LoadImm32,
        args: vec![Value::Register(0), Value::Constant32(4*(main_args as u32))],
        resolved: None,
    }));

    program.push(AssemblyInputLine::Instruction(Instruction {
        source: format!("discard args from main"),
        opcode: Opcode::Add32NoCarryIn,
        args: vec![Value::Register(REG_SP), Value::Register(0), Value::Register(REG_SP)],
        resolved: None,
    }));

    if main.def.return_type != Type::Void {
        for b in 0..4 {
            program.push(AssemblyInputLine::Instruction(Instruction {
                opcode: Opcode::Pop8,
                source: format!("pop result byte {} for main", b),
                args: vec![Value::Register(b)],
                resolved: None
            }));
        }
    }
    program.push(AssemblyInputLine::from_str("halt"));

    for f in &ctxt.function_impls {
        program.push(AssemblyInputLine::Comment(format!("{:?}", &f.1)));
        let f = f.1.emit(&ctxt);
        for l in f.lines {
            program.push(l);
        }
    }

    let mut optimizations_performed : usize;
    while 0 < {optimizations_performed = optimize_assembly(&mut program); optimizations_performed} {
        println!("performed {} optimizations", optimizations_performed);
    }

    program
}

fn compile(entry: &str, input: &str, root: &PathBuf) -> Vec<AssemblyInputLine> {
    let mut input = input.to_owned();

    'reparse: loop {
        let mut ctxt = ProgramContext {
            entry: entry.to_owned(),
            function_impls: BTreeMap::new(),
            function_defs: BTreeMap::new(),
            types: BTreeMap::new(),
            registers_available: (0x10..=0xFF).map(|r| Register(r)).collect(),
        };

        let to_parse = input.clone();

        let mut program = ProgramParser::parse(Rule::program, &to_parse).unwrap();
        let pairs = program.next().unwrap().into_inner();
        for pair in pairs {
            // dbg!(&pair);
            match pair.as_rule() {
                Rule::include => {
                    let original_stmt = pair.as_str().to_owned();
                    let original_byte_offset = input.find(&original_stmt).unwrap();
                    let original_byte_count = original_stmt.len(); //Returns the length of this String, in bytes
                    let mut pairs = pair.into_inner();
                    let relative_include_path = pairs.next().unwrap().as_str();
                    let mut path = root.clone();
                    path.push(relative_include_path);
                    // let path = fs::canonicalize(path).unwrap();
                    let mut file = File::open(&path).expect(&format!("Could not open '{:?}'", &path));
                    let mut contents = String::new();
                    contents += &format!("\n/* BEGIN {} */\n", &original_stmt);
                    file.read_to_string(&mut contents).unwrap();
                    contents += &format!("\n/* END   {} */\n", &original_stmt);
                    input.replace_range(original_byte_offset..original_byte_offset+original_byte_count, &contents);
                    println!("{}",&input);
                    continue 'reparse;
                }
                Rule::struct_decl => {
                    let mut pairs = pair.into_inner();

                    let name = pairs.next().unwrap().as_str().to_owned();

                    let mut fields = Vec::new();
                    for arg in pairs.next().unwrap().into_inner() {
                        let mut field_tokens = arg.into_inner();
                        let mut field_tokens = field_tokens.next().unwrap().into_inner();
                        let field_name = field_tokens.next().unwrap().as_str().to_owned();
                        let field_type = Type::parse(field_tokens.next().unwrap(), false);
                        fields.push((field_name, field_type));
                    }

                    ctxt.types.insert(name, StructDefinition{fields});
                },
                Rule::function => {
                    let f = FunctionDefinition::parse(&ctxt, pair);
                    ctxt.function_defs.insert(f.name.clone(), f);
                },
                Rule::EOI => {},
                _ => panic!("Unexpected rule: {:?}", pair)
            }
        }

        let defs = ctxt.function_defs.clone();
        for (name, def) in defs {
            let def = def.clone();
            let allocated = def.allocate(&mut ctxt);
            ctxt.function_impls.insert(name.clone(), allocated);
        }


        break emit(ctxt);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    enum TestVar {
        Ascii(&'static [u8]),
        Ptr(Vec<u8>),
        U8(u8),
        Usize(u32),
    }

    struct TestComputer<'a>(pub Computer<'a>);

    impl<'a> TestComputer<'a> {
        fn arg_base_addr() -> u32 {
            0x8100
        }

        fn test_programs_dir() -> PathBuf {
            let mut dir = env::current_exe().unwrap();
            dir.pop();
            dir.pop();
            dir.pop();
            dir.pop();
            dir.push("programs");
            dir
        }

        fn from_rom(rom: &[u8]) -> TestComputer<'a> {
            TestComputer(Computer::with_print(rom.to_vec(), false))
        }

        fn run(&mut self, inputs: &[u32], out: u32) {
            dbg!(inputs);
            assert!(inputs.len() <= 3);
            for (i,val) in inputs.iter().rev().enumerate() {
                let i = i as u32;
                *self.0.mem_word_mut(0x80000 + 4*i) = val.to_le_bytes();
            }


            let mut last_pc = None;
            let mut step_count = 0;
            while self.0.step() {
                if last_pc != Some(self.0.pc) {
                    print_state(&mut self.0);
                }
                last_pc = Some(self.0.pc);
                step_count += 1;
                assert!(step_count < 100000000);
            }
            assert_eq!(out, u32::from_le_bytes(*self.0.mem_word_mut(0x80000)));
        }
    }

    fn test_inputs(entry: &str, program: &str, pairs: &[(u32,u32,u32)]) {
        let assembly = compile(entry, program, &TestComputer::test_programs_dir());
        let rom = assemble(assembly);
        for (input1, input2, expected) in pairs {
            let mut c = TestComputer::from_rom(&rom);
            dbg!((input1, input2, expected));
            c.run(&[*input1, *input2], *expected);
        }
    }

    fn test_ttyout(entry: &str, program: &str, pairs: &[(u32,u32,&str)]) {
        let assembly = compile(entry, program, &TestComputer::test_programs_dir());
        let rom = assemble(assembly);
        for (input1, input2, expected) in pairs {
            let mut c = TestComputer::from_rom(&rom);
            dbg!((input1, input2, expected));
            c.run(&[*input1, *input2], 0);

            let mut lines = Vec::new();
            let mut line = String::new();
            for c in &c.0.tty_out {
                let c = *c as char;
                if c == '\n' {
                    lines.push(line);
                    line = String::new();
                } else {
                    line.push(c);
                }
            }
            assert_eq!(lines.iter().last().map(|s| s.as_str()), Some(*expected));
        }
    }

    fn test_ptr_inputs(entry: &str, program: &str, pairs: &[(&[u8],&[u8],u32)]) {
        let assembly = compile(entry, program, &TestComputer::test_programs_dir());
        let rom = assemble(assembly);
        let addr1 = 0x8100;
        let addr2 = 0xA000;

        for (input1, input2, expected) in pairs {
            let mut c = TestComputer::from_rom(&rom);
            dbg!((input1, input2, expected));
            for (i,b) in input1.iter().enumerate() {
                *c.0.mem_byte_mut(addr1 + i as u32) = *b;
            }
            for (i,b) in input2.iter().enumerate() {
                *c.0.mem_byte_mut(addr2 + i as u32) = *b;
            }
            c.run(&[addr1, addr2], *expected);
        }
    }

    fn test_var_inputs(entry: &str, program: &str, cases: &[(&[TestVar], u32)]) {
        let assembly = compile(entry, program, &TestComputer::test_programs_dir());
        let rom = assemble(assembly);
        for (args, expected) in cases
        {
            assert!(args.len() <= 3);

            let mut c = TestComputer::from_rom(&rom);
            let mut arg_addr = TestComputer::arg_base_addr();

            let mut ptr_arg = |bytes: &[u8]| -> u32 {
                let addr = arg_addr;
                arg_addr += bytes.len() as u32;

                for (i,b) in bytes.iter().enumerate() {
                    *c.0.mem_byte_mut(addr + i as u32) = *b;
                }

                let round_up_address = 4 *((arg_addr + 4 + 3) / 4);
                let round_up_bytes = round_up_address - arg_addr;
                for _ in 0..round_up_bytes {
                    c.0.add_data_trap(arg_addr);
                    arg_addr += 1;
                }

                assert_eq!(arg_addr % 4, 0);

                addr
            };

            let arg_values : Vec<_> = args.iter().map(|arg| {
                match arg {
                    TestVar::Ascii(bytes) => ptr_arg(bytes),
                    TestVar::Ptr(bytes) => ptr_arg(&bytes),
                    TestVar::U8(i) => *i as u32,
                    TestVar::Usize(i) => *i as u32,
                }
            }).collect();

            c.run(&arg_values, *expected);
        }
    }


    #[test]
    fn halt() {
        test_inputs(
            "main",
            include_str!("../../programs/halt.j"),
            &[(0,0,1)]);
    }

    #[test]
    fn add_u8() {
        test_inputs(
            "main",
            include_str!("../../programs/add_u8.j"),
            &[(0,0,7)]);
    }

    #[test]
    fn call_parameterless() {
        test_inputs(
            "main",
            include_str!("../../programs/call_parameterless.j"),
            &[(0,0,7)]);
    }

    #[test]
    fn idfn() {
        test_inputs(
            "main",
            include_str!("../../programs/idfn.j"),
            &[(0,0,7)]);
    }

    #[test]
    fn if_eq() {
        test_inputs(
            "main",
            include_str!("../../programs/if_eq.j"),
            &[(6,7,0), (8,7,0), (0,7,0), (0xFF,7,0), (7,7,1)]);
    }

    #[test]
    fn if_gt_unsigned() {
        test_inputs(
            "main",
            include_str!("../../programs/if_gt.j"),
            &[
                (0,1,0),
                (1,2,0),
                (8,7,1),
                (0,0xFF,0),
                (0x7F,7,1), (0xFF, 7, 1), (7,7,0)]);
    }

    #[test]
    fn if_gte_unsigned() {
        test_inputs(
            "main",
            include_str!("../../programs/if_gte.j"),
            &[(6,7,0), (8,7,1), (0,7,0), (0x7F,7,1), (0xFF, 7, 1), (7,7,1)]);
    }

    #[test]
    fn if_lt_unsigned() {
        test_inputs(
            "main",
            include_str!("../../programs/if_lt.j"),
            &[
            (0,0,0),
            (0,1,1),
            (6,7,1), (8,7,0), (0,7,1), (7,0,0), (0x7F,7,0), (0xFF, 7, 0), (7,7,0)]);
    }

    #[test]
    fn if_lte_unsigned() {
        test_inputs(
            "main",
            include_str!("../../programs/if_lte.j"),
            &[(6,7,1), (8,7,0), (0,7,1), (0x7F,7,0), (0xFF, 7, 0), (7,7,1)]);
    }

    #[test]
    fn if_ne() {
        test_inputs(
            "main",
            include_str!("../../programs/if_ne.j"),
            &[(6,7,1),(8,7,1),(0,7,1),(0xFF,7,1), (7,7,0)]);
    }

    #[test]
    fn if_ne_uptr() {
        test_inputs(
            "main",
            include_str!("../../programs/if_ne_uptr.j"),
            &[
                (6,7,1),(8,7,1),(0,7,1),(0xFF,7,1), (7,7,0),
                (0x0,0x100,1),(0x100,0x100,0),(0xAABBCCDD,0xAABBCCDD,0),
                (0xAABBCCDD,0xAABBCCDE,1),(0xAABBCCDD,0xAABBCDDD,1),
                (0xAABBCCDD,0xAABCCCDD,1),(0xAABBCCDD,0xABBBCCDD,1)]);
    }

    #[test]
    fn plusone() {
        test_inputs(
            "main",
            include_str!("../../programs/plusone.j"),
            &[(0,0,7)]);

    }

    #[test]
    fn fac_rec() {
        test_inputs(
            "main",
            include_str!("../../programs/fac_rec.j"),
            &[
                (0xCC,0,1),(0xCC,1,1),(0xCC, 2, 2),(0xCC,5,120)
            ]);
    }

    #[test]
    fn fac_iter() {
        test_inputs(
            "main",
            include_str!("../../programs/fac_iter.j"),
            &[
                (0xCC,0,1),(0xCC,1,1),(0xCC, 2, 2),(0xCC,5,120)
            ]);
    }

    #[test]
    fn fib() {
        test_inputs(
            "main",
            include_str!("../../programs/fib.j"),
            &[
                (0xCC,0,0),
                (0xCC,1,1),
                (0xCC,2,1),
                (0xCC,3,2),
                (0xCC,13,233)
                ]);
    }

    #[test]
    fn add_uptr() {
        test_inputs(
            "main",
            include_str!("../../programs/add_uptr.j"),
            &[
                (0x0,0x0,0x0),
                (0x0,0x1,0x1),
                (0x1,0x0,0x1),
                (0x1,0x1,0x2),
                (0x1,0xFF,0x100),
                (0xAABBCCDD, 0x0, 0xAABBCCDD),
                (0xAABBCCDD, 0x11111111, 0xBBCCDDEE),
                (0xFFFFFFFF, 0x1, 0x0),
                ]);
    }

    #[test]
    fn ptr() {
        test_ptr_inputs(
            "main",
            include_str!("../../programs/ptr.j"),
            &[
                (&0u32.to_le_bytes(), &0u32.to_le_bytes(), 0u32),
                (&0u32.to_le_bytes(), &1u32.to_le_bytes(), 1u32),
                (&1u32.to_le_bytes(), &2u32.to_le_bytes(), 3u32),
                (&0xAABBCCDDu32.to_le_bytes(), &0x11111111u32.to_le_bytes(), 0xBBCCDDEEu32),
                (&0xFFFFFFFFu32.to_le_bytes(), &0x1u32.to_le_bytes(), 0x0u32),
            ]
        );
    }

    #[test]
    fn structs() {
        test_inputs(
            "main",
            include_str!("../../programs/struct.j"),
            &[
                (0x0,0x0,0x0),
                (0x0,0x1,0x1),
                (0x1,0x0,0x1),
                (0x1,0x1,0x2),
                (0xAABBCCDD, 0x11111111, 0xBBCCDDEE),
                (0x1,0xFF,0x100),
                (0xAABBCCDD, 0x0, 0xAABBCCDD),
                (0xFFFFFFFF, 0x1, 0x0),
                ]);
    }

    #[test]
    fn divide() {
        test_inputs(
            "main",
            include_str!("../../programs/divide.j"),
            &[
                (0x1,0x1,0x1),
                (0x2,0x1,0x2),
                (0x1,0x2,0x0),
                (100,10,10),
                ]);
    }

    #[test]
    fn print_hex() {
        test_ttyout(
            "main",
            include_str!("../../programs/print_hex.j"),
            &[
                (0x0,0x0,"0"),
                (0x1,0x0,"1"),
                (0x9,0x0,"9"),
                (0xA,0x0,"A"),
                (0xF,0x0,"F"),
                (0x10,0x0,"10"),
                (0xFF,0x0,"FF"),
                ]);
    }

    #[test]
    fn local_array() {
        test_inputs(
            "main",
            include_str!("../../programs/local_array.j"),
            &[
                (0x0,0x0,0x0),
                (0x0,0x1,0x1),
                (0x1,0x0,0x1),
                (0x1,0xFF,0x0),
                ]);
    }

    #[test]
    fn array_to_ptr() {
        test_inputs(
            "main",
            include_str!("../../programs/array_to_ptr.j"),
            &[
                (0x0,0x0,0x0),
                (0x0,0x1,0x1),
                (0x1,0x0,0x1),
                (0x1,0xFF,0x0),
                ]);
    }

    #[test]
    fn array_loop() {
        test_inputs(
            "main",
            include_str!("../../programs/array_loop.j"),
            &[
                (0x0,0x0,0x0),
                (0x0,0x1,0x1),
                (0x1,0x0,0x1),
                (0x1,0xFF,0x0),
                ]);
    }

    #[test]
    fn strlen() {
        let mut long: Vec<u8> = (0..300).map(|_| 'a' as u8).collect();
        long.push(0);
        let expected = long.len() as u32 - 1;

        test_var_inputs(
            "strlen",
            include_str!("../../programs/strlen.j"),
            &[
                (&[TestVar::Ascii(b"\0")], 0u32),
                (&[TestVar::Ascii(b"hello\0")], 5u32),
                (&[TestVar::Ptr(long)], expected),
            ]
        );
    }

    #[test]
    fn strncmp() {
        let same_len = 300u32;
        let mut long1: Vec<u8> = (0..same_len).map(|_| 'a' as u8).collect();
        let mut long2 = long1.clone();
        long1.push(b'a');
        long1.push(0);
        long2.push(b'b');
        long2.push(0);

        test_var_inputs(
            "strncmp",
            include_str!("../../programs/strncmp.j"),
            &[
                (&[TestVar::Ascii(b"\0"), TestVar::Ascii(b"\0"), TestVar::Usize(0)], 0),
                (&[TestVar::Ascii(b"\0"), TestVar::Ascii(b"\0"), TestVar::Usize(1)], 0),
                (&[TestVar::Ascii(b"\0"), TestVar::Ascii(b"\0"), TestVar::Usize(2)], 0),
                (&[TestVar::Ascii(b"a\0"), TestVar::Ascii(b"a\0"), TestVar::Usize(1)], 0),
                (&[TestVar::Ascii(b"aa\0"), TestVar::Ascii(b"ab\0"), TestVar::Usize(1)], 0),
                (&[TestVar::Ascii(b"aa\0"), TestVar::Ascii(b"ab\0"), TestVar::Usize(2)], 255),
                (&[TestVar::Ascii(b"ab\0"), TestVar::Ascii(b"aa\0"), TestVar::Usize(2)], 1),
                (&[TestVar::Ascii(b"ab\0"), TestVar::Ascii(b"aa\0"), TestVar::Usize(1000)], 1),
                (&[TestVar::Ptr(long1.clone()), TestVar::Ptr(long2.clone()), TestVar::Usize(same_len)], 0),
                (&[TestVar::Ptr(long1), TestVar::Ptr(long2), TestVar::Usize(same_len+1)], 255),
            ]
        );
    }

    #[test]
    fn strstr() {
        test_var_inputs(
            "strstr",
            include_str!("../../programs/strstr.j"),
            &[
                (&[TestVar::Ascii(b"\0"), TestVar::Ascii(b"\0")], 0),
                (&[TestVar::Ascii(b"\0"), TestVar::Ascii(b"a\0")], 0),
                (&[TestVar::Ascii(b"a\0"), TestVar::Ascii(b"\0")], TestComputer::arg_base_addr()),
                (&[TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"h\0")], TestComputer::arg_base_addr()),
                (&[TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"he\0")], TestComputer::arg_base_addr()),
                (&[TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"Z\0")], 0),
                (&[TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"hi\0")], 0),
                (&[TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"el\0")], TestComputer::arg_base_addr() + 1),
                (&[TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"ll\0")], TestComputer::arg_base_addr() + 2),
                (&[TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"lo\0")], TestComputer::arg_base_addr() + 3),
                (&[TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"l\0")], TestComputer::arg_base_addr() + 2),
            ]
        );
    }
}