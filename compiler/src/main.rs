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

use assemble::{AssemblyInputLine, Instruction, Value};
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
    let (_program, assembly) = compile(&input, "main", &env::current_dir().unwrap());

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

fn emit(ctxt: &mut ProgramContext) -> Vec<AssemblyInputLine> {
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

    if main_args != 0 {
        program.push(AssemblyInputLine::Instruction(Instruction {
            source: format!("discard args from main"),
            opcode: Opcode::LoadImm32,
            args: vec![Value::Register(8), Value::Constant32(4*(main_args as u32))],
            resolved: None,
        }));

        program.push(AssemblyInputLine::Instruction(Instruction {
            source: format!("discard args from main"),
            opcode: Opcode::Add32NoCarryIn,
            args: vec![Value::Register(REG_SP), Value::Register(8), Value::Register(REG_SP)],
            resolved: None,
        }));
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

const STATICS_START_ADDRESS: u32 = 0x8000_1000;

fn compile(entry: &str, input: &str, root: &PathBuf) -> (ProgramContext, Vec<AssemblyInputLine>) {
    let mut input = input.to_owned();

    'reparse: loop {
        let mut ctxt = ProgramContext {
            entry: entry.to_owned(),
            function_impls: BTreeMap::new(),
            function_defs: BTreeMap::new(),
            globals: BTreeMap::new(),
            types: BTreeMap::new(),
            registers_available: (0x10..=0xFF).map(|r| Register(r)).collect(),
            static_cur_address: STATICS_START_ADDRESS,
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
                // Rule::global => {
                //     let mut decl = pair.into_inner();
                //     let mut decl_tokens = decl.next().unwrap().into_inner();
                //     let var_name = decl_tokens.next().unwrap().as_str().trim().to_owned();
                //     let var_type = Type::parse(decl_tokens.next().unwrap(), true);
                //     unimplemented!();
                // }
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

        let assembly = emit(&mut ctxt);
        break (ctxt, assembly);
    }
}

#[cfg(test)]
mod tests {
    use super::*;


    #[derive(Debug, PartialEq, Clone)]
    enum TestVar {
        Ascii(&'static [u8]),
        Ptr(Vec<u8>),
        U8(u8),
        Usize(u32),
    }

    impl TestVar {
        fn byte_count(&self) -> u32 {
            match self {
                TestVar::Ascii(_) | TestVar::Ptr(_) | TestVar::Usize(_) => 4,
                TestVar::U8(_) => 1
            }
        }
    }

    impl From<u8> for TestVar {
        fn from(i: u8) -> Self {
            TestVar::U8(i)
        }
    }

    impl From<&u8> for TestVar {
        fn from(i: &u8) -> Self {
            TestVar::U8(*i)
        }
    }

    impl From<u32> for TestVar {
        fn from(i: u32) -> Self {
            TestVar::Usize(i)
        }
    }

    impl From<&u32> for TestVar {
        fn from(i: &u32) -> Self {
            TestVar::Usize(*i)
        }
    }

    struct TestComputer<'a>(pub Computer<'a>);

    impl<'a> TestComputer<'a> {
        fn arg_base_addr() -> u32 {
            0x8100
        }

        fn arg_base_addr_var(offset: u32) -> TestVar {
            TestVar::Usize(TestComputer::arg_base_addr() + offset)
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

        fn run(&mut self, inputs: &[u32]) -> u32{
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
            u32::from_le_bytes(*self.0.mem_word_mut(0x80000))
        }
    }

    fn test_inputs<'a, T1,T2,T3>(entry: &str, program: &str, cases: &'a [(T1,T2,T3)])
        where TestVar: From<&'a T1>, TestVar: From<&'a T2>, TestVar: From<&'a T3>
    {
        let cases: Vec<(Vec<TestVar>,TestVar)> = cases
            .iter()
            .map(|(in1,in2,out)| ([in1.into(), in2.into()].to_vec(), out.into()))
            .collect();
        test_var_inputs(entry, program, cases.as_slice());
    }

    fn test_tty(entry: &str, program: &str, pairs: &[(&str,u32,u32,&str)]) {
        let (_ctxt, assembly) = compile(entry, program, &TestComputer::test_programs_dir());
        let rom = assemble::assemble(assembly);
        for (ttyin, input1, input2, expected) in pairs {
            let mut c = TestComputer::from_rom(&rom);
            dbg!((ttyin, input1, input2, expected));
            for ch in ttyin.chars() {
                c.0.tty_in.push_back(ch as u8);
            }
            assert_eq!(0, c.run(&[*input1, *input2]) & 0xFF);

            let mut out = String::new();
            for c in &c.0.tty_out {
                let c = *c as char;
                out.push(c);
            }

            assert_eq!(out.as_str(), *expected);
        }
    }

    fn test_ptr_inputs(entry: &str, program: &str, pairs: &[(&[u8],&[u8],u32)]) {
        let (_ctxt, assembly) = compile(entry, program, &TestComputer::test_programs_dir());
        let rom = assemble::assemble(assembly);
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
            assert_eq!(*expected, c.run(&[addr1, addr2]));
        }
    }

    fn check_args(ctxt: &ProgramContext, test_case: &(Vec<TestVar>, TestVar)) {
        let entry = &ctxt.function_defs[&ctxt.entry];
        let entry_args = &entry.args;
        let (test_args, test_return) = test_case;
        assert_eq!(entry_args.len(), test_args.len());
        for (i, (entry_arg_name, entry_arg_type)) in entry_args.iter().enumerate() {
            let entry_arg_size = entry_arg_type.byte_count(ctxt);
            let test_arg_size = test_args[i].byte_count();
            assert_eq!(entry_arg_size, test_args[i].byte_count(),
                "Entry arg '{}' type '{:?}' expects size {} but test provided size {}.", 
                entry_arg_name, entry_arg_type, entry_arg_size, test_arg_size);
        }

        let entry_return_size = entry.return_type.byte_count(ctxt);
        let test_return_size = test_return.byte_count();
        assert_eq!(entry_return_size, test_return_size,
            "Entry function returns type '{:?}' expects size {} but test provided size {}.", 
            entry.return_type, entry_return_size, test_return_size);
    }

    fn assemble(entry: &str, program: &str) -> (ProgramContext, Vec<u8>) {
        let (ctxt, assembly) = compile(entry, program, &TestComputer::test_programs_dir());

        let rom = assemble::assemble(assembly);
        (ctxt, rom)
    }

    fn test_var_input(rom: &[u8], args: &Vec<TestVar>) -> u32 {
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

        c.run(&arg_values)
    }

    fn test_var_inputs(entry: &str, program: &str, cases: &[(Vec<TestVar>, TestVar)]) {
        let (ctxt, rom) = assemble(entry, program);
        for case in cases
        {
            check_args(&ctxt, case);
            let (args, expected) = case;
            let result = test_var_input(&rom, args);
            let expected: &TestVar = expected.into();
            let result  = match expected {
                TestVar::U8(_) => TestVar::U8((result & 0xFF) as u8),
                TestVar::Usize(_) => TestVar::Usize(result),
                _ => unimplemented!(),
            };

            assert_eq!(expected, &result);
        }
    }


    #[test]
    fn halt() {
        test_var_inputs(
            "main",
            include_str!("../../programs/halt.j"),
            &[(vec![],1u8.into())]);
    }

    #[test]
    fn add_u8() {
        test_var_inputs(
            "main",
            include_str!("../../programs/add_u8.j"),
            &[(vec![],7u8.into())]);
    }

    #[test]
    fn call_parameterless() {
        test_var_inputs(
            "main",
            include_str!("../../programs/call_parameterless.j"),
            &[(vec![],7u8.into())]);
    }

    #[test]
    fn idfn() {
        test_var_inputs(
            "main",
            include_str!("../../programs/idfn.j"),
            &[(vec![],TestVar::U8(7u8))]);
    }

    #[test]
    fn if_eq() {
        test_inputs(
            "main",
            include_str!("../../programs/if_eq.j"),
            &[(6u8,7u8,0u8), (8,7,0), (0,7,0), (0xFF,7,0), (7,7,1)]);
    }

    #[test]
    fn if_gt_unsigned() {
        test_inputs(
            "main",
            include_str!("../../programs/if_gt.j"),
            &[
                (0u8,1u8,0u8),
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
            &[(6u8,7u8,0u8), (8,7,1), (0,7,0), (0x7F,7,1), (0xFF, 7, 1), (7,7,1)]);
    }

    #[test]
    fn if_lt_unsigned() {
        test_inputs(
            "main",
            include_str!("../../programs/if_lt.j"),
            &[
            (0u8,0u8,0u8),
            (0,1,1),
            (6,7,1), (8,7,0), (0,7,1), (7,0,0), (0x7F,7,0), (0xFF, 7, 0), (7,7,0)]);
    }

    #[test]
    fn if_lte_unsigned() {
        test_inputs(
            "main",
            include_str!("../../programs/if_lte.j"),
            &[(6u8,7u8,1u8), (8,7,0), (0,7,1), (0x7F,7,0), (0xFF, 7, 0), (7,7,1)]);
    }

    #[test]
    fn if_ne() {
        test_inputs(
            "main",
            include_str!("../../programs/if_ne.j"),
            &[(6u8,7u8,1u8),(8,7,1),(0,7,1),(0xFF,7,1), (7,7,0)]);
    }

    #[test]
    fn if_ne_uptr() {
        test_inputs(
            "main",
            include_str!("../../programs/if_ne_uptr.j"),
            &[
                (6u32,7u32,1u8),(8,7,1),(0,7,1),(0xFF,7,1), (7,7,0),
                (0x0,0x100,1),(0x100,0x100,0),(0xAABBCCDD,0xAABBCCDD,0),
                (0xAABBCCDD,0xAABBCCDE,1),(0xAABBCCDD,0xAABBCDDD,1),
                (0xAABBCCDD,0xAABCCCDD,1),(0xAABBCCDD,0xABBBCCDD,1)]);
    }

    #[test]
    fn plusone() {
        test_var_inputs(
            "main",
            include_str!("../../programs/plusone.j"),
            &[(vec![],7u8.into())]);

    }

    #[test]
    fn fac_rec() {
        test_var_inputs(
            "main",
            include_str!("../../programs/fac_rec.j"),
            &[
                (vec![0x0u8.into()],1u8.into()),
                (vec![0x1u8.into()],1u8.into()),
                (vec![0x2u8.into()],2u8.into()),
                (vec![0x5u8.into()],120u8.into()),
            ]);
    }

    #[test]
    fn fac_iter() {
        test_var_inputs(
            "fac",
            include_str!("../../programs/fac_iter.j"),
            &[
                (vec![0u8.into()],1u8.into()),
                (vec![1u8.into()],1u8.into()),
                (vec![2u8.into()],2u8.into()),
                (vec![5u8.into()],120u8.into()),
            ]);
    }

    #[test]
    fn fib() {
        test_var_inputs(
            "fib",
            include_str!("../../programs/fib.j"),
            &[
                (vec![0u8.into()],0u8.into()),
                (vec![1u8.into()],1u8.into()),
                (vec![2u8.into()],1u8.into()),
                (vec![3u8.into()],2u8.into()),
                // (vec![13u8.into()],233u8.into()),
                ]);
    }

    #[test]
    fn fib_memo() {
        test_var_inputs(
            "main",
            include_str!("../../programs/fib_memo.j"),
            &[
                (vec![0u8.into()],0u8.into()),
                (vec![1u8.into()],1u8.into()),
                (vec![2u8.into()],1u8.into()),
                (vec![3u8.into()],2u8.into()),
                (vec![13u8.into()],233u8.into()),
                ]);
    }


    #[test]
    fn statics() {
        test_var_inputs(
            "main",
            include_str!("../../programs/static.j"),
            &[
                (vec![1u8.into()],2u8.into()),
                (vec![2u8.into()],4u8.into()),
                ]);
    }
    #[test]
    fn add_uptr() {
        test_inputs(
            "main",
            include_str!("../../programs/add_uptr.j"),
            &[
                (0x0u32,0x0u32,0x0u32),
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
                (0x0u32,0x0u32,0x0u32),
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
    fn heap() {
        let (_, rom) = assemble("heap_start", include_str!("../../programs/heap.j"));
        let heap_start = test_var_input(&rom, &vec![0u8.into()]);
        assert!(heap_start >= STATICS_START_ADDRESS);
    }

    #[test]
    fn divide() {
        test_inputs(
            "main",
            include_str!("../../programs/divide.j"),
            &[
                (0x1u8,0x1u8,0x1u8),
                (0x2,0x1,0x2),
                (0x1,0x2,0x0),
                (100,10,10),
                ]);
    }

    #[test]
    fn print_hex() {
        test_tty(
            "main",
            include_str!("../../programs/print_hex.j"),
            &[
                ("",0x0,0x0,"0\n"),
                ("",0x1,0x0,"1\n"),
                ("",0x9,0x0,"9\n"),
                ("",0xA,0x0,"A\n"),
                ("",0xF,0x0,"F\n"),
                ("",0x10,0x0,"10\n"),
                ("",0xFF,0x0,"FF\n"),
                ]);
    }

    #[test]
    fn echo() {
        test_tty(
            "main",
            include_str!("../../programs/echo.j"),
            &[
                ("0\n",0x0,0x0,"0\n"),
                ("01\n",0x0,0x0,"01\n"),
                ]);
    }


    #[test]
    fn local_array() {
        test_inputs(
            "main",
            include_str!("../../programs/local_array.j"),
            &[
                (0x0u8,0x0u8,0x0u8),
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
                (0x0u8,0x0u8,0x0u8),
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
                (0x0u8,0x0u8,0x0u8),
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
                (vec![TestVar::Ascii(b"\0")], TestVar::Usize(0)),
                (vec![TestVar::Ascii(b"hello\0")], TestVar::Usize(5)),
                (vec![TestVar::Ptr(long)], TestVar::Usize(expected)),
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
                (vec![TestVar::Ascii(b"\0"), TestVar::Ascii(b"\0"), TestVar::Usize(0)], TestVar::U8(0)),
                (vec![TestVar::Ascii(b"\0"), TestVar::Ascii(b"\0"), TestVar::Usize(1)], TestVar::U8(0)),
                (vec![TestVar::Ascii(b"\0"), TestVar::Ascii(b"\0"), TestVar::Usize(2)], TestVar::U8(0)),
                (vec![TestVar::Ascii(b"a\0"), TestVar::Ascii(b"a\0"), TestVar::Usize(1)], TestVar::U8(0)),
                (vec![TestVar::Ascii(b"aa\0"), TestVar::Ascii(b"ab\0"), TestVar::Usize(1)], TestVar::U8(0)),
                (vec![TestVar::Ascii(b"aa\0"), TestVar::Ascii(b"ab\0"), TestVar::Usize(2)], TestVar::U8(255)),
                (vec![TestVar::Ascii(b"ab\0"), TestVar::Ascii(b"aa\0"), TestVar::Usize(2)], TestVar::U8(1)),
                (vec![TestVar::Ascii(b"ab\0"), TestVar::Ascii(b"aa\0"), TestVar::Usize(1000)], TestVar::U8(1)),
                (vec![TestVar::Ptr(long1.clone()), TestVar::Ptr(long2.clone()), TestVar::Usize(same_len)], TestVar::U8(0)),
                (vec![TestVar::Ptr(long1), TestVar::Ptr(long2), TestVar::Usize(same_len+1)], TestVar::U8(255)),
            ]
        );
    }

    #[test]
    fn strstr() {
        test_var_inputs(
            "strstr",
            include_str!("../../programs/strstr.j"),
            &[
                (vec![TestVar::Ascii(b"\0"), TestVar::Ascii(b"\0")], TestVar::Usize(0)),
                (vec![TestVar::Ascii(b"\0"), TestVar::Ascii(b"a\0")], TestVar::Usize(0)),
                (vec![TestVar::Ascii(b"a\0"), TestVar::Ascii(b"\0")], TestComputer::arg_base_addr_var(0)),
                (vec![TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"h\0")], TestComputer::arg_base_addr_var(0)),
                (vec![TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"he\0")], TestComputer::arg_base_addr_var(0)),
                (vec![TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"Z\0")], TestVar::Usize(0)),
                (vec![TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"hi\0")], TestVar::Usize(0)),
                (vec![TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"el\0")], TestComputer::arg_base_addr_var(1)),
                (vec![TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"ll\0")], TestComputer::arg_base_addr_var(2)),
                (vec![TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"lo\0")], TestComputer::arg_base_addr_var(3)),
                (vec![TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"l\0")], TestComputer::arg_base_addr_var(2)),
            ]
        );
    }
}