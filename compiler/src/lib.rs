extern crate pest;
#[macro_use]
extern crate pest_derive;
use pest::Parser;

extern crate strum;
use strum::IntoEnumIterator;

extern crate packed_struct;

use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::{collections::BTreeSet, fs::File, unimplemented};
use std::{collections::BTreeMap, convert::TryInto, io::Read};

pub mod backend;
mod call;
use call::*;
mod comparison;
use comparison::*;
mod ctxt;
use ctxt::*;
mod expression;
use expression::*;
mod func;
use func::*;
pub mod il;
pub mod ilsim;
mod optimize;
mod stmt;
use stmt::*;
mod parse;
use parse::*;
mod struct_def;
use struct_def::*;
mod types;
use types::*;

use assemble::{AssemblyInputLine, Instruction, Value};
use common::*;
use sim::*;


pub fn print_state(c: &Computer) {
    let pc = u32::from_le_bytes(c.pc);
    //let pc_byte = *c.mem_byte_mut(pc);
    let sp = c.reg_u32(common::REG_SP);
    let op = Opcode::iter().filter(|o| *o as u8 == c.ir0).next();
    print!(
        "pc:{:05x} sp:{:08x} flags:{:01x}",
        pc,
        sp,
        c.flags.bits());
    
    for stack_offset in [0,4,8,0xc] {
        let stack_mem = sp + stack_offset;
        print!(" mem[sp");
        if stack_offset != 0 {
            print!("+{}", stack_offset);
        }
        print!("]:");
        if let Some(stack_slice) = c.try_mem_slice(stack_mem, 4) {
            let slice: &[u8; 4] = stack_slice.try_into().unwrap();
            print!("{:08x}", u32::from_le_bytes(*slice));
        } else {
            print!("err     ");
        }
    }
    for r in 0u8..=255u8 {
        if (r % 4) != 0 { continue; }
        if r == REG_SP { continue; }
        let v = c.reg_u32(r);
        if v == 0xCCCCCCCC { continue; }
        print!(" r{:02x}:{:08x}", r, c.reg_u32(r));
    }
    println!(" ir0:{:?}", &op);
    // println!(
    //     " r0:{:08x} r4:{:08x} r8:{:08x} r10:{:08x} r14:{:08x} ir0:{:?}",
    //     c.reg_u32(0),
    //     c.reg_u32(4),
    //     c.reg_u32(8),
    //     c.reg_u32(0x10),
    //     c.reg_u32(0x14),
    //     &op,
    // );
    // match op {
    //     Some(Opcode::Store32Part1) | Some(Opcode::Store32Part2) => {
    //         let value_reg_offset = if op == Some(Opcode::Store32Part1) { 1 } else { 0xFFFFFFFE };
    //         let value_reg = c.mem_byte(c.ir0_pc.wrapping_add(value_reg_offset));
    //         let value = c.reg_u32(value_reg);
    //         let addr_reg_offset = if op == Some(Opcode::Store32Part1) { 2 } else { 0xFFFFFFFF };
    //         let addr_reg = c.mem_byte(c.ir0_pc.wrapping_add(addr_reg_offset));
    //         let addr = c.reg_u32(addr_reg);
    //         let mem_value = c.mem_word(addr);
    //         println!(" (r{:02x} == {:08x}) ==> (mem[r{:02x}={:08x}] == {:08x})", 
    //             value_reg, value,
    //             addr_reg, addr, mem_value
    //         );
    //     }
    //     _ => {}
    // }

    if let Some(symbol) = c.image.symbols.get(&pc) {
        let mut seen = BTreeSet::new();
        for note in &symbol.notes {
            if seen.insert(note) {
                println!("   {}", note);
            }
        }   
    }
}

pub const INITIAL_STACK: u32 = (RAM_MAX as u32/4)*4;
pub const STATICS_START_ADDRESS: u32 = common::RAM_MIN as u32 + common::REGISTER_COUNT;

pub fn create_program(entry: &str, input: &str, root: &Path) -> ProgramContext {
    let mut input = input.to_owned();

    'reparse: loop {
        let mut ctxt = ProgramContext {
            entry: entry.to_owned(),
            function_defs: BTreeMap::new(),
            struct_types: BTreeMap::new(),
            statics_base_address: STATICS_START_ADDRESS,
            image_base_address: 0,
        };
        
        // add intrinsics
        ctxt.function_defs.insert(
            "__mul8_16".to_owned(),
            FunctionDefinition { 
                name: "__mul8_16".to_owned(), 
                attributes: HashSet::new(),
                args: vec![("x".to_owned(), Type::Number(NumberType::U8)),("y".to_owned(), Type::Number(NumberType::U8))],
                vars: BTreeMap::new(),
                return_type: Type::Number(NumberType::U16), 
                body: FunctionImpl::Intrinsic(Intrinsic::Mul8_16),
            }
        );

        let to_parse = input.clone();

        let mut includes = BTreeSet::new();

        // handle includes
        {
            let mut program = ProgramParser::parse(Rule::program, &to_parse)
                .expect(&format!("Could not parse `{}`.", &to_parse));
            let pairs = program.next().unwrap().into_inner();
            for pair in pairs {
                // dbg!(&pair);
                let rule = pair.as_rule();
                match rule {
                    Rule::include => {
                        let original_stmt = pair.as_str().to_owned();
                        let original_byte_offset = input.find(&original_stmt).unwrap();
                        let original_byte_count = original_stmt.len(); //Returns the length of this String, in bytes
                        let mut pairs = pair.into_inner();
                        let relative_include_path = pairs.next().unwrap().as_str();
                        let mut path : PathBuf = root.into();
                        path.push(relative_include_path);
                        let path = path.canonicalize().unwrap();
                        if includes.insert(path.to_owned()) {
                            let mut file = File::open(&path).expect(&format!("Could not open '{:?}'", &path));
                            let mut contents = String::new();
                            contents += &format!("\n/* BEGIN INCLUDE '{:?}' */\n", &path);
                            file.read_to_string(&mut contents).unwrap();
                            contents += &format!("\n/* END   INCLUDE '{:?}' */\n", &path);
                            // println!("replacing {}+0x{:02x}=`{}` with {:?}", original_byte_offset, original_byte_count, &original_stmt, path);
                            input.replace_range(original_byte_offset..original_byte_offset+original_byte_count, &contents);
                            // println!("{}",&input);
                            continue 'reparse;
                        }
                    }
                    _ => {},
                }
            }
        }
        let mut program = ProgramParser::parse(Rule::program, &to_parse)
            .expect(&format!("Could not parse `{}`.", &to_parse));
        let pairs = program.next().unwrap().into_inner();
        for pair in pairs {
            // dbg!(&pair);
            let rule = pair.as_rule();
            match rule {
                Rule::image_base_address | Rule::statics_base_address => {
                    let mut pairs = pair.into_inner();
                    let hex_number = pairs.next().unwrap();
                    assert_eq!(hex_number.as_rule(), Rule::expression);
                    let hex_number = Expression::parse(hex_number);
                    let hex_number = hex_number.try_get_const().expect("Could not resolve base address as constant.");
                    let hex_number: u32 = hex_number.try_into().expect("base adddress does not fit in u32.");
                    match rule {
                        Rule::image_base_address => { ctxt.image_base_address = hex_number; }
                        Rule::statics_base_address => { ctxt.statics_base_address = hex_number; }
                        _ => panic!()
                    }
                }
                Rule::include => {},
                Rule::struct_decl => {
                    let mut pairs = pair.into_inner();

                    let name = pairs.next().unwrap().as_str().to_owned();

                    let mut fields = Vec::new();
                    for arg in pairs.next().unwrap().into_inner() {
                        let mut field_tokens = arg.into_inner();
                        let mut field_tokens = field_tokens.next().unwrap().into_inner();
                        let field_name = field_tokens.next().unwrap().as_str().to_owned();
                        let field_type = Type::parse(field_tokens.next().unwrap(), true);
                        fields.push((field_name, field_type));
                    }

                    ctxt.struct_types.insert(name, StructDefinition{fields});
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

        break ctxt;
    }
}