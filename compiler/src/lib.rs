extern crate pest;
#[macro_use]
extern crate pest_derive;
use packed_struct::PrimitiveEnum;
use pest::Parser;

extern crate strum;
use strum::IntoEnumIterator;

extern crate packed_struct;

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
pub mod types;
pub use types::*;

use assemble::{AssemblyInputLine, Instruction, Value};
use common::*;
use sim::*;


pub fn print_state(c: &Computer) {
    let pc = u32::from_le_bytes(c.pc);
    //let pc_byte = *c.mem_byte_mut(pc);
    let op = Opcode::from_primitive(c.ir0);
    print!("pc:{:05x}", pc);

    if (0..4).into_iter().all(|i| c.regs_written[(REG_SP+i) as usize]) {
        let sp = c.reg_u32(common::REG_SP);

        print!(" sp:{:08x}", sp);
    
        for stack_offset in [0,4,8,0xc] {
            let stack_mem = sp + stack_offset;
            if let Some(stack_slice) = c.try_mem_slice(stack_mem, 4) {
                let slice: &[u8; 4] = stack_slice.try_into().unwrap();
                let val = u32::from_le_bytes(*slice);
                if val != 0xCCCC_CCCC {
                    print!(" mem[sp");
                    if stack_offset != 0 {
                        print!("+{}", stack_offset);
                    }
                    print!("]:");
                    print!("{:08x}", val);
                }
            }
        }
    }

    print!(" flags:{:01x}", c.flags.bits());

    for r in 0u8..=255u8 {
        if (r % 4) != 0 { continue; }
        if r == REG_SP { continue; }
        if (0..4).into_iter().all(|i| c.regs_written[(r+i) as usize]) {
            print!(" r{:02x}:{:08x}", r, c.reg_u32(r));
        } else if !(0..4).into_iter().any(|i| c.regs_written[(r+i) as usize]) {
        } else {
            for i in 0..4 {
                if c.regs_written[(r+i) as usize] {
                    print!(" r{:02x}:{:02x}", r+i, c.reg_u8(r+i));
                }
            }
        }
        
    }
    if let Some(pc) = c.ir0_pc {
        print!(" ir0:{:?}", &op);
        if let Some(o) = Opcode::from_primitive(c.ir0) {
            for (i, _size) in o.expected_arg_sizes().iter().enumerate() {
                print!(" arg{}:{:x}", i, c.mem_byte(pc + 1 + i as u32));
            }
        }
    }
    println!();
    // println!(
    //     " r0:{:08x} r4:{:08x} r8:{:08x} r10:{:08x} r14:{:08x} ir0:{:?}",
    //     c.reg_u32(0),
    //     c.reg_u32(4),
    //     c.reg_u32(8),
    //     c.reg_u32(0x10),
    //     c.reg_u32(0x14),
    //     &op,
    // );

    if let Some(symbol) = c.image.symbols.get(&pc) {
        let mut seen = BTreeSet::new();
        for note in &symbol.notes {
            if seen.insert(note) {
                // println!("   {}", note);
            }
        }   
    }
}

fn parse_file(path: &Path, input: &str, ctxt: &mut ProgramContext) {
    let mut program = ProgramParser::parse(Rule::program, input)
        .unwrap_or_else(|_| panic!("Could not parse `{}`.", input));
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
                let hex_number = match hex_number.try_get_const() {
                    Some(Constant::Number(n)) => n.as_u32(),
                    _ => panic!("Could not resolve base address as constant."),
                };
                match rule {
                    Rule::image_base_address => { ctxt.image_base_address = hex_number; }
                    Rule::statics_base_address => { ctxt.statics_base_address = hex_number; }
                    _ => panic!()
                }
            }
            Rule::include => {
                let mut pairs = pair.into_inner();
                let relative_include_path = pairs.next().unwrap().as_str();

                let mut paths_checked = Vec::new();
                let mut file = None;
                for root in &ctxt.roots {
                    let mut path : PathBuf = root.into();
                    path.push(relative_include_path);
                    if let Ok(path) = path.canonicalize() {
                        if let Ok(f) = File::open(&path) {
                            file = Some((f, path));
                            break;
                        } else {
                            paths_checked.push(path);
                        }
                    } else {
                        paths_checked.push(path);
                    }
                }
                let (mut file, path) = file.unwrap_or_else(|| {
                    use std::fmt::Write;
                    let mut msg = format!("Could not find `{:?}`. Searched [", &relative_include_path);
                    for p in paths_checked {
                        write!(msg, "{:?}", &p).unwrap();
                    }
                    panic!("{}", msg);
                });
                if ctxt.includes.insert(path.to_owned()) {
                    let mut contents = String::new();
                    file.read_to_string(&mut contents).unwrap();
                    parse_file(&path, &contents, ctxt);
                }
            },
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
                let f = FunctionDefinition::parse(path, &ctxt, pair);
                let existing = ctxt.function_defs.insert(f.name.clone(), f);
                assert!(existing.is_none(), "{:?} is already defined.", &existing);
            },
            Rule::global_const_value => {
                let mut decl = pair.into_inner();
                let mut decl_tokens = decl.next().unwrap().into_inner();
                let var_name = decl_tokens.next().unwrap().as_str().trim().to_owned();
                let unresolved_var_type = Type::parse(decl_tokens.next().unwrap(), true);

                let value = Expression::parse(decl.next().unwrap());
                let const_value = value.try_get_const_bytes().unwrap();

                let var_type = match unresolved_var_type {
                    Type::Array(et, None) => {
                        let element_size = et.byte_count(&ctxt);
                        let element_count = round_up(const_value.byte_count(), element_size);
                        Type::Array(et, Some(element_count))
                    }
                    _ => unresolved_var_type,
                };

                ctxt.consts.insert(var_name, (var_type, const_value));
            }
            Rule::global_static_value => {
                let mut decl = pair.into_inner();
                let mut decl_tokens = decl.next().unwrap().into_inner();
                let var_name = decl_tokens.next().unwrap().as_str().trim().to_owned();
                let var_type = Type::parse(decl_tokens.next().unwrap(), true);
                ctxt.statics.insert(var_name, var_type);
            }
            Rule::EOI => {},
            _ => panic!("Unexpected rule: {:?}", pair)
        }
    }
}

pub fn create_program(path: &Path, entry: &str, input: &str, root: &Path) -> ProgramContext {
    assert_eq!(STATICS_START_ADDRESS, 0x0F1000);

    let mut roots = vec![root.into()];
    if let Some(parent) = root.parent() {
        let mut parent: PathBuf = parent.into();
        parent.push("lib");
        if let Ok(lib) = parent.canonicalize() {
            roots.push(lib);
        }
    }

    let mut ctxt = ProgramContext {
        entry: entry.to_owned(),
        function_defs: BTreeMap::new(),
        struct_types: BTreeMap::new(),
        statics_base_address: STATICS_START_ADDRESS,
        image_base_address: 0,
        consts: BTreeMap::new(),
        statics: BTreeMap::new(),
        roots,
        includes: BTreeSet::new(),
    };

    // add intrinsics
    for intrinsic in Intrinsic::iter() {
        let name = intrinsic.assembly_name().to_owned();
        let body = FunctionImpl::Intrinsic(intrinsic);
        let def = match intrinsic {
            Intrinsic::Mul8_16 => FunctionDefinition { 
                name, 
                attributes: BTreeSet::new(),
                args: vec![("x".to_owned(), Type::Number(NumberType::U8)),("y".to_owned(), Type::Number(NumberType::U8))],
                vars: BTreeMap::new(),
                return_type: Type::Number(NumberType::U16), 
                body,
            },
            Intrinsic::EnableInterrupts | Intrinsic::DisableInterrupts => FunctionDefinition { 
                name, 
                attributes: BTreeSet::new(),
                args: vec![],
                vars: BTreeMap::new(),
                return_type: Type::Void, 
                body,
            },
            Intrinsic::ReadyToRead | Intrinsic::ReadyToWrite | Intrinsic::IoRead0 | Intrinsic::IoRead1 | Intrinsic::IoRead2 => FunctionDefinition { 
                name, 
                attributes: BTreeSet::new(),
                args: vec![],
                vars: BTreeMap::new(),
                return_type: Type::Number(NumberType::U8), 
                body,
            },
            Intrinsic::IoWrite0 | Intrinsic::IoWrite1 | Intrinsic::IoWrite2 => FunctionDefinition { 
                name, 
                attributes: BTreeSet::new(),
                args: vec![("data".to_owned(),Type::Number(NumberType::U8))],
                vars: BTreeMap::new(),
                return_type: Type::Void, 
                body,
            },
        };
        ctxt.function_defs.insert(def.name.to_owned(), def);
    }

    parse_file(path, &input, &mut ctxt);
    ctxt
}