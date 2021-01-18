extern crate pest;
#[macro_use]
extern crate pest_derive;
use pest::{iterators::Pair, Parser};

extern crate strum;

extern crate packed_struct;

use common::*;
use std::{collections::BTreeMap, convert::TryInto};

#[derive(Parser)]
#[grammar = "assembly.pest"]
struct AssemblyParser;

#[derive(Clone, Debug)]
pub enum Value {
    Constant8(u8),
    Constant24(u32),
    Constant32(u32),
    Register(u8),
    Label24(String),
    Label32(String),
    PcOffset(u32),
}

#[derive(Clone, Debug)]
pub struct Instruction {
    pub source: String,
    pub opcode: Opcode,
    pub args: Vec<Value>,
    pub resolved: Option<Vec<u8>>
}

impl Instruction {
    pub fn size(&self) -> u32 {
        let mut sum = 0;
        sum += 1;
        for arg in &self.args {
            sum += match arg {
                Value::Constant8(_) => 1,
                Value::Constant24(_) => 3,
                Value::Constant32(_) => 4,
                Value::Register(_) => 1,
                Value::Label24(_) => 3,
                Value::Label32(_) => 4,
                Value::PcOffset(_) => 4,
            }
        }
        sum
    }

    pub fn resolve(&mut self, labels: &BTreeMap<String, u32>, current_pc: u32) {
        let mut bytes = Vec::new();
        bytes.push(self.opcode as u8);
        for arg in &self.args {
            match arg {
                Value::Constant8(c) => bytes.push(*c),
                Value::Constant24(c) => bytes.extend_from_slice(&u32::to_le_bytes(*c)[0..=2]),
                Value::Constant32(c) => bytes.extend_from_slice(&u32::to_le_bytes(*c)),
                Value::Register(r) => bytes.push(*r),
                Value::Label24(label) => {
                    let address =*labels.get(label).expect(&format!("label not found: {}", label));
                    bytes.extend_from_slice(&u32::to_le_bytes(address)[0..=2]);
                },
                Value::Label32(label) => {
                    let address =*labels.get(label).expect("label not found");
                    bytes.extend_from_slice(&u32::to_le_bytes(address));
                },
                Value::PcOffset(offset) => bytes.extend_from_slice(
                    &u32::to_le_bytes(offset.wrapping_add(current_pc)))
            }
        }

        assert_eq!(self.size(), bytes.len() as u32);

        self.resolved = Some(bytes);
    }
}

fn parse_unsigned_hex(hex: Pair<Rule>) -> u32 {
    assert_eq!(hex.as_rule(), Rule::unsigned_hex_constant);
    u32::from_str_radix(hex.as_str(), 16).unwrap()
}

fn parse_signed_hex(hex: Pair<Rule>) -> u32 {
    assert_eq!(hex.as_rule(), Rule::signed_hex_constant);
    let mut chars = hex.as_str().chars();
    if chars.next().unwrap() == '-' {
        let positive =
            u32::from_str_radix(hex.as_str().trim_start_matches('-'), 16).unwrap();
        ((positive as i64) * (-1i64)).try_into().expect("does not fit in u32")
    } else {
        parse_unsigned_hex(hex.into_inner().next().unwrap())
    }
}

fn parse_char(constant: Pair<Rule>) -> u8 {
    assert_eq!(constant.as_rule(), Rule::char_constant);
    let mut chars = constant.as_str().chars();
    assert_eq!('\'', chars.next().unwrap());
    let c = chars.next().unwrap();
    assert_eq!('\'', chars.next().unwrap());
    assert_eq!(None, chars.next());
    c as u8
}

fn parse_constant8(constant: Pair<Rule>) -> Value {
    assert_eq!(constant.as_rule(), Rule::constant8);
    let constant = constant.into_inner().next().unwrap();
    match constant.as_rule() {
        Rule::signed_hex_constant => Value::Constant8(parse_signed_hex(constant).try_into().expect("does not fit in u8")),
        Rule::char_constant => Value::Constant8(parse_char(constant)),
        r => panic!("unexpexted {:?}", r),
    }
}

fn parse_constant24(constant: Pair<Rule>) -> Value {
    assert_eq!(constant.as_rule(), Rule::constant24);
    let constant = constant.into_inner().next().unwrap();
    match constant.as_rule() {
        Rule::signed_hex_constant => Value::Constant24(parse_signed_hex(constant)),
        Rule::char_constant => Value::Constant24(parse_char(constant) as u32),
        Rule::label => Value::Label24(constant.as_str().to_owned()),
        r => panic!("unexpexted {:?}", r),
    }
}

fn parse_constant32(constant: Pair<Rule>) -> Value {
    assert_eq!(constant.as_rule(), Rule::constant32);
    let constant = constant.into_inner().next().unwrap();
    match constant.as_rule() {
        Rule::signed_hex_constant => Value::Constant32(parse_signed_hex(constant)),
        Rule::char_constant => Value::Constant32(parse_char(constant) as u32),
        Rule::label => Value::Label32(constant.as_str().to_owned()),
        r => panic!("unexpexted {:?}", r),
    }
}

fn parse_instruction(line: Pair<Rule>) -> Instruction {
    assert_eq!(line.as_rule(), Rule::instruction);
    let instruction = line.into_inner().next().unwrap();
    let source = instruction.as_span().as_str().to_owned();

    let mut args = Vec::new();

    let opcode_name = format!("{:?}", instruction.as_rule()); //ew
    let opcode = match &opcode_name {
        name => std::str::FromStr::from_str(name)
            .expect(&format!("Could not parse op {}", name)),
    };

    let arg_tokens = instruction.into_inner();

    for arg in arg_tokens {
        let value = match arg.as_rule() {
            Rule::register | Rule::deref_register => {
                let reg = parse_unsigned_hex(arg.into_inner().next().unwrap());
                Value::Register(reg.try_into().expect("invalid register"))
            }
            Rule::constant8 => parse_constant8(arg),
            Rule::constant24 => parse_constant24(arg),
            Rule::constant32 => parse_constant32(arg),
            r => panic!("unexpexted {:?}", r),
        };

        args.push(value);
    }
    
    Instruction {
        source,
        opcode,
        args,
        resolved: None,
    }
}

#[derive(Debug)]
pub enum AssemblyInputLine {
    Comment(String),
    Label(String),
    Instruction(Instruction),
    PseudoCall(Value),
    PseudoReturn(),
    LiteralString(String),
    Literal8(u8),
}

impl AssemblyInputLine {
    pub fn from_str(input: &str) -> AssemblyInputLine {
        let mut line = AssemblyParser::parse(Rule::line, &input).unwrap();
        AssemblyInputLine::parse(line.next().unwrap())
    }

    fn parse(line: Pair<Rule>) -> AssemblyInputLine {
        assert_eq!(line.as_rule(), Rule::line);
        let line = line.into_inner().next().unwrap();
        match line.as_rule() {
            Rule::instruction => AssemblyInputLine::Instruction(parse_instruction(line)),
            Rule::label => AssemblyInputLine::Label(line.as_str().to_owned()),
            Rule::comment => AssemblyInputLine::Comment(line.as_str().to_owned()),
            Rule::pseudo_call => {
                let call_arg = line.into_inner().next().unwrap();
                // let source = call.as_span().as_str();
                AssemblyInputLine::PseudoCall(parse_constant24(call_arg))
            },
            Rule::pseudo_return => AssemblyInputLine::PseudoReturn(),
            Rule::literal_string => {
                let string = line.into_inner().next().unwrap();
                AssemblyInputLine::LiteralString(string.as_str().to_owned())
            }
            Rule::literal8 => {
                let byte = line.into_inner().next().unwrap();
                let byte = match parse_constant8(byte) {
                    Value::Constant8(byte) => byte,
                    _ => panic!(),
                };
                AssemblyInputLine::Literal8(byte)
            }
            r => panic!("unexpected {:?}", r),
        }
    }
}

#[derive(Debug)]
enum AssemblyOutputLine {
    Comment(String),
    Instruction(Instruction),
    Constant8(u8),
    String(String),
}

pub fn assemble_from_str(input: &str) -> Vec<u8> {
    println!("v2.0 raw");

    {
        use pest::iterators::Pairs;
        fn dump_tree(pairs: Pairs<Rule>, indent: usize) {
            for pair in pairs {
                if pair.as_rule() != Rule::program {
                    print!("# ");
                    for _ in 0..indent {
                        print!(" ");
                    }
                    println!("{:?}: '{}'", pair.as_rule(), pair.as_str());
                }
                dump_tree(pair.into_inner(), indent + 1);
            }
        };

        let assembly = AssemblyParser::parse(Rule::program, input).unwrap();
        dump_tree(assembly, 0);
    }

    let mut assembly = AssemblyParser::parse(Rule::program, &input).unwrap();
    let assembly = assembly.next().unwrap();
    let assembly = assembly.into_inner();

    let mut assembly_lines = Vec::new();
    for line in assembly {
        if line.as_rule() == Rule::EOI {
            break;
        }
        let parsed_line = AssemblyInputLine::parse(line);

        assembly_lines.push(parsed_line);
    }

    assemble_inner(assembly_lines)
}

pub fn assemble(input: Vec<AssemblyInputLine>) -> Vec<u8> {
    println!("v2.0 raw");
    assemble_inner(input)
}

fn assemble_inner(mut input: Vec<AssemblyInputLine>) -> Vec<u8> {

    let mut lines : Vec<AssemblyOutputLine> = Vec::new();
    let mut labels = BTreeMap::new();

    let mut pc = 0u32;
    for line in input.drain(..) {
        let source = format!("{:?}", line);
        //lines.push(AssemblyOutputLine::Comment(source.to_owned()));

        match line {
            AssemblyInputLine::Instruction(inst) => {
                pc += inst.size();
                lines.push(AssemblyOutputLine::Instruction(inst));
            }
            AssemblyInputLine::Comment(comment) => {
                lines.push(AssemblyOutputLine::Comment(comment));
            }
            AssemblyInputLine::Label(label) => {
                labels.insert(label.to_owned(), pc);
                lines.push(AssemblyOutputLine::Comment(label.to_owned()));
            }
            AssemblyInputLine::PseudoReturn() => {
                let inst = Instruction {
                    opcode: Opcode::JmpMem,
                    source: source.to_owned(),
                    args: vec![Value::Register(REG_SP)],
                    resolved: None,
                };
                pc += inst.size();
                lines.push(AssemblyOutputLine::Instruction(inst));
            }
            AssemblyInputLine::PseudoCall(value) => {
                let mut insts = vec![
                    Instruction {
                        opcode: Opcode::AddImm32IgnoreCarry,
                        source: source.to_owned(),
                        args: vec![Value::Register(REG_SP), Value::Constant32((-4i32) as u32)],
                        resolved: None,
                    },
                    Instruction {
                        opcode: Opcode::StoreImm32,
                        source: source.to_owned(),
                        args: vec![Value::Register(REG_SP), Value::PcOffset(6+4)],
                        resolved: None,
                    },
                    Instruction {
                        source: source.to_owned(),
                        opcode: Opcode::JmpImm,
                        args: vec![value],
                        resolved: None,
                    },
                    // remove return address
                    Instruction {
                        opcode: Opcode::AddImm32IgnoreCarry,
                        source: source.to_owned(),
                        args: vec![Value::Register(REG_SP), Value::Constant32(4)],
                        resolved: None,
                    },
                ];

                for inst in insts.drain(..) {
                    pc += inst.size();
                    lines.push(AssemblyOutputLine::Instruction(inst));
                }
            }
            AssemblyInputLine::Literal8(byte) => {
                pc += 1;
                lines.push(AssemblyOutputLine::Constant8(byte));
            }
            AssemblyInputLine::LiteralString(string) => {
                pc += string.as_bytes().len() as u32;
                lines.push(AssemblyOutputLine::String(string));
            }
        }
    }

    let mut rom = Vec::new();
    let mut pc = 0u32;
    for line in lines.as_mut_slice() {
        match line {
            AssemblyOutputLine::Comment(comment) => {
                println!("# {}", &comment);
            }
            AssemblyOutputLine::Instruction(i) => {
                println!("# {:05x} {:?}", pc, i);
                i.resolve(&labels, pc);

                for byte in i.resolved.as_ref().unwrap() {
                    print!("{:02x} ", byte);
                    rom.push(*byte);
                    pc += 1;
                }
                println!();
            }
            AssemblyOutputLine::Constant8(byte) => {
                println!("{:02x} ", byte);
                rom.push(*byte);
                pc += 1;
            }
            AssemblyOutputLine::String(string) => {
                println!("# '{}'", &string);
                for byte in string.as_bytes() {
                    print!("{:02x} ", byte);
                    rom.push(*byte);
                    pc += 1;
                }
                println!();
            }
        }
    }

    rom
}
