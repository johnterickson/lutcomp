extern crate pest;
#[macro_use]
extern crate pest_derive;
use pest::Parser;

extern crate strum;
use strum::IntoEnumIterator;

use std::io;
use std::io::Read;
use std::collections::{BTreeMap,BTreeSet};
use std::{convert::TryInto, str::FromStr};

use assemble::*;
use common::*;
use sim::*;

fn amount_for_round_up(a: u32, b: u32) -> u32 {
    (b - (a % b)) % b
}

#[derive(Parser)]
#[grammar = "j.pest"]
struct ProgramParser;



#[derive(Debug, PartialEq, Eq)]
enum Operator {
    Add,
    Subtract,
    Multiply,
    Or,
    NotEquals,
}

impl Operator {
    fn parse(pair: pest::iterators::Pair<Rule>) -> Operator {
        match pair.as_str() {
            "+" => Operator::Add,
            "-" => Operator::Subtract,
            "*" => Operator::Multiply,
            "||" => Operator::Or,
            "!=" => Operator::NotEquals,
            _ => panic!(),
        }
    }
}

struct FunctionContext {
    pub regs_touched: BTreeSet<u8>,
    pub stack: BTreeMap<String, LocalStorage>,
    pub lines: Vec<AssemblyInputLine>,
    pub additional_offset: u32,
    pub block_counter: usize,
}

impl FunctionContext {
    fn add_inst(&mut self, i: Instruction) {
        //println!("{:?}",&i);
        self.lines.push(AssemblyInputLine::Instruction(i));
    }

    fn add_macro(&mut self, s: String) {
        self.lines.push(AssemblyInputLine::from_str(&s))
    }

    fn find_local(&mut self, local: &str) -> LocalStorage {
        let local = self.stack
            .get(local)
            .expect(&format!("could not find {}", local));
        match local {
            LocalStorage::Stack(offset) => {
                LocalStorage::Stack(*offset + self.additional_offset)
            },
            LocalStorage::Register(r) => {
                self.regs_touched.insert(*r);
                LocalStorage::Register(*r)
            }
        }
    }
}

#[derive(Debug)]
enum Expression {
    Ident(String),
    Number(i32),
    Operation(Operator, Box<Expression>, Box<Expression>)
}

impl Expression {
    fn parse(pair: pest::iterators::Pair<Rule>) -> Expression {
        assert_eq!(Rule::expression, pair.as_rule());
        let pair = pair.into_inner().next().unwrap();
        match pair.as_rule() {
            Rule::number => {
                let mut n = 0;
                let mut digits = pair.into_inner();
                while let Some(digit) = digits.next() {
                    let digit = i32::from_str(digit.as_str()).expect("Couldn't parse integer.");
                    n *= 10;
                    n += digit;
                }
                Expression::Number(n)
            },
            Rule::ident => {
                let mut label = String::new();
                let mut chars = pair.into_inner();
                while let Some(c) = chars.next() {
                    label += c.as_str();
                }
                Expression::Ident(label)
            },
            Rule::operator_expression => {
                let mut pairs = pair.into_inner();
                let left = Expression::parse(pairs.next().unwrap());
                let op = Operator::parse(pairs.next().unwrap());
                let right = Expression::parse(pairs.next().unwrap());
                Expression::Operation(op, Box::new(left), Box::new(right))
            },
            _ => unimplemented!()
        }
    }

    fn is_tail(&self) -> bool {
        match self {
            Expression::Ident(_) => true,
            Expression::Number(_) => true,
            Expression::Operation(_,_,_) => false,
        }
    }

    fn emit(&self, ctxt: &mut FunctionContext) -> () {
        ctxt.lines.push(AssemblyInputLine::Comment(format!("Evaluating expression: {:?} additional_offset:{}", &self, ctxt.additional_offset)));

        match self {
            Expression::Number(n) => {
                let n = *n as u8;

                ctxt.add_inst(Instruction {
                    source: format!("{:?}", &self),
                    opcode: Opcode::LoadImm8,
                    args: vec![Value::Register(0), Value::Constant8(n)],
                    resolved: None,
                });

                ctxt.add_inst(Instruction {
                    source: format!("{:?}", &self),
                    opcode: Opcode::Push8,
                    args: vec![Value::Register(0)],
                    resolved: None,
                });

                ctxt.additional_offset += 1;
            },
            Expression::Ident(n) => {
                let local = ctxt.find_local(n);
                match local {
                    LocalStorage::Register(r) => {
                        // ctxt.add_inst(Instruction::LoadReg(r));
                        unimplemented!();
                    },
                    LocalStorage::Stack(offset) => {
                        ctxt.add_inst(Instruction {
                            source: format!("{:?}", &self),
                            opcode: Opcode::LoadImm32,
                            args: vec![Value::Register(0), Value::Constant32(offset)],
                            resolved: None,
                        });

                        ctxt.add_inst(Instruction {
                            source: format!("{:?}", &self),
                            opcode: Opcode::Add32NoCarryIn,
                            args: vec![Value::Register(REG_SP), Value::Register(0), Value::Register(4)],
                            resolved: None,
                        });

                        ctxt.add_inst(Instruction {
                            source: format!("{:?}", &self),
                            opcode: Opcode::Load8,
                            args: vec![Value::Register(4), Value::Register(0)],
                            resolved: None,
                        });
                        ctxt.add_inst(Instruction {
                            source: format!("{:?}", &self),
                            opcode: Opcode::Push8,
                            args: vec![Value::Register(0)],
                            resolved: None,
                        });
                        ctxt.additional_offset += 1;
                    }
                }
            },
            Expression::Operation(op, left, right) => {
                
                left.emit(ctxt);
                right.emit(ctxt);

                ctxt.add_inst(Instruction {
                    opcode: Opcode::Pop8,
                    resolved: None,
                    source: format!("{:?}", &self),
                    args: vec![Value::Register(1)]
                });
                ctxt.additional_offset -= 1;

                ctxt.add_inst(Instruction {
                    opcode: Opcode::Pop8,
                    resolved: None,
                    source: format!("{:?}", &self),
                    args: vec![Value::Register(0)]
                });
                ctxt.additional_offset -= 1;

                // left in 0; right in 1, result in 2

                match op {
                    Operator::Add => {
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::Add8NoCarry,
                            resolved: None,
                            source: format!("{:?}", &self),
                            args: vec![Value::Register(0), Value::Register(1), Value::Register(2)]
                        });
                    },
                    Operator::Or => {
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::Or8,
                            resolved: None,
                            source: format!("{:?}", &self),
                            args: vec![Value::Register(0), Value::Register(1), Value::Register(2)]
                        });
                    },
                    Operator::Multiply => {
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::Mul8Part1,
                            resolved: None,
                            source: format!("{:?}", &self),
                            args: vec![Value::Register(0), Value::Register(1), Value::Register(2)]
                        });
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::Mul8Part2,
                            resolved: None,
                            source: format!("{:?}", &self),
                            args: vec![]
                        });
                    },
                    Operator::Subtract | Operator::NotEquals => {
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::Negate,
                            resolved: None,
                            source: format!("{:?}", &self),
                            args: vec![Value::Register(1)]
                        });
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::Add8NoCarry,
                            resolved: None,
                            source: format!("{:?}", &self),
                            args: vec![Value::Register(0), Value::Register(1), Value::Register(2)]
                        });
                    },
                }

                ctxt.add_inst(Instruction {
                    opcode: Opcode::Push8,
                    resolved: None,
                    source: format!("{:?}", &self),
                    args: vec![Value::Register(2)]
                });
                ctxt.additional_offset += 1;
            }
        }
        
        ctxt.lines.push(AssemblyInputLine::Comment(format!("Evaluated expression: {:?} additional_offset:{}", &self, ctxt.additional_offset)));
    }
}

const RESULT : &'static str = "RESULT";
const EPILOGUE : &'static str = "EPILOGUE";

#[derive(Debug)]
enum Statement {
    Assign {local: String, value: Expression},
    Call { local: String, function: String, parameters: Vec<Expression> },
    If {predicate: Expression, when_true: Vec<Statement> },
    Return { value: Expression},
    Load {local: String, address: Expression },
    Store {local: String, address: Expression },
}

impl Statement {
    fn parse(pair: pest::iterators::Pair<Rule>) -> Statement {
        assert_eq!(Rule::statement, pair.as_rule());
        let pair = pair.into_inner().next().unwrap();

        match pair.as_rule() {
            Rule::assign => {
                let mut pairs = pair.into_inner();
                let local = pairs.next().unwrap().as_str().trim().to_owned();
                let value = Expression::parse(pairs.next().unwrap());
                Statement::Assign { local, value }
            },
            Rule::call => {
                let mut pairs = pair.into_inner();
                let local = pairs.next().unwrap().as_str().trim().to_owned();
                let function = pairs.next().unwrap().as_str().to_owned();

                let mut parameters = Vec::new();
                while let Some(arg) = pairs.next() {
                    parameters.push(Expression::parse(arg));
                }

                Statement::Call { local, function, parameters }
            },
            Rule::if_statement => {
                let mut pairs = pair.into_inner();
                let predicate = Expression::parse(pairs.next().unwrap());
                let mut when_true = Vec::new();
                while let Some(stmt) = pairs.next() {
                    when_true.push(Statement::parse(stmt));
                }
                Statement::If { predicate, when_true }
            },
            Rule::return_statement => {
                let expr = pair.into_inner().next().unwrap();
                Statement::Return { value: Expression::parse(expr) }
            },
            Rule::load => {
                let mut pairs = pair.into_inner();
                let local = pairs.next().unwrap().as_str().trim().to_owned();
                let address = Expression::parse(pairs.next().unwrap());
                Statement::Load { local, address }
            },
            Rule::store => {
                let mut pairs = pair.into_inner();
                let local = pairs.next().unwrap().as_str().trim().to_owned();
                let address = Expression::parse(pairs.next().unwrap());
                Statement::Store { local, address }
            }
            _ => panic!("Unexpected {:?}", pair)
        }
    }

    fn emit(&self, ctxt: &mut FunctionContext, function_name: &str) -> () {
        ctxt.lines.push(AssemblyInputLine::Comment(format!("Begin statement {:?}", self)));
        match self {
            Statement::Load{local, address} => {
                unimplemented!();
                // address.emit(ctxt);

                // ctxt.add_inst(Instruction {
                //     opcode: Opcode::Pop8,
                //     resolved: None,
                //     source: String::new(),
                //     args: vec![Value::Register(0)]
                // });

                // ctxt.add_inst(Instruction {
                //     opcode: Opcode::Load8,
                //     resolved: None,
                //     source: String::new(),
                //     args: vec![Value::Register(0)]
                // });

                // let local = ctxt.find_local(local);
                // match local {
                //     LocalStorage::Register(r) => {
                //         unimplemented!();
                //     }
                //     LocalStorage::Stack(offset) => {
                //         ctxt.add_inst(Instruction {
                //             opcode: Opcode::LoadImm32,
                //             resolved: None,
                //             source: String::new(),
                //             args: vec![Value::Register(4), Value::Constant32(offset as u32)]
                //         });
                //         ctxt.add_inst(Instruction {
                //             opcode: Opcode::Add32NoCarry,
                //             resolved: None,
                //             source: String::new(),
                //             args: vec![Value::Register(REG_SP),Value::Register(4),Value::Register(8)]
                //         });

                //         ctxt.add_inst(Instruction {
                //             opcode: Opcode::Store8,
                //             resolved: None,
                //             source: String::new(),
                //             args: vec![Value::Register(4), Value::Constant32(offset as u32)]
                //         });

                //         ctxt.add_inst(Instruction::StoreToStack(StackOffset::new(offset as u8)));
                //     }
                // }
            },
            Statement::Store{local, address} => {
                unimplemented!();
                // address.emit(ctxt);
                // ctxt.add_inst(Instruction::StoreAddr);

                // let local = ctxt.find_local(local);
                // match local {
                //     LocalStorage::Register(r) => {
                //         unimplemented!();
                //     }
                //     LocalStorage::Stack(offset) => {
                //         ctxt.add_inst(Instruction::WithoutPush(
                //             PushableInstruction::LoadFromStack(StackOffset::new(offset as u8))));
                //     }
                // }
                // ctxt.add_inst(Instruction::StoreMem);
            },
            Statement::Assign{local, value} => {
                value.emit(ctxt);
                ctxt.add_inst(Instruction {
                    opcode: Opcode::Pop8,
                    resolved: None,
                    source: format!("{:?}", &self),
                    args: vec![Value::Register(0)]
                });
                ctxt.additional_offset -= 1;

                let local = ctxt.find_local(local);
                match local {
                    LocalStorage::Register(r) => {
                        unimplemented!();
                    }
                    LocalStorage::Stack(offset) => {
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::LoadImm32,
                            resolved: None,
                            source: format!("{:?}", &self),
                            args: vec![Value::Register(4), Value::Constant32(offset as u32)]
                        });
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::Add32NoCarryIn,
                            resolved: None,
                            source: format!("{:?}", &self),
                            args: vec![Value::Register(REG_SP),Value::Register(4),Value::Register(8)]
                        });
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::Store8,
                            resolved: None,
                            source: format!("{:?}", &self),
                            args: vec![Value::Register(0), Value::Register(8)]
                        });
                    }
                }
            },
            Statement::Return{ value } => {
                value.emit(ctxt);
                ctxt.add_inst(Instruction {
                    opcode: Opcode::Pop8,
                    resolved: None,
                    source: format!("{:?}", &self),
                    args: vec![Value::Register(0)]
                });
                ctxt.additional_offset -= 1;

                let result_offset = match ctxt.find_local(RESULT) {
                    LocalStorage::Register(_) => unimplemented!(),
                    LocalStorage::Stack(offset) => offset,
                };
                ctxt.add_inst(Instruction {
                            opcode: Opcode::LoadImm32,
                            resolved: None,
                            source: format!("{:?}", &self),
                            args: vec![Value::Register(4), Value::Constant32(result_offset as u32)]
                        });
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::Add32NoCarryIn,
                            resolved: None,
                            source: format!("{:?}", &self),
                            args: vec![Value::Register(REG_SP),Value::Register(4),Value::Register(8)]
                        });
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::Store8,
                            resolved: None,
                            source: format!("{:?}", &self),
                            args: vec![Value::Register(0), Value::Register(8)]
                        });

                // ctxt.add_inst(Instruction::StoreToStack(StackOffset::new(result_offset as u8)));

                // assert_eq!(ctxt.additional_offset, 0);
                
                // if ctxt.additional_offset != 0 {
                //     ctxt.add_inst(Instruction::Discard(StackOffset::new(ctxt.additional_offset as u8)));
                // }
                ctxt.add_inst(Instruction {
                    opcode: Opcode::JmpImm,
                    source: format!("{:?}", &self),
                    args: vec![Value::Label24(format!(":{}__{}", function_name, EPILOGUE))],
                    resolved: None                    
                });
            },
            Statement::Call{ local, function, parameters} => { 

                // put 0xCC in for RESULT
                ctxt.add_inst(Instruction {
                    opcode: Opcode::LoadImm8,
                    source: format!("{:?} placeholder value for RESULT", &self),
                    args: vec![Value::Register(0), Value::Constant8(0xCC)],
                    resolved: None,
                });
                ctxt.add_inst(Instruction {
                    opcode: Opcode::Push8,
                    source: format!("{:?} placeholder value for RESULT", &self),
                    args: vec![Value::Register(0)],
                    resolved: None,
                });
                ctxt.additional_offset += 1;

                let mut args = 0;
                for p in parameters {
                    p.emit(ctxt);
                    args += 1;
                }

                // padding
                let mut padding = 0;
                while ctxt.additional_offset % 4 != 0 {
                    ctxt.add_inst(Instruction {
                        opcode: Opcode::Push8,
                        source: format!("{:?} padding additional_offset:{}", &self, ctxt.additional_offset),
                        args: vec![Value::Register(0)],
                        resolved: None,
                    });
                    ctxt.additional_offset += 1;
                    padding += 1;
                }

                // store return address
                ctxt.add_inst(Instruction {
                    opcode: Opcode::AddImm32IgnoreCarry,
                    source: format!("{:?} return address", &self),
                    args: vec![Value::Register(REG_SP), Value::Constant32((-4i32) as u32)],
                    resolved: None,
                });
                ctxt.add_inst(Instruction {
                    opcode: Opcode::StoreImm32,
                    source: format!("{:?} return address", &self),
                    args: vec![Value::Register(REG_SP), Value::PcOffset(4+6)],
                    resolved: None,
                });
                ctxt.additional_offset += 4;

                // call
                ctxt.add_inst(Instruction {
                    opcode: Opcode::JmpImm,
                    source: format!("{:?} call {}", &self, function),
                    args: vec![Value::Label24(format!(":{}",function))],
                    resolved: None,
                });

                // discard paramters and padding and return address
                ctxt.add_inst(Instruction {
                    opcode: Opcode::AddImm32IgnoreCarry,
                    source: format!("{:?} clean up stack", &self),
                    args: vec![Value::Register(REG_SP), Value::Constant32(args + padding + 4)],
                    resolved: None,
                });
                ctxt.additional_offset -= args + padding + 4;

                // for r in regs_to_save.iter().rev() {
                //     unimplemented!();
                //     // ctxt.add_macro(format!("pop {}", r));
                //     // ctxt.additional_offset -= 1;
                // }

                // result is now at the top of the stack
                // assert_eq!(ctxt.additional_offset, 1);

                ctxt.add_inst(Instruction {
                    opcode: Opcode::Pop8,
                    source: format!("{:?} pop result off stack", &self),
                    args: vec![Value::Register(0)],
                    resolved: None,
                });
                ctxt.additional_offset -= 1;

                // stack is now back to normal
                // assert_eq!(ctxt.additional_offset, 0);

                let local = ctxt.find_local(local);
                match local {
                    LocalStorage::Register(r) => {
                        unimplemented!();

                    },
                    LocalStorage::Stack(offset) => {
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::LoadImm32,
                            resolved: None,
                            source: format!("{:?} store result", &self),
                            args: vec![Value::Register(4), Value::Constant32(offset as u32)]
                        });
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::Add32NoCarryIn,
                            resolved: None,
                            source: format!("{:?} store result", &self),
                            args: vec![Value::Register(REG_SP),Value::Register(4),Value::Register(8)]
                        });
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::Store8,
                            resolved: None,
                            source: format!("{:?} store result", &self),
                            args: vec![Value::Register(0), Value::Register(8)]
                        });
                    }
                }
            },
            Statement::If{predicate, when_true} => {
                let if_skip = "IF_SKIP";
                predicate.emit(ctxt); // result in top of stack

                let jump_label = format!("{}_{}_{}", function_name, if_skip, ctxt.block_counter);

                ctxt.block_counter += 1;

                let source = format!("IF ({:?})  ... ", predicate);

                ctxt.add_inst(Instruction {
                    opcode: Opcode::Pop8,
                    source: source.to_owned(),
                    args: vec![Value::Register(0)],
                    resolved: None,
                });
                ctxt.additional_offset -= 1;

                ctxt.add_inst(Instruction {
                    opcode: Opcode::OrImm8,
                    source: source.to_owned(),
                    args: vec![Value::Register(0), Value::Constant8(0)],
                    resolved: None,
                });
                ctxt.add_inst(Instruction {
                    opcode: Opcode::JzImm,
                    source: source.to_owned(),
                    args: vec![Value::Label24(format!(":{}", &jump_label))],
                    resolved: None,
                });

                // let mut count = 0;
                for s in when_true {
                    // let scope = format!("{}_stmt{}", scope, count);
                    s.emit(ctxt, function_name);
                    // count += 1;
                }
                
                ctxt.lines.push(AssemblyInputLine::Label(format!(":{}", &jump_label)));
            },
        }
        ctxt.lines.push(AssemblyInputLine::Comment(format!("Done  statement {:?}", self)));
    }
}

#[derive(Clone, Copy, Debug)]
enum LocalStorage {
    Register(u8),
    Stack(u32),
}

#[derive(Debug)]
struct Function {
    name: String,
    args: Vec<String>,
    locals: BTreeSet<String>,
    body: Vec<Statement>,
}

impl Function {
    fn parse(pair: pest::iterators::Pair<Rule>) -> Function {
        assert_eq!(Rule::function, pair.as_rule());

        let mut args = Vec::new();

        let mut pairs = pair.into_inner();

        let name = pairs.next().unwrap().as_str().to_owned();

        for arg in pairs.next().unwrap().into_inner() {
            let arg = arg.as_str();
            args.push(arg.to_owned());
        }

        let body : Vec<Statement> = pairs.next().unwrap().into_inner().map(|p| Statement::parse(p)).collect();

        // find locals
        let mut locals = BTreeSet::new();

        fn find_locals(s: &Statement, args: &Vec<String>, locals: &mut BTreeSet<String>) {
            match s {
                Statement::Assign{local, value:_} 
                | Statement::Load{local, address:_}
                | Statement::Store{local, address:_ }
                | Statement::Call{ local, function:_, parameters:_ } => { 
                    if !args.contains(local) {
                        locals.insert(local.clone()); 
                    }
                },
                Statement::Return{ value:_ } => {},
                Statement::If{ predicate:_, when_true:ss } => {
                    for s in ss {
                        find_locals(s, args, locals);
                    }
                },
            }
        };

        for s in body.iter() {
            find_locals(s, &args, &mut locals);
        }

        Function { name, args, locals, body }
    }

    /*

    stack:

    SP ->   [padding so top of stack is 32-bit aligned]
            local 3
            local 2
            local 1
            32-bit return address
            [padding so RA is 32-bit aligned]
            arg 2
            arg 1
            RESULT
    */

    fn emit(&self) -> FunctionContext {
        let mut ctxt = FunctionContext {
            stack: BTreeMap::new(),
            lines: Vec::new(),
            additional_offset: 0,
            regs_touched: BTreeSet::new(),
            block_counter: 0,
        };
        ctxt.lines.push(AssemblyInputLine::Comment(format!("# Function: {}", &self.name)));
        ctxt.lines.push(AssemblyInputLine::Label(format!(":{}", &self.name)));

        let max_register_locals = 0u32;

        let register_local_count = std::cmp::min(max_register_locals, self.locals.len() as u32);
        let stack_local_count = self.locals.len() as u32 - register_local_count;

        let mut stack_size = 1u32; // result
        stack_size += self.args.len() as u32;
        let arg_padding = amount_for_round_up(stack_size, 4);
        dbg!(arg_padding);
        stack_size += arg_padding;
        stack_size += 4; // return address
        stack_size += stack_local_count as u32;
        let local_padding = amount_for_round_up(stack_size, 4);
        dbg!(local_padding);
        stack_size += local_padding;
        dbg!(stack_size);

        let mut offset = (stack_size as isize) - 1;

        ctxt.lines.push(AssemblyInputLine::Comment(format!("# sp+0x{:x} -> {}", offset, RESULT)));
        ctxt.stack.insert(RESULT.to_owned(), LocalStorage::Stack(offset.try_into().unwrap()));
        offset -= 1;

        for arg in &self.args {
            ctxt.lines.push(AssemblyInputLine::Comment(format!("# sp+0x{:x} -> {}", offset, arg)));
            ctxt.stack.insert(arg.clone(), LocalStorage::Stack(offset as u32));
            offset -= 1;
        }

        offset -= arg_padding as isize;

        offset -= 3;
        ctxt.lines.push(AssemblyInputLine::Comment(format!("# sp+0x{:x} -> {}", offset, "RETURN_ADDRESS")));
        dbg!(&ctxt.lines);
        ctxt.stack.insert("RETURN_ADDRESS".to_owned(), LocalStorage::Stack(offset.try_into().unwrap()));
        offset -= 1;

        // offset -= register_local_count as isize;

        for (count, l) in self.locals.iter().enumerate() {
            let storage = match count {
                count if (count as u32) < register_local_count => {
                    unimplemented!();
                    // let reg = if count == 0 { Reg::D } else { Reg::E };
                    // LocalStorage::Register(reg)
                },
                _ => {
                    let s = LocalStorage::Stack(offset.try_into().unwrap());
                    offset -= 1;
                    s
                }
            };

            ctxt.lines.push(AssemblyInputLine::Comment(format!("# {:?} -> {}", storage, l)));
            ctxt.stack.insert(l.clone(), storage);
        }

        offset -= local_padding as isize;

        assert_eq!(-1, offset);

        // assert_eq!(ctxt.regs_used.len(), register_local_count);
        // if register_local_count > 0 {
        //     ctxt.lines.push(Line::Comment(format!("save regs: {:?}", ctxt.regs_used)));
        //     let regs : Vec<Reg> = ctxt.regs_used.iter().cloned().collect();
        //     for r in regs {
        //         ctxt.add_macro(format!("push {}", r));
        //     }
        // }

        let locals_with_padding = local_padding + stack_local_count;
        if locals_with_padding > 0 {
            ctxt.lines.push(AssemblyInputLine::Comment("create stack space".to_owned()));
            ctxt.add_inst(Instruction {
                opcode: Opcode::LoadImm8,
                resolved: None,
                source: format!("filler for allocated stack space"),
                args: vec![Value::Register(0), Value::Constant8(0xBB)]
            });
            for _ in 0..locals_with_padding {
                ctxt.add_inst(Instruction {
                    opcode: Opcode::Push8,
                    resolved: None,
                    source: format!("locals and padding"),
                    args: vec![Value::Register(0)]
                });
            }
        }

        // let mut count = 0;
        for stmt in self.body.iter() {
            // let scope = format!("_function{}_", count);
            stmt.emit(&mut ctxt, &self.name);
            // count += 1;
        }
         
        ctxt.lines.push(AssemblyInputLine::Label(format!(":{}__{}", &self.name, EPILOGUE)));
        if locals_with_padding > 0 {
            ctxt.add_inst(Instruction {
                opcode: Opcode::AddImm32IgnoreCarry,
                source: format!("get stack pointing to RA"),
                args: vec![Value::Register(REG_SP), Value::Constant32(locals_with_padding)],
                resolved: None,
            });
        }

        // if register_local_count > 0 {
        //     ctxt.lines.push(Line::Comment(format!("save regs: {:?}", ctxt.regs_used)));
        //     let regs : Vec<Reg> = ctxt.regs_used.iter().cloned().rev().collect();
        //     for r in regs {
        //         ctxt.add_macro(format!("pop {}", r));
        //     }
        // }

        ctxt.add_macro(format!("!return"));

        ctxt
    }
}


fn main() -> Result<(), std::io::Error> {
    let input = {
        let mut s = String::new();
        let stdin = io::stdin();
        stdin.lock().read_to_string(&mut s)?;
        s
    };
    let assembly = compile(&input);

    let rom = assemble(assembly);

    let mut c = Computer::with_print(rom, false);

    let mut last_pc = None;
    let mut running: bool = true;
    while running {
        running = c.step();
        let pc = u32::from_le_bytes(c.pc);

        if last_pc != Some(pc) {
            let sp = u32::from_le_bytes(*c.mem_word_mut(0x8000C));
            println!(
                "pc:{:05x} sp:{:08x} | mem[sp]:{:08x} mem[sp+4]:{:08x} mem[sp+8]:{:08x} mem[sp+c]:{:08x}| r0:{:08x} r4:{:08x} r8:{:08x} ir0:{:?} ", 
                u32::from_le_bytes(c.pc),
                sp,
                u32::from_le_bytes(*c.mem_word_mut(sp)),
                u32::from_le_bytes(*c.mem_word_mut(sp+4)),
                u32::from_le_bytes(*c.mem_word_mut(sp+8)),
                u32::from_le_bytes(*c.mem_word_mut(sp+0xC)),
                u32::from_le_bytes(*c.mem_word_mut(0x80000)),
                u32::from_le_bytes(*c.mem_word_mut(0x80004)),
                u32::from_le_bytes(*c.mem_word_mut(0x80008)),
                Opcode::iter().filter(|o| *o as u8 == c.ir0).next(),
            );
        }

        last_pc = Some(pc);
    }

    eprintln!("Final R0:{:02x}", u32::from_le_bytes(*c.mem_word_mut(0x80000)));

    Ok(())
}

fn compile(input: &str) -> Vec<AssemblyInputLine> {
    let mut functions = BTreeMap::new();

    let mut program = ProgramParser::parse(Rule::program, &input).unwrap();
    let pairs = program.next().unwrap().into_inner();
    for pair in pairs {
        match pair.as_rule() {
            Rule::function => {
                let f = Function::parse(pair);
                functions.insert(f.name.clone(), f);
            },
            Rule::EOI => { },
            _ => {
                panic!("Unexpected rule: {:?}", pair);
            }
        }
    }

    let main = functions.get("main");
    let main = main.expect("main not found.");

    let mut program = Vec::new();

    program.push(AssemblyInputLine::Comment(format!("set up stack and call main")));
    program.push(AssemblyInputLine::Instruction(Instruction {
        opcode: Opcode::LoadImm32,
        source: String::new(),
        args: vec![Value::Register(REG_SP), Value::Constant32(0x8FFF0)],
        resolved: None
    }));
    program.push(AssemblyInputLine::Instruction(Instruction {
        opcode: Opcode::LoadImm8,
        source: String::new(),
        args: vec![Value::Register(0), Value::Constant8(0xAA)],
        resolved: None
    }));
    for _ in 0..4 {
        program.push(AssemblyInputLine::Instruction(Instruction {
            opcode: Opcode::Push8,
            source: String::new(),
            args: vec![Value::Register(0)],
            resolved: None
        }));
    }
    program.push(AssemblyInputLine::from_str("!call :main"));
    program.push(AssemblyInputLine::from_str("loadimm32 r00 <- $00"));
    program.push(AssemblyInputLine::from_str("pop8 r00"));
    program.push(AssemblyInputLine::from_str("pop8 r00"));
    program.push(AssemblyInputLine::from_str("pop8 r00"));
    program.push(AssemblyInputLine::from_str("pop8 r00"));
    program.push(AssemblyInputLine::from_str("halt"));

    for f in &functions {
        program.push(AssemblyInputLine::Comment(format!("{:?}", &f.1)));
        let f = f.1.emit();
        for l in f.lines {
            program.push(l);
        }
    }

    program
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn halt() {
        let program = include_str!("../../programs/halt.j");
        let assembly = compile(program);
        let rom = assemble(assembly);
        let mut c = Computer::with_print(rom, false);
        while c.step() {}
    }

    #[test]
    fn add() {
        let program = include_str!("../../programs/add.j");
        let assembly = compile(program);
        let rom = assemble(assembly);
        let mut c = Computer::with_print(rom, false);
        while c.step() {}
        assert_eq!(7, u32::from_le_bytes(*c.mem_word_mut(0x80000)));
    }

    #[test]
    fn call_parameterless() {
        let program = include_str!("../../programs/call_parameterless.j");
        let assembly = compile(program);
        let rom = assemble(assembly);
        let mut c = Computer::with_print(rom, false);
        while c.step() {}
        assert_eq!(7, u32::from_le_bytes(*c.mem_word_mut(0x80000)));
    }

    #[test]
    fn idfn() {
        let program = include_str!("../../programs/idfn.j");
        let assembly = compile(program);
        let rom = assemble(assembly);
        let mut c = Computer::with_print(rom, false);
        while c.step() {}
        assert_eq!(7, u32::from_le_bytes(*c.mem_word_mut(0x80000)));
    }

    #[test]
    fn if_ne() {
        let program = include_str!("../../programs/if_ne.j");
        let assembly = compile(program);
        let rom = assemble(assembly);
        let mut c = Computer::with_print(rom, false);
        while c.step() {}
        assert_eq!(7, u32::from_le_bytes(*c.mem_word_mut(0x80000)));
    }

    #[test]
    fn plusone() {
        let program = include_str!("../../programs/plusone.j");
        let assembly = compile(program);
        let rom = assemble(assembly);
        let mut c = Computer::with_print(rom, false);
        while c.step() {}
        assert_eq!(7, u32::from_le_bytes(*c.mem_word_mut(0x80000)));
    }
}