extern crate pest;
#[macro_use]
extern crate pest_derive;
use pest::Parser;

extern crate strum;
use strum::IntoEnumIterator;

use std::{io, unimplemented};
use std::io::Read;
use std::collections::{BTreeMap,BTreeSet};
use std::{convert::TryInto};

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
enum ArithmeticOperator {
    Add,
    Subtract,
    Multiply,
    Or,
}

impl ArithmeticOperator {
    fn parse(pair: pest::iterators::Pair<Rule>) -> ArithmeticOperator {
        match pair.as_str() {
            "+" => ArithmeticOperator::Add,
            "-" => ArithmeticOperator::Subtract,
            "*" => ArithmeticOperator::Multiply,
            "||" => ArithmeticOperator::Or,
            op => panic!(format!("Unknown op: {}", op)),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum ComparisonOperator {
    Equals,
    NotEquals,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
}

impl ComparisonOperator {
    fn parse(pair: pest::iterators::Pair<Rule>) -> ComparisonOperator {
        match pair.as_str() {
            "==" => ComparisonOperator::Equals,
            "!=" => ComparisonOperator::NotEquals,
            ">" => ComparisonOperator::GreaterThan,
            "@gte" => ComparisonOperator::GreaterThanOrEqual,
            "<" => ComparisonOperator::LessThan,
            "@lte" => ComparisonOperator::LessThanOrEqual,
            op => panic!(format!("Unknown op: {}", op)),
        }
    }

    // fn invert(&self) -> ComparisonOperator {
    //     match self {
    //         ComparisonOperator::Equals => ComparisonOperator::NotEquals,
    //         ComparisonOperator::NotEquals => ComparisonOperator::Equals,
    //         ComparisonOperator::GreaterThan => ComparisonOperator::LessThanOrEqual,
    //         ComparisonOperator::GreaterThanOrEqual => ComparisonOperator::LessThan,
    //         ComparisonOperator::LessThan => ComparisonOperator::GreaterThanOrEqual,
    //         ComparisonOperator::LessThanOrEqual => ComparisonOperator::GreaterThan,
    //     }
    // }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum NumberType {
    U8,
    UPTR,
}

impl NumberType {
    fn parse(s: &str) -> NumberType {
        match  s {
            "U8" => NumberType::U8,
            "UPTR" => NumberType::UPTR,
            other => panic!(format!("unknown type {}", other)),
        }
    }
}

impl ByteSize for NumberType {
    fn byte_count(&self) -> u32 {
        match self {
            NumberType::U8 => 1,
            NumberType::UPTR => 4,
        }
    }
}


#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Type {
    Number(NumberType),
}

trait ByteSize {
    fn byte_count(&self) -> u32;
}

impl ByteSize for Type {
    fn byte_count(&self) -> u32 {
        match self {
            Type::Number(nt) => nt.byte_count()
        }
    }
}
#[derive(Clone, Copy, Debug)]
struct BaseOffset(u32);

#[derive(Clone, Copy, Debug)]
enum Storage {
    Register(u8),
    Stack(BaseOffset),
}

#[derive(Clone, Copy, Debug)]
enum Declaration {
    Local,
    Arg,
    Return,
    ReturnAddress,
}
#[derive(Debug)]
struct Variable {
    var_type: Type,
    decl: Declaration,
    storage: Storage,
}

struct FunctionContext {
    pub regs_touched: BTreeSet<u8>,
    pub variables: BTreeMap<String, Variable>,
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

    fn find_local(&self, local: &str) -> &Variable {
        self.variables
            .get(local)
            .expect(&format!("could not find {}", local))
    }

    fn get_stack_offset(&self, offset: BaseOffset) -> u32 {
        offset.0 + self.additional_offset
    }
}

#[derive(Debug)]
enum Expression {
    Ident(String),
    Number(NumberType, i64),
    TtyIn(),
    Arithmetic(ArithmeticOperator, Box<Expression>, Box<Expression>),
    Comparison(ComparisonOperator, Box<Expression>, Box<Expression>),
}

impl Expression {
    fn parse(pair: pest::iterators::Pair<Rule>) -> Expression {
        assert_eq!(Rule::expression, pair.as_rule());
        let pair = pair.into_inner().next().unwrap();
        match pair.as_rule() {
            Rule::number => {
                let mut number = pair.into_inner();
                let number = number.next().unwrap();
                match number.as_rule() {
                    Rule::decimal_number => {
                        Expression::Number(
                            NumberType::U8,
                            i64::from_str_radix(number.as_str().trim(), 10)
                                .expect(&format!("Couldn't parse decimal integer '{}'", number.as_str().trim())))
                    }
                    Rule::hex_number => {
                        let s = number.as_str().trim().trim_start_matches('@');
                        Expression::Number(
                            NumberType::UPTR,
                            i64::from_str_radix(s, 16)
                                .expect(&format!("Couldn't parse hex integer {}", number.as_str())))
                    }
                    Rule::char_literal => {
                        Expression::Number(NumberType::U8, number.as_str().chars().next().unwrap() as u8 as i64)
                    }
                    r => panic!(format!("unexpected {:?}", &r))
                }
                // let mut n = 0;
                // let mut digits = pair.into_inner();
                // while let Some(digit) = digits.next() {
                //     let digit = u8::from_str(digit.as_str()).expect("Couldn't parse integer.");
                //     n *= 10;
                //     n += digit;
                // }
                // Expression::Number(n)
            },
            Rule::ident => {
                let mut label = String::new();
                let mut chars = pair.into_inner();
                while let Some(c) = chars.next() {
                    label += c.as_str();
                }
                Expression::Ident(label)
            },
            Rule::comparison_expression => {
                let mut pairs = pair.into_inner();
                let left = Expression::parse(pairs.next().unwrap());
                let op = ComparisonOperator::parse(pairs.next().unwrap());
                let right = Expression::parse(pairs.next().unwrap());
                Expression::Comparison(op, Box::new(left), Box::new(right))
            },
            Rule::arithmetic_expression => {
                let mut pairs = pair.into_inner();
                let left = Expression::parse(pairs.next().unwrap());
                let op = ArithmeticOperator::parse(pairs.next().unwrap());
                let right = Expression::parse(pairs.next().unwrap());
                Expression::Arithmetic(op, Box::new(left), Box::new(right))
            },
            Rule::ttyin => {
                Expression::TtyIn()
            },
            r => {
                dbg!(r);
                dbg!(&pair);
                dbg!(pair.into_inner().as_str());
                // dbg!(r.into_inner().as_str());
                unimplemented!();
            }
        }
    }

    fn emit_branch(&self, ctxt: &mut FunctionContext, when_true:&str, when_false: &str) {
        if let Expression::Comparison(op, left, right) = self {
            let left_type = left.emit(ctxt);
            if left_type != Type::Number(NumberType::U8) {
                unimplemented!();
            }
            let right_type = right.emit(ctxt);
            if right_type != Type::Number(NumberType::U8) {
                unimplemented!();
            }

            let right_reg = 3;
            ctxt.add_inst(Instruction {
                opcode: Opcode::Pop8,
                resolved: None,
                source: format!("{:?}", &self),
                args: vec![Value::Register(right_reg)]
            });
            ctxt.additional_offset -= 1;

            let left_reg = 2;
            ctxt.add_inst(Instruction {
                opcode: Opcode::Pop8,
                resolved: None,
                source: format!("{:?}", &self),
                args: vec![Value::Register(left_reg)]
            });
            ctxt.additional_offset -= 1;

            // left in 2; right in 3
            let (cond, uncond, jmp_op) = match op {
                ComparisonOperator::Equals | ComparisonOperator::NotEquals => {
                    ctxt.add_inst(Instruction {
                        opcode: Opcode::Cmp8,
                        resolved: None,
                        source: format!("{:?}", &self),
                        args: vec![Value::Register(right_reg), Value::Register(left_reg)]
                    });
                    if op == &ComparisonOperator::Equals {
                        (when_true, when_false, Opcode::JzImm)
                    } else {
                        (when_false,when_true, Opcode::JzImm)
                    }
                },
                ComparisonOperator::GreaterThan | ComparisonOperator::LessThanOrEqual => {
                    ctxt.add_inst(Instruction {
                        opcode: Opcode::Cmp8,
                        resolved: None,
                        source: format!("{:?}", &self),
                        args: vec![Value::Register(left_reg), Value::Register(right_reg)]
                    });
                    if op == &ComparisonOperator::LessThanOrEqual {
                        (when_true,when_false,Opcode::JcImm)
                    } else {
                        (when_false,when_true,Opcode::JcImm)
                    }
                },
                ComparisonOperator::LessThan | ComparisonOperator::GreaterThanOrEqual => {
                    ctxt.add_inst(Instruction {
                        opcode: Opcode::Cmp8,
                        resolved: None,
                        source: format!("{:?}", &self),
                        args: vec![Value::Register(right_reg), Value::Register(left_reg)]
                    });
                    if op == &ComparisonOperator::GreaterThanOrEqual {
                        (when_true,when_false,Opcode::JcImm)
                    } else {
                        (when_false,when_true,Opcode::JcImm)
                    }
                }
            };

            ctxt.add_inst(Instruction {
                opcode: jmp_op,
                source: format!("{:?}", &self),
                args: vec![Value::Label24(cond.to_owned())],
                resolved: None,
            });
            ctxt.add_inst(Instruction {
                opcode: Opcode::JmpImm,
                source: format!("{:?}", &self),
                args: vec![Value::Label24(uncond.to_owned())],
                resolved: None,
            });
        } else {
            panic!("expected comparison expression");
        }

        
    }

    fn emit(&self, ctxt: &mut FunctionContext) -> Type {
        ctxt.lines.push(AssemblyInputLine::Comment(format!("Evaluating expression: {:?} additional_offset:{}", &self, ctxt.additional_offset)));

        let result = match self {
            Expression::Number(t, n) => {
                match t {
                    NumberType::U8 => {
                        let n: u8 = (*n).try_into().expect(&format!("Couldn't cast {} to U8", *n));
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
                    }
                    NumberType::UPTR => {
                        let n: u32 = (*n).try_into().expect(&format!("Couldn't cast {} to UPTR", *n));
                        ctxt.add_inst(Instruction {
                            source: format!("{:?}", &self),
                            opcode: Opcode::LoadImm32,
                            args: vec![Value::Register(0), Value::Constant32(n)],
                            resolved: None,
                        });
                        for r in (0..4).rev() {
                            ctxt.add_inst(Instruction {
                                source: format!("{:?}", &self),
                                opcode: Opcode::Push8,
                                args: vec![Value::Register(r)],
                                resolved: None,
                            });
                            ctxt.additional_offset += 1;
                        }
                    }
                }
                Type::Number(*t)
            },
            Expression::TtyIn() => {
                ctxt.add_inst(Instruction {
                    source: format!("{:?}", &self),
                    opcode: Opcode::TtyIn,
                    args: vec![Value::Register(0)],
                    resolved: None,
                });

                ctxt.add_inst(Instruction {
                    source: format!("{:?}", &self),
                    opcode: Opcode::Push8,
                    args: vec![Value::Register(0)],
                    resolved: None,
                });
                ctxt.additional_offset += 1;
                Type::Number(NumberType::U8)
            }
            Expression::Ident(n) => {
                let local = ctxt.find_local(n);
                let local_type = local.var_type;
                match local.storage {
                    Storage::Register(_r) => {
                        // ctxt.add_inst(Instruction::LoadReg(r));
                        unimplemented!();
                    },
                    Storage::Stack(base_offset) => {
                        let offset = ctxt.get_stack_offset(base_offset);
                        ctxt.add_inst(Instruction {
                            source: format!("stack offset for {} is {:x}: {:?}", n, offset, &self),
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

                        let size = local_type.byte_count();
                        match size {
                            1 => {
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
                            4 => {
                                ctxt.add_inst(Instruction {
                                    source: format!("{:?}", &self),
                                    opcode: Opcode::Load32,
                                    args: vec![Value::Register(4), Value::Register(0)],
                                    resolved: None,
                                });
                                for r in (0..4).rev() {
                                    ctxt.add_inst(Instruction {
                                        source: format!("{:?}", &self),
                                        opcode: Opcode::Push8,
                                        args: vec![Value::Register(r)],
                                        resolved: None,
                                    });
                                    ctxt.additional_offset += 1;
                                }
                            },
                            _ => panic!(),
                        }
                    }
                };
                local_type
            },
            Expression::Arithmetic(op, left, right) => {
                
                // left in 4..8; right in 8..C, result in 0 (&1)

                let left_type = left.emit(ctxt);
                let left_type = match left_type {
                    Type::Number(nt) => nt,
                    _ => panic!()
                };

                let right_type = right.emit(ctxt);
                let right_type = match right_type {
                    Type::Number(nt) => nt,
                    _ => panic!()
                };

                let result_type = match (left_type, right_type) {
                    (NumberType::U8, NumberType::U8) => NumberType::U8,
                    (NumberType::UPTR, _) => NumberType::UPTR,
                    (_, NumberType::UPTR) => NumberType::UPTR,
                    _ => panic!(),
                };

                let right_reg: u8 = 8;
                for i in 0..(right_type.byte_count()) {
                    let r = (right_reg as u32 + i).try_into().unwrap();
                    ctxt.add_inst(Instruction {
                        opcode: Opcode::Pop8,
                        resolved: None,
                        source: format!("pop right {:?}", &self),
                        args: vec![Value::Register(r)]
                    });
                    ctxt.additional_offset -= 1;
                }

                for i in right_type.byte_count()..result_type.byte_count() {
                    let r = (right_reg as u32 + i).try_into().unwrap();
                    ctxt.add_inst(Instruction {
                        source: format!("zero ext right {:?}", &self),
                        opcode: Opcode::LoadImm8,
                        args: vec![Value::Register(r), Value::Constant8(0)],
                        resolved: None,
                    });
                }

                let left_reg: u8 = 4;
                for i in 0..(left_type.byte_count()) {
                    let r = (left_reg as u32 + i).try_into().unwrap();
                    ctxt.add_inst(Instruction {
                        opcode: Opcode::Pop8,
                        resolved: None,
                        source: format!("pop left {:?}", &self),
                        args: vec![Value::Register(r)]
                    });
                    ctxt.additional_offset -= 1;
                }

                for i in left_type.byte_count()..result_type.byte_count() {
                    let r = (left_reg as u32 + i).try_into().unwrap();
                    ctxt.add_inst(Instruction {
                        source: format!("zero ext left {:?}", &self),
                        opcode: Opcode::LoadImm8,
                        args: vec![Value::Register(r), Value::Constant8(0)],
                        resolved: None,
                    });
                }

                let result_reg = 0;

                match op {
                    ArithmeticOperator::Add => {
                        match result_type {
                            NumberType::U8 => {
                                ctxt.add_inst(Instruction {
                                    opcode: Opcode::Add8NoCarry,
                                    resolved: None,
                                    source: format!("u8 add {:?}", &self),
                                    args: vec![Value::Register(left_reg), Value::Register(right_reg), Value::Register(result_reg)]
                                });
                            }
                            NumberType::UPTR => {
                                ctxt.add_inst(Instruction {
                                    opcode: Opcode::Add32NoCarryIn,
                                    resolved: None,
                                    source: format!("uptr add {:?}", &self),
                                    args: vec![Value::Register(left_reg), Value::Register(right_reg), Value::Register(result_reg)]
                                });
                            }
                        }
                    },
                    ArithmeticOperator::Or => {
                        match result_type {
                            NumberType::U8 => {
                                ctxt.add_inst(Instruction {
                                    opcode: Opcode::Or8,
                                    resolved: None,
                                    source: format!("{:?}", &self),
                                    args: vec![Value::Register(left_reg), Value::Register(right_reg), Value::Register(result_reg)]
                                });
                            }
                            NumberType::UPTR => unimplemented!(),
                        }
                    },
                    ArithmeticOperator::Multiply => {
                        match result_type {
                            NumberType::U8 => {
                                assert_eq!(0, result_reg);
                                ctxt.add_inst(Instruction {
                                    opcode: Opcode::Mul8Part1,
                                    resolved: None,
                                    source: format!("{:?}", &self),
                                    args: vec![Value::Register(left_reg), Value::Register(right_reg)]
                                });
                                ctxt.add_inst(Instruction {
                                    opcode: Opcode::Mul8Part2,
                                    resolved: None,
                                    source: format!("{:?}", &self),
                                    args: vec![]
                                });
                            }
                            NumberType::UPTR => unimplemented!()
                        }
                    },
                    ArithmeticOperator::Subtract => {
                        match result_type {
                            NumberType::U8 => {
                                ctxt.add_inst(Instruction {
                                    opcode: Opcode::Negate8,
                                    resolved: None,
                                    source: format!("{:?}", &self),
                                    args: vec![Value::Register(right_reg)]
                                });
                                ctxt.add_inst(Instruction {
                                    opcode: Opcode::Add8NoCarry,
                                    resolved: None,
                                    source: format!("{:?}", &self),
                                    args: vec![Value::Register(left_reg), Value::Register(right_reg), Value::Register(result_reg)]
                                });
                            }
                            NumberType::UPTR => unimplemented!()
                        }
                    },
                }

                for i in (0..result_type.byte_count()).rev() {
                    let r = (result_reg as u32 + i).try_into().unwrap();
                    ctxt.add_inst(Instruction {
                        opcode: Opcode::Push8,
                        resolved: None,
                        source: format!("store result of expression {:?}", &self),
                        args: vec![Value::Register(r)]
                    });
                    ctxt.additional_offset += 1;
                }

                Type::Number(result_type)
            },
            Expression::Comparison(_,_,_) => panic!("cannot evaluate comparison expression.")
        };
        
        ctxt.lines.push(AssemblyInputLine::Comment(format!("Evaluated expression: {:?} additional_offset:{}", &self, ctxt.additional_offset)));
        result
    }
}

const RESULT : &'static str = "RESULT";
const RETURN_ADDRESS : &'static str = "RETURN_ADDRESS";
const EPILOGUE : &'static str = "EPILOGUE";

#[derive(Debug)]
enum Statement {
    Assign {local: String, var_type: NumberType, value: Expression},
    Call { local: String, var_type: NumberType, function: String, parameters: Vec<Expression> },
    IfElse {predicate: Expression, when_true: Vec<Statement>, when_false: Vec<Statement> },
    While {predicate: Expression, while_true: Vec<Statement>},
    Return { value: Expression},
    Load {local: String, address: Expression },
    Store {local: String, address: Expression },
    TtyOut {value: Expression},
}

impl Statement {
    fn parse(pair: pest::iterators::Pair<Rule>) -> Statement {
        assert_eq!(Rule::statement, pair.as_rule());
        let pair = pair.into_inner().next().unwrap();

        match pair.as_rule() {
            Rule::assign => {
                let mut pairs = pair.into_inner();
                let mut decl = pairs.next().unwrap().into_inner();
                let var_name = decl.next().unwrap().as_str().trim().to_owned();
                let var_type = NumberType::parse(decl.next().unwrap().as_str().trim());

                let value = Expression::parse(pairs.next().unwrap());
                Statement::Assign { 
                    local: var_name,
                    var_type,
                    value
                }
            },
            Rule::call => {
                let mut pairs = pair.into_inner();
                let mut decl = pairs.next().unwrap().into_inner();
                let var_name = decl.next().unwrap().as_str().trim().to_owned();
                let var_type = NumberType::parse(decl.next().unwrap().as_str().trim());

                let function = pairs.next().unwrap().as_str().to_owned();

                let mut parameters = Vec::new();
                while let Some(arg) = pairs.next() {
                    parameters.push(Expression::parse(arg));
                }

                Statement::Call { local:var_name, var_type, function, parameters }
            },
            Rule::if_else_statement => {
                let mut pairs = pair.into_inner();
                let predicate = Expression::parse(pairs.next().unwrap());
                let mut when_true = Vec::new();
                let mut when_false = Vec::new();
                while let Some(pair) = pairs.next() {
                    if Rule::else_clause == pair.as_rule() {
                        let mut else_pairs = pair.into_inner();
                        while let Some(pair) = else_pairs.next() {
                            when_false.push(Statement::parse(pair));
                        }
                    } else {
                        when_true.push(Statement::parse(pair));
                    }
                }
            
                Statement::IfElse { predicate, when_true, when_false }
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
            },
            Rule::ttyout => {
                let mut pairs = pair.into_inner();
                let value = Expression::parse(pairs.next().unwrap());
                Statement::TtyOut { value }
            },
            Rule::while_loop => {
                let mut pairs = pair.into_inner();
                let predicate = Expression::parse(pairs.next().unwrap());
                let mut while_true = Vec::new();
                while let Some(pair) = pairs.next() {
                    while_true.push(Statement::parse(pair));
                }
                Statement::While { predicate, while_true }
            },
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
            Statement::TtyOut{value} => {
                value.emit(ctxt);
                ctxt.add_inst(Instruction {
                    opcode: Opcode::Pop8,
                    resolved: None,
                    source: format!("{:?}", &self),
                    args: vec![Value::Register(0)]
                });
                ctxt.additional_offset -= 1;

                ctxt.add_inst(Instruction {
                    opcode: Opcode::TtyOut,
                    resolved: None,
                    source: format!("{:?}", &self),
                    args: vec![Value::Register(0)]
                });
            },
            Statement::Assign{local, var_type, value} => {
                let eval_type = value.emit(ctxt);
                assert_eq!(eval_type, Type::Number(*var_type));

                for r in 0..var_type.byte_count() {
                    ctxt.add_inst(Instruction {
                        opcode: Opcode::Pop8,
                        resolved: None,
                        source: format!("{:?}", &self),
                        args: vec![Value::Register(r.try_into().unwrap())]
                    });
                    ctxt.additional_offset -= 1;
                }

                let local = ctxt.find_local(local);
                match local.storage {
                    Storage::Register(_r) => {
                        unimplemented!();
                    }
                    Storage::Stack(offset) => {
                        let offset = ctxt.get_stack_offset(offset);
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::LoadImm32,
                            resolved: None,
                            source: format!("{:?}", &self),
                            args: vec![Value::Register(4), Value::Constant32(offset)]
                        });
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::Add32NoCarryIn,
                            resolved: None,
                            source: format!("{:?}", &self),
                            args: vec![Value::Register(REG_SP),Value::Register(4),Value::Register(8)]
                        });
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::Store32Part1,
                            resolved: None,
                            source: format!("{:?}", &self),
                            args: vec![Value::Register(0), Value::Register(8)]
                        });
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::Store32Part2,
                            resolved: None,
                            source: format!("{:?}", &self),
                            args: vec![]
                        });
                    }
                }
            },
            Statement::Return{ value } => {
                let expression_type = value.emit(ctxt);
                let byte_count = expression_type.byte_count();
                for i in (0..4) {
                    let r = i.try_into().unwrap();
                    if i < byte_count {
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::Pop8,
                            resolved: None,
                            source: format!("{:?}", &self),
                            args: vec![Value::Register(r)]
                        });
                        ctxt.additional_offset -= 1;
                    } else {
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::LoadImm8,
                            resolved: None,
                            source: format!("{:?}", &self),
                            args: vec![Value::Register(r), Value::Constant8(0)]
                        });
                    }
                }

                let return_var = ctxt.find_local(RESULT);
                let return_type = return_var.var_type;

                assert_eq!(return_type, expression_type);

                let result_offset = match return_var.storage {
                    Storage::Register(_) => unimplemented!(),
                    Storage::Stack(offset) => offset,
                };
                let result_offset = ctxt.get_stack_offset(result_offset);

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
                    args: vec![Value::Register(REG_SP),Value::Register(4), Value::Register(8)]
                });

                ctxt.add_inst(Instruction {
                    opcode: Opcode::Store32Part1,
                    resolved: None,
                    source: format!("{:?}", &self),
                    args: vec![Value::Register(0), Value::Register(8)]
                });
                ctxt.add_inst(Instruction {
                    opcode: Opcode::Store32Part2,
                    resolved: None,
                    source: format!("{:?}", &self),
                    args: vec![]
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
            Statement::Call{ local, var_type, function, parameters} => { 

                match var_type {
                    NumberType::U8 => {},
                    _ => unimplemented!(),
                }

                // put 0xCC in for RESULT
                ctxt.add_inst(Instruction {
                    opcode: Opcode::LoadImm8,
                    source: format!("{:?} placeholder value for RESULT", &self),
                    args: vec![Value::Register(0), Value::Constant8(0xCC)],
                    resolved: None,
                });
                for _ in 0..4 {
                    ctxt.add_inst(Instruction {
                        opcode: Opcode::Push8,
                        source: format!("{:?} placeholder value for RESULT", &self),
                        args: vec![Value::Register(0)],
                        resolved: None,
                    });
                    ctxt.additional_offset += 1;
                }

                for (i,p) in parameters.iter().enumerate() {
                    let arg_type = p.emit(ctxt);
                    match arg_type.byte_count() {
                        4 => {}
                        1 => {
                            ctxt.add_inst(Instruction {
                                opcode: Opcode::Pop8,
                                source: format!("{:?} padding for arg{}", &self, i),
                                args: vec![Value::Register(4)],
                                resolved: None,
                            });
                            ctxt.additional_offset -= 1;

                            ctxt.add_inst(Instruction {
                                opcode: Opcode::LoadImm8,
                                source: format!("{:?} padding for arg{}", &self, i),
                                args: vec![Value::Register(0), Value::Constant8(0xCC)],
                                resolved: None,
                            });
                            for b in 1..4 {
                                ctxt.add_inst(Instruction {
                                    opcode: Opcode::Push8,
                                    source: format!("{:?} padding byte {} for arg{}", &self, b, i),
                                    args: vec![Value::Register(0)],
                                    resolved: None,
                                });
                                ctxt.additional_offset += 1;
                            }

                            ctxt.add_inst(Instruction {
                                opcode: Opcode::Push8,
                                source: format!("{:?} padded byte for arg{}", &self, i),
                                args: vec![Value::Register(4)],
                                resolved: None,
                            });
                            ctxt.additional_offset += 1;
                        }
                        _ => panic!()
                    };
                }

                // store return address
                ctxt.add_inst(Instruction {
                    opcode: Opcode::AddImm32IgnoreCarry,
                    source: format!("{:?} compute return address", &self),
                    args: vec![Value::Register(REG_SP), Value::Constant32((-4i32) as u32)],
                    resolved: None,
                });
                ctxt.add_inst(Instruction {
                    opcode: Opcode::StoreImm32,
                    source: format!("{:?} store return address", &self),
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

                let parameters_bytes = parameters.len() as u32 * 4;

                // discard paramters and padding and return address
                ctxt.add_inst(Instruction {
                    opcode: Opcode::AddImm32IgnoreCarry,
                    source: format!("{:?} clean up stack", &self),
                    args: vec![Value::Register(REG_SP), Value::Constant32(parameters_bytes + 4)],
                    resolved: None,
                });
                ctxt.additional_offset -= parameters_bytes + 4;

                // for r in regs_to_save.iter().rev() {
                //     unimplemented!();
                //     // ctxt.add_macro(format!("pop {}", r));
                //     // ctxt.additional_offset -= 1;
                // }

                // result is now at the top of the stack
                // assert_eq!(ctxt.additional_offset, 1);

                for r in 0..4 {
                    ctxt.add_inst(Instruction {
                        opcode: Opcode::Pop8,
                        source: format!("{:?} pop result off stack", &self),
                        args: vec![Value::Register(r)],
                        resolved: None,
                    });
                    ctxt.additional_offset -= 1;
                }

                // stack is now back to normal
                // assert_eq!(ctxt.additional_offset, 0);

                let local = ctxt.find_local(local);
                match local.storage {
                    Storage::Register(_r) => {
                        unimplemented!();
                    },
                    Storage::Stack(offset) => {
                        let offset = ctxt.get_stack_offset(offset);
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::LoadImm32,
                            resolved: None,
                            source: format!("{:?} store result", &self),
                            args: vec![Value::Register(4), Value::Constant32(offset)]
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
            Statement::While{predicate, while_true} => {
                let source = format!("WHILE ({:?})  ... ", predicate);

                let while_start = format!(":{}_IF_WHILE_START_{}", function_name, ctxt.block_counter);
                let while_body = format!(":{}_IF_WHILE_BODY_{}", function_name, ctxt.block_counter);
                let while_end = format!(":{}_WHILE_END_{}", function_name, ctxt.block_counter);
                ctxt.block_counter += 1;

                ctxt.lines.push(AssemblyInputLine::Label(while_start.to_owned()));
                predicate.emit_branch(ctxt, &while_body, &while_end);
                ctxt.lines.push(AssemblyInputLine::Label(while_body.to_owned()));
                for s in while_true {
                    s.emit(ctxt, function_name);
                }
                ctxt.add_inst(Instruction {
                    opcode: Opcode::JmpImm,
                    source: source.to_owned(),
                    args: vec![Value::Label24(while_start.to_owned())],
                    resolved: None,
                });
                ctxt.lines.push(AssemblyInputLine::Label(while_end.to_owned()));
            },
            Statement::IfElse{predicate, when_true, when_false} => {
                let source = format!("IF ({:?})  ... ", predicate);

                let true_start = format!(":{}_IF_TRUE_START_{}", function_name, ctxt.block_counter);
                let false_start = format!(":{}_IF_FAlSE_START_{}", function_name, ctxt.block_counter);
                let block_end = format!(":{}_IF_END_{}", function_name, ctxt.block_counter);
                ctxt.block_counter += 1;

                predicate.emit_branch(ctxt, &true_start, &false_start);              

                ctxt.lines.push(AssemblyInputLine::Label(true_start.to_owned()));

                let true_start_stack_offset = ctxt.additional_offset;

                for s in when_true {
                    s.emit(ctxt, function_name);
                }

                let true_end_stack_offset = ctxt.additional_offset;

                ctxt.add_inst(Instruction {
                    opcode: Opcode::JmpImm,
                    source: source.to_owned(),
                    args: vec![Value::Label24(block_end.to_owned())],
                    resolved: None,
                });

                ctxt.lines.push(AssemblyInputLine::Label(false_start.to_owned()));
                
                ctxt.additional_offset = true_start_stack_offset;

                for s in when_false {
                    s.emit(ctxt, function_name);
                }

                let false_end_stack_offset = ctxt.additional_offset;
                assert_eq!(true_end_stack_offset, false_end_stack_offset);

                ctxt.add_inst(Instruction {
                    opcode: Opcode::JmpImm,
                    source: source.to_owned(),
                    args: vec![Value::Label24(block_end.to_owned())],
                    resolved: None,
                });

                ctxt.lines.push(AssemblyInputLine::Label(block_end.to_owned()));
            },
        }
        ctxt.lines.push(AssemblyInputLine::Comment(format!("Done  statement {:?}", self)));
    }
}

#[derive(Debug)]
struct Function {
    name: String,
    args: BTreeMap<String,NumberType>,
    locals: BTreeMap<String,NumberType>,
    return_type: NumberType,
    body: Vec<Statement>,
}

impl Function {
    fn parse(pair: pest::iterators::Pair<Rule>) -> Function {
        assert_eq!(Rule::function, pair.as_rule());

        let mut args = BTreeMap::new();

        let mut pairs = pair.into_inner();

        let name = pairs.next().unwrap().as_str().to_owned();

        for arg in pairs.next().unwrap().into_inner() {
            let mut arg_tokens = arg.into_inner();
            let arg_name = arg_tokens.next().unwrap().as_str().to_owned();
            let arg_var_type = NumberType::parse(arg_tokens.next().unwrap().as_str());
            args.insert(arg_name, arg_var_type);
        }

        let mut return_type = None;

        let return_or_body = pairs.next().unwrap();
        let body = if Rule::function_return_type == return_or_body.as_rule() {
            let mut return_type_tokens = return_or_body.into_inner();
            assert!(return_type.is_none());
            return_type = Some(NumberType::parse(return_type_tokens.next().unwrap().as_str()));
            pairs.next().unwrap()
        } else {
            return_or_body
        };

        let body : Vec<Statement> = body.into_inner().map(|p| Statement::parse(p)).collect();


        let return_type = return_type.unwrap_or(NumberType::U8);

        // find locals
        let mut locals = BTreeMap::new();

        // find declared
        fn find_decls(s: &Statement, args: &BTreeMap<String,NumberType>, locals: &mut BTreeMap<String,NumberType>) {
            match s {
                Statement::Assign{local, var_type, value:_} |
                Statement::Call{ local, var_type, function:_, parameters:_ }  => {
                    if !args.contains_key(local) {
                        if let Some(existing) = locals.insert(local.clone(), *var_type) {
                            if existing != *var_type {
                                panic!(format!("Variable '{}' is declared with different types: {:?} and {:?}", local, existing, var_type));
                            }
                        }
                    }
                }
                Statement::IfElse{ predicate:_, when_true, when_false } => {
                    for s in when_true.iter().chain(when_false.iter()) {
                        find_decls(s, args, locals);
                    }
                },
                Statement::While{predicate: _, while_true} => {
                    for s in while_true {
                        find_decls(s, args, locals);
                    }
                },
                Statement::Return{ value:_ } | 
                Statement::TtyOut{ value:_ } |
                Statement::Load{local:_, address:_} |
                Statement::Store{local:_, address:_}
                => {}
            }
        }

        for s in body.iter() {
            find_decls(s, &args, &mut locals);
        }

        // fn check_refs(s: &Statement, args: &Vec<String>, locals: &mut BTreeSet<String>) {
        //     match s {
        //         Statement::Assign{local, var_type:_, value:_} 
        //         | Statement::Load{local, address:_}
        //         | Statement::Store{local, address:_ }
        //         | Statement::Call{ local, function:_, parameters:_ } => { 
        //             if !args.contains(local) {
        //                 locals.insert(local.clone()); 
        //             }
        //         },
        //         Statement::Return{ value:_ } | Statement::TtyOut{ value:_ } => {},
        //         Statement::IfElse{ predicate:_, when_true, when_false } => {
        //             for s in when_true.iter().chain(when_false.iter()) {
        //                 find_locals(s, args, locals);
        //             }
        //         },
        //         Statement::While{predicate: _, while_true} => {
        //             for s in while_true {
        //                 find_locals(s, args, locals);
        //             }
        //         }
        //     }
        // };

        // for s in body.iter() {
        //     find_locals(s, &args, &mut locals);
        // }

        Function { name, args, locals, return_type, body }
    }

    /*

    stack:

    SP ->   local 3 (padded to 4 bytes)
            local 2 (padded to 4 bytes)
            local 1 (padded to 4 bytes)
            32-bit return address
            arg 2 (padded to 4 bytes)
            arg 1  (padded to 4 bytes)
            RESULT (padded to 4 bytes)
    */

    fn emit(&self) -> FunctionContext {
        let mut ctxt = FunctionContext {
            variables: BTreeMap::new(),
            lines: Vec::new(),
            additional_offset: 0,
            regs_touched: BTreeSet::new(),
            block_counter: 0,
        };
        ctxt.lines.push(AssemblyInputLine::Comment(format!("# Function: {}", &self.name)));
        ctxt.lines.push(AssemblyInputLine::Label(format!(":{}", &self.name)));

        let max_register_local_count = 0u32;
        let mut register_local_count = 0;
        while register_local_count < max_register_local_count {
            register_local_count += 1;
            unimplemented!();
        }

        let mut caller_stack_size: u32 = 0;
        caller_stack_size += 4;// RESULT
        caller_stack_size += self.args.len() as u32 * 4;
        caller_stack_size += 4; // return address
        let caller_stack_size = caller_stack_size;

        let callee_stack_size  = self.locals.len() as u32 * 4;

        let mut offset = (caller_stack_size + callee_stack_size) as isize;

        offset -= 4;
        ctxt.lines.push(AssemblyInputLine::Comment(format!("# sp+0x{:x} -> {}", offset, RESULT)));
        ctxt.variables.insert(RESULT.to_owned(), Variable {
            decl: Declaration::Return,
            var_type: Type::Number(self.return_type),
            storage: Storage::Stack(BaseOffset(offset.try_into().unwrap()))
        });

        for arg in &self.args {
            offset -= 4;
            ctxt.lines.push(AssemblyInputLine::Comment(format!("# sp+0x{:x} -> {:?}", offset, arg)));
            ctxt.variables.insert(arg.0.clone(), Variable {
                var_type: Type::Number(*arg.1),
                decl: Declaration::Arg,
                storage: Storage::Stack(BaseOffset(offset as u32))
            });
        }

        offset -= 4;
        ctxt.lines.push(AssemblyInputLine::Comment(format!("# sp+0x{:x} -> {}", offset, RETURN_ADDRESS)));
        // dbg!(&ctxt.lines);
        ctxt.variables.insert(RETURN_ADDRESS.to_owned(), Variable {
            decl: Declaration::ReturnAddress,
            var_type: Type::Number(NumberType::UPTR),
            storage: Storage::Stack(BaseOffset(offset.try_into().unwrap()))
        });

        for (count, l) in self.locals.iter().enumerate() {
            let storage = match count {
                count if (count as u32) < register_local_count => {
                    unimplemented!();
                    // let reg = if count == 0 { Reg::D } else { Reg::E };
                    // LocalStorage::Register(reg)
                },
                _ => {
                    offset -= 4;
                    Storage::Stack(BaseOffset(offset.try_into().unwrap()))
                }
            };

            ctxt.lines.push(AssemblyInputLine::Comment(format!("# {:?} -> {:?}", storage, l)));
            ctxt.variables.insert(l.0.clone(), Variable {
                decl: Declaration::Local,
                var_type: Type::Number(*l.1),
                storage
            });
        }


        assert_eq!(0, offset);

        // assert_eq!(ctxt.regs_used.len(), register_local_count);
        // if register_local_count > 0 {
        //     ctxt.lines.push(Line::Comment(format!("save regs: {:?}", ctxt.regs_used)));
        //     let regs : Vec<Reg> = ctxt.regs_used.iter().cloned().collect();
        //     for r in regs {
        //         ctxt.add_macro(format!("push {}", r));
        //     }
        // }

        if callee_stack_size > 0 {
            ctxt.lines.push(AssemblyInputLine::Comment("create stack space".to_owned()));
            ctxt.add_inst(Instruction {
                opcode: Opcode::LoadImm8,
                resolved: None,
                source: format!("filler for allocated stack space"),
                args: vec![Value::Register(0), Value::Constant8(0xBB)]
            });
            for _ in 0..callee_stack_size {
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
        if callee_stack_size > 0 {
            ctxt.add_inst(Instruction {
                opcode: Opcode::AddImm32IgnoreCarry,
                source: format!("get stack pointing to RA"),
                args: vec![Value::Register(REG_SP), Value::Constant32(callee_stack_size)],
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

fn optimize(assembly: &mut Vec<AssemblyInputLine>) -> usize {

    let mut optimizations_applied = 0;

    let mut instruction_indices = Vec::new();
    for (i, line) in assembly.as_mut_slice().iter_mut().enumerate() {
        if let AssemblyInputLine::Instruction(_inst) = line {
            instruction_indices.push(i);
        }
    }

    // dbg!(&instruction_indices);

    for instruction_pairs in instruction_indices.windows(2) {
        let (slice1, slice2) = assembly.as_mut_slice()[instruction_pairs[0]..].split_at_mut(1);
        let line1 = &mut slice1[0];
        let line2 = &mut slice2[instruction_pairs[1] - instruction_pairs[0]- 1];

        let mut comment_out = false;

        match (&line1, &line2) {
            (AssemblyInputLine::Instruction(i1), 
             AssemblyInputLine::Instruction(i2)) => {
                 if (i1.opcode == Opcode::Push8) && (i2.opcode == Opcode::Pop8) || (i2.opcode == Opcode::Pop8 && i1.opcode == Opcode::Push8) {
                     if let Value::Register(r1) = i1.args[0] {
                        if let Value::Register(r2) = i2.args[0] {
                            if r1 == r2 {
                                // dbg!(i1);
                                // dbg!(i2);
                                comment_out = true;
                            }
                        }
                     }
                 }
            }
            _ => {}
        }

        if comment_out {
            *line1 = AssemblyInputLine::Comment(format!("optimized away push/pop pair: {:?}", line1));
            *line2 = AssemblyInputLine::Comment(format!("optimized away push/pop pair: {:?}", line2));

            optimizations_applied += 1;
        }
    }

    optimizations_applied
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
    let assembly = compile(&input);

    let rom = assemble(assembly);

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
        let pc = u32::from_le_bytes(c.pc);

        if last_ir0 != Some(c.ir0) {
            print_state(&mut c);
        }

        last_ir0 = Some(c.ir0);
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

    let main = functions.get("main").expect("main not found.");

    let mut program = Vec::new();

    program.push(AssemblyInputLine::Comment(format!("set up stack and call main")));
    let initial_stack = 0x8FFF0;
    program.push(AssemblyInputLine::Instruction(Instruction {
        opcode: Opcode::LoadImm32,
        source: format!("init stack to 0x{:x}", initial_stack),
        args: vec![Value::Register(REG_SP), Value::Constant32(initial_stack)],
        resolved: None
    }));

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

    let main_args = main.args.len() as u8;
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
    program.push(AssemblyInputLine::from_str("!call :main"));

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

    for b in 0..4 {
        program.push(AssemblyInputLine::Instruction(Instruction {
            opcode: Opcode::Pop8,
            source: format!("pop result byte {} for main", b),
            args: vec![Value::Register(b)],
            resolved: None
        }));
    }
    program.push(AssemblyInputLine::from_str("halt"));

    for f in &functions {
        program.push(AssemblyInputLine::Comment(format!("{:?}", &f.1)));
        let f = f.1.emit();
        for l in f.lines {
            program.push(l);
        }
    }

    let mut optimizations_performed : usize;
    while 0 < {optimizations_performed = optimize(&mut program); optimizations_performed} {
        println!("performed {} optimizations", optimizations_performed);
    }

    program
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_inputs(program: &str, pairs: &[(u32,u32,u32)]) {
        let assembly = compile(program);
        let rom = assemble(assembly);
        for (input1, input2, expected) in pairs {
            dbg!((input1, input2, expected));
            let mut c = Computer::with_print(rom.clone(), false);
            *c.mem_word_mut(0x80004) = input1.to_le_bytes();
            *c.mem_word_mut(0x80000) = input2.to_le_bytes();

            let mut last_pc = None;
            let mut step_count = 0;
            while c.step() {
                if last_pc != Some(c.pc) {
                    print_state(&mut c);
                }
                last_pc = Some(c.pc);
                step_count += 1;
                assert!(step_count < 100000000);
            }
            assert_eq!(*expected, u32::from_le_bytes(*c.mem_word_mut(0x80000)));
        }
    }

    
    #[test]
    fn halt() {
        test_inputs(
            include_str!("../../programs/halt.j"),
            &[(0,0,1)]);
    }

    #[test]
    fn add_u8() {
        test_inputs(
            include_str!("../../programs/add_u8.j"),
            &[(0,0,7)]);
    }

    #[test]
    fn call_parameterless() {
        test_inputs(
            include_str!("../../programs/call_parameterless.j"),
            &[(0,0,7)]);
    }

    #[test]
    fn idfn() {
        test_inputs(
            include_str!("../../programs/idfn.j"),
            &[(0,0,7)]);
    }

    #[test]
    fn if_eq() {
        test_inputs(
            include_str!("../../programs/if_eq.j"),
            &[(6,7,0), (8,7,0), (0,7,0), (0xFF,7,0), (7,7,1)]);
    }

    #[test]
    fn if_gt_unsigned() {
        test_inputs(
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
            include_str!("../../programs/if_gte.j"),
            &[(6,7,0), (8,7,1), (0,7,0), (0x7F,7,1), (0xFF, 7, 1), (7,7,1)]);
    }

    #[test]
    fn if_lt_unsigned() {
        test_inputs(
            include_str!("../../programs/if_lt.j"),
            &[
            (0,0,0),
            (0,1,1),    
            (6,7,1), (8,7,0), (0,7,1), (7,0,0), (0x7F,7,0), (0xFF, 7, 0), (7,7,0)]);
    }

    #[test]
    fn if_lte_unsigned() {
        test_inputs(
            include_str!("../../programs/if_lte.j"),
            &[(6,7,1), (8,7,0), (0,7,1), (0x7F,7,0), (0xFF, 7, 0), (7,7,1)]);
    }

    #[test]
    fn if_ne() {
        test_inputs(
            include_str!("../../programs/if_ne.j"),
            &[(6,7,1),(8,7,1),(0,7,1),(0xFF,7,1), (7,7,0)]);
    }

   

    #[test]
    fn plusone() {
        test_inputs(
            include_str!("../../programs/plusone.j"),
            &[(0,0,7)]);

    }

    #[test]
    fn fac_rec() {
        test_inputs(
            include_str!("../../programs/fac_rec.j"),
            &[
                (0xCC,0,1),(0xCC,1,1),(0xCC, 2, 2),(0xCC,5,120)
            ]);
    }

    #[test]
    fn fac_iter() {
        test_inputs(
            include_str!("../../programs/fac_iter.j"),
            &[
                (0xCC,0,1),(0xCC,1,1),(0xCC, 2, 2),(0xCC,5,120)
            ]);
    }

    #[test]
    fn fib() {
        test_inputs(
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
            include_str!("../../programs/add_uptr.j"),
            &[
                (0x0,0x0,0x0),
                (0x0,0x1,0x1),
                (0x1,0x0,0x1),
                (0x1,0x1,0x2),
                (0x1,0xFF,0x100),
                (0xAABBCCDD, 0x0, 0xAABBCCDD),
                (0xAABBCCDD, 0x11111111, 0xBBCCDDEE)
                ]);
    }
}