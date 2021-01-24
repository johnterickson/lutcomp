extern crate pest;
#[macro_use]
extern crate pest_derive;
use pest::Parser;

extern crate strum;
use strum::IntoEnumIterator;

use std::{io, unimplemented};
use std::io::Read;
use std::collections::BTreeMap;
use std::{convert::TryInto};

use assemble::*;
use common::*;
use sim::*;

#[derive(Parser)]
#[grammar = "j.pest"]
struct ProgramParser;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
            ">=" => ComparisonOperator::GreaterThanOrEqual,
            "<" => ComparisonOperator::LessThan,
            "<=" => ComparisonOperator::LessThanOrEqual,
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
    fn byte_count(&self, _ctxt: &ProgramContext) -> u32 {
        match self {
            NumberType::U8 => 1,
            NumberType::UPTR => 4,
        }
    }
}


#[derive(Debug)]
struct StructDefinition {
    fields: Vec<(String,Type)>
}

impl StructDefinition {
    fn get_field(&self, field_name: &str) -> (u32, &Type) {
        let (index, field) = self.fields
            .iter().enumerate()
            .filter(|f| &f.1.0 == field_name)
            .next()
            .expect(&format!("could not find field {} in struct {:?}", field_name, &self));
        (4 * index as u32, &field.1)
    }
}

impl ByteSize for StructDefinition {
    fn byte_count(&self, ctxt: &ProgramContext) -> u32 {
        self.fields.iter().map(|f| f.1.byte_count(ctxt)).sum()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Type {
    Number(NumberType),
    Ptr(Box<Type>),
    Struct(String),
    Array(Box<Type>,u32),
}

impl Type {
    fn parse(pair: pest::iterators::Pair<Rule>, is_decl: bool) -> Type {
        assert_eq!(pair.as_rule(), Rule::variable_type);
        let mut tokens = pair.into_inner();
        let variable = tokens.next().unwrap();
        assert!(tokens.next().is_none());
        match variable.as_rule() {
            Rule::pointer_type => {
                let mut tokens = variable.into_inner();
                Type::Ptr(Box::new(Type::parse(tokens.next().unwrap(), is_decl)))
            }
            Rule::number_type => {
                Type::Number(NumberType::parse(variable.as_str().trim()))
            },
            Rule::ident => {
                Type::Struct(variable.as_str().to_owned())
            }
            Rule::array_type => {
                assert!(is_decl);
                let mut tokens = variable.into_inner();
                let val_type = NumberType::parse(tokens.next().unwrap().as_str().trim());
                let count: u32 = match Expression::parse_inner(tokens.next().unwrap()) {
                    Expression::Number(_, v) => {
                        v.try_into().unwrap()
                    }
                    e => panic!(format!("Expected array size but found {:?}", &e))
                };
                assert!(tokens.next().is_none());
                Type::Array(Box::new(Type::Number(val_type)), count)
            }
            _ => panic!(format!("unexpected {:?}", variable))
        }
    }
}

trait ByteSize {
    fn byte_count(&self, ctxt: &ProgramContext) -> u32;
}

impl ByteSize for Type {
    fn byte_count(&self, ctxt: &ProgramContext ) -> u32 {
        match self {
            Type::Number(nt) => nt.byte_count(ctxt),
            Type::Ptr(_) => 4,
            Type::Struct(struct_name) => ctxt.types[struct_name].byte_count(ctxt),
            Type::Array(nt, count) => nt.byte_count(ctxt) * count
        }
    }
}
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq)]
struct BaseOffset(u32);

#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq)]
enum Storage {
    // Register(u8),
    Stack(BaseOffset),
}

#[derive(Clone, Copy, Debug)]
enum Declaration {
    Local,
    Arg,
    Result,
    ReturnAddress,
}
#[derive(Debug)]
struct Variable {
    var_type: Type,
    decl: Declaration,
    storage: Storage,
}

struct FunctionContext<'a> {
    program: &'a ProgramContext,
    function: &'a Function,
    pub lines: Vec<AssemblyInputLine>,
    pub additional_offset: u32,
    pub block_counter: usize,
}

impl<'a> FunctionContext<'a> {
    fn add_inst(&mut self, i: Instruction) {
        //println!("{:?}",&i);
        self.lines.push(AssemblyInputLine::Instruction(i));
    }

    fn add_macro(&mut self, s: String) {
        self.lines.push(AssemblyInputLine::from_str(&s))
    }

    fn find_local(&self, local: &str) -> &Variable {
        self.function.variables
            .get(local)
            .expect(&format!("could not find '{}'", local))
    }

    fn get_stack_offset(&self, offset: BaseOffset) -> u32 {
        offset.0 + self.additional_offset
    }
}

#[derive(Debug, Clone)]
enum Expression {
    Ident(String),
    Number(NumberType, i64),
    TtyIn(),
    Arithmetic(ArithmeticOperator, Box<Expression>, Box<Expression>),
    Comparison(ComparisonOperator, Box<Expression>, Box<Expression>),
    Deref(Box<Expression>),
    LocalFieldDeref(String,String),
    PtrFieldDeref(String,String),
    AddressOf(Box<Expression>),
    Index(String,Box<Expression>),
    Cast{old_type: Type, new_type: Type, value:Box<Expression>},
}

impl Expression {
    fn parse(pair: pest::iterators::Pair<Rule>) -> Expression {
        assert_eq!(Rule::expression, pair.as_rule());
        let pair = pair.into_inner().next().unwrap();
        Expression::parse_inner(pair)
    }

    fn parse_inner(pair: pest::iterators::Pair<Rule>) -> Expression {
        let rule = pair.as_rule();
        match rule {
            Rule::local_field_expression | Rule::ptr_field_expression => {
                let mut pairs = pair.into_inner();
                let local_name = pairs.next().unwrap().as_str().trim().to_owned();
                let field_name = pairs.next().unwrap().as_str().trim().to_owned();
                if rule == Rule::local_field_expression {
                    Expression::LocalFieldDeref(local_name, field_name)
                } else {
                    Expression::PtrFieldDeref(local_name, field_name)
                }
            }
            Rule::deref_expression | Rule::address_of_expression => {
                let mut pairs = pair.into_inner();
                let expected_op = if rule == Rule::deref_expression {
                    Rule::deref_operator
                } else {
                    Rule::address_of_operator
                };
                assert_eq!(pairs.next().unwrap().as_rule(), expected_op);
                let inner = Expression::parse(pairs.next().unwrap());
                assert!(pairs.next().is_none());
                if rule == Rule::deref_expression {
                    Expression::Deref(Box::new(inner))
                } else {
                    Expression::AddressOf(Box::new(inner))
                }
            }
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
                        let number = number.into_inner();
                        let s = number.as_str().trim();
                        Expression::Number(
                            NumberType::UPTR,
                            i64::from_str_radix(s, 16)
                                .expect(&format!("Couldn't parse hex integer {}", number.as_str())))
                    }
                    Rule::char_literal => {
                        let number = number.into_inner().next().unwrap();
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
                Expression::Ident(pair.as_str().trim().to_owned())
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
            Rule::index_expression => {
                let mut pairs = pair.into_inner();
                let ident = pairs.next().unwrap().as_str().trim().to_owned();
                let index = Expression::parse(pairs.next().unwrap());
                Expression::Index(ident, Box::new(index))
            }
            r => {
                dbg!(r);
                dbg!(&pair);
                dbg!(pair.into_inner().as_str());
                unimplemented!();
            }
        }
    }

    fn emit_branch(&self, ctxt: &mut FunctionContext, when_true:&str, when_false: &str) {
        if let Expression::Comparison(op, left, right) = self {
            let left_type = left.emit(ctxt);
            let right_type = right.emit(ctxt);

            let size = match (left_type, right_type, op) {
                (Type::Number(left_type), Type::Number(right_type), op) => {
                    match (left_type, right_type, op) {
                        (NumberType::U8, NumberType::U8, _) => { }
                        (NumberType::UPTR, NumberType::UPTR, ComparisonOperator::Equals) |
                        (NumberType::UPTR, NumberType::UPTR, ComparisonOperator::NotEquals) => { },
                        _ => unimplemented!(),
                    }
                    left_type.byte_count(ctxt.program) as u8
                }
                _ => unimplemented!()
            };

            let right_reg = 8;
            for r in 0..size {
                ctxt.add_inst(Instruction {
                    opcode: Opcode::Pop8,
                    resolved: None,
                    source: format!("{:?}", &self),
                    args: vec![Value::Register(right_reg + r)]
                });
                ctxt.additional_offset -= 1;
            }

            let left_reg = 4;
            for r in 0..size {
                ctxt.add_inst(Instruction {
                    opcode: Opcode::Pop8,
                    resolved: None,
                    source: format!("{:?}", &self),
                    args: vec![Value::Register(left_reg + r)]
                });
                ctxt.additional_offset -= 1;
            }

            let (cond, uncond, jmp_op) = match op {
                ComparisonOperator::Equals | ComparisonOperator::NotEquals => {
                    if size == 1 {
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::Cmp8,
                            resolved: None,
                            source: format!("{:?}", &self),
                            args: vec![Value::Register(right_reg), Value::Register(left_reg)]
                        });
                    } else {
                        for r in 0..size {
                            ctxt.add_inst(Instruction {
                                opcode: Opcode::Invert8,
                                resolved: None,
                                source: format!("{:?}", &self),
                                args: vec![Value::Register(right_reg + r)]
                            });
                        }

                        ctxt.add_inst(Instruction {
                            opcode: Opcode::AddImm32IgnoreCarry,
                            resolved: None,
                            source: format!("{:?}", &self),
                            args: vec![Value::Register(right_reg), Value::Constant32(1)]
                        });

                        ctxt.add_inst(Instruction {
                            opcode: Opcode::Add32NoCarryIn,
                            resolved: None,
                            source: format!("{:?}", &self),
                            args: vec![Value::Register(left_reg), Value::Register(right_reg), Value::Register(left_reg)]
                        });

                        ctxt.add_inst(Instruction {
                            opcode: Opcode::OrImm32,
                            resolved: None,
                            source: format!("{:?}", &self),
                            args: vec![Value::Register(left_reg), Value::Constant32(0)]
                        });
                    }
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

    fn emit_ref(&self, ctxt: &mut FunctionContext, reference: &LogicalReference, local_name: &str) -> Type {
        let ptr_type = reference.emit_local_address_to_reg0(ctxt, local_name);
        match ptr_type {
            Type::Ptr(inner) => {
                let size = inner.byte_count(ctxt.program);
                match size {
                    1 => {
                        ctxt.add_inst(Instruction {
                            source: format!("loading final value from memory {:?}", &self),
                            opcode: Opcode::Load8,
                            args: vec![Value::Register(0), Value::Register(4)],
                            resolved: None,
                        });
                        ctxt.add_inst(Instruction {
                            source: format!("storing loaded value {:?}", &self),
                            opcode: Opcode::Push8,
                            args: vec![Value::Register(4)],
                            resolved: None,
                        });
                        ctxt.additional_offset += 1;
                    }
                    4 => {
                        ctxt.add_inst(Instruction {
                            source: format!("loading final value from memory {:?}", &self),
                            opcode: Opcode::Load32,
                            args: vec![Value::Register(0), Value::Register(4)],
                            resolved: None,
                        });

                        for r in (0..4).rev() {
                            ctxt.add_inst(Instruction {
                                source: format!("storing loaded value {:?}", &self),
                                opcode: Opcode::Push8,
                                args: vec![Value::Register(4 + r)],
                                resolved: None,
                            });
                            ctxt.additional_offset += 1;
                        }
                    }
                    _ => panic!("don't know how to handle size {}", size)
                }
                *inner
            }
            _ => panic!("Expected a pointer but found {:?}", ptr_type)
        }
    }

    fn emit_address_to_reg0(&self, ctxt: &mut FunctionContext) -> Type {
        let t = match self {
            Expression::Ident(local_name) => {
                LogicalReference::Local.emit_local_address_to_reg0(ctxt, local_name)
            }
            Expression::Index(local_name, index_exp) => {
                let index_type = index_exp.emit(ctxt);

                assert_eq!(index_type.byte_count(ctxt.program), 4);

                for r in 0..4 {
                    ctxt.add_inst(Instruction {
                        source: format!("popping index off stack {:?}", &self),
                        opcode: Opcode::Pop8,
                        args: vec![Value::Register(r)],
                        resolved: None,
                    });
                    ctxt.additional_offset -= 1;
                }

                LogicalReference::ArrayIndex{
                    index_reg: Register(0),
                    multiplier: 1
                }.emit_local_address_to_reg0(ctxt, local_name)
            },
            Expression::Deref(inner) => {
                let inner_type = inner.emit(ctxt);

                for r in 0..4 {
                    ctxt.add_inst(Instruction {
                        source: format!("popping address off stack {:?}", &self),
                        opcode: Opcode::Pop8,
                        args: vec![Value::Register(r)],
                        resolved: None,
                    });
                    ctxt.additional_offset -= 1;
                }

                inner_type
            },
            Expression::LocalFieldDeref(local_name, field_name) => {
                LogicalReference::LocalField(field_name.to_owned()).emit_local_address_to_reg0(ctxt, local_name)
            }
            Expression::PtrFieldDeref(local_name, field_name) => {
                LogicalReference::DerefField(field_name.to_owned()).emit_local_address_to_reg0(ctxt, local_name)
            }
            _ => {
                panic!(format!("Don't know how to emit address of {:?}", self));
            }
        };

        dbg!(&t);

        t
    }

    fn emit(&self, ctxt: &mut FunctionContext) -> Type {
        dbg!(&self);
        ctxt.lines.push(AssemblyInputLine::Comment(format!("Evaluating expression: {:?} additional_offset:{}", &self, ctxt.additional_offset)));
        let result = match self {
            Expression::AddressOf(inner) => {
                let inner_type = inner.emit_address_to_reg0(ctxt);

                for r in (0..4).rev() {
                    ctxt.add_inst(Instruction {
                        source: format!("pushing deref result {:?}", &self),
                        opcode: Opcode::Push8,
                        args: vec![Value::Register(r)],
                        resolved: None,
                    });
                    ctxt.additional_offset += 1;
                }

                inner_type
            }
            Expression::Cast{old_type, new_type, value} => {
                let emitted_type = value.emit(ctxt);
                assert_eq!(old_type, &emitted_type);
                new_type.clone()
            }
            Expression::Ident(local_name) => {
                self.emit_ref(ctxt, &LogicalReference::Local, local_name)
            },
            Expression::Index(local_name, index) => {
                let entry_type = {
                    let local = ctxt.find_local(&local_name);
                    match &local.var_type {
                        Type::Ptr(value_type) => {
                            value_type.as_ref().clone()
                        },
                        Type::Array(entry_type, _count) => {
                            entry_type.as_ref().clone()
                        }
                        t => panic!(format!("Expected a Ptr, but found {:?}", &t))
                    }
                };

                let entry_size = entry_type.byte_count(ctxt.program); 
                let ptr_offset_expression: Box<Expression> = if entry_size == 1 {
                    (*index).clone()
                } else {
                    Box::new(Expression::Arithmetic(
                        ArithmeticOperator::Multiply,
                        Box::new(Expression::Number(NumberType::UPTR, entry_size.into())),
                        (*index).clone()))
                };

                let array_start_address = Box::new(Expression::AddressOf(Box::new(Expression::Ident(local_name.clone()))));
                let ptr = Expression::Arithmetic(
                    ArithmeticOperator::Add,
                    array_start_address,
                    ptr_offset_expression    
                );

                let cast = Expression::Cast{
                    old_type: Type::Number(NumberType::UPTR),
                    new_type: Type::Ptr(Box::new(entry_type)), 
                    value: Box::new(ptr)};
                let deref = Expression::Deref(Box::new(cast));

                deref.emit(ctxt)
            },
            Expression::LocalFieldDeref(local_name, field_name) => {
                let reference = LogicalReference::LocalField(field_name.to_owned());
                self.emit_ref(ctxt, &reference, local_name)
            }
            Expression::PtrFieldDeref(local_name, field_name) => {
                let reference = LogicalReference::DerefField(field_name.to_owned());
                self.emit_ref(ctxt, &reference, local_name)
            }
            Expression::Deref(inner) => {
                let ptr_type = inner.emit(ctxt); 
                let inner_type = match ptr_type {
                    Type::Ptr(inner) => inner.as_ref().clone(),
                    other => panic!("Expected a pointer but found {:?}", other),
                };

                for r in 4..8 {
                    ctxt.add_inst(Instruction {
                        source: format!("popping address off stack {:?}", &self),
                        opcode: Opcode::Pop8,
                        args: vec![Value::Register(r)],
                        resolved: None,
                    });
                    ctxt.additional_offset -= 1;
                }

                ctxt.add_inst(Instruction {
                    source: format!("fetching deref {:?}", &self),
                    opcode: Opcode::Load32,
                    args: vec![Value::Register(4), Value::Register(0)],
                    resolved: None,
                });

                for r in (0..inner_type.byte_count(ctxt.program)).rev() {
                    ctxt.add_inst(Instruction {
                        source: format!("pushing deref result {:?}", &self),
                        opcode: Opcode::Push8,
                        args: vec![Value::Register(r.try_into().unwrap())],
                        resolved: None,
                    });
                    ctxt.additional_offset += 1;
                }

                inner_type
            }
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
            Expression::Arithmetic(op, left, right) => {
                
                // left in 4..8; right in 8..C, result in 0 (&1)

                let left_type = left.emit(ctxt);
                let left_type = match left_type {
                    Type::Number(nt) => nt,
                    Type::Ptr(_) => NumberType::UPTR,
                    _ => panic!("can't do math on {:?}", left_type),
                };

                let right_type = right.emit(ctxt);
                let right_type = match right_type {
                    Type::Number(nt) => nt,
                    Type::Ptr(_) => NumberType::UPTR,
                    _ => panic!("can't do math on {:?}", right_type),
                };

                let result_type = match (left_type, right_type) {
                    (NumberType::U8, NumberType::U8) => NumberType::U8,
                    (NumberType::UPTR, _) => NumberType::UPTR,
                    (_, NumberType::UPTR) => NumberType::UPTR,
                };

                let right_reg: u8 = 8;
                for i in 0..(right_type.byte_count(ctxt.program)) {
                    let r = (right_reg as u32 + i).try_into().unwrap();
                    ctxt.add_inst(Instruction {
                        opcode: Opcode::Pop8,
                        resolved: None,
                        source: format!("pop right {:?}", &self),
                        args: vec![Value::Register(r)]
                    });
                    ctxt.additional_offset -= 1;
                }

                for i in right_type.byte_count(ctxt.program)..result_type.byte_count(ctxt.program) {
                    let r = (right_reg as u32 + i).try_into().unwrap();
                    ctxt.add_inst(Instruction {
                        source: format!("zero ext right {:?}", &self),
                        opcode: Opcode::LoadImm8,
                        args: vec![Value::Register(r), Value::Constant8(0)],
                        resolved: None,
                    });
                }

                let left_reg: u8 = 4;
                for i in 0..(left_type.byte_count(ctxt.program)) {
                    let r = (left_reg as u32 + i).try_into().unwrap();
                    ctxt.add_inst(Instruction {
                        opcode: Opcode::Pop8,
                        resolved: None,
                        source: format!("pop left {:?}", &self),
                        args: vec![Value::Register(r)]
                    });
                    ctxt.additional_offset -= 1;
                }

                for i in left_type.byte_count(ctxt.program)..result_type.byte_count(ctxt.program) {
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

                for i in (0..result_type.byte_count(ctxt.program)).rev() {
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

#[derive(Debug, Copy, Clone)]
struct Register(u8);

#[derive(Debug)]
enum LogicalReference {
    Local,
    Deref,
    LocalField(String),
    DerefField(String),
    ArrayIndex{multiplier: u32, index_reg: Register},
}

enum DerefOffset {
    None,
    Constant(u32),
    Register(u32, Register)
}

struct MemoryReference {
    local_offset: u32,
    deref_offset: DerefOffset,
}

impl LogicalReference {
    fn get_offset_for_field<'a>(ctxt: &'a FunctionContext, struct_name: &str, field_name: &str) -> (u32, &'a Type){
        let struct_type = ctxt.program.types.get(struct_name)
            .expect(&format!("Could not find struct '{}'", struct_name));
        struct_type.get_field(field_name)
    }

    fn get_deref_offset<'a>(&self, ctxt: &'a FunctionContext, var_type: &'a Type) -> (MemoryReference, &'a Type) {
        match (self, var_type) {
            (LogicalReference::Deref, Type::Ptr(inner)) => {
                (MemoryReference{local_offset:0, deref_offset: DerefOffset::Constant(0)}, inner.as_ref())
            }
            (LogicalReference::Local, var_type) => {
                (MemoryReference{local_offset:0, deref_offset: DerefOffset::None}, var_type)
            }
            (LogicalReference::LocalField(field_name), Type::Struct(struct_name)) => {
                let (field_offset, target_type) = LogicalReference::get_offset_for_field(ctxt, struct_name, field_name);
                (MemoryReference {local_offset:field_offset, deref_offset: DerefOffset::None}, target_type)
            }
            (LogicalReference::DerefField(field_name), Type::Ptr(inner)) => {
                if let Type::Struct(struct_name) = inner.as_ref() {
                    let (field_offset, target_type) = LogicalReference::get_offset_for_field(ctxt, struct_name, field_name);
                    (MemoryReference {local_offset:0, deref_offset: DerefOffset::Constant(field_offset)}, target_type)
                } else {
                    panic!(format!("'{}' is accessed as a struct but it is '{:?}'", field_name, inner));
                }
            }
            (LogicalReference::ArrayIndex{multiplier, index_reg}, Type::Array(num_type, _)) => {
                assert_eq!(*multiplier, 1);
                assert_eq!(num_type.as_ref(), &Type::Number(NumberType::U8));
                (MemoryReference {local_offset:0, deref_offset: DerefOffset::Register(*multiplier, *index_reg)}, num_type.as_ref())
            }
            _ => panic!(format!("Don't know how to reference '{:?}' via '{:?}'", var_type, &self))
        }
    }

    fn emit_local_address_to_reg0(&self, ctxt: &mut FunctionContext, local_name: &str) -> Type {
        let local = ctxt.find_local(local_name);

        let (mem_ref, final_type) = self.get_deref_offset(ctxt, &local.var_type);
        let final_type = final_type.clone();

        dbg!(&final_type);

        match local.storage {
            Storage::Stack(base_offset) => {
                let offset = ctxt.get_stack_offset(base_offset) + mem_ref.local_offset;

                ctxt.add_inst(Instruction {
                    source: format!("stack offset for {} is {:x}: {:?}", local_name, offset, &self),
                    opcode: Opcode::LoadImm32,
                    args: vec![Value::Register(8), Value::Constant32(offset)],
                    resolved: None,
                });

                match mem_ref.deref_offset {
                    DerefOffset::Register(multiplier, index_reg) => {
                        assert_eq!(multiplier, 1);
                        assert_eq!(index_reg.0, 0);

                        ctxt.add_inst(Instruction {
                            source: format!("calculating stack address for deref {:?}", &self),
                            opcode: Opcode::Add32NoCarryIn,
                            args: vec![Value::Register(REG_SP), Value::Register(8), Value::Register(8)],
                            resolved: None,
                        });
                        ctxt.add_inst(Instruction {
                            source: format!("adding array index {:?}", &self),
                            opcode: Opcode::Add32NoCarryIn,
                            args: vec![Value::Register(8), Value::Register(index_reg.0), Value::Register(0)],
                            resolved: None,
                        });
                    }
                    DerefOffset::Constant(deref_offset) => {
                        ctxt.add_inst(Instruction {
                            source: format!("calculating stack address for deref {:?}", &self),
                            opcode: Opcode::Add32NoCarryIn,
                            args: vec![Value::Register(REG_SP), Value::Register(8), Value::Register(4)],
                            resolved: None,
                        });
                        ctxt.add_inst(Instruction {
                            source: format!("reading base address from stack {:?}", &self),
                            opcode: Opcode::Load32,
                            args: vec![Value::Register(4), Value::Register(0)],
                            resolved: None,
                        });

                        if deref_offset != 0 {
                            ctxt.add_inst(Instruction {
                                source: format!("offseting pointer for field access {:?}", &self),
                                opcode: Opcode::AddImm32IgnoreCarry,
                                args: vec![Value::Register(0), Value::Constant32(deref_offset)],
                                resolved: None,
                            });
                        }
                    }
                    DerefOffset::None => {
                        ctxt.add_inst(Instruction {
                            source: format!("calculating stack address {:?}", &self),
                            opcode: Opcode::Add32NoCarryIn,
                            args: vec![Value::Register(REG_SP), Value::Register(8), Value::Register(0)],
                            resolved: None,
                        });
                    }
                }
            }
        };
        Type::Ptr(Box::new(final_type))
    }
}



#[derive(Debug)]
enum Statement {
    Declare { local: String, var_type: Type },
    Assign { target: Expression, var_type: Option<Type>, value: Expression},
    Call { local: Option<String>, var_type: Option<Type>, function: String, parameters: Vec<Expression> },
    IfElse {predicate: Expression, when_true: Vec<Statement>, when_false: Vec<Statement> },
    While {predicate: Expression, while_true: Vec<Statement>},
    Return { value: Expression},
    TtyOut {value: Expression},
}

impl Statement {
    fn parse(pair: pest::iterators::Pair<Rule>) -> Statement {
        assert_eq!(Rule::statement, pair.as_rule());
        let pair = pair.into_inner().next().unwrap();

        match pair.as_rule() {
            Rule::declare_statement => {
                let mut pairs = pair.into_inner();
                let decl = pairs.next().unwrap();
                match decl.as_rule() {
                    Rule::variable_decl => {
                        let mut decl = decl.into_inner();
                        let var_name = decl.next().unwrap().as_str().trim().to_owned();
                        let var_type = Type::parse(decl.next().unwrap(),true);
                        Statement::Declare {local: var_name, var_type }
                    }
                    _ => panic!("Unexpected {:?}", decl)
                }
            }
            Rule::assign => {
                let mut pairs = pair.into_inner();
                let target = pairs.next().unwrap();
                assert_eq!(Rule::assign_target, target.as_rule());
                let target = target.into_inner().next().unwrap();
                let target_rule = target.as_rule();
                let (target, var_type) = match target_rule {
                    Rule::local_field_expression | Rule::ptr_field_expression => {
                        let mut tokens = target.into_inner();
                        let struct_name = tokens.next().unwrap();
                        assert_eq!(struct_name.as_rule(), Rule::ident);
                        let struct_name = struct_name.as_str().trim().to_owned();
                        let field_name = tokens.next().unwrap();
                        assert_eq!(field_name.as_rule(), Rule::ident);
                        let field_name = field_name.as_str().trim().to_owned();
                        (
                            if target_rule == Rule::local_field_expression {
                                Expression::LocalFieldDeref(struct_name, field_name)
                            } else {
                                Expression::PtrFieldDeref(struct_name, field_name)
                            },
                            None
                        )
                    },
                    Rule::assign_deref => {
                        let mut tokens = target.into_inner();
                        assert_eq!(tokens.next().unwrap().as_rule(), Rule::deref_operator);
                        let name = tokens.next().unwrap();
                        assert_eq!(name.as_rule(), Rule::ident);
                        (
                            Expression::Deref(Box::new(Expression::Ident(name.as_str().trim().to_owned()))),
                            None
                        )
                    },
                    Rule::assign_declare => {
                        let mut tokens = target.into_inner();
                        let decl = tokens.next().unwrap();
                        let (name, var_type) = match decl.as_rule() {
                            Rule::variable_decl => {
                                let mut decl_tokens = decl.into_inner();
                                let var_name = decl_tokens.next().unwrap().as_str().trim().to_owned();
                                let var_type = Type::parse(decl_tokens.next().unwrap(), false);
                                (var_name, Some(var_type))
                            }
                            Rule::ident => {
                                let var_name = decl.as_str().trim().to_owned();
                                (var_name, None)
                            }
                            _ => panic!("Unexpected {:?}", &decl)
                        };
                        ( Expression::Ident(name), var_type)
                    },
                    Rule::index_expression => {
                        let mut tokens = target.into_inner();
                        let name = tokens.next().unwrap().as_str().trim().to_owned();
                        let index_exp = Expression::parse(tokens.next().unwrap());
                        assert!(tokens.next().is_none());
                        (
                            Expression::Index(name, Box::new(index_exp)),
                            None
                        )
                    }
                    _ => panic!("Unexpected {:?}", &target)
                };

                let value = Expression::parse(pairs.next().unwrap());
                Statement::Assign { 
                    target,
                    var_type,
                    value
                }
            },
            Rule::call => {
                let mut pairs = pair.into_inner();
                let mut token = pairs.next().unwrap();
                let (var_name, var_type) = match token.as_rule() {
                    Rule::call_return_value => {
                        let mut variable = token.into_inner();
                        let var_name = variable.next().unwrap().as_str().trim().to_owned();
                        let var_type = if let Some(token) = variable.next() {
                            Some(Type::parse(token, false))
                        } else {
                            None
                        };
                    
                        token = pairs.next().unwrap();

                        (Some(var_name), var_type)
                    }
                    Rule::ident => {
                        (None, None)
                    }
                    _ => panic!(format!("Unexpected {:?}", &token))
                };

                let function = token.as_str().to_owned();

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

    fn emit(&self, ctxt: &mut FunctionContext) -> () {
        ctxt.lines.push(AssemblyInputLine::Comment(format!("Begin statement {:?}", self)));
        dbg!(&self);
        match self {
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
            Statement::Declare{local:_, var_type:_} => {}
            Statement::Assign{target, var_type, value} => {
                let value_type = value.emit(ctxt); // calculate value

                let ptr_type = target.emit_address_to_reg0(ctxt);
                match ptr_type {
                    Type::Ptr(inner) => assert_eq!(&value_type, inner.as_ref()),
                    _ => panic!("expected pointer")
                };

                let value_size = value_type.byte_count(ctxt.program);
                assert!(value_size <= 4);
                let value_size = value_size as u8;

                if let Some(explicit_type) = var_type {
                    assert_eq!(explicit_type, &value_type);
                }

                for r in 0..value_size {
                    ctxt.add_inst(Instruction {
                        opcode: Opcode::Pop8,
                        resolved: None,
                        source: format!("reading value from stack {:?}", &self),
                        args: vec![Value::Register(4 + r)]
                    });
                    ctxt.additional_offset -= 1;
                }

                match value_size {
                    1 => {
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::Store8,
                            resolved: None,
                            source: format!("{:?}", &self),
                            args: vec![Value::Register(4), Value::Register(0)]
                        });
                    }
                    4 => {
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::Store32Part1,
                            resolved: None,
                            source: format!("{:?}", &self),
                            args: vec![Value::Register(4), Value::Register(0)]
                        });
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::Store32Part2,
                            resolved: None,
                            source: format!("{:?}", &self),
                            args: vec![]
                        });
                    }
                    s => panic!(format!("Unexpected size {}", s))
                }
            },
            Statement::Return{ value } => {
                let expression_type = value.emit(ctxt);
                let byte_count = expression_type.byte_count(ctxt.program);
                for i in 0..4 {
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
                let return_type = return_var.var_type.clone();

                assert_eq!(return_type, expression_type);

                let result_offset = match return_var.storage {
                    // Storage::Register(_) => unimplemented!(),
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
                    args: vec![Value::Label24(format!(":{}__{}", &ctxt.function.name, EPILOGUE))],
                    resolved: None                    
                });
            },
            Statement::Call{ local, var_type, function, parameters} => { 

                let f = ctxt.program.functions
                    .get(function)
                    .expect(&format!("could not find function '{}'", &function));

                assert_eq!(f.args.len(), parameters.len());

                let var_type = match (var_type, &f.return_type) {
                    (Some(explicit), Some(return_type)) => {
                        assert_eq!(explicit, return_type);
                        Some(explicit)
                    }
                    (None, Some(return_type)) => {
                        Some(return_type)
                    }
                    (None, None) => {
                        None
                    }
                    (Some(_), None) => {
                        panic!(format!("Function `{}` does not return a value.", function));
                    }
                };

                match var_type {
                    None | Some(Type::Number(NumberType::U8)) => {},
                    _ => unimplemented!(),
                }

                if var_type.is_some() {
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
                }

                for (i,p) in parameters.iter().enumerate() {
                    let param_type = p.emit(ctxt);
                    assert_eq!(f.args[i].1, param_type);
                    match param_type.byte_count(ctxt.program) {
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

                if var_type.is_some() {
                    for r in 0..4 {
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::Pop8,
                            source: format!("{:?} pop result off stack", &self),
                            args: vec![Value::Register(r)],
                            resolved: None,
                        });
                        ctxt.additional_offset -= 1;
                    }
                }

                // stack is now back to normal
                // assert_eq!(ctxt.additional_offset, 0);

                if let Some(local) = local {
                    let local = ctxt.find_local(local);
                    match local.storage {
                        // Storage::Register(_r) => {
                        //     unimplemented!();
                        // },
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
                }
            },
            Statement::While{predicate, while_true} => {
                let source = format!("WHILE ({:?})  ... ", predicate);

                let while_start = format!(":{}_IF_WHILE_START_{}", &ctxt.function.name, ctxt.block_counter);
                let while_body = format!(":{}_IF_WHILE_BODY_{}", &ctxt.function.name, ctxt.block_counter);
                let while_end = format!(":{}_WHILE_END_{}", &ctxt.function.name, ctxt.block_counter);
                ctxt.block_counter += 1;

                ctxt.lines.push(AssemblyInputLine::Label(while_start.to_owned()));
                predicate.emit_branch(ctxt, &while_body, &while_end);
                ctxt.lines.push(AssemblyInputLine::Label(while_body.to_owned()));
                for s in while_true {
                    s.emit(ctxt);
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

                let true_start = format!(":{}_IF_TRUE_START_{}", &ctxt.function.name, ctxt.block_counter);
                let false_start = format!(":{}_IF_FAlSE_START_{}", &ctxt.function.name, ctxt.block_counter);
                let block_end = format!(":{}_IF_END_{}", &ctxt.function.name, ctxt.block_counter);
                ctxt.block_counter += 1;

                predicate.emit_branch(ctxt, &true_start, &false_start);              

                ctxt.lines.push(AssemblyInputLine::Label(true_start.to_owned()));

                let true_start_stack_offset = ctxt.additional_offset;

                for s in when_true {
                    s.emit(ctxt);
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
                    s.emit(ctxt);
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
    args: Vec<(String,Type)>,
    locals: BTreeMap<String,Type>,
    variables: BTreeMap<String,Variable>,
    return_type: Option<Type>,
    body: Vec<Statement>,
    caller_stack_size: u32,
    callee_stack_size: u32,
}

impl Function {
    fn parse(ctxt: &ProgramContext, pair: pest::iterators::Pair<Rule>) -> Function {
        assert_eq!(Rule::function, pair.as_rule());

        let mut args = Vec::new();

        let mut pairs = pair.into_inner();

        let name = pairs.next().unwrap().as_str().to_owned();

        for arg in pairs.next().unwrap().into_inner() {
            let mut arg_tokens = arg.into_inner();
            let arg_name = arg_tokens.next().unwrap().as_str().to_owned();
            let arg_var_type = Type::parse(arg_tokens.next().unwrap(), false);
            args.push((arg_name, arg_var_type));
        }

        let mut return_type = None;

        let return_or_body = pairs.next().unwrap();
        let body = if Rule::function_return_type == return_or_body.as_rule() {
            let mut return_type_tokens = return_or_body.into_inner();
            assert!(return_type.is_none());
            return_type = Some(Type::parse(return_type_tokens.next().unwrap(), false));
            pairs.next().unwrap()
        } else {
            return_or_body
        };

        let body : Vec<Statement> = body.into_inner().map(|p| Statement::parse(p)).collect();

        // find locals
        let mut locals = BTreeMap::new();

        fn walk_decls<F>(ctxt: &ProgramContext, s: &Statement, visitor: &mut F) 
            where F : FnMut(&String, Option<&Type>),
        {
            match s {
                Statement::Assign{target, var_type, value:_} => {
                    match target {
                        Expression::Ident(name) => {
                            visitor(name, var_type.as_ref());
                        }
                        _ => {}
                    }
                }
                Statement::Declare {local, var_type} => {
                    visitor(local, Some(var_type));
                }
                Statement::Call{ local, var_type, function, parameters:_ }  => {
                    if let Some(local) = local {
                        let var_type = match var_type {
                            Some(t) => Some(t),
                            None => ctxt.functions[function].return_type.as_ref(),
                        };
                        visitor(local, var_type);
                    }
                }
                Statement::IfElse{ predicate:_, when_true, when_false } => {
                    for s in when_true {
                        walk_decls(ctxt, s, visitor);
                    }
                    for s in when_false {
                        walk_decls(ctxt, s, visitor);
                    }
                },
                Statement::While{predicate: _, while_true} => {
                    for s in while_true {
                        walk_decls(ctxt, s, visitor);
                    }
                },
                Statement::Return{ value:_ } | 
                Statement::TtyOut{ value:_ } 
                => {}
            }
        }


        let mut find_locals  = |name: &String, var_type: Option<&Type>| {
            if let Some(var_type) = var_type {
                if !locals.contains_key(name) && !args.iter().any(|a|&a.0 == name) {
                    locals.insert(name.clone(), var_type.clone());
                }
            }
        };

        for s in &body {
            walk_decls(ctxt, s, &mut find_locals);
        }

        let mut validate_no_mismatch = |name: &String, var_type: Option<&Type>| {
            if let Some(declared_type) = var_type {
                let existing = locals.get(name)
                    .or_else(|| args.iter().filter(|(n,_)| n == name).map(|a| &a.1).next());
                match existing {
                    Some(existing) => {
                        if existing != declared_type {
                            panic!(format!("Variable '{}' is declared with different types: {:?} and {:?}", name, existing, declared_type));
                        }
                    }
                    None => {
                        assert_eq!(var_type, None);
                        panic!(format!("No type found for '{}'", name));
                    }
                }
            }
        };

        for s in &body {
            walk_decls(ctxt, s, &mut validate_no_mismatch);
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

        let mut variables = BTreeMap::new();


        let mut caller_stack_size: u32 = 0;
        if let Some(return_type) = &return_type {
            caller_stack_size += 4;// RESULT
            assert!(return_type.byte_count(ctxt) <= 4);
        }
        caller_stack_size += args.iter().map(|a| match &a.1 {
            Type::Struct(struct_name) => ctxt.types[struct_name].byte_count(ctxt),
            _ => 4,
        }).sum::<u32>();
        caller_stack_size += 4; // return address

        let callee_stack_size  = locals
            .iter()
            .map(|a| match &a.1 {
                Type::Struct(struct_name) => ctxt.types[struct_name].byte_count(ctxt),
                _ => 4,
            })
            .sum::<u32>();

        let mut offset = (caller_stack_size + callee_stack_size) as isize;

        if let Some(return_type) = &return_type {
            offset -= 4;
            variables.insert(RESULT.to_owned(), Variable {
                decl: Declaration::Result,
                var_type: return_type.clone(),
                storage: Storage::Stack(BaseOffset(offset.try_into().unwrap()))
            });
        }

        for arg in &args {
            offset -= 4;
            variables.insert(arg.0.clone(), Variable {
                var_type: arg.1.clone(),
                decl: Declaration::Arg,
                storage: Storage::Stack(BaseOffset(offset as u32))
            });
        }

        offset -= 4;
        variables.insert(RETURN_ADDRESS.to_owned(), Variable {
            decl: Declaration::ReturnAddress,
            var_type: Type::Number(NumberType::UPTR),
            storage: Storage::Stack(BaseOffset(offset.try_into().unwrap()))
        });

        for (count, l) in locals.iter().enumerate() {
            let storage = match count {
                _ => {
                    let size = l.1.byte_count(ctxt);
                    offset -= std::cmp::max(size,4) as isize;
                    Storage::Stack(BaseOffset(offset.try_into().unwrap()))
                }
            };

            variables.insert(l.0.clone(), Variable {
                decl: Declaration::Local,
                var_type: l.1.clone(),
                storage
            });
        }

        assert_eq!(0, offset);

        Function { name, args, locals, variables, return_type, body, caller_stack_size, callee_stack_size}
    }



    fn emit<'a>(&'a self, program: &'a ProgramContext) -> FunctionContext<'a> {
        let mut ctxt = FunctionContext {
            program,
            function: &self,
            lines: Vec::new(),
            additional_offset: 0,
            block_counter: 0,
        };
        ctxt.lines.push(AssemblyInputLine::Comment(format!("# Function: {}", &self.name)));
        ctxt.lines.push(AssemblyInputLine::Label(format!(":{}", &self.name)));

        // let max_register_local_count = 0u32;
        // let mut register_local_count = 0;
        // while register_local_count < max_register_local_count {
        //     register_local_count += 1;
        //     unimplemented!();
        // }

        let mut stack_objects: Vec<_> = self.variables.iter().collect();
        stack_objects.sort_by_key(|k| k.1.storage);

        for (name, var) in stack_objects {
            match var.storage {
                Storage::Stack(offset) => {
                    ctxt.lines.push(AssemblyInputLine::Comment(format!("# sp+0x{:02x} -> {} {:?} {:?}", offset.0, name, var.decl, var.var_type)));
                }
            }
        }

        if self.callee_stack_size > 0 {
            ctxt.lines.push(AssemblyInputLine::Comment("create stack space".to_owned()));
            ctxt.add_inst(Instruction {
                opcode: Opcode::LoadImm8,
                resolved: None,
                source: format!("filler for allocated stack space"),
                args: vec![Value::Register(0), Value::Constant8(0xBB)]
            });
            for _ in 0..self.callee_stack_size {
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
            stmt.emit(&mut ctxt);
            // count += 1;
        }
         
        ctxt.lines.push(AssemblyInputLine::Label(format!(":{}__{}", &self.name, EPILOGUE)));
        if self.callee_stack_size > 0 {
            ctxt.add_inst(Instruction {
                opcode: Opcode::AddImm32IgnoreCarry,
                source: format!("get stack pointing to RA"),
                args: vec![Value::Register(REG_SP), Value::Constant32(self.callee_stack_size)],
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
        // let pc = u32::from_le_bytes(c.pc);

        if last_ir0 != Some(c.ir0) {
            print_state(&mut c);
        }

        last_ir0 = Some(c.ir0);
    }

    eprintln!("Final R0:{:02x}", u32::from_le_bytes(*c.mem_word_mut(0x80000)));

    Ok(())
}

struct ProgramContext {
    functions: BTreeMap<String, Function>,
    types: BTreeMap<String, StructDefinition>
}

fn emit(ctxt: ProgramContext) -> Vec<AssemblyInputLine> {
    let main = ctxt.functions.get("main").expect("main not found.");

    let mut program = Vec::new();

    program.push(AssemblyInputLine::Comment("Types:".to_owned()));
    for (name, t) in &ctxt.types {
        program.push(AssemblyInputLine::Comment(format!("{} {:?}", name, t)));
    }

    program.push(AssemblyInputLine::Comment(format!("set up stack and call main")));
    let initial_stack = 0x8FFF0;
    program.push(AssemblyInputLine::Instruction(Instruction {
        opcode: Opcode::LoadImm32,
        source: format!("init stack to 0x{:x}", initial_stack),
        args: vec![Value::Register(REG_SP), Value::Constant32(initial_stack)],
        resolved: None
    }));

    if let Some(return_type) = &main.return_type {
        assert!(return_type.byte_count(&ctxt) <= 4);
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

    if main.return_type.is_some() {
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

    for f in &ctxt.functions {
        program.push(AssemblyInputLine::Comment(format!("{:?}", &f.1)));
        let f = f.1.emit(&ctxt);
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

fn compile(input: &str) -> Vec<AssemblyInputLine> {
    let mut ctxt = ProgramContext { functions: BTreeMap::new(), types: BTreeMap::new() };

    let mut program = ProgramParser::parse(Rule::program, &input).unwrap();
    let pairs = program.next().unwrap().into_inner();
    for pair in pairs {
        match pair.as_rule() {
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
                let f = Function::parse(&ctxt, pair);
                ctxt.functions.insert(f.name.clone(), f);
            },
            Rule::EOI => {},
            _ => panic!("Unexpected rule: {:?}", pair)
        }
    }

    emit(ctxt)
}

#[cfg(test)]
mod tests {
    use super::*;

    
    struct TestComputer<'a>(pub Computer<'a>);

    impl<'a> TestComputer<'a> {
        fn from_rom(rom: &[u8]) -> TestComputer<'a> {
            TestComputer(Computer::with_print(rom.to_vec(), false))
        }

        fn run(&mut self, in1: u32, in2: u32, out: u32) {
            *self.0.mem_word_mut(0x80004) = in1.to_le_bytes();
            *self.0.mem_word_mut(0x80000) = in2.to_le_bytes();

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

    fn test_inputs(program: &str, pairs: &[(u32,u32,u32)]) {
        let assembly = compile(program);
        let rom = assemble(assembly);
        for (input1, input2, expected) in pairs {
            let mut c = TestComputer::from_rom(&rom);
            dbg!((input1, input2, expected));
            c.run(*input1, *input2, *expected);
        }
    }

    fn test_ttyout(program: &str, pairs: &[(u32,u32,&str)]) {
        let assembly = compile(program);
        let rom = assemble(assembly);
        for (input1, input2, expected) in pairs {
            let mut c = TestComputer::from_rom(&rom);
            dbg!((input1, input2, expected));
            c.run(*input1, *input2, 0);

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

    fn test_ptr_inputs(program: &str, pairs: &[(&[u8],&[u8],u32)]) {
        let assembly = compile(program);
        let rom = assemble(assembly);
        let addr1 = 0x8100;
        let addr2 = 0x8200;

        for (input1, input2, expected) in pairs {
            let mut c = TestComputer::from_rom(&rom);
            dbg!((input1, input2, expected));
            for (i,b) in input1.iter().enumerate() {
                *c.0.mem_byte_mut(addr1 + i as u32) = *b;
            }
            for (i,b) in input2.iter().enumerate() {
                *c.0.mem_byte_mut(addr2 + i as u32) = *b;
            }
            c.run(addr1, addr2, *expected);
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
                (0xAABBCCDD, 0x11111111, 0xBBCCDDEE),
                (0xFFFFFFFF, 0x1, 0x0),
                ]);
    }

    #[test]
    fn ptr() {
        test_ptr_inputs(
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
    fn array() {
        test_inputs(
            include_str!("../../programs/local_array.j"),
            &[
                (0x0,0x0,0x0),
                ]);
    }
}