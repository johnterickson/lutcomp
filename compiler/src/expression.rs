use super::*;
use assemble::{AssemblyInputLine, Instruction, Value};
use super::storage::*;
use super::parse::*;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ArithmeticOperator {
    Add,
    Subtract,
    Multiply,
    Or,
    And,
}

impl ArithmeticOperator {
    fn parse(pair: pest::iterators::Pair<Rule>) -> ArithmeticOperator {
        match pair.as_str() {
            "+" => ArithmeticOperator::Add,
            "-" => ArithmeticOperator::Subtract,
            "*" => ArithmeticOperator::Multiply,
            "||" => ArithmeticOperator::Or,
            "&" => ArithmeticOperator::And,
            op => panic!("Unknown op: {}", op),
        }
    }

    // fn is_symmetric(&self) -> bool {
    //     match self {
    //         ArithmeticOperator::Add => true,
    //         ArithmeticOperator::Subtract => false,
    //         ArithmeticOperator::Multiply => true,
    //         ArithmeticOperator::Or => true,
    //         ArithmeticOperator::And => true,
    //     }
    // }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Ident(String),
    Number(NumberType, u32),
    TtyIn(),
    Arithmetic(ArithmeticOperator, Box<Expression>, Box<Expression>),
    Comparison(Box<Comparison>),
    Deref(Box<Expression>),
    LocalFieldDeref(String,String),
    PtrFieldDeref(String,String),
    AddressOf(Box<Expression>),
    Index(String, Box<Expression>),
    Cast{old_type: Option<Type>, new_type: Type, value:Box<Expression>},
    Call(Call),
    Optimized{original: Box<Expression>, optimized: Box<Expression>},
}

impl Expression {
    pub fn visit_inner_expressions<F: FnMut(&Expression) -> ()>(&self, f: &mut F) {
        f(self);
        match self {
            Expression::Call(call) => {
                for param in &call.parameters {
                    param.visit_inner_expressions(f);
                }
            }
            Expression::Arithmetic(_, left, right) => {
                left.visit_inner_expressions(f);
                right.visit_inner_expressions(f);
            }
            Expression::Comparison(c) => {
                c.left.visit_inner_expressions(f);
                c.right.visit_inner_expressions(f);
            }
            Expression::Deref(inner) |
            Expression::AddressOf(inner) |
            Expression::Index(_, inner) |
            Expression::Cast{old_type:_, new_type:_, value: inner} |
            Expression::Optimized{original:_, optimized: inner} => {
                inner.visit_inner_expressions(f);
            }
            Expression::Ident(_) |
            Expression::TtyIn() |
            Expression::LocalFieldDeref(_,_) |
            Expression::PtrFieldDeref(_,_) |
            Expression::Number(_,_) => {}
        }
    }

    pub fn try_get_const(&self) -> Option<u32> {
        match self {
            Expression::Number(_, val) => Some(*val),
            Expression::Cast{old_type: _, new_type, value} => {
                if let (Some(v), Some(new_size)) = (value.try_get_const(), new_type.try_byte_count()) {
                    Some(v & ((1 << new_size)-1))
                } else {
                    None
                }
            },
            Expression::Arithmetic(op, left, right) => {
                if let (Some(left), Some(right)) = (left.try_get_const(), right.try_get_const()) {
                    Some(match op {
                        ArithmeticOperator::Add => left + right,
                        ArithmeticOperator::Subtract => left - right,
                        ArithmeticOperator::Multiply => left * right,
                        &ArithmeticOperator::Or => left | right,
                        ArithmeticOperator::And => left & right,
                    })
                }
                else
                {
                    None
                }
            },
            _ => None
        }
    }

    pub fn try_emit_type(&self, ctxt: &ProgramContext, f: Option<&FunctionDefinition>) -> Option<Type> {
        match self {
            Expression::Ident(n) => {
                if let Some(f) = f {
                    let (_,t) = f.find_arg_or_var(n)
                        .expect(&format!("Cannot find {} in {:?}", n, f));
                    Some(t.clone())
                } else {
                    None
                }
            }
            Expression::Index(name, _) => {
                if let Some(f) = f {
                    let (_,t) = f.find_arg_or_var(name)
                        .expect(&format!("Cannot find {} in {:?}", name, f));
                    match t {
                        Type::Array(element_type, _) |
                        Type::Ptr(element_type) => Some(element_type.as_ref().clone()),
                        _ => panic!(),
                    }
                } else {
                    None
                }
            }
            Expression::Number(nt, _) => Some(Type::Number(nt.clone())),
            Expression::AddressOf(inner) => {
                if let Some(inner_type) = inner.try_emit_type(ctxt, f) {
                    Some(Type::Ptr(Box::new(inner_type)))
                } else {
                    None
                }
            },
            Expression::Call(call) => {
                if let Some(f) = ctxt.function_defs.get(&call.function) {
                    Some(f.return_type.clone())
                } else {
                    None
                }
            }
            Expression::Arithmetic(_, left, right) => {
                match (left.try_emit_type(ctxt, f), right.try_emit_type(ctxt, f)) {
                    (Some(t1), Some(t2)) => {
                        if t1 == t2 {
                            Some(t1)
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            }
            Expression::Deref(e) => {
                if let Some(ptr_type) = e.try_emit_type(ctxt, f) {
                    match ptr_type {
                        Type::Ptr(t) => Some(*t),
                        _ => todo!(),
                    }
                } else {
                    None
                }
            }
            _ => None
        }
    }

    pub fn optimize(&mut self, ctxt: &ProgramContext) -> bool {
        let (optimized, new_self) = match self {
           Expression::Arithmetic(op, left, right) => {
               let inner = left.optimize(ctxt) || right.optimize(ctxt);
               if inner {
                   (true, None)
               } else {
                   let (left_val, right_val) = (left.try_get_const(), right.try_get_const());
                   match (op, left_val, right_val) {
                       (ArithmeticOperator::Add, Some(0), _) => {
                           (true, Some((*right).clone()))
                       }
                       (ArithmeticOperator::Add, _, Some(0)) => {
                           (true, Some((*left).clone()))
                       }
                       _ => (false, None)
                   }
               }
           }
           Expression::Comparison(c) => {
               (c.left.optimize(ctxt) || c.right.optimize(ctxt), None)

           }
           Expression::Deref(inner) => {
               (inner.optimize(ctxt), None)
           }
           Expression::AddressOf(inner) => {
               (inner.optimize(ctxt), None)
           }
           Expression::Index(_, index_exp) => {
               (index_exp.optimize(ctxt), None)
           }
           Expression::Cast{old_type, new_type, value} => {
               if old_type.is_none() {
                   *old_type = value.try_emit_type(ctxt, None);
               }

               if let (Some(Type::Number(NumberType::U8)), Type::Number(NumberType::USIZE), Some(v)) = 
                      (&old_type, &new_type, value.try_get_const())
               {
                   let v: u32 = v.try_into().unwrap();
                   (true, Some(Box::new(Expression::Number(NumberType::USIZE, v.into()))))
               }
               else if let (Some(Type::Number(NumberType::U8)), 4, Some(v)) = 
                             (&old_type, &new_type.byte_count(ctxt), value.try_get_const())
               {
                   let v: u32 = v.try_into().unwrap();
                   (true, Some(Box::new(Expression::Cast {
                       old_type: Some(Type::Number(NumberType::USIZE)),
                       new_type: new_type.clone(),
                       value: Box::new(Expression::Number(NumberType::USIZE, v.into()))
                   })))
               } else {
                   (value.optimize(ctxt), None)
               }
           }
           Expression::Optimized{original:_, optimized} => {
               (optimized.optimize(ctxt), None)
           }
           _ => (false, None)
       };

       if let Some(new_self) = new_self {
           *self = Expression::Optimized{original: Box::new(self.clone()), optimized: new_self};
       }

       optimized
   }

    pub fn parse(pair: pest::iterators::Pair<Rule>) -> Expression {
        assert_eq!(Rule::expression, pair.as_rule());
        let pair = pair.into_inner().next().unwrap();
        Expression::parse_inner(pair)
    }

    pub fn parse_inner(pair: pest::iterators::Pair<Rule>) -> Expression {
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
                    Rule::decimal_number | Rule::hex_number => {
                        let is_hex = number.as_rule() == Rule::hex_number;
                        let (radix, skip) = if is_hex {(16,2)} else {(10,0)};
                        let number = number.as_str().trim();
                        let (_prefix, number)= number.split_at(skip);
                        let number = u32::from_str_radix(number, radix)
                            .expect(&format!("Couldn't parse integer '{}'", number));
                        if number < 256 && !is_hex {
                            Expression::Number(NumberType::U8, number)
                        } else {
                            Expression::Number(NumberType::USIZE, number)
                        }
                    }
                    Rule::char_literal => {
                        let number = number.into_inner().next().unwrap();
                        Expression::Number(NumberType::U8, number.as_str().chars().next().unwrap() as u8 as u32)
                    }
                    r => panic!("unexpected {:?}", &r)
                }
            },
            Rule::ident => {
                Expression::Ident(pair.as_str().trim().to_owned())
            },
            Rule::comparison_expression => {
                let mut pairs = pair.into_inner();
                let left = Expression::parse(pairs.next().unwrap());
                let op = ComparisonOperator::parse(pairs.next().unwrap());
                let right = Expression::parse(pairs.next().unwrap());
                Expression::Comparison(Box::new(Comparison { op, left, right}))
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
            Rule::call_expression => {
                Expression::Call(Call::parse(pair))
            }
            Rule::parentheses_expression => {
                let mut pairs = pair.into_inner();
                Expression::parse(pairs.next().unwrap())
            }
            Rule::RAM_MIN_expression => {
                Expression::Number(NumberType::USIZE, RAM_MIN.into())
            }
            Rule::cast_expression => {
                let mut pairs = pair.into_inner();
                let value = Box::new(Expression::parse(pairs.next().unwrap()));
                let new_type = Type::parse(pairs.next().unwrap(), false);
                Expression::Cast{ old_type: None, new_type, value }
            }
            r => {
                dbg!(r);
                dbg!(&pair);
                dbg!(pair.into_inner().as_str());
                unimplemented!();
            }
        }
    }

    fn emit_ref(&self, ctxt: &mut FunctionContext, reference: &LogicalReference, local_name: &str, target_register: Option<Register>) -> (Option<Register>,Type) {
        let emit_result = reference.try_emit_local_address(ctxt, local_name, target_register.unwrap_or(Register(0)));
        match emit_result {
            EmitAddressResult::AddressInRegister{reg: address_reg, ptr_type: ptr_to_stack_type} => {
                match ptr_to_stack_type {
                    Type::Ptr(inner) => {
                        let size = inner.byte_count(ctxt.program);
                        let actual_reg = match size {
                            1 => {
                                if let Some(target_register) = target_register {
                                    ctxt.add_inst(Instruction {
                                        source: format!("loading final value from memory {:?}", &self),
                                        opcode: Opcode::Load8,
                                        args: vec![address_reg.into(), target_register.into()],
                                        resolved: None,
                                    });
                                    Some(target_register)
                                } else {
                                    ctxt.add_inst(Instruction {
                                        source: format!("loading final value from memory {:?}", &self),
                                        opcode: Opcode::Load8,
                                        args: vec![address_reg.into(), Value::Register(4)],
                                        resolved: None,
                                    });
                                    ctxt.add_inst(Instruction {
                                        source: format!("storing loaded value {:?}", &self),
                                        opcode: Opcode::Push8,
                                        args: vec![Value::Register(4)],
                                        resolved: None,
                                    });
                                    ctxt.additional_offset += 1;
                                    None
                                }
                            }
                            4 => {
                                if let Some(target_register) = target_register {
                                    ctxt.add_inst(Instruction {
                                        source: format!("loading final value from memory {:?}", &self),
                                        opcode: Opcode::Load32,
                                        args: vec![address_reg.into(), target_register.into()],
                                        resolved: None,
                                    });
                                    Some(target_register)
                                } else {
                                    ctxt.add_inst(Instruction {
                                        source: format!("loading final value from memory {:?}", &self),
                                        opcode: Opcode::Load32,
                                        args: vec![address_reg.into(), Value::Register(4)],
                                        resolved: None,
                                    });

                                    for r in (0..4).rev() {
                                        let r = r + address_reg.0;
                                        ctxt.add_inst(Instruction {
                                            source: format!("storing loaded value {:?}", &self),
                                            opcode: Opcode::Push8,
                                            args: vec![Value::Register(4 + r)],
                                            resolved: None,
                                        });
                                        ctxt.additional_offset += 1;
                                    }
                                    None
                                }
                            }
                            _ => panic!("don't know how to handle size {}", size)
                        };
                        (actual_reg, *inner)
                    }
                    _ => panic!(),
                }
            }
            EmitAddressResult::ValueInRegister{reg, value_type} => {
                let actual_reg = match value_type.byte_count(ctxt.program) {
                    1 => {
                        if let Some(target_register) = target_register {
                            ctxt.add_inst(Instruction {
                                source: format!("loading final value from reg {:?}", &self),
                                opcode: Opcode::Or8,
                                args: vec![reg.into(), reg.into(), target_register.into()],
                                resolved: None,
                            });
                            Some(target_register)
                        } else {
                            ctxt.add_inst(Instruction {
                                source: format!("storing loaded value {:?}", &self),
                                opcode: Opcode::Push8,
                                args: vec![Value::Register(reg.0)],
                                resolved: None,
                            });
                            ctxt.additional_offset += 1;
                            None
                        }
                    }
                    4 => {
                        if let Some(target_register) = target_register {
                            ctxt.add_inst(Instruction {
                                source: format!("loading final value from reg {:?}", &self),
                                opcode: Opcode::Copy32,
                                args: vec![reg.into(), target_register.into()],
                                resolved: None,
                            });
                            Some(target_register)
                        } else {
                            for i in (0..4).rev() {
                                ctxt.add_inst(Instruction {
                                    source: format!("storing loaded value {:?}", &self),
                                    opcode: Opcode::Push8,
                                    args: vec![Value::Register(reg.0+i)],
                                    resolved: None,
                                });
                                ctxt.additional_offset += 1;
                            }
                            None
                        }
                    }
                    _ => unimplemented!()
                };
                (actual_reg, value_type)
            }
        }
    }

    pub fn try_emit_address(&self, ctxt: &mut FunctionContext, target_register: Register) -> EmitAddressResult {
        let t = match self {
            Expression::Ident(local_name) => {
                LogicalReference::Local.try_emit_local_address(ctxt, local_name, target_register)
            }
            Expression::Index(local_name, index_exp) => {
                let target_index_reg = Register(0);
                let (actual_reg, index_type) = index_exp.emit(ctxt, Some(target_index_reg));
                let byte_count = index_type.byte_count(ctxt.program) as u8;

                let actual_reg = match actual_reg {
                    Some(r) if r == target_index_reg => target_index_reg,
                    None => {
                        for r in 0..byte_count {
                            ctxt.add_inst(Instruction {
                                source: format!("popping index off stack {:?}", &self),
                                opcode: Opcode::Pop8,
                                args: vec![Value::Register(r)],
                                resolved: None,
                            });
                            ctxt.additional_offset -= 1;
                        }
                        Register(0)
                    }
                    _ => panic!(),
                };

                for r in byte_count..4 {
                    ctxt.add_inst(Instruction {
                        source: format!("zero padding index {:?}", &self),
                        opcode: Opcode::LoadImm8,
                        args: vec![Value::Register(r), Value::Constant8(0)],
                        resolved: None,
                    });
                }

                LogicalReference::ArrayIndex{
                    index_reg: actual_reg,
                    multiplier: 1
                }.try_emit_local_address(ctxt, local_name, target_register)
            },
            Expression::Deref(inner) => {
                let (actual_reg, inner_type) = inner.emit(ctxt, Some(target_register));

                match actual_reg {
                    Some(Register(0)) => {},
                    None => {
                        for r in 0..4 {
                            ctxt.add_inst(Instruction {
                                source: format!("popping address off stack {:?}", &self),
                                opcode: Opcode::Pop8,
                                args: vec![Value::Register(r)],
                                resolved: None,
                            });
                            ctxt.additional_offset -= 1;
                        }
                    }
                    _ => panic!(),
                }
                
                EmitAddressResult::AddressInRegister{reg: Register(0), ptr_type: inner_type}
            },
            Expression::LocalFieldDeref(local_name, field_name) => {
                LogicalReference::LocalField(field_name.to_owned()).try_emit_local_address(ctxt, local_name, target_register)
            }
            Expression::PtrFieldDeref(local_name, field_name) => {
                LogicalReference::DerefField(field_name.to_owned()).try_emit_local_address(ctxt, local_name, target_register)
            }
            _ => {
                panic!("Don't know how to emit address of {:?}", self);
            }
        };

        // dbg!(&t);

        t
    }

    pub fn emit(&self, ctxt: &mut FunctionContext, target_register: Option<Register>) -> (Option<Register>,Type) {
        // dbg!(&self);
        ctxt.lines.push(AssemblyInputLine::Comment(format!("Evaluating expression: {:?} additional_offset:{}", &self, ctxt.additional_offset)));
        let result = match self {
            Expression::Call(call) => {
                let t = call.emit(ctxt);
                match target_register {
                    Some(Register(0)) => {
                        (Some(Register(0)), t)
                    }
                    None => {
                        for r in (0..t.byte_count(ctxt.program)).rev() {
                            ctxt.add_inst(Instruction {
                                source: format!("pushing result on stack {:?}", &self),
                                opcode: Opcode::Push8,
                                args: vec![Value::Register(r.try_into().unwrap())],
                                resolved: None,
                            });
                            ctxt.additional_offset += 1;
                        }
                        (None, t)
                    }
                    Some(r) => {
                        match t.byte_count(ctxt.program) {
                            1 => {
                                ctxt.add_inst(Instruction {
                                    source: format!("pushing result on stack {:?}", &self),
                                    opcode: Opcode::Or8,
                                    args: vec![Value::Register(0), Value::Register(0), r.into()],
                                    resolved: None,
                                });
                            }
                            4 => {
                                ctxt.add_inst(Instruction {
                                    source: format!("copying result to register {:?}", &self),
                                    opcode: Opcode::Copy32,
                                    args: vec![Value::Register(0), r.into()],
                                    resolved: None,
                                });
                            }
                            _ => unimplemented!(),
                        }
                        (Some(r), t)
                    }
                }
            }
            Expression::Optimized{original:_, optimized} => {
                optimized.emit(ctxt, target_register)
            }
            Expression::AddressOf(inner) => {
                let address_register = target_register.unwrap_or(Register(0));
                let emit_result = inner.try_emit_address(ctxt, address_register);

                match emit_result {
                    EmitAddressResult::ValueInRegister{..} => 
                        panic!("Address cannot be determined because value is in register: {:?}", &self),
                    EmitAddressResult::AddressInRegister{reg, ptr_type: ptr_to_stack_type} => {
                        if let Some(r) = target_register {
                            (Some(r), ptr_to_stack_type)
                        } else {
                            for r in (0..4).rev() {
                                let r = r + reg.0;
                                ctxt.add_inst(Instruction {
                                    source: format!("pushing deref result {:?}", &self),
                                    opcode: Opcode::Push8,
                                    args: vec![Value::Register(r)],
                                    resolved: None,
                                });
                                ctxt.additional_offset += 1;
                            }
                            (None, ptr_to_stack_type)
                        }
                    }
                }
            }
            Expression::Cast{old_type, new_type, value} => {
                let (actual_reg, emitted_type) = value.emit(ctxt, target_register);
                if let Some(old_type) = old_type {
                    assert_eq!(old_type, &emitted_type);
                }
                assert_eq!(emitted_type.byte_count(ctxt.program), new_type.byte_count(ctxt.program));
                (actual_reg, new_type.clone())
            }
            Expression::Ident(name) => {
                self.emit_ref(ctxt, &LogicalReference::Local, name, target_register)
            },
            Expression::Index(name, index) => {
                let (local_is_ptr, entry_type) = {
                    let var = ctxt.find_var(&name);
                    match &var.var_type {
                        Type::Ptr(value_type) => {
                            (true, value_type.as_ref().clone())
                        },
                        Type::Array(entry_type, _count) => {
                            (false, entry_type.as_ref().clone())
                        }
                        t => panic!("Expected a Ptr, but found {:?}", &t)
                    }
                };

                let entry_size = entry_type.byte_count(ctxt.program); 
                let ptr_offset_expression: Box<Expression> = if entry_size == 1 {
                    (*index).clone()
                } else {
                    Box::new(Expression::Arithmetic(
                        ArithmeticOperator::Multiply,
                        Box::new(Expression::Number(NumberType::USIZE, entry_size.into())),
                        (*index).clone()))
                };

                let array_start_address = if local_is_ptr {
                    Box::new(Expression::Ident(name.clone()))
                } else {
                    Box::new(Expression::AddressOf(Box::new(Expression::Ident(name.clone()))))
                };

                let array_start_address = Box::new(Expression::Cast{
                    old_type: None, //TODO
                    new_type: Type::Number(NumberType::USIZE),
                    value: array_start_address
                });
                let ptr = Expression::Arithmetic(
                    ArithmeticOperator::Add,
                    array_start_address,
                    ptr_offset_expression
                );

                let cast = Expression::Cast{
                    old_type: Some(Type::Number(NumberType::USIZE)),
                    new_type: Type::Ptr(Box::new(entry_type)), 
                    value: Box::new(ptr)};
                let mut deref = Expression::Deref(Box::new(cast));

                deref.optimize(ctxt.program);

                deref.emit(ctxt, target_register)
            },
            Expression::LocalFieldDeref(local_name, field_name) => {
                let reference = LogicalReference::LocalField(field_name.to_owned());
                self.emit_ref(ctxt, &reference, local_name, target_register)
            }
            Expression::PtrFieldDeref(local_name, field_name) => {
                let reference = LogicalReference::DerefField(field_name.to_owned());
                self.emit_ref(ctxt, &reference, local_name, target_register)
            }
            Expression::Deref(inner) => {
                let address_reg = 4;
                let (actual_reg, ptr_type) = inner.emit(ctxt, Some(Register(address_reg))); 
                let inner_type = match ptr_type {
                    Type::Ptr(inner) => inner.as_ref().clone(),
                    other => panic!("Expected a pointer but found {:?}", other),
                };

                match actual_reg {
                    None => {
                        for r in 0..4 {
                            let r = r + address_reg;
                            ctxt.add_inst(Instruction {
                                source: format!("popping address off stack {:?}", &self),
                                opcode: Opcode::Pop8,
                                args: vec![Value::Register(r)],
                                resolved: None,
                            });
                            ctxt.additional_offset -= 1;
                        }
                    }
                    Some(r) if r.0 == address_reg => { }
                    _ => panic!()
                }
                

                if let Some(target_register) = target_register {
                    match inner_type.byte_count(ctxt.program) {
                        1 => {
                            ctxt.add_inst(Instruction {
                                source: format!("fetching deref {:?}", &self),
                                opcode: Opcode::Load8,
                                args: vec![Value::Register(4), target_register.into()],
                                resolved: None,
                            });
                        }
                        4 => {
                            ctxt.add_inst(Instruction {
                                source: format!("fetching deref {:?}", &self),
                                opcode: Opcode::Load32,
                                args: vec![Value::Register(4), target_register.into()],
                                resolved: None,
                            });
                        }
                        _ => unimplemented!(),
                    }
                } else {
                    match inner_type.byte_count(ctxt.program) {
                        1 => {
                            ctxt.add_inst(Instruction {
                                source: format!("fetching deref {:?}", &self),
                                opcode: Opcode::Load8,
                                args: vec![Value::Register(4), Value::Register(0)],
                                resolved: None,
                            });
                        }
                        4 => {
                            ctxt.add_inst(Instruction {
                                source: format!("fetching deref {:?}", &self),
                                opcode: Opcode::Load32,
                                args: vec![Value::Register(4), Value::Register(0)],
                                resolved: None,
                            });
                        }
                        _ => unimplemented!(),
                    }

                    for r in (0..inner_type.byte_count(ctxt.program)).rev() {
                        ctxt.add_inst(Instruction {
                            source: format!("pushing deref result {:?}", &self),
                            opcode: Opcode::Push8,
                            args: vec![Value::Register(r.try_into().unwrap())],
                            resolved: None,
                        });
                        ctxt.additional_offset += 1;
                    }
                }

                (target_register, inner_type)
            }
            Expression::Number(t, n) => {
                match t {
                    NumberType::U8 => {
                        let n: u8 = (*n).try_into().expect(&format!("Couldn't cast {} to U8", *n));
                        
                        if let Some(target_register) = target_register {
                            ctxt.add_inst(Instruction {
                                source: format!("{:?}", &self),
                                opcode: Opcode::LoadImm8,
                                args: vec![target_register.into(), Value::Constant8(n)],
                                resolved: None,
                            });
                        } else {
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
                    }
                    NumberType::USIZE => {
                        let n: u32 = (*n).try_into().expect(&format!("Couldn't cast {} to UPTR", *n));
                        if let Some(target_register) = target_register {
                            ctxt.add_inst(Instruction {
                                source: format!("{:?}", &self),
                                opcode: Opcode::LoadImm32,
                                args: vec![target_register.into(), Value::Constant32(n)],
                                resolved: None,
                            });
                        } else {
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
                }
                (target_register, Type::Number(*t))
            },
            Expression::TtyIn() => {
                if let Some(target_register) = target_register {
                    ctxt.add_inst(Instruction {
                        source: format!("{:?}", &self),
                        opcode: Opcode::TtyIn,
                        args: vec![target_register.into()],
                        resolved: None,
                    });
                } else {
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
                }
                (target_register, Type::Number(NumberType::U8))
            }
            Expression::Arithmetic(op, left, right) => {
                
                // left in 4..8; right in 8..C, result in 0 (&1)
                let (_, left_emit_type) = left.emit(ctxt, None);
                let (left_math_type, left_result_type) = match left_emit_type {
                    Type::Number(nt) => (nt, None),
                    Type::Ptr(_) => (NumberType::USIZE, Some(left_emit_type)),
                    _ => panic!("can't do math on {:?}", left_emit_type),
                };

                let (right_reg, right_emit_type) = right.emit(ctxt, Some(Register(8)));
                assert_eq!(right_reg, Some(Register(8)));
                let right_reg = right_reg.unwrap().0;
                let (right_math_type, right_result_type) = match right_emit_type {
                    Type::Number(nt) => (nt, None),
                    Type::Ptr(_) => (NumberType::USIZE, Some(right_emit_type)),
                    _ => panic!("can't do math on {:?}", right_emit_type),
                };

                let op_math_type = match (left_math_type, right_math_type) {
                    (NumberType::U8, NumberType::U8) => NumberType::U8,
                    (NumberType::USIZE, _) => NumberType::USIZE,
                    (_, NumberType::USIZE) => NumberType::USIZE,
                };

                let final_result_type = left_result_type
                    .or(right_result_type)
                    .or(Some(Type::Number(op_math_type)))
                    .unwrap();

                // dbg!((&left_math_type, &right_math_type, op_math_type, &final_result_type));

                // let right_reg: u8 = 8;
                // for i in 0..(right_math_type.byte_count(ctxt.program)) {
                //     let r = (right_reg as u32 + i).try_into().unwrap();
                //     ctxt.add_inst(Instruction {
                //         opcode: Opcode::Pop8,
                //         resolved: None,
                //         source: format!("pop right {:?}", &self),
                //         args: vec![Value::Register(r)]
                //     });
                //     ctxt.additional_offset -= 1;
                // }

                for i in right_math_type.byte_count(ctxt.program)..op_math_type.byte_count(ctxt.program) {
                    ctxt.add_inst(Instruction {
                        source: format!("zero ext right {:?}", &self),
                        opcode: Opcode::LoadImm8,
                        args: vec![Value::Register(right_reg + i as u8), Value::Constant8(0)],
                        resolved: None,
                    });
                }

                let left_reg: u8 = 4;
                for i in 0..(left_math_type.byte_count(ctxt.program)) {
                    let r = (left_reg as u32 + i).try_into().unwrap();
                    ctxt.add_inst(Instruction {
                        opcode: Opcode::Pop8,
                        resolved: None,
                        source: format!("pop left {:?}", &self),
                        args: vec![Value::Register(r)]
                    });
                    ctxt.additional_offset -= 1;
                }

                for i in left_math_type.byte_count(ctxt.program)..op_math_type.byte_count(ctxt.program) {
                    let r = (left_reg as u32 + i).try_into().unwrap();
                    ctxt.add_inst(Instruction {
                        source: format!("zero ext left {:?}", &self),
                        opcode: Opcode::LoadImm8,
                        args: vec![Value::Register(r), Value::Constant8(0)],
                        resolved: None,
                    });
                }

                let result_reg = target_register.unwrap_or(Register(0));

                match op {
                    ArithmeticOperator::And => {
                        match op_math_type {
                            NumberType::U8 => {
                                ctxt.add_inst(Instruction {
                                    opcode: Opcode::And8,
                                    resolved: None,
                                    source: format!("u8 and {:?}", &self),
                                    args: vec![Value::Register(left_reg), Value::Register(right_reg), result_reg.into()]
                                });
                            }
                            NumberType::USIZE => {
                                ctxt.add_inst(Instruction {
                                    opcode: Opcode::And32,
                                    resolved: None,
                                    source: format!("uptr and {:?}", &self),
                                    args: vec![Value::Register(left_reg), Value::Register(right_reg), result_reg.into()]
                                });
                            }
                        }
                    },
                    ArithmeticOperator::Add => {
                        match op_math_type {
                            NumberType::U8 => {
                                ctxt.add_inst(Instruction {
                                    opcode: Opcode::Add8NoCarry,
                                    resolved: None,
                                    source: format!("u8 add {:?}", &self),
                                    args: vec![Value::Register(left_reg), Value::Register(right_reg), result_reg.into()]
                                });
                            }
                            NumberType::USIZE => {
                                ctxt.add_inst(Instruction {
                                    opcode: Opcode::Add32NoCarryIn,
                                    resolved: None,
                                    source: format!("uptr add {:?}", &self),
                                    args: vec![Value::Register(left_reg), Value::Register(right_reg), result_reg.into()]
                                });
                            }
                        }
                    },
                    ArithmeticOperator::Or => {
                        match op_math_type {
                            NumberType::U8 => {
                                ctxt.add_inst(Instruction {
                                    opcode: Opcode::Or8,
                                    resolved: None,
                                    source: format!("{:?}", &self),
                                    args: vec![Value::Register(left_reg), Value::Register(right_reg), result_reg.into()]
                                });
                            }
                            NumberType::USIZE => {
                                ctxt.add_inst(Instruction {
                                    opcode: Opcode::Or32,
                                    resolved: None,
                                    source: format!("{:?}", &self),
                                    args: vec![Value::Register(left_reg), Value::Register(right_reg), result_reg.into()]
                                });
                            }
                        }
                    },
                    ArithmeticOperator::Multiply => {
                        match op_math_type {
                            NumberType::U8 => {
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
                                if result_reg.0 != 0 {
                                    ctxt.add_inst(Instruction {
                                        opcode: Opcode::Or8,
                                        resolved: None,
                                        source: format!("{:?}", &self),
                                        args: vec![Value::Register(0), Value::Register(0), result_reg.into()]
                                    });
                                }
                            }
                            NumberType::USIZE => unimplemented!()
                        }
                    },
                    ArithmeticOperator::Subtract => {
                        match op_math_type {
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
                                    args: vec![Value::Register(left_reg), Value::Register(right_reg), result_reg.into()]
                                });
                            }
                            NumberType::USIZE => {
                                for r in right_reg..right_reg+4 {
                                    ctxt.add_inst(Instruction {
                                        opcode: Opcode::Invert8,
                                        resolved: None,
                                        source: format!("uptr sub {:?}", &self),
                                        args: vec![Value::Register(r)]
                                    });
                                }
                                ctxt.add_inst(Instruction {
                                    opcode: Opcode::AddImm32IgnoreCarry,
                                    resolved: None,
                                    source: format!("uptr sub {:?}", &self),
                                    args: vec![Value::Register(right_reg), Value::Constant32(1)]
                                });
                                ctxt.add_inst(Instruction {
                                    opcode: Opcode::Add32NoCarryIn,
                                    resolved: None,
                                    source: format!("uptr sub {:?}", &self),
                                    args: vec![Value::Register(left_reg), Value::Register(right_reg), result_reg.into()]
                                });
                            }
                        }
                    },
                }

                if None == target_register {
                    for i in (0..op_math_type.byte_count(ctxt.program) as u8).rev() {
                        let r = (result_reg.0 + i).try_into().unwrap();
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::Push8,
                            resolved: None,
                            source: format!("store result of expression {:?}", &self),
                            args: vec![Value::Register(r)]
                        });
                        ctxt.additional_offset += 1;
                    }
                }

                (target_register, final_result_type)
            },
            Expression::Comparison(_) => panic!("cannot evaluate comparison expression.")
        };
        
        ctxt.lines.push(AssemblyInputLine::Comment(format!("Evaluated expression: {:?} additional_offset:{}", &self, ctxt.additional_offset)));
        result
    }
}