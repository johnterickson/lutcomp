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
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ComparisonOperator {
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
            op => panic!("Unknown op: {}", op),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
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
            Expression::Arithmetic(_, left, right) |
            Expression::Comparison(_, left, right) => {
                left.visit_inner_expressions(f);
                right.visit_inner_expressions(f);
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

    pub fn try_get_const(&self) -> Option<i64> {
        match self {
            Expression::Number(_, val) => Some(*val),
            Expression::Cast{old_type:_, new_type:_, value} => value.try_get_const(),
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

    pub fn try_emit_type(&self) -> Option<Type> {
        match self {
            Expression::Number(nt, _) => Some(Type::Number(nt.clone())),
            Expression::AddressOf(inner) => {
                if let Some(inner_type) = inner.try_emit_type() {
                    Some(Type::Ptr(Box::new(inner_type)))
                } else {
                    None
                }
            },
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
            Expression::Comparison(_op, left, right) => {
                (left.optimize(ctxt) || right.optimize(ctxt), None)

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
                    *old_type = value.try_emit_type();
                }
                if let (Some(Type::Number(NumberType::U8)), Type::Number(NumberType::USIZE), Some(v)) = 
                   (&old_type, &new_type, value.try_get_const())
                {
                    (true, Some(Box::new(Expression::Number(NumberType::USIZE, v))))
                } else if let (Some(Type::Number(NumberType::U8)), 4, Some(v)) = 
                    (&old_type, &new_type.byte_count(ctxt), value.try_get_const())
                {
                    (true, Some(Box::new(Expression::Cast {
                        old_type: Some(Type::Number(NumberType::USIZE)),
                        new_type: new_type.clone(),
                        value: Box::new(Expression::Number(NumberType::USIZE, v))
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
                        let number = i64::from_str_radix(number, radix)
                            .expect(&format!("Couldn't parse integer '{}'", number));
                        if number < 256 && !is_hex {
                            Expression::Number(NumberType::U8, number)
                        } else {
                            Expression::Number(NumberType::USIZE, number)
                        }
                    }
                    Rule::char_literal => {
                        let number = number.into_inner().next().unwrap();
                        Expression::Number(NumberType::U8, number.as_str().chars().next().unwrap() as u8 as i64)
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


    pub fn emit_branch(&self, ctxt: &mut FunctionContext, when_true:&str, when_false: &str) {
        let (op, left, right) = if let Expression::Comparison(op, left, right) = self {
            (op, left, right)
        } else {
            panic!("expected comparison expression, but found: {:?}", &self);
        };

        let left_type = left.emit(ctxt);
        let right_type = right.emit(ctxt);

        let size = left_type.byte_count(ctxt.program);
        if size != right_type.byte_count(ctxt.program) {
            panic!("'{:?}' and '{:?}' are different sizes.", &left_type, &right_type);
        }

        let size: u8 = size.try_into().expect("Comparison size is too big.");
        assert!(size == 1 || size == 4);

        if left_type != right_type {
            panic!("'{:?}' and '{:?}' are different types.", &left_type, &right_type);
        }

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

        let (first_base_reg, second_base_reg) = match op {
            ComparisonOperator::Equals | ComparisonOperator::NotEquals => (right_reg, left_reg),
            ComparisonOperator::GreaterThan | ComparisonOperator::LessThanOrEqual => (left_reg, right_reg),
            ComparisonOperator::LessThan | ComparisonOperator::GreaterThanOrEqual => (right_reg, left_reg),
        };

        // compare MSB
        ctxt.add_inst(Instruction {
            opcode: Opcode::Cmp8,
            resolved: None,
            source: format!("{:?}", &self),
            args: vec![Value::Register(first_base_reg+size-1), Value::Register(second_base_reg+size-1)]
        });
        
        // compare other bytes if needed
        for r in 1..size {
            ctxt.add_inst(Instruction {
                opcode: Opcode::Cmp8IfZero,
                resolved: None,
                source: format!("{:?}", &self),
                args: vec![Value::Register(first_base_reg+size-1-r), Value::Register(second_base_reg+size-1-r)]
            });
        }

        let (cond, uncond, jmp_op) = match op {
            ComparisonOperator::Equals | ComparisonOperator::NotEquals => {
                if op == &ComparisonOperator::Equals {
                    (when_true, when_false, Opcode::JzImm)
                } else {
                    (when_false,when_true, Opcode::JzImm)
                }
            },
            ComparisonOperator::GreaterThan | ComparisonOperator::LessThanOrEqual => {
                if op == &ComparisonOperator::LessThanOrEqual {
                    (when_true,when_false,Opcode::JcImm)
                } else {
                    (when_false,when_true,Opcode::JcImm)
                }
            },
            ComparisonOperator::LessThan | ComparisonOperator::GreaterThanOrEqual => {
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
    }

    fn emit_ref(&self, ctxt: &mut FunctionContext, reference: &LogicalReference, local_name: &str) -> Type {
        let emit_result = reference.try_emit_local_address_to_reg0(ctxt, local_name);
        match emit_result {
            EmitAddressResult::AddressInReg0{ptr_type: ptr_to_stack_type} => {
                match ptr_to_stack_type {
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
                    _ => panic!(),
                }
            }
            EmitAddressResult::ValueInRegister{reg, value_type} => {
                match value_type.byte_count(ctxt.program) {
                    1 => {
                        ctxt.add_inst(Instruction {
                            source: format!("storing loaded value {:?}", &self),
                            opcode: Opcode::Push8,
                            args: vec![Value::Register(reg.0)],
                            resolved: None,
                        });
                        ctxt.additional_offset += 1;
                    }
                    4 => {
                        for i in (0..4).rev() {
                            ctxt.add_inst(Instruction {
                                source: format!("storing loaded value {:?}", &self),
                                opcode: Opcode::Push8,
                                args: vec![Value::Register(reg.0+i)],
                                resolved: None,
                            });
                            ctxt.additional_offset += 1;
                        }
                    }
                    _ => unimplemented!()
                }
                value_type
            }
        }
    }

    pub fn try_emit_address_to_reg0(&self, ctxt: &mut FunctionContext) -> EmitAddressResult {
        let t = match self {
            Expression::Ident(local_name) => {
                LogicalReference::Local.try_emit_local_address_to_reg0(ctxt, local_name)
            }
            Expression::Index(local_name, index_exp) => {
                let index_type = index_exp.emit(ctxt);
                let byte_count = index_type.byte_count(ctxt.program) as u8;

                for r in 0..byte_count {
                    ctxt.add_inst(Instruction {
                        source: format!("popping index off stack {:?}", &self),
                        opcode: Opcode::Pop8,
                        args: vec![Value::Register(r)],
                        resolved: None,
                    });
                    ctxt.additional_offset -= 1;
                }

                for r in byte_count..4 {
                    ctxt.add_inst(Instruction {
                        source: format!("zero padding index {:?}", &self),
                        opcode: Opcode::LoadImm8,
                        args: vec![Value::Register(r), Value::Constant8(0)],
                        resolved: None,
                    });
                }

                LogicalReference::ArrayIndex{
                    index_reg: Register(0),
                    multiplier: 1
                }.try_emit_local_address_to_reg0(ctxt, local_name)
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

                EmitAddressResult::AddressInReg0{ptr_type: inner_type}
            },
            Expression::LocalFieldDeref(local_name, field_name) => {
                LogicalReference::LocalField(field_name.to_owned()).try_emit_local_address_to_reg0(ctxt, local_name)
            }
            Expression::PtrFieldDeref(local_name, field_name) => {
                LogicalReference::DerefField(field_name.to_owned()).try_emit_local_address_to_reg0(ctxt, local_name)
            }
            _ => {
                panic!("Don't know how to emit address of {:?}", self);
            }
        };

        // dbg!(&t);

        t
    }

    pub fn emit(&self, ctxt: &mut FunctionContext) -> Type {
        // dbg!(&self);
        ctxt.lines.push(AssemblyInputLine::Comment(format!("Evaluating expression: {:?} additional_offset:{}", &self, ctxt.additional_offset)));
        let result = match self {
            Expression::Call(args) => {
                let t = args.emit(ctxt);
                for r in (0..t.byte_count(ctxt.program)).rev() {
                    ctxt.add_inst(Instruction {
                        source: format!("pushing result on stack {:?}", &self),
                        opcode: Opcode::Push8,
                        args: vec![Value::Register(r.try_into().unwrap())],
                        resolved: None,
                    });
                    ctxt.additional_offset += 1;
                }
                t
            }
            Expression::Optimized{original:_, optimized} => {
                optimized.emit(ctxt)
            }
            Expression::AddressOf(inner) => {
                let emit_result = inner.try_emit_address_to_reg0(ctxt);

                match emit_result {
                    EmitAddressResult::ValueInRegister{..} => 
                        panic!("Address cannot be determined because value is in register: {:?}", &self),
                    EmitAddressResult::AddressInReg0{ptr_type: ptr_to_stack_type} => {
                        for r in (0..4).rev() {
                            ctxt.add_inst(Instruction {
                                source: format!("pushing deref result {:?}", &self),
                                opcode: Opcode::Push8,
                                args: vec![Value::Register(r)],
                                resolved: None,
                            });
                            ctxt.additional_offset += 1;
                        }
                        ptr_to_stack_type
                    }
                }
            }
            Expression::Cast{old_type, new_type, value} => {
                let emitted_type = value.emit(ctxt);
                if let Some(old_type) = old_type {
                    assert_eq!(old_type, &emitted_type);
                }
                assert_eq!(emitted_type.byte_count(ctxt.program), new_type.byte_count(ctxt.program));
                new_type.clone()
            }
            Expression::Ident(name) => {
                self.emit_ref(ctxt, &LogicalReference::Local, name)
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
                    NumberType::USIZE => {
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
                let left_emit_type = left.emit(ctxt);
                let (left_math_type, left_result_type) = match left_emit_type {
                    Type::Number(nt) => (nt, None),
                    Type::Ptr(_) => (NumberType::USIZE, Some(left_emit_type)),
                    _ => panic!("can't do math on {:?}", left_emit_type),
                };

                let right_emit_type = right.emit(ctxt);
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

                let right_reg: u8 = 8;
                for i in 0..(right_math_type.byte_count(ctxt.program)) {
                    let r = (right_reg as u32 + i).try_into().unwrap();
                    ctxt.add_inst(Instruction {
                        opcode: Opcode::Pop8,
                        resolved: None,
                        source: format!("pop right {:?}", &self),
                        args: vec![Value::Register(r)]
                    });
                    ctxt.additional_offset -= 1;
                }

                for i in right_math_type.byte_count(ctxt.program)..op_math_type.byte_count(ctxt.program) {
                    let r = (right_reg as u32 + i).try_into().unwrap();
                    ctxt.add_inst(Instruction {
                        source: format!("zero ext right {:?}", &self),
                        opcode: Opcode::LoadImm8,
                        args: vec![Value::Register(r), Value::Constant8(0)],
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

                let result_reg = 0;

                match op {
                    ArithmeticOperator::And => {
                        match op_math_type {
                            NumberType::U8 => {
                                ctxt.add_inst(Instruction {
                                    opcode: Opcode::And8,
                                    resolved: None,
                                    source: format!("u8 and {:?}", &self),
                                    args: vec![Value::Register(left_reg), Value::Register(right_reg), Value::Register(result_reg)]
                                });
                            }
                            NumberType::USIZE => {
                                ctxt.add_inst(Instruction {
                                    opcode: Opcode::And32,
                                    resolved: None,
                                    source: format!("uptr and {:?}", &self),
                                    args: vec![Value::Register(left_reg), Value::Register(right_reg), Value::Register(result_reg)]
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
                                    args: vec![Value::Register(left_reg), Value::Register(right_reg), Value::Register(result_reg)]
                                });
                            }
                            NumberType::USIZE => {
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
                        match op_math_type {
                            NumberType::U8 => {
                                ctxt.add_inst(Instruction {
                                    opcode: Opcode::Or8,
                                    resolved: None,
                                    source: format!("{:?}", &self),
                                    args: vec![Value::Register(left_reg), Value::Register(right_reg), Value::Register(result_reg)]
                                });
                            }
                            NumberType::USIZE => unimplemented!(),
                        }
                    },
                    ArithmeticOperator::Multiply => {
                        match op_math_type {
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
                                    args: vec![Value::Register(left_reg), Value::Register(right_reg), Value::Register(result_reg)]
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
                                    args: vec![Value::Register(left_reg), Value::Register(right_reg), Value::Register(result_reg)]
                                });
                            }
                        }
                    },
                }

                for i in (0..op_math_type.byte_count(ctxt.program)).rev() {
                    let r = (result_reg as u32 + i).try_into().unwrap();
                    ctxt.add_inst(Instruction {
                        opcode: Opcode::Push8,
                        resolved: None,
                        source: format!("store result of expression {:?}", &self),
                        args: vec![Value::Register(r)]
                    });
                    ctxt.additional_offset += 1;
                }

                final_result_type
            },
            Expression::Comparison(_,_,_) => panic!("cannot evaluate comparison expression.")
        };
        
        ctxt.lines.push(AssemblyInputLine::Comment(format!("Evaluated expression: {:?} additional_offset:{}", &self, ctxt.additional_offset)));
        result
    }
}