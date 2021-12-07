use crate::il::IlVarId;

use super::*;
use super::parse::*;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ArithmeticOperator {
    Add,
    Subtract,
    Multiply,
    Or,
    And,
    LeftShift,
    RightShift,
}

impl ArithmeticOperator {
    fn parse(pair: pest::iterators::Pair<Rule>) -> ArithmeticOperator {
        match pair.as_str() {
            "+" => ArithmeticOperator::Add,
            "-" => ArithmeticOperator::Subtract,
            "*" => ArithmeticOperator::Multiply,
            "||" => ArithmeticOperator::Or,
            "&" => ArithmeticOperator::And,
            "<<" => ArithmeticOperator::LeftShift,
            ">>" => ArithmeticOperator::RightShift,
            op => panic!("Unknown op: {}", op),
        }
    }
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
                        ArithmeticOperator::Add => left.wrapping_add(right),
                        ArithmeticOperator::Subtract => left.wrapping_sub(right),
                        ArithmeticOperator::Multiply => left.wrapping_mul(right),
                        ArithmeticOperator::Or => left | right,
                        ArithmeticOperator::And => left & right,
                        ArithmeticOperator::LeftShift => left.wrapping_shl(right),
                        ArithmeticOperator::RightShift => left.wrapping_shr(right),
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
                if n == IlVarId::frame_pointer() {
                    Some(Type::Number(NumberType::USIZE))
                } else if let Some(f) = f {
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
                        Type::Number(nt) => {
                            match nt {
                                NumberType::U8 => panic!(),
                                NumberType::USIZE => Some(Type::Number(NumberType::U8)),
                            }
                        }
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
                let left = left.try_emit_type(ctxt, f);
                let right = right.try_emit_type(ctxt, f);
                if let (Some(left), Some(right)) = (left, right) {
                    match (left.byte_count(ctxt),right.byte_count(ctxt)) {
                        (1,1) => Some(Type::Number(NumberType::U8)),
                        (1,4) | (4,1) | (4,4) => Some(Type::Number(NumberType::USIZE)),
                        _ => None,
                    }
                } else {
                    None
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
            },
            Expression::LocalFieldDeref(n, field) => {
                if let Some(f) = f {
                    let (_, struct_type) = f.find_arg_or_var(n)
                        .expect(&format!("Cannot find {} in {:?}", n, f));
                    let struct_type = match struct_type { 
                        Type::Struct(struct_type) => {
                            &ctxt.types[struct_type]
                        }
                        _ => panic!()
                    };
                    let (_, field_type) = struct_type.get_field(field);
                    Some(field_type.clone())
                } else {
                    None
                }
            }
            Expression::PtrFieldDeref(n, field) => {
                if let Some(f) = f {
                    let (_, ptr_type) = f.find_arg_or_var(n)
                        .expect(&format!("Cannot find {} in {:?}", n, f));
                    let element_type = ptr_type.get_element_type().unwrap();
                    let struct_type = match element_type { 
                        Type::Struct(struct_type) => {
                            &ctxt.types[struct_type]
                        }
                        _ => panic!()
                    };
                    let (_, field_type) = struct_type.get_field(field);
                    Some(field_type.clone())
                } else {
                    None
                }
            }
            Expression::TtyIn() => Some(Type::Number(NumberType::U8)),
            Expression::Comparison(_) => todo!(),
            Expression::Cast { old_type:_, new_type, value:_ } => Some(new_type.clone()),
            Expression::Optimized { original:_, optimized } => optimized.try_emit_type(ctxt, f),
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

    pub fn parse_number(number: pest::iterators::Pair<Rule>) -> Expression {
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
                Expression::parse_number(number)
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
}