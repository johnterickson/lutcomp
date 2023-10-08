use std::borrow::Cow;

use crate::il::IlVarId;

use super::*;
use super::parse::*;


#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    BitwiseAnd,
    BitwiseOr,
    ShiftLeft,
    ShiftRight,
    RotateLeft,
    RotateRight,
}

impl BinaryOp {
    pub fn eval(&self, src1: Number, src2: Number) -> Number {
        match (src1, src2) {
            (Number::U8(n1), Number::U8(n2)) => {
                Number::U8(match self {
                    BinaryOp::Add => n1.wrapping_add(n2),
                    BinaryOp::Subtract => n1.wrapping_sub(n2),
                    BinaryOp::Multiply => n1.wrapping_mul(n2),
                    BinaryOp::Divide => {
                        assert_ne!(0, n2);
                        n1.wrapping_div(n2)
                    },
                    BinaryOp::BitwiseAnd => n1 & n2,
                    BinaryOp::BitwiseOr => n1 | n2,
                    BinaryOp::ShiftLeft => n1.wrapping_shl(n2.into()),
                    BinaryOp::ShiftRight => n1.wrapping_shr(n2.into()),
                    BinaryOp::RotateLeft => n1.rotate_left(n2.into()),
                    BinaryOp::RotateRight => n1.rotate_right(n2.into()),
                })
            }
            (Number::U16(n1), Number::U16(n2)) => {
                Number::U16(match self {
                    BinaryOp::Add => n1.wrapping_add(n2),
                    BinaryOp::Subtract => n1.wrapping_sub(n2),
                    BinaryOp::Multiply => {
                        // assert!(n1 <= 0xFF);
                        // assert!(n2 <= 0xFF);
                        (n1 & 0xFF).wrapping_mul(n2 & 0xFF)
                    },
                    BinaryOp::BitwiseAnd => n1 & n2,
                    BinaryOp::BitwiseOr => n1 | n2,
                    BinaryOp::ShiftLeft => n1.wrapping_shl(n2.into()),
                    BinaryOp::ShiftRight => n1.wrapping_shr(n2.into()),
                    BinaryOp::RotateLeft => n1.rotate_left(n2.into()),
                    BinaryOp::RotateRight => n1.rotate_right(n2.into()),
                    &BinaryOp::Divide => todo!(),
                })
            }
            (Number::U32(n1), Number::U32(n2)) => {
                Number::U32(match self {
                    BinaryOp::Add => n1.wrapping_add(n2),
                    BinaryOp::Subtract => n1.wrapping_sub(n2),
                    BinaryOp::Multiply => {
                        // assert!(n1 <= 0xFF);
                        // assert!(n2 <= 0xFF);
                        (n1 & 0xFF).wrapping_mul(n2 & 0xFF) & 0xFFFF
                    },
                    BinaryOp::BitwiseAnd => n1 & n2,
                    BinaryOp::BitwiseOr => n1 | n2,
                    BinaryOp::ShiftLeft => n1.wrapping_shl(n2),
                    BinaryOp::ShiftRight => n1.wrapping_shr(n2),
                    BinaryOp::RotateLeft => n1.rotate_left(n2),
                    BinaryOp::RotateRight => n1.rotate_right(n2),
                    &BinaryOp::Divide => todo!(),
                })
            }
            _ => panic!(),
        }
    }

    fn parse(pair: pest::iterators::Pair<Rule>) -> Self {
        match pair.as_str() {
            "+" => Self::Add,
            "-" => Self::Subtract,
            "*" => Self::Multiply,
            "/" => Self::Divide,
            "|" => Self::BitwiseOr,
            "&" => Self::BitwiseAnd,
            "<<" => Self::ShiftLeft,
            ">>" => Self::ShiftRight,
            "<ROR<" => Self::RotateLeft,
            ">ROR>" => Self::RotateRight,
            op => panic!("Unknown op: {}", op),
        }
    }

    pub fn is_commutative(&self) -> bool {
        match self {
            Self::Add |
            Self::Multiply |
            Self::BitwiseOr |
            Self::BitwiseAnd 
                => true,
            Self::Subtract |
            Self::Divide |
            Self::ShiftLeft |
            Self::ShiftRight |
            Self::RotateLeft |
            Self::RotateRight 
                => false,
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Constant {
    Number(Number),
    ByteArray(Vec<u8>)
}

impl Constant {
    pub fn as_bytes(&self) -> Cow<Vec<u8>> {
        match self {
            Constant::Number(n) => Cow::Owned(n.as_bytes()),
            Constant::ByteArray(ba) => Cow::Borrowed(ba),
        }
    }

    pub fn byte_count(&self) -> u32 {
        match self {
            Constant::Number(n) => n.num_type().byte_count(),
            Constant::ByteArray(ba) => ba.len() as u32,
        }
    }
}

impl From<Number> for Constant {
    fn from(value: Number) -> Self {
        Constant::Number(value)
    }
}

impl From<Vec<u8>> for Constant {
    fn from(value: Vec<u8>) -> Self {
        Constant::ByteArray(value)
    }
}

impl<'a> From<&'a Constant> for Vec<u8> {
    fn from(value: &'a Constant) -> Self {
        match value {
            Constant::Number(n) => n.as_bytes(),
            Constant::ByteArray(ba) => ba.clone(),
        }
    }
}

impl<'a> From<&'a Constant> for Cow<'a,Vec<u8>> {
    fn from(value: &'a Constant) -> Self {
        match value {
            Constant::Number(n) => Cow::Owned(n.as_bytes()),
            Constant::ByteArray(ba) => Cow::Borrowed(ba),
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Expression {
    Ident(String),
    Number(Number),
    TtyIn(),
    Array(Vec<Expression>),
    Arithmetic(BinaryOp, Box<Expression>, Box<Expression>),
    Comparison(Box<Comparison>),
    Deref(Box<Expression>),
    LocalFieldDeref(String,String),
    PtrFieldDeref(String,String),
    AddressOf(Box<Expression>),
    Index(String, Box<Expression>),
    Cast{old_type: Option<Type>, new_type: Type, value:Box<Expression>},
    Call(Call),
}

impl Expression {
    pub fn frame_pointer() -> Expression {
        Expression::Ident(IlVarId::frame_pointer_str().to_owned())
    }

    pub fn try_get_const_bytes(&self) -> Option<Constant> {
        match self {
            Expression::Number(n) => Some(Constant::Number(*n)),
            _ => {
                let mut bytes = Vec::new();
                if self.try_get_const_bytes_inner(&mut bytes) {
                    Some(Constant::ByteArray(bytes))
                } else {
                    None
                }
            }
        }
    }

    fn try_get_const_bytes_inner(&self, bytes: &mut Vec<u8>) -> bool {
        match self {
            Expression::Array(vals) => {
                for val in vals {
                    if !val.try_get_const_bytes_inner(bytes) {
                        return false;
                    }
                }
                true
            }
            Expression::Number(n) => {
                for b in n.as_bytes() {
                    bytes.push(b);
                }
                true
            }
            _ => false
        }
    }

    pub fn try_get_const(&self) -> Option<Constant> {
        match self {
            Expression::Number(n) => Some(Constant::Number(*n)),
            Expression::Cast{old_type: _, new_type, value} => {
                if let (Some(v), Some(new_size)) = (value.try_get_const(), new_type.try_byte_count()) {
                    match v {
                        Constant::Number(v) => {
                            let v = v.as_bytes();
                            let mut new_v = vec![0u8; new_size as usize];
                            new_v.as_mut_slice()[0..v.len()].copy_from_slice(v.as_slice());
                            Some(Constant::Number(new_v.as_slice().try_into().unwrap()))
                        },
                        Constant::ByteArray(ba) => {
                            panic!("Trying to cast a byte array (of length {}) to size {}.", ba.len(), new_size);
                        },
                    }
                } else {
                    None
                }
            },
            Expression::Arithmetic(op, left, right) => {
                if let (Some(Constant::Number(left)), Some(Constant::Number(right))) = (left.try_get_const(), right.try_get_const()) {
                    Some(Constant::Number(op.eval(left, right)))
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
                if n == IlVarId::frame_pointer_str() {
                    Some(Type::Number(NumberType::U32))
                } else if let Some(f) = f {
                    let (_,t) = f.find_arg_or_var(ctxt, n)
                        .expect(&format!("Cannot find {} in {:?}", n, f));
                    Some(t.clone())
                } else {
                    None
                }
            }
            Expression::Index(name, _) => {
                if let Some(f) = f {
                    let (_,t) = f.find_arg_or_var(ctxt, name)
                        .expect(&format!("Cannot find {} in {:?}", name, f));
                    match t {
                        Type::Array(element_type, _) |
                        Type::Ptr(element_type) => Some(element_type.as_ref().clone()),
                        Type::Number(nt) => {
                            match nt {
                                NumberType::U8 | NumberType::U16 => panic!(),
                                NumberType::U32 => Some(Type::Number(NumberType::U8)),
                            }
                        }
                        _ => panic!(),
                    }
                } else {
                    None
                }
            }
            Expression::Number(n) => Some(Type::Number(n.num_type())),
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
                        (1,2) | (2,1) | (2,2) => Some(Type::Number(NumberType::U16)),
                        (1,4) | (4,1) | (2,4) | (4,2) | (4,4) => Some(Type::Number(NumberType::U32)),
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
                    let (_, struct_type) = f.find_arg_or_var(ctxt, n)
                        .expect(&format!("Cannot find {} in {:?}", n, f));
                    let struct_type = match struct_type { 
                        Type::Struct(struct_type) => {
                            &ctxt.struct_types[struct_type]
                        }
                        _ => panic!("Not a struct: {:?}", struct_type)
                    };
                    let (_, field_type) = struct_type.get_field(ctxt, field);
                    Some(field_type.clone())
                } else {
                    None
                }
            }
            Expression::PtrFieldDeref(n, field) => {
                if let Some(f) = f {
                    let (_, ptr_type) = f.find_arg_or_var(ctxt, n)
                        .expect(&format!("Cannot find {} in {:?}", n, f));
                    let element_type = ptr_type.get_element_type().unwrap();
                    let struct_type = match element_type { 
                        Type::Struct(struct_type) => {
                            &ctxt.struct_types[struct_type]
                        }
                        _ => panic!()
                    };
                    let (_, field_type) = struct_type.get_field(ctxt, field);
                    Some(field_type.clone())
                } else {
                    None
                }
            }
            Expression::TtyIn() => Some(Type::Number(NumberType::U8)),
            Expression::Comparison(_) => todo!(),
            Expression::Cast { old_type:_, new_type, value:_ } => Some(new_type.clone()),
            Expression::Array(_) => todo!(),
        }
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
                    Expression::Number(Number::U8(number as u8))
                } else {
                    Expression::Number(Number::U32(number))
                }
            }
            Rule::char_literal => {
                let number = number.into_inner().next().unwrap();
                Expression::Number(Number::U8(number.as_str().chars().next().unwrap() as u8))
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
                let op = BinaryOp::parse(pairs.next().unwrap());
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
                Expression::Number(Number::U32(RAM_MIN))
            }
            Rule::cast_expression => {
                let mut pairs = pair.into_inner();
                let value = Box::new(Expression::parse(pairs.next().unwrap()));
                let new_type = Type::parse(pairs.next().unwrap(), false);
                Expression::Cast{ old_type: None, new_type, value }
            }
            Rule::array_expression => {
                let mut pairs = pair.into_inner();
                let mut vals = Vec::new();
                while let Some(val) = pairs.next() {
                    vals.push(Expression::parse(val));
                }
                Expression::Array(vals)
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