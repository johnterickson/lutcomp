use std::{convert::TryFrom, borrow::Cow};

use super::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NumberType {
    U8,
    U16,
    U32,
}

impl TryFrom<u32> for NumberType {
    type Error = ();

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(NumberType::U8),
            2 => Ok(NumberType::U16),
            4 => Ok(NumberType::U32),
            _ => Err(())
        }
    }
}

impl From<&NumberType> for NumberType {
    fn from(nt: &NumberType) -> Self {
        match nt {
            NumberType::U8 => NumberType::U8,
            NumberType::U16 => NumberType::U16,
            NumberType::U32 => NumberType::U32,
        }
    }
}

impl TryByteSize for NumberType {
    fn try_byte_count(&self) -> Option<u32> {
        Some(self.byte_count())
    }
}

impl ByteSize for NumberType {
    fn byte_count(&self, _ctxt: &ProgramContext) -> u32 {
        self.byte_count()
    }
}

impl NumberType {
    pub fn byte_count(&self) -> u32 {
        match self {
            NumberType::U8 => 1,
            NumberType::U16 => 2,
            NumberType::U32 => 4,
        }
    }

    pub fn try_parse(s: &str) -> Option<Self> {
        match  s {
            "u8" | "char" => Some(Self::U8),
            "u16" => Some(Self::U16),
            "usize" => Some(Self::U32),
            _ => None
        }
    }
    
    fn parse(s: &str) -> Self {
        Self::try_parse(s).expect(&format!("Not a number type: {}", s))
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub enum Number {
    U8(u8),
    U16(u16),
    U32(u32),
}

impl Number {
    pub fn num_type(&self) -> NumberType {
        match self {
            Number::U8(_) => NumberType::U8,
            Number::U16(_) => NumberType::U16,
            Number::U32(_) => NumberType::U32,
        }
    }

    pub fn as_bytes(&self) -> Vec<u8> {
        match self {
            Number::U8(n) => vec![*n],
            Number::U16(n) => n.to_le_bytes().iter().cloned().collect(),
            Number::U32(n) => n.to_le_bytes().iter().cloned().collect(),
        }
    }

    pub fn as_u32(&self) -> u32 {
        match self {
            Number::U8(n) => (*n).into(),
            Number::U16(n) => (*n).into(),
            Number::U32(n) => *n,
        }
    }

    pub fn cast_checked(&self, promoted: NumberType) -> Number {
        match promoted {
            NumberType::U8 => Number::U8(self.as_u32().try_into().unwrap()),
            NumberType::U16 => Number::U16(self.as_u32().try_into().unwrap()),
            NumberType::U32 => Number::U32(self.as_u32().try_into().unwrap()),
        }
    }
}

impl std::fmt::Debug for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::U8(arg0) => write!(f, "0n{}/0x{:02x}u8", arg0, arg0),
            Self::U16(arg0) => write!(f, "0n{}/0x{:04x}u16", arg0, arg0),
            Self::U32(arg0) => if *arg0 >= 0x80000000 {
                write!(f, "0x{:08x}u32", arg0)
            } else {
                write!(f, "0n{}/0x{:08x}u32", arg0, arg0)
            }
        }
    }
}

impl From<u8> for Number {
    fn from(x: u8) -> Self {
        Number::U8(x)
    }
}

impl From<u16> for Number {
    fn from(x: u16) -> Self {
        Number::U16(x)
    }
}

impl From<u32> for Number {
    fn from(x: u32) -> Self {
        Number::U32(x)
    }
}

impl TryFrom<&[u8]> for Number {
    type Error = String;

    fn try_from(b: &[u8]) -> Result<Self, Self::Error> {
        match b.len() {
            1 => Ok(Number::U8(b[0])),
            2 => Ok(Number::U16(u16::from_le_bytes([b[0],b[1]]))),
            4 => Ok(Number::U32(u32::from_le_bytes([b[0],b[1],b[2],b[3]]))),
            _ => Err(format!("Const size of {} bytes does not match a number type.", b.len())),
        } 
    }
}

impl TryFrom<&Vec<u8>> for Number {
    type Error = String;

    fn try_from(b: &Vec<u8>) -> Result<Self, Self::Error> {
        let b: &[u8] = b.as_ref();
        b.try_into()
    }
}

impl<'a> TryFrom<Cow<'a,Vec<u8>>> for Number {
    type Error = String;

    fn try_from(b: Cow<'a,Vec<u8>>) -> Result<Self, Self::Error> {
        let b: &[u8] = b.as_ref();
        b.try_into()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Scope {
    Local,
    Static,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Void,
    Number(NumberType),
    Ptr(Box<Type>),
    Struct(String),
    Array(Box<Type>,Option<u32>),
}

impl Type {
    pub fn get_number_type(&self) -> Option<NumberType> {
        match self {
            Type::Number(nt) => Some(*nt),
            Type::Ptr(_) => Some(NumberType::U32),
            Type::Array(_,_) => Some(NumberType::U32),
            _ => None,
        }
    }

    pub fn get_element_type(&self) -> Option<&Type> {
        match self {
            Type::Ptr(element_type) | Type::Array(element_type, _) => Some(element_type),
            _ => None
        }
    }

    pub fn parse(pair: pest::iterators::Pair<Rule>, is_decl: bool) -> Type {
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
                let val_type = tokens.next().unwrap().as_str().trim();
                let val_type = if let Some(nt) = NumberType::try_parse(val_type) {
                    Type::Number(nt)
                } else {
                    Type::Struct(val_type.to_owned())
                };

                let count = tokens.next().map(|count_token| {
                    let count_exp = Expression::parse(count_token);
                    let count = match count_exp.try_get_const() {
                        Some(Constant::Number(n)) => n.as_u32(),
                        _ => panic!("Could not evaluate array size as a constant value: {:?}", count_exp),
                    };
                    assert!(tokens.next().is_none());
                    count
                });
                Type::Array(Box::new(val_type), count)
            }
            _ => panic!("unexpected {:?}", variable)
        }
    }
}

pub trait ByteSize {
    fn byte_count(&self, ctxt: &ProgramContext) -> u32;
}

pub trait TryByteSize {
    fn try_byte_count(&self) -> Option<u32>;
}

impl ByteSize for Type {
    fn byte_count(&self, ctxt: &ProgramContext ) -> u32 {
        match self {
            Type::Void => 0,
            Type::Number(nt) => nt.byte_count(),
            Type::Ptr(_) => 4,
            Type::Struct(struct_name) => ctxt.struct_types.get(struct_name)
                .expect(&format!("Could not find struct definition for '{}'.", struct_name))
                .byte_count(ctxt),
            Type::Array(nt, count) => nt.byte_count(ctxt) * count.unwrap()
        }
    }
}

impl TryByteSize for Type {
    fn try_byte_count(&self) -> Option<u32> {
        match self {
            Type::Void => panic!(),
            Type::Number(nt) => nt.try_byte_count(),
            Type::Ptr(_) => Some(4),
            Type::Struct(_) => None,
            Type::Array(nt, count) => nt.try_byte_count().map(|s| s * count.unwrap()),
        }
    }
}