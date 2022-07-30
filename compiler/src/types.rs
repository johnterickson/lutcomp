use super::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum NumberType {
    U8,
    U16,
    USIZE,
}

impl NumberType {
    pub fn try_parse(s: &str) -> Option<NumberType> {
        match  s {
            "u8" | "char" => Some(NumberType::U8),
            "u16" => Some(NumberType::U16),
            "usize" => Some(NumberType::USIZE),
            _ => None
        }
    }
    fn parse(s: &str) -> NumberType {
        Self::try_parse(s).expect(&format!("Not a number type: {}", s))
    }
}

impl ByteSize for NumberType {
    fn byte_count(&self, _ctxt: &ProgramContext) -> u32 {
        self.try_byte_count().unwrap()
    }
}

impl TryByteSize for NumberType {
    fn try_byte_count(&self) -> Option<u32> {
        Some(match self {
            NumberType::U8 => 1,
            NumberType::U16 => 2,
            NumberType::USIZE => 4,
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Scope {
    Local,
    Static,
}


#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Void,
    Number(NumberType),
    Ptr(Box<Type>),
    Struct(String),
    Array(Box<Type>,u32),
}

impl Type {
    pub fn get_number_type(&self) -> Option<NumberType> {
        match self {
            Type::Number(nt) => Some(*nt),
            Type::Ptr(_) => Some(NumberType::USIZE),
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
                let count_exp = Expression::parse(tokens.next().unwrap());
                let count = count_exp.try_get_const()
                    .expect(&format!("Could not evaluate array size as a constant value: {:?}", count_exp));
                let count = count.try_into()
                    .expect(&format!("Could not fit array size into 32-bit integer {}", count));
                assert!(tokens.next().is_none());
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
            Type::Number(nt) => nt.byte_count(ctxt),
            Type::Ptr(_) => 4,
            Type::Struct(struct_name) => ctxt.struct_types.get(struct_name)
                .expect(&format!("Could not find struct definition for '{}'.", struct_name))
                .byte_count(ctxt),
            Type::Array(nt, count) => nt.byte_count(ctxt) * count
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
            Type::Array(nt, count) => nt.try_byte_count().map(|s| s * count),
        }
    }
}