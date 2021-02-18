use super::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum NumberType {
    U8,
    USIZE,
}

impl NumberType {
    fn parse(s: &str) -> NumberType {
        match  s {
            "u8" | "char" => NumberType::U8,
            "usize" => NumberType::USIZE,
            other => panic!(format!("unknown type {}", other)),
        }
    }
}

impl ByteSize for NumberType {
    fn byte_count(&self, _ctxt: &ProgramContext) -> u32 {
        match self {
            NumberType::U8 => 1,
            NumberType::USIZE => 4,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Void,
    Number(NumberType),
    Ptr(Box<Type>),
    Struct(String),
    Array(Box<Type>,u32),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Scope {
    Local,
    Static,
}

impl Type {
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

pub trait ByteSize {
    fn byte_count(&self, ctxt: &ProgramContext) -> u32;
}

impl ByteSize for Type {
    fn byte_count(&self, ctxt: &ProgramContext ) -> u32 {
        match self {
            Type::Void => panic!(),
            Type::Number(nt) => nt.byte_count(ctxt),
            Type::Ptr(_) => 4,
            Type::Struct(struct_name) => ctxt.types.get(struct_name)
                .expect(&format!("Could not find struct definition for '{}'.", struct_name))
                .byte_count(ctxt),
            Type::Array(nt, count) => nt.byte_count(ctxt) * count
        }
    }
}