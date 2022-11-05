use crate::*;


#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum ComparisonOperator {
    Equals,
    NotEquals,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
}

impl ComparisonOperator {
    pub fn parse(pair: pest::iterators::Pair<Rule>) -> ComparisonOperator {
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

#[derive(Debug, Clone, Hash)]
pub struct Comparison {
    pub op: ComparisonOperator,
    pub left: Expression,
    pub right: Expression,
}
