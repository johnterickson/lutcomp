use super::*;
use super::parse::*;

#[derive(Debug, Clone)]
pub struct Call {
    pub function: String, 
    pub parameters: Vec<Expression>,
}

impl Call {
    pub fn parse(pair: pest::iterators::Pair<Rule>) -> Call {
        assert_eq!(pair.as_rule(), Rule::call_expression);

        let mut tokens = pair.into_inner();
        let token = tokens.next().unwrap();
        assert_eq!(Rule::ident, token.as_rule());
        let function = token.as_str().to_owned();

        let mut parameters = Vec::new();
        while let Some(arg) = tokens.next() {
            parameters.push(Expression::parse(arg));
        }

        Call{function, parameters}
    }
}