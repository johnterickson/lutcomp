use super::*;

#[derive(Clone, Debug)]
pub enum Statement {
    Declare { scope: Scope, name: String, var_type: Type },
    Assign { target: Expression, var_type: Option<Type>, value: Expression},
    VoidExpression { expression: Expression },
    IfElse {if_blocks: Vec<(Comparison,Vec<Statement>)>, else_block: Vec<Statement> },
    While {predicate: Comparison, while_true: Vec<Statement>},
    Return { value: Option<Expression>},
    TtyOut {value: Expression},
}

impl Statement {
    pub fn optimize(&mut self, ctxt: &ProgramContext) -> bool{
        let mut optimized = false;
        match self {
            Statement::Assign { target, var_type:_, value} => {
                while target.optimize(ctxt) { optimized = true; }
                while value.optimize(ctxt) { optimized = true; }
            }
            Statement::While { predicate, while_true} => {
                while predicate.left.optimize(ctxt) { optimized = true; }
                while predicate.right.optimize(ctxt) { optimized = true; }
                for s in while_true {
                    while s.optimize(ctxt) { optimized = true; }
                }
            }
            Statement::Return {value} => {
                if let Some(value) = value {
                    while value.optimize(ctxt) { optimized = true; }
                }
            }
            Statement::Declare { .. } =>  {},
            Statement::VoidExpression {expression } => {
                while expression.optimize(ctxt) { optimized = true; } 

            }
            Statement::IfElse { if_blocks, else_block: when_false } => {
                for (predicate, when_true) in if_blocks {
                    while predicate.left.optimize(ctxt) { optimized = true; }
                    while predicate.right.optimize(ctxt) { optimized = true; }
                    for s in when_true {
                        while s.optimize(ctxt) { optimized = true; }
                    }
                }
                for s in when_false {
                    while s.optimize(ctxt) { optimized = true; }
                }
            }
            Statement::TtyOut { value } => {
                while value.optimize(ctxt) { optimized = true; }
            }
        }

        optimized
    }

    pub fn parse(pair: pest::iterators::Pair<Rule>) -> Statement {
        assert_eq!(Rule::statement, pair.as_rule());
        let pair = pair.into_inner().next().unwrap();

        match pair.as_rule() {
            Rule::static_statement | Rule::declare_statement => {
                let scope = if pair.as_rule() == Rule::static_statement {
                    Scope::Static
                } else {
                    Scope::Local
                };
                let decl = pair.into_inner().next().unwrap();
                assert_eq!(decl.as_rule(), Rule::variable_decl);
                let mut decl_tokens = decl.into_inner();
                let var_name = decl_tokens.next().unwrap().as_str().trim().to_owned();
                let var_type = Type::parse(decl_tokens.next().unwrap(), true);
                Statement::Declare {scope, name: var_name, var_type }
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
                                let var_type = Type::parse(decl_tokens.next().unwrap(), true);
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
            Rule::if_else_statement => {
                let mut pairs = pair.into_inner();

                let mut if_blocks = Vec::new();
                let mut else_block = Vec::new();

                while let Some(pair) = pairs.next() {
                    match pair.as_rule() {
                        Rule::else_clause => {
                            let mut pairs = pair.into_inner();
                            while let Some(pair) = pairs.next() {
                                else_block.push(Statement::parse(pair));
                            }
                        }
                        Rule::if_clause | Rule::else_if_clause => {
                            let mut when_true = Vec::new();
                            let mut pairs = pair.into_inner();
                            let predicate = match Expression::parse(pairs.next().unwrap()) {
                                Expression::Comparison(c) => *c,
                                _ => panic!()
                            };
                            while let Some(pair) = pairs.next() {
                                when_true.push(Statement::parse(pair));
                            }
                            if_blocks.push((predicate, when_true));
                        }
                        _ => panic!(),
                    }
                }

                Statement::IfElse { if_blocks, else_block }
            },
            Rule::return_statement => {
                let value = pair.into_inner().next().map(|e| Expression::parse(e));
                Statement::Return { value }
            },
            Rule::ttyout => {
                let mut pairs = pair.into_inner();
                let value = Expression::parse(pairs.next().unwrap());
                Statement::TtyOut { value }
            },
            Rule::while_loop => {
                let mut pairs = pair.into_inner();
                let predicate = match Expression::parse(pairs.next().unwrap()) {
                    Expression::Comparison(c) => *c,
                    _ => panic!()
                };
                let mut while_true = Vec::new();
                while let Some(pair) = pairs.next() {
                    while_true.push(Statement::parse(pair));
                }
                Statement::While { predicate, while_true }
            },
            Rule::void_expression_statement => {
                let mut pairs = pair.into_inner();
                let expression = Expression::parse(pairs.next().unwrap());
                Statement::VoidExpression { expression }
            }
            _ => panic!("Unexpected {:?}", pair)
        }
    }
}