use std::{collections::{HashSet}, hash::Hash};

use super::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum FunctionAttribute {
    Inline,
}

impl FunctionAttribute {
    pub fn parse(s: &str) -> Option<FunctionAttribute> {
        match s {
            "inline" => Some(FunctionAttribute::Inline),
            _ => None,
        }
    }
}

#[derive(Clone,Debug)]
pub enum FunctionImpl {
    Body(Vec<Statement>),
    Intrinsic(Intrinsic)
}

#[derive(Clone,Debug)]
pub struct FunctionDefinition {
    pub name: String,
    pub attributes: HashSet<FunctionAttribute>,
    pub args: Vec<(String,Type)>,
    pub vars: BTreeMap<String,(Scope,Type)>,
    pub return_type: Type,
    pub body: FunctionImpl,
}

impl FunctionDefinition {

    pub fn is_intrinsic(&self) -> bool {
        match self.body {
            FunctionImpl::Intrinsic(_) => true,
            _ => false,
        }
    }

    pub fn find_arg_or_var(&self, name: &str) -> Option<(&Scope, &Type)> {
        if let Some((s,t)) = self.vars.get(name) {
            Some((s,t))
        }
        else if let Some((_,t)) = self.args.iter().filter(|(n,_)| n == name).next() {
            Some((&Scope::Local, t))
        } else {
            None
        }
    }

    fn walk_decls<F>(ctxt: &ProgramContext, s: &Statement, visitor: &mut F)
        where F : FnMut(&String, Option<&Scope>, Option<&Type>),
    {
        match s {
            Statement::Continue | Statement::Break => {},
            Statement::Assign{target, var_type, value} => {
                let value_type = value.try_emit_type(ctxt, None);
                let var_type = var_type.as_ref().or(value_type.as_ref());
                match target {
                    Expression::Ident(name) => {
                        visitor(name, None, var_type);
                    }
                    _ => {}
                }
            }
            Statement::Declare {scope, name: local, var_type} => {
                visitor(local, Some(scope), Some(var_type));
            }
            Statement::IfElse{ if_blocks, else_block: when_false } => {
                for (_, when_true) in if_blocks {
                    for s in when_true {
                        FunctionDefinition::walk_decls(ctxt, s, visitor);
                    }
                }
                for s in when_false {
                    FunctionDefinition::walk_decls(ctxt, s, visitor);
                }
            },
            Statement::While{predicate: _, while_true} => {
                for s in while_true {
                    FunctionDefinition::walk_decls(ctxt, s, visitor);
                }
            },
            Statement::Return{ value:_ } |
            Statement::TtyOut{ value:_ } |
            Statement::VoidExpression { expression:_ }
            => {}
        }
    }

    pub fn parse(ctxt: &ProgramContext, pair: pest::iterators::Pair<Rule>) -> FunctionDefinition {
        assert_eq!(Rule::function, pair.as_rule());

        let mut args = Vec::new();

        let mut pairs = pair.into_inner();

        let mut attributes = HashSet::new();

        let mut pair = pairs.next().unwrap();

        if pair.as_rule() == Rule::attributes {
            for pair in pair.into_inner() {
                attributes.insert(FunctionAttribute::parse(pair.as_str()).unwrap());
            }
            pair = pairs.next().unwrap();
        }

        assert_eq!(pair.as_rule(), Rule::ident);
        let name = pair.as_str().to_owned();

        for arg in pairs.next().unwrap().into_inner() {
            let mut arg_tokens = arg.into_inner();
            let arg_name = arg_tokens.next().unwrap().as_str().to_owned();
            let arg_var_type = Type::parse(arg_tokens.next().unwrap(), false);
            args.push((arg_name, arg_var_type));
        }

        let return_or_body = pairs.next().unwrap();
        let (return_type, body) = if Rule::function_return_type == return_or_body.as_rule() {
            let mut return_type_tokens = return_or_body.into_inner();
            let return_type = Type::parse(return_type_tokens.next().unwrap(), false);
            (return_type, pairs.next().unwrap())
        } else {
            (Type::Void, return_or_body)
        };

        let mut body : Vec<Statement> = body.into_inner().map(|p| Statement::parse(p)).collect();

        // find vars
        let mut vars = BTreeMap::new();

        let mut find_vars  = |name: &String, scope: Option<&Scope>, var_type: Option<&Type>| {
            if let Some(var_type) = var_type {
                let scope = scope.unwrap_or(&Scope::Local);
                if !vars.contains_key(name) && !args.iter().any(|a|&a.0 == name) {
                    vars.insert(name.clone(), (*scope, var_type.clone()));
                }
            }
        };

        for s in &body {
            FunctionDefinition::walk_decls(ctxt, s, &mut find_vars);
        }

        let mut validate_no_mismatch = |name: &String, scope: Option<&Scope>, var_type: Option<&Type>| {
            if let Some(declared_type) = var_type {
                let existing = vars.get(name)
                    .map(|(scope, var_type)| (scope, var_type))
                    .or_else(|| args.iter().filter(|(n,_)| n == name).map(|a| (&Scope::Local, &a.1)).next());
                match existing {
                    Some((existing_scope, existing_type)) => {
                        if existing_type != declared_type {
                            panic!("Variable '{}' is declared with different types: {:?} and {:?}", name, existing_type, declared_type);
                        }
                        if let Some(scope) = scope {
                            if existing_scope != scope {
                                panic!("Variable '{}' is declared with different scopes: {:?} and {:?}", name, existing_scope, scope);
                            }
                        }
                    }
                    None => {
                        assert_eq!(var_type, None);
                        panic!("No type found for '{}'", name);
                    }
                }
            }
        };

        for s in &body {
            FunctionDefinition::walk_decls(ctxt, s, &mut validate_no_mismatch);
        }

        for s in body.iter_mut() {
            while s.optimize(ctxt) { }
        }

        let body = FunctionImpl::Body(body);

        FunctionDefinition { name, attributes, args, vars, return_type, body}
    }
}