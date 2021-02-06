use super::*;


pub const RESULT : &'static str = "RESULT";
pub const RETURN_ADDRESS : &'static str = "RETURN_ADDRESS";
pub const EPILOGUE : &'static str = "EPILOGUE";

#[derive(Clone,Debug)]
pub struct FunctionDefinition {
    pub name: String,
    pub args: Vec<(String,Type)>,
    pub locals: BTreeMap<String,Type>,
    pub return_type: Type,
    pub body: Vec<Statement>,
}

impl FunctionDefinition {
    fn walk_decls<F>(ctxt: &ProgramContext, s: &Statement, visitor: &mut F)
        where F : FnMut(&String, Option<&Type>),
    {
        match s {
            Statement::Assign{target, var_type, value:_} => {
                match target {
                    Expression::Ident(name) => {
                        visitor(name, var_type.as_ref());
                    }
                    _ => {}
                }
            }
            Statement::Declare {local, var_type} => {
                visitor(local, Some(var_type));
            }
            Statement::CallAssign{ local, var_type, call}  => {
                if let Some(local) = local {
                    let var_type = Some(match var_type {
                        Some(t) => t,
                        None => &ctxt.function_defs
                                    .get(&call.function)
                                    .expect(&format!("could not find function '{}'", &call.function))
                                    .return_type,
                    });
                    visitor(local, var_type);
                }
            }
            Statement::IfElse{ predicate:_, when_true, when_false } => {
                for s in when_true {
                    FunctionDefinition::walk_decls(ctxt, s, visitor);
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
            Statement::TtyOut{ value:_ }
            => {}
        }
    }

    pub fn parse(ctxt: &ProgramContext, pair: pest::iterators::Pair<Rule>) -> FunctionDefinition {
        assert_eq!(Rule::function, pair.as_rule());

        let mut args = Vec::new();

        let mut pairs = pair.into_inner();

        let name = pairs.next().unwrap().as_str().to_owned();

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

        // find locals
        let mut locals = BTreeMap::new();

        let mut find_locals  = |name: &String, var_type: Option<&Type>| {
            if let Some(var_type) = var_type {
                if !locals.contains_key(name) && !args.iter().any(|a|&a.0 == name) {
                    locals.insert(name.clone(), var_type.clone());
                }
            }
        };

        for s in &body {
            FunctionDefinition::walk_decls(ctxt, s, &mut find_locals);
        }

        let mut validate_no_mismatch = |name: &String, var_type: Option<&Type>| {
            if let Some(declared_type) = var_type {
                let existing = locals.get(name)
                    .or_else(|| args.iter().filter(|(n,_)| n == name).map(|a| &a.1).next());
                match existing {
                    Some(existing) => {
                        if existing != declared_type {
                            panic!(format!("Variable '{}' is declared with different types: {:?} and {:?}", name, existing, declared_type));
                        }
                    }
                    None => {
                        assert_eq!(var_type, None);
                        panic!(format!("No type found for '{}'", name));
                    }
                }
            }
        };

        for s in &body {
            FunctionDefinition::walk_decls(ctxt, s, &mut validate_no_mismatch);
        }

        for s in body.iter_mut() {
            s.optimize(ctxt);
        }

        FunctionDefinition { name, args, locals, return_type, body}
    }

    pub fn allocate(self, ctxt: &mut ProgramContext) -> AllocatedFunction {

        fn mark_as_on_stack(_ctxt: &ProgramContext, e: &Expression, needs_to_be_on_stack: &mut BTreeSet<String>) {
            let mut mark = |e: &Expression| {
                dbg!(e);
                match e {
                    Expression::Ident(name) => {
                        needs_to_be_on_stack.insert(name.clone());
                    }
                    _ => {}
                }
            };
            e.visit_inner_expressions(&mut mark);
        }

        fn find_address_of_exp(ctxt: &ProgramContext, e: &Expression, needs_to_be_on_stack: &mut BTreeSet<String>) {
            let mut find = |e: &Expression| {
                // dbg!(e);
                match e {
                    Expression::AddressOf(inner) => {
                        mark_as_on_stack(ctxt, inner.as_ref(), needs_to_be_on_stack);
                    }
                    Expression::Index(name, ..) => {
                        needs_to_be_on_stack.insert(name.clone());
                    }
                    _ => {}
                }
            };
            e.visit_inner_expressions(&mut find);
        }

        fn find_address_of_stmt(ctxt: &ProgramContext, s: &Statement, needs_to_be_on_stack: &mut BTreeSet<String>) {
            match s {
                Statement::Assign{target:_, var_type:_, value} => {
                    find_address_of_exp(ctxt, value, needs_to_be_on_stack);
                }
                Statement::Declare {local:_, var_type:_} => {}
                Statement::CallAssign{ local:_, var_type:_, call}  => {
                    for p in &call.parameters {
                        find_address_of_exp(ctxt, p, needs_to_be_on_stack);
                    }
                }
                Statement::IfElse{ predicate, when_true, when_false } => {
                    find_address_of_exp(ctxt, predicate, needs_to_be_on_stack);
                    for s in when_true {
                        find_address_of_stmt(ctxt, s, needs_to_be_on_stack);
                    }
                    for s in when_false {
                        find_address_of_stmt(ctxt, s, needs_to_be_on_stack);
                    }
                },
                Statement::While{predicate: _, while_true} => {
                    for s in while_true {
                        find_address_of_stmt(ctxt, s, needs_to_be_on_stack);
                    }
                },
                Statement::Return{ value } |
                Statement::TtyOut{ value } => {
                    find_address_of_exp(ctxt, value, needs_to_be_on_stack);
                }
            }
        }

        let mut needs_to_be_on_stack = BTreeSet::new();

        for s in &self.body {
            find_address_of_stmt(ctxt, s, &mut needs_to_be_on_stack);
        }

        /*
        stack:

        SP ->   local 3 (padded to 4 bytes)
                local 2 (padded to 4 bytes)
                local 1 (padded to 4 bytes)
                32-bit return address (32-bit aligned)
                arg 2 (padded to 4 bytes)
                arg 1  (padded to 4 bytes)

        R0..    RESULT
        */

        let mut variables = BTreeMap::new();
        let mut offset = 0;

        let mut registers_used = BTreeSet::new();

        // locals
        for (name, var_type) in &self.locals {
            let byte_count = var_type.byte_count(ctxt) as u8;
            let storage = {
                let register = if needs_to_be_on_stack.contains(name) {
                    None
                } else if byte_count > 4 {
                    None  
                } else {
                    if let Some(regs) = ctxt.find_registers(byte_count) {
                        for r in &regs {
                            registers_used.insert(*r);
                        }
                        Some(regs[0])
                    } else {
                        None
                    }
                };

                if let Some(reg) = register {
                    Storage::Register(reg)
                }
                else {
                    let size = var_type.byte_count(ctxt);
                    let s = Storage::Stack(BaseOffset(offset.try_into().unwrap()));
                    offset += std::cmp::max(size,4);
                    s
                }
            };

            variables.insert(name.clone(), Variable {
                decl: Declaration::Local,
                var_type: var_type.clone(),
                storage
            });
        }

        let callee_stack_size = offset;

        // return address
        variables.insert(RETURN_ADDRESS.to_owned(), Variable {
            decl: Declaration::ReturnAddress,
            var_type: Type::Number(NumberType::USIZE),
            storage: Storage::Stack(BaseOffset(offset.try_into().unwrap()))
        });
        offset += 4;

        // args
        for arg in self.args.iter().rev() {
            variables.insert(arg.0.clone(), Variable {
                var_type: arg.1.clone(),
                decl: Declaration::Arg,
                storage: Storage::Stack(BaseOffset(offset as u32))
            });
            offset += 4;
        }

        // result
        if self.return_type != Type::Void {
            variables.insert(RESULT.to_owned(), Variable {
                decl: Declaration::Result,
                var_type: self.return_type.clone(),
                storage: Storage::Register(Register(0))
            });
        }

        AllocatedFunction { def: self, registers_used, variables, callee_stack_size }
    }
}

#[derive(Debug)]
pub struct AllocatedFunction {
    pub def: FunctionDefinition,
    pub registers_used: BTreeSet<Register>,
    pub variables: BTreeMap<String,Variable>,
    pub callee_stack_size: u32,
}

impl AllocatedFunction {
    pub fn emit<'a>(&'a self, program: &'a ProgramContext) -> FunctionContext<'a> {
        let mut ctxt = FunctionContext {
            program,
            function: &self,
            lines: Vec::new(),
            additional_offset: 0,
            block_counter: 0,
        };
        ctxt.lines.push(AssemblyInputLine::Comment(format!("# Function: {}", &self.def.name)));
        ctxt.lines.push(AssemblyInputLine::Label(format!(":{}", &self.def.name)));

        // let max_register_local_count = 0u32;
        // let mut register_local_count = 0;
        // while register_local_count < max_register_local_count {
        //     register_local_count += 1;
        //     unimplemented!();
        // }

        let mut vars: Vec<_> = self.variables.iter().collect();
        vars.sort_by_key(|k| k.1.storage);

        for (name, var) in vars {
            match var.storage {
                Storage::Register(r) => {
                    ctxt.lines.push(AssemblyInputLine::Comment(format!("# r{:02x} -> {} {:?} {:?}", r.0, name, var.decl, var.var_type)));
                }
                Storage::Stack(offset) => {
                    ctxt.lines.push(AssemblyInputLine::Comment(format!("# sp+0x{:02x} -> {} {:?} {:?}", offset.0, name, var.decl, var.var_type)));
                }
            }
        }

        if self.callee_stack_size > 0 {
            ctxt.lines.push(AssemblyInputLine::Comment("create stack space".to_owned()));
            ctxt.add_inst(Instruction {
                opcode: Opcode::LoadImm8,
                resolved: None,
                source: format!("filler for allocated stack space"),
                args: vec![Value::Register(0), Value::Constant8(0xBB)]
            });
            for _ in 0..self.callee_stack_size {
                ctxt.add_inst(Instruction {
                    opcode: Opcode::Push8,
                    resolved: None,
                    source: format!("locals and padding"),
                    args: vec![Value::Register(0)]
                });
            }
        }

        // let mut count = 0;
        for stmt in self.def.body.iter() {
            // let scope = format!("_function{}_", count);
            stmt.emit(&mut ctxt);
            // count += 1;
        }

        ctxt.lines.push(AssemblyInputLine::Label(format!(":{}__{}", &self.def.name, EPILOGUE)));
        if self.callee_stack_size > 0 {
            ctxt.add_inst(Instruction {
                opcode: Opcode::AddImm32IgnoreCarry,
                source: format!("get stack pointing to RA"),
                args: vec![Value::Register(REG_SP), Value::Constant32(self.callee_stack_size)],
                resolved: None,
            });
        }

        // if register_local_count > 0 {
        //     ctxt.lines.push(Line::Comment(format!("save regs: {:?}", ctxt.regs_used)));
        //     let regs : Vec<Reg> = ctxt.regs_used.iter().cloned().rev().collect();
        //     for r in regs {
        //         ctxt.add_macro(format!("pop {}", r));
        //     }
        // }

        ctxt.add_macro(format!("!return"));

        ctxt
    }
}

