use super::*;

#[derive(Clone, Debug)]
pub enum Statement {
    Declare { scope: Scope, name: String, var_type: Type },
    Assign { target: Expression, var_type: Option<Type>, value: Expression},
    CallAssign { name: Option<String>, var_type: Option<Type>, call: Call },
    IfElse {predicate: Expression, when_true: Vec<Statement>, when_false: Vec<Statement> },
    While {predicate: Expression, while_true: Vec<Statement>},
    Return { value: Expression},
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
                while predicate.optimize(ctxt) { optimized = true; }
                for s in while_true {
                    while s.optimize(ctxt) { optimized = true; }
                }
            }
            Statement::Return {value} => {
                while value.optimize(ctxt) { optimized = true; }
            }
            Statement::Declare { .. } =>  {},
            Statement::CallAssign { name:_, var_type:_, call } => {
                for p in &mut call.parameters {
                    while p.optimize(ctxt) { optimized = true; } 
                }
            }
            Statement::IfElse { predicate, when_true, when_false } => {
                while predicate.optimize(ctxt) { optimized = true; }
                for s in when_true {
                    while s.optimize(ctxt) { optimized = true; }
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
            Rule::call => {
                // dbg!(&pair);
                let mut pairs = pair.into_inner();
                let mut token = pairs.next().unwrap();
                let (var_name, var_type) = match token.as_rule() {
                    Rule::call_return_value => {
                        let mut variable = token.into_inner();
                        let var_name = variable.next().unwrap().as_str().trim().to_owned();
                        let var_type = if let Some(token) = variable.next() {
                            Some(Type::parse(token, false))
                        } else {
                            None
                        };
                    
                        token = pairs.next().unwrap();

                        (Some(var_name), var_type)
                    }
                    Rule::call_expression => {
                        (None, None)
                    }
                    _ => panic!("Unexpected {:?}", &token)
                };

                let call = Call::parse(token);

                Statement::CallAssign { name:var_name, var_type, call }
            },
            Rule::if_else_statement => {
                let mut pairs = pair.into_inner();
                let predicate = Expression::parse(pairs.next().unwrap());
                let mut when_true = Vec::new();
                let mut when_false = Vec::new();
                while let Some(pair) = pairs.next() {
                    if Rule::else_clause == pair.as_rule() {
                        let mut else_pairs = pair.into_inner();
                        while let Some(pair) = else_pairs.next() {
                            when_false.push(Statement::parse(pair));
                        }
                    } else {
                        when_true.push(Statement::parse(pair));
                    }
                }
            
                Statement::IfElse { predicate, when_true, when_false }
            },
            Rule::return_statement => {
                let expr = pair.into_inner().next().unwrap();
                Statement::Return { value: Expression::parse(expr) }
            },
            Rule::ttyout => {
                let mut pairs = pair.into_inner();
                let value = Expression::parse(pairs.next().unwrap());
                Statement::TtyOut { value }
            },
            Rule::while_loop => {
                let mut pairs = pair.into_inner();
                let predicate = Expression::parse(pairs.next().unwrap());
                let mut while_true = Vec::new();
                while let Some(pair) = pairs.next() {
                    while_true.push(Statement::parse(pair));
                }
                Statement::While { predicate, while_true }
            },
            _ => panic!("Unexpected {:?}", pair)
        }
    }

    pub fn emit(&self, ctxt: &mut FunctionContext) -> () {
        ctxt.lines.push(AssemblyInputLine::Comment(format!("Begin statement {:?}", self)));
        // eprintln!("Emitting statement: {:?}", &self);
        match self {
            Statement::TtyOut{value} => {
                value.emit(ctxt);
                ctxt.add_inst(Instruction {
                    opcode: Opcode::Pop8,
                    resolved: None,
                    source: format!("{:?}", &self),
                    args: vec![Value::Register(0)]
                });
                ctxt.additional_offset -= 1;

                ctxt.add_inst(Instruction {
                    opcode: Opcode::TtyOut,
                    resolved: None,
                    source: format!("{:?}", &self),
                    args: vec![Value::Register(0)]
                });
            },
            Statement::Declare{scope:_, name:_, var_type:_} => {}
            Statement::Assign{target, var_type, value} => {

                let start_offset = ctxt.additional_offset;

                let value_type = value.emit(ctxt); // calculate value
                
                if let Some(explicit_type) = var_type {
                    assert_eq!(explicit_type, &value_type);
                }

                let value_size = value_type.byte_count(ctxt.program);
                assert!(value_size <= 4);
                assert_eq!(start_offset + value_size, ctxt.additional_offset);

                let emit_result= target.try_emit_address_to_reg0(ctxt);
                match emit_result {
                    EmitAddressResult::ValueInRegister{reg, value_type: type_in_reg} => {
                        assert_eq!(value_type, type_in_reg);
                        for i in 0..value_size as u8 {
                            ctxt.add_inst(Instruction {
                                opcode: Opcode::Pop8,
                                resolved: None,
                                source: format!("reading value from stack {:?}", &self),
                                args: vec![Value::Register(reg.0 + i)]
                            });
                            ctxt.additional_offset -= 1;
                        }
                    }
                    EmitAddressResult::AddressInReg0{ptr_type: ptr_to_stack_type} => {
                        match ptr_to_stack_type {
                            Type::Ptr(inner) => assert_eq!(inner.as_ref(), &value_type),
                            _ => panic!("expected pointer")
                        };
                        assert_eq!(start_offset + value_size, ctxt.additional_offset);
    
                        for r in 0..value_size as u8 {
                            ctxt.add_inst(Instruction {
                                opcode: Opcode::Pop8,
                                resolved: None,
                                source: format!("reading value from stack {:?}", &self),
                                args: vec![Value::Register(4 + r)]
                            });
                            ctxt.additional_offset -= 1;
                        }
    
                        assert_eq!(start_offset, ctxt.additional_offset);
    
                        match value_size {
                            1 => {
                                ctxt.add_inst(Instruction {
                                    opcode: Opcode::Store8,
                                    resolved: None,
                                    source: format!("{:?}", &self),
                                    args: vec![Value::Register(4), Value::Register(0)]
                                });
                            }
                            4 => {
                                ctxt.add_inst(Instruction {
                                    opcode: Opcode::Store32Part1,
                                    resolved: None,
                                    source: format!("{:?}", &self),
                                    args: vec![Value::Register(4), Value::Register(0)]
                                });
                                ctxt.add_inst(Instruction {
                                    opcode: Opcode::Store32Part2,
                                    resolved: None,
                                    source: format!("{:?}", &self),
                                    args: vec![]
                                });
                            }
                            s => panic!("Unexpected size {}", s)
                        }
                    }
                }
            },
            Statement::Return{ value } => {
                let expression_type = value.emit(ctxt);
                let byte_count = expression_type.byte_count(ctxt.program);
                assert!(byte_count <= 4);
                for i in 0..byte_count {
                    let r = i.try_into().unwrap();
                    ctxt.add_inst(Instruction {
                        opcode: Opcode::Pop8,
                        resolved: None,
                        source: format!("{:?}", &self),
                        args: vec![Value::Register(r)]
                    });
                    ctxt.additional_offset -= 1;
                }

                let return_var = ctxt.find_var(RESULT);
                let return_type = return_var.var_type.clone();

                assert_eq!(return_type, expression_type);

                match return_var.storage {
                    Storage::FixedAddress(_) => panic!(),
                    Storage::Register(r) => {
                        assert_eq!(r.0, 0);
                    },
                    Storage::Stack(offset) => {
                        let result_offset = ctxt.get_stack_offset(offset);
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::LoadImm32,
                            resolved: None,
                            source: format!("{:?}", &self),
                            args: vec![Value::Register(4), Value::Constant32(result_offset as u32)]
                        });
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::Add32NoCarryIn,
                            resolved: None,
                            source: format!("{:?}", &self),
                            args: vec![Value::Register(REG_SP),Value::Register(4), Value::Register(8)]
                        });
        
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::Store32Part1,
                            resolved: None,
                            source: format!("{:?}", &self),
                            args: vec![Value::Register(0), Value::Register(8)]
                        });
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::Store32Part2,
                            resolved: None,
                            source: format!("{:?}", &self),
                            args: vec![]
                        });
                    }
                };
        
                ctxt.add_inst(Instruction {
                    opcode: Opcode::JmpImm,
                    source: format!("{:?}", &self),
                    args: vec![Value::Label24(format!(":{}__{}", &ctxt.function.def.name, EPILOGUE))],
                    resolved: None                    
                });
            },
            Statement::CallAssign{ name: local, var_type, call } => { 

                let start_offset = ctxt.additional_offset;

                let return_type = call.emit(ctxt).clone();

                // result is now in r0..
                // stack is now back to normal
                assert_eq!(start_offset, ctxt.additional_offset);

                let var_type = match (var_type, &return_type) {
                    (None, Type::Void) => {
                        &Type::Void
                    }
                    (Some(_), Type::Void) => {
                        panic!("Function `{}` does not return a value.", &call.function);
                    }
                    (Some(explicit), return_type) => {
                        assert_eq!(explicit, return_type);
                        explicit
                    }
                    (None, return_type) => {
                        return_type
                    }
                };

                let byte_count = match var_type {
                    Type::Void => 0,
                    t => t.byte_count(ctxt.program),
                };
                
                if byte_count != 0 {
                    match local {
                        None => {
                            // nothing to do
                        }
                        Some(local) => {
                            let local = ctxt.find_var(local);
                            let store_reg0_to_addr_in_reg8 = match local.storage {
                                Storage::Register(r) => {
                                    match byte_count {
                                        1 => {
                                            ctxt.add_inst(Instruction {
                                                opcode: Opcode::Or8,
                                                resolved: None,
                                                source: format!("save call result in target register {:?}", &self),
                                                args: vec![Value::Register(0), Value::Register(0), Value::Register(r.0)]
                                            });
                                        }
                                        4 => {
                                            ctxt.add_inst(Instruction {
                                                opcode: Opcode::Or32,
                                                resolved: None,
                                                source: format!("save call result in target register {:?}", &self),
                                                args: vec![Value::Register(0), Value::Register(0), Value::Register(r.0)]
                                            });
                                        }
                                        _ => unimplemented!(),
                                    }
                                    false
                                }
                                Storage::FixedAddress(addr) => {
                                    ctxt.add_inst(Instruction {
                                        opcode: Opcode::LoadImm32,
                                        resolved: None,
                                        source: format!("load fixed address {:?}", &self),
                                        args: vec![Value::Register(8), Value::Constant32(addr)]
                                    });
                                    true
                                }
                                Storage::Stack(offset) => {
                                    let offset = ctxt.get_stack_offset(offset);
                                    ctxt.add_inst(Instruction {
                                        opcode: Opcode::LoadImm32,
                                        resolved: None,
                                        source: format!("load stack offset for call result {:?}", &self),
                                        args: vec![Value::Register(4), Value::Constant32(offset)]
                                    });
                                    ctxt.add_inst(Instruction {
                                        opcode: Opcode::Add32NoCarryIn,
                                        resolved: None,
                                        source: format!("add stack pointer to offset {:?}", &self),
                                        args: vec![Value::Register(REG_SP),Value::Register(4),Value::Register(8)]
                                    });
                                    true
                                }
                            };

                            if store_reg0_to_addr_in_reg8 {
                                match byte_count {
                                    1 => {
                                        ctxt.add_inst(Instruction {
                                            opcode: Opcode::Store8,
                                            resolved: None,
                                            source: format!("{:?}", &self),
                                            args: vec![Value::Register(0), Value::Register(8)]
                                        });
                                    }
                                    4 => {
                                        ctxt.add_inst(Instruction {
                                            opcode: Opcode::Store32Part1,
                                            resolved: None,
                                            source: format!("{:?}", &self),
                                            args: vec![Value::Register(0), Value::Register(8)]
                                        });
                                        ctxt.add_inst(Instruction {
                                            opcode: Opcode::Store32Part2,
                                            resolved: None,
                                            source: format!("{:?}", &self),
                                            args: vec![]
                                        });
                                    }
                                    s => panic!("Unexpected size {}", s)
                                }
                            }
                        }
                    }
                }
            },
            Statement::While{predicate, while_true} => {
                let source = format!("WHILE ({:?})  ... ", predicate);

                let while_start = format!(":{}_WHILE_START_{}", &ctxt.function.def.name, ctxt.block_counter);
                let while_body = format!(":{}_WHILE_BODY_{}", &ctxt.function.def.name, ctxt.block_counter);
                let while_end = format!(":{}_WHILE_END_{}", &ctxt.function.def.name, ctxt.block_counter);
                ctxt.block_counter += 1;

                let start_offset = ctxt.additional_offset;

                ctxt.lines.push(AssemblyInputLine::Label(while_start.to_owned()));
                predicate.emit_branch(ctxt, &while_body, &while_end);
                assert_eq!(start_offset, ctxt.additional_offset);
                ctxt.lines.push(AssemblyInputLine::Label(while_body.to_owned()));
                for s in while_true {
                    s.emit(ctxt);
                    // assert_eq!(start_offset, ctxt.additional_offset);
                }
                // assert_eq!(start_offset, ctxt.additional_offset);
                ctxt.add_inst(Instruction {
                    opcode: Opcode::JmpImm,
                    source: source.to_owned(),
                    args: vec![Value::Label24(while_start.to_owned())],
                    resolved: None,
                });
                ctxt.lines.push(AssemblyInputLine::Label(while_end.to_owned()));
            },
            Statement::IfElse{predicate, when_true, when_false} => {
                let source = format!("IF ({:?})  ... ", predicate);

                let true_start = format!(":{}_IF_TRUE_START_{}", &ctxt.function.def.name, ctxt.block_counter);
                let false_start = format!(":{}_IF_FAlSE_START_{}", &ctxt.function.def.name, ctxt.block_counter);
                let block_end = format!(":{}_IF_END_{}", &ctxt.function.def.name, ctxt.block_counter);
                ctxt.block_counter += 1;

                let start_offset = ctxt.additional_offset;

                predicate.emit_branch(ctxt, &true_start, &false_start);              

                ctxt.lines.push(AssemblyInputLine::Label(true_start.to_owned()));

                let true_start_stack_offset = ctxt.additional_offset;
                assert_eq!(start_offset, true_start_stack_offset);

                for s in when_true {
                    // dbg!(s);
                    s.emit(ctxt);
                    assert_eq!(start_offset, ctxt.additional_offset);
                }

                let true_end_stack_offset = ctxt.additional_offset;

                ctxt.add_inst(Instruction {
                    opcode: Opcode::JmpImm,
                    source: source.to_owned(),
                    args: vec![Value::Label24(block_end.to_owned())],
                    resolved: None,
                });

                ctxt.lines.push(AssemblyInputLine::Label(false_start.to_owned()));
                
                for s in when_false {
                    s.emit(ctxt);
                    assert_eq!(start_offset, ctxt.additional_offset);
                }

                let false_end_stack_offset = ctxt.additional_offset;
                assert_eq!(true_end_stack_offset, false_end_stack_offset);

                ctxt.add_inst(Instruction {
                    opcode: Opcode::JmpImm,
                    source: source.to_owned(),
                    args: vec![Value::Label24(block_end.to_owned())],
                    resolved: None,
                });

                ctxt.lines.push(AssemblyInputLine::Label(block_end.to_owned()));
            },
        }
        ctxt.lines.push(AssemblyInputLine::Comment(format!("Done  statement {:?}", self)));
    }
}
