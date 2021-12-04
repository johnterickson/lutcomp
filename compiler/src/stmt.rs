use super::*;

#[derive(Clone, Debug)]
pub enum Statement {
    Declare { scope: Scope, name: String, var_type: Type },
    Assign { target: Expression, var_type: Option<Type>, value: Expression},
    VoidExpression { expression: Expression },
    IfElse {predicate: Comparison, when_true: Vec<Statement>, when_false: Vec<Statement> },
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
            Statement::IfElse { predicate, when_true, when_false } => {
                while predicate.left.optimize(ctxt) { optimized = true; }
                while predicate.right.optimize(ctxt) { optimized = true; }
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
            Rule::if_else_statement => {
                let mut pairs = pair.into_inner();
                let predicate = match Expression::parse(pairs.next().unwrap()) {
                    Expression::Comparison(c) => *c,
                    _ => panic!()
                };
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

    pub fn emit(&self, ctxt: &mut FunctionContext) -> () {
        ctxt.lines.push(AssemblyInputLine::Comment(format!("Begin statement {:?}", self)));
        // eprintln!("Emitting statement: {:?}", &self);
        match self {
            Statement::TtyOut{value} => {
                let (actual_reg, actual_type) = value.emit(ctxt, Some(Register(0)));
                assert_eq!(actual_type.byte_count(ctxt.program), 1);
                if let Some(reg) = actual_reg {
                    ctxt.add_inst(Instruction {
                        opcode: Opcode::TtyOut,
                        resolved: None,
                        source: format!("{:?}", &self),
                        args: vec![reg.into()]
                    });
                } else {
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
                }
            },
            Statement::Declare{scope:_, name:_, var_type:_} => {}
            Statement::VoidExpression { expression} => {
                let (actual_reg, returned_type) = expression.emit(ctxt, None);
                assert_eq!(actual_reg, None);
                if let Some(_) = actual_reg {
                    // nothing to pop off stack
                } else {
                    let bytes_to_discard = returned_type.byte_count(ctxt.program);
                    if bytes_to_discard != 0 {
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::AddImm32IgnoreCarry,
                            source: format!("{:?} discarding unneeded value", &self),
                            args: vec![Value::Register(REG_SP), Value::Constant32(bytes_to_discard)],
                            resolved: None,
                        });
                    }
                }
            }
            Statement::Assign{target, var_type, value} => {
                let emit_addr_result = target.try_emit_address(ctxt, Register(0));
                let (target_value_reg, type_in_reg)  = match emit_addr_result {
                    EmitAddressResult::ValueInRegister{reg, value_type} => {
                        if let Some(explicit_type) = var_type {
                            assert_eq!(explicit_type, &value_type);
                        }
                        (Some(reg), value_type)
                    }
                    EmitAddressResult::AddressInRegister{reg: address_reg, ptr_type} => {
                        for r in (0..4).rev() {
                            let r = r + address_reg.0;
                            ctxt.add_inst(Instruction {
                                source: format!("storing address on stack {:?}", &self),
                                opcode: Opcode::Push8,
                                args: vec![Value::Register(r)],
                                resolved: None,
                            });
                            ctxt.additional_offset += 1;
                        }
                        (None, ptr_type)
                    }
                };

                let (actual_value_reg, value_type) = value.emit(ctxt, target_value_reg); // calculate value

                if let Some(explicit_type) = var_type {
                    assert_eq!(explicit_type, &value_type);
                }

                let value_size = value_type.byte_count(ctxt.program);
                assert!(value_size <= 4);

                match (target_value_reg, actual_value_reg) {
                    (Some(target_value_reg), Some(actual_value_reg)) if target_value_reg == actual_value_reg => {
                        assert_eq!(type_in_reg, value_type);
                    },
                    (None, None) => {
                        for i in 0..value_size as u8 {
                            ctxt.add_inst(Instruction {
                                opcode: Opcode::Pop8,
                                resolved: None,
                                source: format!("reading value from stack {:?}", &self),
                                args: vec![Value::Register(4+i)]
                            });
                            ctxt.additional_offset -= 1;
                        }
                        for i in 0..4 {
                            ctxt.add_inst(Instruction {
                                opcode: Opcode::Pop8,
                                resolved: None,
                                source: format!("reading addr from stack {:?}", &self),
                                args: vec![Value::Register(i)]
                            });
                            ctxt.additional_offset -= 1;
                        }
                        
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
                                    opcode: Opcode::Store32_1,
                                    resolved: None,
                                    source: format!("{:?}", &self),
                                    args: vec![Value::Register(4), Value::Register(0)]
                                });
                                ctxt.add_inst(Instruction {
                                    opcode: Opcode::Store32_2,
                                    resolved: None,
                                    source: format!("{:?}", &self),
                                    args: vec![]
                                });
                            }
                            s => panic!("Unexpected size {}", s)
                        }
                    }
                    _ => unimplemented!(),
                }
            },
            Statement::Return{ value } => {

                let (return_storage, return_type) = {
                    let return_var = ctxt.find_var(RESULT);
                    (return_var.storage.clone(), return_var.var_type.clone())
                };

                if let Some(value) = value {
                    assert_ne!(return_type, Type::Void);

                    let byte_count = return_type.byte_count(ctxt.program);
                    assert!(byte_count <= 4);

                    let target_reg = match return_storage {
                        Storage::FixedAddress(_) => panic!(),
                        Storage::Register(r) => {
                            assert_eq!(r.0, 0);
                            Some(r)
                        },
                        Storage::Stack(_) => None,
                    };

                    let (actual_reg, expression_type) = value.emit(ctxt, target_reg);
                    assert_eq!(return_type, expression_type);
                    assert_eq!(actual_reg, target_reg);

                    match (target_reg, actual_reg, return_storage) {
                        (Some(target_reg), Some(actual_reg), Storage::Register(final_reg))
                            if target_reg == actual_reg && actual_reg == final_reg => {}
                        (None, None, Storage::Stack(offset)) => {
                            for i in 0..byte_count {
                                let r = i.try_into().unwrap();
                                ctxt.add_inst(Instruction {
                                    opcode: Opcode::Pop8,
                                    resolved: None,
                                    source: format!("{:?}", &self),
                                    args: vec![Value::Register(r)]
                                });
                                ctxt.additional_offset -= 1;

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
                                    opcode: Opcode::Store32_1,
                                    resolved: None,
                                    source: format!("{:?}", &self),
                                    args: vec![Value::Register(0), Value::Register(8)]
                                });
                                ctxt.add_inst(Instruction {
                                    opcode: Opcode::Store32_2,
                                    resolved: None,
                                    source: format!("{:?}", &self),
                                    args: vec![]
                                });
                            }
                        }
                        _ => panic!(),
                    }

                } else {
                    assert_eq!(return_type, Type::Void);
                }
                
                ctxt.add_inst(Instruction {
                    opcode: Opcode::JmpImm,
                    source: format!("{:?}", &self),
                    args: vec![Value::Label24(format!(":{}__{}", &ctxt.function.def.name, EPILOGUE))],
                    resolved: None                    
                });
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
