#![allow(dead_code)]

use std::{collections::BTreeMap, convert::TryFrom, fmt::Debug, hash::Hash, ops::Range};

use crate::*;

pub fn emit_il(entry: &str, input: &str, root: &Path) -> (ProgramContext, IlProgram) {
    let ctxt = create_program(entry, input, root);
    
    let p =  IlProgram::from_program(&ctxt);
    (ctxt, p)
}


#[derive(Clone)]
pub enum IlInstruction {
    Comment(String),
    Label(IlLabelId),
    AssignNumber{ dest: IlVarId, src: IlNumber },
    AssignVar{ dest: IlVarId, src: IlVarId, size: IlType, src_range: Option<Range<u32>>, dest_range: Option<Range<u32>>},
    AssignUnary{ dest: IlVarId, op: IlUnaryOp, src: IlVarId },
    AssignBinary { dest: IlVarId, op: IlBinaryOp, src1: IlVarId, src2: IlVarId },
    ReadMemory {dest: IlVarId, addr: IlVarId, size: IlType},
    WriteMemory {addr: IlVarId, src: IlVarId, size: IlType},
    Goto(IlLabelId),
    IfThenElse {left: IlVarId, op: IlCmpOp, right: IlVarId, then_label: IlLabelId, else_label: IlLabelId},
    Call {ret: Option<IlVarId>, f: IlFunctionId, args: Vec<IlVarId> },
    Resize {dest: IlVarId, dest_size: IlType, src:IlVarId, src_size: IlType},
    Return { val: Option<IlVarId> },
    TtyIn { dest: IlVarId },
    TtyOut { src: IlVarId },
    Unreachable,
}



#[derive(Clone, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct IlVarId(pub String);

impl IlVarId {
    pub fn frame_pointer() -> &'static str {
        "__frame_pointer"
    }
}

impl Debug for IlVarId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"{}", self.0)
    }
}

#[derive(Clone, Debug)]
pub struct IlVarInfo {
    pub description: String,
    pub location: IlLocation,
    pub var_type: Type,
    pub byte_size: u32,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct IlLabelId(pub String);

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct IlFunctionId(pub String);

#[derive(Clone, Debug)]
pub struct IlFunction {
    pub id: IlFunctionId,
    pub args: Vec<IlVarId>,
    pub body: Vec<IlInstruction>,
    pub vars: BTreeMap<IlVarId, IlVarInfo>,
    pub ret: Option<IlType>,
    labels: BTreeSet<IlLabelId>,
    next_temp_num: usize,
    next_label_num: usize,
    pub vars_stack_size: u32, 
}

#[derive(Debug)]
struct TargetLocation {
    target: IlVarId,
    target_subrange: Option<Range<u32>>,
    mem_size: Option<IlType>
}

impl IlFunction {
    fn new(id: IlFunctionId) -> IlFunction {
        IlFunction {
            id, 
            args: Vec::new(),
            vars: BTreeMap::new(),
            body: Vec::new(),
            labels: BTreeSet::new(),
            next_temp_num: 0,
            next_label_num: 0,
            vars_stack_size: 0,
            ret: None,
        }
    }

    fn alloc_tmp(&mut self, info: IlVarInfo) -> IlVarId {
        let id = IlVarId(format!("t{}", self.next_temp_num));
        self.vars.insert(id.clone(), info);
        self.next_temp_num += 1;
        id
    }

    fn alloc_tmp_for_expression(&mut self, ctxt: &IlContext, e: &Expression) -> (IlVarId, IlVarInfo) {
        let id = IlVarId(format!("t{}", self.next_temp_num));
        let t = e.try_emit_type(ctxt.program, Some(ctxt.func_def))
            .expect(&format!("Could not determine type for '{:?}'.", e));
        let location = match t.byte_count(ctxt.program) {
            0 | 1 => IlLocation::U8,
            4 => IlLocation::U32,
            _ => panic!(),
        };
        let var_type = e.try_emit_type(ctxt.program, Some(ctxt.func_def)).unwrap();
        let info = IlVarInfo {
            description: format!("{} {:?}", &id.0, e),
            byte_size: var_type.byte_count(ctxt.program),
            var_type,
            location,
        };
        self.vars.insert(id.clone(), info);
        self.next_temp_num += 1;
        let (id, info) = self.vars.get_key_value(&id).unwrap();
        (id.clone(), info.clone())
    }

    fn alloc_tmp_and_emit_value(&mut self, ctxt: &IlContext, e: &Expression) -> (IlVarId, IlVarInfo) {
        let (id, info) = self.alloc_tmp_for_expression(ctxt, e);
        self.emit_expression(ctxt, id.clone(), e);
        (id, info)
    }

    fn alloc_named(&mut self, name: String, info: IlVarInfo) -> IlVarId {
        let id = IlVarId(name);
        self.vars.insert(id.clone(), info);
        id
    }

    fn alloc_label(&mut self, info: &str) -> IlLabelId {
        let id = IlLabelId(format!("{}_{}_{}", self.id.0, info, self.next_label_num));
        self.next_label_num += 1;
        assert!(self.labels.insert(id.clone()));
        id
    }

    fn alloc_tmp_and_emit_static_address(&mut self, ctxt: &IlContext, name: &str, info: &IlVarInfo) -> IlVarId {
        let addr = match info.location {
            IlLocation::Static(addr) => addr,
            _ => panic!(),
        };
        let var_type = Type::Ptr(Box::new(info.var_type.clone()));
        let addr_var = self.alloc_tmp(IlVarInfo {
            description: format!("static {:?}", &name),
            location: IlLocation::U32,
            byte_size: var_type.byte_count(ctxt.program),
            var_type
        });
        self.body.push(IlInstruction::AssignNumber {
            dest: addr_var.clone(),
            src: IlNumber::U32(addr),
        });
        addr_var
    }

    fn emit_address(&mut self, ctxt: &IlContext, value: &Expression) -> (IlVarId, Option<IlType>) {
        match value {
            Expression::Ident(n) => {
                let info = ctxt.find_arg_or_var(&self, n);
                match info.location {
                    IlLocation::U8 | IlLocation::U32 => panic!(),
                    IlLocation::FrameOffset(offset) => {
                        let addr = Expression::Arithmetic(
                            ArithmeticOperator::Add,
                            Box::new(Expression::Ident(IlVarId::frame_pointer().to_owned())),
                            Box::new(Expression::Number(NumberType::USIZE, offset))
                        );
                        let (id, _) = self.alloc_tmp_and_emit_value(ctxt, &addr);
                        (id, None)
                    },
                    IlLocation::Static(_) => {
                        let size = info.var_type.byte_count(ctxt.program).try_into().ok();
                        let id = self.alloc_tmp_and_emit_static_address(ctxt, n, &info);
                        (id, size)
                    }
                }
            }
            Expression::Index(var, index) => {
                let info = ctxt.find_arg_or_var(&self, var);
                let ptr_type = info.var_type;
                let element_type = ptr_type.get_element_type().unwrap();
                let element_size = element_type.byte_count(ctxt.program);

                let index_type = ctxt.try_emit_type(index).unwrap();
                let index = match index_type {
                    Type::Number(nt) => {
                        match nt {
                            NumberType::U8 => {
                                Box::new(Expression::Cast {
                                    old_type: Some(index_type),
                                    new_type: Type::Number(NumberType::USIZE),
                                    value: index.clone()
                                })
                            },
                            NumberType::USIZE => {
                                index.clone()
                            },
                        }
                    }
                    _ => panic!(),
                };

                let byte_index_expression = if element_size == 1 {
                    index.clone()
                } else {
                    Box::new(Expression::Arithmetic(
                        ArithmeticOperator::Multiply,
                        index.clone(),
                        Box::new(Expression::Number(NumberType::USIZE, element_size))
                    ))
                };


                let base_addr_num_expression = match info.location {
                    IlLocation::Static(addr) => Expression::Number(NumberType::USIZE, addr),
                    IlLocation::U32 | IlLocation::U8 => {
                        Expression::Cast {
                            old_type: Some(ptr_type.clone()),
                            new_type: Type::Number(NumberType::USIZE),
                            value: Box::new(Expression::Ident(var.clone())),
                        }
                    },
                    IlLocation::FrameOffset(offset) => {
                        let frame = Expression::Ident(IlVarId::frame_pointer().to_owned());
                        if offset == 0 {
                            frame
                        } else {
                            Expression::Arithmetic(
                                ArithmeticOperator::Add,
                                Box::new(frame),
                                Box::new(Expression::Number(NumberType::USIZE, offset))
                            )
                        }
                    },
                };

                let addr_expression = Expression::Arithmetic(
                    ArithmeticOperator::Add,
                    Box::new(base_addr_num_expression),
                    byte_index_expression,
                );

                let addr_expression = Expression::Cast{
                    value: Box::new(addr_expression),
                    old_type: Some(Type::Number(NumberType::USIZE)),
                    new_type: Type::Ptr(Box::new(element_type.clone())),
                };

                let (addr, _) = self.alloc_tmp_and_emit_value(ctxt, &addr_expression);

                (addr, Some(element_size.try_into().unwrap()))
            }
            Expression::LocalFieldDeref(n, field) => {
                let info = ctxt.find_arg_or_var(&self, n);
                let struct_type = match &info.var_type { 
                    Type::Struct(struct_type) => {
                        &ctxt.program.types[struct_type]
                    }
                    _ => panic!()
                };
                let (byte_offset, field_type) = struct_type.get_field(field);
                let field_bytes = field_type.byte_count(ctxt.program).try_into().unwrap();

                let base_expression = match info.location {
                    IlLocation::Static(addr) => Expression::Number(NumberType::USIZE, addr),
                    IlLocation::U8 | IlLocation::U32 => todo!(),
                    IlLocation::FrameOffset(offset) => {
                        let frame = Expression::Ident(IlVarId::frame_pointer().to_owned());
                        if offset == 0 {
                            frame
                        } else {
                            Expression::Arithmetic(
                                ArithmeticOperator::Add,
                                Box::new(frame),
                                Box::new(Expression::Number(NumberType::USIZE, offset)))
                        }
                    },
                };

                let addr_expression = if byte_offset == 0 {
                    base_expression
                } else {
                    Expression::Arithmetic(
                        ArithmeticOperator::Add,
                        Box::new(base_expression),
                        Box::new(Expression::Number(NumberType::USIZE, byte_offset)),
                    )
                };

                let addr_expression = Expression::Cast {
                    value: Box::new(addr_expression),
                    old_type: Some(Type::Number(NumberType::USIZE)),
                    new_type: Type::Ptr(Box::new(field_type.clone()))
                };

                let (addr, _) = self.alloc_tmp_and_emit_value(ctxt, &addr_expression);

                (addr, Some(field_bytes))
            }
            Expression::PtrFieldDeref(n, field) => {
                let info = ctxt.find_arg_or_var(&self, n);
                let ptr_type = info.var_type;
                let element_type = ptr_type.get_element_type().unwrap();
                let struct_type = match element_type { 
                    Type::Struct(struct_type) => {
                        &ctxt.program.types[struct_type]
                    }
                    _ => panic!()
                };
                let (byte_offset, field_type) = struct_type.get_field(field);
                let field_bytes = field_type.byte_count(ctxt.program).try_into().ok();

                let base_expression = match info.location {
                    IlLocation::Static(_) => {
                        todo!()
                    }
                    IlLocation::U8 | IlLocation::U32 => {
                        Expression::Cast {
                            old_type: Some(ptr_type.clone()),
                            new_type: Type::Number(NumberType::USIZE),
                            value: Box::new(Expression::Ident(n.clone())),
                        }
                    },
                    IlLocation::FrameOffset(_) => todo!(),
                };

                let addr_expression = if byte_offset == 0 {
                    base_expression
                } else {
                    Expression::Arithmetic(
                        ArithmeticOperator::Add,
                        Box::new(base_expression),
                        Box::new(Expression::Number(NumberType::USIZE, byte_offset)),
                    )
                };

                let addr_expression = Expression::Cast {
                    value: Box::new(addr_expression),
                    old_type: Some(Type::Number(NumberType::USIZE)),
                    new_type: Type::Ptr(Box::new(field_type.clone()))
                };

                let (addr, _) = self.alloc_tmp_and_emit_value(ctxt, &addr_expression);

                (addr, field_bytes)
            }
            _ => todo!(),
        }
    }

    fn emit_target_expression(&mut self, ctxt: &IlContext, target: &Expression) -> TargetLocation {
        // dbg!(target);
        match target {
            Expression::Ident(n) => {
                let info= ctxt.find_arg_or_var(&self, n);
                match info.location {
                    IlLocation::U8 | IlLocation::U32 => {
                        TargetLocation {
                            target: IlVarId(n.clone()),
                            target_subrange: None,
                            mem_size: None
                        }
                    },
                    IlLocation::FrameOffset(_) | IlLocation::Static(_) => {
                        let (address, size) = self.emit_address(ctxt,  target);
                        TargetLocation {
                            target: address,
                            target_subrange: None,
                            mem_size: Some(size.unwrap())
                        }
                    },
                }
            },
            Expression::Deref(e) => {
                let (addr, info) = self.alloc_tmp_and_emit_value(ctxt, e);
                let element_type = match info.var_type {
                    Type::Ptr(t) |Type::Array(t, _) => t,
                    _ => panic!(),
                };
                let size = element_type.byte_count(ctxt.program).try_into().unwrap();
                TargetLocation {
                    target: addr,
                    target_subrange: None,
                    mem_size: Some(size)
                }
            },
            Expression::LocalFieldDeref(_,_) |
            Expression::PtrFieldDeref(_,_) => {
                let (address, size) = self.emit_address(ctxt,  target);
                TargetLocation {
                    target: address,
                    target_subrange: None,
                    mem_size: Some(size.unwrap())
                }
            }
            Expression::Index(n, index) => {
                let info = ctxt.find_arg_or_var(&self, n);

                match (info.location, info.var_type) {
                    (IlLocation::U32, Type::Number(NumberType::USIZE)) => {
                        let index = index.try_get_const();
                        match (info.location, index) {
                            (IlLocation::U32, Some(index)) => {
                                TargetLocation {
                                    target: IlVarId(n.clone()),
                                    target_subrange: Some(index..(index+1)),
                                    mem_size: None,
                                }
                            }
                            _ => todo!(),
                        }
                    }
                    (_, Type::Ptr(..)) | (_, Type::Array(..)) =>  {
                        let (address, size) = self.emit_address(ctxt,  target);
                        TargetLocation {
                            target: address,
                            target_subrange: None,
                            mem_size: Some(size.unwrap()),
                        }
                    }
                    other => panic!("Don't know how to index into {:?}", other)
                }
            },
            Expression::Optimized { original:_, optimized } => 
                self.emit_target_expression(ctxt, optimized),
            _ => panic!(),
        }
    }

    fn emit_comparison(&mut self, ctxt: &IlContext, c: &Comparison, true_label: IlLabelId, false_label: IlLabelId) {
        let (left, left_info) = self.alloc_tmp_and_emit_value(ctxt, &c.left);
        let (right, right_info) = self.alloc_tmp_and_emit_value(ctxt, &c.right);

        assert_eq!(left_info.var_type, right_info.var_type);

        self.body.push(IlInstruction::IfThenElse{
            left: left.clone(),
            op: c.op.into(), 
            right: right.clone(),
            then_label: true_label.clone(),
            else_label: false_label.clone(),
        });
    }
    
    fn emit_statement(&mut self, ctxt: &mut IlContext, s: &Statement) {
        //dbg!(s);
        match s {
            Statement::Declare { .. } => {},
            Statement::Assign { target, var_type, value } => {
                let emitted_type = match self.emit_target_expression(ctxt, target) {
                    TargetLocation {target, target_subrange: None, mem_size: Some(size)} => {
                        let (value_reg, info) = self.alloc_tmp_and_emit_value(ctxt, value);
                        assert_eq!(size.byte_count(), info.var_type.byte_count(ctxt.program));
                        self.body.push(IlInstruction::WriteMemory{addr: target, src: value_reg.clone(), size});
                        info.var_type
                    }
                    TargetLocation {target, target_subrange: None, mem_size: None} => {
                        self.emit_expression(ctxt, target.clone(), value);
                        self.vars[&target].var_type.clone()
                    }
                    TargetLocation {target, target_subrange: Some(dest_subrange), mem_size: None} => {
                        let size: IlType = (dest_subrange.end - dest_subrange.start).try_into().unwrap();
                        let (tmp, tmp_info) = self.alloc_tmp_and_emit_value(ctxt, value);
                        assert_eq!(size.byte_count(), tmp_info.byte_size);

                        self.body.push(IlInstruction::AssignVar {
                            dest: target,
                            dest_range: Some(dest_subrange),
                            src: tmp,
                            src_range: None,
                            size,
                        });

                        tmp_info.var_type
                    }
                    other => panic!("Don't know how to assign {:?}", other)
                };

                if let Some(var_type) = var_type {
                    assert_eq!(var_type, &emitted_type);
                }
            },
            Statement::VoidExpression { expression } => {
                let (_, _) = self.alloc_tmp_and_emit_value(ctxt, expression);
            },
            Statement::IfElse { if_blocks, else_block } => {
                assert!(if_blocks.len() >= 1);

                let end_label= self.alloc_label("if end");

                let mut prev_false = None;
                for (i, (predicate, when_true)) in if_blocks.iter().enumerate() {
                    if let Some(prev_false) = prev_false.take() {
                        self.body.push(IlInstruction::Label(prev_false));
                    }
                    let true_label = self.alloc_label(&format!("if true {}", i));
                    let false_label = self.alloc_label(&format!("if else {}", i));
                    self.emit_comparison(ctxt, predicate, true_label.clone(), false_label.clone());
                    self.body.push(IlInstruction::Label(true_label));
                    for s in when_true {
                        self.emit_statement(ctxt, s);
                    }
                    self.body.push(IlInstruction::Goto(end_label.clone()));
                    prev_false = Some(false_label);
                }

                self.body.push(IlInstruction::Label(prev_false.unwrap()));
                for s in else_block {
                    self.emit_statement(ctxt, s);
                }

                self.body.push(IlInstruction::Label(end_label.clone()));
            },
            Statement::While { predicate, while_true } => {
                let pred_label = self.alloc_label("while predicate");
                let body_label= self.alloc_label("while body");
                let end_label= self.alloc_label("while end");

                self.body.push(IlInstruction::Label(pred_label.clone()));

                self.emit_comparison(ctxt, predicate, body_label.clone(), end_label.clone());

                self.body.push(IlInstruction::Label(body_label.clone()));

                for s in while_true {
                    self.emit_statement(ctxt, s);
                }

                self.body.push(IlInstruction::Goto(pred_label.clone()));

                self.body.push(IlInstruction::Label(end_label.clone()));
            },
            Statement::Return { value } => {
                let val = if let Some(value) = value {
                    let (val,_) = self.alloc_tmp_and_emit_value(ctxt, value);
                    Some(val.clone())
                } else {
                    None
                };
                
                self.body.push(IlInstruction::Return{val})
            },
            Statement::TtyOut { value } => {
                let (value_reg, _) = self.alloc_tmp_and_emit_value(ctxt, &value);
                self.body.push(IlInstruction::TtyOut{src: value_reg.clone()})
            },
        }
    }

    fn emit_expression(&mut self, ctxt: &IlContext, dest: IlVarId, e: &Expression) {
        // println!("START {:?}", e);
        match e {
            Expression::Ident(name) => {
                let info = ctxt.find_arg_or_var(&self, name);
                match info.location {
                    IlLocation::U8 | IlLocation::U32 => {
                        let src = IlVarId(name.clone());
                        let size = info.location.try_into().unwrap();
                        self.body.push(IlInstruction::AssignVar{dest, src, size, src_range: None, dest_range: None});
                    }
                    IlLocation::FrameOffset(_) => todo!(),
                    IlLocation::Static(_) => {
                        let addr = self.alloc_tmp_and_emit_static_address(ctxt, name, &info);

                        self.body.push(IlInstruction::ReadMemory {
                            dest,
                            addr,
                            size: info.byte_size.try_into().unwrap(),
                        })
                    },
                }
            },
            Expression::Number(num_type, val) => {
                let src = match num_type {
                    NumberType::U8 => IlNumber::U8((*val).try_into().unwrap()),
                    NumberType::USIZE => IlNumber::U32(*val)
                };
                self.body.push(IlInstruction::AssignNumber{dest, src});
            },
            Expression::TtyIn() => {
                self.body.push(IlInstruction::TtyIn {dest});
            },
            Expression::Arithmetic(op, left, right) => {

                let left_type = ctxt.try_emit_type(left).expect(&format!("Could not find type for {:?}.", left));
                let left_type = left_type.get_number_type().expect(&format!("Expected Number, but is {:?}.", left_type));
                let right_type = ctxt.try_emit_type(right).unwrap().get_number_type().unwrap();

                let promo_needed = left_type != right_type;

                let promote = |side: &Box<Expression>, side_type| {
                    if promo_needed && side_type == NumberType::U8 {
                        if let Some(c) = side.try_get_const() {
                            Expression::Number(NumberType::USIZE, c)
                        } else {
                            Expression::Cast {
                                value: side.clone(),
                                old_type: Some(Type::Number(side_type)),
                                new_type: Type::Number(NumberType::USIZE)
                            }
                        }
                    } else {
                        *(side.clone())
                    }
                };

                let left = promote(left, left_type);
                let right = promote(right, right_type);

                let (left_tmp, _) = self.alloc_tmp_and_emit_value(ctxt, &left);
                let (right_tmp, _) = self.alloc_tmp_and_emit_value(ctxt, &right);
                self.body.push(IlInstruction::AssignBinary {
                    dest,
                    op: IlBinaryOp::from(op),
                    src1: left_tmp.clone(),
                    src2: right_tmp.clone(),
                });
            },
            Expression::Comparison(_) => todo!(),
            Expression::Deref(ptr) => {
                let (addr, ptr_info) = self.alloc_tmp_and_emit_value(ctxt, ptr);
                let derefed_type = ptr_info.var_type.get_element_type().unwrap();
                self.body.push(IlInstruction::ReadMemory {
                    dest,
                    addr,
                    size: derefed_type.byte_count(ctxt.program).try_into().unwrap(),
                });
            },
            Expression::LocalFieldDeref(_,_) |
            Expression::PtrFieldDeref(_,_) => {
                let (addr, size) = self.emit_address(ctxt, e);
                self.body.push(IlInstruction::ReadMemory {
                    dest,
                    addr,
                    size: size.unwrap(),
                })
            },
            Expression::AddressOf(n) => {
                let (addr, _mem_size) = self.emit_address(ctxt, n);
                self.body.push(IlInstruction::AssignVar {
                    dest,
                    src: addr,
                    size: IlType::U32,
                    src_range: None,
                    dest_range: None,
                });
            },
            Expression::Index(n, index) => {
                let info = ctxt.find_arg_or_var(&self, n);
                match (info.location, info.var_type) {
                    (IlLocation::U32, Type::Number(NumberType::USIZE)) => {
                        let index = index.try_get_const();
                        match (info.location, index) {
                            (IlLocation::U32, Some(index)) => {
                                self.body.push(IlInstruction::AssignVar {
                                    dest,
                                    dest_range: None,
                                    src: IlVarId(n.clone()),
                                    src_range: Some(index..index+1),
                                    size: IlType::U8,
                                });
                            }
                            _ => todo!(),
                        }
                    }
                    (_, Type::Ptr(..)) | (_, Type::Array(..)) =>  {
                        let (addr, size) = self.emit_address(ctxt, e);
                        self.body.push(IlInstruction::ReadMemory {
                            dest,
                            addr,
                            size: size.unwrap(),
                        });
                    }
                    other => panic!("Don't know how to index into {:?}", other)
                }
            },
            Expression::Cast { old_type, new_type, value } => {
                let old_type = old_type.as_ref().cloned().or_else(|| value.try_emit_type(ctxt.program, Some(ctxt.func_def))).unwrap();
                let (old_num, new_num) = (old_type.get_number_type(), new_type.get_number_type());
                let (old_size, new_size) = (old_type.byte_count(ctxt.program), new_type.byte_count(ctxt.program));
                match (old_size, old_num, new_size, new_num) {
                    (old_size, _, new_size, _) if old_size == new_size => {
                        self.emit_expression(ctxt, dest, value);
                    }
                    (_, Some(old_nt), _, Some(new_nt)) => {
                        let (old_var, old_var_info) = self.alloc_tmp_and_emit_value(ctxt, value);
                        assert_eq!(&old_type, &old_var_info.var_type);
                        assert_ne!(&old_nt, &new_nt);

                        self.body.push(IlInstruction::Resize {
                            src: old_var,
                            src_size: old_size.try_into().unwrap(),
                            dest,
                            dest_size: new_size.try_into().unwrap()
                        });
                    }
                    _ => todo!("{:?}", e),
                }
            },  
            Expression::Call(c) => {

                let callee_def = &ctxt.program.function_defs[&c.function];

                assert_eq!(callee_def.args.len(), c.parameters.len());

                let mut params = Vec::new();
                for ((_, arg_type), p) in callee_def.args.iter().zip(&c.parameters) {
                    let (p_value, info) = self.alloc_tmp_and_emit_value(ctxt, p);
                    assert_eq!(arg_type, &info.var_type);
                    params.push(p_value.clone());
                }

                let ret = match &callee_def.return_type {
                    Type::Void => None,
                    _ => Some(dest),
                };

                self.body.push(IlInstruction::Call {
                    f: IlFunctionId(c.function.clone()),
                    ret,
                    args: params,
                });
            },
            Expression::Optimized { .. } => todo!(),
        
        }
        // println!("END   {:?}", &e);
        // println!("{:#?}", &self.body);
    }

    fn emit_from(ctxt: &mut IlContext) -> IlFunction {
        let id = IlFunctionId(ctxt.func_def.name.clone());
        let mut func = IlFunction::new(id.clone());

        let return_size = ctxt.func_def.return_type.byte_count(ctxt.program);
        func.ret = match return_size {
            0 => None,
            1 => Some(IlType::U8),
            4 => Some(IlType::U32),
            _ => panic!(),
        };
        
        for (i, (name, var_type)) in ctxt.func_def.args.iter().enumerate() {
            let byte_size = var_type.byte_count(ctxt.program);
            let location = byte_size.try_into().unwrap();
            let id = func.alloc_named(
                name.clone(),
                IlVarInfo {
                    description: format!("Arg{} {} {:?}", i, name, var_type),
                    location,
                    var_type: var_type.clone(),
                    byte_size,
                });
            func.args.push(id);
        }

        for (name, (scope, var_type)) in ctxt.func_def.vars.iter() {
            let size = var_type.byte_count(ctxt.program);
            let size = match scope {
                Scope::Local => {
                    match var_type {
                        Type::Struct(_) | Type::Array(_, _) => {
                            let offset = func.vars_stack_size;
                            let size = (size + 3)/4*4;  // round up
                            func.vars_stack_size += size;
                            IlLocation::FrameOffset(offset)
                        },
                        _ => size.try_into().unwrap()
                    }
                },
                Scope::Static => {
                    assert!(
                        ctxt.il.statics.insert(
                            (Some(id.clone()), IlVarId(name.clone())),
                            IlVarInfo {
                                location: IlLocation::Static(ctxt.next_static_addr),
                                var_type: var_type.clone(),
                                description: format!("static {}", name),
                                byte_size: var_type.byte_count(ctxt.program),
                            }
                        ).is_none()
                    );
                    let addr = ctxt.next_static_addr;
                    ctxt.next_static_addr += size;
                    IlLocation::Static(addr)
                },
            };
            

            func.alloc_named(
                name.clone(),
                IlVarInfo {
                    location: size,
                    description: format!("Local {} {:?} {:?}", name, var_type, size),
                    var_type: var_type.clone(),
                    byte_size: var_type.byte_count(ctxt.program)
                });
        }

        if func.vars_stack_size > 0 {
            func.alloc_named(
                IlVarId::frame_pointer().to_owned(),
                IlVarInfo {
                    description: IlVarId::frame_pointer().to_owned(),
                    location: IlLocation::U32,
                    var_type: Type::Number(NumberType::USIZE),
                    byte_size: 4,
                });
        }

        for s in &ctxt.func_def.body {
            func.emit_statement(ctxt, s);
        }

        if ctxt.func_def.return_type == Type::Void {
            func.emit_statement(ctxt, &Statement::Return{value: None });
        }

        func.body.push(IlInstruction::Unreachable);

        func.optimize();
        
        func
    }

    fn optimize(&mut self) {
        while self.optimze_round() {}
    }

    fn find_refs(&mut self) -> BTreeMap<IlVarId, VarReferences> {
        let mut refs = BTreeMap::new();

        for v in self.vars.keys() {
            refs.insert(v.clone(), VarReferences { reads: BTreeSet::new(), writes: BTreeSet::new()});
        }

        for (index, usages) in self.body.iter_mut().map(|s| s.var_usages_mut()).enumerate() {
            
            if let Some(dest) = usages.dest {
                let refs = refs.get_mut(dest).unwrap();
                refs.writes.insert(index);
            }

            for src in usages.srcs {
                let refs = refs.get_mut(src).unwrap();
                refs.reads.insert(index);
            }
        }

        refs
    }

    fn optimze_round(&mut self) -> bool {
        let refs = self.find_refs();

        // dbg!(&self.body);

        for (var, refs) in &refs {
            if refs.writes.len() == 1 && refs.reads.len() == 1 {
                let (read, write) = (refs.reads.iter().next().unwrap(), refs.writes.iter().next().unwrap());
                if read != write {
                    let var_to_remove = var;

                    if let Some(write_src) = match &self.body[*write] {
                        IlInstruction::AssignVar {dest, src, size:_, src_range: None, dest_range: None } => {
                            assert_eq!(dest, var_to_remove);
                            Some(src.clone())
                        }
                        _ => None,
                    } {
                        // dbg!((&self.body[*write], &self.body[*read]));
                        for src in self.body[*read].var_usages_mut().srcs {
                            if src == var_to_remove {
                                *src = write_src.clone();
                            }
                        }
                        // dbg!(&self.body[*read]);

                        self.body.remove(*write);
                        self.vars.remove(var_to_remove);
                        return true;
                    }
                }
            }
        }

        false
    }
}

struct VarReferences {
    reads: BTreeSet<usize>,
    writes: BTreeSet<usize>,
}

#[derive(Clone, Debug)]
pub enum IlUnaryOp {
    Negate,
    BinaryInvert
}

#[derive(Clone, Debug)]
pub enum IlBinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    BitwiseAnd,
    BitwiseOr,
    LeftShift,
    RightShift,
}

impl IlBinaryOp {
    fn from(op: &ArithmeticOperator) -> IlBinaryOp {
        match op {
            ArithmeticOperator::Add => IlBinaryOp::Add,
            ArithmeticOperator::Subtract => IlBinaryOp::Subtract,
            ArithmeticOperator::Multiply => IlBinaryOp::Multiply,
            ArithmeticOperator::Divide => IlBinaryOp::Divide,
            ArithmeticOperator::Or => IlBinaryOp::BitwiseOr,
            ArithmeticOperator::And => IlBinaryOp::BitwiseAnd,
            ArithmeticOperator::LeftShift => IlBinaryOp::LeftShift,
            ArithmeticOperator::RightShift => IlBinaryOp::RightShift,
        }
    }
}

#[derive(Clone)]
pub enum IlCmpOp {
    Equals,
    NotEquals,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual
}

impl Debug for IlCmpOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Equals => write!(f, "=="),
            Self::NotEquals => write!(f, "!="),
            Self::GreaterThan => write!(f, ">"),
            Self::GreaterThanOrEqual => write!(f, ">="),
            Self::LessThan => write!(f, "<"),
            Self::LessThanOrEqual => write!(f, "<="),
        }
    }
}

impl From<ComparisonOperator> for IlCmpOp {
    fn from(op: ComparisonOperator) -> Self {
        match op {
            ComparisonOperator::Equals => IlCmpOp::Equals,
            ComparisonOperator::NotEquals => IlCmpOp::NotEquals,
            ComparisonOperator::GreaterThan => IlCmpOp::GreaterThan,
            ComparisonOperator::GreaterThanOrEqual => IlCmpOp::GreaterThanOrEqual,
            ComparisonOperator::LessThan => IlCmpOp::LessThan,
            ComparisonOperator::LessThanOrEqual => IlCmpOp::LessThanOrEqual,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IlType {
    U8,
    U32,
}

impl TryFrom<u32> for IlType {
    type Error = ();

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(IlType::U8),
            4 => Ok(IlType::U32),
            _ => Err(())
        }
    }
}

impl TryFrom<IlLocation> for IlType {
    type Error = ();

    fn try_from(value: IlLocation) -> Result<Self, Self::Error> {
        match value {
            IlLocation::U8 => Ok(IlType::U8),
            IlLocation::U32 =>Ok(IlType::U32),
            _ => Err(())
        }
    }
}

impl From<&NumberType> for IlType {
    fn from(nt: &NumberType) -> Self {
        match nt {
            NumberType::U8 => IlType::U8,
            NumberType::USIZE => IlType::U32,
        }
    }
}

impl ByteSize for IlType {
    fn byte_count(&self, _ctxt: &ProgramContext) -> u32 {
        match self {
            IlType::U8 => 1,
            IlType::U32 => 4,
        }
    }
}

impl IlType {
    pub fn byte_count(&self) -> u32 {
        match self {
            IlType::U8 => 1,
            IlType::U32 => 4,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IlLocation {
    U8,
    U32,
    FrameOffset(u32),
    Static(u32),
}

impl TryFrom<u32> for IlLocation {
    type Error = ();

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(IlLocation::U8),
            2..=4 => Ok(IlLocation::U32),
            _ => Err(()),
        }
    }
}

pub struct IlUsages<'a> {
    srcs: Vec<&'a IlVarId>,
    dest: Option<&'a IlVarId>,
}

pub struct IlUsagesMut<'a> {
    srcs: Vec<&'a mut IlVarId>,
    dest: Option<&'a mut IlVarId>,
}

impl IlInstruction {
    fn var_usages(&self) -> IlUsages {
        let (srcs, dest) = match self {
            IlInstruction::Unreachable | IlInstruction::Comment(_) => (vec![], None),
            IlInstruction::AssignNumber { dest, src:_} => 
                (vec![], Some(dest)),
            IlInstruction::AssignVar { dest, src, size:_, src_range: _, dest_range: _} => 
                (vec![src], Some(dest)),
            IlInstruction::AssignUnary { dest, op:_, src } => 
                (vec![src], Some(dest)),
            IlInstruction::AssignBinary { dest, op:_, src1, src2 } =>
                (vec![src1, src2], Some(dest)),
            IlInstruction::ReadMemory { dest, addr, size:_ } =>
                (vec![addr], Some(dest)),
            IlInstruction::WriteMemory { addr, src, size:_ } => 
                (vec![addr, src], None),
            IlInstruction::Goto(_) => (vec![], None),
            IlInstruction::IfThenElse { left, op:_, right, then_label:_ , else_label:_ } => 
                (vec![left, right], None),
            IlInstruction::Call { ret, f:_, args } =>
                (args.iter().collect(), ret.as_ref()),
            IlInstruction::Resize { dest, dest_size:_, src, src_size:_ } => 
                (vec![src], Some(dest)),
            IlInstruction::Return { val } => (
                if let Some(v) = val { vec![v] } else {vec![]},
                None),
            IlInstruction::TtyIn { dest } => (vec![], Some(dest)),
            IlInstruction::TtyOut { src } => (vec![src], None),
            IlInstruction::Label(_) => (vec![], None),
        };

        IlUsages { srcs, dest }
    }

    fn var_usages_mut(&mut self) -> IlUsagesMut {
        let (srcs, dest) = match self {
            IlInstruction::Unreachable | IlInstruction::Comment(_) => (vec![], None),
            IlInstruction::AssignNumber { dest, src:_} => 
                (vec![], Some(dest)),
            IlInstruction::AssignVar { dest, src, size:_, src_range:_, dest_range:_} => 
                (vec![src], Some(dest)),
            IlInstruction::AssignUnary { dest, op:_, src } => 
                (vec![src], Some(dest)),
            IlInstruction::AssignBinary { dest, op:_, src1, src2 } =>
                (vec![src1, src2], Some(dest)),
            IlInstruction::ReadMemory { dest, addr, size:_ } =>
                (vec![addr], Some(dest)),
            IlInstruction::WriteMemory { addr, src, size:_ } => 
                (vec![addr, src], None),
            IlInstruction::Goto(_) => (vec![], None),
            IlInstruction::IfThenElse { left, op:_, right, then_label:_ , else_label:_ } => 
                (vec![left, right], None),
            IlInstruction::Call { ret, f:_, args } =>
                (args.iter_mut().collect(), ret.as_mut()),
            IlInstruction::Resize { dest, dest_size:_, src, src_size:_ } => 
                (vec![src], Some(dest)),
            IlInstruction::Return { val } => (
                if let Some(v) = val { vec![v] } else {vec![]},
                None),
            IlInstruction::TtyIn { dest } => (vec![], Some(dest)),
            IlInstruction::TtyOut { src } => (vec![src], None),
            IlInstruction::Label(_) => (vec![], None),
        };

        IlUsagesMut { srcs, dest }
    }
}

impl Debug for IlInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IlInstruction::Unreachable => write!(f, "Unreachable"),
            IlInstruction::Comment(arg0) => write!(f, "#{}", arg0),
            IlInstruction::Label(arg0) => write!(f, ":{}", arg0.0),
            IlInstruction::AssignNumber { dest, src } => write!(f, "{} <- {:?} ", dest.0, src),
            IlInstruction::AssignVar { dest, src, size, src_range, dest_range } => {
                write!(f, "{}", dest.0)?;
                if let Some(dest_range) = dest_range {
                    write!(f, "[0x{:02x}..0x{:02x}]", dest_range.start, dest_range.end)?;
                }
                write!(f, " <- {:?}", src)?;
                if let Some(src_range) = src_range {
                    write!(f, "[0x{:02x}..0x{:02x}]", src_range.start, src_range.end)?;
                }

                write!(f, " {:?}", size)
            }
            IlInstruction::AssignUnary { dest, op, src } => write!(f, "{} <- {:?}({:?})", dest.0, op, src),
            IlInstruction::AssignBinary { dest, op, src1, src2 } => write!(f, "{} <- {} {:?} {:?}", dest.0, src1.0, op, src2),
            IlInstruction::ReadMemory { dest, addr, size } => write!(f, "{} <- mem[{:?}] {:?}", dest.0, addr, size),
            IlInstruction::WriteMemory { addr, src, size } => write!(f, "mem[{:?}] <- {} {:?}", addr, src.0, size),
            IlInstruction::Goto(arg0) => write!(f, "goto {}", arg0.0),
            IlInstruction::IfThenElse { left, op, right, then_label, else_label } => 
                write!(f, "if {} {:?} {:?} then '{}' else '{}'", left.0, op, right, then_label.0, else_label.0),
            IlInstruction::Call { ret, f: func, args } => {
                write!(f, "{:?} <= call {}(", ret.as_ref(), func.0)?;
                for a in args {
                    write!(f, "{},", a.0)?;
                }
                write!(f, ")")
            }
            IlInstruction::Return { val } => write!(f, "return {:?}", val),
            IlInstruction::TtyIn { dest } => write!(f, "{} <- ttyin", dest.0),
            IlInstruction::TtyOut { src } => write!(f, "ttyout <- {}", src.0),
            IlInstruction::Resize { dest, dest_size, src, src_size } => 
                write!(f, "{} {:?} <- {} {:?}", dest.0, dest_size, src.0, src_size),
        }
    }
}


#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub enum IlNumber {
    U8(u8),
    U32(u32),
}

impl IlNumber {
    pub fn il_type(&self) -> IlType {
        match self {
            IlNumber::U8(_) => IlType::U8,
            IlNumber::U32(_) => IlType::U32,
        }
    }

    pub fn as_bytes(&self) -> Vec<u8> {
        match self {
            IlNumber::U8(n) => vec![*n],
            IlNumber::U32(n) => n.to_le_bytes().iter().cloned().collect(),
        }
    }

    pub fn as_u32(&self) -> u32 {
        match self {
            IlNumber::U8(n) => (*n).into(),
            IlNumber::U32(n) => *n,
        }
    }
}

impl Debug for IlNumber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::U8(arg0) => write!(f, "0n{}/0x{:02x}u8", arg0, arg0),
            Self::U32(arg0) => if *arg0 >= 0x80000000 {
                write!(f, "0x{:08x}u32", arg0)
            } else {
                write!(f, "0n{}/0x{:08x}u32", arg0, arg0)
            }
        }
    }
}

impl From<u8> for IlNumber {
    fn from(x: u8) -> Self {
        IlNumber::U8(x)
    }
}

impl From<u32> for IlNumber {
    fn from(x: u32) -> Self {
        IlNumber::U32(x)
    }
}


#[derive(Clone, Debug)]
pub struct IlProgram {
    pub entry: IlFunctionId,
    pub functions: BTreeMap<IlFunctionId, IlFunction>,
    pub statics: BTreeMap<(Option<IlFunctionId>,IlVarId), IlVarInfo>,
    pub image_base_address: u32,
    pub statics_addresses: Range<u32>,
}

impl IlProgram {
    pub fn from_program(ctxt: &ProgramContext) -> IlProgram {
        let mut il = IlProgram {
            entry: IlFunctionId(ctxt.entry.clone()),
            functions: BTreeMap::new(),
            statics: BTreeMap::new(),
            image_base_address: ctxt.image_base_address,
            statics_addresses: 0..0,
        };
            
        let mut next_static_addr = ctxt.statics_base_address;

        for (_, def) in &ctxt.function_defs {
            let mut il_ctxt = IlContext {
                next_static_addr,
                program: ctxt,
                il: &mut il,
                func_def: def,
            };
            let f = IlFunction::emit_from(&mut il_ctxt);
            next_static_addr = il_ctxt.next_static_addr;
            il.functions.insert(f.id.clone(), f);
        }

        il.statics_addresses = ctxt.statics_base_address..next_static_addr;

        il
    }
}

struct IlContext<'a> {
    next_static_addr: u32,
    program: &'a ProgramContext,
    il: &'a mut IlProgram,
    func_def: &'a FunctionDefinition,
}

impl<'a> IlContext<'a> {
    fn find_arg_or_var(&self, il_func: &IlFunction, n: &str) -> IlVarInfo {
        let n = IlVarId(n.to_owned());
        if let Some(var) = il_func.vars.get(&n) {
            return var.clone();
        }

        let static_key = (Some(il_func.id.clone()), n.clone());
        if let Some(var) = self.il.statics.get(&static_key) {
            return var.clone();
        }

        panic!("Could not find '{}'", &n.0);
    }

    fn try_emit_type(&self, e: &Expression) -> Option<Type> {
        e.try_emit_type(self.program, Some(self.func_def))
    }
}

#[derive(Debug)]
pub struct IlLiveness<'a> {
    f: &'a IlFunction,
    ins: Vec<BTreeSet<&'a IlVarId>>, // live at start of i
    outs: Vec<BTreeSet<&'a IlVarId>>, // live at end of i
    succ: Vec<BTreeSet<usize>>,
    labels: BTreeMap<&'a IlLabelId, usize>,
    gen: Vec<BTreeSet<&'a IlVarId>>, //generates liveness
    kill: Vec<BTreeSet<&'a IlVarId>>, //ends liveness
    interferes: BTreeMap<&'a IlVarId, BTreeSet<&'a IlVarId>>,
    pub colors: BTreeMap<&'a IlVarId, usize>,
}

impl<'a> IlLiveness<'a> {
    pub fn calculate(f: &'a IlFunction) -> IlLiveness<'a> {
        let len = f.body.len();

        // dbg!(&f.body);
        // dbg!(&f.vars);

        let labels: BTreeMap<&IlLabelId, usize> = f.body.iter().enumerate()
            .filter_map(|(i,s)| match s {
                IlInstruction::Label(label) => Some((label, i)),
                _ => None
            })
            .collect();

        // dbg!(&labels);
        
        let succ = f.body.iter().enumerate()
            .map(|(i,s)| {
                let nexts = match s {
                    IlInstruction::Goto(target) => [Some(labels[target]), None],
                    IlInstruction::IfThenElse { left:_, op:_, right:_, then_label, else_label } => {
                        [Some(labels[then_label]), Some(labels[else_label])]
                    },
                    IlInstruction::Return {..} => [None, None],
                    IlInstruction::Unreachable => [None, None],
                    _ => [Some(i+1), None]
                };
                nexts.iter().filter_map(|next| next.as_ref()).cloned().collect()
            }).collect();

        let gen: Vec<BTreeSet<&IlVarId>> = f.body.iter()
            .map(|s| {
                let usages = s.var_usages();
                usages.srcs.iter().map(|s| *s).collect()
            }).collect();

        let kill: Vec<BTreeSet<&IlVarId>> = f.body.iter()
            .map(|s| {
                match s {
                    IlInstruction::AssignVar {dest_range: Some(_), ..} => BTreeSet::new(),
                    _ => {
                        let usages = s.var_usages();
                        [usages.dest].iter()
                            .filter_map(|s| s.as_ref())
                            .map(|s| *s)
                            .collect()
                    }
                }
                
            }).collect();

        let mut l = IlLiveness {
            f,
            ins: vec![BTreeSet::new(); len],
            outs: vec![BTreeSet::new(); len],
            succ,
            labels,
            gen,
            kill,
            interferes: f.vars.keys().map(|k| (k, BTreeSet::new())).collect(),
            colors: BTreeMap::new(),
        };

        {
            println!("i | succ[i] | gen[i] | kill[i] | inst");
            for i in 0..len {
                println!("{} | {:?} | {:?} | {:?} | {:?}", i, &l.succ[i], &l.gen[i], &l.kill[i], &f.body[i]);
            }

            let mut run_more = true;

            while run_more {
                println!("i | outs[i] | ins[i]");
                for i in 0..len {
                    println!("{} | {:?} | {:?}", i, &l.outs[i], &l.ins[i]);
                }

                run_more = false;

                for i in (0..len).rev() {
                    for j in &l.succ[i] {
                        for k in &l.ins[*j] {
                            run_more |= l.outs[i].insert(k);
                        }
                    }

                    for g in &l.gen[i] {
                        run_more |= l.ins[i].insert(g);
                    }

                    for o in &l.outs[i] {
                        if l.kill[i].contains(o) {
                            // skip
                        } else {
                            run_more |= l.ins[i].insert(o);
                        }
                    }
                }
            }
        }

        for v1 in &l.f.args {
            for v2 in &l.f.args {
                if v1 != v2 {
                    l.interferes.get_mut(v1).unwrap().insert(v2);
                    l.interferes.get_mut(v2).unwrap().insert(v1);
                }
            }
        }

        for (i, inst) in l.f.body.iter().enumerate() {
            for x in &l.kill[i] {
                for y in  &l.outs[i] {
                    if x == y { continue; }
                    match inst {
                        IlInstruction::AssignVar {dest, src, size:_, src_range:_, dest_range:_}
                            if dest == *x && src == *y => {}
                        _ => {
                            // dbg!(i, x, y);
                            l.interferes.get_mut(*x).unwrap().insert(*y);
                            l.interferes.get_mut(*y).unwrap().insert(*x);
                        }
                    }
                }
            }
        }

        let mut next_color = 0;

        for v in l.f.vars.keys() {
            let neighbors = &l.interferes[v];
            let neighbor_colors: BTreeSet<usize> = neighbors.iter().filter_map(|n| l.colors.get(n)).cloned().collect();
            for i in 0..=next_color {
                if !neighbor_colors.contains(&i) {
                    l.colors.insert(v, i);
                    if i == next_color {
                        next_color += 1;
                    }
                    break;
                }
            }
        }

        l
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fib_liveness() {
        let n = IlVarId("n".to_owned());
        let a = IlVarId("a".to_owned());
        let b = IlVarId("b".to_owned());
        let z = IlVarId("z".to_owned());
        let t = IlVarId("t".to_owned());
        let one = IlVarId("one".to_owned());
        let loop_label = IlLabelId("loop".to_owned());
        let end_label = IlLabelId("end".to_owned());
        let body_label = IlLabelId("body".to_owned());

        let mut vars = BTreeMap::new();
        for v in [&n,&a,&b,&z,&t,&one] {
            vars.insert(
                v.clone(),
                IlVarInfo {
                    byte_size: 1,
                    description: v.0.to_owned(),
                    location: IlLocation::U8,
                    var_type: Type::Number(NumberType::U8)
                }
            );
        }

        let f = IlFunction {
            id: IlFunctionId("fib".to_owned()),
            args: vec![n.clone()],
            next_label_num: 0,
            next_temp_num: 0,
            vars_stack_size: 0, 
            ret: Some(IlType::U8),
            vars,
            labels: ([&loop_label, &body_label, &end_label]).iter().map(|l| (**l).clone()).collect(),
            body : vec![
                IlInstruction::AssignNumber {dest: one.clone(), src: IlNumber::U8(1)},
                IlInstruction::AssignNumber {dest: a.clone(), src: IlNumber::U8(0)},
                IlInstruction::AssignNumber {dest: b.clone(), src: IlNumber::U8(1)},
                IlInstruction::AssignNumber {dest: z.clone(), src: IlNumber::U8(0)},
                IlInstruction::Label(loop_label.clone()),
                IlInstruction::IfThenElse {left: n.clone(), op: IlCmpOp::Equals, right: z.clone(), 
                    then_label: end_label.clone(), else_label: body_label.clone() },
                IlInstruction::Label(body_label.clone()),
                IlInstruction::AssignBinary { dest: t.clone(), op: IlBinaryOp::Add, src1: a.clone(), src2: b.clone() },
                IlInstruction::AssignVar { dest: a.clone(), src: b.clone(), size: IlType::U8, dest_range: None, src_range: None },
                IlInstruction::AssignVar { dest: b.clone(), src: t.clone(), size: IlType::U8, dest_range: None, src_range: None },
                IlInstruction::AssignBinary { dest: n.clone(), op: IlBinaryOp::Subtract, src1: n.clone(), src2: one.clone() },
                IlInstruction::AssignNumber {dest: z.clone(), src: IlNumber::U8(0)},
                IlInstruction::Goto(loop_label.clone()),
                IlInstruction::Label(end_label.clone()),
                IlInstruction::Return{val: Some(a.clone())},
            ]
        };

        let l = IlLiveness::calculate(&f);
        // dbg!(&l);
        assert_eq!(l.ins[0].iter().next(), Some(&&n.clone()));
    }
}