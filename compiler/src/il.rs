#![allow(dead_code)]

use std::{collections::BTreeMap, convert::TryFrom, fmt::Debug, hash::Hash};

use crate::*;

#[derive(Clone, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct IlVarId(String);

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
    pub location: Location,
    pub var_type: Type,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct IlLabelId(String);

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct IlFunctionId(String);

#[derive(Debug)]
pub struct IlFunction {
    pub id: IlFunctionId,
    pub args: Vec<IlVarId>,
    pub body: Vec<IlInstruction>,
    pub vars: BTreeMap<IlVarId, IlVarInfo>,
    labels: BTreeSet<IlLabelId>,
    next_temp_num: usize,
    next_label_num: usize,
    vars_stack_size: u32, 
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
            0 | 1 => Location::U8,
            4 => Location::U32,
            _ => panic!(),
        };
        let info = IlVarInfo {
            description: format!("{} {:?}", &id.0, e),
            var_type: e.try_emit_type(ctxt.program, Some(ctxt.func_def)).unwrap(),
            location
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

    fn alloc_tmp_and_emit_static_address(&mut self, name: &str, info: &IlVarInfo) -> IlVarId {
        let addr = match info.location {
            Location::Static(addr) => addr,
            _ => panic!(),
        };
        let addr_var = self.alloc_tmp(IlVarInfo {
            description: format!("static {:?}", &name),
            location: Location::U32,
            var_type: Type::Ptr(Box::new(info.var_type.clone())),
        });
        self.body.push(IlInstruction::AssignAtom {
            dest: addr_var.clone(),
            src: IlAtom::Number(IlNumber::U32(addr))
        });
        addr_var
    }

    fn emit_address(&mut self, ctxt: &IlContext, value: &Expression) -> (IlVarId, Option<IlType>) {
        match value {
            Expression::Ident(n) => {
                let info = ctxt.find_arg_or_var(&self, n);
                match info.location {
                    Location::U8 | Location::U32 => panic!(),
                    Location::FrameOffset(offset) => {
                        let addr = Expression::Arithmetic(
                            ArithmeticOperator::Add,
                            Box::new(Expression::Ident(IlVarId::frame_pointer().to_owned())),
                            Box::new(Expression::Number(NumberType::USIZE, offset))
                        );
                        let (id, _) = self.alloc_tmp_and_emit_value(ctxt, &addr);
                        (id, None)
                    },
                    Location::Static(_) => {
                        let size = info.var_type.byte_count(ctxt.program).try_into().ok();
                        let id = self.alloc_tmp_and_emit_static_address(n, &info);
                        (id, size)
                    }
                    Location::FramePointer => panic!(),
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
                    Location::Static(addr) => Expression::Number(NumberType::USIZE, addr),
                    Location::U32 | Location::U8 => {
                        Expression::Cast {
                            old_type: Some(ptr_type.clone()),
                            new_type: Type::Number(NumberType::USIZE),
                            value: Box::new(Expression::Ident(var.clone())),
                        }
                    },
                    Location::FrameOffset(offset) => {
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
                    Location::FramePointer => panic!(),
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
                    Location::Static(addr) => Expression::Number(NumberType::USIZE, addr),
                    Location::U8 | Location::U32 => todo!(),
                    Location::FrameOffset(offset) => Expression::Arithmetic(
                        ArithmeticOperator::Add,
                        Box::new(Expression::Ident(IlVarId::frame_pointer().to_owned())),
                        Box::new(Expression::Number(NumberType::USIZE, offset)),
                    ),
                    Location::FramePointer => panic!(),
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
                    Location::Static(_) => {
                        todo!()
                    }
                    Location::U8 | Location::U32 => {
                        Expression::Cast {
                            old_type: Some(ptr_type.clone()),
                            new_type: Type::Number(NumberType::USIZE),
                            value: Box::new(Expression::Ident(n.clone())),
                        }
                    },
                    Location::FrameOffset(_) => todo!(),
                    Location::FramePointer => panic!(),
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

    fn emit_target_expression(&mut self, ctxt: &IlContext, target: &Expression) -> (IlVarId, Option<IlType>) {
        // dbg!(target);
        match target {
            Expression::Ident(n) => {
                let info= ctxt.find_arg_or_var(&self, n);
                match info.location {
                    Location::U8 | Location::U32 => {
                        (IlVarId(n.clone()), None)
                    },
                    Location::FrameOffset(_) | Location::Static(_) => {
                        let (address, size) = self.emit_address(ctxt,  target);
                        (address, Some(size.unwrap()))
                    },
                    Location::FramePointer => panic!(),
                }
            },
            Expression::Deref(e) => {
                let (addr, info) = self.alloc_tmp_and_emit_value(ctxt, e);
                let size = info.var_type.byte_count(ctxt.program).try_into().unwrap();
                (addr, Some(size))
            },
            Expression::LocalFieldDeref(_,_) |
            Expression::PtrFieldDeref(_,_) | 
            Expression::Index(_, _) => {
                let (address, size) = self.emit_address(ctxt,  target);
                (address, Some(size.unwrap()))
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
            right: IlAtom::Var(right.clone()),
            then_label: true_label.clone(),
            else_label: false_label.clone(),
        });
    }
    
    fn emit_statement(&mut self, ctxt: &mut IlContext, s: &Statement) {
        match s {
            Statement::Declare { .. } => {},
            Statement::Assign { target, var_type, value } => {
                let (target, mem_size) = self.emit_target_expression(ctxt, target);
                let emitted_type = if let Some(size) = mem_size {
                    let (value_reg, info) = self.alloc_tmp_and_emit_value(ctxt, value);
                    self.body.push(IlInstruction::WriteMemory{addr: IlAtom::Var(target), src: value_reg.clone(), size});
                    info.var_type
                } else {
                    self.emit_expression(ctxt, target.clone(), value);
                    self.vars[&target].var_type.clone()
                };

                if let Some(var_type) = var_type {
                    assert_eq!(var_type, &emitted_type);
                }
            },
            Statement::VoidExpression { expression } => {
                let (_, _) = self.alloc_tmp_and_emit_value(ctxt, expression);
            },
            Statement::IfElse { predicate, when_true, when_false } => {
                let true_label= self.alloc_label("if true");
                let false_label= self.alloc_label("if false");
                let end_label= self.alloc_label("if end");

                self.emit_comparison(ctxt, predicate, true_label.clone(), false_label.clone());

                self.body.push(IlInstruction::Label(true_label.clone()));
                for s in when_true {
                    self.emit_statement(ctxt, s);
                }
                self.body.push(IlInstruction::Goto(end_label.clone()));

                self.body.push(IlInstruction::Label(false_label.clone()));
                for s in when_false {
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
                let (val,_) = self.alloc_tmp_and_emit_value(ctxt, &value);
                self.body.push(IlInstruction::Return{val: val.clone()})
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
                    Location::U8 | Location::U32 => {
                        let src = IlAtom::Var(IlVarId(name.clone()));
                        self.body.push(IlInstruction::AssignAtom{dest, src});
                    }
                    Location::FrameOffset(_) => todo!(),
                    Location::Static(addr) => {
                        let size = info.var_type.byte_count(ctxt.program).try_into().unwrap();
                        self.body.push(IlInstruction::ReadMemory {
                            dest,
                            addr: IlAtom::Number(IlNumber::U32(addr)),
                            size,
                        })
                    },
                    Location::FramePointer => {
                        self.body.push(IlInstruction::GetFrameAddress { dest });
                    },
                }
            },
            Expression::Number(num_type, val) => {
                let src = IlAtom::Number(match num_type {
                    NumberType::U8 => IlNumber::U8((*val).try_into().unwrap()),
                    NumberType::USIZE => IlNumber::U32(*val)
                });
                self.body.push(IlInstruction::AssignAtom{dest, src});
            },
            Expression::TtyIn() => {
                self.body.push(IlInstruction::TtyIn {dest});
            },
            Expression::Arithmetic(op, left, right) => {

                let left_type = ctxt.try_emit_type(left).unwrap().get_number_type().unwrap();
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
                    src2: IlAtom::Var(right_tmp.clone()),
                });
            },
            Expression::Comparison(_) => todo!(),
            Expression::Deref(ptr) => {
                let (addr, ptr_info) = self.alloc_tmp_and_emit_value(ctxt, ptr);
                let derefed_type = ptr_info.var_type.get_element_type().unwrap();
                self.body.push(IlInstruction::ReadMemory {
                    dest,
                    addr: IlAtom::Var(addr),
                    size: derefed_type.byte_count(ctxt.program).try_into().unwrap(),
                });
            },
            Expression::LocalFieldDeref(_,_) |
            Expression::PtrFieldDeref(_,_) => {
                let (addr, size) = self.emit_address(ctxt, e);
                self.body.push(IlInstruction::ReadMemory {
                    dest,
                    addr: IlAtom::Var(addr),
                    size: size.unwrap(),
                })
            },
            Expression::AddressOf(n) => {
                let (addr, size) = self.emit_address(ctxt, n);
                self.body.push(IlInstruction::AssignAtom {
                    dest,
                    src: IlAtom::Var(addr)
                });
            },
            Expression::Index(_, _) => {
                let (addr, size) = self.emit_address(ctxt, e);

                self.body.push(IlInstruction::ReadMemory {
                    dest,
                    addr: IlAtom::Var(addr),
                    size: size.unwrap(),
                });
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
                
                self.body.push(IlInstruction::Call {
                    f: IlFunctionId(c.function.clone()),
                    ret: dest,
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
        
        for (i, (name, var_type)) in ctxt.func_def.args.iter().enumerate() {
            let size = var_type.byte_count(ctxt.program).try_into().unwrap();
            let id = func.alloc_named(
                name.clone(),
                IlVarInfo {
                    description: format!("Arg{} {} {:?}", i, name, var_type),
                    location: size,
                    var_type: var_type.clone(),
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
                            Location::FrameOffset(offset)
                        },
                        _ => size.try_into().unwrap()
                    }
                },
                Scope::Static => {
                    assert!(
                        ctxt.il.statics.insert(
                            (Some(id.clone()), IlVarId(name.clone())),
                            IlVarInfo {
                                location: Location::Static(ctxt.next_static_addr),
                                var_type: var_type.clone(),
                                description: format!("static {}", name)
                            }
                        ).is_none()
                    );
                    let addr = ctxt.next_static_addr;
                    ctxt.next_static_addr += size;
                    Location::Static(addr)
                },
            };
            

            func.alloc_named(
                name.clone(),
                IlVarInfo {
                    location: size,
                    description: format!("Local {} {:?} {:?}", name, var_type, size),
                    var_type: var_type.clone(),
                });
        }

        if func.vars_stack_size > 0 {
            func.alloc_named(
                IlVarId::frame_pointer().to_owned(),
                IlVarInfo {
                    description: IlVarId::frame_pointer().to_owned(),
                    location: Location::FramePointer,
                    var_type: Type::Number(NumberType::USIZE),
                });
        }

        for s in &ctxt.func_def.body {
            func.emit_statement(ctxt, s);
        }

        if ctxt.func_def.return_type == Type::Void {
            func.emit_statement(ctxt, &Statement::Return{value: Expression::Number(NumberType::U8, 0) });
        }
        
        func
    }
}

#[derive(Debug)]
pub enum IlUnaryOp {
    Negate,
    BinaryInvert
}

#[derive(Debug)]
pub enum IlBinaryOp {
    Add,
    Subtract,
    Multiply,
    BitwiseAnd,
    BitwiseOr,
}

impl IlBinaryOp {
    fn from(op: &ArithmeticOperator) -> IlBinaryOp {
        match op {
            ArithmeticOperator::Add => IlBinaryOp::Add,
            ArithmeticOperator::Subtract => IlBinaryOp::Subtract,
            ArithmeticOperator::Multiply => IlBinaryOp::Multiply,
            ArithmeticOperator::Or => IlBinaryOp::BitwiseOr,
            ArithmeticOperator::And => IlBinaryOp::BitwiseAnd,
        }
    }
}
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

impl ByteSize for IlType {
    fn byte_count(&self, _ctxt: &ProgramContext) -> u32 {
        match self {
            IlType::U8 => 1,
            IlType::U32 => 4,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Location {
    U8,
    U32,
    FrameOffset(u32),
    Static(u32),
    FramePointer,
}

impl TryFrom<u32> for Location {
    type Error = ();

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Location::U8),
            2..=4 => Ok(Location::U32),
            _ => Err(()),
        }
    }
}

pub enum IlInstruction {
    Label(IlLabelId),
    AssignAtom{ dest: IlVarId, src: IlAtom},
    AssignUnary{ dest: IlVarId, op: IlUnaryOp, src: IlAtom },
    AssignBinary { dest: IlVarId, op: IlBinaryOp, src1: IlVarId, src2: IlAtom },
    ReadMemory {dest: IlVarId, addr: IlAtom, size: IlType},
    WriteMemory {addr: IlAtom, src: IlVarId, size: IlType},
    Goto(IlLabelId),
    IfThenElse {left: IlVarId, op: IlCmpOp, right: IlAtom, then_label: IlLabelId, else_label: IlLabelId},
    Call {ret: IlVarId, f: IlFunctionId, args: Vec<IlVarId> },
    Resize {dest: IlVarId, dest_size: IlType, src:IlVarId, src_size: IlType},
    Return { val: IlVarId },
    TtyIn { dest: IlVarId },
    TtyOut { src: IlVarId },
    GetFrameAddress { dest: IlVarId },
}

impl Debug for IlInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IlInstruction::Label(arg0) => write!(f, ":{}", arg0.0),
            IlInstruction::AssignAtom { dest, src } => write!(f, "{} <- {:?}", dest.0, src),
            IlInstruction::AssignUnary { dest, op, src } => write!(f, "{} <- {:?}({:?})", dest.0, op, src),
            IlInstruction::AssignBinary { dest, op, src1, src2 } => write!(f, "{} <- {} {:?} {:?}", dest.0, src1.0, op, src2),
            IlInstruction::ReadMemory { dest, addr, size } => write!(f, "{} <- mem[{:?}] {:?}", dest.0, addr, size),
            IlInstruction::WriteMemory { addr, src, size } => write!(f, "mem[{:?}] <- {} {:?}", addr, src.0, size),
            IlInstruction::Goto(arg0) => write!(f, "goto {}", arg0.0),
            IlInstruction::IfThenElse { left, op, right, then_label, else_label } => 
                write!(f, "if {} {:?} {:?} then '{}' else '{}'", left.0, op, right, then_label.0, else_label.0),
            IlInstruction::Call { ret, f: func, args } => {
                write!(f, "{} <= call {}(", ret.0, func.0)?;
                for a in args {
                    write!(f, "{},", a.0)?;
                }
                write!(f, ")")
            }
            IlInstruction::Return { val } => write!(f, "return {}", val.0),
            IlInstruction::TtyIn { dest } => write!(f, "{} <- ttyin", dest.0),
            IlInstruction::TtyOut { src } => write!(f, "ttyout <- {}", src.0),
            IlInstruction::Resize { dest, dest_size, src, src_size } => 
                write!(f, "{} {:?} <- {} {:?}", dest.0, dest_size, src.0, src_size),
            IlInstruction::GetFrameAddress { dest } => write!(f, "{} <- frame_pointer", dest.0),
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
}

impl Debug for IlNumber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::U8(arg0) => write!(f, "{}u8", arg0),
            Self::U32(arg0) => if *arg0 >= 0x80000000 {
                write!(f, "0x{:08x}u32", arg0)
            } else {
                write!(f, "{}u32", arg0)
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

pub enum IlAtom {
    Number(IlNumber),
    Var(IlVarId)
}

impl Debug for IlAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(arg0) => write!(f, "{:?}", arg0),
            Self::Var(arg0) => write!(f, "{}", arg0.0),
        }
    }
}

#[derive(Debug)]
pub struct IlProgram {
    pub entry: IlFunctionId,
    pub functions: BTreeMap<IlFunctionId, IlFunction>,
    pub statics: BTreeMap<(Option<IlFunctionId>,IlVarId), IlVarInfo>,
}

impl IlProgram {
    pub fn from_program(ctxt: &ProgramContext) -> IlProgram {
        let mut il = IlProgram {
            entry: IlFunctionId(ctxt.entry.clone()),
            functions: BTreeMap::new(),
            statics: BTreeMap::new(),
        };
            
        let mut next_static_addr = STATICS_START_ADDRESS;

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