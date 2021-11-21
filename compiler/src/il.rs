#![allow(dead_code)]

use std::{collections::BTreeMap, convert::TryFrom, fmt::Debug, hash::Hash};

use crate::*;

#[derive(Clone, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct IlVarId(String);

impl Debug for IlVarId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"{}", self.0)
    }
}

#[derive(Debug)]
pub struct IlVarInfo {
    pub description: String,
    pub size: Size,
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
        }
    }

    fn alloc_tmp_with_debug<D: Debug>(&mut self, size: Size, d: D) -> IlVarId {
        self.alloc_tmp(size, format!("{:?}", d))
    }

    fn alloc_tmp(&mut self, size: Size, description: String) -> IlVarId {
        let id = IlVarId(format!("t{}", self.next_temp_num));
        self.next_temp_num += 1;
        match &size {
            Size::U8 | Size::U32 => {},
            _ => panic!(),
        }
        assert!(self.vars.insert(id.clone(), IlVarInfo { description, size }).is_none());
        id
    }

    fn alloc_named(&mut self, name: String, size: Size, description: String) -> IlVarId {
        let id = IlVarId(name);
        assert!(self.vars.insert(id.clone(), IlVarInfo { description, size } ).is_none());
        id
    }

    fn alloc_label(&mut self, info: &str) -> IlLabelId {
        let id = IlLabelId(format!("{}_{}", info, self.next_label_num));
        self.next_label_num += 1;
        assert!(self.labels.insert(id.clone()));
        id
    }

    fn emit_address(&mut self, ctxt: &IlContext, target: &Expression) -> (IlVarId, Size) {
        match target {
            Expression::Index(var, index) => {
                let (scope, ptr_type) = ctxt.func.find_arg_or_var(var)
                    .expect(&format!("Could not find {}", var));

                let element_size = match ptr_type {
                    Type::Ptr(element_type) | Type::Array(element_type, _) => {
                        element_type.byte_count(ctxt.program)
                    }
                    _ => panic!(),
                };

                let base_address = match scope {
                    Scope::Local => IlVarId(var.clone()),
                    Scope::Static => {
                        let key= (Some(self.id.clone()), IlVarId(var.clone()));
                        let addr = ctxt.il.statics.get(&key).expect(&format!("Could not find {:?}", key));
                        let base = self.alloc_tmp_with_debug(Size::U32, index);
                        self.body.push(IlInstruction::AssignAtom {
                            dest: base.clone(),
                            src: IlAtom::Number(IlNumber::U32(*addr))
                        });

                        base
                    }
                };


                let index_size = index.try_emit_type(ctxt.program, Some(ctxt.func)).unwrap().byte_count(ctxt.program);
                let index_val = self.alloc_tmp_with_debug(Size::U32, index);
                match index_size {
                    1 => {
                        let index_u8 = self.alloc_tmp_with_debug(Size::U8, index);
                        self.emit_expression(ctxt, index_u8.clone(), index);
                        self.body.push(IlInstruction::Resize {
                            dest: index_val.clone(),
                            dest_size: Size::U32,
                            src: index_u8,
                            src_size: Size::U8
                        });
                    }
                    4 => {
                        self.emit_expression(ctxt, index_val.clone(), index);
                    }
                    _ => panic!(),
                }

                if element_size != 1 {
                    self.body.push(IlInstruction::AssignBinary {
                        dest: index_val.clone(),
                        op: IlBinaryOp::Multiply,
                        src1: index_val.clone(),
                        src2: IlAtom::Number(IlNumber::U8(element_size.try_into().unwrap())),
                    });
                }

                self.body.push(IlInstruction::AssignBinary {
                    dest: index_val.clone(),
                    op: IlBinaryOp::Add,
                    src1: base_address,
                    src2: IlAtom::Var(index_val.clone()),
                });

                (index_val, element_size.try_into().unwrap())
            }
            _ => todo!(),
        }
    }

    fn emit_target_expression(&mut self, ctxt: &IlContext, target: &Expression) -> (IlVarId, Option<Size>) {
        match target {
            Expression::Ident(n) => {
                let (scope, _) = ctxt.func.find_arg_or_var(n).unwrap();
                match scope {
                    Scope::Local =>  (IlVarId(n.clone()), None),
                    Scope::Static => {
                        todo!();
                    }
                }
            },
            Expression::Deref(_) => todo!(),
            Expression::LocalFieldDeref(_, _) => todo!(),
            Expression::PtrFieldDeref(_, _) => todo!(),
            Expression::Index(_, _) => {
                let (address, size) = self.emit_address(ctxt,  target);
                (address, Some(size))
            },
            Expression::Optimized { original:_, optimized } => 
                self.emit_target_expression(ctxt, optimized),
            _ => panic!(),
        }
    }
    
    fn emit_statement(&mut self, ctxt: &IlContext, s: &Statement) {
        match s {
            Statement::Declare { .. } => {},
            Statement::Assign { target, var_type: _, value } => {
                let (target, mem_size) = self.emit_target_expression(ctxt, target);
                if let Some(size) = mem_size {
                    let value_reg = self.alloc_tmp_with_debug(size, &value);
                    self.emit_expression(ctxt, value_reg.clone(), value);
                    self.body.push(IlInstruction::WriteMemory{addr: IlAtom::Var(target), src: value_reg, size})
                } else {
                    self.emit_expression(ctxt, target, value);
                }
            },
            Statement::VoidExpression { expression } => {
                let value_reg = self.alloc_tmp_with_debug(Size::U8, &expression);
                self.emit_expression(ctxt, value_reg, expression);
            },
            Statement::IfElse { predicate, when_true, when_false } => {
                let true_label= self.alloc_label("if true");
                let false_label= self.alloc_label("if false");
                let end_label= self.alloc_label("if end");

                let left = self.alloc_tmp_with_debug(Size::U8, &predicate.left);
                self.emit_expression(ctxt, left.clone(), &predicate.left);
                let right = self.alloc_tmp_with_debug(Size::U8, &predicate.right);
                self.emit_expression(ctxt, right.clone(), &predicate.right);
                
                self.body.push(IlInstruction::IfThenElse{
                    left, 
                    op: predicate.op.into(), 
                    right: IlAtom::Var(right),
                    then_label: true_label.clone(),
                    else_label: false_label.clone(),
                });

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

                let left = self.alloc_tmp_with_debug(Size::U8, &predicate.left);
                self.emit_expression(ctxt, left.clone(), &predicate.left);
                let right = self.alloc_tmp_with_debug(Size::U8, &predicate.right);
                self.emit_expression(ctxt, right.clone(), &predicate.right);
                
                self.body.push(IlInstruction::IfThenElse{
                    left, 
                    op: predicate.op.into(), 
                    right: IlAtom::Var(right),
                    then_label: body_label.clone(),
                    else_label: end_label.clone(),
                });

                self.body.push(IlInstruction::Label(body_label.clone()));

                for s in while_true {
                    self.emit_statement(ctxt, s);
                }

                self.body.push(IlInstruction::Goto(pred_label.clone()));

                self.body.push(IlInstruction::Label(end_label.clone()));
            },
            Statement::Return { value } => {
                let val = self.alloc_tmp_with_debug(Size::U8, &value);
                self.emit_expression(ctxt, val.clone(), value);
                self.body.push(IlInstruction::Return{val})
            },
            Statement::TtyOut { value } => {
                let value_reg = self.alloc_tmp_with_debug(Size::U8, &value);
                self.emit_expression(ctxt, value_reg.clone(), value);
                self.body.push(IlInstruction::TtyOut{src: value_reg})
            },
        }
    }

    fn emit_expression(&mut self, ctxt: &IlContext, dest: IlVarId, e: &Expression) {
        dbg!(&e);
        match e {
            Expression::Ident(v) => {
                let (scope,_) = ctxt.func.find_arg_or_var(v)
                    .expect(&format!("Could not find {}", v));
                match scope {
                    Scope::Local =>  {
                        let src = IlAtom::Var(IlVarId(v.clone()));
                        self.body.push(IlInstruction::AssignAtom{dest, src});
                    },
                    Scope::Static => {
                        todo!();
                    }
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
                let left_tmp = self.alloc_tmp_with_debug(Size::U8, left);
                self.emit_expression(ctxt, left_tmp.clone(), left);
                let right_tmp = self.alloc_tmp_with_debug(Size::U8, right);
                self.emit_expression(ctxt, right_tmp.clone(), right);
                self.body.push(IlInstruction::AssignBinary {
                    dest,
                    op: IlBinaryOp::from(op),
                    src1: left_tmp,
                    src2: IlAtom::Var(right_tmp),
                });
            },
            Expression::Comparison(_) => todo!(),
            Expression::Deref(_) => todo!(),
            Expression::LocalFieldDeref(_, _) => todo!(),
            Expression::PtrFieldDeref(_, _) => todo!(),
            Expression::AddressOf(_) => todo!(),
            Expression::Index(_, _) => {
                let (addr, size) = self.emit_address(ctxt, e);

                self.body.push(IlInstruction::ReadMemory {
                    dest,
                    addr: IlAtom::Var(addr),
                    size,
                });
            },
            Expression::Cast { old_type, new_type, value } => todo!(),
            Expression::Call(c) => {

                let callee_def = &ctxt.program.function_defs[&c.function];

                assert_eq!(callee_def.args.len(), c.parameters.len());

                let mut params = Vec::new();
                for (i, ((_, arg_type), p)) in callee_def.args.iter().zip(&c.parameters).enumerate() {
                    let size  = arg_type.byte_count(ctxt.program).try_into().unwrap();
                    let p_value = self.alloc_tmp_with_debug(size, p);
                    self.emit_expression(ctxt, p_value.clone(), p);
                    params.push(p_value);
                }
                
                self.body.push(IlInstruction::Call {
                    f: IlFunctionId(c.function.clone()),
                    ret: dest,
                    args: params,
                });
            },
            Expression::Optimized { original, optimized } => todo!(),
        }
    }
}

impl IlFunction {
    fn emit_from(ctxt: &mut IlContext) -> IlFunction {
        let id = IlFunctionId(ctxt.func.name.clone());
        let mut func = IlFunction::new(id.clone());
        
        for (i, (name, var_type)) in ctxt.func.args.iter().enumerate() {
            let size = var_type.byte_count(ctxt.program).try_into().unwrap();
            let id = func.alloc_named(
                name.clone(),
                size,
                format!("Arg{} {} {:?}", i, name, var_type));
            func.args.push(id);
        }

        for (name, (scope, var_type)) in ctxt.func.vars.iter() {
            let size = var_type.byte_count(ctxt.program);
            let size = match scope {
                Scope::Local => {
                    match var_type {
                        Type::Struct(_) | Type::Array(_, _) => {
                            let size = (size + 3)/4*4;  // round up
                            Size::Stack(size)
                        },
                        _ => size.try_into().unwrap()
                    }
                },
                Scope::Static => {
                    assert!(ctxt.il.statics.insert(
                        (Some(id.clone()),IlVarId(name.clone())),
                        ctxt.next_static_addr).is_none());
                    ctxt.next_static_addr += size;
                    Size::Static(size)
                },
            };
            

            func.alloc_named(
                name.clone(),
                size,
                format!("Local {} {:?} {:?}", name, var_type, size));
        }
        
        for s in &ctxt.func.body {
            func.emit_statement(ctxt, s);
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
}

impl IlBinaryOp {
    fn from(op: &ArithmeticOperator) -> IlBinaryOp {
        match op {
            ArithmeticOperator::Add => IlBinaryOp::Add,
            ArithmeticOperator::Subtract => IlBinaryOp::Subtract,
            ArithmeticOperator::Multiply => IlBinaryOp::Multiply,
            ArithmeticOperator::Or => todo!(),
            ArithmeticOperator::And => todo!(),
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
pub enum Size {
    U8,
    U32,
    Stack(u32),
    Static(u32),
}

impl TryFrom<u32> for Size {
    type Error = ();

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Size::U8),
            2..=4 => Ok(Size::U32),
            _ => Err(()),
        }
    }
}

pub enum IlInstruction {
    Label(IlLabelId),
    AssignAtom{ dest: IlVarId, src: IlAtom},
    AssignUnary{ dest: IlVarId, op: IlUnaryOp, src: IlAtom },
    AssignBinary { dest: IlVarId, op: IlBinaryOp, src1: IlVarId, src2: IlAtom },
    ReadMemory {dest: IlVarId, addr: IlAtom, size: Size},
    WriteMemory {addr: IlAtom, src: IlVarId, size: Size},
    Goto(IlLabelId),
    IfThenElse {left: IlVarId, op: IlCmpOp, right: IlAtom, then_label: IlLabelId, else_label: IlLabelId},
    Call {ret: IlVarId, f: IlFunctionId, args: Vec<IlVarId> },
    Resize {dest: IlVarId, dest_size: Size, src:IlVarId, src_size: Size},
    Return { val: IlVarId },
    TtyIn { dest: IlVarId },
    TtyOut { src: IlVarId },
}

impl Debug for IlInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Label(arg0) => write!(f, ":{}", arg0.0),
            Self::AssignAtom { dest, src } => write!(f, "{} <- {:?}", dest.0, src),
            Self::AssignUnary { dest, op, src } => write!(f, "{} <- {:?}({:?})", dest.0, op, src),
            Self::AssignBinary { dest, op, src1, src2 } => write!(f, "{} <- {} {:?} {:?}", dest.0, src1.0, op, src2),
            Self::ReadMemory { dest, addr, size } => write!(f, "{} <- mem[{:?}] {:?}", dest.0, addr, size),
            Self::WriteMemory { addr, src, size } => write!(f, "mem[{:?}] <- {} {:?}", addr, src.0, size),
            Self::Goto(arg0) => write!(f, "goto {}", arg0.0),
            Self::IfThenElse { left, op, right, then_label, else_label } => 
                write!(f, "if {} {:?} {:?} then '{}' else '{}'", left.0, op, right, then_label.0, else_label.0),
            Self::Call { ret, f: func, args } => {
                write!(f, "{} <= call {}(", ret.0, func.0)?;
                for a in args {
                    write!(f, "{},", a.0)?;
                }
                write!(f, ")")
            }
            Self::Return { val } => write!(f, "return {}", val.0),
            Self::TtyIn { dest } => f.debug_struct("TtyIn").field("dest", dest).finish(),
            Self::TtyOut { src } => f.debug_struct("TtyOut").field("src", src).finish(),
            Self::Resize { dest, dest_size, src, src_size } => 
                write!(f, "{} {:?} <- {} {:?}", dest.0, dest_size, src.0, src_size),
        }
    }
}


#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub enum IlNumber {
    U8(u8),
    U32(u32),
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
    pub statics: BTreeMap<(Option<IlFunctionId>,IlVarId), u32>,
}

struct IlContext<'a> {
    next_static_addr: u32,
    program: &'a ProgramContext,
    il: &'a mut IlProgram,
    func: &'a FunctionDefinition,
}

impl IlProgram {
    pub fn from_program(ctxt: &ProgramContext) -> IlProgram {
        let mut il = IlProgram {
            entry: IlFunctionId(ctxt.entry.clone()),
            functions: BTreeMap::new(),
            statics: BTreeMap::new(),
        };

        for (_, def) in &ctxt.function_defs {
            let mut il_ctxt = IlContext {
                next_static_addr: 0x100,
                program: ctxt,
                il: &mut il,
                func: def,
            };
            let f = IlFunction::emit_from(&mut il_ctxt);
            il.functions.insert(f.id.clone(), f);
        }

        il
    }
}
