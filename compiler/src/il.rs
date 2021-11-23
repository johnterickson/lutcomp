#![allow(dead_code)]

use std::{collections::BTreeMap, convert::TryFrom, fmt::Debug, hash::Hash, thread::LocalKey};

use crate::*;

#[derive(Clone, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct IlVarId(String);

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

    fn alloc_tmp(&mut self, info: IlVarInfo) -> IlVarId {
        let id = IlVarId(format!("t{}", self.next_temp_num));
        self.vars.insert(id.clone(), info);
        self.next_temp_num += 1;
        id
    }

    fn alloc_tmp_for_expression(&mut self, ctxt: &IlContext, e: &Expression) -> (IlVarId, IlVarInfo) {
        let id = IlVarId(format!("t{}", self.next_temp_num));
        let t = e.try_emit_type(ctxt.program, Some(ctxt.func))
            .expect(&format!("Could not determine type for '{:?}'.", e));
        let location = match t.byte_count(ctxt.program) {
            1 => Location::U8,
            4 => Location::U32,
            _ => panic!(),
        };
        let info = IlVarInfo {
            description: format!("{} {:?}", &id.0, e),
            var_type: e.try_emit_type(ctxt.program, Some(ctxt.func)).unwrap(),
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
        let id = IlLabelId(format!("{}_{}", info, self.next_label_num));
        self.next_label_num += 1;
        assert!(self.labels.insert(id.clone()));
        id
    }

    fn emit_address(&mut self, ctxt: &IlContext, target: &Expression) -> (IlVarId, IlType) {
        match target {
            Expression::Index(var, index) => {
                let (scope, ptr_type) = ctxt.func.find_arg_or_var(var)
                    .expect(&format!("Could not find {}", var));

                let element_type = ptr_type.get_element_type().unwrap();

                let element_size = element_type.byte_count(ctxt.program);

                let base_address = match scope {
                    Scope::Local => IlVarId(var.clone()),
                    Scope::Static => {
                        let key= (Some(self.id.clone()), IlVarId(var.clone()));
                        let (t, addr) = ctxt.il.statics.get(&key).expect(&format!("Could not find {:?}", key));
                        let base = self.alloc_tmp(IlVarInfo {
                            description: format!("{:?}", &key),
                            location: Location::U32,
                            var_type: Type::Ptr(Box::new(t.clone())),
                        });
                        self.body.push(IlInstruction::AssignAtom {
                            dest: base.clone(),
                            src: IlAtom::Number(IlNumber::U32(*addr))
                        });

                        base
                    }
                };


                let (index_val, index_info) = self.alloc_tmp_for_expression(ctxt, index);
                match index_info.location {
                    Location::U8 => {
                        let (index_u8, _) = self.alloc_tmp_for_expression(ctxt, index);
                        self.emit_expression(ctxt, index_u8.clone(), index);
                        self.body.push(IlInstruction::Resize {
                            dest: index_val.clone(),
                            dest_size: IlType::U32,
                            src: index_u8.clone(),
                            src_size: IlType::U8
                        });
                    }
                    Location::U32 => {
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

                let addr = self.alloc_tmp(IlVarInfo {
                    description: format!("Final address of {:?}", target),
                    location: Location::U32,
                    var_type: Type::Ptr(Box::new(element_type.clone())),
                });
                self.body.push(IlInstruction::AssignBinary {
                    dest: addr.clone(),
                    op: IlBinaryOp::Add,
                    src1: base_address,
                    src2: IlAtom::Var(index_val.clone()),
                });

                (addr, element_size.try_into().unwrap())
            }
            _ => todo!(),
        }
    }

    fn emit_target_expression(&mut self, ctxt: &IlContext, target: &Expression) -> (IlVarId, Option<IlType>) {
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

    fn emit_comparison(&mut self, ctxt: &IlContext, c: &Comparison, true_label: IlLabelId, false_label: IlLabelId) {
        let (left, left_info) = self.alloc_tmp_and_emit_value(ctxt, &c.left);
        let (right, right_info) = self.alloc_tmp_and_emit_value(ctxt, &c.right);

        self.body.push(IlInstruction::IfThenElse{
            left: left.clone(),
            op: c.op.into(), 
            right: IlAtom::Var(right.clone()),
            then_label: true_label.clone(),
            else_label: false_label.clone(),
        });
    }
    
    fn emit_statement(&mut self, ctxt: &IlContext, s: &Statement) {
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
        match e {
            Expression::Ident(v) => {
                let (scope, t) = ctxt.func.find_arg_or_var(v)
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
                let (left_tmp, _) = self.alloc_tmp_and_emit_value(ctxt, left);
                let (right_tmp, _) = self.alloc_tmp_and_emit_value(ctxt, right);
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
            Expression::Cast { old_type, new_type, value } => {
                let old_type = old_type.as_ref().cloned().or_else(|| value.try_emit_type(ctxt.program, Some(ctxt.func))).unwrap();
                if old_type.byte_count(ctxt.program) == new_type.byte_count(ctxt.program) {
                    self.emit_expression(ctxt, dest, value);
                } else {
                    todo!();
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
                IlVarInfo {
                    description: format!("Arg{} {} {:?}", i, name, var_type),
                    location: size,
                    var_type: var_type.clone(),
                });
            func.args.push(id);
        }

        for (name, (scope, var_type)) in ctxt.func.vars.iter() {
            let size = var_type.byte_count(ctxt.program);
            let size = match scope {
                Scope::Local => {
                    match var_type {
                        Type::Struct(_) | Type::Array(_, _) => {
                            let size = (size + 3)/4*4;  // round up
                            Location::Stack(size)
                        },
                        _ => size.try_into().unwrap()
                    }
                },
                Scope::Static => {
                    assert!(
                        ctxt.il.statics.insert(
                            (Some(id.clone()), IlVarId(name.clone())),
                            (var_type.clone(), ctxt.next_static_addr)
                        ).is_none()
                    );
                    ctxt.next_static_addr += size;
                    Location::Static(size)
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
    Stack(u32),
    Static(u32),
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
    pub statics: BTreeMap<(Option<IlFunctionId>,IlVarId), (Type, u32)>,
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
