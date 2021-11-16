use std::{collections::{HashMap, HashSet}, fmt::Debug, hash::Hash};

use crate::*;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct IlVarId(usize);

#[derive(Debug, Default)]
pub struct IlVarInfo {
    pub name: String,
    pub description: String,
}

pub struct IlLabelId(String);
pub struct IlFunctionId(String);

pub struct IlProgram {
    pub functions: HashMap<IlFunctionId, IlFunction>,
}

pub struct IlFunction {
    pub id: IlFunctionId,
    pub args: Vec<IlVarId>,
    pub body: Vec<IlInstruction>,
    pub vars: HashMap<IlVarId, IlVarInfo>,
    pub name_lookup: HashMap<String, IlVarId>,
    next_id: usize,
    next_temp_num: usize,
}


impl IlFunction{
    fn new(id: IlFunctionId) -> IlFunction {
        IlFunction {
            id, 
            args: Vec::new(),
            vars: HashMap::new(),
            body: Vec::new(),
            name_lookup: HashMap::new(),
            next_id: 0,
            next_temp_num: 0
        }
    }
    fn alloc_tmp_with_debug<D: Debug>(&mut self, d: D) -> IlVarId {
        self.alloc_tmp(format!("{:?}", d))
    }

    fn alloc_tmp(&mut self, description: String) -> IlVarId {
        let id = IlVarId(self.next_id);
        self.next_id += 1;
        let name = format!("t{}", self.next_temp_num);
        self.next_temp_num += 1;
        self.vars.insert(id, IlVarInfo { name, description} );
        id
    }

    fn alloc_named(&mut self, name: String, description: String) -> IlVarId {
        let id = IlVarId(self.next_id);
        self.next_id += 1;
        self.vars.insert(id, IlVarInfo { name, description} );
        id
    }
    
    fn lookup(&self, name:&str) -> Option<&IlVarId> {
        self.name_lookup.get(name)
    }

    fn emit_statement(&mut self, s: &Statement) {
        match s {
            Statement::Declare { .. } => {},
            Statement::Assign { target, var_type, value } => todo!(),
            Statement::VoidExpression { expression } => todo!(),
            Statement::IfElse { predicate, when_true, when_false } => todo!(),
            Statement::While { predicate, while_true } => todo!(),
            Statement::Return { value } => {

            },
            Statement::TtyOut { value } => todo!(),
        }
    }

    fn emit_expression(&mut self, dest: IlVarId, e: &Expression) {
        match e {
            Expression::Ident(v) => {
                let id = self.name_lookup.get(v).unwrap();
                let src = IlAtom::Var(*id);
                self.body.push(IlInstruction::AssignAtom{dest, src})
            },
            Expression::Number(num_type, val) => {
                assert_eq!(num_type, &NumberType::U8);
                let src = IlAtom::Number(*val);
                self.body.push(IlInstruction::AssignAtom{dest, src})
            },
            Expression::TtyIn() => todo!(),
            Expression::Arithmetic(op, left, right) => {
                let left_tmp = self.alloc_tmp_with_debug(left);
                self.emit_expression(left_tmp, left);
                let right_tmp = self.alloc_tmp_with_debug(right);
                self.emit_expression(right_tmp, right);
                self.body.push(IlInstruction::AssignBinary {
                    dest,
                    op: IlBinaryOp::from(op),
                    src1: left_tmp,
                    src2: IlAtom::Var(right_tmp),
                });
            },
            Expression::Comparison(_, _, _) => todo!(),
            Expression::Deref(_) => todo!(),
            Expression::LocalFieldDeref(_, _) => todo!(),
            Expression::PtrFieldDeref(_, _) => todo!(),
            Expression::AddressOf(_) => todo!(),
            Expression::Index(_, _) => todo!(),
            Expression::Cast { old_type, new_type, value } => todo!(),
            Expression::Call(_) => todo!(),
            Expression::Optimized { original, optimized } => todo!(),
        }
    }
}

impl IlFunction {
    pub fn emit_from(f: &FunctionDefinition) -> IlFunction {
        let id = IlFunctionId(f.name.clone());
        let mut func = IlFunction::new(id);
        
        for (i, (name, var_type)) in f.args.iter().enumerate() {
            func.alloc_named(
                name.clone(),
                format!("Arg{} {} {:?}", i, name, var_type));
        }

        for (name, (scope, var_type)) in f.vars.iter() {
            assert_eq!(scope, &Scope::Local);
            func.alloc_named(
                name.clone(),
                format!("Local {} {:?}", name, var_type));
        }
        
        for s in &f.body {
            func.emit_statement(s);
        }
        
        func
    }
}

pub enum IlUnaryOp {}
pub enum IlBinaryOp {}

impl IlBinaryOp {
    fn from(op: &ArithmeticOperator) -> IlBinaryOp {
        todo!()
    }
}
pub enum IlCmpOp {}

pub enum IlInstruction {
    Label(IlLabelId),
    AssignAtom{ dest: IlVarId, src: IlAtom},
    AssignUnary{ dest: IlVarId, op: IlUnaryOp, src: IlAtom },
    AssignBinary { dest: IlVarId, op: IlBinaryOp, src1: IlVarId, src2: IlAtom },
    ReadMemory {dest: IlVarId, addr: IlAtom },
    WriteMemory {addr: IlAtom, src: IlVarId },
    Goto(IlLabelId),
    IfThenElse {left: IlVarId, op: IlCmpOp, right: IlAtom, then_label: IlLabelId, else_label: IlLabelId},
    Call {ret: IlVarId, f: IlFunctionId, args: Vec<IlVarId> },
    Return { val: IlVarId }
}

pub enum IlAtom {
    Number(u32),
    Var(IlVarId)
}