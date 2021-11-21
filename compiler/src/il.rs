#![allow(dead_code)]

use std::{collections::HashMap, fmt::Debug, hash::Hash};

use crate::*;

#[derive(Clone, Eq, Hash, PartialEq)]
pub struct IlVarId(String);

impl Debug for IlVarId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"{}", self.0)
    }
}

#[derive(Debug, Default)]
pub struct IlVarInfo {
    pub description: String,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct IlLabelId(String);

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct IlFunctionId(String);

pub struct IlFunction {
    pub id: IlFunctionId,
    pub args: Vec<IlVarId>,
    pub body: Vec<IlInstruction>,
    pub vars: HashMap<IlVarId, IlVarInfo>,
    labels: HashSet<IlLabelId>,
    next_temp_num: usize,
    next_label_num: usize,
}

impl IlFunction {
    fn new(id: IlFunctionId) -> IlFunction {
        IlFunction {
            id, 
            args: Vec::new(),
            vars: HashMap::new(),
            body: Vec::new(),
            labels: HashSet::new(),
            next_temp_num: 0,
            next_label_num: 0,
        }
    }

    fn alloc_tmp_with_debug<D: Debug>(&mut self, d: D) -> IlVarId {
        self.alloc_tmp(format!("{:?}", d))
    }

    fn alloc_tmp(&mut self, description: String) -> IlVarId {
        let id = IlVarId(format!("t{}", self.next_temp_num));
        self.next_temp_num += 1;
        assert!(self.vars.insert(id.clone(), IlVarInfo { description }).is_none());
        id
    }

    fn alloc_named(&mut self, name: String, description: String) -> IlVarId {
        let id = IlVarId(name);
        assert!(self.vars.insert(id.clone(), IlVarInfo { description} ).is_none());
        id
    }

    fn alloc_label(&mut self, info: &str) -> IlLabelId {
        let id = IlLabelId(format!("{}_{}", info, self.next_label_num));
        self.next_label_num += 1;
        assert!(self.labels.insert(id.clone()));
        id
    }

    fn emit_target_expression(&mut self, target: &Expression) -> IlVarId {
        match target {
            Expression::Ident(n) => IlVarId(n.clone()),
            Expression::Number(_, _) => todo!(),
            Expression::TtyIn() => todo!(),
            Expression::Arithmetic(_, _, _) => todo!(),
            Expression::Comparison(_) => todo!(),
            Expression::Deref(_) => todo!(),
            Expression::LocalFieldDeref(_, _) => todo!(),
            Expression::PtrFieldDeref(_, _) => todo!(),
            Expression::AddressOf(_) => todo!(),
            Expression::Index(base, index) => {
                let index_reg =  self.alloc_tmp_with_debug(index);
                self.emit_expression(index_reg.clone(), index);
                todo!();
            },
            Expression::Cast { old_type, new_type, value } => todo!(),
            Expression::Call(_) => todo!(),
            Expression::Optimized { original, optimized } => todo!(),
        }
    }
    
    fn emit_statement(&mut self, s: &Statement) {
        match s {
            Statement::Declare { .. } => {},
            Statement::Assign { target, var_type: _, value } => {
                let target = self.emit_target_expression(target);
                self.emit_expression(target, value);
            },
            Statement::VoidExpression { expression } => {
                let value_reg = self.alloc_tmp_with_debug(&expression);
                self.emit_expression(value_reg, expression);
            },
            Statement::IfElse { predicate, when_true, when_false } => {
                let true_label= self.alloc_label("if true");
                let false_label= self.alloc_label("if false");
                let end_label= self.alloc_label("if end");

                let left = self.alloc_tmp_with_debug(&predicate.left);
                self.emit_expression(left.clone(), &predicate.left);
                let right = self.alloc_tmp_with_debug(&predicate.right);
                self.emit_expression(right.clone(), &predicate.right);
                
                self.body.push(IlInstruction::IfThenElse{
                    left, 
                    op: predicate.op.into(), 
                    right: IlAtom::Var(right),
                    then_label: true_label.clone(),
                    else_label: false_label.clone(),
                });

                self.body.push(IlInstruction::Label(true_label.clone()));
                for s in when_true {
                    self.emit_statement(s);
                }
                self.body.push(IlInstruction::Goto(end_label.clone()));

                self.body.push(IlInstruction::Label(false_label.clone()));
                for s in when_false {
                    self.emit_statement(s);
                }

                self.body.push(IlInstruction::Label(end_label.clone()));


            },
            Statement::While { predicate, while_true } => {
                let pred_label = self.alloc_label("while predicate");
                let body_label= self.alloc_label("while body");
                let end_label= self.alloc_label("while end");

                self.body.push(IlInstruction::Label(pred_label.clone()));

                let left = self.alloc_tmp_with_debug(&predicate.left);
                self.emit_expression(left.clone(), &predicate.left);
                let right = self.alloc_tmp_with_debug(&predicate.right);
                self.emit_expression(right.clone(), &predicate.right);
                
                self.body.push(IlInstruction::IfThenElse{
                    left, 
                    op: predicate.op.into(), 
                    right: IlAtom::Var(right),
                    then_label: body_label.clone(),
                    else_label: end_label.clone(),
                });

                self.body.push(IlInstruction::Label(body_label.clone()));

                for s in while_true {
                    self.emit_statement(s);
                }

                self.body.push(IlInstruction::Goto(pred_label.clone()));

                self.body.push(IlInstruction::Label(end_label.clone()));
            },
            Statement::Return { value } => {
                let val = self.alloc_tmp_with_debug(&value);
                self.emit_expression(val.clone(), value);
                self.body.push(IlInstruction::Return{val})
            },
            Statement::TtyOut { value } => {
                let value_reg = self.alloc_tmp_with_debug(&value);
                self.emit_expression(value_reg.clone(), value);
                self.body.push(IlInstruction::TtyOut{src: value_reg})
            },
        }
    }

    fn emit_expression(&mut self, dest: IlVarId, e: &Expression) {
        match e {
            Expression::Ident(v) => {
                let src = IlAtom::Var(IlVarId(v.clone()));
                self.body.push(IlInstruction::AssignAtom{dest, src});
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
                let left_tmp = self.alloc_tmp_with_debug(left);
                self.emit_expression(left_tmp.clone(), left);
                let right_tmp = self.alloc_tmp_with_debug(right);
                self.emit_expression(right_tmp.clone(), right);
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
            let id = func.alloc_named(
                name.clone(),
                format!("Arg{} {} {:?}", i, name, var_type));
            func.args.push(id);
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

#[derive(Debug)]
pub enum IlUnaryOp {
    Negate,
    BinaryInvert
}

#[derive(Debug)]
pub enum IlBinaryOp {
    Add,
    Subtract,
}

impl IlBinaryOp {
    fn from(op: &ArithmeticOperator) -> IlBinaryOp {
        match op {
            ArithmeticOperator::Add => IlBinaryOp::Add,
            ArithmeticOperator::Subtract => IlBinaryOp::Subtract,
            ArithmeticOperator::Multiply => todo!(),
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

#[derive(Debug)]
pub enum Size {
    U8,
    U32,
}

pub enum IlInstruction {
    Label(IlLabelId),
    AssignAtom{ dest: IlVarId, src: IlAtom},
    AssignUnary{ dest: IlVarId, op: IlUnaryOp, src: IlAtom },
    AssignBinary { dest: IlVarId, op: IlBinaryOp, src1: IlVarId, src2: IlAtom },
    ReadMemory {dest: IlVarId, addr: IlAtom, size: Size},
    WriteMemory {addr: IlAtom, src: IlVarId},
    Goto(IlLabelId),
    IfThenElse {left: IlVarId, op: IlCmpOp, right: IlAtom, then_label: IlLabelId, else_label: IlLabelId},
    Call {ret: IlVarId, f: IlFunctionId, args: Vec<IlVarId> },
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
            Self::ReadMemory { dest, addr, size } => f.debug_struct("ReadMemory").field("dest", dest).field("addr", addr).field("size", size).finish(),
            Self::WriteMemory { addr, src } => f.debug_struct("WriteMemory").field("addr", addr).field("src", src).finish(),
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
            Self::U32(arg0) => write!(f, "{}u32", arg0),
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

pub struct IlProgram {
    pub entry: IlFunctionId,
    pub functions: HashMap<IlFunctionId, IlFunction>,
}

struct SimulationContext<'a> {
    p: &'a IlProgram,
}

impl IlProgram {
    pub fn from_program(ctxt: &ProgramContext) -> IlProgram {
        let mut il = IlProgram {
            entry: IlFunctionId(ctxt.entry.clone()),
            functions: HashMap::new()
        };

        for (_, def) in &ctxt.function_defs {
            let f = IlFunction::emit_from(def);
            il.functions.insert(f.id.clone(), f);
        }

        il
    }

    pub fn simulate(&self, args: &[IlNumber]) -> IlNumber {
        let mut ctxt = SimulationContext {
            p: &self,
        };

        let entry = self.functions.get(&self.entry).unwrap();
        entry.simulate(&mut ctxt, args)
    }
}

impl IlAtom {
    fn value(&self, vars: &HashMap<&IlVarId, IlNumber>) -> IlNumber {
        match self {
            IlAtom::Number(n) => *n,
            IlAtom::Var(v) => vars[v],
        }
    }
}

impl IlFunction {
    fn find_label(&self, label: &IlLabelId) -> Option<usize> {
        self.body.iter()
            .enumerate()
            .filter_map(|(i,s)| {
                if let IlInstruction::Label(l) = s {
                    if l == label {
                        return Some(i);
                    }
                }
                None
            })
            .next()
    }

    fn simulate<'a>(&self, ctxt: &mut SimulationContext<'a>, args: &[IlNumber]) -> IlNumber {
        let mut vars: HashMap<&IlVarId, IlNumber> = HashMap::new();

        assert_eq!(self.args.len(), args.len());
        for (arg_id, arg_value) in self.args.iter().zip(args) {
            vars.insert(arg_id, *arg_value);
        }

        let mut mem: HashMap<u32, u8> = HashMap::new();
        let mut s_index = 0;
        loop {
            let mut inc_pc = true;

            let s = &self.body[s_index];
            dbg!(s);

            match s {
                IlInstruction::Label(_) => {},
                IlInstruction::AssignAtom { dest, src } => {
                    vars.insert(dest, src.value(&vars));
                },
                IlInstruction::AssignUnary { dest, op, src } => {
                    let src = src.value(&vars);
                    let result = match src {
                        IlNumber::U32(n) => {
                            IlNumber::U32(match op {
                                IlUnaryOp::Negate => n.wrapping_neg(),
                                IlUnaryOp::BinaryInvert => !n,
                            })
                        }
                        IlNumber::U8(n) => {
                            IlNumber::U8(match op {
                                IlUnaryOp::Negate => n.wrapping_neg(),
                                IlUnaryOp::BinaryInvert => !n,
                            })
                        }
                    };
                    vars.insert(dest, result);
                },
                IlInstruction::AssignBinary { dest, op, src1, src2 } => {
                    let src1 = vars[src1];
                    let src2 = src2.value(&vars);                    
                    let result = match (src1, src2) {
                        (IlNumber::U8(n1), IlNumber::U8(n2)) => {
                            IlNumber::U8(match op {
                                IlBinaryOp::Add => n1.wrapping_add(n2),
                                IlBinaryOp::Subtract => n1.wrapping_sub(n2),
                            })
                        }
                        (IlNumber::U32(n1), IlNumber::U32(n2)) => {
                            IlNumber::U32(match op {
                                IlBinaryOp::Add => n1.wrapping_add(n2),
                                IlBinaryOp::Subtract => n1.wrapping_sub(n2),
                            })
                        }
                        _ => panic!(),
                    };
                    
                    vars.insert(dest, result);
                }
                IlInstruction::ReadMemory { dest, addr, size } => {
                    let addr = addr.value(&vars);
                    if let IlNumber::U32(addr) = addr {
                        match size {
                            Size::U8 => {
                                vars.insert(dest, IlNumber::U8(mem[&addr]));
                            }
                            Size::U32 => {
                                let r = u32::from_le_bytes([
                                    mem[&(addr+0)],
                                    mem[&(addr+1)],
                                    mem[&(addr+2)],
                                    mem[&(addr+3)],
                                ]);
                                vars.insert(dest, IlNumber::U32(r));
                            }
                        }
                    } else {
                        panic!();
                    }
                }
                IlInstruction::WriteMemory { addr, src} => {
                    let src = vars[src];
                    let addr = addr.value(&vars);
                    if let IlNumber::U32(addr) = addr {
                        match src {
                            IlNumber::U8(n) => {
                                mem.insert(addr, n);
                            },
                            IlNumber::U32(n) => {
                                let bytes = n.to_le_bytes();
                                for (i, b) in bytes.iter().enumerate() {
                                    let i = i as u32;
                                    mem.insert(addr + i, *b);
                                }
                            },
                        }
                    } else {
                        panic!();
                    }
                },
                IlInstruction::Goto(label) => {
                    inc_pc = false;
                    s_index = self.find_label(label).unwrap();
                },
                IlInstruction::IfThenElse { left, op, right, then_label, else_label } => {
                    let left = vars[left];
                    let right = right.value(&vars);
                    let condition_true = match (left, right) {
                        (IlNumber::U8(left), IlNumber::U8(right)) => {
                            match op {
                                IlCmpOp::Equals => left == right,
                                IlCmpOp::NotEquals => left != right,
                                IlCmpOp::GreaterThan => left > right,
                                IlCmpOp::GreaterThanOrEqual => left >= right,
                                IlCmpOp::LessThan => left < right,
                                IlCmpOp::LessThanOrEqual => left <= right,
                            }
                        }
                        (IlNumber::U32(left), IlNumber::U32(right)) => {
                            match op {
                                IlCmpOp::Equals => left == right,
                                IlCmpOp::NotEquals => left != right,
                                IlCmpOp::GreaterThan => left > right,
                                IlCmpOp::GreaterThanOrEqual => left >= right,
                                IlCmpOp::LessThan => left < right,
                                IlCmpOp::LessThanOrEqual => left <= right,
                            }
                        }
                        _ => panic!(),
                    };

                    inc_pc = false;
                    s_index = self.find_label(if condition_true {then_label} else {else_label}).unwrap();
                },
                IlInstruction::Call { ret, f, args } => {
                    let f = &ctxt.p.functions[f];
                    let args: Vec<_> = args.iter().map(|a| vars[a]).collect();
                    let result = f.simulate(ctxt, &args);
                    vars.insert(ret, result);
                },
                IlInstruction::Return { val } => {
                    return vars[val];
                },
                IlInstruction::TtyIn { dest } => todo!(),
                IlInstruction::TtyOut { src } => todo!(),
            }

            if inc_pc {
                s_index += 1;
            }

            dbg!(&vars);
        }
    }
}


#[cfg(test)]
mod tests {
    use crate::tests::test_programs_dir;

    use super::*;

    fn emit_il(entry: &str, input: &str) -> IlProgram {
        let ctxt = create_program(
            entry, 
            input,
            &test_programs_dir());
        
        IlProgram::from_program(&ctxt)
    }

    #[test]
    fn halt() {
        let il = emit_il(
            "main",
            include_str!("../../programs/halt.j"));

        assert_eq!(IlNumber::U8(1), il.simulate(&[]));
    }

    #[test]
    fn divide() {
        let il = emit_il(
            "divide",
            include_str!("../../programs/divide.j"));

        assert_eq!(il.simulate(&[1u8.into(), 1u8.into()]), 1u8.into());
        assert_eq!(il.simulate(&[2u8.into(), 1u8.into()]), 2u8.into());
        assert_eq!(il.simulate(&[1u8.into(), 2u8.into()]), 0u8.into());
        assert_eq!(il.simulate(&[100u8.into(), 10u8.into()]), 10u8.into());
    }

    #[test]
    fn if_ne_uptr() {
        let il = emit_il(
            "main",
            include_str!("../../programs/if_ne_uptr.j"));

        assert_eq!(il.simulate(&[0xAABBCCDDu32.into(), 0xAABBCCDEu32.into()]), 1u8.into());

        // test_inputs(
        //     "main",
        //     include_str!("../../programs/if_ne_uptr.j"),
        //     &[
        //         (6u32,7u32,1u8),(8,7,1),(0,7,1),(0xFF,7,1), (7,7,0),
        //         (0x0,0x100,1),(0x100,0x100,0),(0xAABBCCDD,0xAABBCCDD,0),
        //         (0xAABBCCDD,0xAABBCCDE,1),(0xAABBCCDD,0xAABBCDDD,1),
        //         (0xAABBCCDD,0xAABCCCDD,1),(0xAABBCCDD,0xABBBCCDD,1)]);
    }

    // #[test]
    // fn local_array() {
    //     let il = emit_il(
    //         "main",
    //         include_str!("../../programs/local_array.j"));

    //     assert_eq!(il.simulate(&[0u8.into(), 0u8.into()]), 0u8.into());
    // }
}