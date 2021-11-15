use std::collections::HashMap;

pub struct IlVarId(String);
pub struct IlLabelId(String);
pub struct IlFunctionId(String);

pub struct IlProgram {
    pub functions: HashMap<IlFunctionId, IlFunction>,
}

pub struct IlFunction {
    pub id: IlFunctionId,
    pub args: Vec<IlVarId>,
    pub body: Vec<IlInstruction>,
}

pub enum IlUnaryOp {}
pub enum IlBinaryOp {}
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