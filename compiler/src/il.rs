#![allow(dead_code)]

use std::{collections::{BTreeMap, VecDeque, hash_map::DefaultHasher, HashSet}, convert::TryFrom, fmt::{Debug, Display}, ops::Range, borrow::Cow, hash::{Hash, Hasher}};

use topological_sort::TopologicalSort;

use crate::*;

pub fn emit_il(path: &Path, entry: &str, input: &str, root: &Path) -> (ProgramContext, IlProgram) {
    let ctxt = create_program(path, entry, input, root);
    let p =  IlProgram::from_program(&ctxt);
    (ctxt, p)
}

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub enum IlOperand {
    Number(Number),
    Var(IlVarId)
}
impl IlOperand {
    fn try_get_const<'a>(&'a self, func: &'a IlFunction) -> Option<Cow<'a, Constant>> {
        match self {
            IlOperand::Number(n) => Some(Cow::Owned((*n).into())),
            IlOperand::Var(ident) => func.try_get_const_var(ident).map(|c| Cow::Borrowed(c)),
        }
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum IlInstruction {
    Comment(String),
    Label(IlLabelId),
    GetConstAddress{ dest: IlVarId, const_name: IlVarId },
    AssignNumber{ dest: IlVarId, src: Number },
    AssignVar{ dest: IlVarId, src: IlVarId, size: NumberType, src_range: Option<Range<u32>>, dest_range: Option<Range<u32>>},
    AssignUnary{ dest: IlVarId, op: IlUnaryOp, src: IlVarId },
    AssignBinary { dest: IlVarId, op: BinaryOp, src1: IlVarId, src2: IlOperand },
    ReadMemory {dest: IlVarId, addr: IlVarId, size: NumberType},
    WriteMemory {addr: IlVarId, src: IlVarId, size: NumberType},
    Goto(IlLabelId),
    IfThenElse {left: IlVarId, op: IlCmpOp, right: IlVarId, then_label: IlLabelId, else_label: IlLabelId},
    Call {ret: Option<IlVarId>, f: IlFunctionId, args: Vec<IlVarId> },
    Resize {dest: IlVarId, dest_size: NumberType, src:IlVarId, src_size: NumberType},
    Return { val: Option<IlVarId> },
    TtyIn { dest: IlVarId },
    TtyOut { src: IlVarId },
    Unreachable,
}

impl IlInstruction {
    pub fn is_branch(&self) -> bool {
        match self {
            IlInstruction::Goto(..) | IlInstruction::IfThenElse { .. } | IlInstruction::Return { .. } => true,
            _ => false,
        }
    }

    pub fn is_label(&self) -> bool {
        match self {
            IlInstruction::Label(_) => true,
            _ => false,
        }
    }

    pub fn has_side_effects(&self) -> bool {
        match self {
            IlInstruction::WriteMemory {..} | 
            IlInstruction::Call { .. } |
            IlInstruction::TtyIn { .. } |
            IlInstruction::TtyOut { .. } => true,
            _ => false
        }
    }
}

#[derive(Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct IlVarId(pub String);

impl IlVarId {
    pub fn frame_pointer_str() -> &'static str {
        "__frame_pointer"
    }

    pub fn frame_pointer() -> IlVarId {
        IlVarId(Self::frame_pointer_str().to_owned())
    }
}

impl Debug for IlVarId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"{}", self.0)
    }
}

impl Display for IlVarId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"{}", self.0)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IlVarInfo {
    pub description: String,
    pub location: IlLocation,
    pub var_type: Type,
    pub byte_size: u32,
    pub constant: Option<Constant>,
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct IlLabelId(pub String);

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct IlFunctionId(pub String);

#[derive(Clone, Debug, Default)]
pub struct SourceContext {
    pub contexts: VecDeque<String>,
}

impl SourceContext {
    fn push<D: Debug>(&mut self, d: &D) {
        self.contexts.push_back(format!("{:?}", d));
    }

    fn pop(&mut self) {
        self.contexts.pop_back();
    }
}

#[derive(Clone, Debug)]
pub struct IlFunction {
    pub id: IlFunctionId,
    pub attributes: BTreeSet<FunctionAttribute>,
    pub args: Vec<IlVarId>,
    pub body: Vec<(IlInstruction, Option<Source>, SourceContext)>,
    pub vars: BTreeMap<IlVarId, IlVarInfo>,
    pub ret: Option<NumberType>,
    labels: BTreeSet<IlLabelId>,
    full_expression_hashes: BTreeMap<u16,u64>,
    end_label: IlLabelId,
    next_temp_num: usize,
    next_label_num: usize,
    pub vars_stack_size: u32,
    pub intrinsic: Option<Intrinsic>,
    frame_pointer_size_instruction_index: Option<(IlVarId,usize)>,
}

#[derive(Debug)]
struct TargetLocation {
    target: IlVarId,
    target_subrange: Option<Range<u32>>,
    mem_size: Option<NumberType>
}

impl Display for IlFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "# {:?}(", self.id)?;
        for a in &self.args {
            write!(f, "{:?},", a)?;
        }
        writeln!(f, ")")?;
        for (i, (il, src, _)) in self.body.iter().enumerate() {
            writeln!(f, "# {:02}: {:?} # {:?}", i, il, src)?;
        }
        Ok(())
    }
}

impl IlFunction {

    fn new(id: IlFunctionId, attributes: BTreeSet<FunctionAttribute>, intrinsic: Option<Intrinsic>) -> IlFunction {
        IlFunction {
            end_label: IlLabelId(format!("function_end_{}", &id.0)),
            id,
            attributes,
            args: Vec::new(),
            vars: BTreeMap::new(),
            body: Vec::new(),
            labels: BTreeSet::new(),
            full_expression_hashes: BTreeMap::new(),
            next_temp_num: 0,
            next_label_num: 0,
            vars_stack_size: 0,
            intrinsic,
            frame_pointer_size_instruction_index: None,
            ret: None,
        }
    }

    pub fn isr_name() -> &'static str {
        "isr"
    }

    pub fn is_isr(&self) -> bool {
        self.id.0 == Self::isr_name()
    }

    fn increase_stack_size(&mut self, additional_bytes: u32) {
        if additional_bytes == 0 {
            return;
        }

        let (stack_size_var, instruction_index) = self.frame_pointer_size_instruction_index.as_ref().unwrap();
        let (mut existing, src, src_ctxt) = self.body.remove(*instruction_index);
        match &mut existing {
            IlInstruction::AssignNumber { dest, src } => {
                assert_eq!(dest, stack_size_var);
                assert_eq!(src.as_u32(), self.vars_stack_size);
                self.vars_stack_size += additional_bytes;
                *src = Number::U32(self.vars_stack_size);
            },
            _ => panic!(),
        }
        self.body.insert(*instruction_index, (existing, src, src_ctxt));
    }

    fn add_inst(&mut self, ctxt: &IlContext, inst: IlInstruction) {
        self.body.push((inst, ctxt.src.as_ref().map(|s| s.clone()), ctxt.src_ctxt.clone()));
    }

    fn alloc_tmp(&mut self, info: IlVarInfo) -> IlVarId {
        let id = IlVarId(format!("t{}", self.next_temp_num));
        assert!(self.vars.insert(id.clone(), info).is_none());
        self.next_temp_num += 1;
        id
    }

    fn alloc_tmp_for_expression(&mut self, ctxt: &IlContext, e: &Expression) -> (IlVarId, IlVarInfo) {
        let mut hasher = DefaultHasher::new();
        e.hash(&mut hasher);
        let full_hash =  hasher.finish();
        let brief_hash = (full_hash & 0xFFFF) as u16;
        let id = IlVarId(format!("t{:0x}", brief_hash));

        match self.full_expression_hashes.get(&brief_hash) {
            None => {
                let t = e.try_emit_type(ctxt.program, Some(ctxt.func_def))
                    .unwrap_or_else(|| panic!("Could not determine type for '{:?}'.", e));
                let il_type: NumberType = match t.byte_count(ctxt.program) {
                    0 => NumberType::U8,
                    n => n.try_into().unwrap()
                };
                let location = IlLocation::Reg(il_type);
                let var_type = e.try_emit_type(ctxt.program, Some(ctxt.func_def)).unwrap();
                let byte_size = var_type.byte_count(ctxt.program);
                let constant = e.try_get_const();
                if let Some(constant) = &constant {
                    assert_eq!(byte_size, constant.byte_count())
                }
                let info = IlVarInfo {
                    description: format!("{} {:?}", &id.0, e),
                    byte_size,
                    var_type,
                    location,
                    constant,
                };
                if Some(&info) != self.vars.get(&id) {
                    assert!(self.vars.insert(id.clone(), info.clone()).is_none(),
                        "Could not add {}={:?} as it is already {:?}.", &id, &info, self.vars[&id]);
                }
            }
            Some(existing) => {
                assert_eq!(existing, &full_hash);
            }
        }
        
        let (id, info) = self.vars.get_key_value(&id).unwrap();
        (id.clone(), info.clone())
    }

    fn alloc_tmp_and_emit_value(&mut self, ctxt: &mut IlContext, e: &Expression) -> (IlVarId, IlVarInfo) {
        ctxt.src_ctxt.push(e);
        let (id, info) = self.alloc_tmp_for_expression(ctxt, e);
        self.emit_expression(ctxt, id.clone(), e);
        ctxt.src_ctxt.pop();
        (id, info)
    }

    fn alloc_named(&mut self, name: String, info: IlVarInfo) -> IlVarId {
        let id = IlVarId(name);
        assert!(self.vars.insert(id.clone(), info).is_none());
        id
    }

    fn alloc_label(&mut self, info: &str) -> IlLabelId {
        let id = IlLabelId(format!("{}_{}_{}", self.id.0, info, self.next_label_num));
        self.next_label_num += 1;
        assert!(self.labels.insert(id.clone()));
        id
    }

    fn alloc_tmp_and_emit_static_address(&mut self, ctxt: &IlContext, addr: u32, name: &str, info: &IlVarInfo) -> IlVarId {
        let var_type = Type::Ptr(Box::new(info.var_type.clone()));
        let addr_var = self.alloc_tmp(IlVarInfo {
            description: format!("static {:?} addr", &name),
            location: IlLocation::Reg(NumberType::U32),
            byte_size: var_type.byte_count(ctxt.program),
            var_type,
            constant: None,
        });
        self.add_inst(ctxt, IlInstruction::AssignNumber {
                dest: addr_var.clone(),
                src: Number::U32(addr),
            });
        addr_var
    }

    // fn alloc_tmp_and_emit_const_address(&mut self, ctxt: &IlContext, name: &str, info: &IlVarInfo) -> IlVarId {
    //     let const_name = match &info.location {
    //         IlLocation::Const(name) => name.clone(),
    //         _ => panic!(),
    //     };
    //     let var_type = Type::Ptr(Box::new(info.var_type.clone()));
    //     let addr_var = self.alloc_tmp(IlVarInfo {
    //         description: format!("const {:?}", &name),
    //         location: IlLocation::Reg(IlType::U32),
    //         byte_size: var_type.byte_count(ctxt.program),
    //         var_type
    //     });
    //     self.add_inst(ctxt, IlInstruction::GetConstAddress {
    //             dest: addr_var.clone(),
    //             const_name,
    //         });
    //     addr_var
    // }

    fn emit_address(&mut self, ctxt: &mut IlContext, value: &Expression) -> (IlVarId, Option<u32>) {
        // dbg!(&value);
        match value {
            Expression::Ident(n) => {
                let info = ctxt.find_arg_or_var(&self, n);
                match info.location {
                    IlLocation::Reg(_) => 
                        panic!("Cannot emit address of '{}' because it is in a register.", n)
                    ,
                    IlLocation::FrameOffset(offset) => {
                        let addr = Expression::Arithmetic(
                            BinaryOp::Add,
                            Box::new(Expression::frame_pointer()),
                            Box::new(Expression::Number(Number::U32(offset))),
                        );
                        let (id, _) = self.alloc_tmp_and_emit_value(ctxt, &addr);
                        (id, None)
                    },
                    IlLocation::Static(addr) => {
                        let size = info.var_type.byte_count(ctxt.program).try_into().ok();
                        let id = self.alloc_tmp_and_emit_static_address(ctxt, addr, n, &info);
                        (id, size)
                    }
                    IlLocation::GlobalConst(name) => {
                        (name.clone(), Some(ctxt.il.find_const(&self.id, &name).unwrap().byte_size))
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
                            NumberType::U8 | NumberType::U16 => {
                                Box::new(Expression::Cast {
                                    old_type: Some(index_type),
                                    new_type: Type::Number(NumberType::U32),
                                    value: index.clone()
                                })
                            },
                            NumberType::U32 => {
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
                        BinaryOp::Multiply,
                        index.clone(),
                        Box::new(Expression::Number(Number::U32(element_size))),
                    ))
                };

                let base_addr_num_expression = match info.location {
                    IlLocation::Static(addr) => Expression::Number(Number::U32(addr)),
                    IlLocation::Reg(_) => {
                        Expression::Cast {
                            old_type: Some(ptr_type.clone()),
                            new_type: Type::Number(NumberType::U32),
                            value: Box::new(Expression::Ident(var.clone())),
                        }
                    },
                    IlLocation::FrameOffset(offset) => {
                        let frame = Expression::frame_pointer();
                        if offset == 0 {
                            frame
                        } else {
                            Expression::Arithmetic(
                                BinaryOp::Add,
                                Box::new(frame),
                                Box::new(Expression::Number(Number::U32(offset))),
                            )
                        }
                    },
                    IlLocation::GlobalConst(_) => {
                        let ptr_type = match &ptr_type {
                            Type::Array(et, count) => 
                                Type::Ptr(Box::new(Type::Array(et.clone(), *count))),
                            _ => todo!()
                        };
                        Expression::Cast {
                            old_type: Some(ptr_type),
                            new_type: Type::Number(NumberType::U32),
                            value: Box::new(Expression::Ident(var.clone())),
                        }
                    },
                };

                let addr_expression = Expression::Arithmetic(
                    BinaryOp::Add,
                    Box::new(base_addr_num_expression),
                    byte_index_expression,
                );

                let addr_expression = Expression::Cast{
                    value: Box::new(addr_expression),
                    old_type: Some(Type::Number(NumberType::U32)),
                    new_type: Type::Ptr(Box::new(element_type.clone())),
                };

                let (addr, _) = self.alloc_tmp_and_emit_value(ctxt, &addr_expression);

                (addr, Some(element_size.try_into().unwrap()))
            }
            Expression::LocalFieldDeref(n, field) => {
                let info = ctxt.find_arg_or_var(&self, n);
                let struct_type = match &info.var_type { 
                    Type::Struct(struct_type) => {
                        &ctxt.program.struct_types[struct_type]
                    }
                    t => panic!("Expected Struct but found {:?}", t)
                };
                let (byte_offset, field_type) = struct_type.get_field(ctxt.program, field);
                let field_bytes = field_type.byte_count(ctxt.program).try_into().unwrap();

                let base_expression = match info.location {
                    IlLocation::Static(addr) => Expression::Number(Number::U32(addr)),
                    IlLocation::Reg(_) => todo!(),
                    IlLocation::FrameOffset(offset) => {
                        let frame = Expression::frame_pointer();
                        if offset == 0 {
                            frame
                        } else {
                            Expression::Arithmetic(
                                BinaryOp::Add,
                                Box::new(frame),
                                Box::new(Expression::Number(Number::U32(offset))))
                        }
                    },
                    IlLocation::GlobalConst(_) => todo!(),
                };

                let addr_expression = if byte_offset == 0 {
                    base_expression
                } else {
                    Expression::Arithmetic(
                        BinaryOp::Add,
                        Box::new(base_expression),
                        Box::new(Expression::Number(Number::U32(byte_offset))),
                    )
                };

                let addr_expression = Expression::Cast {
                    value: Box::new(addr_expression),
                    old_type: Some(Type::Number(NumberType::U32)),
                    new_type: Type::Ptr(Box::new(field_type.clone()))
                };

                let (addr, _) = self.alloc_tmp_and_emit_value(ctxt, &addr_expression);

                (addr, Some(field_bytes))
            }
            Expression::PtrFieldDeref(n, field) => {
                let info = ctxt.find_arg_or_var(&self, n);
                let ptr_type = info.var_type;
                let element_type = ptr_type.get_element_type().expect(
                    &format!("Could not find element type for '{:?}' for {}->{}", ptr_type, n, field)
                );
                let struct_type = match element_type { 
                    Type::Struct(struct_type) => {
                        &ctxt.program.struct_types[struct_type]
                    }
                    _ => panic!()
                };
                let (byte_offset, field_type) = struct_type.get_field(ctxt.program, field);
                let field_bytes = field_type.byte_count(ctxt.program).try_into().ok();

                let base_expression = match info.location {
                    IlLocation::Static(_) => {
                        todo!()
                    }
                    IlLocation::Reg(_) => {
                        Expression::Cast {
                            old_type: Some(ptr_type.clone()),
                            new_type: Type::Number(NumberType::U32),
                            value: Box::new(Expression::Ident(n.clone())),
                        }
                    },
                    IlLocation::FrameOffset(_) => todo!(),
                    IlLocation::GlobalConst(_) => todo!(),
                };

                let addr_expression = if byte_offset == 0 {
                    base_expression
                } else {
                    Expression::Arithmetic(
                        BinaryOp::Add,
                        Box::new(base_expression),
                        Box::new(Expression::Number(Number::U32(byte_offset))),
                    )
                };

                let addr_expression = Expression::Cast {
                    value: Box::new(addr_expression),
                    old_type: Some(Type::Number(NumberType::U32)),
                    new_type: Type::Ptr(Box::new(field_type.clone()))
                };

                let (addr, _) = self.alloc_tmp_and_emit_value(ctxt, &addr_expression);

                (addr, field_bytes)
            }
            _ => todo!(),
        }
    }

    fn emit_target_expression(&mut self, ctxt: &mut IlContext, target: &Expression) -> TargetLocation {
        // dbg!(target);
        match target {
            Expression::Ident(n) => {
                let info= ctxt.find_arg_or_var(&self, n);
                match info.location {
                    IlLocation::Reg(_) => {
                        TargetLocation {
                            target: IlVarId(n.clone()),
                            target_subrange: None,
                            mem_size: None
                        }
                    },
                    IlLocation::FrameOffset(_) | IlLocation::Static(_) => {
                        let (address, size) = self.emit_address(ctxt, target);
                        TargetLocation {
                            target: address,
                            target_subrange: None,
                            mem_size: Some(size.unwrap().try_into().unwrap())
                        }
                    },
                    IlLocation::GlobalConst(_) => panic!("cannot write to const"),
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
                    mem_size: Some(size.unwrap().try_into().unwrap())
                }
            }
            Expression::Index(n, index) => {
                let info = ctxt.find_arg_or_var(&self, n);

                match (&info.location, info.var_type) {
                    (IlLocation::Reg(NumberType::U32), Type::Number(NumberType::U32)) => {
                        let index = index.try_get_const();
                        match (info.location, index) {
                            (IlLocation::Reg(NumberType::U32), Some(Constant::Number(index))) => {
                                let index = index.as_u32();
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
                            mem_size: Some(size.unwrap().try_into().unwrap()),
                        }
                    }
                    other => panic!("Don't know how to index into {:?}", other)
                }
            },
            _ => panic!(),
        }
    }

    fn emit_comparison(&mut self, ctxt: &mut IlContext, c: &Comparison, true_label: IlLabelId, false_label: IlLabelId) {
        ctxt.src_ctxt.push(c);

        let (left, left_info) = self.alloc_tmp_and_emit_value(ctxt, &c.left);
        let (right, right_info) = self.alloc_tmp_and_emit_value(ctxt, &c.right);

        assert_eq!(left_info.var_type, right_info.var_type);

        self.add_inst(
            ctxt,
            IlInstruction::IfThenElse{
                left: left.clone(),
                op: c.op.into(), 
                right: right.clone(),
                then_label: true_label.clone(),
                else_label: false_label.clone(),
            });
        
        ctxt.src_ctxt.pop();
    }
    
    fn emit_statement(&mut self, ctxt: &mut IlContext, s: &Statement) {
        
        ctxt.src_ctxt.contexts.clear();
        ctxt.src_ctxt.push(s);

        // dbg!(&s);

        match s {
            Statement::Declare { .. } => {},
            Statement::Assign { target, var_type, value } => {
                let emitted_type = match self.emit_target_expression(ctxt, target) {
                    TargetLocation {target, target_subrange: None, mem_size: Some(size)} => {
                        let (value_reg, info) = self.alloc_tmp_and_emit_value(ctxt, value);
                        assert_eq!(size.byte_count(), info.var_type.byte_count(ctxt.program), "{:?}", s);
                        self.add_inst(ctxt, IlInstruction::WriteMemory{addr: target, src: value_reg.clone(), size});
                        info.var_type
                    }
                    TargetLocation {target, target_subrange: None, mem_size: None} => {
                        self.emit_expression(ctxt, target.clone(), value);
                        self.vars[&target].var_type.clone()
                    }
                    TargetLocation {target, target_subrange: Some(dest_subrange), mem_size: None} => {
                        let size: NumberType = (dest_subrange.end - dest_subrange.start).try_into().unwrap();
                        let (tmp, tmp_info) = self.alloc_tmp_and_emit_value(ctxt, value);
                        assert_eq!(size.byte_count(), tmp_info.byte_size);

                        self.add_inst(ctxt, IlInstruction::AssignVar {
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

                let end_label= self.alloc_label("if_end");

                let mut prev_false = None;
                for (i, (predicate, when_true)) in if_blocks.iter().enumerate() {
                    if let Some(prev_false) = prev_false.take() {
                        self.add_inst(ctxt, IlInstruction::Label(prev_false));
                    }
                    let true_label = self.alloc_label(&format!("if_true_{}", i));
                    let false_label = self.alloc_label(&format!("if_else_{}", i));
                    self.emit_comparison(ctxt, predicate, true_label.clone(), false_label.clone());
                    self.add_inst(ctxt, IlInstruction::Label(true_label));
                    for s in when_true {
                        self.emit_statement(ctxt, s);
                    }
                    self.add_inst(ctxt, IlInstruction::Goto(end_label.clone()));
                    prev_false = Some(false_label);
                }

                self.add_inst(ctxt, IlInstruction::Label(prev_false.unwrap()));
                for s in else_block {
                    self.emit_statement(ctxt, s);
                }

                self.add_inst(ctxt, IlInstruction::Label(end_label.clone()));
            },
            Statement::While { predicate, while_true } => {
                let pred_label = self.alloc_label("while_predicate");
                let body_label= self.alloc_label("while_body");
                let end_label= self.alloc_label("while_end");

                self.add_inst(ctxt, IlInstruction::Label(pred_label.clone()));

                self.emit_comparison(ctxt, predicate, body_label.clone(), end_label.clone());

                self.add_inst(ctxt, IlInstruction::Label(body_label.clone()));

                let loop_ctxt = LoopContext { 
                    break_label: end_label.clone(),
                    continue_label: pred_label.clone()
                };

                ctxt.loops.push_back(loop_ctxt.clone());
                for s in while_true {
                    self.emit_statement(ctxt, s);
                }
                assert_eq!(Some(loop_ctxt), ctxt.loops.pop_back());

                self.add_inst(ctxt, IlInstruction::Goto(pred_label.clone()));

                self.add_inst(ctxt, IlInstruction::Label(end_label.clone()));
            },
            Statement::Continue => {
                let closest_loop = ctxt.loops.iter().last().expect("Continue must be in loop");
                self.add_inst(ctxt, IlInstruction::Goto(closest_loop.continue_label.clone()));
            },
            Statement::Break => {
                let closest_loop = ctxt.loops.iter().last().expect("Break must be in loop");
                self.add_inst(ctxt, IlInstruction::Goto(closest_loop.break_label.clone()));
            },
            Statement::Return { value } => {
                let val = if let Some(value) = value {
                    let (val,_) = self.alloc_tmp_and_emit_value(ctxt, value);
                    Some(val.clone())
                } else {
                    None
                };

                if self.vars_stack_size > 0 {
                    self.add_inst(ctxt, IlInstruction::AssignBinary {
                        dest: IlVarId::frame_pointer(),
                        op: BinaryOp::Add,
                        src1: IlVarId::frame_pointer(),
                        src2: IlOperand::Number(Number::U32(self.vars_stack_size)),
                    });
                }
                
                self.add_inst(ctxt, IlInstruction::Return{val})
            },
            Statement::TtyOut { value } => {
                let (value_reg, _) = self.alloc_tmp_and_emit_value(ctxt, &value);
                self.add_inst(ctxt, IlInstruction::TtyOut{src: value_reg.clone()})
            },
        }
        ctxt.src_ctxt.pop();
    }

    fn try_get_const<'a, 'b>(&self, e: &'a Expression, var_type: Option<&'b Type>) -> Option<Constant> {
        if let Some(val) = e.try_get_const() {
            return Some(val);
        }

        let var_type = if let Some(var_type) = var_type { var_type } else { return None; };
        match e {
            Expression::AddressOf(addr_e) => {
                let inner_type = if let Type::Ptr(inner_type) = var_type { inner_type } else { return None; };

                match addr_e.as_ref() {
                    Expression::Ident(name) => {
                        let info = if let Some(info) = self.vars.get(&IlVarId(name.clone())) { info } else { return None; };
                        assert_eq!(inner_type.as_ref(), &info.var_type);

                        if let Some(info) = self.vars.get(&IlVarId(name.clone())) {
                            match info.location {
                                IlLocation::Static(addr) => Some(Number::U32(addr).into()),
                                _ => None
                            }
                        } else {
                            None
                        }
                    }
                    _ => None
                }
            }
            Expression::Ident(name) => {
                let info = if let Some(info) = self.vars.get(&IlVarId(name.clone())) { info } else { return None; };
                assert_eq!(var_type, &info.var_type);
                match &info.location {
                    IlLocation::Reg(_) => None,
                    IlLocation::FrameOffset(_) => None,
                    IlLocation::Static(_) => None,//Some(*addr),
                    IlLocation::GlobalConst(const_name) => {
                        assert_eq!(name.as_str(), &const_name.0);
                        match var_type {
                            Type::Void => panic!(),
                            Type::Number(nt) => {
                                let num = match info.constant.as_ref() {
                                    Some(Constant::Number(n)) => *n,
                                    c => panic!("unexpected {:?}", c)
                                };
                                assert_eq!(num.num_type().byte_count(), nt.try_byte_count().unwrap());
                                Some(num.into())
                            },
                            Type::Ptr(_) | Type::Struct(_) | Type::Array(_, _) => None,
                        }
                    },
                }
            },
            _ => {
                None
            }
        }
    }

    fn emit_expression(&mut self, ctxt: &mut IlContext, dest: IlVarId, e: &Expression) {
        // println!("START {:?}", e);
        ctxt.src_ctxt.push(e);

        let var_type = ctxt.try_emit_type(e);

        let cons = self.try_get_const(e, var_type.as_ref());

        // if let Some(cons) = &cons {
        //     println!("# const {:?} -> {:?}", e, cons);
        // } else {
        //     // println!("# Not const {:?}", e);
        // }

        let e = match (cons, var_type, e) {
            (_, _, Expression::Number(_)) => Cow::Borrowed(e),
            (Some(Constant::Number(n)), Some(var_type), _) => {
                let byte_count = var_type.byte_count(ctxt.program);
                assert_eq!(byte_count, n.num_type().byte_count());

                let same_type = if let Type::Number(nt2) = var_type {
                    if n.num_type() == nt2 {
                        Some(Expression::Number(n))
                    } else {
                        None
                    }
                } else {
                    None
                };

                let const_exp = Cow::Owned(if let Some(same_type) = same_type {
                    same_type
                } else {
                    Expression::Cast {
                        old_type: Some(Type::Number(n.num_type())),
                        new_type: var_type,
                        value: Box::new(Expression::Number(n))
                    }
                });

                // println!("CONST {:?} -> {:?}", e, const_exp);

                const_exp
            }
            _=> Cow::Borrowed(e),
        };

        match e.as_ref() {
            Expression::Ident(name) => {
                let info = ctxt.find_arg_or_var(&self, name);
                // dbg!(&info);
                match info.location {
                    IlLocation::Reg(_) => {
                        let src = IlVarId(name.clone());
                        let size = info.location.try_into().unwrap();
                        self.add_inst(ctxt, IlInstruction::AssignVar{dest, src, size, src_range: None, dest_range: None});
                    }
                    IlLocation::FrameOffset(_) => todo!(),
                    IlLocation::Static(addr) => {
                        let addr = self.alloc_tmp_and_emit_static_address(ctxt, addr, name, &info);

                        self.add_inst(ctxt, IlInstruction::ReadMemory {
                            dest,
                            addr,
                            size: info.byte_size.try_into().unwrap(),
                        })
                    },
                    IlLocation::GlobalConst(name)  => {
                        self.add_inst(ctxt, IlInstruction::GetConstAddress { dest, const_name: name.clone() });
                    }
                }
            },
            Expression::Number(n) => {
                let src = n.clone();
                self.add_inst(ctxt, IlInstruction::AssignNumber{dest, src});
            },
            Expression::TtyIn() => {
                self.add_inst(ctxt, IlInstruction::TtyIn {dest});
            },
            Expression::Arithmetic(op, left, right) => {

                let (left, right) = match (op.is_commutative(), left.try_get_const(), right.try_get_const()) {
                    (true, Some(_), None) => (right, left),
                    (true, Some(_), Some(_)) => panic!(),
                    _ => (left, right),
                };

                let orig_left_type = ctxt.try_emit_type(left).expect(&format!("Could not find type for {:?}.", left));
                let orig_left_type = orig_left_type.get_number_type().expect(&format!("Expected Number, but is {:?}.", orig_left_type));
                let orig_right_type = ctxt.try_emit_type(right).unwrap().get_number_type().unwrap();

                let promo_needed = orig_left_type != orig_right_type;

                let promote = |side: &Box<Expression>, side_type| {
                    if promo_needed && side_type == NumberType::U8 {
                        let val = if let Some(Constant::Number(c)) = side.try_get_const() {
                            Expression::Number(c)
                        } else {
                            Expression::Cast {
                                value: side.clone(),
                                old_type: Some(Type::Number(side_type)),
                                new_type: Type::Number(NumberType::U32)
                            }
                        };
                        (val, NumberType::U32)
                    } else {
                        (*(side.clone()), side_type)
                    }
                };

                let (left, promoted_num_type) = promote(left, orig_left_type);
                let (right, promoted_num_type_right) = promote(right, orig_right_type);

                assert_eq!(promoted_num_type, promoted_num_type_right);

                let (left_tmp, left_info) = self.alloc_tmp_and_emit_value(ctxt, &left);
                assert_eq!(promoted_num_type.byte_count(), left_info.var_type.byte_count(ctxt.program));

                let right_operand = if let Some(Constant::Number(constant)) = right.try_get_const() {
                    let constant = constant.as_u32();
                    let right_val = match promoted_num_type {
                        NumberType::U8 => Number::U8(constant.try_into().unwrap()),
                        NumberType::U16 => Number::U16(constant.try_into().unwrap()),
                        NumberType::U32 => Number::U32(constant.try_into().unwrap()),
                    };
                    assert_eq!(left_info.byte_size, right_val.num_type().byte_count());

                    IlOperand::Number(right_val)
                } else {
                    let (right_tmp, right_info) = self.alloc_tmp_and_emit_value(ctxt, &right);
                    assert_eq!(left_info.byte_size, right_info.byte_size);
                    IlOperand::Var(right_tmp)
                };


                self.add_inst(ctxt, IlInstruction::AssignBinary {
                    dest,
                    op: BinaryOp::from(*op),
                    src1: left_tmp.clone(),
                    src2: right_operand,
                });
            },
            Expression::Comparison(_) => todo!(),
            Expression::Deref(ptr) => {
                let (addr, ptr_info) = self.alloc_tmp_and_emit_value(ctxt, ptr);
                let derefed_type = ptr_info.var_type.get_element_type().unwrap();
                self.add_inst(ctxt, IlInstruction::ReadMemory {
                    dest,
                    addr,
                    size: derefed_type.byte_count(ctxt.program).try_into().unwrap(),
                });
            },
            Expression::LocalFieldDeref(_,_) |
            Expression::PtrFieldDeref(_,_) => {
                let (addr, size) = self.emit_address(ctxt, e.as_ref());
                self.add_inst(ctxt, IlInstruction::ReadMemory {
                    dest,
                    addr,
                    size: size.unwrap().try_into().unwrap(),
                })
            },
            Expression::AddressOf(n) => {
                let (addr, _mem_size) = self.emit_address(ctxt, n);
                self.add_inst(ctxt, IlInstruction::AssignVar {
                    dest,
                    src: addr,
                    size: NumberType::U32,
                    src_range: None,
                    dest_range: None,
                });
            },
            Expression::Index(n, index) => {
                let info = ctxt.find_arg_or_var(&self, n);
                // dbg!(&info);
                match (&info.location, info.var_type) {
                    (IlLocation::Reg(NumberType::U32), Type::Number(NumberType::U32)) => {
                        let index = index.try_get_const();
                        match (info.location, index) {
                            (IlLocation::Reg(NumberType::U32), Some(Constant::Number(index))) => {
                                let index = index.as_u32();
                                self.add_inst(ctxt, IlInstruction::AssignVar {
                                    dest,
                                    dest_range: None,
                                    src: IlVarId(n.clone()),
                                    src_range: Some(index..index+1),
                                    size: NumberType::U8,
                                });
                            }
                            _ => todo!(),
                        }
                    }
                    (_, Type::Ptr(..)) | (_, Type::Array(..)) =>  {
                        let (addr, size) = self.emit_address(ctxt, e.as_ref());
                        self.add_inst(ctxt, IlInstruction::ReadMemory {
                            dest,
                            addr,
                            size: size.unwrap().try_into().unwrap(),
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

                        self.add_inst(ctxt, IlInstruction::Resize {
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

                self.add_inst(ctxt, IlInstruction::Call {
                    f: IlFunctionId(c.function.clone()),
                    ret,
                    args: params,
                });
            },
            Expression::Array(_) => todo!(),
        
        }
        // println!("END   {:?}", &e);
        // println!("{:#?}", &self.body);
        ctxt.src_ctxt.pop();
    }

    fn emit_from(ctxt: &mut IlContext) -> IlFunction {
        let id = IlFunctionId(ctxt.func_def.name.clone());

        let intrinsic = match &ctxt.func_def.body {
            FunctionImpl::Body(_) => None,
            FunctionImpl::Intrinsic(i) => Some(i.clone()),
        };

        let mut func = IlFunction::new(id.clone(), ctxt.func_def.attributes.clone(), intrinsic);

        let return_size = ctxt.func_def.return_type.byte_count(ctxt.program);
        func.ret = match return_size {
            0 => None,
            n => Some(n.try_into().unwrap())
        };

        // bring global consts into scope
        for (name, (var_type, val)) in &ctxt.program.consts {
            assert!(func.vars.insert(
                IlVarId(name.to_owned()),
                IlVarInfo {
                    description: format!("global const {:?}", &name),
                    location: IlLocation::GlobalConst(IlVarId(name.to_owned())),
                    byte_size: var_type.byte_count(ctxt.program),
                    var_type: var_type.clone(),
                    constant: Some(val.clone()),
                }).is_none());
        }

        // bring global statics into scope
        for ((f, name), info) in &ctxt.il.statics {
            if f.is_some() {
                continue;
            }
            assert!(func.vars.insert(
                name.to_owned(),
                IlVarInfo {
                    description: format!("local view of global static {:?}", &name),
                    location: info.location.clone(),
                    byte_size: info.byte_size,
                    var_type: info.var_type.clone(),
                    constant: None,
                }).is_none());
        }
        
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
                    constant: None,
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
                    let addr = IlProgram::alloc_static(&mut ctxt.next_static_addr, var_type, ctxt.program);
                    let location = IlLocation::Static(addr);
                    assert!(
                        ctxt.il.statics.insert(
                            (Some(id.clone()), IlVarId(name.clone())),
                            IlVarInfo {
                                location: location.clone(),
                                var_type: var_type.clone(),
                                description: format!("function-local static {} addr", name),
                                byte_size: var_type.byte_count(ctxt.program),
                                constant: None,
                            }
                        ).is_none()
                    );
                    location
                },
            };
            

            func.alloc_named(
                name.clone(),
                IlVarInfo {
                    location: size.clone(),
                    description: format!("Local {} {:?} {:?}", name, var_type, size),
                    var_type: var_type.clone(),
                    byte_size: var_type.byte_count(ctxt.program),
                    constant: None,
                });
        }

        if func.vars_stack_size > 0 {
            func.alloc_named(
                IlVarId::frame_pointer_str().to_owned(),
                IlVarInfo {
                    description: IlVarId::frame_pointer_str().to_owned(),
                    location: IlLocation::Reg(NumberType::U32),
                    var_type: Type::Number(NumberType::U32),
                    byte_size: 4,
                    constant: None,
                });

            let stack_size = func.alloc_tmp(IlVarInfo {
                byte_size: 4,
                description: "Stack size negated".to_owned(),
                location: IlLocation::Reg(NumberType::U32),
                var_type: Type::Number(NumberType::U32),
                constant: Some(Number::U32(func.vars_stack_size.wrapping_neg()).into()),
            });

            func.frame_pointer_size_instruction_index = Some((stack_size.clone(), func.body.len()));
            func.add_inst(ctxt, IlInstruction::AssignBinary {
                dest: IlVarId::frame_pointer(),
                op: BinaryOp::Add,
                src1: IlVarId::frame_pointer(),
                src2: IlOperand::Number(Number::U32(func.vars_stack_size.wrapping_neg())),
            });
        }

        match &ctxt.func_def.body {
            FunctionImpl::Body(body) => {
                for (s, src) in body {
                    ctxt.src = Some(src.clone());
                    func.emit_statement(ctxt, s);
                    ctxt.src = None;
                }
            },
            FunctionImpl::Intrinsic(_) => {
                func.add_inst(ctxt, IlInstruction::Unreachable);
            },
        }
        
        if ctxt.func_def.return_type == Type::Void {
            func.emit_statement(ctxt, &Statement::Return{value: None });
        }

        func.labels.insert(func.end_label.clone());
        func.add_inst(ctxt, IlInstruction::Label(func.end_label.clone()));

        func.add_inst(ctxt, IlInstruction::Unreachable);

        func.optimize();
        
        func
    }


    fn find_blocks(&self) -> BTreeMap<usize, Block> {
        let mut labels = BTreeMap::new();
        for (i, (il, _, _)) in self.body.iter().enumerate() {
            if let IlInstruction::Label(label) = il {
                labels.insert(label, i);
            }
        }

        let mut blocks = BTreeMap::new();
        let mut block_start = 0;
        for (i, (il, _, _)) in self.body.iter().enumerate() {
            let range = if il.is_branch() {
                block_start..(i+1)
            } else if il.is_label() {
                block_start..i
            } else {
                continue;
            };

            blocks.insert(
                block_start,
                Block { stmt_indices: range.clone(), outgoing_blocks: vec![]});

            block_start = range.end;
        }

        for b in blocks.values_mut() {
            for i in b.stmt_indices.clone() {
                if let (IlInstruction::Goto(label), _, _) = &self.body[i] {
                    b.outgoing_blocks.push(labels[&label]);
                }
            } 
        }

        blocks
    }

    fn find_refs(&self) -> BTreeMap<IlVarId, VarReferences> {
        let mut refs = BTreeMap::new();

        for v in self.vars.keys() {
            refs.insert(v.clone(), VarReferences { read_indices: BTreeSet::new(), write_indices: BTreeSet::new()});
        }

        for (index, usages) in self.body.iter().map(|s| s.0.var_usages()).enumerate() {
            
            if let Some(dest) = usages.dest {
                let refs = refs.get_mut(dest).unwrap();
                refs.write_indices.insert(index);
            }

            for src in usages.srcs {
                let refs = refs.get_mut(src).unwrap();
                refs.read_indices.insert(index);
            }
        }

        refs
    }
    
    fn optimize(&mut self) {
        let mut round = 0;
        while self.optimze_round() {
            println!("# In {:?}, optimizing round #{}", self.id, round);
            round += 1;
        }
    }

    fn optimze_round(&mut self) -> bool {
        // dbg!(&self.id);

        if self.intrinsic.is_some() {
            return false;
        }

        // remove unused vars
        let refs = self.find_refs();
        {
            let mut removed = false;
            for (v, refs) in &refs {
                if refs.read_indices.is_empty() && refs.write_indices.is_empty() && !self.args.contains(v) {
                    self.vars.remove(v);
                    removed = true;
                }
            }
            if removed { return true; }
        }

        // remove unread vars and instructions that write to them
        {
            for (v, refs) in &refs {
                if let IlLocation::Static(_) = self.vars[v].location {
                    // writes to statics need to be visible outside the function
                    continue;
                }

                if refs.read_indices.is_empty() {
                    for write_index in refs.write_indices.iter().rev() {
                        if self.body[*write_index].0.has_side_effects() {
                            continue;
                        }

                        let removed = self.body.remove(*write_index);
                        println!("# In {:?}, var `{}` is written to, but never read. Removing the instruction that writes to it: {:?}", self.id, v, removed);                        
                        return true;
                    }
                }
            }
        }


        // propogate consts
        {
            let mut consts_found = false;
            let to_try: Vec<_> = self.vars.iter()
                .filter_map(|(name,info)| 
                    if info.constant.is_none() { 
                        let unique_write_insts: HashSet<_> = refs[name].write_indices.iter().map(|i| &self.body[*i].0).collect();
                        if unique_write_insts.len() == 1 { Some(name.clone()) } else { None }
                    } else { 
                        None 
                    }).collect();
            for name in &to_try {
                let const_val = {
                    let (only_write_instruction, _, _) = &self.body[*refs[name].write_indices.first().unwrap()];
                    only_write_instruction.try_get_const(&self)
                };
                if let Some(const_val) = const_val {
                    println!("# In {:?}, resolved the constant val `{:?}` for var `{}`.", self.id, &const_val, name);
                    self.vars.get_mut(name).unwrap().constant = Some(const_val);
                    consts_found = true;
                } else {
                    // println!("# In {:?}, could not resolve var `{}` to a constant.", self.id, name);
                }
            }

            if consts_found {
                return true;
            }
        }

        // replace operations with known constants
        {
            let mut replaced = false;
            for (il, _source, ctxt) in self.body.iter_mut() {
                if let IlInstruction::AssignNumber { .. } = il {
                    continue;
                }

                if let Some(dest) = il.var_usages().dest {
                    if let Some(Constant::Number(const_val)) = &self.vars[dest].constant {
                        let new = IlInstruction::AssignNumber { dest: dest.clone(), src: *const_val };
                        let msg = format!("# In {:?}, replacing `{:?}` with constant `{:?}`.", self.id, il, &new);
                        println!("{}", &msg);
                        ctxt.contexts.push_back(msg);
                        *il = new;
                        replaced = true;
                    }
                }
            }

            if replaced { return true; }
        }

        // check for no-op math
        {
            let mut to_remove = None;
            for (i, (il, _source, _ctxt)) in self.body.iter().enumerate() {
                match il {
                    IlInstruction::AssignVar { dest, src, size:_, src_range: None, dest_range: None } => {
                        if refs[dest].write_indices.len() == 1 && refs[src].write_indices.len() == 0 {
                            assert_eq!(refs[dest].write_indices.first().unwrap(), &i);
                            println!("# In {:?}, `{}` is never written to, but is copied to `{}` - which is also never modified. \
                                      Replacing refs to latter with the former and deleting the copy: {:?}", self.id, src, dest, il);
                            to_remove = Some((i, dest.clone(), src.clone()));
                            break;
                        }
                    }
                    IlInstruction::AssignBinary { dest, op, src1, src2: IlOperand::Number(n)} => {
                        match (op, n.as_u32()) {
                            (BinaryOp::Add | BinaryOp::Subtract | BinaryOp::BitwiseOr, 0) |
                            (BinaryOp::ShiftLeft | BinaryOp::ShiftRight | BinaryOp::RotateLeft | BinaryOp::RotateRight, 0) |
                            (BinaryOp::Multiply, 1) => {
                                if refs[dest].write_indices.len() == 1 {
                                    println!("# In {:?}, removing no-op il instruction: {:?}", self.id, il);
                                    to_remove = Some((i, dest.clone(), src1.clone()));
                                    break;
                                }
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                }
            }

            if let Some((to_remove_index, to_remove_var, replace_with_var)) = to_remove {

                // HexFile::print_commented(&self);
                // println!("# {:?} VarRefs:", &self.id);
                // for (var, refs) in &refs {
                //     if !refs.read_indices.is_empty() || !refs.write_indices.is_empty() {
                //         println!("#  {var}");
                //         for i in &refs.read_indices {
                //             println!("#   read: {} {:?}", *i, self.body[*i].0);
                //         }
                //         for i in &refs.write_indices {
                //             println!("#   write: {} {:?}", *i, self.body[*i].0);
                //         }
                //     }
                // }

                let var_refs = &refs[&to_remove_var];

                for read_index in &var_refs.read_indices {
                    let usages = self.body[*read_index].0.var_usages_mut();
                    for read_var in  usages.srcs{
                        if read_var == &to_remove_var {
                            *read_var = replace_with_var.clone();
                        }
                    }
                }

                self.body.remove(to_remove_index);
                return true;
            }
        }

        // remove no-op jmps
        {
            let mut no_op_goto_indices = Vec::new();
            for (goto_index, pair) in self.body.as_slice().windows(2).enumerate() {
                match (&pair[0].0, &pair[1].0) {
                    (IlInstruction::Goto(target), IlInstruction::Label(label)) if (target == label) => {
                        no_op_goto_indices.push(goto_index);
                    }
                    _ => {}
                }
            }

            for goto_index in no_op_goto_indices.iter().rev() {
                let removed = self.body.remove(*goto_index).0;
                println!("# In {:?}, removing no-op goto instruction #{}: {:?}", self.id, goto_index, removed);
            }

            if !no_op_goto_indices.is_empty() {
                return true;
            }
        }

        // remove unreferenced labels
        {
            let mut unreferenced = self.labels.clone();
            unreferenced.remove(&self.end_label);
            for (il, _, _) in self.body.iter() {
                if let IlInstruction::Label(_) = &il {
                    // these don't count
                } else {
                    for l in il.labels() {
                        unreferenced.remove(l);
                    }
                }
            }

            if unreferenced.len() > 0 {
                let id = self.id.clone();
                self.body.retain_mut(|(il,_,_)| {
                    if let IlInstruction::Label(l) = &il {
                        if unreferenced.contains(l) {
                            println!("# In {:?}, removing unreferenced label: {:?}", id, il);
                            false
                        } else {
                            true
                        }
                    } else {
                        true
                    }
                });

                self.labels.retain(|l| !unreferenced.contains(l));
                return true;
            }
        }

        // merge back-to-back labels
        {
            let mut duplicate_label_indices = Vec::new();
            for (first_index, pair) in self.body.as_slice().windows(2).enumerate() {
                match (&pair[0].0, &pair[1].0) {
                    (IlInstruction::Label(_), IlInstruction::Label(_)) => {
                        duplicate_label_indices.push(first_index);
                    }
                    _ => {}
                }
            }

            for first_index in duplicate_label_indices.iter().rev() {
                let old_labels = [
                    self.body[*first_index + 0].0.labels_mut()[0].clone(),
                    self.body[*first_index + 1].0.labels_mut()[0].clone(),
                ];
                let new_label = if old_labels.iter().any(|l| l == &self.end_label) {
                    self.end_label.clone()
                } else if old_labels[0].0.len() < old_labels[1].0.len() {
                    old_labels[0].clone()
                } else if old_labels[0].0.len() > old_labels[1].0.len() {
                    old_labels[1].clone()
                } else {
                    IlLabelId(format!("{}_MERGED_{}", old_labels[0].0, old_labels[1].0))
                };

                println!("# In {:?}, merging duplicate labels at #{} && #{}: '{:?}' and '{:?}' to '{:?}'",
                    self.id, first_index, first_index + 1, old_labels[0], old_labels[1], &new_label);
                for (inst, _, _) in self.body.iter_mut() {
                    for l in inst.labels_mut() {
                        if old_labels.iter().any(|old| old == l) {
                            *l = new_label.clone();
                        }
                    }
                }

                self.body.remove(*first_index + 1);
                self.labels.remove(&old_labels[0]);
                self.labels.remove(&old_labels[1]);
                self.labels.insert(new_label);
            }

            if !duplicate_label_indices.is_empty() {
                return true;
            }
        }

        let blocks = self.find_blocks();

        // remove duplicate writes of consts within a block
        {
            let mut to_remove_indices = BTreeSet::new();
            for (_, b) in blocks {
                // println!("# found block {}..={}", block_start, block_end);

                for (v, info) in &self.vars {
                    if info.constant.is_none() {
                        continue;
                    }

                    let writes_in_block: BTreeSet<_> = refs[v].write_indices.iter()
                        .filter(|i| b.stmt_indices.contains(*i))
                        .cloned()
                        .collect();
                    if writes_in_block.len() < 2 {
                        continue;
                    }
                    let mut previous_write_index: Option<usize> = None;
                    for write_index in writes_in_block {
                        if let Some(prev) = previous_write_index {
                            if self.body[prev].0 == self.body[write_index].0 {
                                println!("# In {:?}, in block from {:?}, removing repetitive write at {} because it is the same as at {}: {:?}", 
                                    self.id, b.stmt_indices, write_index, prev, self.body[write_index].0);
                                to_remove_indices.insert(write_index);
                            }
                        }

                        previous_write_index = Some(write_index);
                    }
                }
            }

            for index in to_remove_indices.iter().rev() {
                let _ = self.body.remove(*index);
            }

            if !to_remove_indices.is_empty() {
                return true;
            }
        }

        // merge vars with the same const
        {
            let mut var_to_replacement = BTreeMap::new();
            let mut consts: BTreeMap<&Constant, &IlVarId> = BTreeMap::new();
            for (name, info) in &self.vars {
                if let Some(ref c) = info.constant {
                    if let Some(existing) = consts.get(c) {
                        assert!(var_to_replacement.insert(name.clone(), (*existing).clone()).is_none());
                    } else {
                        assert!(consts.insert(c, name).is_none());
                    }
                }
            }

            for (orig_var, replacement_var) in &var_to_replacement {
                for var_ref_index in refs[orig_var].read_indices.iter().chain(refs[orig_var].write_indices.iter()) {
                    let original = self.body[*var_ref_index].0.clone();
                    let mut replaced = original.clone();
                    let vars = replaced.var_usages_mut();
                    for usage in vars.srcs{
                        if usage == orig_var {
                            *usage = replacement_var.clone();
                        }
                    }
                    if let Some(usage) = vars.dest {
                        if usage == orig_var {
                            *usage = replacement_var.clone();
                        }
                    }
                    let msg = format!("# In {:?}, at #{}, replacing reference to {} with {} as they share the same const value: {:?}. {:?} -> {:?}", 
                        self.id, var_ref_index, orig_var, replacement_var, self.vars[orig_var].constant.as_ref().unwrap(), &original, &replaced);
                    println!("{}", &msg);
                    
                    let (il, _, src_ctxt) = self.body.get_mut(*var_ref_index).unwrap();
                    *il = replaced;
                    src_ctxt.contexts.push_back(msg);
                }
            }

            if !var_to_replacement.is_empty() {
                return true;
            }
        }

        // for offset in [0,1] {
        //     for pair in self.body.as_mut_slice()[offset..].chunks_mut(2) {
        //         if pair.len() != 2 { continue; }
        //         let (first,second) = pair.split_first_mut().unwrap();
        //         let second = &mut second[0];
        //         if let (IlInstruction::AssignNumber { dest: num_dest, src },
        //                 IlInstruction::AssignBinary { dest, op, src1, src2 }) = (&mut first.0, &mut second.0) {
        //             if num_dest == src2 && 
        //                *src == IlNumber::U32(4) && 
        //                *op == IlBinaryOp::Multiply
        //             {
        //                 let refs: &VarReferences = &refs[num_dest];
        //                 if refs.reads.len() == 1 && refs.writes.len() == 1 {
        //                     // println!("# MUL4 {:?} {:?} {:?} {:?} {:?} {:?}", num_dest, src, dest, op, src1, src2);
        //                     first.0 = IlInstruction::AssignBinary {
        //                         dest: dest.clone(),
        //                         op:IlBinaryOp::Add,
        //                         src1: src1.clone(),
        //                         src2: src1.clone(),
        //                     };
        //                     second.0 = IlInstruction::AssignBinary {
        //                         dest: dest.clone(),
        //                         op: IlBinaryOp::Add,
        //                         src1: dest.clone(),
        //                         src2: dest.clone(),
        //                     };
        //                 }
        //             }
        //         }
        //     }
        // }
        

        false
    }

    fn try_get_const_var(&self, ident: &IlVarId) -> Option<&Constant> {
        self.vars[ident].constant.as_ref()
    }
}

#[derive(Debug)]
struct VarReferences {
    read_indices: BTreeSet<usize>,
    write_indices: BTreeSet<usize>,
}

#[derive(Debug)]
struct Block {
    stmt_indices: Range<usize>,
    outgoing_blocks: Vec<usize>,
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum IlUnaryOp {
    Negate,
    BinaryInvert
}

#[derive(Clone, Eq, PartialEq, Hash)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IlLocation {
    Reg(NumberType),
    FrameOffset(u32),
    Static(u32),
    GlobalConst(IlVarId),
}

impl TryFrom<u32> for IlLocation {
    type Error = ();

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        value.try_into()
            .map(|il_type| IlLocation::Reg(il_type))
    }
}

impl TryFrom<IlLocation> for NumberType {
    type Error = ();

    fn try_from(value: IlLocation) -> Result<Self, Self::Error> {
        match value {
            IlLocation::Reg(il_type) => Ok(il_type),
            _ => Err(())
        }
    }
}

#[derive(Debug)]
pub struct IlUsages<'a> {
    pub srcs: Vec<&'a IlVarId>,
    pub dest: Option<&'a IlVarId>,
}

#[derive(Debug)]
pub struct IlUsagesMut<'a> {
    srcs: Vec<&'a mut IlVarId>,
    dest: Option<&'a mut IlVarId>,
}

impl IlInstruction {
    pub fn labels(&self) -> Vec<&IlLabelId> {
        match self {
            IlInstruction::Label(l) |
            IlInstruction::Goto(l) => vec![l],
            IlInstruction::IfThenElse { left: _, op: _, right: _, then_label, else_label } => 
                vec![then_label, else_label],
            _ => vec![],                
        }
    }

    pub fn labels_mut(&mut self) -> Vec<&mut IlLabelId> {
        match self {
            IlInstruction::Label(l) |
            IlInstruction::Goto(l) => vec![l],
            IlInstruction::IfThenElse { left: _, op: _, right: _, then_label, else_label } => 
                vec![then_label, else_label],
            _ => vec![],                
        }
    }

    pub fn var_usages(&self) -> IlUsages {
        let (srcs, dest) = match self {
            IlInstruction::Unreachable | IlInstruction::Comment(_) => (vec![], None),
            IlInstruction::AssignNumber { dest, src:_} => 
                (vec![], Some(dest)),
            IlInstruction::AssignVar { dest, src, size:_, src_range: _, dest_range: _} => 
                (vec![src], Some(dest)),
            IlInstruction::AssignUnary { dest, op:_, src } => 
                (vec![src], Some(dest)),
            IlInstruction::AssignBinary { dest, op:_, src1, src2 } =>
                match src2 {
                    IlOperand::Number(_) => (vec![src1], Some(dest)),
                    IlOperand::Var(src2) => (vec![src1, src2], Some(dest)),
                },
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
            IlInstruction::GetConstAddress { dest, const_name: _} => (vec![], Some(dest)),
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
                match src2 {
                    IlOperand::Number(_) => (vec![src1], Some(dest)),
                    IlOperand::Var(src2) => (vec![src1, src2], Some(dest)),
                },
            IlInstruction::ReadMemory { dest, addr, size:_ } =>
                (vec![addr], Some(dest)),
            IlInstruction::WriteMemory { addr, src, size:_ } => 
                (vec![addr, src], None),
            IlInstruction::Goto(_) => (vec![], None),
            IlInstruction::IfThenElse { left, op:_, right, then_label:_ , else_label:_ } => 
                (vec![left, right], None),
            IlInstruction::Call { ret, f:_, args} =>
                (args.iter_mut().collect(), ret.as_mut()),
            IlInstruction::Resize { dest, dest_size:_, src, src_size:_ } => 
                (vec![src], Some(dest)),
            IlInstruction::Return { val } => (
                if let Some(v) = val { vec![v] } else {vec![]},
                None),
            IlInstruction::TtyIn { dest } => (vec![], Some(dest)),
            IlInstruction::TtyOut { src } => (vec![src], None),
            IlInstruction::Label(_) => (vec![], None),
            IlInstruction::GetConstAddress { dest, const_name: _} => (vec![], Some(dest)),
        };

        IlUsagesMut { srcs, dest }
    }

    fn try_get_const(&self, func: &IlFunction) -> Option<Constant> {
        match self {
            IlInstruction::AssignNumber { dest:_, src } => Some((*src).into()),
            IlInstruction::AssignVar { dest:_, src, size, src_range, dest_range } => {
                if src_range.is_some() || dest_range.is_some() {
                    return None; // figure this out later
                }
                let src_info = &func.vars[src];
                if let Some(src_const) = &src_info.constant {
                    assert_eq!(size.byte_count(), src_const.byte_count());
                    Some(src_const.clone())
                } else {
                    None
                }
            }
            IlInstruction::AssignBinary { dest: _, op, src1, src2 } => {
                if let (Some(Constant::Number(src1)), Some(src2)) = (&func.vars[src1].constant, src2.try_get_const(func)) {
                    if let Constant::Number(src2) = src2.as_ref() {
                        Some(op.eval(*src1, *src2).into())
                    } else {
                        None
                    }
                } else {
                    None
                }
            }

            _ => None

            // IlInstruction::Comment(_) => todo!(),
            // IlInstruction::Label(_) => todo!(),
            // IlInstruction::GetConstAddress { dest, const_name } => todo!(),
            // IlInstruction::AssignUnary { dest, op, src } => todo!(),
            // IlInstruction::AssignBinary { dest, op, src1, src2 } => todo!(),
            // IlInstruction::ReadMemory { dest, addr, size } => todo!(),
            // IlInstruction::WriteMemory { addr, src, size } => todo!(),
            // IlInstruction::Goto(_) => todo!(),
            // IlInstruction::IfThenElse { left, op, right, then_label, else_label } => todo!(),
            // IlInstruction::Call { ret, f, args } => todo!(),
            // IlInstruction::Resize { dest, dest_size, src, src_size } => todo!(),
            // IlInstruction::Return { val } => todo!(),
            // IlInstruction::TtyIn { dest } => todo!(),
            // IlInstruction::TtyOut { src } => todo!(),
            // IlInstruction::Unreachable => todo!(),
        }
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
            IlInstruction::Call { ret, f: func, args} => {
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
            IlInstruction::GetConstAddress { dest, const_name } => write!(f, "{} <- const &{}", dest, const_name),
        }
    }
}

#[derive(Clone, Debug)]
pub struct IlProgram {
    pub entry: IlFunctionId,
    pub functions: BTreeMap<IlFunctionId, IlFunction>,
    pub statics: BTreeMap<(Option<IlFunctionId>,IlVarId), IlVarInfo>,
    pub consts: BTreeMap<(Option<IlFunctionId>,IlVarId), IlVarInfo>,
    pub image_base_address: u32,
    pub statics_addresses: Range<u32>,
}

impl Display for IlProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (_, func) in &self.functions {
            writeln!(f, "{}", func)?;
        }
        Ok(())
    }
}

impl IlProgram {
    pub fn find_const(&self, f: &IlFunctionId, v: &IlVarId) -> Option<&IlVarInfo> {
        self.consts.get(&(Some(f.clone()), v.clone())).or(self.consts.get(&(None, v.clone())))
    }

    fn alloc_static(next_static_addr: &mut u32, var_type: &Type, ctxt: &ProgramContext) -> u32 {
        let alignment = var_type.alignment(ctxt);
        let addr = round_up(*next_static_addr, alignment);
        
        let byte_count = var_type.byte_count(ctxt);
        *next_static_addr = addr + byte_count;
        addr        
    }

    pub fn from_program(ctxt: &ProgramContext) -> IlProgram {
        let mut il = IlProgram {
            entry: IlFunctionId(ctxt.entry.clone()),
            functions: BTreeMap::new(),
            statics: BTreeMap::new(),
            image_base_address: ctxt.image_base_address,
            statics_addresses: 0..0,
            consts: BTreeMap::new(),
        };
            
        let mut next_static_addr = ctxt.statics_base_address;

        for (name, (var_type, value)) in &ctxt.consts {
            il.consts.insert(
                (None, IlVarId(name.clone())), // TODO consts should be scoped to functions
                IlVarInfo {
                    description: format!("global constant {:?}", var_type),
                    location: IlLocation::GlobalConst(IlVarId(name.clone())),
                    var_type: var_type.clone(),
                    byte_size: var_type.byte_count(ctxt),
                    constant: Some(value.clone()),
                }
            );
        }

        for (name, var_type) in &ctxt.statics {

            let addr = Self::alloc_static(&mut next_static_addr, var_type, ctxt);

            il.statics.insert(
                (None, IlVarId(name.clone())),
                IlVarInfo {
                    description: format!("global static {:?}", var_type),
                    location: IlLocation::Static(addr),
                    var_type: var_type.clone(),
                    byte_size: var_type.byte_count(ctxt),
                    constant: None,
                }
            );
        }

        for (_, def) in &ctxt.function_defs {
            let mut il_ctxt = IlContext {
                next_static_addr,
                program: ctxt,
                il: &mut il,
                func_def: def,
                loops: VecDeque::new(),
                src_ctxt: SourceContext::default(),
                src: None,
            };
            let f = IlFunction::emit_from(&mut il_ctxt);
            next_static_addr = il_ctxt.next_static_addr;
            il.functions.insert(IlFunctionId(def.name.clone()), f);
        }

        il.statics_addresses = ctxt.statics_base_address..next_static_addr;

        il.inline();

        loop {
            let mut called_functions = BTreeSet::new();
            called_functions.insert(il.entry.clone());
            for isr in il.functions.iter().filter(|f| f.1.is_isr()) {
                called_functions.insert(isr.0.clone());
            }
            il.find_calls( |_, callee, _| {
                called_functions.insert(callee.clone());
            });

            let mut function_removed = false;

            let mut new_functions = BTreeMap::new();
            for (id, f) in il.functions.into_iter() {
                if called_functions.contains(&id) {
                    new_functions.insert(id, f);
                } else {
                    function_removed = true;
                }
            }
            il.functions = new_functions;

            if !function_removed {
                break;
            }
        }

        // after inlining, optimize again
        for (_,f) in il.functions.iter_mut() {
            f.optimize();
        }

        il
    }

    fn find_calls<'a, F: FnMut(&'a IlFunction, &'a IlFunctionId, usize)>(&'a self, mut f: F) {
        for (_, caller) in &self.functions {
            for (i, (stmt, _, _)) in caller.body.iter().enumerate() {
                match stmt {
                    IlInstruction::Call { ret: _, f: callee, args: _ } => {
                        f(caller, callee, i);
                    }
                    _ => {}
                }
            }
        }
    }

    fn inline(&mut self) {

        let mut inline_iteration = 0;

        loop {
            let mut locations = Vec::new();
            let mut ts = TopologicalSort::<&IlFunctionId>::new();
            self.find_calls( |caller, callee, i| {
                let callee = &self.functions[callee];
                if   callee.attributes.contains(&FunctionAttribute::Inline) &&
                    (caller.vars.contains_key(&IlVarId::frame_pointer()) || !callee.vars.contains_key(&IlVarId::frame_pointer()))
                {
                    ts.add_dependency(&callee.id, &caller.id);
                    locations.push((&caller.id, &callee.id, i));
                }
            });

            // dbg!(&locations);
            // dbg!(&ts);

            let mut callee_leaf_functions = ts.pop_all();
            callee_leaf_functions.sort();

            let callee_leaf_function = match callee_leaf_functions.get(0) {
                Some(f) => *f,
                None => return,
            };

            // dbg!(callee_leaf_function);

            let (caller, _, instruction_index) = locations.iter()
                .find(|(_, callee, _)| *callee == callee_leaf_function)
                .cloned()
                .unwrap();
            let caller = caller.clone();

            let mut callee_var_to_caller_inlined_var = BTreeMap::new();
            let mut callee_label_to_caller_inlined_label = BTreeMap::new();
            let mut inlined_instructions = Vec::new();

            let mut caller = self.functions.remove(&caller).unwrap();
            let (call, call_src, call_source_ctxt) = caller.body.remove(instruction_index);
            match call {
                IlInstruction::Call { ret, f, args: caller_param_values } => {

                    let callee = &self.functions[&f];

                    // dbg!((&caller.id, &callee.id));
                    if caller.id == callee.id {
                        todo!("recursive not implemented.");
                    }

                    for label in &callee.labels {
                        let inlined_label = IlLabelId(format!("inline_{}_{}_{}_{}", caller.id.0, callee.id.0, inline_iteration, label.0));
                        callee_label_to_caller_inlined_label.insert(
                            label.clone(),
                            inlined_label.clone(),
                        );
                        caller.labels.insert(inlined_label);
                    }

                    // identity mapping for frame pointer
                    for (var_id, var_info) in &callee.vars {
                        if var_id == &IlVarId::frame_pointer() {
                            assert!(caller.vars.contains_key(&IlVarId::frame_pointer()));
                            continue;
                        }

                        let inlined_name = IlVarId(format!("inline_{}_{}_{}_{}", caller.id.0, callee.id.0, inline_iteration, var_id.0));
                        let inlined_location = match &var_info.location {
                            IlLocation::FrameOffset(o) => IlLocation::FrameOffset(caller.vars_stack_size+o),
                            l => l.clone(),
                        };
                        let mut inlined_var = var_info.clone();
                        inlined_var.location = inlined_location;

                        callee_var_to_caller_inlined_var.insert(var_id.clone(), inlined_name.clone());

                        assert!(caller.vars.insert(
                            inlined_name,
                            inlined_var
                        ).is_none());
                    }

                    caller.increase_stack_size(callee.vars_stack_size);

                    assert_eq!(caller_param_values.len(), callee.args.len());
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
                    for (caller_param_value, arg) in caller_param_values.iter().zip(callee.args.iter()) {
                        inlined_instructions.push((IlInstruction::AssignVar {
                            dest: callee_var_to_caller_inlined_var[arg].clone(),
                            dest_range: None,
                            src: caller_param_value.clone(),
                            src_range: None,
                            size: callee.vars[arg].byte_size.try_into().unwrap(),
                        },
                        call_src.clone(),
                        call_source_ctxt.clone()));
                    }

                    for (stmt, src, src_ctxt) in &callee.body {

                        // dbg!(&stmt);

                        if let IlInstruction::Return { val }  = stmt {
                            match (val.as_ref(), (&ret).as_ref()) {
                                (None, None) => { },
                                (Some(val), Some(ret)) => {
                                    inlined_instructions.push((
                                        IlInstruction::AssignVar {
                                            dest: ret.clone(),
                                            dest_range: None, 
                                            src: callee_var_to_caller_inlined_var[val].clone(),
                                            src_range: None,
                                            size: callee.vars[val].byte_size.try_into().unwrap(),
                                        },
                                        src.clone(),
                                        src_ctxt.clone()));

                                    
                                },
                                _ => panic!(),
                            }

                            inlined_instructions.push((
                                IlInstruction::Goto(callee_label_to_caller_inlined_label[&callee.end_label].clone()),
                                src.clone(),
                                src_ctxt.clone(),
                            ));

                            continue;
                        }

                        let mut stmt = stmt.clone();
                        let usages = stmt.var_usages_mut();
                        for src in usages.srcs {
                            *src = callee_var_to_caller_inlined_var[src].clone();
                        }
                        if let Some(dest) = usages.dest {
                            *dest = callee_var_to_caller_inlined_var[dest].clone();
                        }

                        // dbg!(&stmt);

                        match stmt {
                            IlInstruction::Goto(l) => {
                                inlined_instructions.push((
                                    IlInstruction::Goto(callee_label_to_caller_inlined_label[&l].clone()),
                                    src.clone(),
                                    src_ctxt.clone(),
                                ));
                            }
                            IlInstruction::Label(l) => {
                                inlined_instructions.push((
                                    IlInstruction::Label(callee_label_to_caller_inlined_label[&l].clone()),
                                    src.clone(),
                                    src_ctxt.clone(),
                                ));
                            },
                            IlInstruction::IfThenElse { left, op, right, then_label, else_label } => {
                                inlined_instructions.push((
                                    IlInstruction::IfThenElse
                                    {
                                        left,
                                        op,
                                        right,
                                        then_label: callee_label_to_caller_inlined_label[&then_label].clone(),
                                        else_label: callee_label_to_caller_inlined_label[&else_label].clone(),
                                    },
                                    src.clone(),
                                    src_ctxt.clone(),
                                ));
                            }
                            IlInstruction::Return { val:_ } => {
                                panic!("Should be handled above.")
                            }
                            stmt => {
                                inlined_instructions.push((stmt, src.clone(), src_ctxt.clone()));
                            }
                        }
                    }

                }
                _ => panic!(),
            }

            if let Some(IlInstruction::Unreachable) = inlined_instructions.last().map(|(i,_,_)| i) {
                inlined_instructions.pop();
            }

            for i in inlined_instructions.into_iter().rev() {
                caller.body.insert(instruction_index, i)
            }
            self.functions.insert(caller.id.clone(), caller);

            inline_iteration += 1;
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct LoopContext {
    break_label: IlLabelId,
    continue_label: IlLabelId,
}

struct IlContext<'a> {
    next_static_addr: u32,
    program: &'a ProgramContext,
    il: &'a mut IlProgram,
    func_def: &'a FunctionDefinition,
    loops: VecDeque<LoopContext>,
    src: Option<Source>,
    src_ctxt: SourceContext,
}

impl<'a> IlContext<'a> {
    fn find_arg_or_var(&self, il_func: &IlFunction, n: &str) -> IlVarInfo {
        let n = IlVarId(n.to_owned());
        if let Some(var) = il_func.vars.get(&n) {
            return var.clone();
        }

        if let Some(info) = self.il.find_const(&il_func.id, &n) {
            return info.clone();
        }

        // first look at the function
        let static_key = (Some(il_func.id.clone()), n.clone());
        if let Some(var) = self.il.statics.get(&static_key) {
            return var.clone();
        }

        // then global
        let static_key = (None, static_key.1);
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
        // dbg!(&f);

        let labels: BTreeMap<&IlLabelId, usize> = f.body.iter().enumerate()
            .filter_map(|(i,s)| match &s.0 {
                IlInstruction::Label(label) => Some((label, i)),
                _ => None
            })
            .collect();

        // dbg!(&labels);
        
        let succ = f.body.iter().enumerate()
            .map(|(i,s)| {
                let nexts = match &s.0 {
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
                let usages = s.0.var_usages();
                usages.srcs.iter().map(|s| *s).collect()
            }).collect();

        let kill: Vec<BTreeSet<&IlVarId>> = f.body.iter()
            .map(|s| {
                match &s.0 {
                    IlInstruction::AssignVar {dest_range: Some(_), ..} => BTreeSet::new(),
                    _ => {
                        let usages = s.0.var_usages();
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
            // println!("i | succ[i] | gen[i] | kill[i] | inst");
            // for i in 0..len {
            //     println!("{} | {:?} | {:?} | {:?} | {:?}", i, &l.succ[i], &l.gen[i], &l.kill[i], &f.body[i]);
            // }

            let mut run_more = true;

            while run_more {
                // println!("i | outs[i] | ins[i]");
                // for i in 0..len {
                //     println!("{} | {:?} | {:?}", i, &l.outs[i], &l.ins[i]);
                // }

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
                if v1 != v2 && l.interferes.contains_key(v1) && l.interferes.contains_key(v2) {
                    l.interferes.get_mut(v1).unwrap().insert(v2);
                    l.interferes.get_mut(v2).unwrap().insert(v1);
                }
            }
        }

        for (i, inst) in l.f.body.iter().enumerate() {
            for x in &l.kill[i] {
                for y in  &l.outs[i] {
                    if x == y { continue; }
                    match &inst.0 {
                        IlInstruction::AssignVar {dest, src, size:_, src_range:_, dest_range:_}
                            if dest == *x && src == *y => {}
                        _ => {
                            // dbg!(&l, &inst, i, x, y);
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
                    location: IlLocation::Reg(NumberType::U8),
                    var_type: Type::Number(NumberType::U8),
                    constant: None,
                }
            );
        }

        let ctxt = SourceContext::default();

        let f = IlFunction {
            id: IlFunctionId("fib".to_owned()),
            attributes: BTreeSet::new(),
            args: vec![n.clone()],
            next_label_num: 0,
            next_temp_num: 0,
            vars_stack_size: 0, 
            ret: Some(NumberType::U8),
            intrinsic: None,
            frame_pointer_size_instruction_index: None,
            vars,
            labels: ([&loop_label, &body_label, &end_label]).iter().map(|l| (**l).clone()).collect(),
            full_expression_hashes: BTreeMap::new(),
            end_label: IlLabelId("end".to_owned()),
            body : [
                IlInstruction::AssignNumber {dest: one.clone(), src: Number::U8(1)},
                IlInstruction::AssignNumber {dest: a.clone(), src: Number::U8(0)},
                IlInstruction::AssignNumber {dest: b.clone(), src: Number::U8(1)},
                IlInstruction::AssignNumber {dest: z.clone(), src: Number::U8(0)},
                IlInstruction::Label(loop_label.clone()),
                IlInstruction::IfThenElse {left: n.clone(), op: IlCmpOp::Equals, right: z.clone(), 
                    then_label: end_label.clone(), else_label: body_label.clone() },
                IlInstruction::Label(body_label.clone()),
                IlInstruction::AssignBinary { dest: t.clone(), op: BinaryOp::Add, src1: a.clone(), src2: IlOperand::Var(b.clone()) },
                IlInstruction::AssignVar { dest: a.clone(), src: b.clone(), size: NumberType::U8, dest_range: None, src_range: None },
                IlInstruction::AssignVar { dest: b.clone(), src: t.clone(), size: NumberType::U8, dest_range: None, src_range: None },
                IlInstruction::AssignBinary { dest: n.clone(), op: BinaryOp::Subtract, src1: n.clone(), src2: IlOperand::Var(one.clone()) },
                IlInstruction::AssignNumber {dest: z.clone(), src: Number::U8(0)},
                IlInstruction::Goto(loop_label.clone()),
                IlInstruction::Label(end_label.clone()),
                IlInstruction::Return{val: Some(a.clone())},
            ].iter().map(|i| (i.clone(), None, ctxt.clone())).collect(),
        };

        let l = IlLiveness::calculate(&f);
        // dbg!(&l);
        assert_eq!(l.ins[0].iter().next(), Some(&&n.clone()));
    }
}