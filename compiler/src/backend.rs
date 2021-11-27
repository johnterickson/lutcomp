use crate::*;
use crate::il::*;

struct BackendProgram<'a> {
    frontend_context: &'a ProgramContext,
    il: &'a IlProgram,
    function_info: BTreeMap<IlFunctionId, FunctionInfo>,
    available_registers: BTreeSet<u8>,
}

struct FunctionInfo {
    register_assignments: BTreeMap<IlVarId, Vec<u8>>,
    functions_called: BTreeSet<IlFunctionId>,
}

struct FunctionContext<'a,'b> {
    program: &'a BackendProgram<'b>,
    f_il: &'a IlFunction,
    f_info: &'a FunctionInfo,
    lines: &'a mut Vec<AssemblyInputLine>,
}

impl<'a> BackendProgram<'a> {
    fn create_function_infos(&mut self) {
        for f in self.il.functions.values() {
            let mut info = FunctionInfo {
                functions_called: BTreeSet::new(),
                register_assignments: BTreeMap::new(),
            };

            for s in &f.body {
                if let IlInstruction::Call{f: callee, ..} = s {
                    info.functions_called.insert(callee.clone());
                }
            }

            for (name, var_info) in &f.vars {
                if let Some(regs_needed) = match var_info.location {
                    Location::U8 => Some(1),
                    Location::U32 => Some(4),
                    _ => None,
                } {
                    let regs = self.alloc_registers(regs_needed, regs_needed).unwrap();
                    info.register_assignments.insert(name.clone(), regs);
                }
            }

            self.function_info.insert(f.id.clone(), info);
        }
    }

    fn alloc_registers(&mut self, count: u8, align: u8) -> Option<Vec<u8>> {
        let mut alloced = Vec::new();

        let reg_array: Vec<_> = self.available_registers.iter().collect();
        for w in reg_array.as_slice().windows(count as usize) {
            if w[0] % align != 0 {
                continue;
            }
            let contiguous = w.windows(2).all(|pair| pair[0] + 1 == *pair[1]);
            if contiguous {
                alloced.extend(w.iter().cloned());
                break;            
            }
        }

        if alloced.len() == 0 {
            return None;
        }

        for r in &alloced {
            self.available_registers.remove(&r);
        }

        Some(alloced)
    }
}

impl<'a,'b> FunctionContext<'a,'b> {
    fn find_registers(&mut self, n: &IlVarId) -> Vec<u8> {
        self.f_info.register_assignments.get(n).unwrap().clone()
    }

    fn byte_count(&self, var: &IlVarId) -> u32 {
        let info = &self.f_il.vars[var];
        info.var_type.byte_count(self.program.frontend_context)
    }

    fn emit_reg_to_var(&mut self, dest: &IlVarId, src_regs: &Vec<u8>, size: &IlType, source: String) {
        let byte_count: u8 = size.byte_count().try_into().unwrap();
        assert_eq!(src_regs.len(), byte_count.into());
        let dest_regs = self.find_registers(dest);
        assert_eq!(src_regs.len(), dest_regs.len());
        match byte_count {
            1 => {
                self.lines.push(AssemblyInputLine::Instruction(Instruction {
                    opcode: Opcode::Or8,
                    args: vec![Value::Register(src_regs[0]), Value::Register(src_regs[0]), Value::Register(dest_regs[0])],
                    resolved: None,
                    source
                }));
            }
            4 => {
                self.lines.push(AssemblyInputLine::Instruction(Instruction {
                    opcode: Opcode::Copy32,
                    args: vec![Value::Register(src_regs[0]), Value::Register(dest_regs[0])],
                    resolved: None,
                    source
                }));
            },
            _ => panic!(),
        }
    }

    fn emit_var_to_reg(&mut self, dest_regs: &Vec<u8>, var: &IlVarId, size: &IlType, source: String) {
        let byte_count = self.byte_count(var);
        assert_eq!(byte_count, size.byte_count());
        let src_regs = self.find_registers(var);
        assert_eq!(src_regs.len(), byte_count as usize);
        match byte_count {
            1 => {
                self.lines.push(AssemblyInputLine::Instruction(Instruction {
                    opcode: Opcode::Or8,
                    args: vec![Value::Register(src_regs[0]), Value::Register(src_regs[0]), Value::Register(dest_regs[0])],
                    resolved: None,
                    source
                }));
            }
            4 => {
                self.lines.push(AssemblyInputLine::Instruction(Instruction {
                    opcode: Opcode::Copy32,
                    args: vec![Value::Register(src_regs[0]), Value::Register(dest_regs[0])],
                    resolved: None,
                    source
                }));
            },
            _ => panic!(),
        }
    }

    fn emit_num_to_reg(&mut self, dest_regs: &Vec<u8>, n: &IlNumber, expected_size: &IlType, source: String) {
        assert_eq!(&n.il_type(), expected_size);
        self.lines.push(AssemblyInputLine::Instruction(match n {
            IlNumber::U8(n) => {
                Instruction {
                    opcode: Opcode::LoadImm8,
                    args: vec![Value::Register(dest_regs[0]), Value::Constant8(*n)],
                    resolved: None,
                    source,
                }
            },
            IlNumber::U32(n) => {
                Instruction {
                    opcode: Opcode::LoadImm32,
                    args: vec![Value::Register(dest_regs[0]), Value::Constant32(*n)],
                    resolved: None,
                    source,
                }
            },
        }));
    }
}

pub fn emit_assembly(ctxt: &ProgramContext, program: &IlProgram) -> Vec<AssemblyInputLine> {
    let mut ctxt = BackendProgram {
        frontend_context: ctxt,
        il: program,
        function_info: BTreeMap::new(),
        available_registers: (0x10u8..=0xFFu8).collect(),
    };

    ctxt.create_function_infos();

    emit_assembly_inner(&mut ctxt)
}

fn emit_assembly_inner(ctxt: &mut BackendProgram) -> Vec<AssemblyInputLine> {
    let mut lines = Vec::new();

    lines.push(AssemblyInputLine::ImageBaseAddress(ctxt.il.image_base_address));
    lines.push(AssemblyInputLine::Instruction(Instruction {
        opcode: Opcode::LoadImm32,
        args: vec![Value::Register(REG_SP), Value::Constant32(INITIAL_STACK)],
        source: format!("Initialzing stack register to 0x{:08x}.", INITIAL_STACK),
        resolved: None,
    }));

    // args are in r00, r04, r08
    // return is in r00

    lines.push(AssemblyInputLine::from_str(&format!("!call :{}", ctxt.il.entry.0)));
    lines.push(AssemblyInputLine::from_str("halt"));


    for f in ctxt.il.functions.values() {

        let mut ctxt = FunctionContext {
            program: ctxt,
            f_il: f,
            f_info: &ctxt.function_info[&f.id],
            lines: &mut lines,
        };

        ctxt.lines.push(AssemblyInputLine::Label(format!(":{}", &f.id.0)));

        let stack_size = f.vars_stack_size;

        if stack_size > 0 {
            ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                opcode: Opcode::AddImm32IgnoreCarry,
                source: format!("get stack pointing to RA"),
                args: vec![Value::Register(REG_SP), Value::Constant32(stack_size.wrapping_neg())],
                resolved: None,
            }));
        }
        
        for (i, arg_name) in f.args.iter().enumerate() {
            let size = ctxt.byte_count(arg_name);
            let il_type = size.try_into().unwrap();
            let byte_size: u8 = size.try_into().unwrap();
            let arg_reg_start = 4u8*(i as u8);
            let arg_regs = (arg_reg_start..(arg_reg_start+byte_size)).collect();
            let local_regs = ctxt.find_registers(arg_name);
            ctxt.emit_reg_to_var(
                arg_name,
                &arg_regs,
                &il_type,
                format!("Save function parameter '{}' registers {:?} to locals {:?}.", arg_name.0, &arg_regs, &local_regs));
        }

        for s in &f.body {
            dbg!(s);
            let source = format!("{:?}", &s);
            match s {
                IlInstruction::Label(label) => {
                    ctxt.lines.push(AssemblyInputLine::Label(label.0.to_owned()));
                },
                IlInstruction::AssignAtom { dest, src, size } => {
                    let dest_regs = ctxt.find_registers(dest);
                    match src {
                        IlAtom::Number(n) => ctxt.emit_num_to_reg(&dest_regs, n, size, source),
                        IlAtom::Var(var) => ctxt.emit_var_to_reg(&dest_regs, var, size, source),
                    }
                },
                IlInstruction::AssignUnary { dest, op, src } => todo!(),
                IlInstruction::AssignBinary { dest, op, src1, src2 } => {
                    let src1_regs = ctxt.find_registers(src1);
                    let byte_count = ctxt.byte_count(src1);
                    let size = byte_count.try_into().unwrap();
                    
                    assert_eq!(src1_regs.len(), byte_count as usize);

                    let dest_size = ctxt.byte_count(dest);
                    assert_eq!(dest_size, byte_count);
                    let dest_regs = ctxt.find_registers(dest);
                    assert_eq!(dest_regs.len(), dest_size as usize);

                    if let (IlBinaryOp::Add, IlAtom::Number(n)) = (op, src2) {
                        todo!();
                    } else {
                        let (src2_regs, src2_size) = match src2 {
                            IlAtom::Number(n) => {
                                ctxt.emit_num_to_reg(&dest_regs, n, &size, source.clone());
                                (dest_regs.clone(), n.il_type().byte_count())
                            }
                            IlAtom::Var(n) => {
                                let src2 = ctxt.find_registers(n);
                                let len = src2.len() as u32;
                                (src2, len)
                            }
                        };

                        assert_eq!(dest_size, src2_size);

                        match op {
                            IlBinaryOp::Add | IlBinaryOp::BitwiseAnd | IlBinaryOp::BitwiseOr => {

                                let opcode = match (op, size) {
                                    (IlBinaryOp::Add, IlType::U8) => Opcode::Add8NoCarryIn,
                                    (IlBinaryOp::Add, IlType::U32) => Opcode::Add32NoCarryIn,
                                    (IlBinaryOp::BitwiseAnd, IlType::U8) => Opcode::And8,
                                    (IlBinaryOp::BitwiseAnd, IlType::U32) => Opcode::And32,
                                    (IlBinaryOp::BitwiseOr, IlType::U8) => Opcode::Or8,
                                    (IlBinaryOp::BitwiseOr, IlType::U32) => Opcode::Or32,
                                    _ => panic!(),
                                };

                                ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                    opcode,
                                    args: vec![Value::Register(src1_regs[0]), Value::Register(src2_regs[0]), Value::Register(dest_regs[0])],
                                    resolved: None,
                                    source
                                }));
                            },
                            IlBinaryOp::Subtract => {
                                match size {
                                    IlType::U8 => {
                                        ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                            opcode: Opcode::Negate8,
                                            args: vec![Value::Register(src2_regs[0])],
                                            resolved: None,
                                            source: source.clone()
                                        }));

                                        ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                            opcode: Opcode::Add8NoCarryIn,
                                            args: vec![Value::Register(src1_regs[0]), Value::Register(src2_regs[0]), Value::Register(dest_regs[0])],
                                            resolved: None,
                                            source
                                        }));
                                    }
                                    IlType::U32 => {
                                        for r in &src2_regs {
                                            ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                                opcode: Opcode::Invert8,
                                                args: vec![Value::Register(*r)],
                                                resolved: None,
                                                source: source.clone()
                                            }));
                                        }

                                        ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                            opcode: Opcode::AddImm32IgnoreCarry,
                                            args: vec![Value::Register(src2_regs[0]), Value::Constant32(1)],
                                            resolved: None,
                                            source: source.clone()
                                        }));

                                        ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                            opcode: Opcode::Add32NoCarryIn,
                                            args: vec![Value::Register(src1_regs[0]), Value::Register(src2_regs[0]), Value::Register(dest_regs[0])],
                                            resolved: None,
                                            source: source.clone()
                                        }));
                                    },
                                }
                            },
                            IlBinaryOp::Multiply => {
                                match size {
                                    IlType::U8 => {
                                        ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                            opcode: Opcode::Mul8Part1,
                                            args: vec![Value::Register(src1_regs[0]), Value::Register(src2_regs[0])],
                                            resolved: None,
                                            source: source.clone()
                                        }));
                                        ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                            opcode: Opcode::Mul8Part2,
                                            args: vec![],
                                            resolved: None,
                                            source: source.clone()
                                        }));

                                        let product_regs = vec![0u8];
                                        let product_size = IlType::U8;
                                        ctxt.emit_reg_to_var(dest, &product_regs, &product_size, source)
                                    },
                                    IlType::U32 => todo!(),
                                }
                            },
                        }
                    }
                },
                IlInstruction::ReadMemory { dest, addr, size } => {
                    let dest_regs = ctxt.find_registers(dest);
                    assert_eq!(size.byte_count() as usize, dest_regs.len());

                    let first_addr_reg = match addr {
                        IlAtom::Number(n) => {
                            match n {
                                IlNumber::U8(_) => panic!(),
                                IlNumber::U32(n) => {
                                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                        opcode: Opcode::LoadImm32,
                                        args: vec![Value::Register(4), Value::Constant32(*n)],
                                        resolved: None,
                                        source: source.clone(),
                                    }));
                                    4u8
                                },
                            }
                        }
                        IlAtom::Var(n) => {
                            let regs = ctxt.find_registers(n);
                            assert_eq!(regs.len(), 4);
                            regs[0]
                        },
                    };

                    let op = match size {
                        IlType::U8 => Opcode::Load8,
                        IlType::U32 => Opcode::Load32,
                    };

                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                        opcode: op,
                        args: vec![Value::Register(first_addr_reg), Value::Register(dest_regs[0])],
                        resolved: None,
                        source
                    }));
                },
                IlInstruction::WriteMemory { addr, src, size } => {
                    let src_regs = ctxt.find_registers(src);
                    assert_eq!(src_regs.len(), size.byte_count() as usize);

                    let first_addr_reg = match addr {
                        IlAtom::Number(n) => {
                            match n {
                                IlNumber::U8(_) => panic!(),
                                IlNumber::U32(n) => {
                                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                        opcode: Opcode::LoadImm32,
                                        args: vec![Value::Register(4), Value::Constant32(*n)],
                                        resolved: None,
                                        source: source.clone(),
                                    }));
                                    4u8
                                },
                            }
                        }
                        IlAtom::Var(n) => {
                            let regs = ctxt.find_registers(n);
                            assert_eq!(regs.len(), 4);
                            regs[0]
                        },
                    };

                    match size {
                        IlType::U8 => {
                            ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                opcode: Opcode::Store8,
                                args: vec![Value::Register(src_regs[0]), Value::Register(first_addr_reg)],
                                resolved: None,
                                source: source.clone(),
                            }));
                        },
                        IlType::U32 => {
                            ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                opcode: Opcode::Store32Part1,
                                args: vec![Value::Register(src_regs[0]), Value::Register(first_addr_reg)],
                                resolved: None,
                                source: source.clone(),
                            }));
                            ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                opcode: Opcode::Store32Part2,
                                args: vec![],
                                resolved: None,
                                source: source.clone(),
                            }));
                        },
                    };
                },
                IlInstruction::Goto(label) => {
                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                        opcode: Opcode::JmpImm,
                        args: vec![Value::Label24(label.0.to_owned())],
                        resolved: None,
                        source
                    }));
                },
                IlInstruction::IfThenElse { left, op: cmp_op, right, then_label, else_label } => {
                    let left_regs = ctxt.find_registers(left);
                    let left_byte_count = ctxt.byte_count(left);

                    let (right_regs, right_byte_count) = match right {
                        IlAtom::Number(n) => {
                            let right_size = n.il_type();
                            let byte_count: u8 = right_size.byte_count().try_into().unwrap();
                            let dest_regs = (4..(4 + byte_count)).collect();
                            ctxt.emit_num_to_reg(&dest_regs, n, &right_size, source.clone());
                            (dest_regs.clone(), right_size.byte_count())
                        }
                        IlAtom::Var(n) => {
                            let src2 = ctxt.find_registers(n);
                            let len = src2.len() as u32;
                            (src2, len)
                        }
                    };

                    assert_eq!(left_byte_count, right_byte_count);

                    let (mut first_base_regs, mut second_base_regs) = match cmp_op {
                        IlCmpOp::Equals | IlCmpOp::NotEquals => (left_regs, right_regs),
                        IlCmpOp::GreaterThan | IlCmpOp::LessThanOrEqual => (left_regs, right_regs),
                        IlCmpOp::LessThan | IlCmpOp::GreaterThanOrEqual => (right_regs, left_regs),
                    };

                    first_base_regs.reverse();
                    second_base_regs.reverse();

                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                        opcode: Opcode::Cmp8,
                        args: vec![Value::Register(first_base_regs[0]), Value::Register(second_base_regs[0])],
                        resolved: None,
                        source: source.clone()
                    }));

                    for i in 1..left_byte_count {
                        ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                            opcode: Opcode::Cmp8IfZero,
                            args: vec![Value::Register(first_base_regs[i as usize]), Value::Register(second_base_regs[i as usize])],
                            resolved: None,
                            source: source.clone(),
                        }));
                    }

                    let (op, cond_jmp, jmp) = match cmp_op {
                        IlCmpOp::Equals => (Opcode::JzImm, then_label, else_label),
                        IlCmpOp::NotEquals => (Opcode::JzImm, else_label, then_label),
                        IlCmpOp::GreaterThanOrEqual | IlCmpOp::LessThanOrEqual => (Opcode::JcImm, then_label, else_label),
                        IlCmpOp::LessThan | IlCmpOp::GreaterThan => (Opcode::JcImm, else_label, then_label),
                    };

                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                        opcode:op,
                        args: vec![Value::Label24(cond_jmp.0.clone())],
                        resolved: None,
                        source: source.clone(),
                    }));

                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                        opcode: Opcode::JmpImm,
                        args: vec![Value::Label24(jmp.0.clone())],
                        resolved: None,
                        source: source.clone(),
                    }));
                },
                IlInstruction::Call { ret, f: target, args} => {

                    if target == &ctxt.f_il.id {
                        todo!("need to save registers between function calls.");
                    }

                    let target = &ctxt.program.il.functions[target];
                    assert_eq!(target.args.len(), args.len());

                    for (i, (arg_name, arg_value)) in target.args.iter().zip(args.iter()).enumerate() {
                        let arg_size = target.vars[arg_name].var_type.byte_count(ctxt.program.frontend_context);
                        let byte_count = ctxt.byte_count(arg_value);
                        assert_eq!(arg_size, byte_count);
                        let il_type: IlType = byte_count.try_into().unwrap();
                        let i: u8 = i.try_into().unwrap();
                        let dest_regs: Vec<u8> = ((4u8*i)..(4*i+4)).collect();
                        ctxt.emit_var_to_reg(&dest_regs, arg_value, &il_type, 
                            format!("Arg{}[{}]={} {}", i, arg_name.0, arg_value.0, source));
                    }

                    ctxt.lines.push(AssemblyInputLine::from_str(&format!("!call :{}", target.id.0)));

                    dbg!(&(target.ret, ret));

                    match (target.ret, ret) {
                        (None, None) => {}
                        (Some(ret_type), Some(ret_var)) => {
                            let byte_count = ret_type.byte_count();
                            let src_regs: Vec<u8> = (0..(byte_count.try_into().unwrap())).collect();
                            ctxt.emit_reg_to_var(ret_var, &src_regs, &ret_type, source);
                        }
                        _ => panic!("{:?} vs {:?}", target.ret, ret),
                    }
                },
                IlInstruction::Resize { dest, dest_size, src, src_size } => todo!(),
                IlInstruction::Return { val } => {
                    if let Some(val) = val {
                        let src_regs = ctxt.find_registers(val);
                        ctxt.lines.push(AssemblyInputLine::Instruction(match src_regs.len() {
                            1 => Instruction {
                                opcode: Opcode::Or8,
                                args: vec![Value::Register(src_regs[0]), Value::Register(src_regs[0]), Value::Register(0)],
                                source,
                                resolved: None,
                            },
                            4 => Instruction {
                                opcode: Opcode::Copy32,
                                args: vec![Value::Register(src_regs[0]), Value::Register(0)],
                                source,
                                resolved: None,
                            },
                            _ => todo!(),
                        }));
                    }

                    if stack_size > 0 {
                        ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                            opcode: Opcode::AddImm32IgnoreCarry,
                            source: format!("get stack pointing to RA"),
                            args: vec![Value::Register(REG_SP), Value::Constant32(stack_size)],
                            resolved: None,
                        }));
                    }

                    ctxt.lines.push(AssemblyInputLine::from_str("!return"));
                },
                IlInstruction::TtyIn { dest } => todo!(),
                IlInstruction::TtyOut { src } => todo!(),
                IlInstruction::GetFrameAddress { dest } => {
                    let src_regs = (REG_SP..(REG_SP+4)).collect();
                    let size = IlType::U32;
                    ctxt.emit_reg_to_var(dest, &src_regs, &size, source);
                },
            }
        }
    }

    lines
}