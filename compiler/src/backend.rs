use crate::*;
use crate::il::*;

struct FunctionContext<'a> {
    program: &'a ProgramContext,
    registers_available: &'a mut BTreeSet<u8>,
    assigned_vars: BTreeMap<IlVarId, Vec<u8>>,
    lines: &'a mut Vec<AssemblyInputLine>,
    f: &'a IlFunction,
}

impl<'a> FunctionContext<'a> {
    fn find_or_alloc_registers(&mut self, n: &IlVarId, count: u8, align: u8) -> Option<Vec<u8>> {
        if let Some(regs) = self.assigned_vars.get(n) {
            assert_eq!(regs.len(), count.into());
            assert_eq!(regs.len() % (align as usize), 0);
            Some(regs.clone())
        } else {
            if let Some(alloced) = self.alloc_registers(count, align) {
                self.assigned_vars.insert(n.clone(), alloced.clone());
                Some(alloced)
            } else {
                None
            }
        }
    }

    fn alloc_registers(&mut self, count: u8, align: u8) -> Option<Vec<u8>> {
        let mut alloced = Vec::new();

        let reg_array: Vec<_> = self.registers_available.iter().collect();
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
            self.registers_available.remove(&r);
        }

        Some(alloced)
    }

    fn emit_reg_to_var(&mut self, dest: &IlVarId, src_regs: &Vec<u8>, size: &IlType, source: String) {
        let byte_count: u8 = size.byte_count().try_into().unwrap();
        assert_eq!(src_regs.len(), byte_count.into());
        let dest_regs = self.find_or_alloc_registers(dest, byte_count, byte_count).unwrap();
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
        let info = &self.f.vars[var];
        let byte_count = info.var_type.byte_count(self.program);
        assert_eq!(byte_count, size.byte_count());
        match byte_count {
            1 => {
                let src_regs = self.assigned_vars.get(var).unwrap();
                assert_eq!(src_regs.len(), byte_count as usize);
                self.lines.push(AssemblyInputLine::Instruction(Instruction {
                    opcode: Opcode::Or8,
                    args: vec![Value::Register(src_regs[0]), Value::Register(src_regs[0]), Value::Register(dest_regs[0])],
                    resolved: None,
                    source
                }));
            }
            4 => {
                let src_regs = self.assigned_vars.get(var).unwrap();
                assert_eq!(src_regs.len(), byte_count as usize);
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

    let mut lines = Vec::new();

    let entry = &program.entry;
    let entry = program.functions.get(&entry).unwrap();

    lines.push(AssemblyInputLine::ImageBaseAddress(program.image_base_address));
    lines.push(AssemblyInputLine::Instruction(Instruction {
        opcode: Opcode::LoadImm32,
        args: vec![Value::Register(REG_SP), Value::Constant32(INITIAL_STACK)],
        source: format!("Initialzing stack register to 0x{:08x}.", INITIAL_STACK),
        resolved: None,
    }));

    // args are in r00, r04, r08
    // return is in r00

    lines.push(AssemblyInputLine::from_str(&format!("!call :{}", program.entry.0)));
    lines.push(AssemblyInputLine::from_str("halt"));

    let mut registers_available: BTreeSet<u8> = (0x10u8..=0xFFu8).collect();
    for f in program.functions.values() {
        let mut ctxt = FunctionContext {
            program: ctxt,
            registers_available: &mut registers_available,
            assigned_vars: BTreeMap::new(),
            lines: &mut lines,
            f,
        };

        for (i, arg_name) in f.args.iter().enumerate() {
            let arg_info = &f.vars[arg_name];
            let size = arg_info.var_type.byte_count(ctxt.program) as u8;
            let arg_reg = 4u8*(i as u8);
            ctxt.assigned_vars.insert(arg_name.clone(), (arg_reg..(arg_reg+size)).collect());
        }

        let stack_size = f.vars_stack_size;
        
        ctxt.lines.push(AssemblyInputLine::Label(format!(":{}", &f.id.0)));

        if stack_size > 0 {
            ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                opcode: Opcode::AddImm32IgnoreCarry,
                source: format!("get stack pointing to RA"),
                args: vec![Value::Register(REG_SP), Value::Constant32(stack_size.wrapping_neg())],
                resolved: None,
            }));
        }

        for s in &f.body {
            dbg!(s);
            let source = format!("{:?}", &s);
            match s {
                IlInstruction::Label(label) => {
                    ctxt.lines.push(AssemblyInputLine::Label(label.0.to_owned()));
                },
                IlInstruction::AssignAtom { dest, src, size } => {
                    let byte_count = size.byte_count().try_into().unwrap();
                    let dest_regs = ctxt.find_or_alloc_registers(dest, byte_count, byte_count).unwrap();
                    match src {
                        IlAtom::Number(n) => ctxt.emit_num_to_reg(&dest_regs, n, size, source),
                        IlAtom::Var(var) => ctxt.emit_var_to_reg(&dest_regs, var, size, source),
                    }
                },
                IlInstruction::AssignUnary { dest, op, src } => todo!(),
                IlInstruction::AssignBinary { dest, op, src1, src2 } => {
                    let src1_regs = ctxt.assigned_vars[src1].clone();
                    let byte_count = ctxt.f.vars[src1].var_type.byte_count(ctxt.program);
                    let size = byte_count.try_into().unwrap();
                    
                    assert_eq!(src1_regs.len(), byte_count as usize);

                    let dest_size = ctxt.f.vars[dest].var_type.byte_count(ctxt.program);
                    assert_eq!(dest_size, byte_count);
                    let byte_count = byte_count.try_into().unwrap();
                    let dest_regs = ctxt.find_or_alloc_registers(dest, byte_count, byte_count).unwrap();
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
                                let src2 = &ctxt.assigned_vars[n];
                                (src2.clone(), src2.len() as u32)
                            }
                        };

                        assert_eq!(dest_size, src2_size);

                        match op {
                            IlBinaryOp::Add => {
                                match size {
                                    IlType::U8 => {
                                        ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                            opcode: Opcode::Add8NoCarryIn,
                                            args: vec![Value::Register(src1_regs[0]), Value::Register(src2_regs[0]), Value::Register(dest_regs[0])],
                                            resolved: None,
                                            source
                                        }));
                                    },
                                    IlType::U32 => {
                                        ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                            opcode: Opcode::Add32NoCarryIn,
                                            args: vec![Value::Register(src1_regs[0]), Value::Register(src2_regs[0]), Value::Register(dest_regs[0])],
                                            resolved: None,
                                            source
                                        }));
                                    },
                                }
                            },
                            IlBinaryOp::Subtract => todo!(),
                            IlBinaryOp::Multiply => todo!(),
                            IlBinaryOp::BitwiseAnd => todo!(),
                            IlBinaryOp::BitwiseOr => todo!(),
                        }
                    }
                },
                IlInstruction::ReadMemory { dest, addr, size } => todo!(),
                IlInstruction::WriteMemory { addr, src, size } => todo!(),
                IlInstruction::Goto(label) => {
                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                        opcode: Opcode::JmpImm,
                        args: vec![Value::Label24(label.0.to_owned())],
                        resolved: None,
                        source
                    }));
                },
                IlInstruction::IfThenElse { left, op, right, then_label, else_label } => todo!(),
                IlInstruction::Call { ret, f: target, args} => {
                    for (i, a) in args.iter().enumerate() {
                        let info = &ctxt.f.vars[a];
                        let byte_count = info.var_type.byte_count(ctxt.program);
                        let il_type: IlType = byte_count.try_into().unwrap();
                        let i: u8 = i.try_into().unwrap();
                        let dest_regs: Vec<u8> = ((4u8*i)..(4*i+4)).collect();
                        ctxt.emit_var_to_reg(&dest_regs, a, &il_type, 
                            format!("Arg{}={} {}", i, a.0, source));
                    }

                    ctxt.lines.push(AssemblyInputLine::from_str(&format!("!call :{}", target.0)));

                    match (ctxt.f.ret, ret) {
                        (None, None) => {}
                        (Some(ret_type), Some(ret_var)) => {
                            let byte_count = ret_type.byte_count();
                            let size = byte_count.try_into().unwrap();
                            let src_regs: Vec<u8> = (0..(byte_count.try_into().unwrap())).collect();
                            ctxt.emit_reg_to_var(ret_var, &src_regs, &size, source);
                        }
                        _ => panic!(),
                    }
                },
                IlInstruction::Resize { dest, dest_size, src, src_size } => todo!(),
                IlInstruction::Return { val } => {
                    let src_regs = ctxt.assigned_vars.get(val).unwrap();
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
                IlInstruction::GetFrameAddress { dest } => todo!(),
            }
        }
    }

    lines
}