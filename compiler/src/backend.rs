use crate::*;
use crate::il::*;

pub struct BackendProgram<'a> {
    pub frontend_context: &'a ProgramContext,
    pub il: &'a IlProgram,
    pub function_info: BTreeMap<IlFunctionId, FunctionInfo>,
    available_registers: BTreeSet<u8>,
    next_round_robin_reg: u8,
}

#[derive(Debug)]
pub struct FunctionInfo {
    pub register_assignments: BTreeMap<IlVarId, Vec<u8>>,
    pub functions_called: BTreeSet<IlFunctionId>,
    pub registers_used: BTreeSet<u8>,
}

struct FunctionContext<'a,'b> {
    program: &'a BackendProgram<'b>,
    f_il: &'a IlFunction,
    f_info: &'a FunctionInfo,
    lines: &'a mut Vec<AssemblyInputLine>,
}

fn are_regs_aligned_and_contiguous(regs: &[u8]) -> bool {
    let len: u8 = regs.len().try_into().unwrap();
    let aligned = (regs[0] % len) == 0;
    let contiguous = regs.windows(2).all(|pair| pair[0] + 1 == pair[1]);
    aligned && contiguous
}

impl<'a> BackendProgram<'a> {
    fn create_function_infos(&mut self) {
        for f in self.il.functions.values() {
            let mut info = FunctionInfo {
                functions_called: BTreeSet::new(),
                register_assignments: BTreeMap::new(),
                registers_used: BTreeSet::new(),
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
                    let regs = self.alloc_registers(&mut info, regs_needed, regs_needed)
                        .expect(&format!("Could not allocate a register for {:#?}", &f.body));
                    for r in &regs {
                        info.registers_used.insert(*r);
                    }
                    assert!(info.register_assignments.insert(name.clone(), regs).is_none());
                }
            }

            self.function_info.insert(f.id.clone(), info);
        }
    }

    fn alloc_registers(&mut self, info: &mut FunctionInfo, count: u8, align: u8) -> Option<Vec<u8>> {
        let mut alloced = Vec::new();

        let reg_array: Vec<_> = self.available_registers.iter().cloned().collect();
        for w in reg_array.as_slice().windows(count as usize) {
            if are_regs_aligned_and_contiguous(w) {
                alloced.extend(w.iter().cloned());
                break;
            }
        }

        if alloced.len() != 0 {
            for r in &alloced {
                self.available_registers.remove(&r);
            }
    
            Some(alloced)
        } else {
            let mut attempts = 1000;
            loop {
                while (self.next_round_robin_reg % align) != 0 {
                    self.next_round_robin_reg = self.next_round_robin_reg.wrapping_add(1)
                }

                if self.next_round_robin_reg < 0x10 {
                    self.next_round_robin_reg = 0x10;
                }

                let r = self.next_round_robin_reg;
                let regs: Vec<_> = (r..=(r-1+count)).collect();

                let conflict = regs.iter().any(|r| info.registers_used.contains(r));

                if conflict {
                    attempts -= 1;
                    if attempts == 0 {
                        dbg!(&self.available_registers);
                        dbg!(info);
                        break None;
                    }
                    self.next_round_robin_reg = self.next_round_robin_reg.wrapping_add(1);
                } else {
                    for r in &regs {
                        assert!(info.registers_used.insert(*r));
                    }
                    self.next_round_robin_reg = self.next_round_robin_reg.wrapping_add(count);
                    break Some(regs);
                }
            }
        }

        
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

pub fn emit_assembly<'a>(ctxt: &'a ProgramContext, program: &'a IlProgram) -> (BackendProgram<'a>, Vec<AssemblyInputLine>) {
    let mut ctxt = BackendProgram {
        frontend_context: ctxt,
        il: program,
        function_info: BTreeMap::new(),
        available_registers: (0x10u8..=0xFF).collect(),
        next_round_robin_reg: 0x10
    };

    ctxt.create_function_infos();

    // dbg!(&ctxt.function_info);

    let lines = emit_assembly_inner(&mut ctxt);
    (ctxt, lines)
}

fn emit_assembly_inner(ctxt: &mut BackendProgram) -> Vec<AssemblyInputLine> {
    let mut lines = Vec::new();

    lines.push(AssemblyInputLine::ImageBaseAddress(ctxt.il.image_base_address));
    lines.push(AssemblyInputLine::Label(format!("entry")));
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
        for (i, arg_name) in f.args.iter().enumerate() { 
            ctxt.lines.push(AssemblyInputLine::Comment(format!("Arg{}={}", i, arg_name.0)));
        }
        for (var_name, var_info) in &f.vars {
            let reg_assignment = ctxt.f_info.register_assignments.get(var_name);
            ctxt.lines.push(AssemblyInputLine::Comment(format!("Var {} ({}) {:?} {:?}", 
                var_name.0, var_info.description, var_info.location, reg_assignment)));
        }

        let stack_size = f.vars_stack_size;

        if stack_size > 0 {
            ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                opcode: Opcode::AddImm32IgnoreCarry,
                source: format!("reserve {} bytes of stack space for locals", stack_size),
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
                IlInstruction::Comment(c) => {
                    ctxt.lines.push(AssemblyInputLine::Comment(c.to_owned()));
                },
                IlInstruction::Label(label) => {
                    ctxt.lines.push(AssemblyInputLine::Label(label.0.to_owned()));
                },
                IlInstruction::AssignNumber { dest, src} => {
                    let dest_regs = ctxt.find_registers(dest);
                    ctxt.emit_num_to_reg(&dest_regs, src, &src.il_type(), source);
                }
                IlInstruction::AssignVar { dest, src, size} => {
                    let dest_regs = ctxt.find_registers(dest);
                    ctxt.emit_var_to_reg(&dest_regs, src, size, source);
                },
                IlInstruction::AssignUnary { dest:_, op:_, src:_ } => todo!(),
                IlInstruction::AssignBinary { dest, op, src1, src2 } => {
                    let src1_regs = ctxt.find_registers(src1);
                    let byte_count = ctxt.byte_count(src1);
                    let size = byte_count.try_into().unwrap();
                    
                    assert_eq!(src1_regs.len(), byte_count as usize);

                    let dest_size = ctxt.byte_count(dest);
                    assert_eq!(dest_size, byte_count);
                    let dest_regs = ctxt.find_registers(dest);
                    assert_eq!(dest_regs.len(), dest_size as usize);

                    let src2_regs = ctxt.find_registers(src2);
                    let src2_size = src2_regs.len() as u32;

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
                            let temp_regs: Vec<_> = (0u8..(src2_size.try_into().unwrap())).collect();
                            match size {
                                IlType::U8 => {
                                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                        opcode: Opcode::Or8,
                                        args: vec![Value::Register(src2_regs[0]), Value::Register(src2_regs[0]), Value::Register(temp_regs[0])],
                                        resolved: None,
                                        source: source.clone()
                                    }));

                                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                        opcode: Opcode::Negate8,
                                        args: vec![Value::Register(temp_regs[0])],
                                        resolved: None,
                                        source: source.clone()
                                    }));

                                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                        opcode: Opcode::Add8NoCarryIn,
                                        args: vec![Value::Register(src1_regs[0]), Value::Register(temp_regs[0]), Value::Register(dest_regs[0])],
                                        resolved: None,
                                        source
                                    }));
                                }
                                IlType::U32 => {
                                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                        opcode: Opcode::Copy32,
                                        args: vec![Value::Register(src2_regs[0]), Value::Register(temp_regs[0])],
                                        resolved: None,
                                        source: source.clone()
                                    }));

                                    for r in &temp_regs {
                                        ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                            opcode: Opcode::Invert8,
                                            args: vec![Value::Register(*r)],
                                            resolved: None,
                                            source: source.clone()
                                        }));
                                    }

                                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                        opcode: Opcode::AddImm32IgnoreCarry,
                                        args: vec![Value::Register(temp_regs[0]), Value::Constant32(1)],
                                        resolved: None,
                                        source: source.clone()
                                    }));

                                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                        opcode: Opcode::Add32NoCarryIn,
                                        args: vec![Value::Register(src1_regs[0]), Value::Register(temp_regs[0]), Value::Register(dest_regs[0])],
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
                },
                IlInstruction::ReadMemory { dest, addr, size } => {
                    let dest_regs = ctxt.find_registers(dest);
                    assert_eq!(size.byte_count() as usize, dest_regs.len());

                    let addr_regs = ctxt.find_registers(addr);
                    assert_eq!(addr_regs.len(), 4);
                    let first_addr_reg = addr_regs[0];

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

                    let addr_regs = ctxt.find_registers(addr);
                    assert_eq!(addr_regs.len(), 4);
                    let first_addr_reg = addr_regs[0];

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

                    let right_regs = ctxt.find_registers(right);
                    let right_byte_count = ctxt.byte_count(right);

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

                    let my_regs = ctxt.f_info.registers_used.clone();
                    ctxt.lines.push(AssemblyInputLine::Comment(format!("Registers used by this function: {:?}", my_regs)));

                    let target_info = &ctxt.program.function_info[&target.id];
                    let target_regs = target_info.registers_used.clone();
                    ctxt.lines.push(AssemblyInputLine::Comment(format!("Registers used by callee: {:?}", target_regs)));


                    let regs_to_save: Vec<_> = target_regs.intersection(&my_regs).cloned().collect();
                    ctxt.lines.push(AssemblyInputLine::Comment(format!("Registers to save: {:?}", regs_to_save)));

                    // let u32_regs : Vec<_> = regs_to_save
                    //     .windows(4)
                    //     .filter(|w| are_regs_aligned_and_contiguous(w))
                    //     .map(|w| (w[0], w))
                    //     .collect();

                    // let u8_regs = {
                    //     let mut u8_regs: BTreeSet<u8> = regs_to_save.iter().cloned().collect();
                    
                    //     for (_,all) in u32_regs {
                    //         for r in all {
                    //             u8_regs.remove(r);
                    //         }
                    //     }

                    //     u8_regs
                    // };

                    for r in regs_to_save.iter().rev() {
                        assert!(*r >= 0x10);
                        ctxt.lines.push(AssemblyInputLine::Instruction(
                            Instruction {
                                opcode: Opcode::Push8,
                                args: vec![Value::Register(*r)],
                                source: format!("Saving reg0x{:02x} before {}", *r, &source),
                                resolved: None,
                            }));
                    }

                    ctxt.lines.push(AssemblyInputLine::from_str(&format!("!call :{}", target.id.0)));

                    for r in &regs_to_save {
                        assert!(*r >= 0x10);
                        ctxt.lines.push(AssemblyInputLine::Instruction(
                            Instruction {
                                opcode: Opcode::Pop8,
                                args: vec![Value::Register(*r)],
                                source: format!("Restoring reg0x{:02x} after {}", *r, &source),
                                resolved: None,
                            }));
                    }

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
                IlInstruction::Resize { dest, dest_size, src, src_size } => {
                    let src_regs = ctxt.find_registers(src);
                    assert_eq!(src_size.byte_count() as usize, src_regs.len());

                    let dest_regs = ctxt.find_registers(dest);
                    assert_eq!(dest_size.byte_count() as usize, dest_regs.len());

                    match (src_size, dest_size) {
                        (IlType::U8, IlType::U32) => {
                            ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                opcode: Opcode::LoadImm32,
                                args: vec![Value::Register(dest_regs[0]), Value::Constant32(0)],
                                resolved: None,
                                source: format!("Zero-pad for {}", &source)
                            }));
                        }
                        (IlType::U32, IlType::U8) => { }
                        _ => panic!(),
                    }

                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                        opcode: Opcode::Or8,
                        args: vec![Value::Register(src_regs[0]), Value::Register(src_regs[0]), Value::Register(dest_regs[0])],
                        resolved: None,
                        source
                    }));
                },
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
                            source: format!("Dealloc {} bytes from stack", stack_size),
                            args: vec![Value::Register(REG_SP), Value::Constant32(stack_size)],
                            resolved: None,
                        }));
                    }

                    ctxt.lines.push(AssemblyInputLine::from_str("!return"));
                },
                IlInstruction::TtyIn { dest } => {
                    let dest_regs = ctxt.find_registers(dest);
                    assert_eq!(dest_regs.len(), 1);
                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                        opcode: Opcode::TtyIn,
                        source,
                        args: vec![Value::Register(dest_regs[0])],
                        resolved: None,
                    }));
                },
                IlInstruction::TtyOut { src } => {
                    let src_regs = ctxt.find_registers(src);
                    assert_eq!(src_regs.len(), 1);
                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                        opcode: Opcode::TtyOut,
                        source,
                        args: vec![Value::Register(src_regs[0])],
                        resolved: None,
                    }));
                },
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