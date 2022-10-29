use std::ops::Range;
use std::str::FromStr;

use assemble::AssemblyDirective;

use crate::*;
use crate::il::*;
use crate::optimize::optimize_assembly;

pub struct BackendProgram<'a> {
    pub frontend_context: &'a ProgramContext,
    pub il: &'a IlProgram,
    pub function_info: BTreeMap<IlFunctionId, FunctionInfo>,
    next_round_robin_reg: u8,
}

#[derive(Clone,Debug)]
pub struct FunctionInfo {
    pub register_assignments: BTreeMap<IlVarId, Vec<u8>>,
    pub functions_directly_called: BTreeSet<IlFunctionId>,
    pub functions_transitively_called: BTreeSet<IlFunctionId>,
    pub registers_directly_used: BTreeSet<u8>,
    pub registers_transitively_used: BTreeSet<u8>,
    available_registers: BTreeSet<u8>,
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
    let contiguous = regs.windows(2).all(|pair| 
        if let Some(p0) = pair[0].checked_add(1) {
            p0 == pair[1]
        } else {
            false
        });
    aligned && contiguous
}

impl<'a> BackendProgram<'a> {
    fn create_function_infos(&mut self) {
        let mut function_info = BTreeMap::new();
        for f in self.il.functions.values() {
            let mut info = FunctionInfo {
                functions_directly_called: BTreeSet::new(),
                functions_transitively_called: BTreeSet::new(),
                register_assignments: BTreeMap::new(),
                registers_directly_used: BTreeSet::new(),
                registers_transitively_used: BTreeSet::new(),
                available_registers: (0x10u8..=0xFF).collect(),
            };

            for s in &f.body {
                if let IlInstruction::Call{f: callee, ..} = &s.0 {
                    info.functions_directly_called.insert(callee.clone());
                }
            }

            let liveness = IlLiveness::calculate(f);

            for (name, var_info) in &f.vars {
                if let IlLocation::Reg(il_type) = var_info.location {
                    let regs_needed = il_type.byte_count().try_into().unwrap();
                    let regs = self.alloc_registers(name, &liveness, &mut info, regs_needed, regs_needed)
                        .or_else(|| panic!("Could not allocate a register for {:#?}", &f.body)).unwrap();
                    for r in &regs {
                        if !(REG_SP..REG_SP+4).contains(r) {
                            info.registers_directly_used.insert(*r);
                        }
                    }
                    assert!(info.register_assignments.insert(name.clone(), regs).is_none());
                }
            }

            info.functions_transitively_called = info.functions_directly_called.clone();
            info.registers_transitively_used = info.registers_directly_used.clone();

            function_info.insert(f.id.clone(), info);
        }

        let mut more = true;
        while more {
            more = false;
            for f in self.il.functions.keys() {
                let callees = function_info[f].functions_transitively_called.clone();
                for callee in callees {
                    let callee_registers = function_info[&callee].registers_transitively_used.clone();
                    let info = function_info.get_mut(f).unwrap();
                    more |= info.functions_transitively_called.insert(callee);
                    for r in callee_registers {
                        more |= info.registers_transitively_used.insert(r);
                    }
                }
            }
        }

        self.function_info = function_info;
    }

    fn alloc_registers(&mut self, name: &IlVarId, liveness: &IlLiveness, info: &mut FunctionInfo, count: u8, align: u8) -> Option<Vec<u8>> {

        if name.0 == IlVarId::frame_pointer_str() {
            return Some((REG_SP..(REG_SP+4)).collect());
        }

        let mut alloced = Vec::new();

        let color = liveness.colors[name];

        // first try to reuse
        for (v, regs) in &info.register_assignments {
            if v.0 == IlVarId::frame_pointer_str() { continue; }
            if regs.len() != count as usize { continue; }
            if regs[0] % align != 0 { continue; }
            if liveness.colors[v] == color {
                //println!("Reusing {}'s regs: {:?} for {} as they are both color {}.", v.0, regs, name.0, color);
                return Some(regs.clone());
            }
        }

        let available_reg_count = info.available_registers.len();
        let reg_array: Vec<u8> = info.available_registers.iter()
            .cycle()
            .skip_while(|r| **r < self.next_round_robin_reg)
            .take(available_reg_count)
            .cloned()
            .collect();

        for w in reg_array.as_slice().windows(count as usize) {
            if are_regs_aligned_and_contiguous(w) {
                alloced.extend(w.iter().cloned());
                break;
            }
        }

        if alloced.len() != 0 {
            for r in &alloced {
                info.available_registers.remove(&r);
                self.next_round_robin_reg = r.wrapping_add(1);
            }

            if self.next_round_robin_reg < 0x10 {
                self.next_round_robin_reg = 0x10;
            }

            Some(alloced)
        } else {
            None
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
        let opcode = match byte_count {
            1 => Opcode::Copy8,
            4 => Opcode::Copy32,
            _ => panic!(),
        };

        self.lines.push(AssemblyInputLine::Instruction(Instruction {
            opcode,
            args: vec![Value::Register(src_regs[0]), Value::Register(dest_regs[0])],
            resolved: None,
            source
        }));
    }

    fn emit_var_to_reg(&mut self, dest_regs: &[u8], src: &IlVarId, src_range: &Option<Range<u32>>, size: &IlType, source: String) {
        let byte_count: u8 = size.byte_count().try_into().unwrap();
        assert_eq!(byte_count as usize, dest_regs.len());

        let src_regs = self.find_registers(src);

        let src_regs = if let Some(src_range) = src_range {
            let src_range = (src_range.start as usize)..(src_range.end as usize);
            &src_regs[src_range]
        } else {
            &src_regs
        };

        assert_eq!(byte_count as usize, src_regs.len());

        let opcode = match byte_count {
            1 => Opcode::Copy8,
            4 => Opcode::Copy32,
            _ => panic!(),
        };

        self.lines.push(AssemblyInputLine::Instruction(Instruction {
            opcode,
            args: vec![Value::Register(src_regs[0]), Value::Register(dest_regs[0])],
            resolved: None,
            source
        }));
    }

    fn emit_num_to_reg(&mut self, dest_regs: &Vec<u8>, n: &IlNumber, expected_size: &IlType, source: String) {
        assert_eq!(&n.il_type(), expected_size);
        match n {
            IlNumber::U8(n) => {
                self.lines.push(AssemblyInputLine::Instruction(
                    Instruction {
                        opcode: Opcode::LoadImm8,
                        args: vec![Value::Register(dest_regs[0]), Value::Constant8(*n)],
                        resolved: None,
                        source,
                    }));
            }
            IlNumber::U16(n) => {
                self.lines.push(AssemblyInputLine::Instruction(
                    Instruction {
                        opcode: Opcode::LoadImm8,
                        args: vec![Value::Register(dest_regs[0]), Value::Constant8((*n % 256) as u8)],
                        resolved: None,
                        source: source.clone(),
                    }));
                self.lines.push(AssemblyInputLine::Instruction(
                    Instruction {
                        opcode: Opcode::LoadImm8,
                        args: vec![Value::Register(dest_regs[0]), Value::Constant8((*n / 256) as u8)],
                        resolved: None,
                        source,
                    }));
            }
            IlNumber::U32(n) => {
                self.lines.push(AssemblyInputLine::Instruction(
                    Instruction {
                        opcode: Opcode::LoadImm32,
                        args: vec![Value::Register(dest_regs[0]), Value::Constant32(*n)],
                        resolved: None,
                        source,
                    }));
            }
        }
    }
}

pub fn emit_assembly<'a>(ctxt: &'a ProgramContext, program: &'a IlProgram) -> (BackendProgram<'a>, Vec<AssemblyInputLine>) {
    let mut ctxt = BackendProgram {
        frontend_context: ctxt,
        il: program,
        function_info: BTreeMap::new(),
        next_round_robin_reg: 0x10,
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
        opcode: Opcode::Init,
        args: vec![],
        source: format!("Initialzing flags and internal regs."),
        resolved: None,
    }));
    lines.push(AssemblyInputLine::Instruction(Instruction {
        opcode: Opcode::LoadImm32,
        args: vec![Value::Register(REG_SP), Value::Constant32(INITIAL_STACK)],
        source: format!("Initialzing stack register to 0x{:08x}.", INITIAL_STACK),
        resolved: None,
    }));

    // args are in r00, r04, r08
    // return is in r00

    if let Some((isr_function_name, isr)) = ctxt.il.functions.iter().filter(|f| f.1.is_isr()).next() {

        assert_eq!(0, isr.args.len());
        assert_eq!(None, isr.ret);

        lines.push(AssemblyInputLine::Instruction(Instruction {
            opcode: Opcode::LoadImm32,
            args: vec![Value::Register(0x10), Value::Constant32(INTERRUPT_ISR)],
            source: format!("load address of ISR slot 0x{:08x} into R10.", INTERRUPT_ISR),
            resolved: None,
        }));

        lines.push(AssemblyInputLine::Instruction(Instruction {
            opcode: Opcode::StoreImm32,
            args: vec![Value::Register(0x10), Value::Label32(format!(":{}", isr_function_name.0))],
            source: format!("store address of ISR {} to ISR slot 0x{:08x} into R10.", isr_function_name.0, INTERRUPT_ISR),
            resolved: None,
        }));
    }

    lines.push(AssemblyInputLine::from_str(&format!("!call :{}", ctxt.il.entry.0)).unwrap());
    lines.push(AssemblyInputLine::Instruction(Instruction {
        opcode: Opcode::Halt,
        args: vec![Value::Constant32(HaltCode::Success as u32)],
        source: format!("Halting after return from entry function."),
        resolved: None,
    }));

    for (name, (info, bytes)) in &ctxt.il.consts {
        lines.push(AssemblyInputLine::Label(format!(":{}", &name.0)));
        lines.push(AssemblyInputLine::Comment(format!("const {:?}", info.var_type)));
        for b in bytes {
            lines.push(AssemblyInputLine::Literal8(*b));
        }
    }


    for f in ctxt.il.functions.values() {

        if f.intrinsic.is_some() {
            continue;
        }

        let mut ctxt = FunctionContext {
            program: ctxt,
            f_il: f,
            f_info: &ctxt.function_info[&f.id],
            lines: &mut lines,
        };

        ctxt.lines.push(AssemblyInputLine::Directive(
            AssemblyDirective::FunctionStart(f.id.0.to_string())
        ));
        ctxt.lines.push(AssemblyInputLine::Label(format!(":{}", &f.id.0)));
        ctxt.lines.push(AssemblyInputLine::Comment(format!("Ret {:?}", f.ret)));
        for (i, arg_name) in f.args.iter().enumerate() {
            ctxt.lines.push(AssemblyInputLine::Comment(format!("Arg{}={}", i, arg_name.0)));
        }
        for (var_name, var_info) in &f.vars {
            let reg_assignment = ctxt.f_info.register_assignments.get(var_name);
            ctxt.lines.push(AssemblyInputLine::Comment(format!("Var {} ({}) {:?} {:?}",
                var_name.0, var_info.description, var_info.location, reg_assignment)));
        }

        if f.is_isr() {
            // ISR needs to save regs that it touches
            let my_regs = &ctxt.f_info.registers_directly_used;
            ctxt.lines.push(AssemblyInputLine::Comment(format!("Registers used by this function: {:?}", my_regs)));

            for r in my_regs.iter().rev() {
                assert!(*r >= 0x10);
                ctxt.lines.push(AssemblyInputLine::Instruction(
                    Instruction {
                        opcode: Opcode::Push8,
                        args: vec![Value::Register(*r)],
                        source: format!("Saving reg0x{:02x} before before entering ISR body", *r),
                        resolved: None,
                    }));
            }
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
            // dbg!(s);
            let source = format!("{:?}\n\t{:?}", &s.0, &s.1);
            ctxt.lines.push(AssemblyInputLine::Comment(source.to_owned()));
            match &s.0 {
                IlInstruction::Unreachable => {
                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                        opcode: Opcode::Halt,
                        args: vec![Value::Constant32(HaltCode::CompilerUnreachable as u32)],
                        source,
                        resolved: None,
                    }));
                }
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
                
                IlInstruction::AssignVar { dest, src, size, src_range, dest_range} => {
                    let dest_regs = ctxt.find_registers(dest);
                    let dest_regs = if let Some(dest_range) = dest_range {
                        let dest_range = (dest_range.start as usize)..(dest_range.end as usize);
                        &dest_regs[dest_range]
                    } else {
                        &dest_regs
                    };
                    ctxt.emit_var_to_reg(dest_regs, src, src_range, size, source);
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
                        IlBinaryOp::ShiftLeft | IlBinaryOp::ShiftRight | IlBinaryOp::RotateLeft | IlBinaryOp::RotateRight => {
                            match size {
                                IlType::U8 => {
                                    let mode = match op {
                                        IlBinaryOp::RotateLeft | &IlBinaryOp::RotateRight => ShiftMode::Rotate,
                                        IlBinaryOp::ShiftLeft | &IlBinaryOp::ShiftRight => ShiftMode::Logical,
                                        _ => panic!(),
                                    };
                                    let dir = match op {
                                        IlBinaryOp::RotateLeft | &IlBinaryOp::ShiftLeft => ShiftDirection::Left,
                                        IlBinaryOp::RotateRight | &IlBinaryOp::ShiftRight => ShiftDirection::Right,
                                        _ => panic!(),
                                    };
                                    let command = ShiftCommand { mode, dir };
                                    use packed_struct::prelude::*;
                                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                        opcode: Opcode::Shift8,
                                        args: vec![
                                            Value::Constant8(command.pack().unwrap()[0]),
                                            Value::Register(src2_regs[0]),
                                            Value::Register(src1_regs[0]),
                                            Value::Register(dest_regs[0])],
                                        resolved: None,
                                        source
                                    }));
                                },
                                IlType::U16 | IlType::U32 => todo!(),
                            }
                        },
                        IlBinaryOp::Add | IlBinaryOp::BitwiseAnd | IlBinaryOp::BitwiseOr | IlBinaryOp::Divide=> {
                            if (&IlBinaryOp::Add, IlType::U32) == (op, size) {
                                if let Some(const_src2) = ctxt.f_il.consts.get(&src2) {
                                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                        opcode: Opcode::Copy32,
                                        args: vec![Value::Register(src1_regs[0]), Value::Register(dest_regs[0])],
                                        resolved: None,
                                        source: source.clone()
                                    }));
                                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                        opcode: Opcode::AddImm32IgnoreCarry,
                                        args: vec![Value::Register(dest_regs[0]), Value::Constant32(const_src2.as_u32())],
                                        resolved: None,
                                        source: source.clone(),
                                    }));
                                } else if let Some(const_src1) = ctxt.f_il.consts.get(&src1) {
                                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                        opcode: Opcode::Copy32,
                                        args: vec![Value::Register(src2_regs[0]), Value::Register(dest_regs[0])],
                                        resolved: None,
                                        source: source.clone()
                                    }));
                                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                        opcode: Opcode::AddImm32IgnoreCarry,
                                        args: vec![Value::Register(dest_regs[0]), Value::Constant32(const_src1.as_u32())],
                                        resolved: None,
                                        source: source.clone(),
                                    }));
                                } else {
                                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                        opcode: Opcode::ClearCarry, args: vec![], resolved: None, source: source.clone(),
                                    }));
    
                                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                        opcode: Opcode::AddCarry32_1,
                                        args: vec![Value::Register(src1_regs[0]), Value::Register(src2_regs[0]), Value::Register(dest_regs[0])],
                                        resolved: None,
                                        source: source.clone()
                                    }));
    
                                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                        opcode: Opcode::AddCarry32_2, args: vec![], resolved: None, source
                                    }));
                                }
                            } else {

                                let opcode1 = match (op, size) {
                                    (IlBinaryOp::Add, IlType::U8) => Opcode::Add8NoCarryIn,
                                    (IlBinaryOp::BitwiseAnd, IlType::U8) => Opcode::And8,
                                    (IlBinaryOp::BitwiseAnd, IlType::U32) => Opcode::And32,
                                    (IlBinaryOp::BitwiseOr, IlType::U8) => Opcode::Or8,
                                    (IlBinaryOp::BitwiseOr, IlType::U32) => Opcode::Or32,
                                    (IlBinaryOp::Divide, IlType::U8) => Opcode::Divide8,
                                    _ => panic!(),
                                };

                                ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                    opcode: opcode1,
                                    args: vec![Value::Register(src1_regs[0]), Value::Register(src2_regs[0]), Value::Register(dest_regs[0])],
                                    resolved: None,
                                    source: source
                                }));
                            }
                        },
                        IlBinaryOp::Subtract => {
                            let temp_regs: Vec<_> = (0u8..(src2_size.try_into().unwrap())).collect();
                            match size {
                                IlType::U8 => {
                                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                        opcode: Opcode::Copy8,
                                        args: vec![Value::Register(src2_regs[0]), Value::Register(temp_regs[0])],
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
                                IlType::U16 => todo!(),
                                IlType::U32 => {
                                    if let Some(const_src2) = ctxt.f_il.consts.get(&src2) {
                                        let negated = const_src2.as_u32().wrapping_neg();

                                        ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                            opcode: Opcode::Copy32,
                                            args: vec![Value::Register(src1_regs[0]), Value::Register(dest_regs[0])],
                                            resolved: None,
                                            source: source.clone()
                                        }));
                                        ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                            opcode: Opcode::AddImm32IgnoreCarry,
                                            args: vec![Value::Register(dest_regs[0]), Value::Constant32(negated)],
                                            resolved: None,
                                            source: source.clone()
                                        }));
                                    } else {
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
                                            opcode: Opcode::ClearCarry,
                                            args: vec![],
                                            resolved: None,
                                            source: source.clone()
                                        }));

                                        ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                            opcode: Opcode::AddCarry32_1,
                                            args: vec![Value::Register(src1_regs[0]), Value::Register(temp_regs[0]), Value::Register(dest_regs[0])],
                                            resolved: None,
                                            source: source.clone()
                                        }));

                                        ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                            opcode: Opcode::AddCarry32_2,
                                            args: vec![],
                                            resolved: None,
                                            source: source.clone()
                                        }));
                                    }
                                },
                            }
                        },
                        IlBinaryOp::Multiply => {
                            match size {
                                IlType::U8 => {
                                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                        opcode: Opcode::Mul8_8,
                                        args: vec![Value::Register(src1_regs[0]), Value::Register(src2_regs[0]), Value::Register(dest_regs[0])],
                                        resolved: None,
                                        source: source.clone()
                                    }));
                                },
                                IlType::U16 => {
                                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                        opcode: Opcode::Mul8_16,
                                        args: vec![Value::Register(src1_regs[0]), Value::Register(src2_regs[0]), Value::Register(dest_regs[0])],
                                        resolved: None,
                                        source: source.clone()
                                    }));
                                }
                                IlType::U32 => {
                                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                        opcode: Opcode::Mul8_16,
                                        args: vec![Value::Register(src1_regs[0]), Value::Register(src2_regs[0]), Value::Register(dest_regs[0])],
                                        resolved: None,
                                        source: source.clone()
                                    }));
                                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                        opcode: Opcode::LoadImm8,
                                        args: vec![Value::Register(dest_regs[2]), Value::Constant8(0)],
                                        resolved: None,
                                        source: source.clone()
                                    }));
                                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                        opcode: Opcode::LoadImm8,
                                        args: vec![Value::Register(dest_regs[3]), Value::Constant8(0)],
                                        resolved: None,
                                        source: source.clone()
                                    }));
                                },
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
                        IlType::U16 => todo!(),
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
                        IlType::U16 => todo!(),
                        IlType::U32 => {
                            ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                opcode: Opcode::Store32_1,
                                args: vec![Value::Register(src_regs[0]), Value::Register(first_addr_reg)],
                                resolved: None,
                                source: source.clone(),
                            }));
                            ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                opcode: Opcode::Store32_2,
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

                    if let Some(i) = &target.intrinsic {
                        match i {
                            &Intrinsic::Mul8_16 => {
                                assert_eq!(2, args.len());
                                let src1_reg = ctxt.find_registers(&args[0]);
                                assert_eq!(1, src1_reg.len());
                                let src2_reg = ctxt.find_registers(&args[1]);
                                assert_eq!(1, src2_reg.len());
                                let dst_reg = ctxt.find_registers(ret.as_ref().unwrap());
                                assert_eq!(2, dst_reg.len());
                                ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                    source: format!("{:?}", i),
                                    opcode: Opcode::Mul8_16,
                                    args: vec![Value::Register(src1_reg[0]), Value::Register(src2_reg[0]), Value::Register(dst_reg[0])],
                                    resolved: None,
                                }));
                            }
                            Intrinsic::EnableInterrupts => {
                                assert_eq!(0, args.len());
                                assert_eq!(&None, ret);
                                ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                    source: format!("{:?}", i),
                                    opcode: Opcode::EnableInterrupts,
                                    args: vec![],
                                    resolved: None,
                                }));
                            },
                            Intrinsic::DisableInterrupts => {
                                assert_eq!(0, args.len());
                                assert_eq!(&None, ret);
                                ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                    source: format!("{:?}", i),
                                    opcode: Opcode::DisableInterrupts,
                                    args: vec![],
                                    resolved: None,
                                }));
                            }
                            Intrinsic::ReadyToRead | Intrinsic::ReadyToWrite | Intrinsic::IoRead0 | Intrinsic::IoRead1 | Intrinsic::IoRead2 => {
                                let opcode = match i {
                                    Intrinsic::ReadyToRead => Opcode::IoReadyToRead,
                                    Intrinsic::ReadyToWrite => Opcode::IoReadyToWrite,
                                    Intrinsic::IoRead0 => Opcode::In0,
                                    Intrinsic::IoRead1 => Opcode::In1,
                                    Intrinsic::IoRead2 => Opcode::In2,
                                    _ => panic!(),
                                };
                                assert_eq!(0, args.len());
                                let dst_reg = ctxt.find_registers(ret.as_ref().unwrap());
                                assert_eq!(1, dst_reg.len());
                                ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                    source: format!("{:?}", i),
                                    opcode: opcode,
                                    args: vec![Value::Register(dst_reg[0])],
                                    resolved: None,
                                }));
                            },
                            Intrinsic::IoWrite0 | Intrinsic::IoWrite1 | Intrinsic::IoWrite2 => {
                                let opcode = match i {
                                    Intrinsic::IoWrite0 => Opcode::Out0,
                                    Intrinsic::IoWrite1 => Opcode::Out1,
                                    Intrinsic::IoWrite2 => Opcode::Out2,
                                    _ => panic!(),
                                };

                                assert_eq!(1, args.len());
                                let src1_reg = ctxt.find_registers(&args[0]);
                                assert_eq!(1, src1_reg.len());
                                assert_eq!(&None, ret);
                                ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                    source: format!("{:?}", i),
                                    opcode: opcode,
                                    args: vec![Value::Register(src1_reg[0])],
                                    resolved: None,
                                }));
                            },
                        }

                        continue;
                    }

                    for (i, (arg_name, arg_value)) in target.args.iter().zip(args.iter()).enumerate() {
                        let arg_size = target.vars[arg_name].var_type.byte_count(ctxt.program.frontend_context);
                        let byte_count = ctxt.byte_count(arg_value);
                        assert_eq!(arg_size, byte_count);
                        let il_type: IlType = byte_count.try_into().unwrap();
                        let i: u8 = i.try_into().unwrap();
                        let byte_count: u8 = byte_count.try_into().unwrap();
                        let dest_regs: Vec<u8> = ((4u8*i)..(4u8*i+byte_count)).collect();
                        ctxt.emit_var_to_reg(&dest_regs, arg_value, &None, &il_type,
                            format!("Arg{}[{}]={} {}", i, arg_name.0, arg_value.0, source));
                    }

                    let my_regs = ctxt.f_info.registers_directly_used.clone();
                    ctxt.lines.push(AssemblyInputLine::Comment(format!("Registers used by this function: {:?}", my_regs)));

                    let target_info = &ctxt.program.function_info[&target.id];
                    let target_regs = target_info.registers_transitively_used.clone();
                    ctxt.lines.push(AssemblyInputLine::Comment(format!("Registers used by callee tree: {:?}", target_regs)));

                    // TODO: recurisively get a list of registers used and only save the intersection

                    let regs_to_save: Vec<_> = my_regs.intersection(&target_regs).cloned().collect();
                    // let regs_to_save: Vec<_> = target_regs.iter().cloned().collect();
                    ctxt.lines.push(AssemblyInputLine::Comment(format!("Registers to save: {:?}", regs_to_save)));

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

                    ctxt.lines.push(AssemblyInputLine::from_str(&format!("!call :{}", target.id.0)).unwrap());

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
                        (IlType::U8, IlType::U16) => {
                            ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                opcode: Opcode::LoadImm8,
                                args: vec![Value::Register(dest_regs[1]), Value::Constant8(0)],
                                resolved: None,
                                source: format!("Zero-pad for {}", &source)
                            }));
                        }
                        (IlType::U32, IlType::U8) => { }
                        _ => todo!("Cast from {:?} to {:?}", &src_size, &dest_size),
                    }

                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                        opcode: Opcode::Copy8,
                        args: vec![Value::Register(src_regs[0]), Value::Register(dest_regs[0])],
                        resolved: None,
                        source
                    }));
                },
                IlInstruction::Return { val } => {
                    if let Some(val) = val {
                        let src_regs = ctxt.find_registers(val);
                        match src_regs.len() {
                            1 => ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                opcode: Opcode::Copy8,
                                args: vec![Value::Register(src_regs[0]), Value::Register(0)],
                                source,
                                resolved: None,
                            })),
                            2 => {
                                ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                    opcode: Opcode::Copy8,
                                    args: vec![Value::Register(src_regs[0]), Value::Register(0)],
                                    source: source.clone(),
                                    resolved: None,
                                }));
                                ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                    opcode: Opcode::Copy8,
                                    args: vec![Value::Register(src_regs[1]), Value::Register(1)],
                                    source,
                                    resolved: None,
                                }));
                            },
                            4 => ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                                opcode: Opcode::Copy32,
                                args: vec![Value::Register(src_regs[0]), Value::Register(0)],
                                source,
                                resolved: None,
                            })),
                            _ => todo!(),
                        }
                    }

                    if stack_size > 0 {
                        ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                            opcode: Opcode::AddImm32IgnoreCarry,
                            source: format!("Dealloc {} bytes from stack", stack_size),
                            args: vec![Value::Register(REG_SP), Value::Constant32(stack_size)],
                            resolved: None,
                        }));
                    }

                    if f.is_isr() {
                        // ISR needs to save regs that it touches
                        let my_regs = &ctxt.f_info.registers_directly_used;
                        ctxt.lines.push(AssemblyInputLine::Comment(format!("Registers used by this function: {:?}", my_regs)));

                        for r in my_regs {
                            assert!(*r >= 0x10);
                            ctxt.lines.push(AssemblyInputLine::Instruction(
                                Instruction {
                                    opcode: Opcode::Pop8,
                                    args: vec![Value::Register(*r)],
                                    source: format!("Restoring reg0x{:02x} after ISR body", *r),
                                    resolved: None,
                                }));
                        }
            
                        ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                            opcode: Opcode::ReturnFromInterrupt,
                            source: format!("return from ISR"),
                            args: vec![],
                            resolved: None,
                        }));
                    } else {
                        ctxt.lines.push(AssemblyInputLine::from_str("!return").unwrap());
                    }
                },
                IlInstruction::TtyIn { dest } => {
                    let dest_regs = ctxt.find_registers(dest);
                    assert_eq!(dest_regs.len(), 1);
                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                        opcode: IoPort::Tty.in_opcode(),
                        source,
                        args: vec![Value::Register(dest_regs[0])],
                        resolved: None,
                    }));
                },
                IlInstruction::TtyOut { src } => {
                    let src_regs = ctxt.find_registers(src);
                    assert_eq!(src_regs.len(), 1);
                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                        opcode: IoPort::Tty.out_opcode(),
                        source,
                        args: vec![Value::Register(src_regs[0])],
                        resolved: None,
                    }));
                },
                IlInstruction::GetConstAddress { dest, const_name } => {
                    let dest_regs = ctxt.find_registers(dest);
                    assert_eq!(dest_regs.len(), 4);
                    ctxt.lines.push(AssemblyInputLine::Instruction(Instruction {
                        opcode: Opcode::LoadImm32,
                        source,
                        args: vec![Value::Register(dest_regs[0]), Value::Label32(format!(":{}", const_name))],
                        resolved: None,
                    }));
                },
            }
        }
        
        lines.push(AssemblyInputLine::Directive(
            AssemblyDirective::FunctionEnd,
        ));
    }

    optimize_assembly(&mut lines);

    lines
}