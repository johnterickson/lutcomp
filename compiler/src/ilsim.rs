use crate::*;
use crate::il::*;

struct SimulationContext<'a> {
    program: &'a IlProgram,
    entry_function: &'a IlFunctionId,
    entry_args: &'a [IlNumber],
    stack: Vec<u8>,
    stack_pointer: u32,
    stack_slice_start_addr: u32,
    mem: BTreeMap<u32, u8>,
}

impl<'a> SimulationContext<'a> {
    fn run(&mut self) -> IlNumber {
        let entry = self.program.functions.get(&self.entry_function).unwrap();
        entry.simulate(self, &self.entry_args)
    }
}

impl IlProgram {
    fn create_sim<'a>(&'a self, args: &'a [IlNumber]) -> SimulationContext<'a> {
        SimulationContext {
            program: &self,
            entry_function: &self.entry,
            entry_args: args,
            stack: vec![0u8; 1024],
            stack_pointer: u32::max_value()-4+1,
            stack_slice_start_addr: u32::max_value()-1024+1,
            mem: BTreeMap::new(),
        }
    }

    pub fn simulate(&self, args: &[IlNumber]) -> IlNumber {
        self.create_sim(args).run()
    }
}

impl IlAtom {
    fn value(&self, vars: &BTreeMap<&IlVarId, Option<IlNumber>>) -> IlNumber {
        match self {
            IlAtom::Number(n) => *n,
            IlAtom::Var(v) => vars[v].unwrap(),
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
        let mut vars: BTreeMap<&IlVarId, Option<IlNumber>> = BTreeMap::new();

        for (id, info) in self.vars.iter().rev() {
            if let Size::Stack(stack_size) = info.size {
                ctxt.stack_pointer -= stack_size;
                assert!(vars.insert(id, Some(IlNumber::U32(ctxt.stack_pointer))).is_none());
            }
            if let Size::Static(size) = info.size {
                let key = (Some(self.id.clone()), id.clone());
                let addr = *ctxt.program.statics.get(&key).unwrap();
                assert!(vars.insert(id, Some(IlNumber::U32(addr))).is_none());
            }
        }

        assert_eq!(self.args.len(), args.len());
        for (arg_id, arg_value) in self.args.iter().zip(args) {
            assert!(vars.insert(arg_id, Some(*arg_value)).is_none());
        }

        let mut s_index = 0;
        loop {
            let mut inc_pc = true;

            let s = &self.body[s_index];
            dbg!(s);

            match s {
                IlInstruction::Label(_) => {},
                IlInstruction::AssignAtom { dest, src } => {
                    vars.insert(dest, Some(src.value(&vars)));
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
                    vars.insert(dest, Some(result));
                },
                IlInstruction::AssignBinary { dest, op, src1, src2 } => {
                    let src1 = vars.get(src1)
                        .expect(&format!("Could not find {:?}.", &src1))
                        .expect(&format!("{:?} has no value.", &src1));
                    let src2 = src2.value(&vars);                    
                    let result = match (src1, src2) {
                        (IlNumber::U8(n1), IlNumber::U8(n2)) => {
                            IlNumber::U8(match op {
                                IlBinaryOp::Add => n1.wrapping_add(n2),
                                IlBinaryOp::Subtract => n1.wrapping_sub(n2),
                                IlBinaryOp::Multiply => n1.wrapping_mul(n2),
                            })
                        }
                        (IlNumber::U32(n1), IlNumber::U32(n2)) => {
                            IlNumber::U32(match op {
                                IlBinaryOp::Add => n1.wrapping_add(n2),
                                IlBinaryOp::Subtract => n1.wrapping_sub(n2),
                                IlBinaryOp::Multiply => unimplemented!(),
                            })
                        }
                        _ => panic!(),
                    };
                    
                    vars.insert(dest, Some(result));
                }
                IlInstruction::ReadMemory { dest, addr, size } => {
                    let addr = addr.value(&vars);
                    if let IlNumber::U32(addr) = addr {
                        match size {
                            Size::U8 => {
                                vars.insert(dest, Some(IlNumber::U8(ctxt.mem[&addr])));
                            }
                            Size::U32 => {
                                assert_eq!(0, addr % 4);
                                let r = u32::from_le_bytes([
                                    ctxt.mem[&(addr+0)],
                                    ctxt.mem[&(addr+1)],
                                    ctxt.mem[&(addr+2)],
                                    ctxt.mem[&(addr+3)],
                                ]);
                                vars.insert(dest, Some(IlNumber::U32(r)));
                            }
                            _ => panic!(),
                        }
                    } else {
                        panic!();
                    }
                }
                IlInstruction::WriteMemory { addr, src, size} => {
                    let src = vars[src].unwrap();
                    let addr = addr.value(&vars);
                    if let IlNumber::U32(addr) = addr {
                        match src {
                            IlNumber::U8(n) => {
                                assert_eq!(size, &Size::U8);
                                ctxt.mem.insert(addr, n);
                            },
                            IlNumber::U32(n) => {
                                assert_eq!(0, addr % 4);
                                assert_eq!(size, &Size::U32);
                                let bytes = n.to_le_bytes();
                                for (i, b) in bytes.iter().enumerate() {
                                    let i = i as u32;
                                    ctxt.mem.insert(addr + i, *b);
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
                    let left = vars[left].unwrap();
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
                    let f = &ctxt.program.functions[f];
                    let args: Vec<_> = args.iter().map(|a| vars[a].unwrap()).collect();
                    let result = f.simulate(ctxt, &args);
                    vars.insert(ret, Some(result));
                },
                IlInstruction::Return { val } => {
                    return vars[val].unwrap();
                },
                IlInstruction::TtyIn { dest } => todo!(),
                IlInstruction::TtyOut { src } => todo!(),
                IlInstruction::Resize { dest, dest_size, src, src_size } => {
                    let n = match (dest_size, vars.get(src).unwrap().unwrap()) {
                        (Size::U32, IlNumber::U8(n)) => IlNumber::U32(n as u32),
                        (Size::U8, IlNumber::U32(n)) => IlNumber::U8((n & 0xFF) as u8),
                        _ => panic!(),
                    };
                    
                    vars.insert(dest, Some(n));
                },
            }

            if inc_pc {
                s_index += 1;
            }

            println!("{:?}", &vars);
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

        assert_eq!(il.simulate(&[0xAABBCCDDu32.into(), 0xAABBCCDDu32.into()]), 0u8.into());
        assert_eq!(il.simulate(&[0xAABBCCDDu32.into(), 0xAABBCCDEu32.into()]), 1u8.into());
        assert_eq!(il.simulate(&[0xAABBCCDDu32.into(), 0xAABBCDDDu32.into()]), 1u8.into());
        assert_eq!(il.simulate(&[0xAABBCCDDu32.into(), 0xAABCCCDDu32.into()]), 1u8.into());
        assert_eq!(il.simulate(&[0xAABBCCDDu32.into(), 0xABBBCCDDu32.into()]), 1u8.into());
    }

    #[test]
    fn local_array() {
        let il = emit_il(
            "main",
            include_str!("../../programs/local_array.j"));

        assert_eq!(il.simulate(&[0u8.into(), 0u8.into()]), 0u8.into());
        assert_eq!(il.simulate(&[0u8.into(), 1u8.into()]), 1u8.into());
        assert_eq!(il.simulate(&[1u8.into(), 0u8.into()]), 1u8.into());
        assert_eq!(il.simulate(&[1u8.into(), 0xFFu8.into()]), 0u8.into());
    }

    #[test]
    fn fac_rec() {
        let il = emit_il(
            "main",
            include_str!("../../programs/fac_rec.j"));
        for (ins, out) in 
            &[
                ([0x0u8.into()],1u8.into()),
                ([0x1u8.into()],1u8.into()),
                ([0x2u8.into()],2u8.into()),
                ([0x5u8.into()],120u8.into()),
            ]
        {
            assert_eq!(il.simulate(ins), *out);
        }
    }

    #[test]
    fn fac_iter() {
        let il = emit_il(
            "fac",
            include_str!("../../programs/fac_iter.j"));
        for (ins, out) in 
            &[
                (vec![0u8.into()],1u8.into()),
                (vec![1u8.into()],1u8.into()),
                (vec![2u8.into()],2u8.into()),
                (vec![5u8.into()],120u8.into()),
            ]
        {
            assert_eq!(il.simulate(ins), *out);
        }
    }

    #[test]
    fn fib() {
        let il = emit_il(
            "fib",
            include_str!("../../programs/fib.j"));
        for (ins, out) in 
            &[
                (vec![0u8.into()],0u8.into()),
                (vec![1u8.into()],1u8.into()),
                (vec![2u8.into()],1u8.into()),
                (vec![3u8.into()],2u8.into()),
                // (vec![13u8.into()],233u8.into()),
            ]
        {
            assert_eq!(il.simulate(ins), *out);
        }
    }

    #[test]
    fn fib_memo() {
        let il = emit_il(
            "main",
            include_str!("../../programs/fib_memo.j"));
        dbg!(&il);
        for (ins, out) in 
            &[
                (vec![0u8.into()],0u8.into()),
                (vec![1u8.into()],1u8.into()),
                (vec![2u8.into()],1u8.into()),
                (vec![3u8.into()],2u8.into()),
                (vec![13u8.into()],233u8.into()),
            ]
        {
            assert_eq!(il.simulate(ins), *out);
        }
    }
}
