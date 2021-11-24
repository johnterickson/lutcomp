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
            if let Location::Stack(stack_size) = info.location {
                ctxt.stack_pointer -= stack_size;
                assert!(vars.insert(id, Some(IlNumber::U32(ctxt.stack_pointer))).is_none());
            }
            if let Location::Static(size) = info.location {
                let key = (Some(self.id.clone()), id.clone());
                let info = ctxt.program.statics.get(&key).unwrap();
                let addr = match &info.location {
                    Location::Static(addr) => *addr,
                    _ => panic!(),
                };
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
                            IlType::U8 => {
                                vars.insert(dest, Some(IlNumber::U8(ctxt.mem[&addr])));
                            }
                            IlType::U32 => {
                                assert_eq!(0, addr % 4);
                                let r = u32::from_le_bytes([
                                    ctxt.mem[&(addr+0)],
                                    ctxt.mem[&(addr+1)],
                                    ctxt.mem[&(addr+2)],
                                    ctxt.mem[&(addr+3)],
                                ]);
                                vars.insert(dest, Some(IlNumber::U32(r)));
                            }
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
                                assert_eq!(size, &IlType::U8);
                                ctxt.mem.insert(addr, n);
                            },
                            IlNumber::U32(n) => {
                                assert_eq!(0, addr % 4);
                                assert_eq!(size, &IlType::U32);
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
                    let src = vars.get(src).unwrap().unwrap();
                    assert_eq!(&src.il_type(), src_size);
                    let n = match (dest_size, src) {
                        (IlType::U32, IlNumber::U8(n)) => IlNumber::U32(n as u32),
                        (IlType::U8, IlNumber::U32(n)) => IlNumber::U8((n & 0xFF) as u8),
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
    use crate::tests::{TestComputer, TestVar, test_programs_dir};

    use super::*;

    fn emit_il(entry: &str, input: &str) -> IlProgram {
        let ctxt = create_program(
            entry, 
            input,
            &test_programs_dir());
        
        IlProgram::from_program(&ctxt)
    }

    
    fn test_var_inputs(entry: &str, program: &str, cases: &[(Vec<TestVar>, TestVar)]) {
        let il = emit_il(entry,program);

        for case in cases
        {
            let  (ins, expected) = case;
            let mut ptr = TestComputer::arg_base_addr();
            let mut args = Vec::new();
            let mut mem = BTreeMap::new();
            for input in ins {
                if let Some(bytes) = match input {
                    TestVar::Ascii(s) => Some(*s),
                    TestVar::Ptr(s) => Some(s.as_slice()),
                    TestVar::U8(u8) => { args.push(IlNumber::U8(*u8)); None}
                    TestVar::Usize(u32) => { args.push(IlNumber::U32(*u32)); None}
                } {
                    let arg = ptr;
                    for b in bytes {
                        mem.insert(ptr, *b);
                        ptr += 1;
                    }
                    args.push(IlNumber::U32(arg));

                    ptr = (ptr+3)/4*4; // round up
                    ptr += 4; // add buffer
                }
            }

            let mut sim = il.create_sim(&args);
            sim.mem.append(&mut mem);
            let expected= match expected {
                TestVar::Ascii(_) => todo!(),
                TestVar::Ptr(_) => todo!(),
                TestVar::U8(n) => IlNumber::U8(*n),
                TestVar::Usize(n) => IlNumber::U32(*n),
            };
            assert_eq!(expected, sim.run(), "{:?}", &case);
        }
    }

    fn test_inputs<'a, T1,T2,T3>(entry: &str, program: &str, cases: &'a [(T1,T2,T3)])
        where TestVar: From<&'a T1>, TestVar: From<&'a T2>, TestVar: From<&'a T3>
    {
        let cases: Vec<(Vec<TestVar>,TestVar)> = cases
            .iter()
            .map(|(in1,in2,out)| ([in1.into(), in2.into()].to_vec(), out.into()))
            .collect();
        test_var_inputs(entry, program, cases.as_slice());
    }


    #[test]
    fn halt() {
        test_var_inputs(
            "main",
            include_str!("../../programs/halt.j"),
            &[(vec![],1u8.into())]);
    }

    #[test]
    fn halt_base_address() {
        test_var_inputs(
            "main",
            include_str!("../../programs/halt_base_address.j"),
            &[(vec![],1u8.into())]);
    }

    #[test]
    fn add_u8() {
        test_var_inputs(
            "main",
            include_str!("../../programs/add_u8.j"),
            &[(vec![],7u8.into())]);
    }

    #[test]
    fn call_parameterless() {
        test_var_inputs(
            "main",
            include_str!("../../programs/call_parameterless.j"),
            &[(vec![],7u8.into())]);
    }

    #[test]
    fn idfn() {
        test_var_inputs(
            "main",
            include_str!("../../programs/idfn.j"),
            &[(vec![],TestVar::U8(7u8))]);
    }

    #[test]
    fn if_eq() {
        test_inputs(
            "main",
            include_str!("../../programs/if_eq.j"),
            &[(6u8,7u8,0u8), (8,7,0), (0,7,0), (0xFF,7,0), (7,7,1)]);
    }

    #[test]
    fn if_gt_unsigned() {
        test_inputs(
            "main",
            include_str!("../../programs/if_gt.j"),
            &[
                (0u8,1u8,0u8),
                (1,2,0),
                (8,7,1),
                (0,0xFF,0),
                (0x7F,7,1), (0xFF, 7, 1), (7,7,0)]);
    }

    #[test]
    fn if_gte_unsigned() {
        test_inputs(
            "main",
            include_str!("../../programs/if_gte.j"),
            &[(6u8,7u8,0u8), (8,7,1), (0,7,0), (0x7F,7,1), (0xFF, 7, 1), (7,7,1)]);
    }

    #[test]
    fn if_lt_unsigned() {
        test_inputs(
            "main",
            include_str!("../../programs/if_lt.j"),
            &[
            (0u8,0u8,0u8),
            (0,1,1),
            (6,7,1), (8,7,0), (0,7,1), (7,0,0), (0x7F,7,0), (0xFF, 7, 0), (7,7,0)]);
    }

    #[test]
    fn if_lte_unsigned() {
        test_inputs(
            "main",
            include_str!("../../programs/if_lte.j"),
            &[(6u8,7u8,1u8), (8,7,0), (0,7,1), (0x7F,7,0), (0xFF, 7, 0), (7,7,1)]);
    }

    #[test]
    fn if_ne() {
        test_inputs(
            "main",
            include_str!("../../programs/if_ne.j"),
            &[(6u8,7u8,1u8),(8,7,1),(0,7,1),(0xFF,7,1), (7,7,0)]);
    }

    #[test]
    fn if_ne_uptr() {
        test_inputs(
            "main",
            include_str!("../../programs/if_ne_uptr.j"),
            &[
                (6u32,7u32,1u8),(8,7,1),(0,7,1),(0xFF,7,1), (7,7,0),
                (0x0,0x100,1),(0x100,0x100,0),(0xAABBCCDD,0xAABBCCDD,0),
                (0xAABBCCDD,0xAABBCCDE,1),(0xAABBCCDD,0xAABBCDDD,1),
                (0xAABBCCDD,0xAABCCCDD,1),(0xAABBCCDD,0xABBBCCDD,1)]);
    }

    #[test]
    fn plusone() {
        test_var_inputs(
            "main",
            include_str!("../../programs/plusone.j"),
            &[(vec![],7u8.into())]);

    }

    #[test]
    fn fac_rec() {
        test_var_inputs(
            "main",
            include_str!("../../programs/fac_rec.j"),
            &[
                (vec![0x0u8.into()],1u8.into()),
                (vec![0x1u8.into()],1u8.into()),
                (vec![0x2u8.into()],2u8.into()),
                (vec![0x5u8.into()],120u8.into()),
            ]);
    }

    #[test]
    fn fac_iter() {
        test_var_inputs(
            "fac",
            include_str!("../../programs/fac_iter.j"),
            &[
                (vec![0u8.into()],1u8.into()),
                (vec![1u8.into()],1u8.into()),
                (vec![2u8.into()],2u8.into()),
                (vec![5u8.into()],120u8.into()),
            ]);
    }

    #[test]
    fn fib() {
        test_var_inputs(
            "fib",
            include_str!("../../programs/fib.j"),
            &[
                (vec![0u8.into()],0u8.into()),
                (vec![1u8.into()],1u8.into()),
                (vec![2u8.into()],1u8.into()),
                (vec![3u8.into()],2u8.into()),
                // (vec![13u8.into()],233u8.into()),
                ]);
    }

    #[test]
    fn fib_memo() {
        test_var_inputs(
            "main",
            include_str!("../../programs/fib_memo.j"),
            &[
                (vec![0u8.into()],0u8.into()),
                (vec![1u8.into()],1u8.into()),
                (vec![2u8.into()],1u8.into()),
                (vec![3u8.into()],2u8.into()),
                (vec![13u8.into()],233u8.into()),
                ]);
    }


    #[test]
    fn statics() {
        test_var_inputs(
            "main",
            include_str!("../../programs/static.j"),
            &[
                (vec![1u8.into()],2u8.into()),
                (vec![2u8.into()],4u8.into()),
                ]);
    }
    
    #[test]
    fn add_uptr() {
        test_inputs(
            "main",
            include_str!("../../programs/add_uptr.j"),
            &[
                (0x0u32,0x0u32,0x0u32),
                (0x0,0x1,0x1),
                (0x1,0x0,0x1),
                (0x1,0x1,0x2),
                (0x1,0xFF,0x100),
                (0xAABBCCDD, 0x0, 0xAABBCCDD),
                (0xAABBCCDD, 0x11111111, 0xBBCCDDEE),
                (0xFFFFFFFF, 0x1, 0x0),
                ]);
    }

    #[test]
    fn cmp_usize() {
        test_inputs(
            "cmp_usize",
            include_str!("../../programs/cmp_usize.j"),
            &[
                (0x0u32,0x0u32,0x0u8),
                (0x0,0x1,0xFF),
                (0x1,0x0,0x1),
                (0x100,0x0,0x1),
                (0x100,0x2,0x1),
                (0x0,0x100,0xFF),
                (0x2,0x100,0xFF),
                (0xbbccddee,0xbbccddee,0x0),
                (0x01020304,0x04030201,0xFF),
                (0x04030201,0x01020304,0x1),
                ]);
    }

    #[test]
    fn ptr() {
        test_var_inputs(
            "main",
            include_str!("../../programs/ptr.j"),
            &[
                (vec![0u32.to_le_bytes().into(), 0u32.to_le_bytes().into()], 0u32.into()),
                (vec![0u32.to_le_bytes().into(), 1u32.to_le_bytes().into()], 1u32.into()),
                (vec![1u32.to_le_bytes().into(), 2u32.to_le_bytes().into()], 3u32.into()),
                (vec![0xAABBCCDDu32.to_le_bytes().into(), 0x11111111u32.to_le_bytes().into()], 0xBBCCDDEEu32.into()),
                (vec![0xFFFFFFFFu32.to_le_bytes().into(), 0x1u32.to_le_bytes().into()], 0x0u32.into()),
            ]
        );
    }

    // #[test]
    // fn struct_pass_by_ref() {
    //     test_inputs(
    //         "test_add",
    //         include_str!("../../programs/struct.j"),
    //         &[
    //             (0x0u32,0x0u32,0x0u32),
    //             (0x0,0x1,0x1),
    //             (0x1,0x0,0x1),
    //             (0x1,0x1,0x2),
    //             (0xAABBCCDD, 0x11111111, 0xBBCCDDEE),
    //             (0x1,0xFF,0x100),
    //             (0xAABBCCDD, 0x0, 0xAABBCCDD),
    //             (0xFFFFFFFF, 0x1, 0x0),
    //             ]);
    // }


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
    fn strstr() {
        test_var_inputs(
            "strstr",
            include_str!("../../programs/strstr.j"),
            &[
                (vec![TestVar::Ascii(b"\0"), TestVar::Ascii(b"\0")], TestVar::Usize(0)),
                (vec![TestVar::Ascii(b"\0"), TestVar::Ascii(b"a\0")], TestVar::Usize(0)),
                (vec![TestVar::Ascii(b"a\0"), TestVar::Ascii(b"\0")], TestComputer::arg_base_addr_var(0)),
                (vec![TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"h\0")], TestComputer::arg_base_addr_var(0)),
                (vec![TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"he\0")], TestComputer::arg_base_addr_var(0)),
                (vec![TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"Z\0")], TestVar::Usize(0)),
                (vec![TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"hi\0")], TestVar::Usize(0)),
                (vec![TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"el\0")], TestComputer::arg_base_addr_var(1)),
                (vec![TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"ll\0")], TestComputer::arg_base_addr_var(2)),
                (vec![TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"lo\0")], TestComputer::arg_base_addr_var(3)),
                (vec![TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"l\0")], TestComputer::arg_base_addr_var(2)),
            ]);
    }
}
