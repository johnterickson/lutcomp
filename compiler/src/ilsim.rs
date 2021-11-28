use crate::*;
use crate::il::*;

struct SimulationContext<'a> {
    program: &'a IlProgram,
    entry_function: &'a IlFunctionId,
    entry_args: &'a [IlNumber],
    stack_pointer: u32,
    mem: BTreeMap<u32, u8>,
}

impl<'a> SimulationContext<'a> {
    fn run(&mut self) -> IlNumber {
        let entry = self.program.functions.get(&self.entry_function).unwrap();
        entry.simulate(self, &self.entry_args).unwrap()
    }
}

impl IlProgram {
    fn create_sim<'a>(&'a self, args: &'a [IlNumber]) -> SimulationContext<'a> {
        SimulationContext {
            program: &self,
            entry_function: &self.entry,
            entry_args: args,
            stack_pointer: u32::max_value()-4+1,
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

    fn simulate<'a>(&self, ctxt: &mut SimulationContext<'a>, args: &[IlNumber]) -> Option<IlNumber> {
        let mut vars: BTreeMap<&IlVarId, Option<IlNumber>> = BTreeMap::new();

        for (id, info) in self.vars.iter().rev() {
            if let Location::FrameOffset(stack_size) = info.location {
                ctxt.stack_pointer -= stack_size;
                assert!(vars.insert(id, Some(IlNumber::U32(ctxt.stack_pointer))).is_none());
            }
            if let Location::Static(addr) = info.location {
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
                IlInstruction::AssignAtom { dest, src , size} => {

                    if let Some(Some(n)) = vars.get(dest) {
                        assert_eq!(&n.il_type(), size);
                    }

                    let value = src.value(&vars);
                    assert_eq!(&value.il_type(), size);
                    vars.insert(dest, Some(value));
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
                                IlBinaryOp::BitwiseAnd => n1 & n2,
                                IlBinaryOp::BitwiseOr => n1 | n2,
                            })
                        }
                        (IlNumber::U32(n1), IlNumber::U32(n2)) => {
                            IlNumber::U32(match op {
                                IlBinaryOp::Add => n1.wrapping_add(n2),
                                IlBinaryOp::Subtract => n1.wrapping_sub(n2),
                                IlBinaryOp::Multiply => unimplemented!(),
                                IlBinaryOp::BitwiseAnd => n1 & n2,
                                IlBinaryOp::BitwiseOr => n1 | n2,
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
                    match (result, ret) {
                        (Some(result), Some(ret)) => {
                            vars.insert(ret, Some(result));
                        }
                        (None, None) => {}
                        _ => panic!("Callee '{}' returned {:?}, but {:?} was expected.", f.id.0, result, f.ret),
                    }
                },
                IlInstruction::Return { val } => {
                    return val.as_ref().map(|v| vars[v].unwrap());
                },
                IlInstruction::TtyIn { .. } => todo!(),
                IlInstruction::TtyOut { .. } => todo!(),
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
                IlInstruction::GetFrameAddress { dest } => {
                    vars.insert(dest, Some(IlNumber::U32(ctxt.stack_pointer)));
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
    use crate::{backend::emit_assembly, tests::{TestComputer, TestVar, check_args, test_programs_dir}};

    use super::*;

    fn run_var_input<'a>(
        ctxt: &'a ProgramContext,
        il: &'a IlProgram,
        rom: &'a Image,
        ins: &Vec<TestVar>) -> (TestComputer<'a>, IlNumber)
    {
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

        let mut c = TestComputer::from_rom(&ctxt, &rom);
        for (addr, b) in &mem {
            *c.comp.mem_byte_mut(*addr) = *b;
        }

        // run in IL simulator
        let mut sim = il.create_sim(&args);
        sim.mem.append(&mut mem);
        let il_result = sim.run();

        // run in HW simulator
        let hw_sim_args: Vec<_> = args.iter().map(|a| match a {
            IlNumber::U8(n) => (*n).into(),
            IlNumber::U32(n) => *n,
        }).collect();
        let hw_result = c.run(hw_sim_args.as_slice());
        let hw_result = match il_result.il_type() {
            IlType::U32 => IlNumber::U32(hw_result),
            IlType::U8 => IlNumber::U8((hw_result & 0xFF) as u8)
        };

        assert_eq!(il_result, hw_result, "IL and HW have different results for {:?}", ins);
        (c, il_result)
    }
    
    fn test_var_inputs(entry: &str, program: &str, cases: &[(Vec<TestVar>, TestVar)]) {
        let (ctxt, il) = emit_il(entry, program, &test_programs_dir());
        // dbg!(&il);

        let (_, asm) = emit_assembly(&ctxt, &il);
        let rom = assemble::assemble(asm);

        for case in cases
        {
            check_args(&ctxt, case);
            let (ins, expected) = case;

            let (_, actual) = run_var_input(&ctxt, &il, &rom, ins);
            
            let expected= match expected {
                TestVar::Ascii(_) => todo!(),
                TestVar::Ptr(_) => todo!(),
                TestVar::U8(n) => IlNumber::U8(*n),
                TestVar::Usize(n) => IlNumber::U32(*n),
            };
            assert_eq!(expected, actual, "{:?}", &(ins, expected));
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

    fn test_tty(entry: &str, program: &str, pairs: &[(&str,u32,u32,&str)]) {
        let (ctxt, il) = emit_il(entry,program, &test_programs_dir());
        // dbg!(&il);

        let (_, asm) = emit_assembly(&ctxt, &il);
        let rom = assemble::assemble(asm);

        for (ttyin, input1, input2, expected) in pairs {
            let mut c = TestComputer::from_rom(&ctxt, &rom);
            dbg!((ttyin, input1, input2, expected));
            for ch in ttyin.chars() {
                c.comp.tty_in.push_back(ch as u8);
            }
            assert_eq!(0, c.run(&[*input1, *input2]) & 0xFF);

            let mut out = String::new();
            for c in &c.comp.tty_out {
                let c = *c as char;
                out.push(c);
            }

            assert_eq!(out.as_str(), *expected);
        }
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

    #[test]
    fn struct_pass_by_ref() {
        test_inputs(
            "test_add",
            include_str!("../../programs/struct.j"),
            &[
                (0x0u32,0x0u32,0x0u32),
                (0x0,0x1,0x1),
                (0x1,0x0,0x1),
                (0x1,0x1,0x2),
                (0xAABBCCDD, 0x11111111, 0xBBCCDDEE),
                (0x1,0xFF,0x100),
                (0xAABBCCDD, 0x0, 0xAABBCCDD),
                (0xFFFFFFFF, 0x1, 0x0),
                ]);
    }

    #[test]
    fn struct_return_by_ref() {
        test_inputs(
            "test_ret_static",
            include_str!("../../programs/struct_ret.j"),
            &[
                (0xAABBCCDDu32, 0x11111111u32, 0xBBCCDDEEu32)
                ]);
    }

    #[test]
    fn heap_nofree() {
        test_var_inputs(
            "get_heap",
            include_str!("../../programs/heap_nofree.j"),
            &[
                (vec![], STATICS_START_ADDRESS.into())
            ]
        );
    }

    
    #[test]
    fn heap_nofree_alloc() {
        test_var_inputs(
            "test1",
            include_str!("../../programs/heap_nofree.j"),
            &[
                (vec![], 0u32.into())
            ]
        );
    }

    fn assemble(entry: &str, program: &str) -> (ProgramContext, IlProgram, Image) {
        let (ctxt, program) = emit_il(entry, program, &test_programs_dir());
        let (_, assembly) = emit_assembly(&ctxt, &program);
        let rom = assemble::assemble(assembly);
        (ctxt, program, rom)
    }

    #[test]
    fn heap_init() {
        let (ctxt, il, rom) = assemble("heap_init", include_str!("../../programs/heap.j"));
        let (_, heap_start) = run_var_input(&ctxt, &il, &rom, &vec![]);
        assert_eq!(heap_start, IlNumber::U32(STATICS_START_ADDRESS));

        let (ctxt, il, rom) = assemble("test_get_heap_head", include_str!("../../programs/heap.j"));
        let (c, heap_entry) = run_var_input(&ctxt, &il, &rom, &vec![]);
        assert_eq!(heap_entry, IlNumber::U32(STATICS_START_ADDRESS+4));

        let heap_type = ctxt.types.get("heap").unwrap();
        let (head_offset, _) = heap_type.get_field("head");
        let heap_entry_type = ctxt.types.get("heap_entry").unwrap();

        let header_size = heap_entry_type.byte_count(&ctxt);

        let heap_start = heap_start.as_u32();
        let head_entry_addr = heap_start + 4;
        assert_eq!(c.comp.mem_word(heap_start + head_offset), head_entry_addr);
        assert_eq!(c.comp.mem_word(head_entry_addr), 0); 
        let len = 1024-header_size;
        assert_eq!(c.comp.mem_word(head_entry_addr+4), len); 
        assert_eq!(c.comp.mem_byte(head_entry_addr+8), 1); 

        let max_static = ctxt.statics_cur_address;
        assert_eq!(max_static, head_entry_addr + header_size + len);
    }

    // #[test]
    // fn heap_is_entry_bad() {
    //     let (ctxt, rom) = assemble("test_heap_is_entry_bad", include_str!("../../programs/heap.j"));
    //     let (_, is_bad) = test_var_input(&ctxt, &rom, &vec![1u32.into()]);
    //     assert_eq!(is_bad & 0xFF, 0);

    //     let (_, is_bad) = test_var_input(&ctxt, &rom, &vec![1024u32.into()]);
    //     assert_eq!(is_bad & 0xFF, 1);
    // }

    // #[test]
    // fn heap_alloc() {
    //     let alloc_size = 4u32;
    //     let (ctxt, rom) = assemble("test_heap_alloc", include_str!("../../programs/heap.j"));
    //     let (c, allocated_addr) = test_var_input(&ctxt, &rom, &vec![alloc_size.into()]);

    //     const HEADER_SIZE : u32 = 0xc;
        
    //     let heap_addr = STATICS_START_ADDRESS;
    //     let max_static = ctxt.statics_cur_address;
    //     let head_entry_addr = heap_addr + 4;
    //     assert_eq!(c.comp.mem_word(heap_addr), head_entry_addr);
    //     let new_entry_addr = c.comp.mem_word(head_entry_addr);
    //     assert_eq!(new_entry_addr, head_entry_addr+1024-HEADER_SIZE-alloc_size); 
    //     let head_entry_len = 1024-HEADER_SIZE-HEADER_SIZE-alloc_size;
    //     assert_eq!(c.comp.mem_word(head_entry_addr+4), head_entry_len); 
    //     assert_eq!(c.comp.mem_byte(head_entry_addr+8), 1); 

    //     assert_eq!(new_entry_addr+HEADER_SIZE, allocated_addr);

    //     assert_eq!(c.comp.mem_word(new_entry_addr), 0);
    //     assert_eq!(c.comp.mem_word(new_entry_addr+4), alloc_size);
    //     assert_eq!(c.comp.mem_byte(new_entry_addr+8), 0);
    //     assert_eq!(max_static-alloc_size, allocated_addr);
    //     assert_eq!(new_entry_addr+HEADER_SIZE, allocated_addr);
    // }


    #[test]
    fn divide() {
        let (_ctxt, il) = emit_il(
            "divide",
            include_str!("../../programs/divide.j"),
            &test_programs_dir());

        assert_eq!(il.simulate(&[1u8.into(), 1u8.into()]), 1u8.into());
        assert_eq!(il.simulate(&[2u8.into(), 1u8.into()]), 2u8.into());
        assert_eq!(il.simulate(&[1u8.into(), 2u8.into()]), 0u8.into());
        assert_eq!(il.simulate(&[100u8.into(), 10u8.into()]), 10u8.into());
    }

    
    #[test]
    fn print_hex() {
        test_tty(
            "printHexTest",
            include_str!("../../programs/print_hex.j"),
            &[
                ("",0x0,0x0,"00\n"),
                ("",0x1,0x0,"01\n"),
                ("",0x9,0x0,"09\n"),
                ("",0xA,0x0,"0A\n"),
                ("",0xF,0x0,"0F\n"),
                ("",0x10,0x0,"10\n"),
                ("",0xAA,0x0,"AA\n"),
                ("",0xFF,0x0,"FF\n"),
                ]);
    }

    #[test]
    fn echo() {
        test_tty(
            "main",
            include_str!("../../programs/echo.j"),
            &[
                ("0\nq",0x0,0x0,"Hi!\n:>0\n:>q"),
                ("01\nq",0x0,0x0,"Hi!\n:>01\n:>q"),
                ]);
    }

    // #[test]
    // fn echoline() {
    //     test_tty(
    //         "test_echoline",
    //         include_str!("../../programs/echoline.j"),
    //         &[
    //             ("0\n",0x0,0x0,"0\n"),
    //             ("01\n",0x0,0x0,"01\n"),
    //             ]);
    // }

    #[test]
    fn local_array() {
        let (_ctxt, il) = emit_il(
            "main",
            include_str!("../../programs/local_array.j"),
            &test_programs_dir());

        assert_eq!(il.simulate(&[0u8.into(), 0u8.into()]), 0u8.into());
        assert_eq!(il.simulate(&[0u8.into(), 1u8.into()]), 1u8.into());
        assert_eq!(il.simulate(&[1u8.into(), 0u8.into()]), 1u8.into());
        assert_eq!(il.simulate(&[1u8.into(), 0xFFu8.into()]), 0u8.into());
    }

    #[test]
    fn array_to_ptr() {
        test_inputs(
            "main",
            include_str!("../../programs/array_to_ptr.j"),
            &[
                (0x0u8,0x0u8,0x0u8),
                (0x0,0x1,0x1),
                (0x1,0x0,0x1),
                (0x1,0xFF,0x0),
                ]);
    }

    #[test]
    fn array_loop() {
        test_inputs(
            "main",
            include_str!("../../programs/array_loop.j"),
            &[
                (0x0u8,0x0u8,0x0u8),
                (0x0,0x1,0x1),
                (0x1,0x0,0x1),
                (0x1,0xFF,0x0),
                ]);
    }

    #[test]
    fn strlen() {
        let mut long: Vec<u8> = (0..300).map(|_| 'a' as u8).collect();
        long.push(0);
        let expected = long.len() as u32 - 1;

        test_var_inputs(
            "strlen",
            include_str!("../../programs/strlen.j"),
            &[
                (vec![TestVar::Ascii(b"\0")], TestVar::Usize(0)),
                (vec![TestVar::Ascii(b"hello\0")], TestVar::Usize(5)),
                (vec![TestVar::Ptr(long)], TestVar::Usize(expected)),
            ]
        );
    }

    #[test]
    fn strncmp() {
        let same_len = 300u32;
        let mut long1: Vec<u8> = (0..same_len).map(|_| 'a' as u8).collect();
        let mut long2 = long1.clone();
        long1.push(b'a');
        long1.push(0);
        long2.push(b'b');
        long2.push(0);

        test_var_inputs(
            "strncmp",
            include_str!("../../programs/strncmp.j"),
            &[
                (vec![TestVar::Ascii(b"\0"), TestVar::Ascii(b"\0"), TestVar::Usize(0)], TestVar::U8(0)),
                (vec![TestVar::Ascii(b"\0"), TestVar::Ascii(b"\0"), TestVar::Usize(1)], TestVar::U8(0)),
                (vec![TestVar::Ascii(b"\0"), TestVar::Ascii(b"\0"), TestVar::Usize(2)], TestVar::U8(0)),
                (vec![TestVar::Ascii(b"a\0"), TestVar::Ascii(b"a\0"), TestVar::Usize(1)], TestVar::U8(0)),
                (vec![TestVar::Ascii(b"aa\0"), TestVar::Ascii(b"ab\0"), TestVar::Usize(1)], TestVar::U8(0)),
                (vec![TestVar::Ascii(b"aa\0"), TestVar::Ascii(b"ab\0"), TestVar::Usize(2)], TestVar::U8(255)),
                (vec![TestVar::Ascii(b"ab\0"), TestVar::Ascii(b"aa\0"), TestVar::Usize(2)], TestVar::U8(1)),
                (vec![TestVar::Ascii(b"ab\0"), TestVar::Ascii(b"aa\0"), TestVar::Usize(1000)], TestVar::U8(1)),
                (vec![TestVar::Ptr(long1.clone()), TestVar::Ptr(long2.clone()), TestVar::Usize(same_len)], TestVar::U8(0)),
                (vec![TestVar::Ptr(long1), TestVar::Ptr(long2), TestVar::Usize(same_len+1)], TestVar::U8(255)),
            ]
        );
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

    #[test]
    fn parse_hex_nibble() {
        test_var_inputs(
            "parseHexNibble",
            include_str!("../../programs/bootram.j"),
            &[
                (vec![TestVar::U8('9' as u8)], TestVar::U8(0x9)),
                (vec![TestVar::U8('a' as u8)], TestVar::U8(0xA)),
            ]
        );
    }
}
