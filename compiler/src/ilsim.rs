use crate::*;
use crate::il::*;

use std::collections::VecDeque;
use std::collections::btree_map::Entry;

#[derive(Debug)]
struct SimulationContext<'a> {
    program: &'a IlProgram,
    entry_function: &'a IlFunctionId,
    entry_args: &'a [Number],
    stack_pointer: u32,
    mem: BTreeMap<u32, u8>,
    tty_in: VecDeque<u8>,
}

impl<'a> SimulationContext<'a> {
    fn run(&mut self) -> (u64, Number) {
        let entry = self.program.functions.get(&self.entry_function).unwrap();
        let (ticks, result) = entry.simulate(self, &self.entry_args);
        (ticks, result.unwrap())
    }
}

impl IlProgram {
    fn create_sim<'a>(&'a self, args: &'a [Number]) -> SimulationContext<'a> {
        SimulationContext {
            program: &self,
            entry_function: &self.entry,
            entry_args: args,
            stack_pointer: u32::max_value()-4+1,
            mem: BTreeMap::new(),
            tty_in: VecDeque::new(),
        }
    }

    pub fn simulate_with_ticks(&self, args: &[Number]) -> (u64, Number) {
        self.create_sim(args).run()
    }

    pub fn simulate(&self, args: &[Number]) -> Number {
        self.create_sim(args).run().1
    }
}

impl IlFunction {
    fn find_label(&self, label: &IlLabelId) -> Option<usize> {
        self.body.iter()
            .enumerate()
            .filter_map(|(i,s)| {
                if let IlInstruction::Label(l) = &s.0 {
                    if l == label {
                        return Some(i);
                    }
                }
                None
            })
            .next()
    }

    fn simulate<'a>(&self, ctxt: &mut SimulationContext<'a>, args: &[Number]) -> (u64, Option<Number>) {
        let mut vars: BTreeMap<&IlVarId, Number> = BTreeMap::new();

        let last_const_addr = ROM_MAX;
        let const_addrs: BTreeMap<_,_> = ctxt.program.consts.iter().map(|((_,name), info)| {
            let addr = last_const_addr - info.byte_size;
            let addr = addr / 4 * 4;
            (name.clone(), addr)
        }).collect();

        for (id, info) in self.vars.iter().rev() {
            if let IlLocation::Static(addr) = info.location {
                assert!(vars.insert(id, Number::U32(addr)).is_none());
            }
        }

        assert_eq!(self.args.len(), args.len());
        for (arg_id, arg_value) in self.args.iter().zip(args) {
            assert!(vars.insert(arg_id, *arg_value).is_none());
        }

        let frame_pointer = IlVarId::frame_pointer();

        let mut s_index = 0;
        let mut ticks = 0;
        loop {
            ticks += 1;
            if let Some(fp) = vars.get(&frame_pointer) {
                ctxt.stack_pointer = fp.as_u32();
            }
            vars.insert(&frame_pointer, Number::U32(ctxt.stack_pointer));

            let mut inc_pc = true;

            let s = &self.body[s_index];
            // println!("{} {:?};", self.id.0, s);
            // {
            //     let usages = s.var_usages();
            //     if usages.dest.is_some() || usages.srcs.len() > 0 {
            //         print!(" ");
            //         if let Some(dest) = usages.dest {
            //             print!("{}[{:?}] <- ", dest.0, vars.get(&dest));
            //         }

            //         for src in usages.srcs {
            //             print!(" {}[{:?}]", src.0, vars.get(&src));
            //         }

            //         println!();
            //     }
            // }
            // println!("{} {:?}; [{:?}]", self.id.0, s, vars);

            // dbg!(&s.0);

            match &s.0 {
                IlInstruction::Unreachable => panic!(),
                IlInstruction::Comment(_) | IlInstruction::Label(_) => {},
                IlInstruction::AssignVar { dest, src , size, dest_range, src_range} => {
                    let value = *vars.get(src).expect(&format!("Could not find var {}.", src));

                    let value = if let Some(src_range) = src_range {
                        match value {
                            Number::U32(n) => {
                                assert_eq!(src_range.len(), 1);
                                let b= n.to_le_bytes()[src_range.start as usize];
                                Number::U8(b.into())
                            }
                            Number::U8(_) | Number::U16(_) => panic!(),
                        }
                    } else {
                        value
                    };

                    let entry = vars.entry(dest);

                    match (entry, dest_range) {
                        (Entry::Vacant(_), Some(_)) => panic!("Cannot write to subrange of uninitialized value."),
                        (Entry::Vacant(emtpy), None) => {
                            emtpy.insert(value);
                        },
                        (Entry::Occupied(mut existing), None) => {
                            assert_eq!(existing.get().num_type(), *size);
                            existing.insert(value);
                        },
                        (Entry::Occupied(mut existing), Some(dest_range)) => {
                            assert_eq!(dest_range.len(), 1);
                            match (&value, existing.get_mut()) {
                                (Number::U8(value), &mut Number::U32(ref mut n)) => {
                                    let mut bytes = n.to_le_bytes();
                                    bytes[dest_range.start as usize] = *value;
                                    *n = u32::from_le_bytes(bytes);
                                }
                                other => panic!("{:?}", other),
                            };
                        }
                    }
                },
                IlInstruction::AssignNumber { dest, src} => {

                    if let Some(n) = vars.get(dest) {
                        assert_eq!(n.num_type(), src.num_type());
                    }

                    vars.insert(dest, *src);
                },
                IlInstruction::AssignUnary { dest, op, src } => {
                    let src = vars.get(src).unwrap();
                    let result = match src {
                        Number::U32(n) => {
                            Number::U32(match op {
                                IlUnaryOp::Negate => n.wrapping_neg(),
                                IlUnaryOp::BinaryInvert => !n,
                            })
                        }
                        Number::U16(n) => {
                            Number::U16(match op {
                                IlUnaryOp::Negate => n.wrapping_neg(),
                                IlUnaryOp::BinaryInvert => !n,
                            })
                        }
                        Number::U8(n) => {
                            Number::U8(match op {
                                IlUnaryOp::Negate => n.wrapping_neg(),
                                IlUnaryOp::BinaryInvert => !n,
                            })
                        }
                    };
                    vars.insert(dest, result);
                },
                IlInstruction::AssignBinary { dest, op, src1, src2 } => {
                    let src1 = *vars.get(src1)
                        .expect(&format!("Could not find {:?}.", &src1));
                    let src2 = match src2 {
                        IlOperand::Number(n) => *n,
                        IlOperand::Var(src2) => *vars.get(src2)
                            .expect(&format!("Could not find {:?}.", &src2)),
                    };
                    let result = op.eval(src1, src2);
                    vars.insert(dest, result);
                }
                IlInstruction::ReadMemory { dest, addr, size } => {
                    let addr = *vars.get(addr).unwrap();
                    if let Number::U32(addr) = addr {
                        match size {
                            NumberType::U8 => {
                                vars.insert(dest, Number::U8(ctxt.mem[&addr]));
                            }
                            NumberType::U16 => {
                                assert_eq!(0, addr % 2);
                                let r = u16::from_le_bytes([
                                    ctxt.mem[&(addr+0)],
                                    ctxt.mem[&(addr+1)],
                                ]);
                                vars.insert(dest, Number::U16(r));
                            }
                            NumberType::U32 => {
                                assert_eq!(0, addr % 4);
                                let r = u32::from_le_bytes([
                                    ctxt.mem[&(addr+0)],
                                    ctxt.mem[&(addr+1)],
                                    ctxt.mem[&(addr+2)],
                                    ctxt.mem[&(addr+3)],
                                ]);
                                vars.insert(dest, Number::U32(r));
                            }
                        }
                    } else {
                        panic!();
                    }
                }
                IlInstruction::WriteMemory { addr, src, size} => {
                    let src = vars[src];
                    let addr = vars[addr];
                    if let Number::U32(addr) = addr {
                        match src {
                            Number::U8(n) => {
                                assert_eq!(size, &NumberType::U8);
                                ctxt.mem.insert(addr, n);
                            },
                            Number::U16(n) => {
                                assert_eq!(0, addr % 2);
                                assert_eq!(size, &NumberType::U16);
                                let bytes = n.to_le_bytes();
                                for (i, b) in bytes.iter().enumerate() {
                                    let i = i as u32;
                                    ctxt.mem.insert(addr + i, *b);
                                }
                            },
                            Number::U32(n) => {
                                assert_eq!(0, addr % 4);
                                assert_eq!(size, &NumberType::U32);
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
                    let left = vars[left];
                    let right = vars[right];
                    let condition_true = match (left, right) {
                        (Number::U8(left), Number::U8(right)) => {
                            match op {
                                IlCmpOp::Equals => left == right,
                                IlCmpOp::NotEquals => left != right,
                                IlCmpOp::GreaterThan => left > right,
                                IlCmpOp::GreaterThanOrEqual => left >= right,
                                IlCmpOp::LessThan => left < right,
                                IlCmpOp::LessThanOrEqual => left <= right,
                            }
                        }
                        (Number::U32(left), Number::U32(right)) => {
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
                    let args: Vec<_> = args.iter().map(|a| vars[a]).collect();

                    let result = if let Some(i) = &f.intrinsic {
                        match i {
                            Intrinsic::Mul8_16 => {
                                let src1 = match args[0] {
                                    Number::U8(n) => n,
                                    _ => panic!(),
                                };
                                let src2 = match args[1] {
                                    Number::U8(n) => n,
                                    _ => panic!(),
                                };
                                Some(Number::U16((src1 as u16)*(src2 as u16)))
                            },
                            _ => todo!(),
                        }
                    } else {
                       let (caller_ticks, result) = f.simulate(ctxt, &args);
                       ticks += caller_ticks;
                       result
                    };

                    match (result, ret) {
                        (Some(result), Some(ret)) => {
                            vars.insert(ret, result);
                        }
                        (None, None) => {}
                        _ => panic!("Callee '{}' returned {:?}, but {:?} was expected.", f.id.0, result, f.ret),
                    }
                },
                IlInstruction::Return { val } => {
                    return (ticks, val.as_ref().map(|v| vars[v]));
                },
                IlInstruction::TtyIn { dest } => {
                    {
                        let stdin_channel = NONBLOCKING_STDIN.lock().unwrap();
                        if let Ok(line) = stdin_channel.try_recv() {
                            for c in line.chars() {
                                ctxt.tty_in.push_back(c as u8);
                            }
                        }
                    }
                    let peek = Number::U8(ctxt.tty_in.pop_front().map_or(0x00, |c| 0x80 | c));
                    match vars.entry(dest) {
                        Entry::Vacant(v) => {
                            v.insert(peek);
                        }
                        Entry::Occupied(mut existing) => {
                            assert_eq!(existing.get().num_type(), NumberType::U8);
                            existing.insert(peek);
                        },
                    }
                }
                IlInstruction::TtyOut { src } => {
                    let c: u8 = vars[src].as_u32().try_into().unwrap();
                    print!("{}", c as char);
                },
                IlInstruction::Resize { dest, dest_size, src, src_size } => {
                    let src = vars[src];
                    assert_eq!(&src.num_type(), src_size);
                    let n = match (dest_size, src) {
                        (NumberType::U16, Number::U8(n)) => Number::U16(n as u16),
                        (NumberType::U32, Number::U8(n)) => Number::U32(n as u32),
                        (NumberType::U8, Number::U32(n)) => Number::U8((n & 0xFF) as u8),
                        _ => panic!(),
                    };

                    vars.insert(dest, n);
                },
                IlInstruction::GetConstAddress { dest, const_name } => {
                    let addr = Number::U32(const_addrs[const_name]);

                    match vars.entry(dest) {
                        Entry::Vacant(v) => {
                            v.insert(addr);
                        }
                        Entry::Occupied(mut existing) => {
                            assert_eq!(existing.get().num_type(), addr.num_type());
                            existing.insert(addr);
                        },
                    }
                }
            }

            if inc_pc {
                s_index += 1;
            }

            // println!("{:?}", &vars);
        }
    }
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use crate::backend::emit_assembly;

    use super::*;

    #[test]
    fn halt() {
        test_var_inputs(
            "main",
            include_str!("../../programs/test/halt.j"),
            &[(vec![],1u8.into())]);
    }

    #[test]
    fn halt_base_address() {
        test_var_inputs(
            "main",
            include_str!("../../programs/test/halt_base_address.j"),
            &[(vec![],1u8.into())]);
    }

    #[test]
    fn halt_base_ram() {
        let (_ctxt, _il, ram_image) = assemble(
            "main",
            include_str!("../../programs/test/halt_ram.j"));


        let mut loader = Vec::new();
        loader.push(Opcode::JmpImm as u8);
        loader.extend(&ram_image.start_addr.to_le_bytes()[0..3]);
        for _ in 0..10 {
            loader.push(Opcode::Halt as u8);
        }

        let mut c = Computer::from_raw(loader);
        c.mem_slice_mut(ram_image.start_addr, ram_image.bytes.len() as u32)
            .copy_from_slice(&ram_image.bytes);

        while c.step() {}
        assert_eq!(1, c.reg_u8(0));
    }

    #[test]
    fn add_u8() {
        test_var_inputs(
            "main",
            include_str!("../../programs/test/add_u8.j"),
            &[(vec![],7u8.into())]);
    }

    #[test]
    fn mul8() {
        test_var_inputs(
            "mul8",
            include_str!("../../programs/lib/mul.j"),
            &[
                (vec![0u8.into(), 0u8.into()],0u8.into()),
                (vec![128u8.into(), 0u8.into()],0u8.into()),
                (vec![0u8.into(), 128u8.into()],0u8.into()),
                (vec![8u8.into(), 8u8.into()],64u8.into()),
                (vec![16u8.into(), 16u8.into()],0u8.into()),
                (vec![51u8.into(), 5u8.into()],255u8.into()),
            ]);
    }

    #[test]
    fn mul8_16() {
        test_var_inputs(
            "mul8_16",
            include_str!("../../programs/lib/mul.j"),
            &[
                (vec![16u8.into(), 16u8.into()],Number::U16(256).into()),
                (vec![0u8.into(), 0u8.into()],0u16.into()),
                (vec![128u8.into(), 0u8.into()],0u16.into()),
                (vec![0u8.into(), 128u8.into()],0u16.into()),
                (vec![8u8.into(), 8u8.into()],64u16.into()),
                (vec![51u8.into(), 5u8.into()],255u16.into()),
            ]);
    }

    #[test]
    fn mul16_32() {
        test_var_inputs(
            "mul16_32",
            include_str!("../../programs/lib/mul.j"),
            &[
                (vec![0u32.into(), 0u32.into()], 0u32.into()),
                (vec![16u32.into(), 16u32.into()], 256u32.into()),
                (vec![100u32.into(), 100u32.into()], 10000u32.into()),
                (vec![1000u32.into(), 1000u32.into()], 1_000_000u32.into()),
                (vec![0xABCDu32.into(), 0x1234u32.into()], 0xC37_4FA4u32.into()),
            ]);
    }

    #[test]
    fn mul32_32() {
        test_var_inputs(
            "mul32_32",
            include_str!("../../programs/lib/mul.j"),
            &[
                (vec![0u32.into(), 0u32.into()], 0u32.into()),
                (vec![16u32.into(), 16u32.into()], 256u32.into()),
                (vec![100u32.into(), 100u32.into()], 10000u32.into()),
                (vec![1000u32.into(), 1000u32.into()], 1_000_000u32.into()),
                (vec![0xABCDu32.into(), 0x1234u32.into()], 0xC37_4FA4u32.into()),
                (vec![0xDEADBEEFu32.into(), 0x1u32.into()], 0xDEADBEEFu32.into()),
                (vec![10_000u32.into(), 100_000u32.into()], 1_000_000_000u32.into()),
            ]);
    }

    #[test]
    fn mul32_64() {
        test_var_inputs(
            "test_mul32_64",
            include_str!("../../programs/lib/mul.j"),
            &[
                (vec![0u32.into(), 0u32.into()], 0u64.into()),
                (vec![16u32.into(), 16u32.into()], 256u64.into()),
                (vec![100u32.into(), 100u32.into()], 10000u64.into()),
                (vec![1000u32.into(), 1000u32.into()], 1_000_000u64.into()),
                (vec![0xABCDu32.into(), 0x1234u32.into()], 0xC37_4FA4u64.into()),
                (vec![0xDEADBEEFu32.into(), 0x1u32.into()], 0xDEADBEEFu64.into()),
                (vec![10_000u32.into(), 100_000u32.into()], 1_000_000_000u64.into()),
                (vec![0xDEADBEEFu32.into(), 0xDEADBEEFu32.into()], 0xC1B1CD12216DA321u64.into()),
                (vec![0xDEADBEEFu32.into(), 0xBEEFDEADu32.into()], 0xA615999BCA1E4983u64.into()),
                (vec![0xBEEFDEADu32.into(), 0xDEADBEEFu32.into()], 0xA615999BCA1E4983u64.into()),
            ]);
    }

    #[test]
    fn add_u64() {
        test_var_inputs(
            "test_add_U64",
            include_str!("../../programs/lib/U64.j"),
            &[
                (vec![0u64.into(), 0u64.into()], 0u64.into()),
                (vec![0xFFFF_FFFFu64.into(), 0x1u64.into()], 0x1_0000_0000u64.into()),
                (vec![0x1u64.into(), 0xFFFF_FFFFu64.into()], 0x1_0000_0000u64.into()),
            ]);
    }

    #[test]
    fn shiftright1() {
        test_var_inputs(
            "shiftright1",
            include_str!("../../programs/lib/div.j"),
            &[
                (vec![1u32.into()],0u32.into()),
                (vec![0x100u32.into()],0x80u32.into()),
                (vec![0x1_0000u32.into()],0x8000u32.into()),
                (vec![0x100_0000u32.into()],0x80_0000u32.into()),
                (vec![0x2222_2222u32.into()],0x1111_1111u32.into()),
                (vec![0x1111_1111u32.into()],0x0888_8888u32.into()),
            ]);
    }

    #[test]
    fn shiftright3() {

        let cases: Vec<_> = [1u32,0x100u32,0x1_0000u32,0x100_0000u32,0x2222_2222u32].iter()
            .map(|i| (vec![i.into()], (i >>3).into()))
            .collect();

        test_var_inputs(
            "shiftright3",
            include_str!("../../programs/lib/div.j"),
            &cases);
    }

    #[test]
    fn div16() {
        test_var_inputs(
            "div16",
            include_str!("../../programs/lib/div.j"),
            &[
                (vec![0xFFFFu32.into(), 0xFFu32.into()], (0xFFFFu32/0xFF).into()),
                (vec![0xFFFFu32.into(), 0x1u32.into()], 0xFFFFu32.into()),
                (vec![0xFFFFu32.into(), 0xFFFFu32.into()], 0x1u32.into()),
                (vec![0x100u32.into(), 0x100u32.into()], 1u32.into()),
                (vec![1u32.into(), 1u32.into()], 1u32.into()),
            ]);
    }

    #[test]
    fn div32() {
        test_var_inputs(
            "div32",
            include_str!("../../programs/lib/div.j"),
            &[
                (vec![13u32.into(), 4u32.into()], 3u32.into()),
                (vec![0xFFFFu32.into(), 0xFFu32.into()], (0xFFFFu32/0xFF).into()),
                (vec![0xFFFFu32.into(), 0x1u32.into()], 0xFFFFu32.into()),
                (vec![0xFFFFu32.into(), 0xFFFFu32.into()], 0x1u32.into()),
                (vec![0x100u32.into(), 0x100u32.into()], 1u32.into()),
                (vec![1u32.into(), 1u32.into()], 1u32.into()),
                (vec![0xDEADBEEFu32.into(), 0xDEADu32.into()], 0x1_0000u32.into()),
            ]);
    }

    #[test]
    fn div32_by10() {
        let cases: Vec<_> = [13u32,0xFFFFu32,0x100u32,1u32,0xDEADBEEFu32].iter()
            .map(|i| (vec![i.into()], (i/10).into()))
            .collect();
        test_var_inputs(
            "div32_by10",
            include_str!("../../programs/lib/div.j"),
            cases.as_slice());
    }

    #[test]
    fn call_parameterless() {
        test_var_inputs(
            "main",
            include_str!("../../programs/test/call_parameterless.j"),
            &[(vec![],7u8.into())]);
    }

    #[test]
    fn idfn() {
        test_var_inputs(
            "main",
            include_str!("../../programs/test/idfn.j"),
            &[(vec![],TestVar::Num(Number::U8(7u8)))]);
    }

    #[test]
    fn if_eq() {
        test_inputs(
            "if_eq",
            include_str!("../../programs/test/if.j"),
            &[(6u8,7u8,0u8), (8,7,0), (0,7,0), (0xFF,7,0), (7,7,1)]);
    }

    #[test]
    fn if_gt_unsigned() {
        test_inputs(
            "if_gt",
            include_str!("../../programs/test/if.j"),
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
            "if_gte",
            include_str!("../../programs/test/if.j"),
            &[(6u8,7u8,0u8), (8,7,1), (0,7,0), (0x7F,7,1), (0xFF, 7, 1), (7,7,1)]);
    }

    #[test]
    fn if_lt_unsigned() {
        test_inputs(
            "if_lt",
            include_str!("../../programs/test/if.j"),
            &[
            (0u8,0u8,0u8),
            (0,1,1),
            (6,7,1), (8,7,0), (0,7,1), (7,0,0), (0x7F,7,0), (0xFF, 7, 0), (7,7,0)]);
    }

    #[test]
    fn if_lte_unsigned() {
        test_inputs(
            "if_lte",
            include_str!("../../programs/test/if.j"),
            &[(6u8,7u8,1u8), (8,7,0), (0,7,1), (0x7F,7,0), (0xFF, 7, 0), (7,7,1)]);
    }

    #[test]
    fn if_ne() {
        test_inputs(
            "if_ne",
            include_str!("../../programs/test/if.j"),
            &[(6u8,7u8,1u8),(8,7,1),(0,7,1),(0xFF,7,1), (7,7,0)]);
    }

    #[test]
    fn if_ne_uptr() {
        test_inputs(
            "if_ne_uptr",
            include_str!("../../programs/test/if.j"),
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
            include_str!("../../programs/test/plusone.j"),
            &[(vec![],7u8.into())]);

    }

    #[test]
    fn fac_rec() {
        test_var_inputs(
            "main",
            include_str!("../../programs/test/fac_rec.j"),
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
            include_str!("../../programs/test/fac_iter.j"),
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
            include_str!("../../programs/test/fib.j"),
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
            include_str!("../../programs/test/fib_memo.j"),
            &[
                (vec![0u8.into()],0u8.into()),
                (vec![1u8.into()],1u8.into()),
                (vec![2u8.into()],1u8.into()),
                (vec![3u8.into()],2u8.into()),
                (vec![13u8.into()],233u8.into()),
                ]);
    }


    #[test]
    fn statics_local() {
        test_var_inputs(
            "test1",
            include_str!("../../programs/test/static.j"),
            &[
                (vec![1u8.into()],2u8.into()),
                (vec![2u8.into()],4u8.into()),
                ]);
    }

    #[test]
    fn statics_global() {
        test_var_inputs(
            "equal",
            include_str!("../../programs/test/static.j"),
            &[
                (vec![1u8.into(),1u8.into()],1u8.into()),
                (vec![2u8.into(),4u8.into()],0u8.into()),
                ]);
    }

    #[test]
    fn statics_global_inlined() {
        test_var_inputs(
            "inlined",
            include_str!("../../programs/test/static.j"),
            &[
                (vec![1u8.into(),1u8.into()],1u8.into()),
                (vec![2u8.into(),4u8.into()],0u8.into()),
                ]);
    }

    #[test]
    fn statics_global_shared_across_functions() {
        test_var_inputs(
            "test3",
            include_str!("../../programs/test/static.j"),
            &[
                (vec![17u8.into()],0u8.into()),
                (vec![18u8.into()],0u8.into()),
                ]);
    }

    #[test]
    fn add_uptr() {
        test_inputs(
            "main",
            include_str!("../../programs/test/add_uptr.j"),
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
            include_str!("../../programs/test/cmp_usize.j"),
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
            include_str!("../../programs/test/ptr.j"),
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
            include_str!("../../programs/test/struct.j"),
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
    fn struct_array() {
        test_inputs(
            "test_add2",
            include_str!("../../programs/test/struct.j"),
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
            include_str!("../../programs/test/struct_ret.j"),
            &[
                (0xAABBCCDDu32, 0x11111111u32, 0xBBCCDDEEu32)
                ]);
    }

    #[test]
    fn heap_nofree() {
        test_var_inputs(
            "get_heap",
            include_str!("../../programs/test/heap_nofree.j"),
            &[
                (vec![], STATICS_START_ADDRESS.into())
            ]
        );
    }


    #[test]
    fn heap_nofree_alloc() {
        test_var_inputs(
            "test1",
            include_str!("../../programs/test/heap_nofree.j"),
            &[
                (vec![], 0u32.into())
            ]
        );
    }

    #[test]
    fn heap_init() {
        let (ctxt, il, rom) = assemble("heap_init", include_str!("../../programs/lib/heap.j"));
        let (_, heap_start) = run_var_input(&ctxt, &il, &rom, &vec![], NumberType::U32);
        assert_eq!(heap_start, Number::U32(STATICS_START_ADDRESS));

        let (ctxt, il, rom) = assemble("test_get_heap_head", include_str!("../../programs/lib/heap.j"));
        let (c, heap_entry) = run_var_input(&ctxt, &il, &rom, &vec![], NumberType::U32);
        assert_eq!(heap_entry, Number::U32(STATICS_START_ADDRESS+4));

        let heap_type = ctxt.struct_types.get("heap").unwrap();
        let (head_offset, _) = heap_type.get_field(&ctxt, "head");
        let heap_entry_type = ctxt.struct_types.get("heap_entry").unwrap();

        let header_size = heap_entry_type.byte_count(&ctxt);
        assert_eq!(header_size, 12);


        let heap_start = heap_start.as_u32();
        let head_entry_addr = heap_start + 4;
        assert_eq!(c.comp.mem_word(heap_start + head_offset), head_entry_addr);
        assert_eq!(c.comp.mem_word(head_entry_addr), 0);
        let len = 1024-header_size;
        assert_eq!(c.comp.mem_word(head_entry_addr+4), len);
        assert_eq!(c.comp.mem_byte(head_entry_addr+8), 1);

        let max_static = il.statics_addresses.end;
        assert_eq!(max_static, head_entry_addr + header_size + len);
    }

    #[test]
    fn heap_is_entry_bad() {
        let (ctxt, il, rom) = assemble("test_heap_is_entry_bad", include_str!("../../programs/lib/heap.j"));
        let (_, is_bad) = run_var_input(&ctxt, &il, &rom, &vec![1u32.into()], NumberType::U8);
        assert_eq!(is_bad.as_u32(), 0);

        let (_, is_bad) = run_var_input(&ctxt, &il, &rom, &vec![1024u32.into()], NumberType::U8);
        assert_eq!(is_bad.as_u32(), 1);
    }

    #[test]
    fn heap_alloc() {
        let alloc_size = 4u32;
        let (ctxt, il, rom) = assemble("test_heap_alloc", include_str!("../../programs/lib/heap.j"));
        let (c, allocated_addr) = run_var_input(&ctxt, &il, &rom, &vec![alloc_size.into()], NumberType::U32);
        let allocated_addr = allocated_addr.as_u32();

        const HEADER_SIZE : u32 = 0xc;

        let heap_addr = STATICS_START_ADDRESS;
        let max_static = il.statics_addresses.end;
        let head_entry_addr = heap_addr + 4;
        assert_eq!(c.comp.mem_word(heap_addr), head_entry_addr);
        let new_entry_addr = c.comp.mem_word(head_entry_addr);
        assert_eq!(new_entry_addr, head_entry_addr+1024-HEADER_SIZE-alloc_size);
        let head_entry_len = 1024-HEADER_SIZE-HEADER_SIZE-alloc_size;
        assert_eq!(c.comp.mem_word(head_entry_addr+4), head_entry_len);
        assert_eq!(c.comp.mem_byte(head_entry_addr+8), 1);

        assert_eq!(new_entry_addr+HEADER_SIZE, allocated_addr);

        assert_eq!(c.comp.mem_word(new_entry_addr), 0);
        assert_eq!(c.comp.mem_word(new_entry_addr+4), alloc_size);
        assert_eq!(c.comp.mem_byte(new_entry_addr+8), 0);
        assert_eq!(max_static-alloc_size, allocated_addr);
        assert_eq!(new_entry_addr+HEADER_SIZE, allocated_addr);
    }

    #[test]
    fn print_hex() {
        test_tty(
            "printHexTest",
            include_str!("../../programs/lib/print_hex.j"),
            &[
                ("",0x0,0x0,"00\n"),
                ("",0x1,0x0,"01\n"),
                ("",0x9,0x0,"09\n"),
                ("",0xA,0x0,"0A\n"),
                ("",0xF,0x0,"0F\n"),
                ("",0x10,0x0,"10\n"),
                ("",0x1F,0x0,"1F\n"),
                ("",0x20,0x0,"20\n"),
                ("",0x90,0x0,"90\n"),
                ("",0xA0,0x0,"A0\n"),
                ("",0xAA,0x0,"AA\n"),
                ("",0xFF,0x0,"FF\n"),
                ]);
    }

    #[test]
    fn echo() {
        test_tty(
            "main",
            include_str!("../../programs/test/echo.j"),
            &[
                ("0\nq",0x0,0x0,"Hi!\n>:0\n>:q"),
                ("01\nq",0x0,0x0,"Hi!\n>:01\n>:q"),
                ]);
    }

    #[test]
    fn echoline() {
        test_tty(
            "test_echoline",
            include_str!("../../programs/lib/stdio.j"),
            &[
                ("0\n",0x0,0x0,"0"),
                ("01\n",0x0,0x0,"01"),
                ("hello\n",0x0,0x0,"hello"),
                ]);
    }

    #[test]
    fn print_dec8() {
        test_tty(
            "print_dec8_test",
            include_str!("../../programs/app/rpn.j"),
            &[
                ("",0,0,"0"),
                ("",9,0,"9"),
                ("",10,0,"10"),
                ("",99,0,"99"),
                ("",100,0,"100"),
                ("",255,0,"255"),
                ]);
    }

    #[test]
    fn print_dec32() {
        test_tty(
            "print_dec32_test",
            include_str!("../../programs/app/rpn.j"),
            &[
                ("",0,0,"0"),
                ("",9,0,"9"),
                ("",10,0,"10"),
                ("",99,0,"99"),
                ("",100,0,"100"),
                ("",255,0,"255"),
                ]);
    }

    #[test]
    fn rpn() {
        test_tty(
            "main",
            include_str!("../../programs/app/rpn.j"),
            &[
                ("0\nq",0x0,0x0,"RPN\n"),
                ("1 2 3 d\nq",0x0,0x0,"RPN\n0:1\n1:2\n2:3\n"),
                ("0\n0+q",0x0,0x0,"RPN\n0\n"),
                ("1\n2+q",0x0,0x0,"RPN\n3\n"),
                ("5\n2-q",0x0,0x0,"RPN\n3\n"),
                ("3\n4*q",0x0,0x0,"RPN\n12\n"),
                ("13\n4/q",0x0,0x0,"RPN\n3\n"),
                ("3\n4+5*q",0x0,0x0,"RPN\n7\n35\n"),
                ("6 7 * q",0x0,0x0,"RPN\n42\n"),
                ("1000 1000* 50000/q",0x0,0x0,"RPN\n1000000\n20\n"),
            ]);
    }

    #[test]
    fn local_array() {
        let (_ctxt, il) = emit_il(
            &PathBuf::from(file!()),
            "main",
            include_str!("../../programs/test/local_array.j"),
            &test_programs_dir());

        assert_eq!(il.simulate(&[0u8.into(), 0u8.into()]), 0u8.into());
        assert_eq!(il.simulate(&[0u8.into(), 1u8.into()]), 1u8.into());
        assert_eq!(il.simulate(&[1u8.into(), 0u8.into()]), 1u8.into());
        assert_eq!(il.simulate(&[1u8.into(), 0xFFu8.into()]), 0u8.into());
    }

    #[test]
    fn array_to_ptr() {
        // unsafe { backtrace_on_stack_overflow::enable() };
        test_inputs(
            "main",
            include_str!("../../programs/test/array_to_ptr.j"),
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
            include_str!("../../programs/test/array_loop.j"),
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
            include_str!("../../programs/lib/strlen.j"),
            &[
                (vec![TestVar::Ascii(b"\0")], TestVar::Num(Number::U32(0))),
                (vec![TestVar::Ascii(b"hello\0")], TestVar::Num(Number::U32(5))),
                (vec![TestVar::Ptr(long)], TestVar::Num(Number::U32(expected))),
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
            include_str!("../../programs/lib/strncmp.j"),
            &[
                (vec![TestVar::Ascii(b"\0"), TestVar::Ascii(b"\0"), TestVar::Num(Number::U32(0))], TestVar::Num(Number::U8(0))),
                (vec![TestVar::Ascii(b"\0"), TestVar::Ascii(b"\0"), TestVar::Num(Number::U32(1))], TestVar::Num(Number::U8(0))),
                (vec![TestVar::Ascii(b"\0"), TestVar::Ascii(b"\0"), TestVar::Num(Number::U32(2))], TestVar::Num(Number::U8(0))),
                (vec![TestVar::Ascii(b"a\0"), TestVar::Ascii(b"a\0"), TestVar::Num(Number::U32(1))], TestVar::Num(Number::U8(0))),
                (vec![TestVar::Ascii(b"aa\0"), TestVar::Ascii(b"ab\0"), TestVar::Num(Number::U32(1))], TestVar::Num(Number::U8(0))),
                (vec![TestVar::Ascii(b"aa\0"), TestVar::Ascii(b"ab\0"), TestVar::Num(Number::U32(2))], TestVar::Num(Number::U8(255))),
                (vec![TestVar::Ascii(b"ab\0"), TestVar::Ascii(b"aa\0"), TestVar::Num(Number::U32(2))], TestVar::Num(Number::U8(1))),
                (vec![TestVar::Ascii(b"ab\0"), TestVar::Ascii(b"aa\0"), TestVar::Num(Number::U32(1000))], TestVar::Num(Number::U8(1))),
                (vec![TestVar::Ptr(long1.clone()), TestVar::Ptr(long2.clone()), TestVar::Num(Number::U32(same_len))], TestVar::Num(Number::U8(0))),
                (vec![TestVar::Ptr(long1), TestVar::Ptr(long2), TestVar::Num(Number::U32(same_len+1))], TestVar::Num(Number::U8(255))),
            ]
        );
    }

    #[test]
    fn strstr() {
        test_var_inputs(
            "strstr",
            include_str!("../../programs/lib/strstr.j"),
            &[
                (vec![TestVar::Ascii(b"\0"), TestVar::Ascii(b"\0")], TestVar::Num(Number::U32(0))),
                (vec![TestVar::Ascii(b"\0"), TestVar::Ascii(b"a\0")], TestVar::Num(Number::U32(0))),
                (vec![TestVar::Ascii(b"a\0"), TestVar::Ascii(b"\0")], TestComputer::arg_base_addr_var(0)),
                (vec![TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"h\0")], TestComputer::arg_base_addr_var(0)),
                (vec![TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"he\0")], TestComputer::arg_base_addr_var(0)),
                (vec![TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"Z\0")], TestVar::Num(Number::U32(0))),
                (vec![TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"hi\0")], TestVar::Num(Number::U32(0))),
                (vec![TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"el\0")], TestComputer::arg_base_addr_var(1)),
                (vec![TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"ll\0")], TestComputer::arg_base_addr_var(2)),
                (vec![TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"lo\0")], TestComputer::arg_base_addr_var(3)),
                (vec![TestVar::Ascii(b"hello\0"), TestVar::Ascii(b"l\0")], TestComputer::arg_base_addr_var(2)),
            ]);
    }

    #[test]
    fn parse_hex_nibble() {
        let mut cases = Vec::new();
        for (i, c) in ('0'..='9').enumerate() {
            cases.push((vec![TestVar::Num(Number::U8(c as u8))], TestVar::Num(Number::U8(i.try_into().unwrap()))));
        }
        for (i, c) in ('a'..='f').enumerate() {
            let i: u8 = i.try_into().unwrap();
            cases.push((vec![TestVar::Num(Number::U8(c as u8))], TestVar::Num(Number::U8(10+i))));
            cases.push((vec![TestVar::Num(Number::U8(char::to_ascii_uppercase(&c) as u8))], TestVar::Num(Number::U8(10+i))));
        }

        test_var_inputs(
            "parseHexNibble",
            include_str!("../../programs/lib/print_hex.j"),
            &[
                (vec![TestVar::Num(Number::U8('9' as u8))], TestVar::Num(Number::U8(0x9))),
                (vec![TestVar::Num(Number::U8('a' as u8))], TestVar::Num(Number::U8(0xA))),
            ]
        );
    }

    #[test]
    fn parse_hex() {
        let mut cases = Vec::new();
        for i in 0..=0xFFu8 {
            cases.push((vec![TestVar::String(format!("{:02x}", i))], TestVar::Num(Number::U8(i))));
            cases.push((vec![TestVar::String(format!("{:02X}", i))], TestVar::Num(Number::U8(i))));
        }
        test_var_inputs(
            "parseHex",
            include_str!("../../programs/lib/print_hex.j"),
            &cases
        );
    }

    #[test]
    fn delay() {
        let find_tick_count = |delay_count| {
            let cases = vec![
                (vec![TestVar::Num(Number::U32(delay_count))], TestVar::Num(Number::U8(0))),
            ];
            test_var_inputs(
                "test_delay",
                include_str!("../../programs/lib/delay.j"),
                &cases
            )
        };

        let baseline = find_tick_count(0);
        let single = find_tick_count(1);
        let ten = find_tick_count(10);
        dbg!(baseline, single, ten);

        // ticks = m * delay_count + b;
        let b = baseline;
        let m = (ten - baseline) / 10;
        dbg!(m, b);

        let frequency = 500_000;
        let ticks_per_ms = frequency / 1000;
        let ms_per_delay_count = m / ticks_per_ms;
        dbg!(ms_per_delay_count);
    }

    #[test]
    fn bootram_sanity() {
        let a = RAM_MIN + 1024;
        let (_loader_ctxt, _loader_il, loader_image) = assemble(
            "main",
            include_str!("../../programs/app/bootram.j"));

        let mut c = Computer::from_image(Cow::Owned(loader_image), false);

        for ch in (format!("s{:08x}\n", a)).chars() {
            c.tty.tty_in.push_back(ch as u8);
        }

        c.tty.tty_in.push_back('a' as u8);
        c.tty.tty_in.push_back('\n' as u8);

        for b in &[0xEF,0xBE, 0xAD,0xDE] {
            for ch in (format!("w{:02x}\nn\n", b)).chars() {
                c.tty.tty_in.push_back(ch as u8);
            }
        }

        c.tty.tty_in.push_back('q' as u8);
        c.tty.tty_in.push_back('\n' as u8);

        println!("{}", std::str::from_utf8(c.tty.tty_in.as_slices().0).unwrap());
        println!("{}", std::str::from_utf8(c.tty.tty_in.as_slices().1).unwrap());

        let mut last_pc = u32::from_le_bytes(c.pc);
        let mut running: bool = true;
        while running {
            running = c.step();
            let pc = u32::from_le_bytes(c.pc);
            if pc != last_pc {
                //print_state(&c);
                last_pc = pc;
            }
        }

        let mut out = String::new();
        for c in &c.tty.tty_out {
            out.push(*c as char);
        }

        assert_eq!(out.as_str(), &format!("READY\n{:08x}\n", RAM_MIN + 1024));

        assert_eq!(0xDEADBEEF, c.mem_word(RAM_MIN + 1024));
    }

    #[test]
    fn bootram() {
        let (_ctxt, _ram_il, ram_image) = assemble(
            "main",
            include_str!("../../programs/test/hello_ram.j"));

        let (_loader_ctxt, _loader_il, loader_image) = assemble(
            "main",
            include_str!("../../programs/app/bootram.j"));

        let mut c = Computer::from_image(Cow::Owned(loader_image), false);

        for ch in (format!("s{:08x}\n", ram_image.start_addr)).chars() {
            c.tty.tty_in.push_back(ch as u8);
        }

        for b in &ram_image.bytes {
            for ch in (format!("w{:02x}\nn\n", b)).chars() {
                c.tty.tty_in.push_back(ch as u8);
            }
        }

        // overwrite the return address
        for ch in (format!("s{:08x}\n", INITIAL_STACK-4)).chars() {
            c.tty.tty_in.push_back(ch as u8);
        }

        for b in ram_image.start_addr.to_le_bytes() {
            for ch in (format!("w{:02x}\nn\n", b)).chars() {
                c.tty.tty_in.push_back(ch as u8);
            }
        }

        c.tty.tty_in.push_back('q' as u8);
        c.tty.tty_in.push_back('\n' as u8);

        println!("{}", std::str::from_utf8(c.tty.tty_in.as_slices().0).unwrap());
        println!("{}", std::str::from_utf8(c.tty.tty_in.as_slices().1).unwrap());

        let mut last_ir0 = None;
        let mut running: bool = true;
        while running {
            running = c.step();

            if last_ir0 != Some(c.ir0) {
                //print_state(&c);
            }

            last_ir0 = Some(c.ir0);
        }

        let mut out = String::new();
        for c in &c.tty.tty_out {
            out.push(*c as char);
        }

        assert_eq!(out.as_str(), "READY\nHi_from_RAM!\n");

        let r0 = c.reg_u32(0);
        assert_eq!(r0, 0xAABBCCDD);
    }

    #[test]
    fn else_if() {
        test_var_inputs(
            "else_if",
            include_str!("../../programs/test/else_if.j"),
            &[
                (vec![0u8.into()], 0u8.into()),
                (vec![1u8.into()], 1u8.into()),
                (vec![2u8.into()], 2u8.into()),
                (vec![3u8.into()], 2u8.into()),
            ]
        );
    }

    pub fn test_programs_dir() -> PathBuf {
        let mut dir = std::env::current_exe().unwrap();
        dir.pop();
        dir.pop();
        dir.pop();
        dir.pop();
        dir.push("programs");
        dir.push("test");
        dir
    }
    
    #[test]
    fn echo_ps2_isr() {
        test_ps2(
            "main",
            include_str!("../../programs/test/keyboard_isr.j"),
            &[
                ("abcq\n",0x0,0x0,"abc"),
                ("aBcq\n",0x0,0x0,"aBc"),
                ("a bq\n",0x0,0x0,"a b"),
            ]);
    }

    #[test]
    fn echo_tty_isr() {
        test_tty(
            "main",
            include_str!("../../programs/test/keyboard_isr.j"),
            &[
                ("abcq\n",0x0,0x0,"abc"),
                ("aBcq\n",0x0,0x0,"aBc"),
                ("a bq\n",0x0,0x0,"a b"),
            ]);
    }

    pub struct TestComputer<'a> {
        pub comp: Computer<'a>,
    }

    impl<'a> TestComputer<'a> {
        pub fn arg_base_addr() -> u32 {
            STATICS_START_ADDRESS+1000
        }

        pub fn arg_base_addr_var(offset: u32) -> TestVar {
            TestVar::Num(Number::U32(TestComputer::arg_base_addr() + offset))
        }

        pub fn setup_args(ins: &Vec<TestVar>) -> (Vec<Number>, BTreeMap<u32,u8>) {
            let mut ptr = Self::arg_base_addr();
            let mut args = Vec::new();
            let mut mem = BTreeMap::new();
            for input in ins {
                if let Some(bytes) = match input {
                    TestVar::Ascii(s) => Some(*s),
                    TestVar::String(s) => Some(s.as_bytes()),
                    TestVar::Ptr(s) => Some(s.as_slice()),
                    TestVar::Num(n) => { args.push(*n); None}
                } {
                    let arg_base = ptr;
                    for b in bytes {
                        mem.insert(ptr, *b);
                        ptr += 1;
                    }
                    args.push(Number::U32(arg_base));

                    ptr = (ptr+3)/4*4; // round up
                    ptr += 4; // add buffer
                }
            }
            (args, mem)
        }

        pub fn from_rom(rom: &'a Image) -> TestComputer<'a> {
            TestComputer{
                comp: Computer::from_image(Cow::Borrowed(&rom), false),
            }
        }

        pub fn run(&mut self, inputs: &[u32], output_size: u32) -> u32{
            dbg!(inputs);
            assert!(inputs.len() <= 3);
            for (i,val) in inputs.iter().enumerate() {
                let i = i as u8;
                self.comp.reg_u32_set(4*i, *val);
            }

            let mut last_pc = None;
            let mut step_count = 0u64;
            while self.comp.step() {
                if last_pc != self.comp.ir0_pc {
                    //print_state(&mut self.comp);
                }
                last_pc = self.comp.ir0_pc;
                step_count += 1;
                assert!(step_count < 1_0000_0000);
            }
            match output_size {
                1 => self.comp.reg_u8(0).into(),
                2 => self.comp.reg_u16(0).into(),
                4 => self.comp.reg_u32(0),
                _ => panic!(),
            }
        }
    }


    #[derive(Debug, PartialEq, Clone)]
    pub enum TestVar {
        Ascii(&'static [u8]),
        String(String),
        Ptr(Vec<u8>),
        Num(Number),
    }

    impl TestVar {
        fn byte_count(&self) -> u32 {
            match self {
                TestVar::String(_) | TestVar::Ascii(_) | TestVar::Ptr(_) => 4,
                TestVar::Num(n) => n.num_type().byte_count(),
            }
        }
    }

    impl From<Number> for TestVar {
        fn from(i: Number) -> Self {
            TestVar::Num(i)
        }
    }

    impl From<u8> for TestVar {
        fn from(i: u8) -> Self {
            TestVar::Num(i.into())
        }
    }

    impl From<&u8> for TestVar {
        fn from(i: &u8) -> Self {
           (*i).into()
        }
    }

    impl From<u16> for TestVar {
        fn from(i: u16) -> Self {
            TestVar::Num(i.into())
        }
    }

    impl From<&u16> for TestVar {
        fn from(i: &u16) -> Self {
            (*i).into()
        }
    }

    impl From<u32> for TestVar {
        fn from(i: u32) -> Self {
            TestVar::Num(i.into())
        }
    }

    impl From<&u32> for TestVar {
        fn from(i: &u32) -> Self {
            (*i).into()
        }
    }

    impl From<[u8;4]> for TestVar {
        fn from(bytes: [u8;4]) -> Self {
            TestVar::Ptr(bytes.iter().cloned().collect())
        }
    }

    impl From<u64> for TestVar {
        fn from(n: u64) -> Self {
            let bytes = n.to_le_bytes();
            TestVar::Ptr(bytes.iter().cloned().collect())
        }
    }

    fn assemble(entry: &str, program: &str) -> (ProgramContext, IlProgram, Image) {
        let (ctxt, program) = emit_il(&PathBuf::from(file!()), entry, program, &test_programs_dir());
        let (_, assembly) = emit_assembly(&ctxt, &program);
        let rom = assemble::assemble(assembly);
        (ctxt, program, rom)
    }

    fn check_args_by_il_type(ctxt: &ProgramContext, ins: &Vec<TestVar>, out_type: NumberType) {
        check_args_inner(ctxt, (ins, out_type.byte_count()));
    }

    fn check_args(ctxt: &ProgramContext, test_case: &(Vec<TestVar>, TestVar)) {
        let (test_args, test_return) = test_case;
        check_args_inner(ctxt, (test_args, test_return.byte_count()));
    }

    fn check_args_inner(ctxt: &ProgramContext, test_case: (&Vec<TestVar>, u32)) {
        let entry = &ctxt.function_defs[&ctxt.entry];
        // dbg!(&entry);
        let entry_args = &entry.args;
        let (test_args, test_return_size) = test_case;
        assert_eq!(entry_args.len(), test_args.len());
        for (i, (entry_arg_name, entry_arg_type)) in entry_args.iter().enumerate() {
            let entry_arg_size = entry_arg_type.byte_count(ctxt);
            let test_arg_size = test_args[i].byte_count();
            assert_eq!(entry_arg_size, test_args[i].byte_count(),
                "Entry arg '{}' type '{:?}' expects size {} but test provided size {}.",
                entry_arg_name, entry_arg_type, entry_arg_size, test_arg_size);
        }

        let entry_return_size = entry.return_type.byte_count(ctxt);
        assert_eq!(entry_return_size, test_return_size,
            "Entry function returns type '{:?}' expects size {} but test provided size {}.",
            entry.return_type, entry_return_size, test_return_size);
    }

    fn run_var_input<'a>(
        ctxt: &'a ProgramContext,
        il: &'a IlProgram,
        rom: &'a Image,
        ins: &Vec<TestVar>,
        out_type: NumberType) -> (TestComputer<'a>, Number)
    {
        check_args_by_il_type(ctxt, ins, out_type);
        let (args, mut mem) = TestComputer::setup_args(ins);

        let mut c = TestComputer::from_rom(&rom);
        for (addr, b) in &mem {
            *c.comp.mem_byte_mut(*addr) = *b;
        }

        // run in IL simulator
        let mut sim = il.create_sim(&args);
        sim.mem.append(&mut mem);
        let (_,il_result) = sim.run();

        // run in HW simulator
        let hw_sim_args: Vec<_> = args.iter().map(|a| a.as_u32()).collect();
        let hw_result = c.run(hw_sim_args.as_slice(), il_result.num_type().byte_count());
        let hw_result = match il_result.num_type() {
            NumberType::U32 => Number::U32(hw_result),
            NumberType::U16 => Number::U16(hw_result.try_into().unwrap()),
            NumberType::U8 => Number::U8(hw_result.try_into().unwrap()),
        };

        assert_eq!(il_result, hw_result, "IL and HW have different results for {:?}", ins);
        (c, il_result)
    }

    fn test_var_inputs<'a>(entry: &'a str, program: &str, cases: &[(Vec<TestVar>, TestVar)]) -> u64 {
        let (ctxt, il) = emit_il(&PathBuf::from(file!()), entry, program, &test_programs_dir());

        let (_, asm) = emit_assembly(&ctxt, &il);
        let rom = assemble::assemble(asm);

        let mut last_tick_count = None;
        for case in cases
        {
            check_args(&ctxt, case);
            let (ins, expected) = case;

            let expected_type = match expected {
                TestVar::Num(n) => n.num_type(),
                _ => NumberType::U32,
            };

            let (tc, actual) = run_var_input(&ctxt, &il, &rom, ins, expected_type);

            match expected {
                TestVar::Ascii(expected) => {
                    let actual = tc.comp.mem_slice(actual.as_u32(), expected.len().try_into().unwrap());
                    let actual = std::str::from_utf8(actual).unwrap();
                    let expected = std::str::from_utf8(expected).unwrap();
                    assert_eq!(expected, actual, "{:?}", &ins);
                },
                TestVar::Ptr(expected) => {
                    let actual = tc.comp.mem_slice(actual.as_u32(), expected.len().try_into().unwrap());
                    assert_eq!(expected.as_slice(), actual, "{:?}", &ins);
                },
                TestVar::String(expected) => {
                    let actual = tc.comp.mem_slice(actual.as_u32(), expected.len().try_into().unwrap());
                    let actual = std::str::from_utf8(actual).unwrap();
                    assert_eq!(expected, actual, "{:?}", &ins);
                },
                TestVar::Num(expected) => {
                    assert_eq!(*expected, actual, "{:?}", &(ins, expected));
                },
            };

            last_tick_count = Some(tc.comp.tick_count);
        }

        last_tick_count.unwrap()
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

    fn test_ps2(entry: &str, program: &str, pairs: &[(&str,u32,u32,&str)]) {
        test_io(entry, program, pairs, |c, ttyin| {
            for ch in ttyin.chars() {
                for code in ASCII_TO_PS2_SCAN_CODES[ch as usize] {
                    if code == 0 {
                        break;
                    }

                    c.comp.ps2.queue.push_back(code);
                }
            }
        })
    }

    fn test_tty(entry: &str, program: &str, pairs: &[(&str,u32,u32,&str)]) {
        test_io(entry, program, pairs, |c, ttyin| {
            for ch in ttyin.chars() {
                c.comp.tty.tty_in.push_back(ch as u8);
            }
        })
    }
    
    fn test_io<T, F: FnMut(&mut TestComputer, &T)->()>(entry: &str, program: &str, pairs: &[(T,u32,u32,&str)], mut apply: F)
        where T: std::fmt::Debug
    {
        eprintln!("Compiling to IL...");
        let (ctxt, il) = emit_il(&PathBuf::from(file!()), entry,program, &test_programs_dir());
        // dbg!(&il);

        eprintln!("Compiling to assembly...");
        let (_, asm) = emit_assembly(&ctxt, &il);

        eprintln!("Assembling...");
        let rom = assemble::assemble(asm);

        eprintln!("Running...");
        for (arg, input1, input2, expected) in pairs {
            let mut c = TestComputer::from_rom(&rom);
            dbg!((input1, input2, expected));
            apply(&mut c, arg);

            assert_eq!(0, c.run(&[*input1, *input2], 1));

            let mut out = String::new();
            for c in &c.comp.tty.tty_out {
                let c = *c as char;
                out.push(c);
            }

            assert_eq!(out.as_str(), *expected,
                "Failed for `{:?}`,{:?},{:?} -> `{}`", arg, input1, input2, expected);
        }
    }
}
