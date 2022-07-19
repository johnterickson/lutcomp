use super::*;

pub fn optimize_assembly(assembly: &mut Vec<AssemblyInputLine>) -> usize {

    let mut optimizations_applied = 0;

    let mut instruction_indices = Vec::new();
    for (i, line) in assembly.as_mut_slice().iter_mut().enumerate() {
        match line {
            AssemblyInputLine::Comment(..) => {},
            _ => {
                instruction_indices.push(i);
            }
        }
    }

    let mut label_refs = BTreeMap::new();
    for (i, line) in assembly.as_mut_slice().iter_mut().enumerate() {
        match line {
            AssemblyInputLine::Instruction(inst) => {
                match inst.opcode {
                    Opcode::JcImm | Opcode::JmpImm | Opcode::JmpMem | Opcode::JmpReg | Opcode::JnImm | Opcode::JzImm => {
                        match &inst.args[0] {
                            Value::Label24(l) | Value::Label32(l) => {
                                label_refs.entry(l.clone())
                                    .or_insert_with(|| BTreeSet::new())
                                    .insert(i);
                            }
                            _ => todo!(),
                        }
                    }
                    _ => {}
                }
            }
            _ => {}
        }
    }

    for line in assembly.iter_mut() {
        if let AssemblyInputLine::Instruction(i) = line {
            if (i.opcode == Opcode::Copy8 || i.opcode == Opcode::Copy32) && i.args[0] == i.args[1] {
                *line = AssemblyInputLine::Comment(format!("optimized away noop copy {:?}", i));
                optimizations_applied += 1;
            }
        }
    }

    for instruction_pairs in instruction_indices.windows(2) {
        let index1 = instruction_pairs[0];
        let index2 = instruction_pairs[1];
        let (slice1, slice2) = assembly.as_mut_slice()[index1..].split_at_mut(1);
        let line1 = &mut slice1[0];
        let line2 = &mut slice2[index2 - index1 - 1];

        let mut new_line1 = None;
        let mut new_line2 = None;

        match (&line1, &line2) {
            (AssemblyInputLine::Instruction(inst), AssemblyInputLine::Label(label)) => {
                if inst.opcode == Opcode::JmpImm {
                    if let Value::Label24(jmp_label) = &inst.args[0] {
                        if jmp_label == label {
                            if label_refs[label].len() == 1 {
                                assert_eq!(&index1, label_refs[label].iter().next().unwrap());
                                new_line1 = Some(AssemblyInputLine::Comment(
                                    format!("Optimized away unconditional jump to the next instruction: {:?}", inst)
                                ));
                            }
                        }
                    }
                }
            }
            (AssemblyInputLine::Instruction(i1), AssemblyInputLine::Instruction(i2)) => {
                 if (i1.opcode == Opcode::Push8) && (i2.opcode == Opcode::Pop8) || (i2.opcode == Opcode::Pop8 && i1.opcode == Opcode::Push8) {
                     if let (Value::Register(r1), Value::Register(r2)) = (&i1.args[0], &i2.args[0]) {
                        if r1 == r2 {
                            let c = format!("optimized away push/pop pair: {:?} {:?}", line1, line2);
                            new_line1 = Some(AssemblyInputLine::Comment(c.clone()));
                            new_line2 = Some(AssemblyInputLine::Comment(c));
                        }
                        else if i1.opcode == Opcode::Push8 && i2.opcode == Opcode::Pop8 {
                            let comment = format!("Optimized push/pop pair to a move: {:?} {:?}", i1, i2);
                            new_line1 = Some(AssemblyInputLine::Instruction(Instruction {
                                opcode: Opcode::Copy8,
                                args: vec![Value::Register(*r1), Value::Register(*r2)],
                                resolved: None,
                                source: comment.clone(),
                            }));
                            new_line2 = Some(AssemblyInputLine::Comment(comment));
                        }
                     }
                 }
            }
            _ => {}
        }

        if let Some(new_line1) = new_line1 {
            *line1 = new_line1;
            optimizations_applied += 1;
        }
        if let Some(new_line2) = new_line2 {
            *line2 = new_line2;
            optimizations_applied += 1;
        }
    }

    optimizations_applied
}