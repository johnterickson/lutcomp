use super::*;

pub fn optimize_assembly(assembly: &mut Vec<AssemblyInputLine>) -> usize {

    let mut optimizations_applied = 0;

    let mut instruction_indices = Vec::new();
    for (i, line) in assembly.as_mut_slice().iter_mut().enumerate() {
        if let AssemblyInputLine::Instruction(_inst) = line {
            instruction_indices.push(i);
        }
    }

    // dbg!(&instruction_indices);

    for instruction_pairs in instruction_indices.windows(2) {
        let (slice1, slice2) = assembly.as_mut_slice()[instruction_pairs[0]..].split_at_mut(1);
        let line1 = &mut slice1[0];
        let line2 = &mut slice2[instruction_pairs[1] - instruction_pairs[0] - 1];

        let mut new_line1 = None;
        let mut new_line2 = None;

        match (&line1, &line2) {
            (AssemblyInputLine::Instruction(i1),
             AssemblyInputLine::Instruction(i2)) => {
                 if (i1.opcode == Opcode::Push8) && (i2.opcode == Opcode::Pop8) || (i2.opcode == Opcode::Pop8 && i1.opcode == Opcode::Push8) {
                     if let (Value::Register(r1), Value::Register(r2)) = (&i1.args[0], &i2.args[0]) {
                        if r1 == r2 {
                            new_line1 = Some(AssemblyInputLine::Comment(format!("optimized away push/pop pair: {:?}", line1)));
                            new_line2 = Some(AssemblyInputLine::Comment(format!("optimized away push/pop pair: {:?}", line2)));
                        }
                        else if i1.opcode == Opcode::Push8 && i2.opcode == Opcode::Pop8 {
                            let comment = format!("Optimized push/pop pair to a move: {:?} {:?}", i1, i2);
                            new_line1 = Some(AssemblyInputLine::Instruction(Instruction {
                                opcode: Opcode::Or8,
                                args: vec![Value::Register(*r1), Value::Register(*r1), Value::Register(*r2)],
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