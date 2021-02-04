use super::*;

pub fn optimize(assembly: &mut Vec<AssemblyInputLine>) -> usize {

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
        let line2 = &mut slice2[instruction_pairs[1] - instruction_pairs[0]- 1];

        let mut comment_out = false;

        match (&line1, &line2) {
            (AssemblyInputLine::Instruction(i1),
             AssemblyInputLine::Instruction(i2)) => {
                 if (i1.opcode == Opcode::Push8) && (i2.opcode == Opcode::Pop8) || (i2.opcode == Opcode::Pop8 && i1.opcode == Opcode::Push8) {
                     if let Value::Register(r1) = i1.args[0] {
                        if let Value::Register(r2) = i2.args[0] {
                            if r1 == r2 {
                                // dbg!(i1);
                                // dbg!(i2);
                                comment_out = true;
                            }
                        }
                     }
                 }
            }
            _ => {}
        }

        if comment_out {
            *line1 = AssemblyInputLine::Comment(format!("optimized away push/pop pair: {:?}", line1));
            *line2 = AssemblyInputLine::Comment(format!("optimized away push/pop pair: {:?}", line2));

            optimizations_applied += 1;
        }
    }

    optimizations_applied
}