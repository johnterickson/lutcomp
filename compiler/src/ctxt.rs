use super::*;
use assemble::{AssemblyInputLine, Instruction};

pub struct FunctionContext<'a> {
    pub program: &'a ProgramContext,
    pub function: &'a AllocatedFunction,
    pub lines: Vec<AssemblyInputLine>,
    pub additional_offset: u32,
    pub block_counter: usize,
}

impl<'a> FunctionContext<'a> {
    pub fn add_inst(&mut self, i: Instruction) {
        self.lines.push(AssemblyInputLine::Instruction(i));
    }

    pub fn add_macro(&mut self, s: String) {
        self.lines.push(AssemblyInputLine::from_str(&s))
    }

    pub fn find_local(&self, local: &str) -> &Variable {
        self.function.variables
            .get(local)
            .expect(&format!("could not find '{}'", local))
    }

    pub fn get_stack_offset(&self, offset: BaseOffset) -> u32 {
        offset.0 + self.additional_offset
    }
}

pub struct ProgramContext {
    pub entry: String,
    pub function_defs: BTreeMap<String, FunctionDefinition>,
    pub function_impls: BTreeMap<String, AllocatedFunction>,
    pub types: BTreeMap<String, StructDefinition>,
    pub registers_available: Vec<Register>,
}

impl ProgramContext {
    fn find_contiguous(&self, count: u8) -> Option<usize> {
        for (i, w) in self.registers_available.windows(count as usize).enumerate() {
            let range = w.iter().max().unwrap().0 - w.iter().min().unwrap().0;
            if range + 1 == count {
                return Some(i);
            }
        }
        None
    }

    pub fn find_registers(&mut self, count: u8) -> Option<Vec<Register>> {
        let first_r = self.find_contiguous(count);
        if let Some(first_r) = first_r {
            let v = self.registers_available[first_r..first_r+count as usize].to_vec();
            for _ in 0..count {
                self.registers_available.remove(first_r);
            }
            Some(v)
        } else {
            None
        }
    }
}