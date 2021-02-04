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
    pub registers_available: BTreeSet<Register>,
}