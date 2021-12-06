use super::*;

pub struct ProgramContext {
    pub entry: String,
    pub function_defs: BTreeMap<String, FunctionDefinition>,
    pub function_impls: BTreeMap<String, AllocatedFunction>,
    pub types: BTreeMap<String, StructDefinition>,
    pub registers_available: Vec<Register>,
    pub statics_cur_address: u32,
    pub image_base_address: u32,
    pub statics_base_address: u32,
}

impl ProgramContext {
    fn find_contiguous(&self, count: u8, alignment: u8) -> Option<usize> {
        for (i, w) in self.registers_available.windows(count as usize).enumerate() {
            if w[0].0 % alignment != 0 {
                continue;
            }
            let range = w.iter().max().unwrap().0 - w.iter().min().unwrap().0;
            if range + 1 == count {
                return Some(i);
            }
        }
        None
    }

    pub fn find_registers(&mut self, count: u8, alignment: u8) -> Option<Vec<Register>> {
        let first_r = self.find_contiguous(count, alignment);
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