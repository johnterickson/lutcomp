use super::*;

#[derive(Debug)]
pub struct StructDefinition {
    pub fields: Vec<(String,Type)>
}

impl StructDefinition {
    pub fn get_field(&self, ctxt: &ProgramContext, field_name: &str) -> (u32, &Type) {
        let mut offset = 0u32;
        for (name, field_type) in  &self.fields {
            if name == field_name {
                return (offset, field_type);
            }

            let field_size = field_type.byte_count(ctxt);
            offset += Self::round_up_field_size(field_size);
        }

        panic!("could not find field {} in struct {:?}", field_name, &self);
    }

    fn round_up_field_size(s: u32) -> u32 {
        ((s + 3) / 4) * 4
    }
}

impl ByteSize for StructDefinition {
    fn byte_count(&self, ctxt: &ProgramContext) -> u32 {
        self.fields.iter().map(|f| 
            StructDefinition::round_up_field_size(f.1.byte_count(ctxt))).sum()
    }
}