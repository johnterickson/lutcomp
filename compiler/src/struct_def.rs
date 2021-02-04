use super::*;

#[derive(Debug)]
pub struct StructDefinition {
    pub fields: Vec<(String,Type)>
}

impl StructDefinition {
    pub fn get_field(&self, field_name: &str) -> (u32, &Type) {
        let (index, field) = self.fields
            .iter().enumerate()
            .filter(|f| &f.1.0 == field_name)
            .next()
            .expect(&format!("could not find field {} in struct {:?}", field_name, &self));
        (4 * index as u32, &field.1)
    }
}

impl ByteSize for StructDefinition {
    fn byte_count(&self, ctxt: &ProgramContext) -> u32 {
        self.fields.iter().map(|f| f.1.byte_count(ctxt)).sum()
    }
}