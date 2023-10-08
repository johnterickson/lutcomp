use super::*;


#[derive(Debug)]
pub struct StructField {
    pub name: String,
    pub var_type: Type,
    pub struct_offset: u32,
    pub implied_alignment: Option<u32>,
}

#[derive(Debug)]
pub struct StructDefinition {
    pub fields: BTreeMap<String, StructField>,
    pub alignment: u32,
    pub total_size: u32,
}

// impl StructDefinition {
//     pub fn get_field(&self, field_name: &str) -> &StructField {
//         let mut offset = 0u32;
//         for (name, field_type) in  &self.fields {
//             if name == field_name {
//                 return (offset, field_type);
//             }

//             let field_size = field_type.byte_count(ctxt);
//             offset += Self::round_up_field_size(field_size);
//         }

//         panic!("could not find field {} in struct {:?}", field_name, &self);
//     }

//     fn round_up_field_size(s: u32) -> u32 {
//         ((s + 3) / 4) * 4
//     }
// }

impl ByteSize for StructDefinition {
    fn byte_count(&self, _: &ProgramContext) -> u32 {
        self.total_size
    }
}