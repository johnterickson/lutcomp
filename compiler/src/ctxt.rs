use super::*;

pub struct ProgramContext {
    pub entry: String,
    pub function_defs: BTreeMap<String, FunctionDefinition>,
    pub struct_types: BTreeMap<String, StructDefinition>,
    pub image_base_address: u32,
    pub statics_base_address: u32,
    pub consts: BTreeMap<String, (Type, Vec<u8>)>,
    pub statics: BTreeMap<String, Type>,
}