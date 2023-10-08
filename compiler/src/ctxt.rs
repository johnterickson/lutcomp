use super::*;

pub struct ProgramContext {
    pub entry: String,
    pub image_base_address: u32,
    pub statics_base_address: u32,
    pub function_defs: BTreeMap<String, FunctionDefinition>,
    pub struct_types: BTreeMap<String, StructDefinition>,
    pub consts: BTreeMap<String, (Type, Constant)>,
    pub statics: BTreeMap<String, Type>,
    pub roots: Vec<PathBuf>,
    pub includes: BTreeSet<PathBuf>,
}
