use super::*;

#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct BaseOffset(pub u32);

#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum Storage {
    Register(Register),
    Stack(BaseOffset),
    FixedAddress(u32),
}
