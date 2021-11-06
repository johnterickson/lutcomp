use super::*;
use assemble::{Instruction, Value};

#[derive(Debug, Copy, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub struct Register(pub u8);

#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct BaseOffset(pub u32);

#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum Storage {
    Register(Register),
    Stack(BaseOffset),
    FixedAddress(u32),
}
#[derive(Debug)]
pub enum LogicalReference {
    Local,
    LocalField(String),
    DerefField(String),
    ArrayIndex{multiplier: u32, index_reg: Register},
}

#[derive(Debug)]
enum DerefOffset {
    None,
    Constant(u32),
    Register(u32, Register)
}

#[derive(Debug)]
pub enum EmitAddressResult {
    AddressInReg0{ptr_type: Type},
    ValueInRegister{reg: Register, value_type: Type},
}

#[derive(Debug)]
struct MemoryReference {
    local_offset: u32,
    deref_offset: DerefOffset,
}

impl LogicalReference {
    fn get_offset_for_field<'a>(ctxt: &'a FunctionContext, struct_name: &str, field_name: &str) -> (u32, &'a Type){
        let struct_type = ctxt.program.types.get(struct_name)
            .expect(&format!("Could not find struct '{}'", struct_name));
        struct_type.get_field(field_name)
    }

    fn get_deref_offset<'a>(&self, ctxt: &'a FunctionContext, var_type: &'a Type) -> (MemoryReference, &'a Type) {
        // dbg!((&self, &var_type));
        match (self, var_type) {
            // (LogicalReference::Deref, Type::Ptr(inner)) => {
            //     (MemoryReference{local_offset:0, deref_offset: DerefOffset::Constant(0)}, inner.as_ref())
            // }
            (LogicalReference::Local, var_type) => {
                (MemoryReference{local_offset:0, deref_offset: DerefOffset::None}, var_type)
            }
            (LogicalReference::LocalField(field_name), Type::Struct(struct_name)) => {
                let (field_offset, target_type) = LogicalReference::get_offset_for_field(ctxt, struct_name, field_name);
                (MemoryReference {local_offset:field_offset, deref_offset: DerefOffset::None}, target_type)
            }
            (LogicalReference::DerefField(field_name), Type::Ptr(inner)) => {
                if let Type::Struct(struct_name) = inner.as_ref() {
                    let (field_offset, target_type) = LogicalReference::get_offset_for_field(ctxt, struct_name, field_name);
                    (MemoryReference {local_offset:0, deref_offset: DerefOffset::Constant(field_offset)}, target_type)
                } else {
                    panic!("'{}' is accessed as a struct but it is '{:?}'", field_name, inner);
                }
            }
            (LogicalReference::ArrayIndex{multiplier, index_reg}, Type::Array(num_type, _)) => {
                assert_eq!(*multiplier, 1);
                assert_eq!(num_type.as_ref(), &Type::Number(NumberType::U8));
                (MemoryReference {local_offset:0, deref_offset: DerefOffset::Register(*multiplier, *index_reg)}, num_type.as_ref())
            }
            _ => panic!("Don't know how to reference '{:?}' via '{:?}'", var_type, &self)
        }
    }



    pub fn try_emit_local_address_to_reg0(&self, ctxt: &mut FunctionContext, local_name: &str) -> EmitAddressResult {
        let local = ctxt.find_var(local_name);
        // dbg!(&local);

        let (mem_ref, final_type) = self.get_deref_offset(ctxt, &local.var_type);
        let final_type = final_type.clone();

        // dbg!(&final_type);

        match local.storage {
            Storage::FixedAddress(addr) => {
                match mem_ref.deref_offset {
                    DerefOffset::Register(multiplier, index_reg) => {
                        assert_eq!(multiplier, 1);
                        assert_eq!(index_reg.0, 0);

                        ctxt.add_inst(Instruction {
                            source: format!("loading fixed address as base {:?}", &local_name),
                            opcode: Opcode::LoadImm32,
                            args: vec![Value::Register(8), Value::Constant32(addr)],
                            resolved: None,
                        });
                        ctxt.add_inst(Instruction {
                            source: format!("adding array index {:?}", &self),
                            opcode: Opcode::Add32NoCarryIn,
                            args: vec![Value::Register(8), Value::Register(index_reg.0), Value::Register(0)],
                            resolved: None,
                        });
                    },
                    DerefOffset::None => {
                        ctxt.add_inst(Instruction {
                            source: format!("loading fixed address {:?}", &local_name),
                            opcode: Opcode::LoadImm32,
                            args: vec![Value::Register(0), Value::Constant32(addr)],
                            resolved: None,
                        });
                    }
                    o => unimplemented!("{:?}", &o),
                }
                EmitAddressResult::AddressInReg0{ptr_type: Type::Ptr(Box::new(final_type))}
            }
            Storage::Register(r) => {
                assert_eq!(0, mem_ref.local_offset);
                let byte_count = final_type.byte_count(ctxt.program);
                match (mem_ref.deref_offset, byte_count) {
                    (DerefOffset::Constant(c), _) => {
                        if c == 0 {
                            ctxt.add_inst(Instruction {
                                source: format!("copying address to reg0 {:?}", &self),
                                opcode: Opcode::Or32,
                                args: vec![Value::Register(r.0), Value::Register(r.0), Value::Register(0)],
                                resolved: None,
                            });
                        } else {
                            ctxt.add_inst(Instruction {
                                source: format!("copying base address to reg0 {:?}", &self),
                                opcode: Opcode::Or32,
                                args: vec![Value::Register(r.0), Value::Register(r.0), Value::Register(0)],
                                resolved: None,
                            });
                            ctxt.add_inst(Instruction {
                                source: format!("adding offset to register {:?}", &self),
                                opcode: Opcode::AddImm32IgnoreCarry,
                                args: vec![Value::Register(0), Value::Constant32(c)],
                                resolved: None,
                            });
                        }
                        EmitAddressResult::AddressInReg0{ptr_type: Type::Ptr(Box::new(final_type))}
                    }
                    (DerefOffset::None, 4) => {
                        ctxt.add_inst(Instruction {
                            source: format!("copying value to register {:?}", &self),
                            opcode: Opcode::Or32,
                            args: vec![Value::Register(r.0), Value::Register(r.0), Value::Register(0)],
                            resolved: None,
                        });
                        EmitAddressResult::ValueInRegister{reg:r, value_type: final_type}
                        // if i != 0 {

                        // }
                    }
                    (DerefOffset::None, byte_count) if byte_count >= 1 && byte_count <=3 => {
                        for i in 0..byte_count as u8 {
                            ctxt.add_inst(Instruction {
                                source: format!("copying register {:?}", &self),
                                opcode: Opcode::Or8,
                                args: vec![Value::Register(r.0 + i), Value::Register(r.0 + i), Value::Register(i)],
                                resolved: None,
                            });
                        }
                        EmitAddressResult::ValueInRegister{reg:r, value_type: final_type}
                    }
                    (d,b) => unimplemented!("{:?} is {:?} and does not fit in a register.", &final_type, &(d, b)),
                }
                
                // if let Storage::ValueRegister(_) = local.storage {
                // } else {
                //     EmitAddressResult::AddressInReg0{ptr_type: Type::Ptr(Box::new(final_type))}
                // }
            }
            Storage::Stack(base_offset) => {
                let offset = ctxt.get_stack_offset(base_offset) + mem_ref.local_offset;

                ctxt.add_inst(Instruction {
                    source: format!("stack offset for {} is {:x}: {:?}", local_name, offset, &self),
                    opcode: Opcode::LoadImm32,
                    args: vec![Value::Register(8), Value::Constant32(offset)],
                    resolved: None,
                });

                match mem_ref.deref_offset {
                    DerefOffset::Register(multiplier, index_reg) => {
                        assert_eq!(multiplier, 1);
                        assert_eq!(index_reg.0, 0);

                        ctxt.add_inst(Instruction {
                            source: format!("calculating stack address for deref {:?}", &self),
                            opcode: Opcode::Add32NoCarryIn,
                            args: vec![Value::Register(REG_SP), Value::Register(8), Value::Register(8)],
                            resolved: None,
                        });
                        ctxt.add_inst(Instruction {
                            source: format!("adding array index {:?}", &self),
                            opcode: Opcode::Add32NoCarryIn,
                            args: vec![Value::Register(8), Value::Register(index_reg.0), Value::Register(0)],
                            resolved: None,
                        });
                    }
                    DerefOffset::Constant(deref_offset) => {
                        ctxt.add_inst(Instruction {
                            source: format!("calculating stack address for deref {:?}", &self),
                            opcode: Opcode::Add32NoCarryIn,
                            args: vec![Value::Register(REG_SP), Value::Register(8), Value::Register(4)],
                            resolved: None,
                        });
                        ctxt.add_inst(Instruction {
                            source: format!("reading base address from stack {:?}", &self),
                            opcode: Opcode::Load32,
                            args: vec![Value::Register(4), Value::Register(0)],
                            resolved: None,
                        });

                        if deref_offset != 0 {
                            ctxt.add_inst(Instruction {
                                source: format!("offseting pointer for field access {:?}", &self),
                                opcode: Opcode::AddImm32IgnoreCarry,
                                args: vec![Value::Register(0), Value::Constant32(deref_offset)],
                                resolved: None,
                            });
                        }
                    }
                    DerefOffset::None => {
                        ctxt.add_inst(Instruction {
                            source: format!("calculating stack address {:?}", &self),
                            opcode: Opcode::Add32NoCarryIn,
                            args: vec![Value::Register(REG_SP), Value::Register(8), Value::Register(0)],
                            resolved: None,
                        });
                    }
                }
                EmitAddressResult::AddressInReg0{ptr_type: Type::Ptr(Box::new(final_type))}
            }
        }
    }
}
