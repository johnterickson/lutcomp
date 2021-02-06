use super::*;
use assemble::{Instruction, Value};
use super::parse::*;

#[derive(Debug, Clone)]
pub struct Call {
    pub function: String, 
    pub parameters: Vec<Expression>,
}

impl Call {
    pub fn parse(pair: pest::iterators::Pair<Rule>) -> Call {
        assert_eq!(pair.as_rule(), Rule::call_expression);

        let mut tokens = pair.into_inner();
        let token = tokens.next().unwrap();
        assert_eq!(Rule::ident, token.as_rule());
        let function = token.as_str().to_owned();

        let mut parameters = Vec::new();
        while let Some(arg) = tokens.next() {
            parameters.push(Expression::parse(arg));
        }

        Call{function, parameters}
    }

    pub fn emit(&self, ctxt: &mut FunctionContext) -> Type {

        let start_offset = ctxt.additional_offset;

        let f = ctxt.program.function_impls
            .get(&self.function)
            .expect(&format!("could not find function '{}'", &self.function));

        assert_eq!(f.def.args.len(), self.parameters.len());

        let registers_to_save: Vec<_> = ctxt.function.registers_used.intersection(&f.registers_used).cloned().collect();

        for r in &registers_to_save {
            ctxt.add_inst(Instruction {
                opcode: Opcode::Push8,
                source: format!("{:?} save register before call", &self),
                args: vec![Value::Register(r.0)],
                resolved: None,
            });
            ctxt.additional_offset += 1;
        }

        let ret_value_offset = ctxt.additional_offset;
        
        for (i,p) in self.parameters.iter().enumerate() {
            let param_type = p.emit(ctxt);
            assert_eq!(f.def.args[i].1, param_type);
            match param_type.byte_count(ctxt.program) {
                4 => {}
                1 => {
                    ctxt.add_inst(Instruction {
                        opcode: Opcode::Pop8,
                        source: format!("{:?} padding for arg{}", &self, i),
                        args: vec![Value::Register(4)],
                        resolved: None,
                    });
                    ctxt.additional_offset -= 1;

                    ctxt.add_inst(Instruction {
                        opcode: Opcode::LoadImm8,
                        source: format!("{:?} padding byte value for arg{}", &self, i),
                        args: vec![Value::Register(0), Value::Constant8(0xCC)],
                        resolved: None,
                    });
                    for b in 1..4 {
                        ctxt.add_inst(Instruction {
                            opcode: Opcode::Push8,
                            source: format!("{:?} padding byte {} for arg{}", &self, b, i),
                            args: vec![Value::Register(0)],
                            resolved: None,
                        });
                        ctxt.additional_offset += 1;
                    }

                    ctxt.add_inst(Instruction {
                        opcode: Opcode::Push8,
                        source: format!("{:?} padded byte for arg{}", &self, i),
                        args: vec![Value::Register(4)],
                        resolved: None,
                    });
                    ctxt.additional_offset += 1;
                }
                _ => panic!()
            };
        }

        // store return address
        ctxt.add_inst(Instruction {
            opcode: Opcode::AddImm32IgnoreCarry,
            source: format!("{:?} compute return address", &self),
            args: vec![Value::Register(REG_SP), Value::Constant32((-4i32) as u32)],
            resolved: None,
        });
        ctxt.add_inst(Instruction {
            opcode: Opcode::StoreImm32,
            source: format!("{:?} store return address", &self),
            args: vec![Value::Register(REG_SP), Value::PcOffset(4+6)],
            resolved: None,
        });
        ctxt.additional_offset += 4;

        // call
        ctxt.add_inst(Instruction {
            opcode: Opcode::JmpImm,
            source: format!("{:?} call {}", &self, &self.function),
            args: vec![Value::Label24(format!(":{}", &self.function))],
            resolved: None,
        });

        let parameters_bytes = self.parameters.len() as u32 * 4;

        // discard paramters and padding and return address
        ctxt.add_inst(Instruction {
            opcode: Opcode::AddImm32IgnoreCarry,
            source: format!("{:?} clean up stack", &self),
            args: vec![Value::Register(REG_SP), Value::Constant32(parameters_bytes + 4)],
            resolved: None,
        });
        ctxt.additional_offset -= parameters_bytes + 4;


        // result is in r0..
        assert_eq!(ret_value_offset, ctxt.additional_offset);

        assert_eq!(start_offset + registers_to_save.len() as u32, ctxt.additional_offset);

        if registers_to_save.len() > 0 {
            for r in registers_to_save.iter().rev() {
                ctxt.add_inst(Instruction {
                    opcode: Opcode::Pop8,
                    source: format!("{:?} restore saved register after call", &self),
                    args: vec![Value::Register(r.0)],
                    resolved: None,
                });
                ctxt.additional_offset -= 1;
            }
        }

        assert_eq!(start_offset, ctxt.additional_offset);

        f.def.return_type.clone()
    }
}