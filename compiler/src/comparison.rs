use crate::*;


#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ComparisonOperator {
    Equals,
    NotEquals,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
}

impl ComparisonOperator {
    pub fn parse(pair: pest::iterators::Pair<Rule>) -> ComparisonOperator {
        match pair.as_str() {
            "==" => ComparisonOperator::Equals,
            "!=" => ComparisonOperator::NotEquals,
            ">" => ComparisonOperator::GreaterThan,
            ">=" => ComparisonOperator::GreaterThanOrEqual,
            "<" => ComparisonOperator::LessThan,
            "<=" => ComparisonOperator::LessThanOrEqual,
            op => panic!("Unknown op: {}", op),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Comparison {
    pub op: ComparisonOperator,
    pub left: Expression,
    pub right: Expression,
}

impl Comparison {
    pub fn emit_branch(&self, ctxt: &mut FunctionContext, when_true:&str, when_false: &str) {
        let (actual_left_reg, left_type) = self.left.emit(ctxt, None);
        assert_eq!(actual_left_reg, None);
        let (actual_right_reg, right_type) = self.right.emit(ctxt, None);
        assert_eq!(actual_right_reg, None);

        let size = left_type.byte_count(ctxt.program);
        if size != right_type.byte_count(ctxt.program) {
            panic!("'{:?}' and '{:?}' are different sizes.", &left_type, &right_type);
        }

        let size: u8 = size.try_into().expect("Comparison size is too big.");
        assert!(size == 1 || size == 4);

        if left_type != right_type {
            panic!("'{:?}' and '{:?}' are different types.", &left_type, &right_type);
        }

        let right_reg = 8;
        for r in 0..size {
            ctxt.add_inst(Instruction {
                opcode: Opcode::Pop8,
                resolved: None,
                source: format!("{:?}", &self),
                args: vec![Value::Register(right_reg + r)]
            });
            ctxt.additional_offset -= 1;
        }

        let left_reg = 4;
        for r in 0..size {
            ctxt.add_inst(Instruction {
                opcode: Opcode::Pop8,
                resolved: None,
                source: format!("{:?}", &self),
                args: vec![Value::Register(left_reg + r)]
            });
            ctxt.additional_offset -= 1;
        }

        let (first_base_reg, second_base_reg) = match self.op {
            ComparisonOperator::Equals | ComparisonOperator::NotEquals => (right_reg, left_reg),
            ComparisonOperator::GreaterThan | ComparisonOperator::LessThanOrEqual => (left_reg, right_reg),
            ComparisonOperator::LessThan | ComparisonOperator::GreaterThanOrEqual => (right_reg, left_reg),
        };

        // compare MSB
        ctxt.add_inst(Instruction {
            opcode: Opcode::Cmp8,
            resolved: None,
            source: format!("{:?}", &self),
            args: vec![Value::Register(first_base_reg+size-1), Value::Register(second_base_reg+size-1)]
        });
        
        // compare other bytes if needed
        for r in 1..size {
            ctxt.add_inst(Instruction {
                opcode: Opcode::Cmp8IfZero,
                resolved: None,
                source: format!("{:?}", &self),
                args: vec![Value::Register(first_base_reg+size-1-r), Value::Register(second_base_reg+size-1-r)]
            });
        }

        let (cond, uncond, jmp_op) = match self.op {
            ComparisonOperator::Equals | ComparisonOperator::NotEquals => {
                if self.op == ComparisonOperator::Equals {
                    (when_true, when_false, Opcode::JzImm)
                } else {
                    (when_false,when_true, Opcode::JzImm)
                }
            },
            ComparisonOperator::GreaterThan | ComparisonOperator::LessThanOrEqual => {
                if self.op == ComparisonOperator::LessThanOrEqual {
                    (when_true,when_false,Opcode::JcImm)
                } else {
                    (when_false,when_true,Opcode::JcImm)
                }
            },
            ComparisonOperator::LessThan | ComparisonOperator::GreaterThanOrEqual => {
                if self.op == ComparisonOperator::GreaterThanOrEqual {
                    (when_true,when_false,Opcode::JcImm)
                } else {
                    (when_false,when_true,Opcode::JcImm)
                }
            }
        };

        ctxt.add_inst(Instruction {
            opcode: jmp_op,
            source: format!("{:?}", &self),
            args: vec![Value::Label24(cond.to_owned())],
            resolved: None,
        });
        ctxt.add_inst(Instruction {
            opcode: Opcode::JmpImm,
            source: format!("{:?}", &self),
            args: vec![Value::Label24(uncond.to_owned())],
            resolved: None,
        });
    }
}