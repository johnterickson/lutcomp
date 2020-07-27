extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;

use std::io;
use std::io::ErrorKind;
use std::io::Read;
use std::collections::{BTreeMap,BTreeSet};
use std::str::FromStr;

use common::*;

#[derive(Parser)]
#[grammar = "j.pest"]
struct ProgramParser;

#[derive(Debug, PartialEq, Eq)]
enum Operator {
    Add,
    Subtract,
    Multiply,
    Or,
    Equals,
    NotEquals
}

impl Operator {
    fn parse(pair: pest::iterators::Pair<Rule>) -> Operator {
        match pair.as_str() {
            "+" => Operator::Add,
            "-" => Operator::Subtract,
            "*" => Operator::Multiply,
            "||" => Operator::Or,
            "==" => Operator::Equals,
            "!=" => Operator::NotEquals,
            _ => panic!(),
        }
    }
}

struct FunctionContext {
    pub regs_touched: BTreeSet<Reg>,
    pub stack: BTreeMap<String, LocalStorage>,
    pub lines: Vec<Line>,
    pub additional_offset: usize,
    pub block_counter: usize,
}

impl FunctionContext {
    fn add_inst(&mut self, i: Instruction) {
        //println!("{:?}",&i);
        self.lines.push(Line::Instruction(i));
    }

    fn add_macro(&mut self, s: String) {
        let line = Line::parse(s);
        self.lines.push(line);
    }

    fn find_local(&mut self, local: &str) -> LocalStorage {
        let local = self.stack
            .get(local)
            .expect(&format!("could not find {}", local));
        match local {
            LocalStorage::Stack(offset) => {
                LocalStorage::Stack(*offset + self.additional_offset)
            },
            LocalStorage::Register(r) => {
                self.regs_touched.insert(*r);
                LocalStorage::Register(*r)
            }
        }
    }
}

#[derive(Debug)]
enum Expression {
    Ident(String),
    Number(i32),
    Operation(Operator, Box<Expression>, Box<Expression>)
}

impl Expression {
    fn parse(pair: pest::iterators::Pair<Rule>) -> Expression {
        assert_eq!(Rule::expression, pair.as_rule());
        let pair = pair.into_inner().next().unwrap();
        match pair.as_rule() {
            Rule::number => {
                let mut n = 0;
                let mut digits = pair.into_inner();
                while let Some(digit) = digits.next() {
                    let digit = i32::from_str(digit.as_str()).expect("Couldn't parse integer.");
                    n *= 10;
                    n += digit;
                }
                Expression::Number(n)
            },
            Rule::ident => {
                let mut label = String::new();
                let mut chars = pair.into_inner();
                while let Some(c) = chars.next() {
                    label += c.as_str();
                }
                Expression::Ident(label)
            },
            Rule::operator_expression => {
                let mut pairs = pair.into_inner();
                let left = Expression::parse(pairs.next().unwrap());
                let op = Operator::parse(pairs.next().unwrap());
                let right = Expression::parse(pairs.next().unwrap());
                Expression::Operation(op, Box::new(left), Box::new(right))
            },
            _ => unimplemented!()
        }
    }

    fn is_tail(&self) -> bool {
        match self {
            Expression::Ident(_) => true,
            Expression::Number(_) => true,
            Expression::Operation(_,_,_) => false,
        }
    }

    // if target_stack, output is in top of stack; else, in ACC
    fn emit(&self, ctxt: &mut FunctionContext, target_stack: bool) -> () {
        ctxt.lines.push(Line::Comment(format!("Evaluating expression: {:?} additional_offset:{}", &self, ctxt.additional_offset)));

        match self {
            Expression::Number(n) => {
                let n = *n as u8;
                let sign_extended = ((n as i8) << 4) >> 4;
                let needs_load_hi = sign_extended != (n as i8);

                if needs_load_hi {
                    ctxt.add_inst(Instruction::WithoutPush(PushableInstruction::LoadLo(Target::Absolute(n & 0xF))));
                    ctxt.add_inst(Instruction::with_push(target_stack, PushableInstruction::LoadHi(Target::Absolute((n>>4) & 0xF))));

                } else {
                    ctxt.add_inst(Instruction::with_push(target_stack, PushableInstruction::LoadLo(Target::Absolute(n & 0xF))));
                }

                if target_stack {
                    ctxt.additional_offset += 1;
                }
            },
            Expression::Ident(n) => {
                let local = ctxt.find_local(n);
                match local {
                    LocalStorage::Register(r) => {
                        // ctxt.add_inst(Instruction::LoadReg(r));
                        unimplemented!();
                    },
                    LocalStorage::Stack(offset) => {
                        ctxt.add_inst(Instruction::with_push(target_stack, 
                            PushableInstruction::LoadFromStack(StackOffset::new(offset as u8))));

                        if target_stack {
                            ctxt.additional_offset += 1;
                        }
                    }
                }
            },
            Expression::Operation(op, left, right) => {
                // if left.is_tail() && right.is_tail() {
                //     left.emit(ctxt, Reg::C);
                //     right.emit(ctxt, Reg::ACC)
                // }
                // else 
                {
                    left.emit(ctxt, true); //store left on the stack
                }

                // left in c; right in acc

                match op {
                    Operator::Add => {
                        right.emit(ctxt, false); // left on top of stack; right in ACC
                        ctxt.add_inst(Instruction::WithoutPush(PushableInstruction::Add(StackOffset::top())));
                    },
                    Operator::Multiply => {
                        right.emit(ctxt, false); // left on top of stack; right in ACC
                        ctxt.add_inst(Instruction::WithoutPush(PushableInstruction::Mul(StackOffset::top())));
                    },
                    Operator::Subtract => {
                        right.emit(ctxt, true);

                        // sp+0 right
                        // sp+1 left
                        
                        ctxt.add_inst(Instruction::WithoutPush(PushableInstruction::Not(StackOffset::top())));

                        // ACC ~right
                        
                        ctxt.add_inst(Instruction::StoreToStack(StackOffset::top()));

                        // sp+0 ~right

                        ctxt.add_inst(Instruction::WithoutPush(PushableInstruction::LoadLo(Target::Absolute(1))));

                        // ACC 1

                        ctxt.add_inst(Instruction::WithoutPush(PushableInstruction::Add(StackOffset::top())));

                        // ACC ~right + 1
                        ctxt.add_inst(Instruction::StoreToStack(StackOffset::top()));

                        ctxt.add_inst(Instruction::Discard(StackOffset::new(1)));
                        ctxt.additional_offset -= 1;

                        // ACC left + (~right + 1) == left - right
                        ctxt.add_inst(Instruction::WithoutPush(PushableInstruction::Add(StackOffset::top())));
                    },
                    Operator::Equals => {
                        assert!(!target_stack);
                        right.emit(ctxt, false); // left on top of stack; right in ACC

                        //  left == right --> ACC == 0
                        //  left != right --> ACC != 0
                        ctxt.add_inst(Instruction::WithoutPush(PushableInstruction::Xor(StackOffset::top())));
                        //  left ^ right == 0 --> left == right
                    },
                    Operator::NotEquals => {
                        //  left == right --> ACC != 0
                        //  left != right --> ACC == 0
                        assert!(!target_stack);
                        right.emit(ctxt, false); // left on top of stack; right in ACC

                        unimplemented!();
                    }
                    _ => unimplemented!()
                }

                
                if target_stack {
                    ctxt.add_inst(Instruction::StoreToStack(StackOffset::top()));
                } else {
                    ctxt.add_inst(Instruction::Discard(StackOffset::new(1)));
                    ctxt.additional_offset -= 1;
                }
            }
        }
        
        ctxt.lines.push(Line::Comment(format!("Evaluated expression: {:?} additional_offset:{}", &self, ctxt.additional_offset)));
    }
}

const RESULT : &'static str = "RESULT";
const EPILOGUE : &'static str = "EPILOGUE";

#[derive(Debug)]
enum Statement {
    Assign {local: String, value: Expression},
    Call { local: String, function: String, parameters: Vec<Expression> },
    If {predicate: Expression, when_true: Vec<Statement> },
    Return { value: Expression},
    Load {local: String, address: Expression },
    Store {local: String, address: Expression },
}

impl Statement {
    fn parse(pair: pest::iterators::Pair<Rule>) -> Statement {
        assert_eq!(Rule::statement, pair.as_rule());
        let pair = pair.into_inner().next().unwrap();

        match pair.as_rule() {
            Rule::assign => {
                let mut pairs = pair.into_inner();
                let local = pairs.next().unwrap().as_str().trim().to_owned();
                let value = Expression::parse(pairs.next().unwrap());
                Statement::Assign { local, value }
            },
            Rule::call => {
                let mut pairs = pair.into_inner();
                let local = pairs.next().unwrap().as_str().trim().to_owned();
                let function = pairs.next().unwrap().as_str().to_owned();

                let mut parameters = Vec::new();
                while let Some(arg) = pairs.next() {
                    parameters.push(Expression::parse(arg));
                }

                Statement::Call { local, function, parameters }
            },
            Rule::if_statement => {
                let mut pairs = pair.into_inner();
                let predicate = Expression::parse(pairs.next().unwrap());
                let mut when_true = Vec::new();
                while let Some(stmt) = pairs.next() {
                    when_true.push(Statement::parse(stmt));
                }
                Statement::If { predicate, when_true }
            },
            Rule::return_statement => {
                let expr = pair.into_inner().next().unwrap();
                Statement::Return { value: Expression::parse(expr) }
            },
            Rule::load => {
                let mut pairs = pair.into_inner();
                let local = pairs.next().unwrap().as_str().trim().to_owned();
                let address = Expression::parse(pairs.next().unwrap());
                Statement::Load { local, address }
            },
            Rule::store => {
                let mut pairs = pair.into_inner();
                let local = pairs.next().unwrap().as_str().trim().to_owned();
                let address = Expression::parse(pairs.next().unwrap());
                Statement::Store { local, address }
            }
            _ => panic!("Unexpected {:?}", pair)
        }
    }

    fn emit(&self, ctxt: &mut FunctionContext, function_name: &str) -> () {
        ctxt.lines.push(Line::Comment(format!("Begin statement {:?}", self)));
        match self {
            Statement::Load{local, address} => {
                address.emit(ctxt, false);
                ctxt.add_inst(Instruction::StoreAddr);
                ctxt.add_inst(Instruction::WithoutPush(PushableInstruction::LoadMem));

                let local = ctxt.find_local(local);
                match local {
                    LocalStorage::Register(r) => {
                        unimplemented!();
                    }
                    LocalStorage::Stack(offset) => {
                        ctxt.add_inst(Instruction::StoreToStack(StackOffset::new(offset as u8)));
                    }
                }
            },
            Statement::Store{local, address} => {
                address.emit(ctxt, false);
                ctxt.add_inst(Instruction::StoreAddr);

                let local = ctxt.find_local(local);
                match local {
                    LocalStorage::Register(r) => {
                        unimplemented!();
                    }
                    LocalStorage::Stack(offset) => {
                        ctxt.add_inst(Instruction::WithoutPush(
                            PushableInstruction::LoadFromStack(StackOffset::new(offset as u8))));
                    }
                }
                ctxt.add_inst(Instruction::StoreMem);
            },
            Statement::Assign{local, value} => {
                let local = ctxt.find_local(local);
                match local {
                    LocalStorage::Register(r) => {
                        unimplemented!();
                    }
                    LocalStorage::Stack(offset) => {
                        value.emit(ctxt, false);
                        ctxt.add_inst(Instruction::StoreToStack(StackOffset::new(offset as u8)));
                    }
                }
            },
            Statement::Return{ value } => {
                value.emit(ctxt, false);

                let result_offset = match ctxt.find_local(RESULT) {
                    LocalStorage::Register(_) => unimplemented!(),
                    LocalStorage::Stack(offset) => offset,
                };

                ctxt.add_inst(Instruction::StoreToStack(StackOffset::new(result_offset as u8)));


                if ctxt.additional_offset != 0 {
                    ctxt.add_inst(Instruction::Discard(StackOffset::new(ctxt.additional_offset as u8)));
                }
                ctxt.add_inst(Instruction::Jmp(Target::Label(
                    format!(":{}__{}", function_name, EPILOGUE)
                )));
            },
            Statement::Call{ local, function, parameters} => { 

                assert_eq!(ctxt.additional_offset, 0);

                // save space for result
                ctxt.add_inst(Instruction::WithPush(PushableInstruction::Not(StackOffset::top())));
                ctxt.additional_offset += 1;

                let regs_to_save : Vec<Reg> = ctxt.regs_touched.iter().cloned().collect();

                for r in &regs_to_save {
                    unimplemented!();
                    // ctxt.add_macro(format!("push {}", r));
                    // ctxt.additional_offset += 1;
                }

                for p in parameters {
                    p.emit(ctxt, true);
                }

                ctxt.add_macro(format!("call :{}", function));

                // discard paramters
                ctxt.add_inst(Instruction::Discard(StackOffset::new(parameters.len() as u8)));
                ctxt.additional_offset -= parameters.len();

                for r in regs_to_save.iter().rev() {
                    unimplemented!();
                    // ctxt.add_macro(format!("pop {}", r));
                    // ctxt.additional_offset -= 1;
                }

                // pop result into b
                ctxt.add_inst(Instruction::PopDiscard(StackOffset::top()));
                ctxt.additional_offset -= 1;

                // stack is now back to normal

                let local = ctxt.find_local(local);
                match local {
                    LocalStorage::Register(r) => {
                        unimplemented!();

                    },
                    LocalStorage::Stack(offset) => {
                        ctxt.add_inst(Instruction::StoreToStack(StackOffset::new(offset as u8)));
                    }
                }
            },
            Statement::If{predicate, when_true} => {
                let if_skip = "IF_SKIP";
                predicate.emit(ctxt, false); // result in ACC

                let jump_label = format!("{}_{}_{}", function_name, if_skip, ctxt.block_counter);

                ctxt.block_counter += 1;

                // WEIRD: interpret 0 as true
                ctxt.add_inst(Instruction::Jnz(Target::Label(format!(":{}", &jump_label))));

                // let mut count = 0;
                for s in when_true {
                    // let scope = format!("{}_stmt{}", scope, count);
                    s.emit(ctxt, function_name);
                    // count += 1;
                }
                
                ctxt.lines.push(Line::Label(format!(":{}", &jump_label)));
            },
        }
        ctxt.lines.push(Line::Comment(format!("Done  statement {:?}", self)));
    }
}

#[derive(Clone, Copy, Debug)]
enum LocalStorage {
    Register(Reg),
    Stack(usize),
}

#[derive(Debug)]
struct Function {
    name: String,
    args: Vec<String>,
    locals: BTreeSet<String>,
    body: Vec<Statement>,
}

impl Function {
    fn parse(pair: pest::iterators::Pair<Rule>) -> Function {
        assert_eq!(Rule::function, pair.as_rule());

        let mut args = Vec::new();

        let mut pairs = pair.into_inner();

        let name = pairs.next().unwrap().as_str().to_owned();

        for arg in pairs.next().unwrap().into_inner() {
            let arg = arg.as_str();
            args.push(arg.to_owned());
        }

        let body : Vec<Statement> = pairs.next().unwrap().into_inner().map(|p| Statement::parse(p)).collect();

        // find locals
        let mut locals = BTreeSet::new();

        fn add_locals(s: &Statement, args: &Vec<String>, locals: &mut BTreeSet<String>) {
            match s {
                Statement::Assign{local, value:_} 
                | Statement::Load{local, address:_}
                | Statement::Store{local, address:_ }
                | Statement::Call{ local, function:_, parameters:_ } => { 
                    if !args.contains(local) {
                        locals.insert(local.clone()); 
                    }
                },
                Statement::Return{ value:_ } => {},
                Statement::If{ predicate:_, when_true:ss } => {
                    for s in ss {
                        add_locals(s, args, locals);
                    }
                },
            }
        };

        for s in body.iter() {
            add_locals(s, &args, &mut locals);
        }

        Function { name, args, locals, body }
    }

    /*

    stack:

    SP ->   local 3
            local 2
            local 1
            return address
            arg 2
            arg 1
            RESULT
    */

    fn emit(&self) -> FunctionContext {
        let mut ctxt = FunctionContext {
            stack: BTreeMap::new(),
            lines: Vec::new(),
            additional_offset: 0,
            regs_touched: BTreeSet::new(),
            block_counter: 0,
        };
        ctxt.lines.push(Line::Comment(format!("# Function: {}", &self.name)));
        ctxt.lines.push(Line::Label(format!(":{}", &self.name)));

        let max_register_locals = 0;

        let register_local_count = std::cmp::min(max_register_locals, self.locals.len());
        let stack_local_count = self.locals.len() - register_local_count;

        let stack_size = 0
            + 1 // result
            + self.args.len()
            + 1 // return address
            + stack_local_count;
        let mut offset = (stack_size - 1) as isize;

        ctxt.lines.push(Line::Comment(format!("# sp+{} -> {}", offset, RESULT)));
        ctxt.stack.insert(RESULT.to_owned(), LocalStorage::Stack(offset as usize));
        offset -= 1;

        for arg in &self.args {
            ctxt.lines.push(Line::Comment(format!("# sp+{} -> {}", offset, arg)));
            ctxt.stack.insert(arg.clone(), LocalStorage::Stack(offset as usize));
            offset -= 1;
        }

        ctxt.lines.push(Line::Comment(format!("# sp+{} -> {}", offset, "RETURN_ADDRESS")));
        ctxt.stack.insert("RETURN_ADDRESS".to_owned(), LocalStorage::Stack(offset as usize));
        offset -= 1;

        // offset -= register_local_count as isize;

        for (count, l) in self.locals.iter().enumerate() {
            let storage = match count {
                count if count < register_local_count => {
                    unimplemented!();
                    // let reg = if count == 0 { Reg::D } else { Reg::E };
                    // LocalStorage::Register(reg)
                },
                _ => {
                    let s = LocalStorage::Stack(offset as usize);
                    offset -= 1;
                    s
                }
            };

            ctxt.lines.push(Line::Comment(format!("# {:?} -> {}", storage, l)));
            ctxt.stack.insert(l.clone(), storage);
        }

        assert_eq!(-1, offset);

        // assert_eq!(ctxt.regs_used.len(), register_local_count);
        // if register_local_count > 0 {
        //     ctxt.lines.push(Line::Comment(format!("save regs: {:?}", ctxt.regs_used)));
        //     let regs : Vec<Reg> = ctxt.regs_used.iter().cloned().collect();
        //     for r in regs {
        //         ctxt.add_macro(format!("push {}", r));
        //     }
        // }

        if stack_local_count > 0 {
            ctxt.lines.push(Line::Comment("create stack space".to_owned()));
            ctxt.add_inst(Instruction::Alloc(StackOffset::new(stack_local_count as u8)));
        }

        // let mut count = 0;
        for stmt in self.body.iter() {
            // let scope = format!("_function{}_", count);
            stmt.emit(&mut ctxt, &self.name);
            // count += 1;
        }
         
        ctxt.lines.push(Line::Label(format!(":{}__{}", &self.name, EPILOGUE)));
        if stack_local_count > 0 {
            ctxt.add_inst(Instruction::Discard(StackOffset::new(stack_local_count as u8)));
        }

        // if register_local_count > 0 {
        //     ctxt.lines.push(Line::Comment(format!("save regs: {:?}", ctxt.regs_used)));
        //     let regs : Vec<Reg> = ctxt.regs_used.iter().cloned().rev().collect();
        //     for r in regs {
        //         ctxt.add_macro(format!("pop {}", r));
        //     }
        // }

        ctxt.add_macro(format!("ret"));

        ctxt
    }
}


fn main() -> Result<(), std::io::Error> {
    let input = {
        let mut s = String::new();
        let stdin = io::stdin();
        stdin.lock().read_to_string(&mut s)?;
        s
    };

    let mut functions = BTreeMap::new();

    let mut program = ProgramParser::parse(Rule::program, &input).unwrap();
    let pairs = program.next().unwrap().into_inner();
    for pair in pairs {
        match pair.as_rule() {
            Rule::function => {
                let f = Function::parse(pair);
                functions.insert(f.name.clone(), f);
            },
            Rule::EOI => { },
            _ => {
                panic!("Unexpected rule: {:?}", pair);
            }
        }
    }

    let main = functions.get("main");
    if main.is_none() {
        println!("main not found!");
        return Err(std::io::Error::from(ErrorKind::NotFound));
    }

    let mut program = Vec::new();

    program.push(Line::Comment(format!("call main")));
    program.push(Line::Instruction(Instruction::WithPush(PushableInstruction::Not(StackOffset::top()))));
    program.push(Line::parse(format!("call :main")));
    program.push(Line::Instruction(Instruction::WithoutPush(PushableInstruction::LoadFromStack(StackOffset::top()))));
    program.push(Line::parse(format!("halt")));

    for f in &functions {
        program.push(Line::Comment(format!("{:?}", &f.1)));
        let f = f.1.emit();
        for l in f.lines {
            program.push(l);
        }
    }

    let rom = assemble(program);

    simulate(&rom, 10000000);

    Ok(())
}
