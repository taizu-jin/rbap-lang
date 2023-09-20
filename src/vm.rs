#![allow(dead_code)]

use crate::{
    code::*,
    compiler::Bytecode,
    error::{Error, Result, VMError},
    object::{CompiledFunction, Object},
};

pub const STACK_SIZE: usize = 2048;
pub const GLOBAL_SIZE: usize = 65536;
pub const MAX_FRAMES: usize = 1024;

struct Frame {
    func: CompiledFunction,
    ip: usize,
    base_pointer: usize,
}

impl Frame {
    fn new(func: CompiledFunction, base_pointer: usize) -> Self {
        Self {
            func,
            ip: 0,
            base_pointer,
        }
    }

    fn instructions(&self) -> &[u8] {
        self.func.instructions.as_ref()
    }
}

pub struct VM {
    constants: Vec<Object>,
    stack: Vec<Object>,
    globals: Box<[Object; GLOBAL_SIZE]>,
    frames: Vec<Frame>,

    last_popped: Option<Object>,
}

impl VM {
    pub fn new(bytecode: Bytecode) -> Self {
        let Bytecode {
            instructions,
            constants,
        } = bytecode;

        let main_fn = CompiledFunction {
            instructions,
            num_locals: 0,
            num_parameters: 0,
        };
        let main_frame = Frame::new(main_fn, 0);

        let mut frames = Vec::with_capacity(MAX_FRAMES);
        frames.push(main_frame);

        Self {
            constants,
            stack: Vec::with_capacity(STACK_SIZE),
            globals: vec![Object::Null; GLOBAL_SIZE].try_into().unwrap(),
            frames,
            last_popped: None,
        }
    }

    pub fn run(&mut self) -> Result<()> {
        while self.current_frame().ip < self.current_frame().instructions().len() {
            let ip = self.current_frame().ip;
            let ins = self.current_frame().instructions();

            let opcode = Opcode::lookup(ins[ip])?;

            match opcode {
                &OP_CONSTANT => {
                    let slice: [u8; 2] = ins[ip + 1..=ip + 2].try_into().unwrap();
                    let const_index = u16::from_be_bytes(slice) as usize;
                    self.current_frame_mut().ip += 2;
                    let constant = self.get_constant(const_index)?;

                    self.push(constant)?;
                }
                &OP_SET_GLOBAL => {
                    let slice: [u8; 2] = ins[ip + 1..=ip + 2].try_into().unwrap();
                    let global_index = u16::from_be_bytes(slice) as usize;
                    self.current_frame_mut().ip += 2;
                    self.globals[global_index] = self.pop()?;
                }
                &OP_GET_GLOBAL => {
                    let slice: [u8; 2] = ins[ip + 1..=ip + 2].try_into().unwrap();
                    let global_index = u16::from_be_bytes(slice) as usize;
                    self.current_frame_mut().ip += 2;
                    self.push(self.globals[global_index].clone())?;
                }
                &OP_POP => {
                    self.pop()?;
                }
                opcode if matches!(opcode, &OP_ADD | &OP_SUB | &OP_MUL | &OP_DIV) => {
                    self.execute_binary_intiger_operation(opcode)?
                }
                opcode if matches!(opcode, &OP_AND | &OP_OR) => {
                    self.execute_binary_bool_operation(opcode)?
                }
                &OP_MINUS => self.execute_minus_operator()?,
                &OP_TRUE => self.push(Object::Bool(true))?,
                &OP_FALSE => self.push(Object::Bool(false))?,
                &OP_NOT => self.execute_not_operator()?,
                opcode if matches!(opcode, &OP_EQUAL | &OP_NOT_EQUAL | &OP_GREATER_THAN) => {
                    self.execute_comparison(opcode)?
                }
                opcode => unimplemented!("handling for {} not implemented", opcode),
            }

            self.current_frame_mut().ip += 1;
        }

        Ok(())
    }
    fn current_frame(&self) -> &Frame {
        self.frames
            .last()
            .expect("At least one frame should always be present")
    }

    fn current_frame_mut(&mut self) -> &mut Frame {
        self.frames
            .last_mut()
            .expect("At least one frame should always be present")
    }

    fn get_constant(&self, index: usize) -> Result<Object> {
        self.constants
            .get(index)
            .ok_or_else(|| Error::from(VMError::ConstantIndexOutOfBounds(index)))
            .cloned()
    }

    fn push(&mut self, object: Object) -> Result<()> {
        if self.stack.len() + 1 > STACK_SIZE {
            return Err(VMError::StackOverflow.into());
        }

        self.stack.push(object);

        Ok(())
    }

    fn pop(&mut self) -> Result<Object> {
        let object = self
            .stack
            .pop()
            .ok_or_else(|| Error::from(VMError::StackEmpty))?;

        self.last_popped = Some(object.clone());
        Ok(object)
    }

    fn execute_binary_intiger_operation(&mut self, opcode: &Opcode) -> Result<()> {
        let right = self.pop()?;
        let left = self.pop()?;

        match (left, right) {
            (Object::Int(left), Object::Int(right)) => {
                let result = match *opcode {
                    OP_ADD => left + right,
                    OP_SUB => left - right,
                    OP_MUL => left * right,
                    OP_DIV => left / right,
                    _ => unreachable!("should be unreacheable due to the callee check"),
                };

                self.push(Object::Int(result))
            }
            (left, right) => Err(VMError::UnsupportedTypes(left.into(), right.into()).into()),
        }
    }

    fn execute_minus_operator(&mut self) -> Result<()> {
        let operand = self.pop()?;

        let value = if let Object::Int(value) = operand {
            value
        } else {
            return Err(Error::from(VMError::UnsupportedTypeForNegation(
                operand.into(),
            )));
        };

        self.push(Object::Int(-value))
    }

    fn execute_comparison(&mut self, opcode: &Opcode) -> Result<()> {
        let right = self.pop()?;
        let left = self.pop()?;

        match *opcode {
            OP_EQUAL => self.push(Object::Bool(right == left)),
            OP_NOT_EQUAL => self.push(Object::Bool(right != left)),
            OP_GREATER_THAN => self.push(Object::Bool(left > right)),
            _ => unreachable!("should be unreacheable due to the callee check"),
        }
    }

    fn execute_not_operator(&mut self) -> Result<()> {
        let operand = self.pop()?;

        let value = if let Object::Bool(value) = operand {
            value
        } else {
            return Err(Error::from(VMError::UnsupportedTypeForNegation(
                operand.into(),
            )));
        };

        self.push(Object::Bool(!value))
    }

    fn execute_binary_bool_operation(&mut self, opcode: &Opcode) -> Result<()> {
        let right = self.pop()?;
        let left = self.pop()?;

        match (left, right) {
            (Object::Bool(left), Object::Bool(right)) => {
                let result = match *opcode {
                    OP_AND => left & right,
                    OP_OR => left | right,
                    _ => unreachable!("should be unreacheable due to the callee check"),
                };

                self.push(Object::Bool(result))
            }
            (left, right) => Err(VMError::UnsupportedTypes(left.into(), right.into()).into()),
        }
    }

    pub fn last_popped_stack_elem(&self) -> Option<Object> {
        self.last_popped.clone()
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::{Debug, Display};

    use crate::{
        ast::Program, compiler::Compiler, error::Result, lexer::Lexer, object::Object,
        parser::Parser,
    };

    use super::*;

    trait GetInput {
        fn input(&self) -> &'static str;
    }

    trait GetErrorMessage {
        fn message<T: Display>(&self, got: &T) -> String;
    }

    #[derive(Debug)]
    struct TestCaseInt {
        input: &'static str,
        expected: i64,
    }

    impl GetInput for TestCaseInt {
        fn input(&self) -> &'static str {
            self.input
        }
    }

    impl GetErrorMessage for TestCaseInt {
        fn message<T: Display>(&self, expected: &T) -> String {
            format!("expected {}, got {}", self.expected, expected)
        }
    }

    impl PartialEq<Object> for TestCaseInt {
        fn eq(&self, other: &Object) -> bool {
            match other {
                Object::Int(int) => *int == self.expected,
                _ => false,
            }
        }
    }

    macro_rules! def_case_int {
        ($($input:literal, $integer:literal),*) => {
            vec![$(
            TestCaseInt {
                input: $input,
                expected: $integer,
            }),*
            ]
        };
    }

    #[derive(Debug)]
    struct TestCaseBool {
        input: &'static str,
        expected: bool,
    }

    impl GetInput for TestCaseBool {
        fn input(&self) -> &'static str {
            self.input
        }
    }

    impl GetErrorMessage for TestCaseBool {
        fn message<T: Display>(&self, expected: &T) -> String {
            format!("expected {}, got {}", self.expected, expected)
        }
    }

    impl PartialEq<Object> for TestCaseBool {
        fn eq(&self, other: &Object) -> bool {
            match other {
                Object::Bool(b) => *b == self.expected,
                _ => false,
            }
        }
    }

    macro_rules! def_case_bool {
        ($($input:literal, $integer:literal),*) => {
            vec![$(
            TestCaseBool {
                input: $input,
                expected: $integer,
            }),*
            ]
        };
    }

    #[test]
    fn test_integer_arithmetic() -> Result<()> {
        let tests = def_case_int!(
            "1.",
            1,
            "2.",
            2,
            "1 + 2.",
            3,
            "1 - 2.",
            -1,
            "1 * 2.",
            2,
            "4 / 2.",
            2,
            "50 / 2 * 2 + 10 - 5.",
            55,
            "5 * (2 + 10).",
            60,
            "-5.",
            -5,
            "-10.",
            -10
        );

        run_vm_tests(tests)
    }

    fn run_vm_tests<T>(tests: Vec<T>) -> Result<()>
    where
        T: PartialEq<Object> + GetInput + GetErrorMessage + Debug,
    {
        for test in tests {
            let program = parse(test.input().into());

            let mut compiler = Compiler::new();
            compiler.compile(program)?;

            let mut vm = VM::new(compiler.bytecode());
            vm.run()?;

            let stack_element = vm.last_popped_stack_elem().unwrap();

            assert_eq!(test, stack_element, "{}", test.message(&stack_element))
        }

        Ok(())
    }

    fn parse(input: String) -> Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer.iter());
        parser.parse()
    }

    #[test]
    fn test_boolean_expressions() -> Result<()> {
        let tests = def_case_bool!(
            "rbap_true.",
            true,
            "rbap_false.",
            false,
            "1 < 2.",
            true,
            "1 > 2.",
            false,
            "1 < 1.",
            false,
            "1 > 1.",
            false,
            "1 == 1.",
            true,
            "1 <> 1.",
            false,
            "1 == 2.",
            false,
            "1 <> 2.",
            true,
            "rbap_true == rbap_true.",
            true,
            "rbap_false == rbap_false.",
            true,
            "rbap_true == rbap_false.",
            false,
            "rbap_false == rbap_true.",
            false,
            "rbap_true <> rbap_true.",
            false,
            "rbap_false <> rbap_true.",
            true,
            "rbap_true <> rbap_false.",
            true,
            "(1 < 2) == rbap_true.",
            true,
            "(1 < 2) == rbap_false.",
            false,
            "(1 > 2) == rbap_true.",
            false,
            "(1 > 2) == rbap_false.",
            true,
            "NOT rbap_true.",
            false,
            "NOT rbap_false.",
            true,
            "NOT NOT rbap_true.",
            true,
            "NOT NOT rbap_false.",
            false
        );

        run_vm_tests(tests)
    }

    #[test]
    fn test_global_assignment_statements() -> Result<()> {
        let tests = def_case_int!(
            "DATA: lv_one TYPE i.
             lv_one = 1.
             lv_one.",
            1,
            "DATA: lv_one TYPE i,
                   lv_two TYPE i.
             lv_one = 1.
             lv_two = 2.
             lv_one + lv_two.",
            3,
            "DATA: lv_one TYPE i,
             lv_two TYPE i.
             lv_one = 1.
             lv_two = lv_one + lv_one.
             lv_one + lv_two.",
            3
        );

        run_vm_tests(tests)
    }
}
