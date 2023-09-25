use std::fmt::Arguments;

use crate::{
    code::*,
    compiler::Bytecode,
    error::{Error, Result, VMError},
    object::{CompiledFunction, Object},
};

pub const STACK_SIZE: usize = 2048;
pub const GLOBAL_SIZE: usize = 65536;
pub const MAX_FRAMES: usize = 1024;

pub trait Writer {
    fn print(&mut self, arguments: Arguments);
}

pub struct StdoutWriter;

impl Writer for StdoutWriter {
    fn print(&mut self, arguments: Arguments) {
        print!("{}", arguments);
    }
}

trait ReadBytes {
    fn read_bytes<W: Writer>(vm: &mut VM<W>) -> Self;
}

impl ReadBytes for u8 {
    fn read_bytes<W: Writer>(vm: &mut VM<W>) -> Self {
        let ip = vm.current_frame().ip as usize;
        let ins = vm.current_frame().instructions();
        let slice: [u8; 1] = ins[ip + 1..=ip + 1].try_into().unwrap();
        let u8 = u8::from_be_bytes(slice);
        vm.current_frame_mut().ip += 1;

        u8
    }
}

impl ReadBytes for u16 {
    fn read_bytes<W: Writer>(vm: &mut VM<W>) -> Self {
        let ip = vm.current_frame().ip as usize;
        let ins = vm.current_frame().instructions();
        let slice: [u8; 2] = ins[ip + 1..=ip + 2].try_into().unwrap();
        let u16 = u16::from_be_bytes(slice);
        vm.current_frame_mut().ip += 2;

        u16
    }
}

struct Frame {
    func: CompiledFunction,
    ip: isize,
    base_pointer: usize,
}

impl Frame {
    fn new(func: CompiledFunction, base_pointer: usize) -> Self {
        Self {
            func,
            ip: -1,
            base_pointer,
        }
    }

    fn instructions(&self) -> &[u8] {
        self.func.instructions.as_ref()
    }
}

pub struct VM<W>
where
    W: Writer,
{
    constants: Vec<Object>,
    stack: Box<[Object; STACK_SIZE]>,
    /// Always points to the next value. Top of the stack is stack\[sp-1\].
    sp: usize,
    globals: Box<[Object; GLOBAL_SIZE]>,
    frames: Vec<Frame>,
    writer: W,
}

impl<W> VM<W>
where
    W: Writer,
{
    pub fn new(bytecode: Bytecode, writer: W) -> Self {
        let Bytecode {
            instructions,
            constants,
        } = bytecode;

        let main_fn = CompiledFunction {
            instructions,
            num_locals: 0,
            num_parameters: 0,
            ty: crate::ast::DataType::None,
        };
        let main_frame = Frame::new(main_fn, 0);

        let mut frames = Vec::with_capacity(MAX_FRAMES);
        frames.push(main_frame);

        Self {
            constants,
            stack: vec![Object::Null; STACK_SIZE].try_into().unwrap(),
            sp: 0,
            globals: vec![Object::Null; GLOBAL_SIZE].try_into().unwrap(),
            frames,
            writer,
        }
    }

    pub fn run(&mut self) -> Result<()> {
        while ((self.current_frame().ip + 1) as usize) < self.current_frame().instructions().len() {
            self.current_frame_mut().ip += 1;
            let opcode = self.current_frame().instructions()[self.current_frame().ip as usize];
            let opcode = Opcode::lookup(opcode)?;

            match opcode {
                &OP_CONSTANT => {
                    let const_index = u16::read_bytes(self) as usize;
                    let constant = self.get_constant(const_index)?;
                    self.push(constant)?;
                }
                &OP_SET_GLOBAL => {
                    let global_index = u16::read_bytes(self) as usize;
                    self.globals[global_index] = self.pop();
                }
                &OP_GET_GLOBAL => {
                    let global_index = u16::read_bytes(self) as usize;
                    self.push(self.globals[global_index].clone())?;
                }
                &OP_POP => {
                    self.pop();
                }
                opcode if matches!(opcode, &OP_ADD | &OP_SUB | &OP_MUL | &OP_DIV) => {
                    self.execute_binary_intiger_operation(opcode)?
                }
                opcode if matches!(opcode, &OP_AND | &OP_OR) => {
                    self.execute_binary_bool_operation(opcode)?
                }
                &OP_STRING_TEMPLATE => {
                    let count = u16::read_bytes(self) as usize;
                    let mut result = String::new();

                    for _ in 0..count {
                        let object = self.pop();

                        match object {
                            Object::String(s) => result = format!("{}{}", s, result),
                            Object::Int(i) => result = format!("{}{}", i, result),
                            Object::Bool(b) => {
                                result = format!(
                                    "{}{}",
                                    if b { "rbap_true" } else { "rbap_false" },
                                    result
                                );
                            }
                            Object::Null => unreachable!("null"),
                            Object::Function(_) => unreachable!("func"),
                        }
                    }
                    self.push(Object::String(result))?;
                }
                &OP_MINUS => self.execute_minus_operator()?,
                &OP_TRUE => self.push(Object::Bool(true))?,
                &OP_FALSE => self.push(Object::Bool(false))?,
                &OP_NOT => self.execute_not_operator()?,
                opcode if matches!(opcode, &OP_EQUAL | &OP_NOT_EQUAL | &OP_GREATER_THAN) => {
                    self.execute_comparison(opcode)?
                }
                &OP_JUMP => {
                    let pos = u16::read_bytes(self) as isize;
                    self.current_frame_mut().ip = pos - 1;
                }
                &OP_JUMP_NOT_TRUTH => {
                    let pos = u16::read_bytes(self) as usize;

                    let condition = if let Object::Bool(condition) = self.pop() {
                        condition
                    } else {
                        unreachable!("there will always be a condition as last element")
                    };

                    if !condition {
                        self.current_frame_mut().ip = (pos - 1) as isize;
                    }
                }
                &OP_CURRENT_FUNCTION => {
                    let current_function = self.current_frame().func.clone();
                    self.push(current_function.into())?;
                }
                &OP_FUNCTION => {
                    let const_index = u16::read_bytes(self) as usize;
                    self.push_function(const_index)?;
                }
                &OP_GET_LOCAL => {
                    let local_index = u8::read_bytes(self) as usize;
                    let base_pointer = self.current_frame().base_pointer;
                    let local = self.stack[base_pointer + local_index].clone();
                    self.push(local)?;
                }
                &OP_SET_LOCAL => {
                    let local_index = u8::read_bytes(self) as usize;
                    let base_pointer = self.current_frame().base_pointer;
                    self.stack[base_pointer + local_index] = self.pop();
                }
                &OP_RETURN => {
                    let frame = self.pop_frame()?;
                    self.sp = frame.base_pointer - 1;
                    self.push(Object::Null)?;
                }
                &OP_RETURN_VALUE => {
                    let return_value = self.pop();
                    let frame = self.pop_frame()?;
                    self.sp = frame.base_pointer - 1;
                    self.push(return_value)?;
                }
                &OP_CALL => {
                    let num_args = u8::read_bytes(self) as usize;
                    self.execute_call(num_args)?;
                }
                &OP_WRITE => {
                    let count = u16::read_bytes(self) as usize;
                    for _ in 0..count {
                        let component = self.pop();
                        self.writer.print(format_args!("{}", component));
                    }
                }
                opcode => unimplemented!("handling for {} not implemented", opcode),
            }
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
        if self.sp >= STACK_SIZE {
            return Err(VMError::StackOverflow.into());
        }

        self.stack[self.sp] = object;
        self.sp += 1;

        Ok(())
    }

    fn pop(&mut self) -> Object {
        let object = self.stack[self.sp - 1].clone();
        self.sp -= 1;

        object
    }

    fn execute_binary_intiger_operation(&mut self, opcode: &Opcode) -> Result<()> {
        let right = self.pop();
        let left = self.pop();

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
        let operand = self.pop();

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
        let right = self.pop();
        let left = self.pop();

        match *opcode {
            OP_EQUAL => self.push(Object::Bool(right == left)),
            OP_NOT_EQUAL => self.push(Object::Bool(right != left)),
            OP_GREATER_THAN => self.push(Object::Bool(left > right)),
            _ => unreachable!("should be unreacheable due to the callee check"),
        }
    }

    fn execute_not_operator(&mut self) -> Result<()> {
        let operand = self.pop();

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
        let right = self.pop();
        let left = self.pop();

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

    fn push_function(&mut self, const_index: usize) -> Result<()> {
        let function = self.get_constant(const_index)?;

        if !matches!(function, Object::Function(..)) {
            return Err(Error::from(VMError::ConstantNotFunction(function)));
        }

        self.push(function)
    }

    fn push_frame(&mut self, frame: Frame) -> Result<()> {
        if self.frames.len() + 1 > MAX_FRAMES {
            return Err(VMError::FrameOverflow.into());
        }

        self.frames.push(frame);

        Ok(())
    }

    fn pop_frame(&mut self) -> Result<Frame> {
        self.frames
            .pop()
            .ok_or_else(|| Error::from(VMError::FramesEmpty))
    }

    fn execute_call(&mut self, num_args: usize) -> Result<()> {
        let callee = self.stack[self.sp - 1 - num_args].clone();

        if let Object::Function(f) = callee {
            self.call_function(f, num_args)?;
        } else {
            return Err(VMError::NonFunctionCall.into());
        }

        Ok(())
    }

    fn call_function(&mut self, func: CompiledFunction, num_args: usize) -> Result<()> {
        if num_args != func.num_parameters {
            return Err(VMError::WrongArgumentCount {
                want: func.num_parameters,
                got: num_args,
            }
            .into());
        }

        let num_locals = func.num_locals;
        let frame = Frame::new(func, self.sp - num_args);
        self.sp = frame.base_pointer + num_locals;
        self.push_frame(frame)
    }

    pub fn last_popped_stack_elem(&self) -> Object {
        self.stack[self.sp].clone()
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

    #[derive(Default)]
    struct DummyWriter(Vec<String>);

    impl Writer for DummyWriter {
        fn print(&mut self, arguments: Arguments) {
            self.0.push(arguments.to_string())
        }
    }

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
        fn message<T: Display>(&self, got: &T) -> String {
            format!("expected {}, got {}", self.expected, got)
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
        fn message<T: Display>(&self, got: &T) -> String {
            format!("expected {}, got {}", self.expected, got)
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
        ($($input:literal, $bool:literal),*) => {
            vec![$(
            TestCaseBool {
                input: $input,
                expected: $bool,
            }),*
            ]
        };
    }

    #[derive(Debug)]
    struct TestCaseString {
        input: &'static str,
        expected: &'static str,
    }

    impl GetInput for TestCaseString {
        fn input(&self) -> &'static str {
            self.input
        }
    }

    impl GetErrorMessage for TestCaseString {
        fn message<T: Display>(&self, got: &T) -> String {
            format!("expected {}, got {}", self.expected, got)
        }
    }

    impl PartialEq<Object> for TestCaseString {
        fn eq(&self, other: &Object) -> bool {
            match other {
                Object::String(s) => *s == self.expected,
                _ => false,
            }
        }
    }

    macro_rules! def_case_str {
        ($($input:literal, $string:literal),*) => {
            vec![$(
            TestCaseString {
                input: $input,
                expected: $string,
            }),*
            ]
        };
    }

    #[derive(Debug)]
    struct TestCaseNull {
        input: &'static str,
    }

    impl GetInput for TestCaseNull {
        fn input(&self) -> &'static str {
            self.input
        }
    }

    impl GetErrorMessage for TestCaseNull {
        fn message<T: Display>(&self, got: &T) -> String {
            format!("expected {}, got {}", Object::Null, got)
        }
    }

    impl PartialEq<Object> for TestCaseNull {
        fn eq(&self, other: &Object) -> bool {
            other == &Object::Null
        }
    }

    macro_rules! def_case_null {
        ($($input:literal),*) => {
            vec![$(
            TestCaseNull {
                input: $input,
            }),*
            ]
        };
    }

    #[derive(Debug)]
    struct TestCaseWriter {
        input: &'static str,
        expected_writer_content: Vec<String>,
    }

    macro_rules! def_case_writer {
        ($($input:literal, $($expected:literal),*);*) => {
            vec![$(
            TestCaseWriter {
                input: $input,
                expected_writer_content: vec![$($expected.to_string()),*]
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

            let mut vm = VM::new(compiler.bytecode(), DummyWriter::default());
            vm.run()?;

            let stack_element = vm.last_popped_stack_elem();

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
            false,
            "rbap_true AND rbap_true.",
            true,
            "rbap_false AND rbap_false.",
            false,
            "rbap_true AND rbap_false.",
            false,
            "rbap_true OR rbap_true.",
            true,
            "rbap_false OR rbap_false.",
            false,
            "rbap_true OR rbap_false.",
            true
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

    #[test]
    fn test_string_expressions() -> Result<()> {
        let tests = def_case_str!(
            "'rbap lang'.",
            "rbap lang",
            "|some { 'string' } template|.",
            "some string template",
            "|one { 2 } three|.",
            "one 2 three",
            "|is it { rbap_true }|.",
            "is it rbap_true",
            "|is it { rbap_false }|.",
            "is it rbap_false"
        );

        run_vm_tests(tests)
    }

    #[test]
    fn test_conditionals() -> Result<()> {
        let tests = def_case_int!(
            "IF rbap_true. 10. ENDIF.",
            10,
            "IF rbap_true. 10. ELSE. 20. ENDIF.",
            10,
            "IF rbap_false. 10. ELSE. 20. ENDIF.",
            20,
            "IF 1 < 2. 10. ENDIF.",
            10,
            "IF 1 < 2. 10. ELSE. 20. ENDIF.",
            10,
            "IF 1 > 2. 10. ELSE. 20. ENDIF.",
            20
        );

        run_vm_tests(tests)
    }

    #[test]
    fn test_conditionals_without_bock() -> Result<()> {
        let tests = def_case_bool!(
            "IF rbap_false. 10. ENDIF.",
            false,
            "IF rbap_true. ELSE. 20. ENDIF.",
            true,
            "IF rbap_true. ELSE. ENDIF.",
            true
        );

        run_vm_tests(tests)
    }

    #[test]
    fn test_functions_without_return_value() -> Result<()> {
        let tests = def_case_null!("METHOD no_return. ENDMETHOD. no_return().");

        run_vm_tests(tests)
    }

    #[test]
    fn test_functions() -> Result<()> {
        let tests = def_case_int!(
            "METHOD sum_5_10 RETURNING rv_sum TYPE i. rv_sum = 5 + 10. ENDMETHOD. sum_5_10().",
            15,
            "METHOD one RETURNING rv_one TYPE i. rv_one = 1. ENDMETHOD.
             METHOD two RETURNING rv_two TYPE i. rv_two = 2. ENDMETHOD.
            one() + two().",
            3,
            "METHOD a RETURNING rv_a TYPE i. rv_a = 1. ENDMETHOD.
             METHOD b RETURNING rv_b TYPE i. rv_b = a() + 1. ENDMETHOD.
             METHOD c RETURNING rv_c TYPE i. rv_c = b() + 1. ENDMETHOD.
            c().",
            3
        );

        run_vm_tests(tests)
    }

    #[test]
    fn test_functions_with_bindings() -> Result<()> {
        let tests = def_case_int!(
            "METHOD one RETURNING rv_one TYPE i.
                DATA: lv_one TYPE i.

                lv_one = 1.
                rv_one = lv_one.
             ENDMETHOD.

             one().",
            1,
            "METHOD one_and_two RETURNING rv_result TYPE i.
                DATA: lv_one TYPE i,
                      lv_two TYPE i.

                lv_one = 1.
                lv_two = 2.
                rv_result = lv_one + lv_two.
             ENDMETHOD.

             one_and_two().",
            3,
            "DATA: gv_global_seed TYPE i.

             gv_global_seed = 50.
             
             METHOD minus_one RETURNING rv_result TYPE i.
                DATA: lv_num TYPE i.

                lv_num = 1.
                rv_result = gv_global_seed - lv_num.
             ENDMETHOD.

             METHOD minus_two RETURNING rv_result TYPE i.
                DATA: lv_num TYPE i.

                lv_num = 2.
                rv_result = gv_global_seed - lv_num.
             ENDMETHOD.

             minus_one() + minus_two().",
            97
        );

        run_vm_tests(tests)
    }

    #[test]
    fn test_functions_with_arguments_and_bindings() -> Result<()> {
        let tests = def_case_int!(
            "METHOD identity IMPORTING iv_id TYPE i RETURNING rv_id TYPE i.
                rv_id = iv_id.
             ENDMETHOD.

             identity(4).",
            4,
            "METHOD sum IMPORTING iv_a TYPE i
                                  iv_b TYPE i 
                        RETURNING rv_sum TYPE i.
                rv_sum = iv_a + iv_b.
             ENDMETHOD.

             sum(1, 2).",
            3,
            "METHOD sum IMPORTING iv_a TYPE i
                                  iv_b TYPE i 
                        RETURNING rv_sum TYPE i.
                DATA: lv_sum TYPE i.
                lv_sum = iv_a + iv_b.
                rv_sum = lv_sum.
             ENDMETHOD.

             sum(1, 2) + sum(3, 4).",
            10,
            "METHOD sum IMPORTING iv_a TYPE i
                                  iv_b TYPE i 
                        RETURNING rv_sum TYPE i.
                DATA: lv_sum TYPE i.
                lv_sum = iv_a + iv_b.
                rv_sum = lv_sum.
             ENDMETHOD.

             METHOD outer RETURNING rv_result TYPE i.
                rv_result = sum(1, 2) + sum(3, 4).
             ENDMETHOD.

             outer().",
            10,
            "DATA gv_num TYPE i.
             gv_num = 10.

             METHOD sum IMPORTING iv_a TYPE i
                                  iv_b TYPE i 
                        RETURNING rv_sum TYPE i.
                DATA: lv_sum TYPE i.
                lv_sum = iv_a + iv_b.
                rv_sum = lv_sum + gv_num.
             ENDMETHOD.

             METHOD outer RETURNING rv_result TYPE i.
                rv_result = sum(1, 2) + sum(3, 4) + gv_num.
             ENDMETHOD.

             outer() + gv_num.",
            50
        );

        run_vm_tests(tests)
    }

    #[test]
    fn test_recursive_functions() -> Result<()> {
        let tests = def_case_int!(
            "METHOD count_down IMPORTING iv_x TYPE i RETURNING rv_result TYPE i.
                IF iv_x == 0.
                    rv_result = 0.
                ELSE.
                    rv_result = count_down(iv_x - 1).
                ENDIF.
             ENDMETHOD.

             count_down(1).",
            0
        );

        run_vm_tests(tests)
    }

    #[test]
    fn test_recursive_fibonacci() -> Result<()> {
        let tests = def_case_int!(
            "METHOD fibonacci IMPORTING iv_x TYPE i RETURNING rv_result TYPE i.
                IF iv_x == 0.
                    rv_result = 0.
                ELSE.
                    IF iv_x == 1.
                        rv_result = 1.
                    ELSE.
                        rv_result = fibonacci(iv_x - 1) + fibonacci(iv_x - 2).
                    ENDIF.
                ENDIF.
             ENDMETHOD.

             fibonacci(15).",
            610
        );

        run_vm_tests(tests)
    }

    #[test]
    fn test_write_statement() -> Result<()> {
        let tests = def_case_writer!(
            "WRITE: 'some string'.", "some string";
            "WRITE: / 'some', / 'string'.", "\n", "some", "\n", "string");

        for test in tests {
            let program = parse(test.input.into());

            let mut compiler = Compiler::new();
            compiler.compile(program)?;

            let mut vm = VM::new(compiler.bytecode(), DummyWriter::default());
            vm.run()?;

            assert_eq!(
                vm.writer.0.len(),
                test.expected_writer_content.len(),
                "printed expression count does not match. got={} want={}",
                vm.writer.0.len(),
                test.expected_writer_content.len()
            );

            for (got, want) in vm.writer.0.iter().zip(test.expected_writer_content.iter()) {
                assert_eq!(
                    got, want,
                    "printed component does not match expected value.\n\tgot={:?}\n\twant={:?}",
                    got, want
                );
            }
        }

        Ok(())
    }
}
