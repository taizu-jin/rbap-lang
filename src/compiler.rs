mod symbol_table;

use std::fs::{read_to_string, File};
use std::path::Path;

use serde::{Deserialize, Serialize};

use crate::ast::{DataType, Expression, Operator, Statement};
use crate::code::*;
use crate::error::{CompilerError, Error, ParseInfixError, ParsePrefixError, Result};
use crate::lexer::Lexer;
use crate::object::CompiledFunction;
use crate::parser::Parser;
use crate::{ast::Node, code::Instructions, object::Object};

pub use self::symbol_table::SymbolTable;
use self::symbol_table::{Scope, Symbol};

#[derive(Serialize, Deserialize)]
pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize, PartialEq)]
struct EmittedInstruction {
    opcode: u8,
    _position: usize,
}

impl PartialEq<Opcode> for EmittedInstruction {
    fn eq(&self, other: &Opcode) -> bool {
        self.opcode == other.code
    }
}

#[derive(Debug, Default, Serialize, Deserialize, PartialEq)]
struct CompilationScope {
    instructions: Instructions,
    last_instruction: EmittedInstruction,
    prev_instruction: EmittedInstruction,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct Compiler {
    constants: Vec<Object>,
    symbol_table: SymbolTable,

    scopes: Vec<CompilationScope>,
}

impl Compiler {
    pub(crate) fn new() -> Self {
        Self::with_state(Vec::new(), SymbolTable::new())
    }

    pub(crate) fn with_state(constants: Vec<Object>, symbol_table: SymbolTable) -> Self {
        let main_scope = CompilationScope::default();

        Self {
            constants,
            symbol_table,
            scopes: vec![main_scope],
        }
    }

    /// Consumes self and returns a tuple of constants and symbol table.
    pub(crate) fn consume(self) -> (Vec<Object>, SymbolTable) {
        (self.constants, self.symbol_table)
    }

    pub(crate) fn bytecode(&mut self) -> Bytecode {
        Bytecode {
            instructions: self
                .scopes
                .pop()
                .expect("at least one scope is always initialized upon creation")
                .instructions,
            constants: self.constants.clone(),
        }
    }

    pub fn compile<P>(src: P) -> Result<Compiler>
    where
        P: AsRef<Path>,
    {
        let input = read_to_string(src).map_err(Error::from)?;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer.iter());
        let program = parser.parse();

        let constants = Vec::new();
        let symbol_table = SymbolTable::new();
        let mut compiler = Compiler::with_state(constants, symbol_table);

        compiler.compile_node(program)?;

        Ok(compiler)
    }

    pub fn into_file<P>(self, path: P) -> Result<()>
    where
        P: AsRef<Path>,
    {
        let file = File::options()
            .write(true)
            .create(true)
            .truncate(true)
            .open(path)
            .map_err(Error::from)?;

        bincode::serialize_into(&file, &self)?;

        Ok(())
    }

    pub fn from_file<P>(path: P) -> Result<Self>
    where
        P: AsRef<Path>,
    {
        let file = File::options().read(true).open(path).map_err(Error::from)?;
        Ok(bincode::deserialize_from(file)?)
    }

    pub(crate) fn compile_node(&mut self, node: impl Into<Node>) -> Result<()> {
        let node = node.into();

        match node {
            Node::Program(p) => {
                for s in p.statements {
                    self.compile_node(s)?;
                }
            }
            Node::Statement(s) => match s {
                Statement::Expression(e) => {
                    self.compile_node(e)?;
                    self.emit(OP_POP, &[]);
                }
                Statement::Declaration(d) => {
                    for declaration in d.as_ref() {
                        self.symbol_table
                            .define(declaration.ident.to_string(), declaration.ty);
                    }
                }
                Statement::Write(w) => {
                    let expressions: Vec<_> = w.into();
                    let count: u16 = expressions
                        .len()
                        .try_into()
                        .expect("max string template bytes count reached");

                    for exp in expressions.into_iter().rev() {
                        self.compile_node(exp)?;
                    }

                    self.emit(OP_WRITE, &[count as i32]);
                }
                Statement::Assignment(a) => {
                    let symbol = self.symbol_table.resolve(a.ident.as_ref())?;
                    let ty = Self::get_type(&a.value, &self.symbol_table)?;

                    if symbol.ty != ty {
                        return Err(CompilerError::from((symbol.ty, ty)).into());
                    }

                    self.compile_node(a.value)?;

                    let index: u8 = symbol.index.try_into().expect("max symbol count reached");

                    if symbol.scope == Scope::Global {
                        self.emit(OP_SET_GLOBAL, &[index as i32]);
                    } else {
                        self.emit(OP_SET_LOCAL, &[index as i32]);
                    }
                }
                Statement::Block(block) => {
                    for statement in block.statements {
                        self.compile_node(statement)?;
                    }
                }
                Statement::If(is) => {
                    self.compile_node(is.condition)?;

                    let jump_not_truth_pos = self.emit(OP_JUMP_NOT_TRUTH, &[9999]);

                    self.compile_node(is.consequence)?;

                    let after_not_truth_pos: u16;

                    if let Some(alternative) = is.alternative {
                        let jump_pos = self.emit(OP_JUMP, &[9999]);

                        after_not_truth_pos = self
                            .current_instructions()
                            .len()
                            .try_into()
                            .expect("max jump index reached");

                        self.compile_node(alternative)?;

                        let after_pos: u16 = self
                            .current_instructions()
                            .len()
                            .try_into()
                            .expect("max jump index reached");
                        self.change_operand(jump_pos, after_pos as i32)?;
                    } else {
                        after_not_truth_pos = self
                            .current_instructions()
                            .len()
                            .try_into()
                            .expect("max jump index reached");
                    }

                    self.change_operand(jump_not_truth_pos, after_not_truth_pos as i32)?;
                }
                Statement::Function(f) => {
                    let symbol = self.symbol_table.define(
                        f.name.clone(),
                        f.ret.as_ref().map_or(DataType::None, |r| r.ty),
                    );

                    self.enter_scope();

                    self.symbol_table.define_function_name(
                        f.name,
                        f.ret.as_ref().map_or(DataType::None, |r| r.ty),
                    );

                    let num_parameters = f.parameters.len();

                    for p in f.parameters {
                        self.symbol_table.define(p.ident, p.ty);
                    }

                    let mut ret_ty = DataType::None;

                    if let Some(ret) = &f.ret {
                        ret_ty = ret.ty;
                        self.symbol_table.define(ret.ident.to_owned(), ret.ty);
                    }

                    self.compile_node(f.body)?;

                    if let Some(ret) = f.ret {
                        let symbol = self.symbol_table.resolve(&ret.ident)?;
                        self.load_symbol(symbol);
                        self.emit(OP_RETURN_VALUE, &[]);
                    } else {
                        self.emit(OP_RETURN, &[]);
                    }

                    let num_locals = self.symbol_table.num_definitions;
                    let instructions = self.leave_scope();

                    let compiled_function = CompiledFunction {
                        instructions,
                        num_locals,
                        num_parameters,
                        ty: ret_ty,
                    };

                    let fn_index = self.add_constant(compiled_function.into());

                    self.emit(OP_FUNCTION, &[fn_index]);

                    let index: u8 = symbol.index.try_into().expect("max symbol count reached");
                    if symbol.scope == Scope::Global {
                        self.emit(OP_SET_GLOBAL, &[index as i32]);
                    } else {
                        self.emit(OP_SET_LOCAL, &[index as i32]);
                    }
                }
            },
            Node::Expression(e) => match e {
                Expression::IntLiteral(i) => {
                    let int = Object::Int(i.into());
                    let constant = self.add_constant(int);
                    self.emit(OP_CONSTANT, &[constant]);
                }
                Expression::StringLiteral(s) => {
                    let str = Object::String(s.into());
                    let constant = self.add_constant(str);
                    self.emit(OP_CONSTANT, &[constant]);
                }
                Expression::Ident(id) => {
                    let symbol = self.symbol_table.resolve(id.as_ref())?;

                    self.load_symbol(symbol);
                }
                Expression::BoolLiteral(b) => {
                    if b.into() {
                        self.emit(OP_TRUE, &[]);
                    } else {
                        self.emit(OP_FALSE, &[]);
                    }
                }
                Expression::StringTemplate(st) => {
                    let expressions: Vec<_> = st.into();
                    let count: u16 = expressions
                        .len()
                        .try_into()
                        .expect("max string template bytes count reached");

                    for exp in expressions {
                        self.compile_node(exp)?;
                    }

                    self.emit(OP_STRING_TEMPLATE, &[count as i32]);
                }
                Expression::CallExpression(ce) => {
                    self.compile_node(ce.function)?;
                    let arg_count: u8 = ce
                        .arguments
                        .len()
                        .try_into()
                        .expect("reached max function call argument count");

                    for arg in ce.arguments {
                        self.compile_node(arg)?;
                    }

                    self.emit(OP_CALL, &[arg_count as i32]);
                }
                Expression::InfixExpression(ie) => {
                    Self::check_types(&ie.left, &ie.right, &self.symbol_table)?;

                    let (left, right, operand_op_code) = match ie.operator {
                        Operator::Add => (*ie.left, *ie.right, OP_ADD),
                        Operator::Div => (*ie.left, *ie.right, OP_DIV),
                        Operator::Sub => (*ie.left, *ie.right, OP_SUB),
                        Operator::Mul => (*ie.left, *ie.right, OP_MUL),
                        Operator::GreaterThan => (*ie.left, *ie.right, OP_GREATER_THAN),
                        Operator::LesserThan => (*ie.right, *ie.left, OP_GREATER_THAN),
                        Operator::Equal => (*ie.left, *ie.right, OP_EQUAL),
                        Operator::NotEqual => (*ie.left, *ie.right, OP_NOT_EQUAL),
                        Operator::And => (*ie.left, *ie.right, OP_AND),
                        Operator::Or => (*ie.left, *ie.right, OP_OR),
                        o => return Err(ParseInfixError::UnsupportedOperator(o).into()),
                    };

                    self.compile_node(left)?;
                    self.compile_node(right)?;
                    self.emit(operand_op_code, &[]);
                }
                Expression::PrefixExpression(pe) => {
                    self.compile_node(*pe.right)?;
                    match pe.operator {
                        Operator::Not => self.emit(OP_NOT, &[]),
                        Operator::Sub => self.emit(OP_MINUS, &[]),
                        o => return Err(ParsePrefixError::UnsupportedOperator(o).into()),
                    };
                }
            },
        }

        Ok(())
    }

    fn add_constant(&mut self, obj: Object) -> i32 {
        self.constants.push(obj);
        (self.constants.len() - 1) as i32
    }

    fn emit(&mut self, op: Opcode, operands: &[i32]) -> usize {
        let mut ins = make(&op, operands);
        let pos = self.add_instruction(&mut ins);

        self.set_last_instruction(op, pos);

        pos
    }

    fn add_instruction(&mut self, ins: &mut Vec<u8>) -> usize {
        let pos_new_instructions = self.current_instructions().len();
        self.current_instructions_mut().append(ins);

        pos_new_instructions
    }

    fn current_scope(&self) -> &CompilationScope {
        self.scopes
            .last()
            .expect("at least one scope is always present")
    }

    fn current_scope_mut(&mut self) -> &mut CompilationScope {
        self.scopes
            .last_mut()
            .expect("at least one scope is always present")
    }

    fn current_instructions(&self) -> &[u8] {
        self.current_scope().instructions.as_ref()
    }
    fn current_instructions_mut(&mut self) -> &mut Vec<u8> {
        self.current_scope_mut().instructions.as_mut()
    }

    fn set_last_instruction(&mut self, op: Opcode, pos: usize) {
        let current_scope = &mut self.current_scope_mut();
        let previous = current_scope.last_instruction;
        let last = EmittedInstruction {
            opcode: op.into(),
            _position: pos,
        };

        current_scope.prev_instruction = previous;
        current_scope.last_instruction = last;
    }

    fn load_symbol(&mut self, symbol: Symbol) {
        match symbol.scope {
            Scope::Global => {
                let index: u8 = symbol.index.try_into().expect("max symbol count reached");
                self.emit(OP_GET_GLOBAL, &[index as i32])
            }
            Scope::Local => {
                let index: u8 = symbol.index.try_into().expect("max symbol count reached");
                self.emit(OP_GET_LOCAL, &[index as i32])
            }
            Scope::Function => self.emit(OP_CURRENT_FUNCTION, &[]),
        };
    }

    fn get_type(expression: &Expression, symbol_table: &SymbolTable) -> Result<DataType> {
        let dt = match expression {
            Expression::IntLiteral(_) => DataType::Int,
            Expression::BoolLiteral(_) => DataType::Bool,
            Expression::StringLiteral(_) => DataType::String,
            Expression::StringTemplate(_) => DataType::String,
            Expression::Ident(i) => {
                let symbol = symbol_table.resolve(i.as_ref())?;
                symbol.ty
            }
            Expression::InfixExpression(ie) => {
                let left = Self::get_type(&ie.left, symbol_table)?;
                let right = Self::get_type(&ie.right, symbol_table)?;

                if left != right {
                    return Err(CompilerError::from((left, right)).into());
                }

                if ie.operator.is_boolean() {
                    DataType::Bool
                } else {
                    left
                }
            }
            Expression::PrefixExpression(pe) => Self::get_type(&pe.right, symbol_table)?,
            Expression::CallExpression(ce) => symbol_table.resolve(ce.function.as_ref())?.ty,
        };

        Ok(dt)
    }

    fn check_types(
        left: &Expression,
        right: &Expression,
        symbol_table: &SymbolTable,
    ) -> Result<()> {
        let left = Self::get_type(left, symbol_table)?;
        let right = Self::get_type(right, symbol_table)?;

        if left != right {
            return Err(CompilerError::from((left, right)).into());
        }

        Ok(())
    }

    fn change_operand(&mut self, op_pos: usize, operand: i32) -> Result<()> {
        let op = self
            .current_instructions()
            .as_ref()
            .get(op_pos)
            .ok_or(Error::from(CompilerError::IndexOutOfBounds(op_pos)))?;
        let op = Opcode::lookup(*op)?;

        let new = make(op, &[operand]);
        self.replace_instruction(op_pos, new);

        Ok(())
    }

    fn replace_instruction(&mut self, pos: usize, mut instruction: Vec<u8>) {
        let instructions = self.current_instructions_mut();

        instructions[pos..pos + instruction.len()].swap_with_slice(instruction.as_mut_slice());
    }

    fn enter_scope(&mut self) {
        let scope = CompilationScope {
            ..Default::default()
        };

        self.scopes.push(scope);
        self.symbol_table.enclose();
    }

    fn leave_scope(&mut self) -> Instructions {
        let scope = self
            .scopes
            .pop()
            .expect("at least one scope is always initialized");

        let symbol_table = self
            .symbol_table
            .outer
            .take()
            .expect("should always have an outer scope");

        self.symbol_table = *symbol_table;

        scope.instructions
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Compiler::new()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::Program,
        error::{ErrorKind, Result},
        lexer::Lexer,
        parser::Parser,
    };

    use super::*;

    struct TestCase {
        input: &'static str,
        expected_constants: Vec<Object>,
        expected_instructions: Instructions,
    }

    macro_rules! define_case {
            ($input:expr; $($constants:expr),*; $instructions:expr) => {
                TestCase {
                    input: $input,
                    expected_constants: vec![$($constants), *],
                    expected_instructions: $instructions,
                }
            };

        }

    #[test]
    fn test_string_expressions() -> Result<()> {
        let tests = vec![
            define_case!("'monkey'.";
                         Object::String("monkey".into());
                         [make(&OP_CONSTANT, &[0]),
                          make(&OP_POP, &[])].concat().into()),
            define_case!("|some { 'string' } template|.";
                         Object::String("some ".into()),
                         Object::String("string".into()),
                         Object::String(" template".into());
                         [
                          make(&OP_CONSTANT, &[0]),
                          make(&OP_CONSTANT, &[1]),
                          make(&OP_CONSTANT, &[2]),
                          make(&OP_STRING_TEMPLATE, &[3]),
                          make(&OP_POP, &[])].concat().into()),
            define_case!("|some { |other { 'string' }| } template|.";
                         Object::String("some ".into()),
                         Object::String("other ".into()),
                         Object::String("string".into()),
                         Object::String(" template".into());
                         [
                          make(&OP_CONSTANT, &[0]),
                          make(&OP_CONSTANT, &[1]),
                          make(&OP_CONSTANT, &[2]),
                          make(&OP_STRING_TEMPLATE, &[2]),
                          make(&OP_CONSTANT, &[3]),
                          make(&OP_STRING_TEMPLATE, &[3]),
                          make(&OP_POP, &[])].concat().into()),
        ];

        run_compiler_tests(tests)
    }

    fn run_compiler_tests(tests: Vec<TestCase>) -> Result<()> {
        for test in tests {
            let program = parse(test.input.into());

            let mut compiler = Compiler::new();
            compiler.compile_node(program)?;

            let bytecode = compiler.bytecode();

            test_instructions(&test.expected_instructions, &bytecode.instructions)?;
            test_constants(&test.expected_constants, &bytecode.constants);
        }

        Ok(())
    }

    fn parse(input: String) -> Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer.iter());
        parser.parse()
    }

    fn test_instructions(want: impl AsRef<[u8]>, got: impl AsRef<[u8]>) -> Result<()> {
        let want = want.as_ref();
        let got = got.as_ref();
        assert_eq!(
            want.len(),
            got.len(),
            "wrong instructions length.\nwant={} {:?}\ngot={} {:?}",
            want.len(),
            want,
            got.len(),
            got
        );

        for (i, (want, got)) in want.iter().zip(got.iter()).enumerate() {
            let want = Opcode::lookup(*want)?;
            let got = Opcode::lookup(*got)?;
            assert_eq!(
                want, got,
                "wrong instruction at {}.\nwant={}\ngot={}",
                i, want, got
            );
        }

        Ok(())
    }

    fn test_constants(want: &Vec<Object>, got: &Vec<Object>) {
        assert_eq!(
            want.len(),
            got.len(),
            "wrong constants length.\nwant={}\ngot={}",
            want.len(),
            got.len()
        );

        for (i, (want, got)) in want.iter().zip(got.iter()).enumerate() {
            match (got, want) {
                (Object::Function(got), Object::Function(want)) => assert_eq!(
                    want, got,
                    "wrong constant at {}.\nwant={}\ngot={}",
                    i, want, got
                ),
                _ => assert_eq!(
                    want, got,
                    "wrong constant at {}.\nwant={}\ngot={}",
                    i, want, got
                ),
            };
        }
    }

    #[test]
    fn test_integer_arithmetic() -> Result<()> {
        let tests = vec![
            define_case!("1. 2.";
                         Object::Int(1), Object::Int(2);
                         [make(&OP_CONSTANT, &[0]),
                          make(&OP_POP, &[]),
                          make(&OP_CONSTANT, &[1]),
                          make(&OP_POP, &[])].concat().into()),
            define_case!("1 + 2.";
                     Object::Int(1), Object::Int(2);
                     [
                     make(&OP_CONSTANT, &[0]),
                     make(&OP_CONSTANT, &[1]),
                     make(&OP_ADD, &[]),
                     make(&OP_POP, &[]),
                     ].concat().into()),
            define_case!("1 - 2.";
                     Object::Int(1), Object::Int(2);
                     [
                     make(&OP_CONSTANT, &[0]),
                     make(&OP_CONSTANT, &[1]),
                     make(&OP_SUB, &[]),
                     make(&OP_POP, &[]),
                     ].concat().into()),
            define_case!("1 * 2.";
                     Object::Int(1), Object::Int(2);
                     [
                     make(&OP_CONSTANT, &[0]),
                     make(&OP_CONSTANT, &[1]),
                     make(&OP_MUL, &[]),
                     make(&OP_POP, &[]),
                     ].concat().into()),
            define_case!("2 / 1.";
                     Object::Int(2), Object::Int(1);
                     [
                     make(&OP_CONSTANT, &[0]),
                     make(&OP_CONSTANT, &[1]),
                     make(&OP_DIV, &[]),
                     make(&OP_POP, &[]),
                     ].concat().into()),
            define_case!("-1.";
                     Object::Int(1);
                     [
                     make(&OP_CONSTANT, &[0]),
                     make(&OP_MINUS, &[]),
                     make(&OP_POP, &[]),
                     ].concat().into()),
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_boolean_expressions() -> Result<()> {
        let tests = vec![
            define_case!("rbap_true.";;
                         [make(&OP_TRUE, &[]),
                         make(&OP_POP, &[])].concat().into()),
            define_case!("rbap_false.";;
                         [make(&OP_FALSE, &[]),
                         make(&OP_POP, &[])].concat().into()),
            define_case!("1 > 2.";Object::Int(1), Object::Int(2);
                         [make(&OP_CONSTANT, &[0]),
                         make(&OP_CONSTANT, &[1]),
                         make(&OP_GREATER_THAN, &[]),
                         make(&OP_POP, &[])].concat().into()),
            define_case!("1 < 2.";Object::Int(2), Object::Int(1);
                         [make(&OP_CONSTANT, &[0]),
                         make(&OP_CONSTANT, &[1]),
                         make(&OP_GREATER_THAN, &[]),
                         make(&OP_POP, &[])].concat().into()),
            define_case!("1 == 2.";Object::Int(1), Object::Int(2);
                         [make(&OP_CONSTANT, &[0]),
                         make(&OP_CONSTANT, &[1]),
                         make(&OP_EQUAL, &[]),
                         make(&OP_POP, &[])].concat().into()),
            define_case!("1 <> 2.";Object::Int(1), Object::Int(2);
                         [make(&OP_CONSTANT, &[0]),
                         make(&OP_CONSTANT, &[1]),
                         make(&OP_NOT_EQUAL, &[]),
                         make(&OP_POP, &[])].concat().into()),
            define_case!("rbap_true == rbap_false.";;
                         [make(&OP_TRUE, &[]),
                         make(&OP_FALSE, &[]),
                         make(&OP_EQUAL, &[]),
                         make(&OP_POP, &[])].concat().into()),
            define_case!("rbap_true <> rbap_false.";;
                         [make(&OP_TRUE, &[]),
                         make(&OP_FALSE, &[]),
                         make(&OP_NOT_EQUAL, &[]),
                         make(&OP_POP, &[])].concat().into()),
            define_case!("NOT rbap_true.";;
                         [make(&OP_TRUE, &[]),
                         make(&OP_NOT, &[]),
                         make(&OP_POP, &[])].concat().into()),
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_write_statement() -> Result<()> {
        let tests = vec![
            define_case!("WRITE:/ 'print', / 'a', / 'string'.";
                         Object::String("string".into()),
                         Object::String("\n".into()),
                         Object::String("a".into()),
                         Object::String("\n".into()),
                         Object::String("print".into()),
                         Object::String("\n".into());
                         [
                         make(&OP_CONSTANT, &[0]),
                         make(&OP_CONSTANT, &[1]),
                         make(&OP_CONSTANT, &[2]),
                         make(&OP_CONSTANT, &[3]),
                         make(&OP_CONSTANT, &[4]),
                         make(&OP_CONSTANT, &[5]),
                         make(&OP_WRITE, &[6]),
                         ].concat().into()),
            define_case!("WRITE: 'some string'.";
                         Object::String("some string".into());
                         [
                         make(&OP_CONSTANT, &[0]),
                         make(&OP_WRITE, &[1]),
                         ].concat().into()),
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_conditionals_expressions() -> Result<()> {
        let tests = vec![
            define_case!("IF rbap_true. 10. ENDIF. 20.";
                         Object::Int(10), Object::Int(20);
                         [make(&OP_TRUE, &[]),
                         make(&OP_JUMP_NOT_TRUTH, &[8]),
                         make(&OP_CONSTANT, &[0]),
                         make(&OP_POP, &[]),
                         make(&OP_CONSTANT, &[1]),
                         make(&OP_POP, &[])].concat().into()),
            define_case!("IF rbap_true. 10. ELSE. 15. ENDIF. 20.";
                         Object::Int(10), Object::Int(15), Object::Int(20);
                         [make(&OP_TRUE, &[]),            
                         make(&OP_JUMP_NOT_TRUTH, &[11]),
                         make(&OP_CONSTANT, &[0]),      
                         make(&OP_POP, &[]),           
                         make(&OP_JUMP, &[15]),
                         make(&OP_CONSTANT, &[1]),
                         make(&OP_POP, &[]),
                         make(&OP_CONSTANT, &[2]),
                         make(&OP_POP, &[])].concat().into()),
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_declaration_and_assingment_statements() -> Result<()> {
        let tests = vec![
            define_case!("DATA: lv_one TYPE i,
                                lv_two type i.

                          lv_one = 1.
                          lv_two = 2.";
                         Object::Int(1), Object::Int(2);
                         [
                         make(&OP_CONSTANT, &[0]),
                         make(&OP_SET_GLOBAL, &[0]),
                         make(&OP_CONSTANT, &[1]),
                         make(&OP_SET_GLOBAL, &[1]),
                         ].concat().into()),
            define_case!("DATA: lv_one TYPE i,
                                lv_two type i.

                          lv_one = 1.
                          lv_two = 2.";
                         Object::Int(1), Object::Int(2);
                         [
                         make(&OP_CONSTANT, &[0]),
                         make(&OP_SET_GLOBAL, &[0]),
                         make(&OP_CONSTANT, &[1]),
                         make(&OP_SET_GLOBAL, &[1]),
                         ].concat().into()),
            define_case!("DATA lv_one TYPE i.

                          lv_one = 1.
                          lv_one.";
                         Object::Int(1);
                         [
                         make(&OP_CONSTANT, &[0]),
                         make(&OP_SET_GLOBAL, &[0]),
                         make(&OP_GET_GLOBAL, &[0]),
                         make(&OP_POP, &[]),
                         ].concat().into()),
            define_case!("DATA: lv_one TYPE i,
                                lv_two TYPE i.

                          lv_one = 1.
                          lv_two = lv_one.
                          lv_two.";
                         Object::Int(1);
                         [
                         make(&OP_CONSTANT, &[0]),
                         make(&OP_SET_GLOBAL, &[0]),
                         make(&OP_GET_GLOBAL, &[0]),
                         make(&OP_SET_GLOBAL, &[1]),
                         make(&OP_GET_GLOBAL, &[1]),
                         make(&OP_POP, &[]),
                         ].concat().into()),
            define_case!("DATA: lv_true TYPE rbap_bool.

                          lv_true = rbap_true.
                          lv_true.";;
                         [
                         make(&OP_TRUE, &[]),
                         make(&OP_SET_GLOBAL, &[0]),
                         make(&OP_GET_GLOBAL, &[0]),
                         make(&OP_POP, &[]),
                         ].concat().into()),
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_type_check_throws_an_error() {
        let inputs = vec![
            "DATA: lv_int TYPE i. lv_int = 'string'.",
            "DATA: lv_string TYPE string. lv_string = 1 + 1.",
            "DATA: lv_bool TYPE rbap_bool. lv_bool = -1.",
            "DATA: lv_bool TYPE rbap_bool. lv_bool = |some { 'string' } template|.",
            "DATA: lv_int TYPE i, lv_string TYPE string. lv_int = lv_string.",
            "DATA: lv_bool TYPE rbap_bool.
             METHOD return_string RETURNING rv_string TYPE string.
             ENDMETHOD.
             lv_bool = return_string().",
            "DATA: lv_bool TYPE rbap_bool.
             METHOD return_nothing.
             ENDMETHOD.
             lv_bool = return_nothing().",
        ];

        for input in inputs {
            let program = parse(input.into());

            let mut compiler = Compiler::new();

            match compiler.compile_node(program) {
                Err(e) if e.kind() == ErrorKind::CompilerExpectedDataType => continue,
                Err(e) => panic!("Unexpected error occured: {}", e),
                _ => panic!(
                    "Input should NOT have passed type check.\n\tinput='{}'",
                    input
                ),
            }
        }
    }

    #[test]
    fn test_type_check_string() -> Result<()> {
        let inputs = vec![
            "DATA: lv_string TYPE string. lv_string = 'string'.",
            "DATA: lv_string TYPE string. lv_string = |some { 'string' } template|.",
            "DATA: lv_string TYPE string, lv_some TYPE string. lv_some = 'some'. lv_string = lv_some.",
            "DATA: lv_string TYPE string.
             METHOD return_string RETURNING rv_string TYPE string.
             ENDMETHOD.
             lv_string = return_string().",
        ];
        for input in inputs {
            let program = parse(input.into());

            let mut compiler = Compiler::new();
            compiler.compile_node(program)?;
        }

        Ok(())
    }

    #[test]
    fn test_type_check_bool() -> Result<()> {
        let inputs = vec![
            "DATA: lv_bool TYPE rbap_bool. lv_bool = rbap_true.",
            "DATA: lv_bool TYPE rbap_bool. lv_bool = rbap_false.",
            "DATA: lv_bool TYPE rbap_bool. lv_bool = NOT rbap_true.",
            "DATA: lv_bool TYPE rbap_bool. lv_bool = rbap_true AND rbap_true.",
            "DATA: lv_bool TYPE rbap_bool. lv_bool = rbap_true OR rbap_true.",
            "DATA: lv_bool TYPE rbap_bool. lv_bool = rbap_true OR rbap_true AND rbap_true.",
            "DATA: lv_bool TYPE rbap_bool, lv_true type rbap_bool. lv_bool = lv_true.",
            "DATA: lv_bool TYPE rbap_bool. lv_bool = 1 < 2.",
            "DATA: lv_bool TYPE rbap_bool. lv_bool = 1 > 2.",
            "DATA: lv_bool TYPE rbap_bool. lv_bool = 1 == 2.",
            "DATA: lv_bool TYPE rbap_bool. lv_bool = 1 <> 2.",
            "DATA: lv_bool TYPE rbap_bool.
             METHOD return_bool RETURNING rv_true TYPE rbap_bool.
             ENDMETHOD.
             lv_bool = return_bool().",
        ];
        for input in inputs {
            let program = parse(input.into());

            let mut compiler = Compiler::new();
            compiler.compile_node(program)?;
        }

        Ok(())
    }

    #[test]
    fn test_type_check_int() -> Result<()> {
        let inputs = vec![
            "DATA: lv_int TYPE i. lv_int = 1.",
            "DATA: lv_int TYPE i. lv_int = -1.",
            "DATA: lv_int TYPE i. lv_int = 1 + 2.",
            "DATA: lv_int TYPE i. lv_int = 1 * 2.",
            "DATA: lv_int TYPE i. lv_int = 2 - 1.",
            "DATA: lv_int TYPE i. lv_int = 2 / 1 - 1.",
            "DATA: lv_int TYPE i, lv_one type i. lv_one = 1. lv_int = lv_one.",
            "DATA: lv_int TYPE i.
             METHOD return_int RETURNING rv_int TYPE i.
             ENDMETHOD.
             lv_int = return_int().",
        ];
        for input in inputs {
            let program = parse(input.into());

            let mut compiler = Compiler::new();
            compiler.compile_node(program)?;
        }

        Ok(())
    }

    #[test]
    fn test_compiler_scopes() {
        let mut compiler = Compiler::new();
        assert_eq!(
            compiler.scopes.len(),
            1,
            "scope length is wrong.\n\tgot={}\n\twant={}",
            compiler.scopes.len(),
            1
        );

        compiler.emit(OP_MUL, &[]);

        compiler.enter_scope();

        assert_eq!(
            compiler.scopes.len(),
            2,
            "scope length is wrong.\n\tgot={}\n\twant={}",
            compiler.scopes.len(),
            2
        );

        compiler.emit(OP_SUB, &[]);

        assert_eq!(
            compiler.current_instructions().len(),
            1,
            "instruction length wrong.\n\tgot={}\n\twant={}",
            compiler.current_instructions().len(),
            1
        );

        let last = compiler.current_scope().last_instruction;

        assert_eq!(
            last.opcode,
            OP_SUB.into(),
            "last.op_code wrong.\n\tgot={}\n\twant={}",
            last.opcode,
            OP_SUB
        );

        assert!(
            compiler.symbol_table.outer.is_some(),
            "compiler did not enclose symbol table",
        );

        compiler.leave_scope();

        assert_eq!(
            compiler.scopes.len(),
            1,
            "scope length is wrong.\n\tgot={}\n\twant={}",
            compiler.scopes.len(),
            1
        );

        assert!(
            compiler.symbol_table.outer.is_none(),
            "compiler modified global symbol table incorrectly",
        );

        compiler.emit(OP_ADD, &[]);

        assert_eq!(
            compiler.current_instructions().len(),
            2,
            "instruction length wrong.\n\tgot={}\n\twant={}",
            compiler.current_instructions().len(),
            2
        );

        let last = compiler.current_scope().last_instruction;

        assert_eq!(
            last.opcode,
            OP_ADD.into(),
            "last.op_code wrong.\n\tgot={}\n\twant={}",
            last.opcode,
            OP_ADD
        );

        let previous = compiler.current_scope().prev_instruction;

        assert_eq!(
            previous.opcode,
            OP_MUL.into(),
            "previous.op_code wrong.\n\tgot={}\n\twant={}",
            previous.opcode,
            OP_MUL
        );
    }

    #[test]
    fn test_function_declarations() -> Result<()> {
        let tests = vec![
            define_case!("METHOD sum IMPORTING iv_left TYPE i iv_right TYPE i RETURNING rv_sum TYPE i.
                            rv_sum = 5 + 10.
                          ENDMETHOD.";
                         Object::Int(5), Object::Int(10),
                         Object::Function(CompiledFunction{
                             instructions: [
                                 make(&OP_CONSTANT, &[0]),
                                 make(&OP_CONSTANT, &[1]),
                                 make(&OP_ADD, &[]),
                                 make(&OP_SET_LOCAL, &[2]),
                                 make(&OP_GET_LOCAL, &[2]),
                                 make(&OP_RETURN_VALUE, &[]),
                             ].concat().into(),
                             num_parameters: 2,
                             num_locals: 3,
                             ty: DataType::Int,
                         });
                         [
                         make(&OP_FUNCTION, &[2]),
                         make(&OP_SET_GLOBAL, &[0]),
                         ].concat().into()),
            define_case!("METHOD sum IMPORTING iv_left TYPE i iv_right TYPE i.
                            5 + 10.
                          ENDMETHOD.";
                         Object::Int(5), Object::Int(10),
                         Object::Function(CompiledFunction{
                             instructions: [
                                 make(&OP_CONSTANT, &[0]),
                                 make(&OP_CONSTANT, &[1]),
                                 make(&OP_ADD, &[]),
                                 make(&OP_POP, &[]),
                                 make(&OP_RETURN, &[]),
                             ].concat().into(),
                             num_parameters: 2,
                             num_locals: 2,
                             ty: DataType::None,
                         });
                         [
                         make(&OP_FUNCTION, &[2]),
                         make(&OP_SET_GLOBAL, &[0]),
                         ].concat().into()),
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_function_calls() -> Result<()> {
        let tests = vec![
            define_case!("METHOD sum IMPORTING iv_left TYPE i iv_right TYPE i RETURNING rv_sum TYPE i.
                            rv_sum = 5 + 10.
                          ENDMETHOD.

                          sum(5, 15).";
                         Object::Int(5), Object::Int(10), 
                         Object::Function(CompiledFunction{
                             instructions: [
                                 make(&OP_CONSTANT, &[0]),
                                 make(&OP_CONSTANT, &[1]),
                                 make(&OP_ADD, &[]),
                                 make(&OP_SET_LOCAL, &[2]),
                                 make(&OP_GET_LOCAL, &[2]),
                                 make(&OP_RETURN_VALUE, &[]),
                             ].concat().into(),
                             num_parameters: 2,
                             num_locals: 3,
                             ty: DataType::Int,
                         }),
                         Object::Int(5), Object::Int(15);
                         [
                         make(&OP_FUNCTION, &[2]),
                         make(&OP_SET_GLOBAL, &[0]),
                         make(&OP_GET_GLOBAL, &[0]),
                         make(&OP_CONSTANT, &[3]),
                         make(&OP_CONSTANT, &[4]),
                         make(&OP_CALL, &[2]),
                         make(&OP_POP, &[]),
                         ].concat().into()),
            define_case!("METHOD sum IMPORTING iv_left TYPE i iv_right TYPE i.
                            5 + 10.
                          ENDMETHOD.

                          sum(5, 15).";
                         Object::Int(5), Object::Int(10), 
                         Object::Function(CompiledFunction{
                             instructions: [
                                 make(&OP_CONSTANT, &[0]),
                                 make(&OP_CONSTANT, &[1]),
                                 make(&OP_ADD, &[]),
                                 make(&OP_POP, &[]),
                                 make(&OP_RETURN, &[]),
                             ].concat().into(),
                             num_parameters: 2,
                             num_locals: 2,
                             ty: DataType::None,
                         }),
                         Object::Int(5), Object::Int(15);
                         [
                         make(&OP_FUNCTION, &[2]),
                         make(&OP_SET_GLOBAL, &[0]),
                         make(&OP_GET_GLOBAL, &[0]),
                         make(&OP_CONSTANT, &[3]),
                         make(&OP_CONSTANT, &[4]),
                         make(&OP_CALL, &[2]),
                         make(&OP_POP, &[]),
                         ].concat().into()),
            define_case!("METHOD sum_5_10 RETURNING rv_sum TYPE i.
                            rv_sum = 5 + 10.
                          ENDMETHOD.

                          sum_5_10().";
                         Object::Int(5), Object::Int(10), 
                         Object::Function(CompiledFunction{
                             instructions: [
                                 make(&OP_CONSTANT, &[0]),
                                 make(&OP_CONSTANT, &[1]),
                                 make(&OP_ADD, &[]),
                                 make(&OP_SET_LOCAL, &[0]),
                                 make(&OP_GET_LOCAL, &[0]),
                                 make(&OP_RETURN_VALUE, &[]),
                             ].concat().into(),
                             num_parameters: 0,
                             num_locals: 1,
                             ty: DataType::Int,
                         });
                         [
                         make(&OP_FUNCTION, &[2]),
                         make(&OP_SET_GLOBAL, &[0]),
                         make(&OP_GET_GLOBAL, &[0]),
                         make(&OP_CALL, &[0]),
                         make(&OP_POP, &[]),
                         ].concat().into()),
            define_case!("METHOD no_return.
                          ENDMETHOD.

                          no_return().";
                         Object::Function(CompiledFunction{
                             instructions: [
                                 make(&OP_RETURN, &[]),
                             ].concat().into(),
                             num_parameters: 0,
                             num_locals: 0,
                             ty: DataType::None,
                         });
                         [
                         make(&OP_FUNCTION, &[0]),
                         make(&OP_SET_GLOBAL, &[0]),
                         make(&OP_GET_GLOBAL, &[0]),
                         make(&OP_CALL, &[0]),
                         make(&OP_POP, &[]),
                         ].concat().into()),
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_recursive_functions() -> Result<()> {
        let tests = vec![
            define_case!("METHOD count_down IMPORTING iv_x TYPE i RETURNING rv_result TYPE i.
                            rv_result = count_down(iv_x - 1).
                          ENDMETHOD.

                          count_down(1).";
                         Object::Int(1),
                         Object::Function(CompiledFunction{
                             instructions: [
                                 make(&OP_CURRENT_FUNCTION, &[]),
                                 make(&OP_GET_LOCAL, &[0]),
                                 make(&OP_CONSTANT, &[0]),
                                 make(&OP_SUB, &[]),
                                 make(&OP_CALL, &[1]),
                                 make(&OP_SET_LOCAL, &[1]),
                                 make(&OP_GET_LOCAL, &[1]),
                                 make(&OP_RETURN_VALUE, &[]),
                             ].concat().into(),
                             num_parameters: 1,
                             num_locals: 2,
                             ty: DataType::Int,
                         }),
                         Object::Int(1);
                         [
                         make(&OP_FUNCTION, &[1]),
                         make(&OP_SET_GLOBAL, &[0]),
                         make(&OP_GET_GLOBAL, &[0]),
                         make(&OP_CONSTANT, &[2]),
                         make(&OP_CALL, &[1]),
                         make(&OP_POP, &[]),
                         ].concat().into()),
        ];

        run_compiler_tests(tests)
    }
}
