#![allow(dead_code)]

mod symbol_table;

use crate::code::*;
use crate::error::{CompilerError, Error, ParseInfixError, ParsePrefixError, Result};
use crate::{ast::Node, code::Instructions, object::Object};

use self::symbol_table::{Scope, Symbol, SymbolTable};

pub struct Bytecode {
    instructions: Instructions,
    constants: Vec<Object>,
}

#[derive(Clone, Copy, Default)]
struct EmittedInstruction {
    opcode: u8,
    position: usize,
}

#[derive(Default)]
struct CompilationScope {
    instructions: Instructions,
    last_insturction: EmittedInstruction,
    prev_instruction: EmittedInstruction,
}

struct Compiler {
    constants: Vec<Object>,
    symbol_table: SymbolTable,

    scopes: Vec<CompilationScope>,
    scope_index: usize,
}

impl Compiler {
    pub fn new() -> Self {
        let main_scope = CompilationScope::default();

        Self {
            constants: Vec::new(),
            symbol_table: SymbolTable::new(),
            scopes: vec![main_scope],
            scope_index: 0,
        }
    }

    pub fn bytecode(mut self) -> Bytecode {
        Bytecode {
            instructions: self.scopes.remove(self.scope_index).instructions,
            constants: self.constants,
        }
    }

    pub fn compile(&mut self, node: impl Into<Node>) -> Result<()> {
        let node = node.into();

        match node {
            Node::Program(p) => {
                for s in p.statements {
                    self.compile(s)?;
                }
            }
            Node::Statement(s) => match s {
                crate::ast::Statement::Expression(e) => {
                    self.compile(e)?;
                    self.emit(OP_POP, &[]);
                }
                crate::ast::Statement::Declaration(d) => {
                    for declaration in d.as_ref() {
                        self.symbol_table.define(declaration.ident.as_ref());
                    }
                }
                crate::ast::Statement::Write(_) => todo!(),
                crate::ast::Statement::Assignment(a) => {
                    let symbol = self
                        .symbol_table
                        .resolve(a.ident.as_ref())
                        .ok_or(Error::from(CompilerError::UndefinedVariable(a.ident)))?;
                    self.compile(a.value)?;

                    if symbol.scope == Scope::Global {
                        self.emit(OP_SET_GLOBAL, &[symbol.index.try_into().unwrap()]);
                    } else {
                        self.emit(OP_SET_LOCAL, &[symbol.index.try_into().unwrap()]);
                    }
                }
                crate::ast::Statement::Block(_) => todo!(),
                crate::ast::Statement::If(_) => todo!(),
                crate::ast::Statement::Function(_) => todo!(),
            },
            Node::Expression(e) => match e {
                crate::ast::Expression::IntLiteral(i) => {
                    let int = Object::Int(i.into());
                    let constant = self.add_constant(int);
                    self.emit(OP_CONSTANT, &[constant]);
                }
                crate::ast::Expression::StringLiteral(s) => {
                    let str = Object::String(s.into());
                    let constant = self.add_constant(str);
                    self.emit(OP_CONSTANT, &[constant]);
                }
                crate::ast::Expression::Ident(id) => {
                    let symbol = self
                        .symbol_table
                        .resolve(id.as_ref())
                        .ok_or(Error::from(CompilerError::UndefinedVariable(id.into())))?;

                    self.load_symbol(symbol);
                }
                crate::ast::Expression::BoolLiteral(b) => {
                    if b.into() {
                        self.emit(OP_TRUE, &[]);
                    } else {
                        self.emit(OP_FALSE, &[]);
                    }
                }
                crate::ast::Expression::StringTemplate(_) => todo!(),
                crate::ast::Expression::CallExpression(_) => todo!(),
                crate::ast::Expression::InfixExpression(ie) => {
                    let (left, right, operand_op_code) = match ie.operator {
                        crate::ast::Operator::Add => (*ie.left, *ie.right, OP_ADD),
                        crate::ast::Operator::Div => (*ie.left, *ie.right, OP_DIV),
                        crate::ast::Operator::Sub => (*ie.left, *ie.right, OP_SUB),
                        crate::ast::Operator::Mul => (*ie.left, *ie.right, OP_MUL),
                        crate::ast::Operator::GreaterThan => (*ie.left, *ie.right, OP_GREATER_THAN),
                        crate::ast::Operator::LesserThan => (*ie.right, *ie.left, OP_GREATER_THAN),
                        crate::ast::Operator::Equal => (*ie.left, *ie.right, OP_EQUAL),
                        crate::ast::Operator::NotEqual => (*ie.left, *ie.right, OP_NOT_EQUAL),
                        o => return Err(ParseInfixError::UnsupportedOperator(o).into()),
                    };

                    self.compile(left)?;
                    self.compile(right)?;
                    self.emit(operand_op_code, &[]);
                }
                crate::ast::Expression::PrefixExpression(pe) => {
                    self.compile(*pe.right)?;
                    match pe.operator {
                        crate::ast::Operator::Not => self.emit(OP_NOT, &[]),
                        crate::ast::Operator::Sub => self.emit(OP_MINUS, &[]),
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

    fn current_instructions(&self) -> &Instructions {
        &self.scopes[self.scope_index].instructions
    }
    fn current_instructions_mut(&mut self) -> &mut Instructions {
        &mut self.scopes[self.scope_index].instructions
    }

    fn set_last_instruction(&mut self, op: Opcode, pos: usize) {
        let current_scope = &mut self.scopes[self.scope_index];
        let previous = current_scope.prev_instruction;
        let last = EmittedInstruction {
            opcode: *op,
            position: pos,
        };

        current_scope.prev_instruction = previous;
        current_scope.last_insturction = last;
    }

    fn load_symbol(&mut self, symbol: Symbol) {
        match symbol.scope {
            Scope::Global => self.emit(
                OP_GET_GLOBAL,
                &[symbol.index.try_into().expect("max symbol count reached")],
            ),
            Scope::Local => self.emit(
                OP_GET_LOCAL,
                &[symbol.index.try_into().expect("max symbol count reached")],
            ),
            Scope::Function => self.emit(OP_CURRENT_CLOSURE, &[]),
        };
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Compiler::new()
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast::Program, error::Result, lexer::Lexer, parser::Parser};

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
        let tests = vec![define_case!("'monkey'.";
                         Object::String("monkey".into());
                         [make(&OP_CONSTANT, &[0]),
                          make(&OP_POP, &[])].concat().into())];

        run_compiler_tests(tests)
    }

    fn run_compiler_tests(tests: Vec<TestCase>) -> Result<()> {
        for test in tests {
            let program = parse(test.input.into());

            let mut compiler = Compiler::new();
            compiler.compile(program)?;

            let bytecode = compiler.bytecode();

            test_instructions(&test.expected_instructions, &bytecode.instructions);
            test_constants(&test.expected_constants, &bytecode.constants);
        }

        Ok(())
    }

    fn parse(input: String) -> Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer.iter());
        parser.parse()
    }

    fn test_instructions(want: &Instructions, got: &Instructions) {
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
            assert_eq!(
                want, got,
                "wrong instruction at {}.\nwant={}\ngot={}",
                i, want, got
            );
        }
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
            assert_eq!(
                want, got,
                "wrong constant at {}.\nwant={}\ngot={}",
                i, want, got
            );
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

    fn test_conditionals_expressions() -> Result<()> {
        let tests = vec![define_case!("IF rbap_true. 10. ENDIF. 20.";
                         Object::Int(1), Object::Int(20);
                         [make(&OP_TRUE, &[]),
                         make(&OP_JUMP_NOT_TRUTH, &[10]),
                         make(&OP_CONSTANT, &[0]),
                         make(&OP_JUMP, &[11]),
                         make(&OP_NULL, &[]),
                         make(&OP_POP, &[]),
                         make(&OP_CONSTANT, &[1]),
                         make(&OP_POP, &[])].concat().into())];

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
        ];

        run_compiler_tests(tests)
    }
}
