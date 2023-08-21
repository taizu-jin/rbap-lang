use crate::code::{make, Opcode, OP_CONSTANT, OP_POP};
use crate::error::Result;
use crate::{ast::Node, code::Instructions, object::Object};

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

    scopes: Vec<CompilationScope>,
    scope_index: usize,
}

impl Compiler {
    pub fn new() -> Self {
        let main_scope = CompilationScope::default();

        Self {
            constants: Vec::new(),
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

    pub fn compile(&mut self, node: Node) -> Result<()> {
        match node {
            Node::Program(p) => {
                for s in p.statements {
                    self.compile(s.into())?;
                }
            }
            Node::Statement(s) => match s {
                crate::ast::Statement::Expression(e) => {
                    self.compile(e.into())?;
                    self.emit(OP_POP, &[]);
                }
                crate::ast::Statement::DataDeclaration(_) => todo!(),
                crate::ast::Statement::Write(_) => todo!(),
                crate::ast::Statement::Data(_) => todo!(),
            },
            Node::Expression(e) => match e {
                crate::ast::Expression::IntLiteral(i) => {
                    let int = Object::Int(i);
                    let constant = self.add_constant(int);
                    self.emit(OP_CONSTANT, &[constant]);
                }
                crate::ast::Expression::StringLiteral(s) => {
                    let str = Object::String(s);
                    let constant = self.add_constant(str);
                    self.emit(OP_CONSTANT, &[constant]);
                }
                crate::ast::Expression::Ident(_) => todo!(),
                crate::ast::Expression::StringTemplate(_) => todo!(),
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
            compiler.compile(program.into())?;

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
            "wrong instructions length.\nwant={}\ngot={}",
            want.len(),
            got.len()
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
}
