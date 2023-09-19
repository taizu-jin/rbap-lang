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
    globals: Vec<Object>,
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
            globals: Vec::with_capacity(GLOBAL_SIZE),
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
                &OP_POP => {
                    self.pop()?;
                }
                opcode if matches!(opcode, &OP_ADD | &OP_SUB | &OP_MUL | &OP_DIV) => {
                    self.execute_binary_operation(opcode)?
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

    fn execute_binary_operation(&mut self, opcode: &Opcode) -> Result<()> {
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

}

}
