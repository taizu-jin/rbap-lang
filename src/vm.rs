use crate::{
    compiler::Bytecode,
    object::{CompiledFunction, Object},
};

pub const STACK_SIZE: usize = 2048;
pub const GLOBAL_SIZE: usize = 65536;
pub const MAX_FRAMES: usize = 1024;

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

struct VM {
    constants: Vec<Object>,
    stack: Vec<Object>,
    globals: Vec<Object>,
    frames: Vec<Frame>,
}

impl VM {
    fn new(bytecode: Bytecode) -> Self {
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
        }
    }
}
