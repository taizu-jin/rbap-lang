use crate::error::{Error, Result};
use std::{
    collections::HashMap,
    fmt::Display,
    ops::{Deref, DerefMut},
    sync::OnceLock,
};

/// Define an `Opcode` constant.
///
/// # Example
///
/// ```
/// # #[macro_use] extern crate rbap_lang;
/// // Define an OP_ADD constant with opcode of 0x00
/// define_constant!(OP_ADD, 0x00);
///
/// // Define an OP_CONSTANT constant with opcode of 0x01
/// // with a single operand with width of 2 bytes
/// define_constant!(OP_CONSTANT, 0x01, 2);
/// ```
#[macro_export]
macro_rules! define_constant {
    ($name:ident, $opcode:expr $(, $width:expr)*) => {
        pub const $name: $crate::code::Opcode = $crate::code::Opcode {
            code: $opcode,
            label: stringify!($name),
            operand_widths: &[$(&$width),*],
        };
    };
}

define_constant!(OP_CONSTANT, 0x00, 2);
define_constant!(OP_POP, 0x01);
define_constant!(OP_ADD, 0x02);
define_constant!(OP_SUB, 0x03);
define_constant!(OP_MUL, 0x04);
define_constant!(OP_DIV, 0x05);
define_constant!(OP_GREATER_THAN, 0x06);
define_constant!(OP_EQUAL, 0x07);
define_constant!(OP_NOT_EQUAL, 0x08);

static DEFINITIONS: OnceLock<HashMap<u8, &'static Opcode>> = OnceLock::new();

#[derive(Default)]
pub struct Instructions(Vec<u8>);

impl Deref for Instructions {
    type Target = Vec<u8>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Instructions {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<Vec<u8>> for Instructions {
    fn from(value: Vec<u8>) -> Self {
        Instructions(value)
    }
}

impl Display for Instructions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut i = 0;
        while i < self.0.len() {
            let opcode = match Opcode::lookup(self.0[i]) {
                Ok(opcode) => opcode,
                Err(e) => panic!("index: {}, error: {}", i, e),
            };

            let (operands, read) = Opcode::read_operands(opcode, &self.0[i + 1..]);

            write!(f, "{:04} {}", i, opcode.label)?;
            for operand in operands {
                write!(f, " {}", operand)?;
            }
            writeln!(f)?;

            i += 1 + read;
        }

        Ok(())
    }
}

static DEFINITIONS: OnceLock<HashMap<u8, &'static Opcode>> = OnceLock::new();
pub struct Opcode {
    pub code: u8,
    pub label: &'static str,
    pub operand_widths: &'static [&'static usize],
}

impl Deref for Opcode {
    type Target = u8;

    fn deref(&self) -> &Self::Target {
        &self.code
    }
}

impl<'a> Opcode {
    fn lookup(op: u8) -> Result<&'static Opcode> {
        let defitions = Opcode::get_definitions();
        match defitions.get(&op) {
            Some(opcode) => Ok(*opcode),
            None => Err(Error::undefined_opcode(op)),
        }
    }

    fn get_definitions() -> &'a HashMap<u8, &'static Opcode> {
        DEFINITIONS.get_or_init(|| {
            let mut map = HashMap::new();
            map.insert(*OP_CONSTANT, &OP_CONSTANT);
            map.insert(*OP_ADD, &OP_ADD);
            map
        })
    }

    fn read_operands(op: &Opcode, instructions: &[u8]) -> (Vec<i32>, usize) {
        let mut offset = 0;

        let operands =
            op.operand_widths
                .iter()
                .map(|w| {
                    let o = match w {
                        1 => u8::from_be_bytes([instructions[offset]]) as i32,
                        2 => u16::from_be_bytes([instructions[offset], instructions[offset + 1]])
                            as i32,
                        w => panic!("unsupported operand width of {} bytes", w),
                    };
                    offset += **w;
                    o
                })
                .collect();
        (operands, offset)
    }
}

/// Create an instruction from opcode & a slice of operands.
///
/// # Panics
///
/// Opcode operand count and width must match with passed in operands.
pub fn make(op: &Opcode, operands: &[i32]) -> Vec<u8> {
    let mut instruction_len = 1;

    for w in op.operand_widths {
        instruction_len += *w;
    }

    let mut instruction = vec![0; instruction_len];
    instruction[0] = op.code;

    let mut offset = 1;

    for (i, o) in operands.iter().enumerate() {
        let width = match op.operand_widths.get(i) {
            Some(w) => **w,
            None => panic!("expected opcode to have {} operands", i),
        };

        match width {
            1 => {
                let operand = u8::to_be_bytes(*o as u8);
                instruction.splice(offset..offset + width, operand);
            }
            2 => {
                let operand = u16::to_be_bytes(*o as u16);
                instruction.splice(offset..offset + width, operand);
            }
            w => panic!("unsupported operand width of {} bytes", w),
        };

        offset += width;
    }

    instruction
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_make() {
        struct TestCase {
            op: Opcode,
            operands: Vec<i32>,
            expected: Vec<u8>,
        }

        macro_rules! define_case {
            ($op_code:expr; $($operand:expr),*; $($expected:expr),*) => {
                TestCase {
                    op: $op_code,
                    operands: vec![$($operand), *],
                    expected: vec![*$op_code, $($expected), *],
                }
            };
        }

        let tests = vec![
            define_case!(OP_CONSTANT; 65534; 255, 254),
            define_case!(OP_ADD;;),
        ];

        for test in tests {
            let instruction = make(&test.op, &test.operands);

            assert_eq!(
                instruction.len(),
                test.expected.len(),
                "insruction has wrong length. want={}, got={}",
                test.expected.len(),
                instruction.len(),
            );

            for (i, (a, e)) in instruction.iter().zip(test.expected.iter()).enumerate() {
                assert_eq!(a, e, "wrong byte at pos {}. want={}, got={}", i, e, a)
            }
        }
    }

    #[test]
    fn test_instructions_string() {
        let instructions =
            Instructions([make(&OP_ADD, &[]), make(&OP_CONSTANT, &[65535])].concat());

        let expected = r#"0000 OP_ADD
0001 OP_CONSTANT 65535
"#;

        assert_eq!(
            instructions.to_string(),
            expected,
            "instructions wrongly formatted.\nwant={}\ngot={}",
            expected,
            instructions
        )
    }

    #[test]
    fn test_read_operands() {
        struct TestCase {
            op: Opcode,
            operands: Vec<i32>,
            bytes_read: usize,
        }

        macro_rules! define_case {
            ($op_code:expr; $($operand:expr),*; $bytes_read:expr) => {
                TestCase {
                    op: $op_code,
                    operands: vec![$($operand), *],
                    bytes_read: $bytes_read,
                }
            };
        }

        let tests = vec![define_case!(OP_CONSTANT; 65535; 2)];

        for test in tests {
            let instruction = make(&test.op, &test.operands);

            let (operands, read) = Opcode::read_operands(&test.op, &instruction[1..]);

            assert_eq!(
                read, test.bytes_read,
                "bytes read is incorrect. want={}, got={}",
                test.bytes_read, read
            );

            for (want, got) in test.operands.iter().zip(operands.iter()) {
                assert_eq!(want, got, "operand wrong. want={} , got={}", want, got);
            }
        }
    }
}
