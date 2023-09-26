use serde::{Deserialize, Serialize};

use crate::error::{CompilerError, Result};
use std::{collections::HashMap, fmt::Display, sync::OnceLock};

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
define_constant!(OP_TRUE, 0x09);
define_constant!(OP_FALSE, 0x0A);
define_constant!(OP_JUMP_NOT_TRUTH, 0x0B, 2);
define_constant!(OP_JUMP, 0x0C, 2);
define_constant!(OP_NOT, 0x0D);
define_constant!(OP_MINUS, 0x0E);
define_constant!(OP_GET_GLOBAL, 0x0F, 2);
define_constant!(OP_SET_GLOBAL, 0x10, 2);
define_constant!(OP_GET_LOCAL, 0x11, 1);
define_constant!(OP_SET_LOCAL, 0x12, 1);
define_constant!(OP_CURRENT_FUNCTION, 0x13);
define_constant!(OP_AND, 0x14);
define_constant!(OP_OR, 0x15);
define_constant!(OP_STRING_TEMPLATE, 0x16, 2);
define_constant!(OP_WRITE, 0x17, 2);
define_constant!(OP_FUNCTION, 0x18, 2);
define_constant!(OP_CALL, 0x19, 1);
define_constant!(OP_RETURN, 0x1A);
define_constant!(OP_RETURN_VALUE, 0x1B);

static DEFINITIONS: OnceLock<HashMap<u8, &'static Opcode>> = OnceLock::new();

#[derive(Default, Debug, PartialEq, PartialOrd, Clone, Serialize, Deserialize)]
pub struct Instructions(Vec<u8>);

impl AsRef<[u8]> for Instructions {
    fn as_ref(&self) -> &[u8] {
        &self.0
    }
}

impl AsMut<Vec<u8>> for Instructions {
    fn as_mut(&mut self) -> &mut Vec<u8> {
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

#[derive(Debug, PartialEq, Eq)]
pub struct Opcode {
    pub code: u8,
    pub label: &'static str,
    pub operand_widths: &'static [&'static usize],
}

impl<'a> Opcode {
    pub fn lookup(op: u8) -> Result<&'static Opcode> {
        let defitions = Opcode::get_definitions();
        match defitions.get(&op) {
            Some(opcode) => Ok(*opcode),
            None => Err(CompilerError::UndefinedOpcode(op).into()),
        }
    }

    fn get_definitions() -> &'a HashMap<u8, &'static Opcode> {
        DEFINITIONS.get_or_init(|| {
            let mut map = HashMap::new();
            map.insert(OP_CONSTANT.into(), &OP_CONSTANT);
            map.insert(OP_POP.into(), &OP_POP);
            map.insert(OP_ADD.into(), &OP_ADD);
            map.insert(OP_SUB.into(), &OP_SUB);
            map.insert(OP_MUL.into(), &OP_MUL);
            map.insert(OP_DIV.into(), &OP_DIV);
            map.insert(OP_GREATER_THAN.into(), &OP_GREATER_THAN);
            map.insert(OP_EQUAL.into(), &OP_EQUAL);
            map.insert(OP_NOT_EQUAL.into(), &OP_NOT_EQUAL);
            map.insert(OP_TRUE.into(), &OP_TRUE);
            map.insert(OP_FALSE.into(), &OP_FALSE);
            map.insert(OP_JUMP_NOT_TRUTH.into(), &OP_JUMP_NOT_TRUTH);
            map.insert(OP_JUMP.into(), &OP_JUMP);
            map.insert(OP_NOT.into(), &OP_NOT);
            map.insert(OP_MINUS.into(), &OP_MINUS);
            map.insert(OP_GET_GLOBAL.into(), &OP_GET_GLOBAL);
            map.insert(OP_SET_GLOBAL.into(), &OP_SET_GLOBAL);
            map.insert(OP_GET_LOCAL.into(), &OP_GET_LOCAL);
            map.insert(OP_SET_LOCAL.into(), &OP_SET_LOCAL);
            map.insert(OP_CURRENT_FUNCTION.into(), &OP_CURRENT_FUNCTION);
            map.insert(OP_AND.into(), &OP_AND);
            map.insert(OP_OR.into(), &OP_OR);
            map.insert(OP_STRING_TEMPLATE.into(), &OP_STRING_TEMPLATE);
            map.insert(OP_WRITE.into(), &OP_WRITE);
            map.insert(OP_FUNCTION.into(), &OP_FUNCTION);
            map.insert(OP_CALL.into(), &OP_CALL);
            map.insert(OP_RETURN.into(), &OP_RETURN);
            map.insert(OP_RETURN_VALUE.into(), &OP_RETURN_VALUE);
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

impl From<Opcode> for u8 {
    fn from(value: Opcode) -> Self {
        value.code
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

impl Display for Opcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}({:#04x})W{:?}",
            self.label, self.code, self.operand_widths
        )
    }
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
                    expected: vec![$op_code.into(), $($expected), *],
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
