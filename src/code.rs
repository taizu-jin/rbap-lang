use std::ops::Deref;

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
            operand_widths: &[$(&$width),*],
        };
    };
}

define_constant!(OP_CONSTANT, 0x00, 2);
define_constant!(OP_ADD, 0x01);

pub struct Opcode {
    pub code: u8,
    pub operand_widths: &'static [&'static usize],
}

impl Deref for Opcode {
    type Target = u8;

    fn deref(&self) -> &Self::Target {
        &self.code
    }
}

impl<'a> Opcode {
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

        let operand = match width {
            2 => i16::to_be_bytes(*o as i16),
            w => panic!("unsupported operand width of {} bytes", w),
        };

        instruction.splice(offset..offset + width, operand);

        offset += width;
    }

    instruction
}

#[cfg(test)]
mod tests {

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
    use super::*;

    #[test]
    fn test_make() {
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
