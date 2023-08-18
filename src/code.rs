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
    use super::{make, Opcode, OP_ADD, OP_CONSTANT};

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
}
