//! Bitwise (and/or/xor) and shift ops for int, bool, and string operands.
use super::*;

impl Interpreter {
    /// Pop the two operands of an integer bitwise / shift operator. A
    /// `Failure` operand (e.g. `"A".Int` inside `$n +& ...`) throws its
    /// contained exception rather than being silently coerced to 0, and an
    /// object operand numifies through its user `.Numeric` first -- the way
    /// the prefix `+^` already did -- so `C.new +> 2` shifts what
    /// `C.new.Numeric` returns instead of the object's 0 (#9566).
    fn pop_int_bitop_operands(&mut self) -> Result<(Value, Value), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        Self::throw_if_failure(&left)?;
        Self::throw_if_failure(&right)?;
        let left = if Self::value_needs_numeric_bridge(&left) {
            self.coerce_numeric_bridge_value(left)?
        } else {
            left
        };
        let right = if Self::value_needs_numeric_bridge(&right) {
            self.coerce_numeric_bridge_value(right)?
        } else {
            right
        };
        Ok((left, right))
    }

    pub(super) fn exec_bit_and_op(&mut self) -> Result<(), RuntimeError> {
        let (left, right) = self.pop_int_bitop_operands()?;
        let result = crate::builtins::int_bitop(&left, &right, crate::builtins::BitOp::And);
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_bit_or_op(&mut self) -> Result<(), RuntimeError> {
        let (left, right) = self.pop_int_bitop_operands()?;
        let result = crate::builtins::int_bitop(&left, &right, crate::builtins::BitOp::Or);
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_bit_xor_op(&mut self) -> Result<(), RuntimeError> {
        let (left, right) = self.pop_int_bitop_operands()?;
        let result = crate::builtins::int_bitop(&left, &right, crate::builtins::BitOp::Xor);
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_bit_shift_left_op(&mut self) -> Result<(), RuntimeError> {
        let (left, right) = self.pop_int_bitop_operands()?;
        let result = crate::builtins::int_shift_left(&left, &right);
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_bit_shift_right_op(&mut self) -> Result<(), RuntimeError> {
        let (left, right) = self.pop_int_bitop_operands()?;
        let result = crate::builtins::int_shift_right(&left, &right);
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_bool_bit_or_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack
            .push(Value::truth(left.truthy() | right.truthy()));
    }

    pub(super) fn exec_bool_bit_and_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack
            .push(Value::truth(left.truthy() & right.truthy()));
    }

    pub(super) fn exec_bool_bit_xor_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack
            .push(Value::truth(left.truthy() ^ right.truthy()));
    }

    /// String bitwise AND (~&): AND corresponding codepoints of two strings.
    /// Delegates to the shared `str_bitwise_op` so the opcode path (used by
    /// `$x ~&= …`) matches the `infix:<~&>` builtin exactly: codepoint-wise
    /// (not byte-wise), truncating to the **shorter** operand (`~&` does not
    /// pad), with NFC normalization. Also handles `Buf` operands.
    pub(super) fn exec_str_bit_and_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = Self::str_bitwise_op(&left, &right, |a, b| a & b, false)?;
        self.stack.push(result);
        Ok(())
    }

    /// String bitwise OR (~|): OR corresponding codepoints; pads to the longer
    /// operand. Delegates to the shared `str_bitwise_op` (see `~&` above).
    pub(super) fn exec_str_bit_or_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = Self::str_bitwise_op(&left, &right, |a, b| a | b, true)?;
        self.stack.push(result);
        Ok(())
    }

    /// String bitwise XOR (~^): XOR corresponding codepoints; pads to the longer
    /// operand. Delegates to the shared `str_bitwise_op` (see `~&` above).
    pub(super) fn exec_str_bit_xor_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = Self::str_bitwise_op(&left, &right, |a, b| a ^ b, true)?;
        self.stack.push(result);
        Ok(())
    }

    /// String bitwise shift left (~<): treat the left string's bytes as a
    /// big-endian bit string and shift it left by N bits (`"a" ~< 8` → `"a\0"`,
    /// i.e. the value times 2**N, appending low-order zero bits).
    pub(super) fn exec_str_shift_left_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let n = shift_count(&right);
        let l = left.to_string_value();
        let out = str_shift_left_bytes(l.as_bytes(), n);
        self.stack
            .push(Value::str(String::from_utf8_lossy(&out).into_owned()));
    }

    /// String bitwise shift right (~>): treat the left string's bytes as a
    /// big-endian bit string and shift it right by N bits (`"aa" ~> 8` → `"a"`,
    /// dropping the low-order N bits).
    pub(super) fn exec_str_shift_right_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let n = shift_count(&right);
        let l = left.to_string_value();
        let out = str_shift_right_bytes(l.as_bytes(), n);
        self.stack
            .push(Value::str(String::from_utf8_lossy(&out).into_owned()));
    }
}

/// Coerce a shift-count operand to a non-negative bit count.
fn shift_count(v: &Value) -> usize {
    match v.view() {
        ValueView::Int(i) => i.max(0) as usize,
        ValueView::Num(f) => (f as i64).max(0) as usize,
        _ => v.to_string_value().parse::<i64>().unwrap_or(0).max(0) as usize,
    }
}

/// Read bit `k` (counted from the least-significant bit) of a big-endian byte
/// string, where `bytes[bytes.len()-1]` is the least-significant byte.
fn get_bit_be(bytes: &[u8], k: usize) -> bool {
    let byte_from_end = k / 8;
    if byte_from_end >= bytes.len() {
        return false;
    }
    let idx = bytes.len() - 1 - byte_from_end;
    (bytes[idx] >> (k % 8)) & 1 == 1
}

fn set_bit_be(bytes: &mut [u8], k: usize) {
    let byte_from_end = k / 8;
    let idx = bytes.len() - 1 - byte_from_end;
    bytes[idx] |= 1 << (k % 8);
}

fn str_shift_left_bytes(input: &[u8], n: usize) -> Vec<u8> {
    let in_bits = input.len() * 8;
    if in_bits == 0 {
        return Vec::new();
    }
    let out_len = (in_bits + n).div_ceil(8).max(1);
    let mut out = vec![0u8; out_len];
    for k in 0..in_bits {
        if get_bit_be(input, k) {
            set_bit_be(&mut out, k + n);
        }
    }
    out
}

fn str_shift_right_bytes(input: &[u8], n: usize) -> Vec<u8> {
    let in_bits = input.len() * 8;
    if n >= in_bits {
        return Vec::new();
    }
    let out_len = (in_bits - n).div_ceil(8).max(1);
    let mut out = vec![0u8; out_len];
    for k in n..in_bits {
        if get_bit_be(input, k) {
            set_bit_be(&mut out, k - n);
        }
    }
    out
}
