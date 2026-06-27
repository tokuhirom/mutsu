//! Bitwise (and/or/xor) and shift ops for int, bool, and string operands.
use super::*;
use num_traits::ToPrimitive;

impl Interpreter {
    pub(super) fn exec_bit_and_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let (l, r) = runtime::coerce_numeric(left, right);
        let result = match (l, r) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a & b),
            (Value::BigInt(a), Value::BigInt(b)) => Value::from_bigint(&*a & &*b),
            (Value::BigInt(a), Value::Int(b)) => {
                Value::from_bigint(&*a & &num_bigint::BigInt::from(b))
            }
            (Value::Int(a), Value::BigInt(b)) => {
                Value::from_bigint(&num_bigint::BigInt::from(a) & &*b)
            }
            _ => Value::Int(0),
        };
        self.stack.push(result);
    }

    pub(super) fn exec_bit_or_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let (l, r) = runtime::coerce_numeric(left, right);
        let result = match (l, r) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a | b),
            (Value::BigInt(a), Value::BigInt(b)) => Value::from_bigint(&*a | &*b),
            (Value::BigInt(a), Value::Int(b)) => {
                Value::from_bigint(&*a | &num_bigint::BigInt::from(b))
            }
            (Value::Int(a), Value::BigInt(b)) => {
                Value::from_bigint(&num_bigint::BigInt::from(a) | &*b)
            }
            _ => Value::Int(0),
        };
        self.stack.push(result);
    }

    pub(super) fn exec_bit_xor_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let (l, r) = runtime::coerce_numeric(left, right);
        let result = match (l, r) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a ^ b),
            (Value::BigInt(a), Value::BigInt(b)) => Value::from_bigint(&*a ^ &*b),
            (Value::BigInt(a), Value::Int(b)) => {
                Value::from_bigint(&*a ^ &num_bigint::BigInt::from(b))
            }
            (Value::Int(a), Value::BigInt(b)) => {
                Value::from_bigint(&num_bigint::BigInt::from(a) ^ &*b)
            }
            _ => Value::Int(0),
        };
        self.stack.push(result);
    }

    pub(super) fn exec_bit_shift_left_op(&mut self) {
        fn shift_left_i64(a: i64, b: i64) -> Value {
            if b < 0 {
                let shift = b.unsigned_abs();
                let shifted = if shift >= i64::BITS as u64 {
                    if a < 0 { -1 } else { 0 }
                } else {
                    a >> (shift as u32)
                };
                return Value::Int(shifted);
            }
            let shift = b as u64;
            if shift >= i64::BITS as u64 {
                return Value::from_bigint(num_bigint::BigInt::from(a) << (shift as usize));
            }
            // Use BigInt for the shift to avoid i64 overflow (Raku integers are arbitrary precision)
            Value::from_bigint(num_bigint::BigInt::from(a) << (shift as usize))
        }

        fn shift_left_bigint(a: num_bigint::BigInt, b: i64) -> Value {
            if b < 0 {
                Value::from_bigint(a >> (b.unsigned_abs() as usize))
            } else {
                Value::from_bigint(a << (b as usize))
            }
        }

        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let (l, r) = runtime::coerce_numeric(left, right);
        let result = match (l, r) {
            (Value::Int(a), Value::Int(b)) => shift_left_i64(a, b),
            (l, r) => {
                let a = l.to_bigint();
                let b_big = r.to_bigint();
                let b = b_big.to_i64().unwrap_or_else(|| {
                    if b_big.sign() == num_bigint::Sign::Minus {
                        i64::MIN
                    } else {
                        i64::MAX
                    }
                });
                shift_left_bigint(a, b)
            }
        };
        self.stack.push(result);
    }

    pub(super) fn exec_bit_shift_right_op(&mut self) {
        fn shift_right_i64(a: i64, b: i64) -> Value {
            if b < 0 {
                let shift = b.unsigned_abs();
                if shift >= i64::BITS as u64 {
                    return Value::from_bigint(num_bigint::BigInt::from(a) << (shift as usize));
                }
                if let Some(v) = a.checked_shl(shift as u32) {
                    Value::Int(v)
                } else {
                    Value::from_bigint(num_bigint::BigInt::from(a) << (shift as usize))
                }
            } else {
                let shift = b as u64;
                let shifted = if shift >= i64::BITS as u64 {
                    if a < 0 { -1 } else { 0 }
                } else {
                    a >> (shift as u32)
                };
                Value::Int(shifted)
            }
        }

        fn shift_right_bigint(a: num_bigint::BigInt, b: i64) -> Value {
            if b < 0 {
                Value::from_bigint(a << (b.unsigned_abs() as usize))
            } else {
                Value::from_bigint(a >> (b as usize))
            }
        }

        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let (l, r) = runtime::coerce_numeric(left, right);
        let result = match (l, r) {
            (Value::Int(a), Value::Int(b)) => shift_right_i64(a, b),
            (l, r) => {
                let a = l.to_bigint();
                let b_big = r.to_bigint();
                let b = b_big.to_i64().unwrap_or_else(|| {
                    if b_big.sign() == num_bigint::Sign::Minus {
                        i64::MIN
                    } else {
                        i64::MAX
                    }
                });
                shift_right_bigint(a, b)
            }
        };
        self.stack.push(result);
    }

    pub(super) fn exec_bool_bit_or_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack.push(Value::Bool(left.truthy() | right.truthy()));
    }

    pub(super) fn exec_bool_bit_and_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack.push(Value::Bool(left.truthy() & right.truthy()));
    }

    pub(super) fn exec_bool_bit_xor_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack.push(Value::Bool(left.truthy() ^ right.truthy()));
    }

    /// String bitwise AND (~&): AND corresponding bytes of two strings.
    pub(super) fn exec_str_bit_and_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let l = left.to_string_value();
        let r = right.to_string_value();
        let lb = l.as_bytes();
        let rb = r.as_bytes();
        let len = lb.len().max(rb.len());
        let result: Vec<u8> = (0..len)
            .map(|i| {
                let a = lb.get(i).copied().unwrap_or(0);
                let b = rb.get(i).copied().unwrap_or(0);
                a & b
            })
            .collect();
        self.stack
            .push(Value::str(String::from_utf8_lossy(&result).into_owned()));
    }

    /// String bitwise OR (~|): OR corresponding bytes of two strings.
    pub(super) fn exec_str_bit_or_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let l = left.to_string_value();
        let r = right.to_string_value();
        let lb = l.as_bytes();
        let rb = r.as_bytes();
        let len = lb.len().max(rb.len());
        let result: Vec<u8> = (0..len)
            .map(|i| {
                let a = lb.get(i).copied().unwrap_or(0);
                let b = rb.get(i).copied().unwrap_or(0);
                a | b
            })
            .collect();
        self.stack
            .push(Value::str(String::from_utf8_lossy(&result).into_owned()));
    }

    /// String bitwise XOR (~^): XOR corresponding bytes of two strings.
    pub(super) fn exec_str_bit_xor_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let l = left.to_string_value();
        let r = right.to_string_value();
        let lb = l.as_bytes();
        let rb = r.as_bytes();
        let len = lb.len().max(rb.len());
        let result: Vec<u8> = (0..len)
            .map(|i| {
                let a = lb.get(i).copied().unwrap_or(0);
                let b = rb.get(i).copied().unwrap_or(0);
                a ^ b
            })
            .collect();
        self.stack
            .push(Value::str(String::from_utf8_lossy(&result).into_owned()));
    }

    /// String bitwise shift left (~<): not yet implemented in Raku either.
    /// TODO: Implement when Raku spec is finalized for this operator.
    pub(super) fn exec_str_shift_left_op(&mut self) {
        let _right = self.stack.pop().unwrap();
        let _left = self.stack.pop().unwrap();
        self.stack.push(Value::Nil);
    }

    /// String bitwise shift right (~>): not yet implemented in Raku either.
    /// TODO: Implement when Raku spec is finalized for this operator.
    pub(super) fn exec_str_shift_right_op(&mut self) {
        let _right = self.stack.pop().unwrap();
        let _left = self.stack.pop().unwrap();
        self.stack.push(Value::Nil);
    }
}
