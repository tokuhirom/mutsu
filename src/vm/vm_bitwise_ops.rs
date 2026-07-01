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
    match v {
        Value::Int(i) => (*i).max(0) as usize,
        Value::Num(f) => (*f as i64).max(0) as usize,
        other => other.to_string_value().parse::<i64>().unwrap_or(0).max(0) as usize,
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
