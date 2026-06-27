use super::*;

impl Interpreter {
    fn buf_class_name(val: &Value) -> Option<String> {
        if let Value::Instance { class_name, .. } = val {
            let cn = class_name.resolve();
            if cn == "Buf"
                || cn == "Blob"
                || cn == "utf8"
                || cn == "utf16"
                || cn.starts_with("Buf[")
                || cn.starts_with("Blob[")
                || cn.starts_with("buf")
                || cn.starts_with("blob")
            {
                return Some(cn.to_string());
            }
        }
        None
    }

    pub(crate) fn str_bitwise_op(
        left: &Value,
        right: &Value,
        op: fn(u32, u32) -> u32,
        pad_to_max: bool,
    ) -> Result<Value, RuntimeError> {
        let left_is_buf = Self::is_buf_value(left);
        let right_is_buf = Self::is_buf_value(right);
        let any_buf = left_is_buf || right_is_buf;
        if any_buf {
            // For Buf values, operate on bytes
            let lb = if left_is_buf {
                Self::extract_buf_bytes(left)
            } else {
                crate::runtime::utils::coerce_to_str(left)
                    .as_bytes()
                    .to_vec()
            };
            let rb = if right_is_buf {
                Self::extract_buf_bytes(right)
            } else {
                crate::runtime::utils::coerce_to_str(right)
                    .as_bytes()
                    .to_vec()
            };
            // Buf bitwise ops always extend to the longer operand,
            // padding the shorter one with zeros (even for ~&).
            let len = lb.len().max(rb.len());
            let mut out = Vec::with_capacity(len);
            for i in 0..len {
                let a = lb.get(i).copied().unwrap_or(0) as u32;
                let b = rb.get(i).copied().unwrap_or(0) as u32;
                out.push(op(a, b) as u8);
            }
            let byte_vals: Vec<Value> = out.into_iter().map(|b| Value::Int(b as i64)).collect();
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("bytes".to_string(), Value::array(byte_vals));
            let result_type = match (Self::buf_class_name(left), Self::buf_class_name(right)) {
                (Some(l), Some(r)) if l == r => l,
                _ => "Buf".to_string(),
            };
            Ok(Value::make_instance(
                crate::symbol::Symbol::intern(&result_type),
                attrs,
            ))
        } else {
            // For strings, operate on Unicode codepoints (ordinal values)
            let ls = crate::runtime::utils::coerce_to_str(left);
            let rs = crate::runtime::utils::coerce_to_str(right);
            let lc: Vec<u32> = ls.chars().map(|c| c as u32).collect();
            let rc: Vec<u32> = rs.chars().map(|c| c as u32).collect();
            let len = if pad_to_max {
                lc.len().max(rc.len())
            } else {
                lc.len().min(rc.len())
            };
            let mut out = String::with_capacity(len);
            for i in 0..len {
                let a = lc.get(i).copied().unwrap_or(0);
                let b = rc.get(i).copied().unwrap_or(0);
                let result_cp = op(a, b);
                if let Some(ch) = char::from_u32(result_cp) {
                    out.push(ch);
                }
            }
            // Apply NFC normalization to the result
            use unicode_normalization::UnicodeNormalization;
            Ok(Value::str(out.nfc().collect::<String>()))
        }
    }

    pub(crate) fn shift_left_i64(a: i64, b: i64) -> Value {
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

    pub(crate) fn shift_right_i64(a: i64, b: i64) -> Value {
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

    pub(crate) fn shift_left_bigint(a: &num_bigint::BigInt, b: i64) -> Value {
        if b < 0 {
            Value::from_bigint(a >> (b.unsigned_abs() as usize))
        } else {
            Value::from_bigint(a << (b as usize))
        }
    }

    pub(crate) fn shift_right_bigint(a: &num_bigint::BigInt, b: i64) -> Value {
        if b < 0 {
            Value::from_bigint(a << (b.unsigned_abs() as usize))
        } else {
            Value::from_bigint(a >> (b as usize))
        }
    }
}
