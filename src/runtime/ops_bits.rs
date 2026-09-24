use super::*;

impl Interpreter {
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
            let byte_vals: Vec<Value> = out.into_iter().map(|b| Value::int(b as i64)).collect();
            let result_type = match (Self::buf_class_name(left), Self::buf_class_name(right)) {
                (Some(l), Some(r)) if l == r => l,
                _ => "Buf".to_string(),
            };
            Ok(crate::value::value_buf::make_buf(
                crate::symbol::Symbol::intern(&result_type),
                byte_vals,
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
}
