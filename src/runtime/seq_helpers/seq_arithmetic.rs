use super::super::*;

impl Interpreter {
    pub(in crate::runtime) fn seq_value_to_f64(v: &Value) -> Option<f64> {
        match v {
            Value::Int(i) => Some(*i as f64),
            Value::Num(f) => Some(*f),
            Value::Rat(n, d) => {
                if *d != 0 {
                    Some(*n as f64 / *d as f64)
                } else {
                    None
                }
            }
            Value::Bool(b) => Some(if *b { 1.0 } else { 0.0 }),
            _ => None,
        }
    }

    pub(in crate::runtime) fn seq_value_to_rat(v: &Value) -> Option<(i64, i64)> {
        match v {
            Value::Int(i) => Some((*i, 1)),
            Value::Rat(n, d) if *d != 0 => Some((*n, *d)),
            _ => None,
        }
    }

    pub(in crate::runtime) fn seq_geometric_ratio_rat(
        a: &Value,
        b: &Value,
        c: &Value,
    ) -> Option<(i64, i64)> {
        fn normalize(mut n: i128, mut d: i128) -> Option<(i64, i64)> {
            if d == 0 {
                return None;
            }
            if d < 0 {
                n = -n;
                d = -d;
            }
            fn gcd_i128(mut x: i128, mut y: i128) -> i128 {
                x = x.abs();
                y = y.abs();
                while y != 0 {
                    let t = y;
                    y = x % y;
                    x = t;
                }
                x
            }
            let g = gcd_i128(n, d);
            let nn = n / g;
            let dd = d / g;
            if nn < i64::MIN as i128
                || nn > i64::MAX as i128
                || dd < i64::MIN as i128
                || dd > i64::MAX as i128
            {
                return None;
            }
            Some((nn as i64, dd as i64))
        }

        let (an, ad) = Self::seq_value_to_rat(a)?;
        let (bn, bd) = Self::seq_value_to_rat(b)?;
        let (cn, cd) = Self::seq_value_to_rat(c)?;
        if an == 0 || bn == 0 {
            return None;
        }

        // b^2 == a*c in rational space means a,b,c are geometric.
        let left = (bn as i128) * (bn as i128) * (ad as i128) * (cd as i128);
        let right = (an as i128) * (cn as i128) * (bd as i128) * (bd as i128);
        if left != right {
            return None;
        }

        // ratio = b / a.
        let num = (bn as i128) * (ad as i128);
        let den = (bd as i128) * (an as i128);
        normalize(num, den)
    }

    /// Check if a value matches a type name for sequence type endpoints.
    /// Handles the Num->Rat mapping since mutsu uses Num for decimal values.
    pub(in crate::runtime) fn seq_type_matches(val: &Value, type_name: &str) -> bool {
        let actual = super::super::value_type_name(val);
        if actual == type_name {
            return true;
        }
        // Special case: Num values with non-integer values match "Rat"
        if type_name == "Rat"
            && let Value::Num(f) = val
        {
            return *f != f.floor(); // non-integer Num matches Rat
        }
        false
    }

    pub(in crate::runtime) fn seq_values_equal(a: &Value, b: &Value) -> bool {
        // Junction endpoint: check if a matches any junction value
        if let Value::Junction { values, .. } = b {
            return values.iter().any(|v| Self::seq_values_equal(a, v));
        }
        if let Value::Junction { values, .. } = a {
            return values.iter().any(|v| Self::seq_values_equal(v, b));
        }
        match (a, b) {
            (Value::Int(x), Value::Int(y)) => x == y,
            (Value::Str(x), Value::Str(y)) => x == y,
            (Value::Bool(x), Value::Bool(y)) => x == y,
            _ => {
                if let (Some(fa), Some(fb)) = (Self::seq_value_to_f64(a), Self::seq_value_to_f64(b))
                {
                    (fa - fb).abs() < 1e-12
                } else {
                    false
                }
            }
        }
    }

    // Compute the successor of a string (increment the last character, carrying over)
    pub(in crate::runtime) fn string_succ(s: &str) -> String {
        crate::builtins::str_increment::string_succ(s)
    }

    pub(in crate::runtime) fn digit_string_succ_radix(s: &str, radix: u32) -> String {
        if s.is_empty() || !(2..=10).contains(&radix) {
            return s.to_string();
        }
        let mut chars: Vec<char> = s.chars().collect();
        let mut carry = true;
        for ch in chars.iter_mut().rev() {
            if !carry {
                break;
            }
            if !ch.is_ascii_digit() {
                return Self::string_succ(s);
            }
            let mut digit = (*ch as u8 - b'0') as u32;
            if digit + 1 >= radix {
                digit = 0;
                *ch = (b'0' + digit as u8) as char;
            } else {
                digit += 1;
                *ch = (b'0' + digit as u8) as char;
                carry = false;
            }
        }
        if carry {
            chars.insert(0, '1');
        }
        chars.into_iter().collect()
    }

    pub(in crate::runtime) fn digit_string_pred_radix(
        s: &str,
        radix: u32,
    ) -> Result<String, RuntimeError> {
        if s.is_empty() || !(2..=10).contains(&radix) {
            return Self::string_pred(s);
        }
        let mut chars: Vec<char> = s.chars().collect();
        let mut borrow = true;
        for ch in chars.iter_mut().rev() {
            if !borrow {
                break;
            }
            if !ch.is_ascii_digit() {
                return Self::string_pred(s);
            }
            let mut digit = (*ch as u8 - b'0') as u32;
            if digit == 0 {
                digit = radix - 1;
                *ch = (b'0' + digit as u8) as char;
            } else {
                digit -= 1;
                *ch = (b'0' + digit as u8) as char;
                borrow = false;
            }
        }
        if borrow {
            return Err(RuntimeError::new("Decrement out of range"));
        }
        Ok(chars.into_iter().collect())
    }

    // Compute the predecessor of a string (decrement the last character).
    // For multi-char alphabetic/digit strings, the leftmost character is never
    // removed; underflow (e.g. "AA".pred) is an error.
    pub(in crate::runtime) fn string_pred(s: &str) -> Result<String, RuntimeError> {
        if s.is_empty() {
            return Ok(String::new());
        }
        let mut chars: Vec<char> = s.chars().collect();
        // For single-char strings, just decrement the codepoint
        if chars.len() == 1 {
            let ch = chars[0];
            if let Some(prev) = char::from_u32(ch as u32 - 1) {
                return Ok(prev.to_string());
            }
            return Ok(s.to_string());
        }
        // Multi-char: decrement from end with borrow
        let mut borrow = true;
        for ch in chars.iter_mut().rev() {
            if !borrow {
                break;
            }
            if ch.is_ascii_lowercase() {
                if *ch == 'a' {
                    *ch = 'z';
                } else {
                    *ch = (*ch as u8 - 1) as char;
                    borrow = false;
                }
            } else if ch.is_ascii_uppercase() {
                if *ch == 'A' {
                    *ch = 'Z';
                } else {
                    *ch = (*ch as u8 - 1) as char;
                    borrow = false;
                }
            } else if ch.is_ascii_digit() {
                if *ch == '0' {
                    *ch = '9';
                } else {
                    *ch = (*ch as u8 - 1) as char;
                    borrow = false;
                }
            } else {
                if let Some(prev) = char::from_u32(*ch as u32 - 1) {
                    *ch = prev;
                }
                borrow = false;
            }
        }
        if borrow && chars.len() > 1 {
            return Err(RuntimeError::new("Decrement out of range"));
        }
        Ok(chars.into_iter().collect())
    }

    // Add step to a sequence value, preserving type where possible
    pub(in crate::runtime) fn seq_add(val: &Value, step: f64) -> Value {
        match val {
            Value::Int(i) => {
                if step == step.floor() && step.abs() < i64::MAX as f64 {
                    Value::Int(*i + step as i64)
                } else {
                    Value::Num(*i as f64 + step)
                }
            }
            Value::Num(f) => Value::Num(*f + step),
            Value::Rat(n, d) => {
                if *d != 0 && step == step.floor() && step.abs() < i64::MAX as f64 {
                    make_rat(*n + step as i64 * *d, *d)
                } else {
                    Value::Num(*n as f64 / *d as f64 + step)
                }
            }
            _ => Value::Num(Self::seq_value_to_f64(val).unwrap_or(0.0) + step),
        }
    }

    // Multiply a sequence value by a rational ratio (num/den), preserving Rat type
    pub(in crate::runtime) fn seq_mul_rat(val: &Value, num: i64, den: i64) -> Value {
        match val {
            Value::Int(i) => {
                if let Some(product) = i.checked_mul(num) {
                    if den == 1 {
                        Value::Int(product)
                    } else {
                        make_rat(product, den)
                    }
                } else {
                    Value::Num(*i as f64 * num as f64 / den as f64)
                }
            }
            Value::Num(f) => {
                let result = *f * num as f64 / den as f64;
                Value::Num(result)
            }
            Value::Rat(n, d) => {
                if let (Some(new_num), Some(new_den)) = (n.checked_mul(num), d.checked_mul(den)) {
                    make_rat(new_num, new_den)
                } else {
                    Value::Num(*n as f64 / *d as f64 * num as f64 / den as f64)
                }
            }
            _ => Self::seq_mul(val, num as f64 / den as f64),
        }
    }

    // Multiply a sequence value by ratio, preserving type where possible
    pub(in crate::runtime) fn seq_mul(val: &Value, ratio: f64) -> Value {
        match val {
            Value::Int(i) => {
                let result = *i as f64 * ratio;
                if ratio == ratio.floor() && result.abs() < i64::MAX as f64 {
                    Value::Int(result as i64)
                } else {
                    Value::Num(result)
                }
            }
            Value::Num(f) => Value::Num(*f * ratio),
            Value::Rat(n, d) => {
                if *d != 0 && ratio == ratio.floor() && ratio.abs() < i64::MAX as f64 {
                    make_rat(*n * ratio as i64, *d)
                } else {
                    Value::Num(*n as f64 / *d as f64 * ratio)
                }
            }
            _ => Value::Num(Self::seq_value_to_f64(val).unwrap_or(0.0) * ratio),
        }
    }
}
