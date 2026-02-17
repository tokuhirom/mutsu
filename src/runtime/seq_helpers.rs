use super::*;

impl Interpreter {
    pub(super) fn smart_match(&mut self, left: &Value, right: &Value) -> bool {
        match (left, right) {
            (Value::Version { .. }, Value::Version { parts, plus, minus }) => {
                Self::version_smart_match(left, parts, *plus, *minus)
            }
            // When RHS is a callable (Sub), invoke it with LHS as argument and
            // return truthiness of the result.  If the sub accepts no parameters,
            // call it with no arguments (simple closure truth).
            (_, Value::Sub { params, .. }) => {
                let func = right.clone();
                let args = if params.is_empty() {
                    vec![]
                } else {
                    vec![left.clone()]
                };
                match self.call_sub_value(func, args, false) {
                    Ok(result) => result.truthy(),
                    Err(_) => false,
                }
            }
            // Built-in routines used as callables in smartmatch
            (_, Value::Routine { .. }) => {
                let func = right.clone();
                match self.call_sub_value(func, vec![left.clone()], false) {
                    Ok(result) => result.truthy(),
                    Err(_) => false,
                }
            }
            (_, Value::Regex(pat)) => {
                let text = left.to_string_value();
                if let Some(captures) = self.regex_match_with_captures(pat, &text) {
                    // Set $/ to the matched substring
                    if let Some((start, end)) = self.regex_find_first(pat, &text) {
                        let chars: Vec<char> = text.chars().collect();
                        let matched: String = chars[start..end].iter().collect();
                        self.env.insert("/".to_string(), Value::Str(matched));
                    }
                    for (k, v) in captures {
                        self.env.insert(format!("<{}>", k), Value::Str(v));
                    }
                    return true;
                }
                self.env.insert("/".to_string(), Value::Nil);
                false
            }
            // Hash ~~ Pair: check that key exists in hash and value smartmatches
            (Value::Hash(map), Value::Pair(key, val)) => {
                if let Some(hash_val) = map.get(key.as_str()) {
                    self.smart_match(hash_val, val)
                } else {
                    // Key not in hash: check if the pair value matches Nil/Any
                    self.smart_match(&Value::Nil, val)
                }
            }
            // Scalar ~~ Hash: check key existence
            (_, Value::Hash(map)) => {
                let key = left.to_string_value();
                map.contains_key(&key)
            }
            // When RHS is a type/Package, check type membership
            (_, Value::Package(type_name)) => {
                // A Package on the LHS is a type object - only matches the same type
                if let Value::Package(left_name) = left {
                    return Self::type_matches(type_name, left_name);
                }
                self.type_matches_value(type_name, left)
            }
            // When LHS is a type object (Package), only match same type or type hierarchy
            (Value::Package(_), _) => false,
            // When RHS is NaN, check if LHS is also NaN
            (_, Value::Num(b)) if b.is_nan() => Self::value_is_nan(left),
            (Value::Num(a), _) if a.is_nan() => Self::value_is_nan(right),
            // Complex comparison (NaN-aware: any NaN component means NaN smartmatch)
            (Value::Complex(ar, ai), Value::Complex(br, bi)) => {
                let a_nan = ar.is_nan() || ai.is_nan();
                let b_nan = br.is_nan() || bi.is_nan();
                if a_nan && b_nan {
                    true
                } else if a_nan || b_nan {
                    false
                } else {
                    ar == br && ai == bi
                }
            }
            (Value::Int(a), Value::Complex(br, bi)) => (*a as f64) == *br && *bi == 0.0,
            (Value::Complex(ar, ai), Value::Int(b)) => *ar == (*b as f64) && *ai == 0.0,
            (Value::Num(a), Value::Complex(br, bi)) => {
                if a.is_nan() && (br.is_nan() || bi.is_nan()) {
                    true
                } else {
                    *a == *br && *bi == 0.0
                }
            }
            (Value::Complex(ar, ai), Value::Num(b)) => {
                if b.is_nan() && (ar.is_nan() || ai.is_nan()) {
                    true
                } else {
                    *ar == *b && *ai == 0.0
                }
            }
            (Value::Complex(ar, ai), Value::Rat(n, d)) => {
                if *d != 0 {
                    *ar == (*n as f64 / *d as f64) && *ai == 0.0
                } else {
                    false
                }
            }
            (Value::Rat(n, d), Value::Complex(br, bi)) => {
                if *d != 0 {
                    (*n as f64 / *d as f64) == *br && *bi == 0.0
                } else {
                    false
                }
            }
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Num(a), Value::Num(b)) => a == b,
            (Value::Int(a), Value::Num(b)) => (*a as f64) == *b,
            (Value::Num(a), Value::Int(b)) => *a == (*b as f64),
            (Value::Rat(an, ad), Value::Rat(bn, bd)) => an * bd == bn * ad,
            (Value::Int(a), Value::Rat(n, d)) => *a * d == *n,
            (Value::Rat(n, d), Value::Int(b)) => *n == *b * d,
            (Value::Str(a), Value::Str(b)) => a == b,
            // Str ~~ Numeric: numify LHS and compare
            (Value::Str(a), Value::Int(b)) => a.trim().parse::<f64>() == Ok(*b as f64),
            (Value::Str(a), Value::Num(b)) => a.trim().parse::<f64>().is_ok_and(|v| {
                if v.is_nan() && b.is_nan() {
                    true
                } else {
                    v == *b
                }
            }),
            (Value::Str(a), Value::Rat(n, d)) => {
                if *d != 0 {
                    a.trim()
                        .parse::<f64>()
                        .is_ok_and(|v| v == *n as f64 / *d as f64)
                } else {
                    false
                }
            }
            (Value::Int(a), Value::Str(b)) => b.trim().parse::<f64>() == Ok(*a as f64),
            (Value::Nil, Value::Str(s)) => s.is_empty(),
            // Instance identity: two instances match iff they have the same id
            (Value::Instance { id: id_a, .. }, Value::Instance { id: id_b, .. }) => id_a == id_b,
            // When RHS is a Bool, result is that Bool
            (_, Value::Bool(b)) => *b,
            // Instance identity
            (Value::Instance { .. }, _) | (_, Value::Instance { .. }) => false,
            // Range ~~ Range: LHS is subset of RHS (all range variants)
            (l, r) if l.is_range() && r.is_range() => {
                let (l_min, l_max) = Self::range_effective_bounds(l);
                let (r_min, r_max) = Self::range_effective_bounds(r);
                l_min >= r_min && l_max <= r_max
            }
            // Default: compare equality
            _ => left.to_string_value() == right.to_string_value(),
        }
    }

    /// Get effective inclusive bounds of a range (accounting for exclusivity).
    fn range_effective_bounds(v: &Value) -> (i64, i64) {
        match v {
            Value::Range(a, b) => (*a, *b),
            Value::RangeExcl(a, b) => (*a, *b - 1),
            Value::RangeExclStart(a, b) => (*a + 1, *b),
            Value::RangeExclBoth(a, b) => (*a + 1, *b - 1),
            _ => (0, 0),
        }
    }

    pub(super) fn seq_value_to_f64(v: &Value) -> Option<f64> {
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

    pub(super) fn seq_values_equal(a: &Value, b: &Value) -> bool {
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
    pub(super) fn string_succ(s: &str) -> String {
        if s.is_empty() {
            return String::new();
        }
        let mut chars: Vec<char> = s.chars().collect();
        // Carry-over increment from the last character
        let mut carry = true;
        for ch in chars.iter_mut().rev() {
            if !carry {
                break;
            }
            if ch.is_ascii_lowercase() {
                if *ch == 'z' {
                    *ch = 'a';
                } else {
                    *ch = (*ch as u8 + 1) as char;
                    carry = false;
                }
            } else if ch.is_ascii_uppercase() {
                if *ch == 'Z' {
                    *ch = 'A';
                } else {
                    *ch = (*ch as u8 + 1) as char;
                    carry = false;
                }
            } else if ch.is_ascii_digit() {
                if *ch == '9' {
                    *ch = '0';
                } else {
                    *ch = (*ch as u8 + 1) as char;
                    carry = false;
                }
            } else {
                *ch = char::from_u32(*ch as u32 + 1).unwrap_or(*ch);
                carry = false;
            }
        }
        if carry {
            // All characters carried over, prepend appropriate char
            let first = chars[0];
            let prefix = if first.is_ascii_lowercase() {
                'a'
            } else if first.is_ascii_uppercase() {
                'A'
            } else if first.is_ascii_digit() {
                '1'
            } else {
                first
            };
            chars.insert(0, prefix);
        }
        chars.into_iter().collect()
    }

    // Add step to a sequence value, preserving type where possible
    pub(super) fn seq_add(val: &Value, step: f64) -> Value {
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

    // Multiply a sequence value by ratio, preserving type where possible
    pub(super) fn seq_mul(val: &Value, ratio: f64) -> Value {
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
