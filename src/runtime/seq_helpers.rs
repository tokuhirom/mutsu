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
            (_, Value::Sub(data)) => {
                let func = right.clone();
                let _ = data; // keep pattern match shape explicit for callable RHS
                if let Ok(result) = self.call_sub_value(func.clone(), vec![left.clone()], false) {
                    return result.truthy();
                }
                match self.call_sub_value(func, vec![], false) {
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
                    // Set $/ to a Match object with from/to/str and positional captures
                    if let Some((start, end)) = self.regex_find_first(pat, &text) {
                        let chars: Vec<char> = text.chars().collect();
                        let matched: String = chars[start..end].iter().collect();
                        let match_obj = Value::make_match_object_with_captures(
                            matched,
                            start as i64,
                            end as i64,
                            &captures.positional,
                            &captures.named,
                        );
                        self.env.insert("/".to_string(), match_obj);
                    }
                    // Set positional capture variables ($0, $1, etc.)
                    for (i, v) in captures.positional.iter().enumerate() {
                        self.env.insert(i.to_string(), Value::Str(v.clone()));
                    }
                    // Set named capture variables
                    for (k, v) in &captures.named {
                        let value = if v.len() == 1 {
                            Value::Str(v[0].clone())
                        } else {
                            Value::array(v.iter().cloned().map(Value::Str).collect())
                        };
                        self.env.insert(format!("<{}>", k), value);
                    }
                    return true;
                }
                self.env.insert("/".to_string(), Value::Nil);
                false
            }
            (_, Value::Junction { kind, values }) => match kind {
                crate::value::JunctionKind::Any => values.iter().any(|v| self.smart_match(left, v)),
                crate::value::JunctionKind::All => values.iter().all(|v| self.smart_match(left, v)),
                crate::value::JunctionKind::One => {
                    values.iter().filter(|v| self.smart_match(left, v)).count() == 1
                }
                crate::value::JunctionKind::None => {
                    values.iter().all(|v| !self.smart_match(left, v))
                }
            },
            // IO::Path/Str ~~ Pair(:e), :d, :f, :r, :w, :x file tests
            (_, Value::Pair(key, val))
                if matches!(val.as_ref(), Value::Bool(true))
                    && matches!(key.as_str(), "e" | "d" | "f" | "r" | "w" | "x" | "s" | "z") =>
            {
                let path_str = match left {
                    Value::Instance {
                        class_name,
                        attributes,
                        ..
                    } if class_name == "IO::Path" => {
                        attributes.get("path").map(|v| v.to_string_value())
                    }
                    Value::Str(s) => Some(s.clone()),
                    _ => None,
                };
                if let Some(p) = path_str {
                    let path = std::path::Path::new(&p);
                    match key.as_str() {
                        "e" => path.exists(),
                        "d" => path.is_dir(),
                        "f" => path.is_file(),
                        "r" => path.exists(), // simplified: exists = readable
                        "w" => path.exists(), // simplified
                        "x" => {
                            #[cfg(unix)]
                            {
                                use std::os::unix::fs::PermissionsExt;
                                std::fs::metadata(&p)
                                    .map(|m| m.permissions().mode() & 0o111 != 0)
                                    .unwrap_or(false)
                            }
                            #[cfg(not(unix))]
                            {
                                false
                            }
                        }
                        "s" => std::fs::metadata(&p).map(|m| m.len() > 0).unwrap_or(false),
                        "z" => std::fs::metadata(&p).map(|m| m.len() == 0).unwrap_or(false),
                        _ => false,
                    }
                } else {
                    false
                }
            }
            // Hash ~~ Pair: check that key exists in hash and value smartmatches
            (Value::Hash(map), Value::Pair(key, val)) => {
                if let Some(hash_val) = map.get(key.as_str()) {
                    self.smart_match(hash_val, val)
                } else {
                    // Key not in hash: compare against an undefined type object.
                    self.smart_match(&Value::Package("Mu".to_string()), val)
                }
            }
            // Hash ~~ Hash: structural equality (eqv)
            (Value::Hash(lmap), Value::Hash(rmap)) => {
                if lmap.len() != rmap.len() {
                    return false;
                }
                for (k, lv) in lmap.iter() {
                    match rmap.get(k) {
                        Some(rv) => {
                            if !self.smart_match(lv, rv) {
                                return false;
                            }
                        }
                        None => return false,
                    }
                }
                true
            }
            // Array ~~ Hash: check if any element exists as a key in the hash
            (Value::Array(items, ..), Value::Hash(map)) => items.iter().any(|item| {
                let key = item.to_string_value();
                map.contains_key(&key)
            }),
            // Regex ~~ Hash: check if any key matches the regex
            (Value::Regex(pat), Value::Hash(map)) => {
                for key in map.keys() {
                    if self.regex_find_first(pat, key).is_some() {
                        return true;
                    }
                }
                false
            }
            // Scalar ~~ Hash: check key existence
            (_, Value::Hash(map)) => {
                let key = left.to_string_value();
                map.contains_key(&key)
            }
            // When RHS is a type/Package, check type membership
            (_, Value::Package(type_name)) => {
                // Handle type smileys (:U, :D, :_)
                let (base_type, smiley) = super::types::strip_type_smiley(type_name);

                // A Package on the LHS is a type object - check type hierarchy
                if let Value::Package(left_name) = left {
                    let type_ok = if Self::type_matches(base_type, left_name) {
                        true
                    } else if let Some(class_def) = self.classes.get(left_name.as_str()) {
                        class_def
                            .parents
                            .clone()
                            .iter()
                            .any(|parent| Self::type_matches(base_type, parent))
                    } else {
                        false
                    };
                    if !type_ok {
                        return false;
                    }
                    // Check definedness constraint
                    return match smiley {
                        Some(":U") => true,  // Package is undefined
                        Some(":D") => false, // Package is not defined
                        _ => true,
                    };
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
            // IO::Path ~~ IO::Path: compare by path string value
            (
                Value::Instance {
                    class_name: cn_a,
                    attributes: attrs_a,
                    ..
                },
                Value::Instance {
                    class_name: cn_b,
                    attributes: attrs_b,
                    ..
                },
            ) if cn_a == "IO::Path" && cn_b == "IO::Path" => {
                let path_a = attrs_a.get("path").map(|v| v.to_string_value());
                let path_b = attrs_b.get("path").map(|v| v.to_string_value());
                path_a == path_b
            }
            // Instance identity: two instances match iff they have the same id
            (Value::Instance { id: id_a, .. }, Value::Instance { id: id_b, .. }) => id_a == id_b,
            // When RHS is a Bool, result is that Bool
            (_, Value::Bool(b)) => *b,
            // Instance identity
            (Value::Instance { .. }, _) | (_, Value::Instance { .. }) => false,
            // Range ~~ Range: LHS is subset of RHS.
            // Uses raw bound values. Exclusivity is compared pairwise:
            // if RHS excludes a bound, LHS must either also exclude it
            // or have a strictly interior value.
            (l, r) if l.is_range() && r.is_range() => {
                let r_str = Self::range_has_string_endpoints(r);
                let (_, _, l_es, l_ee) = Self::range_exclusivity(l);
                let (_, _, r_es, r_ee) = Self::range_exclusivity(r);
                if r_str {
                    let (l_min_s, l_max_s) = Self::range_raw_string_bounds(l);
                    let (r_min_s, r_max_s) = Self::range_raw_string_bounds(r);
                    let min_ok = if r_es {
                        l_min_s > r_min_s || (l_min_s == r_min_s && l_es)
                    } else {
                        l_min_s >= r_min_s
                    };
                    let max_ok = if r_ee {
                        l_max_s < r_max_s || (l_max_s == r_max_s && l_ee)
                    } else {
                        l_max_s <= r_max_s
                    };
                    min_ok && max_ok
                } else {
                    let (l_min, l_max) = Self::range_raw_bounds_f64(l);
                    let (r_min, r_max) = Self::range_raw_bounds_f64(r);
                    let min_ok = if r_es {
                        l_min > r_min || (l_min == r_min && l_es)
                    } else {
                        l_min >= r_min
                            || (l_min.is_nan() && r_min.is_nan())
                            || (l_min.is_nan() && r_min.is_infinite() && r_min < 0.0)
                    };
                    let max_ok = if r_ee {
                        l_max < r_max || (l_max == r_max && l_ee)
                    } else {
                        l_max <= r_max
                            || (l_max.is_nan() && r_max.is_nan())
                            || (l_max.is_nan() && r_max.is_infinite() && r_max > 0.0)
                    };
                    min_ok && max_ok
                }
            }
            // Range ~~ Numeric: numify range (element count) and compare with ==
            (l, r) if l.is_range() && r.is_numeric() => {
                let elems = Self::range_elems_f64(l);
                let rval = r.to_f64();
                elems == rval
            }
            // Value ~~ Range: check if value is contained in the range
            (l, r) if r.is_range() => Self::value_in_range(l, r),
            // Default: compare equality
            _ => left.to_string_value() == right.to_string_value(),
        }
    }

    /// Get raw bounds of a range as f64 (NOT adjusted for exclusivity).
    fn range_raw_bounds_f64(v: &Value) -> (f64, f64) {
        match v {
            Value::Range(a, b) => (*a as f64, *b as f64),
            Value::RangeExcl(a, b) => (*a as f64, *b as f64),
            Value::RangeExclStart(a, b) => (*a as f64, *b as f64),
            Value::RangeExclBoth(a, b) => (*a as f64, *b as f64),
            Value::GenericRange { start, end, .. } => (start.to_f64(), end.to_f64()),
            _ => (0.0, 0.0),
        }
    }

    /// Get exclusivity flags for a range: (start_val, end_val, excl_start, excl_end).
    fn range_exclusivity(v: &Value) -> (f64, f64, bool, bool) {
        match v {
            Value::Range(a, b) => (*a as f64, *b as f64, false, false),
            Value::RangeExcl(a, b) => (*a as f64, *b as f64, false, true),
            Value::RangeExclStart(a, b) => (*a as f64, *b as f64, true, false),
            Value::RangeExclBoth(a, b) => (*a as f64, *b as f64, true, true),
            Value::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => (start.to_f64(), end.to_f64(), *excl_start, *excl_end),
            _ => (0.0, 0.0, false, false),
        }
    }

    /// Check if a range has string endpoints.
    fn range_has_string_endpoints(v: &Value) -> bool {
        match v {
            Value::GenericRange { start, end, .. } => {
                matches!(**start, Value::Str(_)) || matches!(**end, Value::Str(_))
            }
            _ => false,
        }
    }

    /// Get raw string bounds of a range.
    fn range_raw_string_bounds(v: &Value) -> (String, String) {
        match v {
            Value::GenericRange { start, end, .. } => {
                (start.to_string_value(), end.to_string_value())
            }
            Value::Range(a, b) => (a.to_string(), b.to_string()),
            Value::RangeExcl(a, b) => (a.to_string(), b.to_string()),
            Value::RangeExclStart(a, b) => (a.to_string(), b.to_string()),
            Value::RangeExclBoth(a, b) => (a.to_string(), b.to_string()),
            _ => (String::new(), String::new()),
        }
    }

    /// Compute element count of a range as f64.
    fn range_elems_f64(v: &Value) -> f64 {
        match v {
            Value::Range(a, b) => {
                if *b == i64::MAX || *a == i64::MIN {
                    f64::INFINITY
                } else {
                    (*b - *a + 1) as f64
                }
            }
            Value::RangeExcl(a, b) => {
                if *b == i64::MAX || *a == i64::MIN {
                    f64::INFINITY
                } else {
                    (*b - *a) as f64
                }
            }
            Value::RangeExclStart(a, b) => {
                if *b == i64::MAX || *a == i64::MIN {
                    f64::INFINITY
                } else {
                    (*b - *a) as f64
                }
            }
            Value::RangeExclBoth(a, b) => {
                if *b == i64::MAX || *a == i64::MIN {
                    f64::INFINITY
                } else {
                    (*b - *a - 1) as f64
                }
            }
            Value::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                let s = start.to_f64();
                let e = end.to_f64();
                let count = e - s + 1.0;
                let adj = if *excl_start { 1.0 } else { 0.0 } + if *excl_end { 1.0 } else { 0.0 };
                count - adj
            }
            _ => 0.0,
        }
    }

    /// Check if a value is contained within a range.
    fn value_in_range(val: &Value, range: &Value) -> bool {
        let (r_min, r_max) = Self::range_raw_bounds_f64(range);
        let (_, _, r_es, r_ee) = Self::range_exclusivity(range);

        // For string ranges, compare strings
        if Self::range_has_string_endpoints(range) {
            let v_str = val.to_string_value();
            let (r_min_s, r_max_s) = Self::range_raw_string_bounds(range);
            let min_ok = if r_es {
                v_str > r_min_s
            } else {
                v_str >= r_min_s
            };
            let max_ok = if r_ee {
                v_str < r_max_s
            } else {
                v_str <= r_max_s
            };
            return min_ok && max_ok;
        }

        let v = val.to_f64();
        let min_ok = if r_es { v > r_min } else { v >= r_min };
        let max_ok = if r_ee { v < r_max } else { v <= r_max };
        min_ok && max_ok
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

    /// Check if a value matches a type name for sequence type endpoints.
    /// Handles the Num→Rat mapping since mutsu uses Num for decimal values.
    pub(super) fn seq_type_matches(val: &Value, type_name: &str) -> bool {
        let actual = super::value_type_name(val);
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

    pub(super) fn seq_values_equal(a: &Value, b: &Value) -> bool {
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

    // Compute the predecessor of a string (decrement the last character)
    pub(super) fn string_pred(s: &str) -> String {
        if s.is_empty() {
            return String::new();
        }
        let mut chars: Vec<char> = s.chars().collect();
        // For single-char strings, just decrement the codepoint
        if chars.len() == 1 {
            let ch = chars[0];
            if let Some(prev) = char::from_u32(ch as u32 - 1) {
                return prev.to_string();
            }
            return s.to_string();
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
            // Remove leading character on borrow
            chars.remove(0);
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

    // Multiply a sequence value by a rational ratio (num/den), preserving Rat type
    pub(super) fn seq_mul_rat(val: &Value, num: i64, den: i64) -> Value {
        match val {
            Value::Int(i) => {
                // In geometric sequences with rational ratio, always produce Rat
                if let Some(product) = i.checked_mul(num) {
                    make_rat(product, den)
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
