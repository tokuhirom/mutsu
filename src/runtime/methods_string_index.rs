use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(super) fn dispatch_index(
        &self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Type objects (Package) as needle are not allowed
        if let Some(Value::Package(type_name)) = args.first() {
            return Err(RuntimeError::new(format!(
                "Cannot resolve caller index({}:U)",
                type_name
            )));
        }
        let mut positional: Vec<Value> = Vec::new();
        let mut ignore_case = false;
        let mut ignore_mark = false;
        for arg in args {
            if let Value::Pair(key, value) = arg {
                match key.as_str() {
                    "i" | "ignorecase" => ignore_case = value.truthy(),
                    "m" | "ignoremark" => ignore_mark = value.truthy(),
                    _ => {}
                }
            } else {
                positional.push(arg.clone());
            }
        }
        // Handle list of needles: \(<a o>) passes an Array as first arg
        let needles: Vec<String> = if let Some(Value::Array(items, ..)) = positional.first() {
            items.iter().map(|v| v.to_string_value()).collect()
        } else {
            vec![
                positional
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default(),
            ]
        };
        let start = if let Some(pos) = positional.get(1) {
            match self.value_to_position(pos) {
                Ok(v) => v,
                Err(err) => {
                    // Convert out-of-range errors to Failures
                    return Ok(Self::runtime_error_to_failure(err));
                }
            }
        } else {
            0
        };
        let text = target.to_string_value();
        let len = text.chars().count() as i64;
        if start < 0 {
            return Ok(RuntimeError::out_of_range_failure(
                "start",
                Value::Int(start),
                &format!("0..{}", len),
            ));
        }
        if start > len {
            return Ok(Value::Nil);
        }
        let hay: String = text.chars().skip(start as usize).collect();
        let mut best: Option<usize> = None;
        for needle in &needles {
            let pos = if ignore_case && ignore_mark {
                self.index_ignorecase_ignoremark(&hay, needle)
            } else if ignore_case {
                self.index_ignorecase(&hay, needle)
            } else if ignore_mark {
                self.index_ignoremark(&hay, needle)
            } else {
                hay.find(needle.as_str()).map(|p| hay[..p].chars().count())
            };
            if let Some(char_pos) = pos {
                best = Some(match best {
                    Some(prev) => prev.min(char_pos),
                    None => char_pos,
                });
            }
        }
        match best {
            Some(char_pos) => Ok(Value::Int(char_pos as i64 + start)),
            None => Ok(Value::Nil),
        }
    }

    /// Str.indices(needle, pos?, :overlap, :i, :ignorecase, :m, :ignoremark)
    pub(super) fn dispatch_indices(
        &self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if let Some(Value::Package(type_name)) = args.first() {
            return Err(RuntimeError::new(format!(
                "Cannot resolve caller indices({}:U)",
                type_name
            )));
        }
        let mut positional: Vec<Value> = Vec::new();
        let mut ignore_case = false;
        let mut ignore_mark = false;
        let mut overlap = false;
        for arg in args {
            if let Value::Pair(key, value) = arg {
                match key.as_str() {
                    "i" | "ignorecase" => ignore_case = value.truthy(),
                    "m" | "ignoremark" => ignore_mark = value.truthy(),
                    "overlap" => overlap = value.truthy(),
                    _ => {}
                }
            } else {
                positional.push(arg.clone());
            }
        }
        let needle = positional
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let start = if let Some(pos) = positional.get(1) {
            match self.value_to_position(pos) {
                Ok(v) => v,
                Err(err) => {
                    return Ok(Self::runtime_error_to_failure(err));
                }
            }
        } else {
            0
        };
        let text = target.to_string_value();
        let len = text.chars().count() as i64;
        if start < 0 {
            return Ok(RuntimeError::out_of_range_failure(
                "start",
                Value::Int(start),
                &format!("0..{}", len),
            ));
        }
        let text_chars: Vec<char> = text.chars().collect();
        let mut results: Vec<Value> = Vec::new();
        if needle.is_empty() {
            for i in (start as usize)..=text_chars.len() {
                results.push(Value::Int(i as i64));
            }
        } else {
            let needle_len = needle.chars().count();
            let mut pos = start as usize;
            while pos <= text_chars.len() {
                let hay: String = text_chars[pos..].iter().collect();
                let found = if ignore_case && ignore_mark {
                    self.index_ignorecase_ignoremark(&hay, &needle)
                } else if ignore_case {
                    self.index_ignorecase(&hay, &needle)
                } else if ignore_mark {
                    self.index_ignoremark(&hay, &needle)
                } else {
                    hay.find(needle.as_str()).map(|p| hay[..p].chars().count())
                };
                match found {
                    Some(char_pos) => {
                        let absolute_pos = pos + char_pos;
                        results.push(Value::Int(absolute_pos as i64));
                        if overlap {
                            pos = absolute_pos + 1;
                        } else {
                            pos = absolute_pos + needle_len;
                        }
                    }
                    None => break,
                }
            }
        }
        Ok(Value::Array(
            std::sync::Arc::new(crate::value::ArrayData::new(results)),
            crate::value::ArrayKind::List,
        ))
    }

    pub(super) fn dispatch_rindex(
        &self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if let Some(Value::Package(type_name)) = args.first() {
            return Err(RuntimeError::new(format!(
                "Cannot resolve caller rindex({}:U)",
                type_name
            )));
        }
        let mut positional: Vec<Value> = Vec::new();
        for arg in args {
            if !matches!(arg, Value::Pair(..)) {
                positional.push(arg.clone());
            }
        }
        // Handle list of needles
        let needles: Vec<String> = if let Some(Value::Array(items, ..)) = positional.first() {
            items.iter().map(|v| v.to_string_value()).collect()
        } else {
            vec![
                positional
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default(),
            ]
        };
        let text = target.to_string_value();
        let char_len = text.chars().count() as i64;
        // Optional position argument (maximum char index to consider)
        let max_pos = if let Some(pos_val) = positional.get(1) {
            // Check for negative values first (returns Failure, not exception)
            let is_negative = match pos_val {
                Value::Int(i) => *i < 0,
                Value::Num(f) => *f < 0.0,
                _ => false,
            };
            if is_negative {
                let got = pos_val.clone();
                let mut ex_attrs = std::collections::HashMap::new();
                ex_attrs.insert(
                    "message".to_string(),
                    Value::str("Attempt to use negative position with rindex".to_string()),
                );
                ex_attrs.insert("got".to_string(), got);
                let exception = Value::make_instance(Symbol::intern("X::OutOfRange"), ex_attrs);
                let mut failure_attrs = std::collections::HashMap::new();
                failure_attrs.insert("exception".to_string(), exception);
                failure_attrs.insert("handled".to_string(), Value::Bool(false));
                return Ok(Value::make_instance(
                    Symbol::intern("Failure"),
                    failure_attrs,
                ));
            }
            // For large out-of-range values, return a Failure wrapping X::OutOfRange
            let pos = match self.value_to_position(pos_val) {
                Ok(p) => p,
                Err(_) => {
                    let got = pos_val.clone();
                    let mut ex_attrs = std::collections::HashMap::new();
                    ex_attrs.insert(
                        "message".to_string(),
                        Value::str("Position out of range".to_string()),
                    );
                    ex_attrs.insert("got".to_string(), got);
                    let exception = Value::make_instance(Symbol::intern("X::OutOfRange"), ex_attrs);
                    let mut failure_attrs = std::collections::HashMap::new();
                    failure_attrs.insert("exception".to_string(), exception);
                    failure_attrs.insert("handled".to_string(), Value::Bool(false));
                    return Ok(Value::make_instance(
                        Symbol::intern("Failure"),
                        failure_attrs,
                    ));
                }
            };
            if pos > char_len {
                return Ok(Value::Nil);
            }
            Some(pos as usize)
        } else {
            None
        };
        // For empty needle, return max_pos or char_len
        if needles.len() == 1 && needles[0].is_empty() {
            return match max_pos {
                Some(p) => Ok(Value::Int(p as i64)),
                None => Ok(Value::Int(char_len)),
            };
        }
        let mut best: Option<usize> = None;
        for needle in &needles {
            // Search the entire string with rfind
            let pos = {
                let chars: Vec<char> = text.chars().collect();
                let n_chars: Vec<char> = needle.chars().collect();
                if n_chars.is_empty() {
                    match max_pos {
                        Some(p) => Some(p),
                        None => Some(chars.len()),
                    }
                } else if n_chars.len() > chars.len() {
                    None
                } else {
                    // max_start: the highest starting position where a match can begin
                    let max_start = match max_pos {
                        Some(p) => p.min(chars.len() - n_chars.len()),
                        None => chars.len() - n_chars.len(),
                    };
                    let mut found = None;
                    for i in (0..=max_start).rev() {
                        if chars[i..i + n_chars.len()] == n_chars[..] {
                            found = Some(i);
                            break;
                        }
                    }
                    found
                }
            };
            if let Some(char_pos) = pos {
                best = Some(match best {
                    Some(prev) => prev.max(char_pos),
                    None => char_pos,
                });
            }
        }
        match best {
            Some(char_pos) => Ok(Value::Int(char_pos as i64)),
            None => Ok(Value::Nil),
        }
    }

    pub(super) fn index_ignorecase(&self, hay: &str, needle: &str) -> Option<usize> {
        let hay_lower = hay.to_lowercase();
        let needle_lower = needle.to_lowercase();
        hay_lower
            .find(&needle_lower)
            .map(|byte_pos| hay_lower[..byte_pos].chars().count())
    }

    pub(super) fn index_ignoremark(&self, hay: &str, needle: &str) -> Option<usize> {
        let hay_stripped = self.strip_marks(hay);
        let needle_stripped = self.strip_marks(needle);
        hay_stripped
            .find(&needle_stripped)
            .map(|byte_pos| hay_stripped[..byte_pos].chars().count())
    }

    pub(super) fn index_ignorecase_ignoremark(&self, hay: &str, needle: &str) -> Option<usize> {
        let hay_stripped = self.strip_marks(hay).to_lowercase();
        let needle_stripped = self.strip_marks(needle).to_lowercase();
        hay_stripped
            .find(&needle_stripped)
            .map(|byte_pos| hay_stripped[..byte_pos].chars().count())
    }

    pub(super) fn strip_marks(&self, s: &str) -> String {
        use unicode_normalization::UnicodeNormalization;
        s.nfd()
            .filter(|c| !unicode_normalization::char::is_combining_mark(*c))
            .collect()
    }

    /// Convert a RuntimeError to a Failure value (for operations that should soft-fail).
    pub(super) fn runtime_error_to_failure(err: RuntimeError) -> Value {
        let ex = if let Some(exception) = err.exception {
            *exception
        } else {
            let mut attrs = HashMap::new();
            attrs.insert("message".to_string(), Value::str(err.message));
            Value::make_instance(Symbol::intern("X::AdHoc"), attrs)
        };
        let mut failure_attrs = HashMap::new();
        failure_attrs.insert("exception".to_string(), ex);
        Value::make_instance(Symbol::intern("Failure"), failure_attrs)
    }

    pub(super) fn out_of_range_error(&self, got: Value) -> RuntimeError {
        let mut attrs = HashMap::new();
        attrs.insert("got".to_string(), got);
        attrs.insert("message".to_string(), Value::str_from("X::OutOfRange"));
        let ex = Value::make_instance(Symbol::intern("X::OutOfRange"), attrs);
        let mut err = RuntimeError::new("X::OutOfRange".to_string());
        err.exception = Some(Box::new(ex));
        err
    }

    pub(super) fn value_to_position(&self, pos: &Value) -> Result<i64, RuntimeError> {
        match pos {
            Value::Int(i) => Ok(*i),
            Value::Num(f) => {
                if f.abs() > i64::MAX as f64 {
                    Err(self.out_of_range_error(Value::Num(*f)))
                } else {
                    Ok(*f as i64)
                }
            }
            Value::Rat(n, d) => {
                if *d == 0 {
                    Err(self.out_of_range_error(Value::Rat(*n, *d)))
                } else {
                    Ok(*n / *d)
                }
            }
            Value::Str(s) => Ok(s.parse::<i64>().unwrap_or(0)),
            Value::BigInt(b) => {
                if b.as_ref() > &num_bigint::BigInt::from(i64::MAX)
                    || b.as_ref() < &num_bigint::BigInt::from(i64::MIN)
                {
                    Err(self.out_of_range_error(Value::bigint((**b).clone())))
                } else {
                    Ok(b.to_string().parse::<i64>().unwrap_or(0))
                }
            }
            _ => Ok(0),
        }
    }
}
