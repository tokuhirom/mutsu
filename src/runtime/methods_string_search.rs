use super::*;

impl Interpreter {
    pub(super) fn dispatch_contains(
        &self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut positional: Vec<Value> = Vec::new();
        let mut ignore_case = false;
        for arg in args {
            if let Value::Pair(key, value) = arg {
                if matches!(key.as_str(), "i" | "ignorecase" | "m" | "ignoremark") {
                    ignore_case = value.truthy();
                }
            } else {
                positional.push(arg.clone());
            }
        }
        let needle = positional
            .first()
            .cloned()
            .unwrap_or(Value::str(String::new()));
        let start = if let Some(pos) = positional.get(1) {
            match pos {
                Value::Int(i) => *i,
                Value::Num(f) => *f as i64,
                Value::Str(s) => s.parse::<i64>().unwrap_or(0),
                Value::BigInt(b) => {
                    if b.as_ref() > &num_bigint::BigInt::from(i64::MAX) {
                        return Ok(RuntimeError::out_of_range_failure(
                            "start",
                            Value::BigInt(b.clone()),
                            "0..Inf",
                        ));
                    }
                    b.to_string().parse::<i64>().unwrap_or(0)
                }
                _ => 0,
            }
        } else {
            0
        };
        let text = target.to_string_value();
        let len = text.chars().count() as i64;
        if start < 0 || start > len {
            return Ok(RuntimeError::out_of_range_failure(
                "start",
                Value::Int(start),
                &format!("0..{}", len),
            ));
        }
        let hay: String = text.chars().skip(start as usize).collect();
        Ok(Self::contains_value(&hay, &needle, ignore_case))
    }

    fn contains_value(hay: &str, needle: &Value, ignore_case: bool) -> Value {
        match needle {
            Value::Junction { kind, values } => {
                let mapped = values
                    .iter()
                    .map(|v| Self::contains_value(hay, v, ignore_case))
                    .collect::<Vec<_>>();
                Value::junction(kind.clone(), mapped)
            }
            _ => {
                let needle = needle.to_string_value();
                let ok = if ignore_case {
                    hay.to_lowercase().contains(&needle.to_lowercase())
                } else {
                    hay.contains(&needle)
                };
                Value::Bool(ok)
            }
        }
    }

    pub(super) fn dispatch_starts_with(
        &self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        self.dispatch_prefix_suffix_check(target, args, true)
    }

    pub(super) fn dispatch_ends_with(
        &self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        self.dispatch_prefix_suffix_check(target, args, false)
    }

    pub(super) fn dispatch_prefix_suffix_check(
        &self,
        target: Value,
        args: &[Value],
        is_prefix: bool,
    ) -> Result<Value, RuntimeError> {
        let method_name = if is_prefix {
            "starts-with"
        } else {
            "ends-with"
        };
        // Separate positional and named args first
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
        // Type objects (Package) as needle should throw
        if let Some(Value::Package(type_name)) = positional.first() {
            return Err(RuntimeError::new(format!(
                "Cannot resolve caller {}({}:U)",
                method_name, type_name
            )));
        }
        let needle = positional
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let text = target.to_string_value();

        let ok = match (ignore_case, ignore_mark) {
            (false, false) => {
                if is_prefix {
                    text.starts_with(needle.as_str())
                } else {
                    text.ends_with(needle.as_str())
                }
            }
            (true, false) => {
                let t = text.to_lowercase();
                let n = needle.to_lowercase();
                if is_prefix {
                    t.starts_with(n.as_str())
                } else {
                    t.ends_with(n.as_str())
                }
            }
            (false, true) => {
                let t = self.strip_marks(&text);
                let n = self.strip_marks(&needle);
                if is_prefix {
                    t.starts_with(n.as_str())
                } else {
                    t.ends_with(n.as_str())
                }
            }
            (true, true) => {
                let t = self.strip_marks(&text).to_lowercase();
                let n = self.strip_marks(&needle).to_lowercase();
                if is_prefix {
                    t.starts_with(n.as_str())
                } else {
                    t.ends_with(n.as_str())
                }
            }
        };
        Ok(Value::Bool(ok))
    }
}
