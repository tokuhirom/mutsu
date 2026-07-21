use super::*;

/// Fold a string for `.contains` matching: `:ignoremark` strips combining marks
/// (NFD, drop combining code points), `:ignorecase` lowercases. Applied to both
/// the haystack and each needle so the comparison is symmetric.
fn fold_for_contains(s: &str, ignore_case: bool, ignore_mark: bool) -> String {
    use unicode_normalization::UnicodeNormalization;
    let stripped: String = if ignore_mark {
        s.nfd()
            .filter(|c| !unicode_normalization::char::is_combining_mark(*c))
            .collect()
    } else {
        s.to_string()
    };
    if ignore_case {
        stripped.to_lowercase()
    } else {
        stripped
    }
}

impl Interpreter {
    pub(super) fn dispatch_contains(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut positional: Vec<Value> = Vec::new();
        let mut ignore_case = false;
        let mut ignore_mark = false;
        for arg in args {
            if let ValueView::Pair(key, value) = arg.view() {
                match key.as_str() {
                    "i" | "ignorecase" => ignore_case = value.truthy(),
                    "m" | "ignoremark" => ignore_mark = value.truthy(),
                    _ => {}
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
            match pos.view() {
                ValueView::Int(i) => i,
                ValueView::Num(f) => f as i64,
                ValueView::Str(s) => s.parse::<i64>().unwrap_or(0),
                ValueView::BigInt(b) => {
                    if b.as_ref() > &num_bigint::BigInt::from(i64::MAX) {
                        return Ok(RuntimeError::out_of_range_failure(
                            "start",
                            Value::bigint_arc(b.clone()),
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
                Value::int(start),
                &format!("0..{}", len),
            ));
        }
        let hay: String = text.chars().skip(start as usize).collect();
        // A Regex needle means "does the pattern match anywhere from `start`?"
        // (`"abc".contains(/b/)`), not a literal search for the regex's gist.
        if let ValueView::Regex(pattern) = needle.view() {
            let found = self.regex_find_first(&pattern, &hay).is_some();
            return Ok(Value::truth(found));
        }
        // `:ignorecase`/`:ignoremark` fold the haystack once here; each needle is
        // folded the same way inside contains_value.
        let folded_hay = fold_for_contains(&hay, ignore_case, ignore_mark);
        Ok(Self::contains_value(
            &folded_hay,
            &needle,
            ignore_case,
            ignore_mark,
        ))
    }

    fn contains_value(hay: &str, needle: &Value, ignore_case: bool, ignore_mark: bool) -> Value {
        match needle.view() {
            ValueView::Junction { kind, values } => {
                let mapped = values
                    .iter()
                    .map(|v| Self::contains_value(hay, v, ignore_case, ignore_mark))
                    .collect::<Vec<_>>();
                Value::junction(kind, mapped)
            }
            _ => {
                let needle = fold_for_contains(&needle.to_string_value(), ignore_case, ignore_mark);
                let ok = hay.contains(&needle);
                Value::truth(ok)
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
            if let ValueView::Pair(key, value) = arg.view() {
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
        if let Some(ValueView::Package(type_name)) = positional.first().map(Value::view) {
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
        Ok(Value::truth(ok))
    }
}
