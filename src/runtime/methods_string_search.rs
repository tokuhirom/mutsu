use super::*;

impl Interpreter {
    // Cost: O(d * m) amortized, d = chars searched from `$pos`, m = chars of
    // the needle, once the invocant's grapheme index is cached (`$pos` is
    // resolved through it; the suffix is borrowed). :i/:m fold the suffix,
    // O(d + m) extra.
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
        // `start` is a grapheme index, resolved through the cached index (#9140).
        let (text, idx) = crate::builtins::grapheme_index::str_and_index(&target);
        let len = idx.len() as i64;
        if start < 0 || start > len {
            return Ok(RuntimeError::out_of_range_failure(
                "start",
                Value::int(start),
                &format!("0..{}", len),
            ));
        }
        // Cost (Regex needle): O(1) setup for a `Str` invocant matched recently
        // (its MatchTarget is cached per payload), O(n) the first time, n =
        // chars of the invocant; plus O(start) to map `$pos`, plus the search.
        // A Regex needle means "does the pattern match anywhere from `start`?"
        // (`"abc".contains(/b/)`), not a literal search for the regex's gist.
        // It matches against the WHOLE subject from `$pos` (`:c($pos)`), not a
        // copied suffix, so a lookbehind or `<<` sees what precedes `$pos` and
        // `^` stays anchored at the start of the string.
        if let ValueView::Regex(..) = needle.view() {
            let from = idx.byte_at(&text, start as usize);
            let target = crate::runtime::MatchTarget::of_subject(&text);
            let min_pos = text[..from].chars().count();
            let found = self
                .regex_find_first_from_with_all_captures_in_value(&needle, &target, min_pos)
                .is_some();
            return Ok(Value::truth(found));
        }
        Ok(crate::builtins::str_prim::contains(
            &text,
            &idx,
            start as usize,
            &needle,
            crate::builtins::str_prim::Fold::new(ignore_case, ignore_mark),
        ))
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

    // Cost: O(m) amortized, m = chars of the needle (`str_prim::affix_matches`).
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
        Ok(Value::truth(crate::builtins::str_prim::affix_matches(
            &target,
            &needle,
            is_prefix,
            crate::builtins::str_prim::Fold::new(ignore_case, ignore_mark),
        )))
    }
}
