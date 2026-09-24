use super::*;
use crate::symbol::Symbol;

/// Truncate an already-numeric `Value` towards zero.
fn numeric_value_to_i64(val: &Value) -> i64 {
    match val.view() {
        ValueView::Int(i) => i,
        ValueView::Num(f) => f as i64,
        ValueView::Rat(n, d) | ValueView::FatRat(n, d) if d != 0 => n / d,
        ValueView::BigInt(b) => b.to_string().parse::<i64>().unwrap_or(i64::MAX),
        ValueView::Bool(b) => i64::from(b),
        _ => 0,
    }
}

/// Numify a `Cool` offset/length argument of `substr` the way rakudo's
/// `Cool.substr` candidate does: the value is coerced with `.Int`, so a numeric
/// string is parsed (`"3.7"` is 3, `" 4 "` is 4) and a `Bool` becomes 0/1.
/// A string that is not a valid number raises `X::Str::Numeric`, matching raku
/// (`"abc".substr(0, "abc")` dies rather than silently taking the whole rest).
fn substr_cool_to_i64(val: &Value) -> Result<i64, RuntimeError> {
    if let Some(s) = val.as_str() {
        let trimmed = s.trim();
        if trimmed.is_empty() {
            return Ok(0);
        }
        return match crate::runtime::str_numeric::parse_raku_str_to_numeric(trimmed) {
            Some(numeric) => Ok(numeric_value_to_i64(&numeric)),
            None => {
                let (pos, reason) =
                    crate::runtime::str_numeric::str_numeric_failure(s).unwrap_or((
                        0,
                        "base-10 number must begin with valid digits or '.'".into(),
                    ));
                let indicator = crate::runtime::str_numeric::build_source_indicator(s, pos);
                Err(RuntimeError::typed_msg(
                    "X::Str::Numeric",
                    format!("Cannot convert string to number: {reason} {indicator}"),
                ))
            }
        };
    }
    Ok(numeric_value_to_i64(&crate::runtime::coerce_to_numeric(
        val.clone(),
    )))
}

impl Interpreter {
    // Cost: O(m) amortized, m = chars of the needle, once the invocant's
    // grapheme index is cached (`$pos` is resolved through it).
    pub(super) fn dispatch_substr_eq(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Separate positional and named args
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
        if positional.is_empty() {
            return Err(RuntimeError::new(
                "Too few positionals passed to 'substr-eq'",
            ));
        }
        // Type objects (Package) as needle should throw
        if let ValueView::Package(type_name) = positional[0].view() {
            return Err(RuntimeError::new(format!(
                "Cannot resolve caller substr-eq({}:U)",
                type_name
            )));
        }
        // Positions are graphemes, like `substr` (codepoints used to be
        // counted here, so `"q\x[301]a".substr-eq("a", 1)` was False).
        let (text, idx) = crate::builtins::grapheme_index::str_and_index(&target);
        let needle = positional[0].to_string_value();
        let len = idx.len() as i64;
        let start = if let Some(pos) = positional.get(1) {
            match self.substr_resolve_position(pos, len as usize) {
                Ok(v) => v,
                Err(err) => return Ok(Self::runtime_error_to_failure(err)),
            }
        } else {
            0
        };
        if start < 0 || start > len {
            return Ok(RuntimeError::out_of_range_failure(
                "start",
                Value::int(start),
                &format!("0..{}", len),
            ));
        }
        let eq = crate::builtins::str_prim::eq_at(
            &text,
            &idx,
            start as usize,
            &needle,
            crate::builtins::str_prim::Fold::new(ignore_case, ignore_mark),
        );
        Ok(Value::truth(eq))
    }

    // Cost: O(k) amortized, k = chars returned, once the invocant's grapheme
    // index is cached (built in O(n) on first use, `grapheme_index`).
    pub(super) fn dispatch_substr(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Positions are resolved against the cached grapheme index, so a
        // `substr` costs O(result), not O(invocant) (#9140).
        let (s, idx) = crate::builtins::grapheme_index::str_and_index(&target);
        let total_len = idx.len();
        let slice = |start: usize, end: usize| {
            let (b0, b1) = idx.byte_range(&s, start, end.saturating_sub(start));
            Value::str(s[b0..b1].to_string())
        };

        // Check if first arg is a Range — handle substr($str, 6..8) form
        if let Some(first_arg) = args.first()
            && let Some((range_start, range_end)) =
                self.substr_extract_range(first_arg, total_len)?
        {
            let rs = range_start.min(total_len);
            let re = range_end.min(total_len);
            return Ok(slice(rs, re));
        }

        // First arg: start position
        let start_raw: i64 = if let Some(pos) = args.first() {
            self.substr_resolve_position(pos, total_len)?
        } else {
            0
        };

        // Resolve negative start (from WhateverCode calling convention)
        let start = if start_raw < 0 {
            total_len as i64 + start_raw
        } else {
            start_raw
        };

        // Out-of-range check: return Failure wrapping X::OutOfRange
        if start < 0 || start as usize > total_len {
            return self.substr_out_of_range_failure(start, total_len);
        }

        let start = start as usize;

        // Second arg: length (can be Int, WhateverCode/Sub, Num/Inf, or absent)
        let end = if let Some(len_val) = args.get(1) {
            match len_val.view() {
                ValueView::Int(i) => {
                    let len = i.max(0) as usize;
                    (start + len).min(total_len)
                }
                ValueView::Num(f) if f.is_infinite() && f > 0.0 => total_len,
                ValueView::Num(f) => {
                    let len = (f as i64).max(0) as usize;
                    (start + len).min(total_len)
                }
                ValueView::Rat(n, d) if d != 0 => {
                    let len = (n / d).max(0) as usize;
                    (start + len).min(total_len)
                }
                ValueView::Sub(..) => {
                    // WhateverCode: call with remaining length to get actual length
                    let remaining = if start <= total_len {
                        (total_len - start) as i64
                    } else {
                        0
                    };
                    let result =
                        self.eval_call_on_value(len_val.clone(), vec![Value::int(remaining)])?;
                    let len = match result.view() {
                        ValueView::Int(i) => i.max(0) as usize,
                        ValueView::Num(f) => (f as i64).max(0) as usize,
                        ValueView::Rat(n, d) if d != 0 => (n / d).max(0) as usize,
                        _ => 0,
                    };
                    (start + len).min(total_len)
                }
                // A `Cool` length (`"3"`, `True`, an Int-valued enum) is numified
                // the way rakudo's `Cool.substr` candidate does. This is what
                // makes raku's own colon-call gotcha reproduce:
                // `$band.substr: 0, 3 .uc` parses as `$band.substr(0, "3")`.
                ValueView::Str(_) | ValueView::Bool(_) | ValueView::Enum { .. } => {
                    let len = substr_cool_to_i64(len_val)?.max(0) as usize;
                    (start + len).min(total_len)
                }
                _ => total_len, // default: take rest
            }
        } else {
            total_len // no length: take rest
        };

        Ok(slice(start, end))
    }

    /// substr-rw in non-lvalue context: just return the substring (same as substr).
    /// When a variable name is available, returns a Proxy for binding support.
    // Cost: as `dispatch_substr`: O(k) amortized.
    pub(super) fn dispatch_substr_rw(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        self.dispatch_substr(target, args)
    }

    /// Resolve a position argument to an i64, handling Int, Num, Rat, BigInt, WhateverCode/Sub.
    pub(crate) fn substr_resolve_position(
        &mut self,
        pos: &Value,
        total_len: usize,
    ) -> Result<i64, RuntimeError> {
        // See `substr_extract_range`: an itemized argument keeps its `Scalar`.
        let pos = pos.descalarize();
        match pos.view() {
            ValueView::Int(i) => Ok(i),
            ValueView::Num(f) => Ok(f as i64),
            ValueView::Rat(n, d) if d != 0 => Ok(n / d),
            ValueView::BigInt(b) => {
                if b.as_ref() > &num_bigint::BigInt::from(i64::MAX)
                    || b.as_ref() < &num_bigint::BigInt::from(i64::MIN)
                {
                    Err(self.out_of_range_error(Value::bigint((**b).clone())))
                } else {
                    Ok(b.to_string().parse::<i64>().unwrap_or(0))
                }
            }
            ValueView::Sub(..) => {
                // WhateverCode/Callable: call with total_len to resolve position
                let result =
                    self.eval_call_on_value(pos.clone(), vec![Value::int(total_len as i64)])?;
                match result.view() {
                    ValueView::Int(i) => Ok(i),
                    ValueView::Num(f) => Ok(f as i64),
                    ValueView::Rat(n, d) if d != 0 => Ok(n / d),
                    _ => Ok(0),
                }
            }
            // A `Cool` offset is numified like rakudo's `Cool.substr`: `"3.7"`
            // truncates to 3 and `" 4 "` is 4, rather than failing a strict
            // `i64` parse and silently starting at 0.
            ValueView::Str(_) | ValueView::Bool(_) | ValueView::Enum { .. } => {
                substr_cool_to_i64(pos)
            }
            _ => Ok(pos.to_string_value().parse::<i64>().unwrap_or(0)),
        }
    }

    /// Extract start/end indices from a Range value for substr.
    /// Returns Some((start, end)) if the value is a Range, None otherwise.
    pub(crate) fn substr_extract_range(
        &mut self,
        val: &Value,
        total_len: usize,
    ) -> Result<Option<(usize, usize)>, RuntimeError> {
        // A `$`-held Range is itemized (`my $r = 1..3`), so the argument arrives
        // wrapped in a `Scalar`. Read through it before asking "is this a
        // Range?" — otherwise `$s.substr($r)` silently fell to the "not a Range"
        // default and took the whole rest of the string.
        let val = val.descalarize();
        match val.view() {
            ValueView::Range(a, b) => {
                let end = b.saturating_add(1).max(0) as usize;
                Ok(Some((a as usize, end)))
            }
            ValueView::RangeExcl(a, b) => Ok(Some((a as usize, b as usize))),
            ValueView::RangeExclStart(a, b) => {
                let end = b.saturating_add(1).max(0) as usize;
                Ok(Some(((a + 1) as usize, end)))
            }
            ValueView::RangeExclBoth(a, b) => Ok(Some(((a + 1) as usize, b as usize))),
            ValueView::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                let s = self.substr_resolve_position(start, total_len)?;
                let s = if excl_start { s + 1 } else { s };
                let e_val = self.substr_resolve_position(end, total_len)?;
                // For Inf end (e.g., 10..*), e_val will be very large; clamp later
                let e = if excl_end { e_val } else { e_val + 1 };
                let s = s.max(0) as usize;
                let e = e.max(0) as usize;
                Ok(Some((s, e)))
            }
            _ => Ok(None),
        }
    }

    /// Return a Failure wrapping X::OutOfRange for substr out-of-range start.
    fn substr_out_of_range_failure(
        &self,
        start: i64,
        total_len: usize,
    ) -> Result<Value, RuntimeError> {
        let mut ex_attrs = std::collections::HashMap::new();
        ex_attrs.insert(
            "what".to_string(),
            Value::str("Start argument to substr".to_string()),
        );
        ex_attrs.insert("got".to_string(), Value::str(start.to_string()));
        ex_attrs.insert("range".to_string(), Value::str(format!("0..{}", total_len)));
        ex_attrs.insert(
            "message".to_string(),
            Value::str("X::OutOfRange".to_string()),
        );
        let exception = Value::make_instance(Symbol::intern("X::OutOfRange"), ex_attrs);
        let mut failure_attrs = std::collections::HashMap::new();
        failure_attrs.insert("exception".to_string(), exception);
        failure_attrs.insert("handled".to_string(), Value::FALSE);
        Ok(Value::make_instance(
            Symbol::intern("Failure"),
            failure_attrs,
        ))
    }
}
