use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    // chr / ord / chrs / ords / unival / univals removed (Slice 6.3 / Category A
    // dedup): native implementations in src/builtins/functions.rs are
    // authoritative (reached via call_function_fallback -> native_function).
    // chrs is routed through native_function_variadic; ords/unival/univals are in
    // native_function_1arg (unival/univals delegate to the .unival/.univals
    // method, the single source of truth in methods_0arg/dispatch_core_unicode).
    // flip / lc / uc / tc / trim / chars removed (Slice 6.3 dedup) — same.
    pub(super) fn builtin_sprintf(
        &mut self,
        args: &[Value],
        z_mode: bool,
    ) -> Result<Value, RuntimeError> {
        let fmt = match args.first() {
            Some(Value::Str(s)) => s.to_string(),
            _ => String::new(),
        };
        // Flatten array arguments (Raku: sprintf("%d", [42]) treats array elements as args)
        let rest = &args[1..];
        let flattened: Vec<Value>;
        let mut actual_args: Vec<Value> = if rest.len() == 1 {
            if let Value::Array(items, ..) = &rest[0] {
                flattened = items.as_ref().clone().items;
                flattened
            } else {
                rest.to_vec()
            }
        } else {
            rest.to_vec()
        };
        super::sprintf::validate_sprintf_directives(&fmt, actual_args.len())?;
        super::sprintf::validate_sprintf_arg_types(&fmt, &actual_args)?;
        // The pure formatter only knows `to_string_value`/`.gist`/numeric unbox,
        // so a directive whose arg is a user object must first dispatch the user's
        // coercion method matching the directive: `%s` → `.Str` (Raku uses `.Str`
        // only, not `.Stringy`); integer directives (`%d %i %u %b %o %x %X %c`) →
        // `.Int`; float directives (`%e %f %g`) → `.Numeric`. The coerced value
        // replaces the arg (a string for `%s`, the numeric value otherwise). Any
        // captured-outer write the method makes is recorded for the surrounding
        // `sprintf` call op's `apply_pending_rw_writeback` to drain into the
        // caller's slot (same as every other user-method call).
        for (idx, spec) in super::sprintf::sprintf_arg_specs(&fmt) {
            let Some(arg) = actual_args.get(idx) else {
                continue;
            };
            let class_name = match arg {
                Value::Instance { class_name, .. } => Some(class_name.resolve().to_string()),
                Value::Package(name) => Some(name.resolve().to_string()),
                _ => None,
            };
            let Some(cn) = class_name else { continue };
            let method = match spec.to_ascii_lowercase() {
                's' => "Str",
                'd' | 'i' | 'u' | 'b' | 'o' | 'x' | 'c' => "Int",
                'e' | 'f' | 'g' => "Numeric",
                _ => continue,
            };
            if !self.has_user_method(&cn, method) {
                continue;
            }
            let arg_clone = actual_args[idx].clone();
            if let Ok(v) = self.call_method_with_values(arg_clone, method, vec![]) {
                actual_args[idx] = if method == "Str" {
                    Value::str(v.to_string_value())
                } else {
                    v
                };
            }
        }
        let rendered = if z_mode {
            super::sprintf::format_zprintf_args(&fmt, &actual_args)
        } else {
            super::sprintf::format_sprintf_args(&fmt, &actual_args)
        };
        Ok(Value::str(rendered))
    }

    pub(super) fn builtin_make_format(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let fmt = args.first().map(Value::to_string_value).unwrap_or_default();
        let mut attrs = HashMap::new();
        attrs.insert("format".to_string(), Value::str(fmt));
        Ok(Value::make_instance(Symbol::intern("Format"), attrs))
    }

    pub(super) fn builtin_quotewords_atom(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        Ok(Self::quotewords_args_result(args, true))
    }

    pub(super) fn builtin_words_atom(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        Ok(Self::quotewords_args_result(args, false))
    }

    fn quotewords_args_result(args: &[Value], allomorphic: bool) -> Value {
        let mut items = Vec::new();
        let mut pending_literal = String::new();
        for value in args {
            if let Some(literal) = Self::quotewords_literal_value(value) {
                pending_literal.push_str(&literal);
                continue;
            }
            if !pending_literal.is_empty() {
                items.push(Value::str(std::mem::take(&mut pending_literal)));
            }
            Self::quotewords_value_into(value, &mut items, allomorphic);
        }
        if !pending_literal.is_empty() {
            items.push(Value::str(pending_literal));
        }
        match items.len() {
            0 => Value::array(Vec::new()),
            1 => items.into_iter().next().unwrap(),
            _ => Value::array(items),
        }
    }

    pub(super) fn builtin_unknown_backslash_escape(
        &self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let escape = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_else(|| "?".to_string());
        Err(RuntimeError::typed_msg(
            "X::Backslash::UnrecognizedSequence",
            format!("Unrecognized backslash sequence: \\{}", escape),
        ))
    }

    fn quotewords_literal_value(value: &Value) -> Option<String> {
        let Value::Scalar(inner) = value else {
            return None;
        };
        let Value::Pair(marker, payload) = inner.as_ref() else {
            return None;
        };
        if marker == "__mutsu_qw_literal" {
            Some(payload.to_string_value())
        } else {
            None
        }
    }

    fn quotewords_value_into(value: &Value, out: &mut Vec<Value>, allomorphic: bool) {
        match value {
            Value::Array(items, kind) if !kind.is_itemized() => {
                for item in items.iter() {
                    Self::quotewords_value_into(item, out, allomorphic);
                }
            }
            Value::Seq(items) | Value::Slip(items) => {
                for item in items.iter() {
                    Self::quotewords_value_into(item, out, allomorphic);
                }
            }
            other => {
                for word in other.to_string_value().split_whitespace() {
                    if allomorphic {
                        out.push(crate::parser::angle_word_value_full_allomorphic(word));
                    } else {
                        out.push(Value::str(word.to_string()));
                    }
                }
            }
        }
    }

    /// Handle .split() method with full support for regex splitters, limits, and named params.
    pub(crate) fn handle_split_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        use crate::builtins::split::{SplitOpts, apply_split_opts, parse_split_limit};

        let (positional, mut opts) = SplitOpts::from_args(&args);
        if positional.is_empty() {
            return Err(RuntimeError::new("Must specify a pattern for split"));
        }

        opts.check_conflicts("Str")?;

        let splitter = positional[0].clone();
        if positional.len() > 1 {
            opts.limit = parse_split_limit(positional[1])?;
        }

        let text = target.to_string_value();
        let parts = self.split_with_splitter(&text, &splitter, opts.limit)?;
        let result = apply_split_opts(parts, &opts);
        Ok(Value::Seq(std::sync::Arc::new(result)))
    }

    /// Handle split() function with full support for regex splitters.
    pub(crate) fn handle_split_function(
        &mut self,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        use crate::builtins::split::{SplitOpts, apply_split_opts, parse_split_limit};

        let (positional, mut opts) = SplitOpts::from_args(&args);
        if positional.len() < 2 {
            return Err(RuntimeError::new(
                "split requires at least 2 arguments: splitter and string",
            ));
        }

        let splitter = positional[0].clone();
        let target = positional[1].clone();
        if positional.len() > 2 {
            opts.limit = parse_split_limit(positional[2])?;
        }

        let text = target.to_string_value();
        let parts = self.split_with_splitter(&text, &splitter, opts.limit)?;
        let result = apply_split_opts(parts, &opts);
        Ok(Value::Seq(std::sync::Arc::new(result)))
    }

    /// Core split implementation that handles string, regex, and list splitters.
    fn split_with_splitter(
        &mut self,
        text: &str,
        splitter: &Value,
        limit: Option<usize>,
    ) -> Result<Vec<(String, Option<SplitMatch>)>, RuntimeError> {
        match splitter {
            Value::Regex(pattern) => self.split_by_regex(text, pattern, limit),
            Value::RegexWithAdverbs(a) => self.split_by_regex(text, &a.pattern, limit),
            Value::Array(items, _) => {
                // Check if any item is a regex
                let has_regex = items
                    .iter()
                    .any(|v| matches!(v, Value::Regex(_) | Value::RegexWithAdverbs { .. }));
                if has_regex {
                    self.split_by_regex_list(text, items, limit)
                } else {
                    let strings: Vec<String> = items.iter().map(|v| v.to_string_value()).collect();
                    Ok(split_by_strings_static(text, &strings, limit))
                }
            }
            _ => {
                let sep = splitter.to_string_value();
                Ok(split_by_string_static(text, &sep, limit))
            }
        }
    }

    /// Split by a single regex pattern.
    fn split_by_regex(
        &mut self,
        text: &str,
        pattern: &str,
        limit: Option<usize>,
    ) -> Result<Vec<(String, Option<SplitMatch>)>, RuntimeError> {
        if limit == Some(0) {
            return Ok(Vec::new());
        }
        let chars: Vec<char> = text.chars().collect();
        let mut result = Vec::new();

        if text.is_empty() {
            result.push((String::new(), None));
            return Ok(result);
        }

        let max_splits = limit.map(|l| if l > 0 { l - 1 } else { 0 });
        let mut splits_done = 0;
        let mut pos = 0; // current segment start
        let mut search_from = 0; // where to start searching for next match

        loop {
            if let Some(max) = max_splits
                && splits_done >= max
            {
                let remaining: String = chars[pos..].iter().collect();
                result.push((remaining, None));
                return Ok(result);
            }

            if search_from > chars.len() {
                break;
            }

            // Try to match regex starting from search_from, using full text for context
            if let Some((from, to, pcaps)) =
                self.regex_find_first_from_with_captures(pattern, text, search_from)
            {
                let segment: String = chars[pos..from].iter().collect();
                let matched: String = chars[from..to].iter().collect();
                result.push((
                    segment,
                    Some(SplitMatch {
                        from,
                        to,
                        matched,
                        splitter_index: 0,
                        is_regex: true,
                        orig: text.to_string(),
                        positional_captures: pcaps,
                    }),
                ));
                splits_done += 1;
                if from == to {
                    // Zero-width match: next segment starts at from,
                    // but search for next match from from+1
                    pos = from;
                    search_from = from + 1;
                } else {
                    pos = to;
                    search_from = to;
                }
            } else {
                let remaining: String = chars[pos..].iter().collect();
                result.push((remaining, None));
                return Ok(result);
            }
        }

        if pos <= chars.len() {
            let remaining: String = chars[pos..].iter().collect();
            result.push((remaining, None));
        }
        Ok(result)
    }

    /// Split by a list of splitters (mix of string and regex).
    fn split_by_regex_list(
        &mut self,
        text: &str,
        splitters: &[Value],
        limit: Option<usize>,
    ) -> Result<Vec<(String, Option<SplitMatch>)>, RuntimeError> {
        if limit == Some(0) {
            return Ok(Vec::new());
        }
        let chars: Vec<char> = text.chars().collect();
        let mut result = Vec::new();

        if text.is_empty() {
            result.push((String::new(), None));
            return Ok(result);
        }

        let max_splits = limit.map(|l| if l > 0 { l - 1 } else { 0 });
        let mut splits_done = 0;
        let mut pos = 0;

        loop {
            if let Some(max) = max_splits
                && splits_done >= max
            {
                let remaining: String = chars[pos..].iter().collect();
                result.push((remaining, None));
                return Ok(result);
            }

            // (abs_from, abs_to, idx, matched, is_regex) — positions in full text
            let mut best: Option<(usize, usize, usize, String, bool)> = None;

            for (idx, splitter) in splitters.iter().enumerate() {
                match splitter {
                    Value::Regex(_) | Value::RegexWithAdverbs(_) => {
                        let pattern: &str = match splitter {
                            Value::Regex(p) => p,
                            Value::RegexWithAdverbs(a) => &a.pattern,
                            _ => unreachable!(),
                        };
                        if let Some((from, to)) = self.regex_find_first_from(pattern, text, pos) {
                            let matched: String = chars[from..to].iter().collect();
                            let is_better = match &best {
                                None => true,
                                Some((bf, bt, _, _, _)) => {
                                    from < *bf || (from == *bf && (to - from) > (*bt - *bf))
                                }
                            };
                            if is_better {
                                best = Some((from, to, idx, matched, true));
                            }
                        }
                    }
                    _ => {
                        let sep = splitter.to_string_value();
                        if sep.is_empty() {
                            continue;
                        }
                        let sep_chars: Vec<char> = sep.chars().collect();
                        if pos + sep_chars.len() <= chars.len() {
                            for start in pos..=(chars.len() - sep_chars.len()) {
                                if chars[start..start + sep_chars.len()] == sep_chars[..] {
                                    let is_better = match &best {
                                        None => true,
                                        Some((bf, _, _, _, _)) => start < *bf,
                                    };
                                    if is_better {
                                        best = Some((
                                            start,
                                            start + sep_chars.len(),
                                            idx,
                                            sep.clone(),
                                            false,
                                        ));
                                    }
                                    break;
                                }
                            }
                        }
                    }
                }
            }

            match best {
                Some((from, to, idx, matched, is_regex)) => {
                    let segment: String = chars[pos..from].iter().collect();
                    result.push((
                        segment,
                        Some(SplitMatch {
                            from,
                            to,
                            matched,
                            splitter_index: idx,
                            is_regex,
                            orig: text.to_string(),
                            positional_captures: Vec::new(),
                        }),
                    ));
                    pos = to;
                    if pos == from {
                        // Zero-width match: advance by one
                        pos += 1;
                    }
                    splits_done += 1;
                }
                None => {
                    let remaining: String = chars[pos..].iter().collect();
                    result.push((remaining, None));
                    return Ok(result);
                }
            }
        }
    }
}

/// SplitMatch from the builtins split module (re-export for local use).
use crate::builtins::split::SplitMatch;

/// Static string split (no interpreter needed).
fn split_by_string_static(
    text: &str,
    sep: &str,
    limit: Option<usize>,
) -> Vec<(String, Option<SplitMatch>)> {
    if limit == Some(0) {
        return Vec::new();
    }
    let mut result = Vec::new();
    let chars: Vec<char> = text.chars().collect();

    if text.is_empty() {
        if sep.is_empty() {
            return result;
        }
        result.push((String::new(), None));
        return result;
    }

    if sep.is_empty() {
        let max_splits = limit.map(|l| if l > 0 { l - 1 } else { 0 });
        let mut seg_start = 0;

        for (splits_done, match_pos) in (0..=chars.len()).enumerate() {
            if let Some(max) = max_splits
                && splits_done >= max
            {
                let remaining: String = chars[seg_start..].iter().collect();
                result.push((remaining, None));
                return result;
            }
            let segment: String = chars[seg_start..match_pos].iter().collect();
            result.push((
                segment,
                Some(SplitMatch {
                    from: match_pos,
                    to: match_pos,
                    matched: String::new(),
                    splitter_index: 0,
                    is_regex: false,
                    orig: String::new(),
                    positional_captures: Vec::new(),
                }),
            ));
            seg_start = match_pos;
        }
        result.push((String::new(), None));
        return result;
    }

    let sep_chars: Vec<char> = sep.chars().collect();
    let sep_len = sep_chars.len();
    let max_splits = limit.map(|l| if l > 0 { l - 1 } else { 0 });
    let mut splits_done = 0;
    let mut pos = 0;

    loop {
        if let Some(max) = max_splits
            && splits_done >= max
        {
            let remaining: String = chars[pos..].iter().collect();
            result.push((remaining, None));
            return result;
        }

        let mut found = None;
        if pos + sep_len <= chars.len() {
            for start in pos..=(chars.len() - sep_len) {
                if chars[start..start + sep_len] == sep_chars[..] {
                    found = Some(start);
                    break;
                }
            }
        }

        match found {
            Some(match_pos) => {
                let segment: String = chars[pos..match_pos].iter().collect();
                result.push((
                    segment,
                    Some(SplitMatch {
                        from: match_pos,
                        to: match_pos + sep_len,
                        matched: sep.to_string(),
                        splitter_index: 0,
                        is_regex: false,
                        orig: String::new(),
                        positional_captures: Vec::new(),
                    }),
                ));
                pos = match_pos + sep_len;
                splits_done += 1;
            }
            None => {
                let remaining: String = chars[pos..].iter().collect();
                result.push((remaining, None));
                return result;
            }
        }
    }
}

/// Static multi-string split (no interpreter needed).
fn split_by_strings_static(
    text: &str,
    splitters: &[String],
    limit: Option<usize>,
) -> Vec<(String, Option<SplitMatch>)> {
    if limit == Some(0) {
        return Vec::new();
    }
    let mut result = Vec::new();
    let chars: Vec<char> = text.chars().collect();

    if text.is_empty() {
        result.push((String::new(), None));
        return result;
    }

    let splitter_chars: Vec<Vec<char>> = splitters.iter().map(|s| s.chars().collect()).collect();
    let max_splits = limit.map(|l| if l > 0 { l - 1 } else { 0 });
    let mut splits_done = 0;
    let mut pos = 0;

    loop {
        if let Some(max) = max_splits
            && splits_done >= max
        {
            let remaining: String = chars[pos..].iter().collect();
            result.push((remaining, None));
            return result;
        }

        let mut best: Option<(usize, usize, usize)> = None;
        for (idx, sep_chars) in splitter_chars.iter().enumerate() {
            let sep_len = sep_chars.len();
            if sep_len == 0 {
                continue;
            }
            if pos + sep_len <= chars.len() {
                for start in pos..=(chars.len() - sep_len) {
                    if chars[start..start + sep_len] == sep_chars[..] {
                        match best {
                            None => {
                                best = Some((start, sep_len, idx));
                            }
                            Some((best_pos, best_len, _)) => {
                                if start < best_pos || (start == best_pos && sep_len > best_len) {
                                    best = Some((start, sep_len, idx));
                                }
                            }
                        }
                        break;
                    }
                }
            }
        }

        match best {
            Some((match_pos, match_len, splitter_idx)) => {
                let segment: String = chars[pos..match_pos].iter().collect();
                let matched: String = chars[match_pos..match_pos + match_len].iter().collect();
                result.push((
                    segment,
                    Some(SplitMatch {
                        from: match_pos,
                        to: match_pos + match_len,
                        matched,
                        splitter_index: splitter_idx,
                        is_regex: false,
                        orig: String::new(),
                        positional_captures: Vec::new(),
                    }),
                ));
                pos = match_pos + match_len;
                splits_done += 1;
            }
            None => {
                let remaining: String = chars[pos..].iter().collect();
                result.push((remaining, None));
                return result;
            }
        }
    }
}
