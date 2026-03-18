use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(super) fn builtin_chrs(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut result = String::new();
        for arg in args {
            for item in Self::value_to_list(arg) {
                if let Value::Int(i) = item
                    && i >= 0
                    && (i as u64) <= 0x10ffff
                    && let Some(ch) = std::char::from_u32(i as u32)
                {
                    result.push(ch);
                    continue;
                }
                result.push_str(&item.to_string_value());
            }
        }
        Ok(Value::str(result))
    }

    pub(super) fn builtin_chr(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let Some(arg) = args.first() else {
            return Ok(Value::str(String::new()));
        };
        let (code, display) = match arg {
            Value::Int(i) => (*i, format!("{}", i)),
            Value::BigInt(n) => {
                let hex = format!("{:X}", &**n);
                return Err(RuntimeError::new(format!(
                    "Codepoint {} (0x{}) is out of bounds in 'chr'",
                    n, hex
                )));
            }
            Value::Num(f) => (*f as i64, format!("{}", *f as i64)),
            _ => {
                let s = arg.to_string_value();
                let i = s.parse::<i64>().unwrap_or(0);
                (i, format!("{}", i))
            }
        };
        if !(0..=0x10FFFF).contains(&code) {
            let hex = format!("{:X}", code);
            return Err(RuntimeError::new(format!(
                "Codepoint {} (0x{}) is out of bounds in 'chr'",
                display, hex
            )));
        }
        if let Some(ch) = std::char::from_u32(code as u32) {
            Ok(Value::str(ch.to_string()))
        } else {
            Err(RuntimeError::new(format!(
                "Codepoint {} (0x{:X}) is out of bounds in 'chr'",
                display, code
            )))
        }
    }

    pub(super) fn builtin_ord(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        if let Some(val) = args.first()
            && let Some(ch) = val.to_string_value().chars().next()
        {
            return Ok(Value::Int(ch as u32 as i64));
        }
        Ok(Value::Nil)
    }

    pub(super) fn builtin_ords(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        if let Some(val) = args.first() {
            let codes = val
                .to_string_value()
                .chars()
                .map(|ch| Value::Int(ch as u32 as i64))
                .collect();
            return Ok(Value::array(codes));
        }
        Ok(Value::array(Vec::new()))
    }

    pub(super) fn builtin_unival(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let Some(arg) = args.first() else {
            return Ok(Value::Nil);
        };
        let ch = match arg {
            Value::Int(i) if *i >= 0 => {
                let Some(ch) = char::from_u32(*i as u32) else {
                    return Ok(Value::Num(f64::NAN));
                };
                ch
            }
            _ => {
                let s = arg.to_string_value();
                let Some(ch) = s.chars().next() else {
                    return Ok(Value::Nil);
                };
                ch
            }
        };
        if let Some((n, d)) = crate::builtins::unicode::unicode_rat_value(ch) {
            return Ok(crate::value::make_rat(n, d));
        }
        if let Some(n) = crate::builtins::unicode::unicode_numeric_int_value(ch) {
            return Ok(Value::Int(n));
        }
        if let Some(n) = crate::builtins::unicode::unicode_decimal_digit_value(ch) {
            return Ok(Value::Int(n as i64));
        }
        Ok(Value::Num(f64::NAN))
    }

    pub(super) fn builtin_flip(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        use unicode_normalization::UnicodeNormalization;
        use unicode_segmentation::UnicodeSegmentation;
        let val = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        // Flip by grapheme clusters, then normalize to NFC so combining sequences
        // are emitted in canonical composed form when possible.
        let reversed = val.graphemes(true).rev().collect::<String>();
        Ok(Value::str(reversed.nfc().collect::<String>()))
    }

    pub(super) fn builtin_lc(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let val = args.first().cloned().unwrap_or(Value::Nil);
        Ok(Value::str(val.to_string_value().to_lowercase()))
    }

    pub(super) fn builtin_uc(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let val = args.first().cloned().unwrap_or(Value::Nil);
        Ok(Value::str(val.to_string_value().to_uppercase()))
    }

    pub(super) fn builtin_tc(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let val = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let mut result = String::new();
        let mut capitalize = true;
        for ch in val.chars() {
            if capitalize {
                for c in ch.to_uppercase() {
                    result.push(c);
                }
                capitalize = false;
            } else {
                result.push(ch);
            }
        }
        Ok(Value::str(result))
    }

    pub(super) fn builtin_trim(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let val = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        Ok(Value::str(val.trim().to_string()))
    }

    pub(super) fn builtin_chars(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        use unicode_segmentation::UnicodeSegmentation;
        let val = args.first().cloned();
        Ok(match val {
            Some(Value::Str(s)) => Value::Int(s.graphemes(true).count() as i64),
            Some(v) => Value::Int(v.to_string_value().graphemes(true).count() as i64),
            _ => Value::Int(0),
        })
    }

    pub(super) fn builtin_sprintf(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let fmt = match args.first() {
            Some(Value::Str(s)) => s.to_string(),
            _ => String::new(),
        };
        // Flatten array arguments (Raku: sprintf("%d", [42]) treats array elements as args)
        let rest = &args[1..];
        let flattened: Vec<Value>;
        let actual_args = if rest.len() == 1 {
            if let Value::Array(items, ..) = &rest[0] {
                flattened = items.as_ref().clone();
                &flattened
            } else {
                rest
            }
        } else {
            rest
        };
        let rendered = super::sprintf::format_sprintf_args(&fmt, actual_args);
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
        use crate::builtins::split::{SplitOpts, apply_split_opts};

        let (positional, mut opts) = SplitOpts::from_args(&args);
        if positional.is_empty() {
            return Err(RuntimeError::new("Must specify a pattern for split"));
        }

        let splitter = positional[0].clone();
        if positional.len() > 1 {
            opts.limit = Some(crate::runtime::to_int(positional[1]).max(0) as usize);
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
        use crate::builtins::split::{SplitOpts, apply_split_opts};

        let (positional, mut opts) = SplitOpts::from_args(&args);
        if positional.len() < 2 {
            return Err(RuntimeError::new(
                "split requires at least 2 arguments: splitter and string",
            ));
        }

        let splitter = positional[0].clone();
        let target = positional[1].clone();
        if positional.len() > 2 {
            opts.limit = Some(crate::runtime::to_int(positional[2]).max(0) as usize);
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
            Value::Regex(pattern) | Value::RegexWithAdverbs { pattern, .. } => {
                self.split_by_regex(text, pattern, limit)
            }
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

            // Try to match regex starting from pos
            let remaining_text: String = chars[pos..].iter().collect();
            if let Some((from, to)) = self.regex_find_first(pattern, &remaining_text) {
                let abs_from = pos + from;
                let abs_to = pos + to;

                // Avoid infinite loop on zero-width match
                if from == to && abs_from == pos && !result.is_empty() {
                    if pos < chars.len() {
                        let ch_str = chars[pos].to_string();
                        result.push((ch_str, None));
                        pos += 1;
                        continue;
                    } else {
                        break;
                    }
                }

                let segment: String = chars[pos..abs_from].iter().collect();
                let matched: String = chars[abs_from..abs_to].iter().collect();
                result.push((
                    segment,
                    Some(SplitMatch {
                        from: abs_from,
                        to: abs_to,
                        matched,
                        splitter_index: 0,
                    }),
                ));
                pos = abs_to;
                if pos == abs_from {
                    // Zero-width match: advance by one to prevent infinite loop
                    pos += 1;
                }
                splits_done += 1;
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

            let remaining_text: String = chars[pos..].iter().collect();
            let mut best: Option<(usize, usize, usize, String)> = None; // (from, to, idx, matched)

            for (idx, splitter) in splitters.iter().enumerate() {
                match splitter {
                    Value::Regex(pattern) | Value::RegexWithAdverbs { pattern, .. } => {
                        if let Some((from, to)) = self.regex_find_first(pattern, &remaining_text) {
                            let matched: String =
                                remaining_text.chars().skip(from).take(to - from).collect();
                            let is_better = match &best {
                                None => true,
                                Some((bf, bt, _, _)) => {
                                    from < *bf || (from == *bf && (to - from) > (*bt - *bf))
                                }
                            };
                            if is_better {
                                best = Some((from, to, idx, matched));
                            }
                        }
                    }
                    _ => {
                        let sep = splitter.to_string_value();
                        if sep.is_empty() {
                            continue;
                        }
                        let sep_chars: Vec<char> = sep.chars().collect();
                        let rem_chars: Vec<char> = remaining_text.chars().collect();
                        if sep_chars.len() <= rem_chars.len() {
                            for start in 0..=(rem_chars.len() - sep_chars.len()) {
                                if rem_chars[start..start + sep_chars.len()] == sep_chars[..] {
                                    let is_better = match &best {
                                        None => true,
                                        Some((bf, _, _, _)) => start < *bf,
                                    };
                                    if is_better {
                                        best = Some((
                                            start,
                                            start + sep_chars.len(),
                                            idx,
                                            sep.clone(),
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
                Some((from, to, idx, matched)) => {
                    let abs_from = pos + from;
                    let abs_to = pos + to;
                    let segment: String = chars[pos..abs_from].iter().collect();
                    result.push((
                        segment,
                        Some(SplitMatch {
                            from: abs_from,
                            to: abs_to,
                            matched,
                            splitter_index: idx,
                        }),
                    ));
                    pos = abs_to;
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
