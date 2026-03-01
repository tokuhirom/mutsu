use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(super) fn translate_newlines_for_encode(&self, input: &str) -> String {
        match self.newline_mode {
            NewlineMode::Lf => input.to_string(),
            NewlineMode::Cr => input.replace('\n', "\r"),
            NewlineMode::Crlf => input.replace('\n', "\r\n"),
        }
    }

    pub(super) fn translate_newlines_for_decode(&self, input: &str) -> String {
        match self.newline_mode {
            NewlineMode::Lf => input.to_string(),
            NewlineMode::Cr => input.replace('\r', "\n"),
            NewlineMode::Crlf => input.replace("\r\n", "\n"),
        }
    }

    pub(super) fn encode_with_encoding(
        &self,
        input: &str,
        encoding_name: &str,
    ) -> Result<Vec<u8>, RuntimeError> {
        let encoding = self
            .find_encoding(encoding_name)
            .map(|e| e.name.as_str())
            .unwrap_or(encoding_name)
            .to_lowercase();

        match encoding.as_str() {
            "ascii" => Ok(input
                .chars()
                .map(|c| if (c as u32) <= 0x7F { c as u8 } else { b'?' })
                .collect()),
            "iso-8859-1" => Ok(input
                .chars()
                .map(|c| if (c as u32) <= 0xFF { c as u8 } else { b'?' })
                .collect()),
            "utf-16" | "utf-16le" => {
                Ok(input.encode_utf16().flat_map(|u| u.to_le_bytes()).collect())
            }
            "utf-16be" => Ok(input.encode_utf16().flat_map(|u| u.to_be_bytes()).collect()),
            _ => {
                if let Some(enc) = Self::lookup_encoding_rs_codec(&encoding) {
                    if matches!(encoding.as_str(), "windows-1251" | "windows-1252") {
                        return Ok(Self::encode_single_byte_with_encoding_rs(input, enc));
                    }
                    let (encoded, _used_encoding, _had_errors) = enc.encode(input);
                    return Ok(encoded.into_owned());
                }
                Ok(input.as_bytes().to_vec())
            }
        }
    }

    pub(super) fn decode_with_encoding(
        &self,
        bytes: &[u8],
        encoding_name: &str,
    ) -> Result<String, RuntimeError> {
        let encoding = self
            .find_encoding(encoding_name)
            .map(|e| e.name.as_str())
            .unwrap_or(encoding_name)
            .to_lowercase();

        match encoding.as_str() {
            "ascii" => Ok(bytes
                .iter()
                .map(|b| if *b <= 0x7F { *b as char } else { '\u{FFFD}' })
                .collect()),
            "iso-8859-1" => Ok(bytes.iter().map(|b| *b as char).collect()),
            "utf-16" | "utf-16le" => {
                if !bytes.len().is_multiple_of(2) {
                    return Err(RuntimeError::new("Invalid utf-16 byte length"));
                }
                let units: Vec<u16> = bytes
                    .chunks_exact(2)
                    .map(|chunk| u16::from_le_bytes([chunk[0], chunk[1]]))
                    .collect();
                Ok(String::from_utf16_lossy(&units))
            }
            "utf-16be" => {
                if !bytes.len().is_multiple_of(2) {
                    return Err(RuntimeError::new("Invalid utf-16be byte length"));
                }
                let units: Vec<u16> = bytes
                    .chunks_exact(2)
                    .map(|chunk| u16::from_be_bytes([chunk[0], chunk[1]]))
                    .collect();
                Ok(String::from_utf16_lossy(&units))
            }
            _ => {
                if let Some(enc) = Self::lookup_encoding_rs_codec(&encoding) {
                    let (decoded, _used_encoding, _had_errors) = enc.decode(bytes);
                    return Ok(decoded.into_owned());
                }
                Ok(String::from_utf8_lossy(bytes).into_owned())
            }
        }
    }

    pub(super) fn lookup_encoding_rs_codec(
        encoding: &str,
    ) -> Option<&'static encoding_rs::Encoding> {
        let label = match encoding {
            "windows-932" => "shift_jis",
            _ => encoding,
        };
        encoding_rs::Encoding::for_label(label.as_bytes())
    }

    pub(super) fn encode_single_byte_with_encoding_rs(
        input: &str,
        enc: &'static encoding_rs::Encoding,
    ) -> Vec<u8> {
        let mut reverse = HashMap::with_capacity(256);
        for b in 0u8..=255 {
            let one = [b];
            let (decoded, _used_encoding, _had_errors) = enc.decode(&one);
            let mut chars = decoded.chars();
            if let (Some(ch), None) = (chars.next(), chars.next()) {
                reverse.insert(ch, b);
            }
        }
        input
            .chars()
            .map(|ch| reverse.get(&ch).copied().unwrap_or(b'?'))
            .collect()
    }
    pub(super) fn dispatch_subst(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let text = target.to_string_value();
        let mut positional: Vec<Value> = Vec::new();
        let mut global = false;
        for arg in args {
            if let Value::Pair(key, value) = arg {
                match key.as_str() {
                    "g" | "global" => global = value.truthy(),
                    _ => {}
                }
            } else {
                positional.push(arg.clone());
            }
        }
        let pattern = positional
            .first()
            .ok_or_else(|| RuntimeError::new("subst requires a pattern argument"))?;
        let replacement_val = positional.get(1).cloned();
        let is_closure = matches!(
            replacement_val,
            Some(Value::Sub(_)) | Some(Value::WeakSub(_))
        );
        let replacement_str = if is_closure {
            String::new()
        } else {
            replacement_val
                .as_ref()
                .map(|v| v.to_string_value())
                .unwrap_or_default()
        };

        match pattern {
            Value::Regex(pat) => {
                let matches = self.regex_find_all(pat, &text);
                if matches.is_empty() {
                    return Ok(Value::Str(text));
                }
                let chars: Vec<char> = text.chars().collect();
                if global {
                    let mut result = String::new();
                    let mut last_end = 0;
                    for (start, end) in &matches {
                        let prefix: String = chars[last_end..*start].iter().collect();
                        result.push_str(&prefix);
                        let matched_text: String = chars[*start..*end].iter().collect();
                        let repl = self.eval_subst_replacement(
                            &replacement_val,
                            is_closure,
                            &replacement_str,
                            &matched_text,
                        )?;
                        result.push_str(&repl);
                        last_end = *end;
                    }
                    let suffix: String = chars[last_end..].iter().collect();
                    result.push_str(&suffix);
                    Ok(Value::Str(result))
                } else {
                    // Replace first match only
                    if let Some((start, end)) = matches.first() {
                        let prefix: String = chars[..*start].iter().collect();
                        let suffix: String = chars[*end..].iter().collect();
                        let matched_text: String = chars[*start..*end].iter().collect();
                        let repl = self.eval_subst_replacement(
                            &replacement_val,
                            is_closure,
                            &replacement_str,
                            &matched_text,
                        )?;
                        Ok(Value::Str(format!("{}{}{}", prefix, repl, suffix)))
                    } else {
                        Ok(Value::Str(text))
                    }
                }
            }
            Value::Str(pat) => {
                if global {
                    Ok(Value::Str(text.replace(pat, &replacement_str)))
                } else {
                    Ok(Value::Str(text.replacen(pat, &replacement_str, 1)))
                }
            }
            _ => {
                let pat_str = pattern.to_string_value();
                if global {
                    Ok(Value::Str(text.replace(&pat_str, &replacement_str)))
                } else {
                    Ok(Value::Str(text.replacen(&pat_str, &replacement_str, 1)))
                }
            }
        }
    }

    /// Evaluate a subst replacement — either a static string or a closure call.
    pub(super) fn eval_subst_replacement(
        &mut self,
        replacement_val: &Option<Value>,
        is_closure: bool,
        replacement_str: &str,
        matched_text: &str,
    ) -> Result<String, RuntimeError> {
        if !is_closure {
            return Ok(replacement_str.to_string());
        }
        let sub_data = match replacement_val {
            Some(Value::Sub(data)) => data.clone(),
            Some(Value::WeakSub(weak)) => weak
                .upgrade()
                .ok_or_else(|| RuntimeError::new("subst closure has been garbage collected"))?,
            _ => return Ok(replacement_str.to_string()),
        };
        let saved = self.env.clone();
        // Set up closure environment
        for (k, v) in &sub_data.env {
            self.env.insert(k.clone(), v.clone());
        }
        // Set $/ to the matched text
        let match_val = Value::Str(matched_text.to_string());
        self.env.insert("/".to_string(), match_val.clone());
        self.env.insert("$_".to_string(), match_val);
        let result = self.eval_block_value(&sub_data.body).unwrap_or(Value::Nil);
        self.env = saved;
        Ok(result.to_string_value())
    }

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
            .unwrap_or(Value::Str(String::new()));
        let start = if let Some(pos) = positional.get(1) {
            match pos {
                Value::Int(i) => *i,
                Value::Num(f) => *f as i64,
                Value::Str(s) => s.parse::<i64>().unwrap_or(0),
                Value::BigInt(b) => {
                    if b > &num_bigint::BigInt::from(i64::MAX) {
                        return Err(RuntimeError::new("X::OutOfRange"));
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
            return Err(RuntimeError::new("X::OutOfRange"));
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
            self.value_to_position(pos)?
        } else {
            0
        };
        let text = target.to_string_value();
        let len = text.chars().count() as i64;
        if start < 0 {
            return Err(self.out_of_range_error(Value::Int(start)));
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

    pub(super) fn out_of_range_error(&self, got: Value) -> RuntimeError {
        let mut attrs = HashMap::new();
        attrs.insert("got".to_string(), got);
        attrs.insert(
            "message".to_string(),
            Value::Str("X::OutOfRange".to_string()),
        );
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
            Value::Str(s) => Ok(s.parse::<i64>().unwrap_or(0)),
            Value::BigInt(b) => {
                if b > &num_bigint::BigInt::from(i64::MAX)
                    || b < &num_bigint::BigInt::from(i64::MIN)
                {
                    Err(self.out_of_range_error(Value::BigInt(b.clone())))
                } else {
                    Ok(b.to_string().parse::<i64>().unwrap_or(0))
                }
            }
            _ => Ok(0),
        }
    }

    pub(super) fn dispatch_substr_eq(
        &self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            return Err(RuntimeError::new(
                "Too few positionals passed to 'substr-eq'",
            ));
        }
        let text = target.to_string_value();
        let needle = args[0].to_string_value();
        let start = if let Some(pos) = args.get(1) {
            self.value_to_position(pos)?
        } else {
            0
        };
        let len = text.chars().count() as i64;
        if start < 0 || start > len {
            return Err(self.out_of_range_error(Value::Int(start)));
        }
        let substr: String = text
            .chars()
            .skip(start as usize)
            .take(needle.len())
            .collect();
        Ok(Value::Bool(substr == needle))
    }

    pub(super) fn dispatch_substr(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let s = target.to_string_value();
        let chars: Vec<char> = s.chars().collect();
        let total_len = chars.len();

        // First arg: start position
        let start = if let Some(pos) = args.first() {
            match pos {
                Value::Int(i) => {
                    let i = *i;
                    if i < 0 {
                        (total_len as i64 + i).max(0) as usize
                    } else {
                        i as usize
                    }
                }
                other => other.to_string_value().parse::<i64>().unwrap_or(0).max(0) as usize,
            }
        } else {
            0
        };

        // Second arg: length (can be Int, WhateverCode/Sub, or absent)
        let end = if let Some(len_val) = args.get(1) {
            match len_val {
                Value::Int(i) => {
                    let len = (*i).max(0) as usize;
                    (start + len).min(total_len)
                }
                Value::Sub { .. } => {
                    // WhateverCode: call with remaining length to get actual length
                    let remaining = if start <= total_len {
                        (total_len - start) as i64
                    } else {
                        0
                    };
                    let result =
                        self.eval_call_on_value(len_val.clone(), vec![Value::Int(remaining)])?;
                    let len = match &result {
                        Value::Int(i) => (*i).max(0) as usize,
                        _ => 0,
                    };
                    (start + len).min(total_len)
                }
                _ => total_len, // default: take rest
            }
        } else {
            total_len // no length: take rest
        };

        let start = start.min(total_len);
        Ok(Value::Str(chars[start..end].iter().collect()))
    }
}
