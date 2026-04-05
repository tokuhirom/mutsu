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

    pub(crate) fn translate_newlines_for_decode(&self, input: &str) -> String {
        match self.newline_mode {
            NewlineMode::Lf => input.to_string(),
            NewlineMode::Cr => input.replace('\r', "\n"),
            NewlineMode::Crlf => input.replace("\r\n", "\n"),
        }
    }

    pub(super) fn encode_with_encoding_and_replacement(
        &self,
        input: &str,
        encoding_name: &str,
        replacement: Option<&str>,
    ) -> Result<Vec<u8>, RuntimeError> {
        let encoding = self
            .find_encoding(encoding_name)
            .map(|e| e.name.as_str())
            .unwrap_or(encoding_name)
            .to_lowercase();

        if let Some(repl) = replacement {
            match encoding.as_str() {
                "ascii" => {
                    return Ok(input
                        .chars()
                        .flat_map(|ch| {
                            if (ch as u32) <= 0x7F {
                                vec![ch as u8]
                            } else {
                                repl.as_bytes().to_vec()
                            }
                        })
                        .collect());
                }
                "iso-8859-1" => {
                    return Ok(input
                        .chars()
                        .flat_map(|ch| {
                            if (ch as u32) <= 0xFF {
                                vec![ch as u8]
                            } else {
                                repl.as_bytes().to_vec()
                            }
                        })
                        .collect());
                }
                _ => {
                    if let Some(enc) = Self::lookup_encoding_rs_codec(&encoding) {
                        let mut result = Vec::new();
                        for ch in input.chars() {
                            let s = ch.to_string();
                            let (encoded, _enc, had_errors) = enc.encode(&s);
                            if had_errors {
                                result.extend_from_slice(repl.as_bytes());
                            } else {
                                result.extend_from_slice(&encoded);
                            }
                        }
                        return Ok(result);
                    }
                }
            }
        }
        self.encode_with_encoding(input, encoding_name)
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
            "ascii" => {
                for ch in input.chars() {
                    if (ch as u32) > 0x7F {
                        return Err(RuntimeError::new(format!(
                            "Error encoding ascii string: character '{}' (U+{:04X}) is not representable",
                            ch, ch as u32
                        )));
                    }
                }
                Ok(input.bytes().collect())
            }
            "iso-8859-1" => {
                for ch in input.chars() {
                    if (ch as u32) > 0xFF {
                        return Err(RuntimeError::new(format!(
                            "Error encoding iso-8859-1 string: character '{}' (U+{:04X}) is not representable",
                            ch, ch as u32
                        )));
                    }
                }
                Ok(input.chars().map(|c| c as u8).collect())
            }
            "utf-16" | "utf-16le" => {
                Ok(input.encode_utf16().flat_map(|u| u.to_le_bytes()).collect())
            }
            "utf-16be" => Ok(input.encode_utf16().flat_map(|u| u.to_be_bytes()).collect()),
            _ => {
                if let Some(enc) = Self::lookup_encoding_rs_codec(&encoding) {
                    for ch in input.chars() {
                        let s = ch.to_string();
                        let (_, _, had_errors) = enc.encode(&s);
                        if had_errors {
                            return Err(RuntimeError::new(format!(
                                "Error encoding {} string: character '{}' (U+{:04X}) is not representable",
                                encoding, ch, ch as u32
                            )));
                        }
                    }
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
            "utf-16" | "utf16" => {
                let (data, be) = if bytes.len() >= 2 && bytes[0] == 0xFE && bytes[1] == 0xFF {
                    (&bytes[2..], true)
                } else if bytes.len() >= 2 && bytes[0] == 0xFF && bytes[1] == 0xFE {
                    (&bytes[2..], false)
                } else {
                    (bytes, false)
                };
                if !data.len().is_multiple_of(2) {
                    return Err(RuntimeError::new("Invalid utf-16 byte length"));
                }
                let units: Vec<u16> = data
                    .chunks_exact(2)
                    .map(|c| {
                        if be {
                            u16::from_be_bytes([c[0], c[1]])
                        } else {
                            u16::from_le_bytes([c[0], c[1]])
                        }
                    })
                    .collect();
                Ok(String::from_utf16_lossy(&units))
            }
            "utf-16le" | "utf16le" => {
                if !bytes.len().is_multiple_of(2) {
                    return Err(RuntimeError::new("Invalid utf-16 byte length"));
                }
                let units: Vec<u16> = bytes
                    .chunks_exact(2)
                    .map(|c| u16::from_le_bytes([c[0], c[1]]))
                    .collect();
                Ok(String::from_utf16_lossy(&units))
            }
            "utf-16be" | "utf16be" => {
                if !bytes.len().is_multiple_of(2) {
                    return Err(RuntimeError::new("Invalid utf-16be byte length"));
                }
                let units: Vec<u16> = bytes
                    .chunks_exact(2)
                    .map(|c| u16::from_be_bytes([c[0], c[1]]))
                    .collect();
                Ok(String::from_utf16_lossy(&units))
            }
            "utf-8" | "utf8" => match std::str::from_utf8(bytes) {
                Ok(s) => Ok(s.strip_prefix('\u{FEFF}').unwrap_or(s).to_string()),
                Err(e) => {
                    let vup = e.valid_up_to();
                    if e.error_len().is_none() {
                        Err(RuntimeError::new(
                            "Malformed termination of UTF-8 string".to_string(),
                        ))
                    } else {
                        let pfx = std::str::from_utf8(&bytes[..vup]).unwrap_or("");
                        let (mut line, mut col) = (1usize, 1usize);
                        for ch in pfx.chars() {
                            if ch == '\n' {
                                line += 1;
                                col = 1;
                            } else {
                                col += 1;
                            }
                        }
                        let s = if vup > 0 { vup - 1 } else { 0 };
                        let end = (vup + 2).min(bytes.len());
                        let near: Vec<String> =
                            bytes[s..end].iter().map(|b| format!("{:02x}", b)).collect();
                        Err(RuntimeError::new(format!(
                            "Malformed UTF-8 near bytes {} at line {} col {}",
                            near.join(" "),
                            line,
                            col
                        )))
                    }
                }
            },
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
            Value::Regex(pat) | Value::RegexWithAdverbs { pattern: pat, .. } => {
                let is_p5 = matches!(pattern, Value::RegexWithAdverbs { perl5: true, .. });
                let pat = if is_p5 {
                    self.interpolate_regex_pattern(pat)
                } else {
                    pat.to_string()
                };
                let pat_global =
                    matches!(pattern, Value::RegexWithAdverbs { global: true, .. }) || global;
                let all_captures = if is_p5 {
                    #[cfg(feature = "pcre2")]
                    {
                        self.regex_match_all_with_captures_p5(&pat, &text)
                    }
                    #[cfg(not(feature = "pcre2"))]
                    {
                        self.regex_match_all_with_captures(&pat, &text)
                    }
                } else {
                    self.regex_match_all_with_captures(&pat, &text)
                };
                if all_captures.is_empty() {
                    return Ok(Value::str(text));
                }
                let chars: Vec<char> = text.chars().collect();
                if pat_global {
                    let selected = self.select_non_overlapping_matches(all_captures);
                    let mut result = String::new();
                    let mut last_end = 0;
                    for captures in &selected {
                        let prefix: String = chars[last_end..captures.from].iter().collect();
                        result.push_str(&prefix);
                        let repl = self.eval_subst_replacement(
                            &replacement_val,
                            is_closure,
                            &replacement_str,
                            &captures.matched,
                            Some(captures),
                            Some(&text),
                        )?;
                        result.push_str(&repl);
                        last_end = captures.to;
                    }
                    let suffix: String = chars[last_end..].iter().collect();
                    result.push_str(&suffix);
                    Ok(Value::str(result))
                } else if let Some(captures) = {
                    if is_p5 {
                        #[cfg(feature = "pcre2")]
                        {
                            self.regex_match_with_captures_p5(&pat, &text)
                        }
                        #[cfg(not(feature = "pcre2"))]
                        {
                            self.regex_match_with_captures(&pat, &text)
                        }
                    } else {
                        self.regex_match_with_captures(&pat, &text)
                    }
                } {
                    // Set $/ to the match object
                    let match_obj = Value::make_match_object_full(
                        captures.matched.clone(),
                        captures.from as i64,
                        captures.to as i64,
                        &captures.positional,
                        &captures.named,
                        &captures.named_subcaps,
                        &captures.positional_subcaps,
                        &captures.positional_quantified,
                        Some(&text),
                    );
                    self.env.insert("/".to_string(), match_obj);
                    let prefix: String = chars[..captures.from].iter().collect();
                    let suffix: String = chars[captures.to..].iter().collect();
                    let repl = self.eval_subst_replacement(
                        &replacement_val,
                        is_closure,
                        &replacement_str,
                        &captures.matched,
                        Some(&captures),
                        Some(&text),
                    )?;
                    Ok(Value::str(format!("{}{}{}", prefix, repl, suffix)))
                } else {
                    Ok(Value::str(text))
                }
            }
            Value::Str(pat) => {
                if global {
                    Ok(Value::str(text.replace(pat.as_str(), &replacement_str)))
                } else {
                    Ok(Value::str(text.replacen(pat.as_str(), &replacement_str, 1)))
                }
            }
            _ => {
                let pat_str = pattern.to_string_value();
                if global {
                    Ok(Value::str(text.replace(&pat_str, &replacement_str)))
                } else {
                    Ok(Value::str(text.replacen(&pat_str, &replacement_str, 1)))
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
        captures: Option<&RegexCaptures>,
        orig_text: Option<&str>,
    ) -> Result<String, RuntimeError> {
        if !is_closure {
            // Expand $0, $1, ... capture references in string replacements
            if let Some(caps) = captures
                && !caps.positional.is_empty()
            {
                let expanded = crate::vm::vm_string_regex_ops::expand_capture_refs(
                    replacement_str,
                    &caps.positional,
                );
                return Ok(expanded);
            }
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
        if let Some(captures) = captures {
            let match_obj = Value::make_match_object_full(
                captures.matched.clone(),
                captures.from as i64,
                captures.to as i64,
                &captures.positional,
                &captures.named,
                &captures.named_subcaps,
                &captures.positional_subcaps,
                &captures.positional_quantified,
                orig_text,
            );
            self.env.insert("/".to_string(), match_obj.clone());
            self.env.insert("$_".to_string(), match_obj.clone());
            let positional_len = captures
                .positional_slots
                .len()
                .max(captures.positional.len());
            for i in 0..positional_len {
                let value = if let Some(Some((capture, _, _))) = captures.positional_slots.get(i) {
                    Value::str(capture.clone())
                } else if let Some(capture) = captures.positional.get(i) {
                    Value::str(capture.clone())
                } else {
                    Value::Nil
                };
                self.env.insert(i.to_string(), value);
            }
            if positional_len == 0 {
                self.env.insert("0".to_string(), Value::Nil);
            }
            if let Value::Instance { ref attributes, .. } = match_obj
                && let Some(Value::Hash(named_hash)) = attributes.get("named")
            {
                for (k, v) in named_hash.iter() {
                    self.env.insert(format!("<{}>", k), v.clone());
                }
            }
        } else {
            let match_val = Value::str(matched_text.to_string());
            self.env.insert("/".to_string(), match_val.clone());
            self.env.insert("$_".to_string(), match_val);
        }
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
    fn runtime_error_to_failure(err: RuntimeError) -> Value {
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
        if positional.is_empty() {
            return Err(RuntimeError::new(
                "Too few positionals passed to 'substr-eq'",
            ));
        }
        // Type objects (Package) as needle should throw
        if let Value::Package(type_name) = &positional[0] {
            return Err(RuntimeError::new(format!(
                "Cannot resolve caller substr-eq({}:U)",
                type_name
            )));
        }
        let text = target.to_string_value();
        let needle = positional[0].to_string_value();
        let len = text.chars().count() as i64;
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
                Value::Int(start),
                &format!("0..{}", len),
            ));
        }
        let substr: String = text
            .chars()
            .skip(start as usize)
            .take(needle.chars().count())
            .collect();
        let eq = match (ignore_case, ignore_mark) {
            (false, false) => substr == needle,
            (true, false) => substr.to_lowercase() == needle.to_lowercase(),
            (false, true) => self.strip_marks(&substr) == self.strip_marks(&needle),
            (true, true) => {
                self.strip_marks(&substr).to_lowercase() == self.strip_marks(&needle).to_lowercase()
            }
        };
        Ok(Value::Bool(eq))
    }

    pub(super) fn dispatch_substr(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let s = target.to_string_value();
        let chars: Vec<char> = s.chars().collect();
        let total_len = chars.len();

        // Check if first arg is a Range — handle substr($str, 6..8) form
        if let Some(first_arg) = args.first()
            && let Some((range_start, range_end)) =
                self.substr_extract_range(first_arg, total_len)?
        {
            let rs = range_start.min(total_len);
            let re = range_end.min(total_len);
            return Ok(Value::str(chars[rs..re].iter().collect()));
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
            match len_val {
                Value::Int(i) => {
                    let len = (*i).max(0) as usize;
                    (start + len).min(total_len)
                }
                Value::Num(f) if f.is_infinite() && *f > 0.0 => total_len,
                Value::Num(f) => {
                    let len = (*f as i64).max(0) as usize;
                    (start + len).min(total_len)
                }
                Value::Rat(n, d) if *d != 0 => {
                    let len = (*n / *d).max(0) as usize;
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
                        Value::Num(f) => (*f as i64).max(0) as usize,
                        Value::Rat(n, d) if *d != 0 => (*n / *d).max(0) as usize,
                        _ => 0,
                    };
                    (start + len).min(total_len)
                }
                _ => total_len, // default: take rest
            }
        } else {
            total_len // no length: take rest
        };

        Ok(Value::str(chars[start..end].iter().collect()))
    }

    /// substr-rw in non-lvalue context: just return the substring (same as substr).
    /// When a variable name is available, returns a Proxy for binding support.
    pub(super) fn dispatch_substr_rw(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        self.dispatch_substr(target, args)
    }

    /// Create a Proxy for substr-rw binding.
    /// The Proxy's FETCH returns the current substring and STORE modifies the original string.
    pub(crate) fn make_substr_rw_proxy(
        &mut self,
        var_name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let target = self
            .env
            .get(var_name)
            .cloned()
            .unwrap_or(Value::str(String::new()));
        let s = target.to_string_value();
        let chars: Vec<char> = s.chars().collect();
        let str_len = chars.len();

        // Resolve range
        let (start, end) = self.resolve_substr_rw_range(args, str_len)?;
        let len = end - start;

        let make_param_def = |name: &str| crate::ast::ParamDef {
            name: name.to_string(),
            default: None,
            multi_invocant: false,
            required: false,
            named: false,
            slurpy: false,
            double_slurpy: false,
            onearg: false,
            sigilless: false,
            type_constraint: None,
            literal_value: None,
            sub_signature: None,
            where_constraint: None,
            traits: Vec::new(),
            optional_marker: false,
            outer_sub_signature: None,
            code_signature: None,
            is_invocant: false,
            shape_constraints: None,
        };

        // Create FETCH sub: reads substr from the variable
        let fetch_body = vec![crate::ast::Stmt::Expr(crate::ast::Expr::MethodCall {
            target: Box::new(crate::ast::Expr::Var(var_name.to_string())),
            name: crate::symbol::Symbol::intern("substr"),
            args: vec![
                crate::ast::Expr::Literal(Value::Int(start as i64)),
                crate::ast::Expr::Literal(Value::Int(len as i64)),
            ],
            modifier: None,
            quoted: false,
        })];
        let fetcher = Value::make_sub(
            crate::symbol::Symbol::intern(""),
            crate::symbol::Symbol::intern("__substr_rw_fetch"),
            vec!["$".to_string()],
            vec![make_param_def("$")],
            fetch_body,
            false,
            self.env.clone(),
        );

        // Create STORE sub: modifies the variable via substr-rw lvalue assignment
        let store_body = vec![crate::ast::Stmt::Expr(crate::ast::Expr::Call {
            name: crate::symbol::Symbol::intern("__mutsu_assign_named_sub_lvalue"),
            args: vec![
                crate::ast::Expr::Literal(Value::str("substr-rw".to_string())),
                crate::ast::Expr::ArrayLiteral(vec![
                    crate::ast::Expr::Var(var_name.to_string()),
                    crate::ast::Expr::Literal(Value::Int(start as i64)),
                    crate::ast::Expr::Literal(Value::Int(len as i64)),
                ]),
                crate::ast::Expr::Var("__mutsu_substr_rw_store_value".to_string()),
            ],
        })];
        let storer = Value::make_sub(
            crate::symbol::Symbol::intern(""),
            crate::symbol::Symbol::intern("__substr_rw_store"),
            vec!["$".to_string(), "__mutsu_substr_rw_store_value".to_string()],
            vec![
                make_param_def("$"),
                make_param_def("__mutsu_substr_rw_store_value"),
            ],
            store_body,
            false,
            self.env.clone(),
        );

        Ok(Value::Proxy {
            fetcher: Box::new(fetcher),
            storer: Box::new(storer),
            subclass: None,
            decontainerized: false,
        })
    }

    /// Resolve a position argument to an i64, handling Int, Num, Rat, BigInt, WhateverCode/Sub.
    pub(crate) fn substr_resolve_position(
        &mut self,
        pos: &Value,
        total_len: usize,
    ) -> Result<i64, RuntimeError> {
        match pos {
            Value::Int(i) => Ok(*i),
            Value::Num(f) => Ok(*f as i64),
            Value::Rat(n, d) if *d != 0 => Ok(*n / *d),
            Value::BigInt(b) => {
                if b.as_ref() > &num_bigint::BigInt::from(i64::MAX)
                    || b.as_ref() < &num_bigint::BigInt::from(i64::MIN)
                {
                    Err(self.out_of_range_error(Value::bigint((**b).clone())))
                } else {
                    Ok(b.to_string().parse::<i64>().unwrap_or(0))
                }
            }
            Value::Sub { .. } => {
                // WhateverCode/Callable: call with total_len to resolve position
                let result =
                    self.eval_call_on_value(pos.clone(), vec![Value::Int(total_len as i64)])?;
                match &result {
                    Value::Int(i) => Ok(*i),
                    Value::Num(f) => Ok(*f as i64),
                    Value::Rat(n, d) if *d != 0 => Ok(*n / *d),
                    _ => Ok(0),
                }
            }
            other => Ok(other.to_string_value().parse::<i64>().unwrap_or(0)),
        }
    }

    /// Extract start/end indices from a Range value for substr.
    /// Returns Some((start, end)) if the value is a Range, None otherwise.
    pub(crate) fn substr_extract_range(
        &mut self,
        val: &Value,
        total_len: usize,
    ) -> Result<Option<(usize, usize)>, RuntimeError> {
        match val {
            Value::Range(a, b) => {
                let end = b.saturating_add(1).max(0) as usize;
                Ok(Some((*a as usize, end)))
            }
            Value::RangeExcl(a, b) => Ok(Some((*a as usize, *b as usize))),
            Value::RangeExclStart(a, b) => {
                let end = b.saturating_add(1).max(0) as usize;
                Ok(Some(((*a + 1) as usize, end)))
            }
            Value::RangeExclBoth(a, b) => Ok(Some(((*a + 1) as usize, *b as usize))),
            Value::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                let s = self.substr_resolve_position(start, total_len)?;
                let s = if *excl_start { s + 1 } else { s };
                let e_val = self.substr_resolve_position(end, total_len)?;
                // For Inf end (e.g., 10..*), e_val will be very large; clamp later
                let e = if *excl_end { e_val } else { e_val + 1 };
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
        failure_attrs.insert("handled".to_string(), Value::Bool(false));
        Ok(Value::make_instance(
            Symbol::intern("Failure"),
            failure_attrs,
        ))
    }
}
