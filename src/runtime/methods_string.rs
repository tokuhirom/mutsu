use super::*;
use crate::symbol::Symbol;

/// The `:samecase`/`:samemark`/`:samespace` substitution adverbs, applied to a
/// computed replacement against the matched text (shared with the `s///`
/// operator via `apply_subst_case_transforms`).
#[derive(Clone, Copy, Default)]
pub(super) struct SubstCaseTransforms {
    pub samecase: bool,
    pub samemark: bool,
    pub sigspace: bool,
    pub samespace: bool,
}

impl SubstCaseTransforms {
    fn any(&self) -> bool {
        self.samecase || self.samemark || self.samespace
    }

    fn apply(&self, replacement: &str, matched: &str) -> String {
        if !self.any() {
            return replacement.to_string();
        }
        crate::vm::vm_string_regex_ops::apply_subst_case_transforms(
            replacement,
            matched,
            self.samecase,
            self.samemark,
            self.sigspace,
            self.samespace,
        )
    }
}

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
                        // Pre-encode the replacement string in the target encoding
                        let repl_encoded = {
                            let (enc_bytes, _, _) = enc.encode(repl);
                            enc_bytes.into_owned()
                        };
                        let mut result = Vec::new();
                        for ch in input.chars() {
                            let s = ch.to_string();
                            let (encoded, _enc, had_errors) = enc.encode(&s);
                            if had_errors || !Self::char_is_encodable(ch, &encoded, enc) {
                                result.extend_from_slice(&repl_encoded);
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
            "utf8-c8" => Ok(super::utf8_c8::encode_utf8_c8(input)),
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
                        let (encoded, _, had_errors) = enc.encode(&s);
                        if had_errors || !Self::char_is_encodable(ch, &encoded, enc) {
                            return Err(RuntimeError::new(format!(
                                "Error encoding {} string: could not encode codepoint 0x{:x}",
                                encoding, ch as u32
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

    /// Strict ASCII decode: every byte must be <= 0x7F, otherwise Raku throws
    /// `X::Str::Encode`-style "Will not decode invalid ASCII" rather than
    /// silently substituting a replacement character.
    fn decode_ascii_strict(bytes: &[u8]) -> Result<String, RuntimeError> {
        if let Some(b) = bytes.iter().find(|b| **b > 0x7F) {
            return Err(RuntimeError::new(format!(
                "Will not decode invalid ASCII (code point ({}) > 127 found)",
                b
            )));
        }
        Ok(bytes.iter().map(|b| *b as char).collect())
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
            "utf8-c8" => Ok(super::utf8_c8::decode_utf8_c8(bytes)),
            "ascii" => Self::decode_ascii_strict(bytes),
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

    /// Decode bytes with an optional replacement string for unmappable/invalid
    /// sequences.  When `replacement` is `None` and the input contains errors,
    /// a `RuntimeError` is raised (Raku strict-decode semantics).  When
    /// `replacement` is `Some(r)`, every error sequence is replaced by `r`.
    pub(crate) fn decode_with_encoding_and_replacement(
        &self,
        bytes: &[u8],
        encoding_name: &str,
        replacement: Option<&str>,
    ) -> Result<String, RuntimeError> {
        let encoding = self
            .find_encoding(encoding_name)
            .map(|e| e.name.as_str())
            .unwrap_or(encoding_name)
            .to_lowercase();

        match encoding.as_str() {
            "utf8-c8" => Ok(super::utf8_c8::decode_utf8_c8(bytes)),
            "ascii" => match replacement {
                Some(repl) => Ok(bytes
                    .iter()
                    .map(|b| {
                        if *b <= 0x7F {
                            (*b as char).to_string()
                        } else {
                            repl.to_string()
                        }
                    })
                    .collect()),
                None => Self::decode_ascii_strict(bytes),
            },
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
                    if let Some(repl) = replacement {
                        return Ok(Self::decode_utf8_with_replacement(bytes, repl));
                    }
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
                    return Self::decode_encoding_rs(enc, bytes, replacement);
                }
                Ok(String::from_utf8_lossy(bytes).into_owned())
            }
        }
    }

    /// Decode bytes using encoding_rs with proper error handling.
    /// When `replacement` is None, throws on any decode error.
    /// When `replacement` is Some(r), replaces error sequences with `r`.
    pub(crate) fn decode_encoding_rs(
        enc: &'static encoding_rs::Encoding,
        bytes: &[u8],
        replacement: Option<&str>,
    ) -> Result<String, RuntimeError> {
        // Fast path: if no replacement, try lossy decode and check for errors
        if replacement.is_none() {
            let (decoded, _used_encoding, had_errors) = enc.decode(bytes);
            if had_errors {
                return Err(RuntimeError::new(format!(
                    "Error decoding {} byte(s) as {}",
                    bytes.len(),
                    enc.name()
                )));
            }
            return Ok(decoded.into_owned());
        }
        let repl = replacement.unwrap();
        // Slow path: incremental decode to insert custom replacement strings
        let mut decoder = enc.new_decoder_without_bom_handling();
        let max_len = decoder
            .max_utf8_buffer_length_without_replacement(bytes.len())
            .unwrap_or(bytes.len() * 4);
        let mut output = String::with_capacity(max_len);
        let mut total_read = 0;
        loop {
            if total_read > bytes.len() {
                break;
            }
            let src = &bytes[total_read..];
            let is_last = true; // We provide all remaining input at once
            let needed = decoder
                .max_utf8_buffer_length_without_replacement(src.len())
                .unwrap_or(src.len() * 4 + 16);
            output.reserve(needed);
            let (result, read) =
                decoder.decode_to_string_without_replacement(src, &mut output, is_last);
            total_read += read;
            match result {
                encoding_rs::DecoderResult::InputEmpty => {
                    break;
                }
                encoding_rs::DecoderResult::Malformed(_, _) => {
                    output.push_str(repl);
                    // The malformed bytes are already accounted for in `read`
                    // (the decoder consumed them). Continue decoding the rest.
                }
                encoding_rs::DecoderResult::OutputFull => {
                    // Grow output and retry
                    output.reserve(needed + 1024);
                }
            }
        }
        Ok(output)
    }

    /// Decode UTF-8 bytes with a replacement string for invalid sequences.
    fn decode_utf8_with_replacement(bytes: &[u8], replacement: &str) -> String {
        let mut result = String::new();
        let mut pos = 0;
        while pos < bytes.len() {
            match std::str::from_utf8(&bytes[pos..]) {
                Ok(s) => {
                    result.push_str(s);
                    break;
                }
                Err(e) => {
                    let valid_up_to = e.valid_up_to();
                    if valid_up_to > 0 {
                        result
                            .push_str(std::str::from_utf8(&bytes[pos..pos + valid_up_to]).unwrap());
                    }
                    result.push_str(replacement);
                    pos += valid_up_to + e.error_len().unwrap_or(1);
                }
            }
        }
        result
    }

    /// Check whether a character can be encoded in the given encoding following
    /// Raku's rules. This catches cases where `encoding_rs` (WHATWG) silently
    /// maps codepoints that Raku considers non-encodable:
    /// - GB18030: U+E000..U+E864 (PUA range excluded by the Chinese national
    ///   standard but mapped by WHATWG).
    /// - Other encodings: round-trip check (encode then decode must produce the
    ///   same character).
    fn char_is_encodable(ch: char, encoded: &[u8], enc: &'static encoding_rs::Encoding) -> bool {
        // GB18030-specific: Raku rejects PUA U+E000..U+E864.
        if enc == encoding_rs::GB18030 {
            let cp = ch as u32;
            if (0xE000..=0xE864).contains(&cp) {
                return false;
            }
        }
        // General round-trip check for other mismatches.
        let (decoded, _, had_errors) = enc.decode(encoded);
        if had_errors {
            return false;
        }
        let mut chars = decoded.chars();
        matches!(chars.next(), Some(c) if c == ch) && chars.next().is_none()
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
    /// Validate an `:nth` index list for substitution: every index must be at
    /// least 1 and the list must be monotonically increasing. Rakudo's lazy
    /// match iterator cannot rewind, so a non-increasing list (e.g.
    /// `:nth(2,4,1,6)`) or a zero/negative index throws.
    fn validate_subst_nth_list(nth_list: &[i64]) -> Result<(), RuntimeError> {
        let mut prev: i64 = 0;
        for &n in nth_list {
            if n < 1 {
                return Err(RuntimeError::new(format!(
                    "Attempt to retrieve before :1st match -- :nth({n})"
                )));
            }
            if n < prev {
                return Err(RuntimeError::new(format!(
                    "Attempt to fetch match #{n} after #{prev}"
                )));
            }
            prev = n;
        }
        Ok(())
    }

    pub(super) fn dispatch_subst(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let text = target.to_string_value();
        let mut positional: Vec<Value> = Vec::new();
        let mut global = false;
        let mut nth: Option<Vec<i64>> = None;
        let mut x_count: Option<Value> = None;
        let mut pos_start: Option<usize> = None;
        let mut continue_from: Option<usize> = None;
        let mut transforms = SubstCaseTransforms::default();
        for arg in args {
            if let Value::Pair(key, value) = arg {
                match key.as_str() {
                    "g" | "global" => global = value.truthy(),
                    "x" => x_count = Some(*value.clone()),
                    "nth" => match value.as_ref() {
                        Value::Int(n) => nth = Some(vec![*n]),
                        Value::Array(items, _) => {
                            nth = Some(items.iter().map(|v: &Value| v.to_f64() as i64).collect());
                        }
                        _ => nth = Some(vec![value.to_f64() as i64]),
                    },
                    "1st" | "first" if value.truthy() => nth = Some(vec![1]),
                    "2nd" | "second" if value.truthy() => nth = Some(vec![2]),
                    "3rd" | "third" if value.truthy() => nth = Some(vec![3]),
                    "4th" | "fourth" if value.truthy() => nth = Some(vec![4]),
                    "p" | "pos" => pos_start = Some(value.to_f64() as usize),
                    "c" | "continue" => continue_from = Some(value.to_f64() as usize),
                    "samecase" | "ii" => transforms.samecase = value.truthy(),
                    "samemark" | "mm" => transforms.samemark = value.truthy(),
                    "samespace" | "ss" => transforms.samespace = value.truthy(),
                    "sigspace" | "s" => transforms.sigspace = value.truthy(),
                    _ => {}
                }
            } else {
                positional.push(arg.clone());
            }
        }
        // The `:x` adverb must be an Int/Range (or Whatever); anything else
        // (e.g. a class instance) is an X::Str::Match::x error, mirroring `.match`.
        if let Some(x) = &x_count
            && !Self::is_valid_match_x_arg(x)
        {
            return Err(Self::str_match_x_error("subst", x));
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

        // Helper to determine how many matches to use based on :x
        let resolve_x_count = |x: &Option<Value>| -> Option<(usize, usize)> {
            match x {
                None => None,
                Some(Value::Int(n)) => Some((*n as usize, *n as usize)),
                Some(Value::Str(s)) if s.as_str() == "Inf" || s.as_str() == "*" => {
                    Some((0, usize::MAX))
                }
                Some(Value::Whatever) | Some(Value::Sub(_)) | Some(Value::WeakSub(_)) => {
                    Some((0, usize::MAX))
                }
                Some(Value::Range(lo, hi)) => {
                    if *lo > *hi {
                        Some((usize::MAX, 0)) // Empty range: always fail
                    } else {
                        Some((*lo as usize, *hi as usize))
                    }
                }
                Some(Value::RangeExcl(lo, hi)) => {
                    Some((*lo as usize, (*hi as usize).saturating_sub(1)))
                }
                Some(Value::RangeExclStart(lo, hi)) => Some(((*lo as usize) + 1, *hi as usize)),
                Some(Value::RangeExclBoth(lo, hi)) => {
                    Some(((*lo as usize) + 1, (*hi as usize).saturating_sub(1)))
                }
                Some(v) => Some((v.to_f64() as usize, v.to_f64() as usize)),
            }
        };

        match pattern {
            Value::Regex(_) | Value::RegexWithAdverbs(_) => {
                let pat: &str = match pattern {
                    Value::Regex(p) => p,
                    Value::RegexWithAdverbs(a) => &a.pattern,
                    _ => unreachable!(),
                };
                let is_p5 = matches!(pattern, Value::RegexWithAdverbs(a) if a.perl5);
                let pat = if is_p5 {
                    self.interpolate_regex_pattern(pat)
                } else {
                    pat.to_string()
                };
                let pat_global =
                    matches!(pattern, Value::RegexWithAdverbs(a) if a.global) || global;
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
                    self.env.insert("/".to_string(), Value::Nil);
                    return Ok(Value::str(text));
                }
                let chars: Vec<char> = text.chars().collect();

                let has_adverbs = nth.is_some()
                    || x_count.is_some()
                    || pos_start.is_some()
                    || continue_from.is_some();

                if has_adverbs || pat_global {
                    let mut selected = self.select_non_overlapping_matches(all_captures);

                    // Apply :c(N) - filter matches starting from character position N
                    if let Some(c) = continue_from {
                        selected.retain(|cap| cap.from >= c);
                    }

                    // Apply :p(N) - first match must start at exactly position N
                    // If it doesn't, return original string (failure)
                    if let Some(p) = pos_start {
                        // Filter to matches at or after position p
                        selected.retain(|cap| cap.from >= p);
                        // The first remaining match must be exactly at p
                        if selected.is_empty() || selected[0].from != p {
                            self.env.insert("/".to_string(), Value::Nil);
                            return Ok(Value::str(text));
                        }
                    }

                    // If no :g, :x, :nth - single match mode
                    if !pat_global && x_count.is_none() && nth.is_none() {
                        selected.truncate(1);
                    }

                    // Apply :nth - select specific 1-based match indices
                    if let Some(ref nth_list) = nth {
                        Self::validate_subst_nth_list(nth_list)?;
                        let total = selected.len();
                        let mut indices: Vec<usize> = Vec::new();
                        for &n in nth_list {
                            if (n as usize) <= total && !indices.contains(&(n as usize - 1)) {
                                indices.push(n as usize - 1);
                            }
                        }
                        let new_selected: Vec<_> = indices
                            .iter()
                            .filter_map(|&i| selected.get(i).cloned())
                            .collect();
                        selected = new_selected;
                    }

                    // Apply :x(N) - select exactly N matches (or range)
                    if let Some((lo, hi)) = resolve_x_count(&x_count) {
                        let count = selected.len();
                        if count < lo {
                            self.env.insert("/".to_string(), Value::Nil);
                            return Ok(Value::str(text));
                        }
                        if count > hi {
                            selected.truncate(hi);
                        }
                    }

                    if selected.is_empty() {
                        self.env.insert("/".to_string(), Value::Nil);
                        return Ok(Value::str(text));
                    }

                    let mut result = String::new();
                    let mut last_end = 0;
                    for captures in &selected {
                        let prefix: String = chars[last_end..captures.from].iter().collect();
                        result.push_str(&prefix);
                        let repl = self.eval_subst_replacement_cased(
                            &replacement_val,
                            is_closure,
                            &replacement_str,
                            &captures.matched,
                            Some(captures),
                            Some(&text),
                            transforms,
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
                    let repl = self.eval_subst_replacement_cased(
                        &replacement_val,
                        is_closure,
                        &replacement_str,
                        &captures.matched,
                        Some(&captures),
                        Some(&text),
                        transforms,
                    )?;
                    Ok(Value::str(format!("{}{}{}", prefix, repl, suffix)))
                } else {
                    Ok(Value::str(text))
                }
            }
            Value::Str(pat) => {
                let has_adverbs = nth.is_some()
                    || x_count.is_some()
                    || pos_start.is_some()
                    || continue_from.is_some();
                if has_adverbs || global {
                    let pat_str = pat.as_str();
                    let mut str_matches: Vec<(usize, usize)> = Vec::new();
                    let mut search_start = 0;
                    while let Some(pos) = text[search_start..].find(pat_str) {
                        let abs_pos = search_start + pos;
                        str_matches.push((abs_pos, abs_pos + pat_str.len()));
                        search_start = abs_pos + pat_str.len().max(1);
                    }

                    let char_indices: Vec<(usize, usize)> = str_matches
                        .iter()
                        .map(|&(start, end)| {
                            let cs = text[..start].chars().count();
                            let ce = text[..end].chars().count();
                            (cs, ce)
                        })
                        .collect();

                    let mut keep: Vec<usize> = (0..str_matches.len()).collect();

                    if let Some(c) = continue_from {
                        keep.retain(|&i| char_indices[i].0 >= c);
                    }
                    if let Some(p) = pos_start {
                        keep.retain(|&i| char_indices[i].0 == p);
                    }
                    if !global && nth.is_none() && x_count.is_none() {
                        keep.truncate(1);
                    }
                    if let Some(ref nth_list) = nth {
                        Self::validate_subst_nth_list(nth_list)?;
                        let total = keep.len();
                        let mut selected: Vec<usize> = Vec::new();
                        for &n in nth_list {
                            if (n as usize) <= total {
                                let chosen = keep[n as usize - 1];
                                if !selected.contains(&chosen) {
                                    selected.push(chosen);
                                }
                            }
                        }
                        keep = selected;
                    }
                    if let Some((lo, hi)) = resolve_x_count(&x_count) {
                        let count = keep.len();
                        if count < lo {
                            return Ok(Value::str(text));
                        }
                        if count > hi {
                            keep.truncate(hi);
                        }
                    }

                    let mut result = String::new();
                    let mut last_end = 0;
                    for &idx in &keep {
                        let (start, end) = str_matches[idx];
                        result.push_str(&text[last_end..start]);
                        result.push_str(&transforms.apply(&replacement_str, &text[start..end]));
                        last_end = end;
                    }
                    result.push_str(&text[last_end..]);
                    Ok(Value::str(result))
                } else {
                    let repl = transforms.apply(&replacement_str, pat.as_str());
                    Ok(Value::str(text.replacen(pat.as_str(), &repl, 1)))
                }
            }
            _ => {
                let pat_str = pattern.to_string_value();
                let repl = transforms.apply(&replacement_str, &pat_str);
                if global {
                    Ok(Value::str(text.replace(&pat_str, &repl)))
                } else {
                    Ok(Value::str(text.replacen(&pat_str, &repl, 1)))
                }
            }
        }
    }

    /// Evaluate a subst replacement — either a static string or a closure call.
    /// Evaluate a subst replacement — either a static string or a closure call —
    /// and apply the `:samecase`/`:samemark`/`:samespace` transforms (against
    /// `matched_text`) to the result, matching the `s///` operator.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn eval_subst_replacement_cased(
        &mut self,
        replacement_val: &Option<Value>,
        is_closure: bool,
        replacement_str: &str,
        matched_text: &str,
        captures: Option<&RegexCaptures>,
        orig_text: Option<&str>,
        transforms: SubstCaseTransforms,
    ) -> Result<String, RuntimeError> {
        let cased = |s: String| -> String { transforms.apply(&s, matched_text) };
        if !is_closure {
            // Expand $0, $1, ... capture references in string replacements
            if let Some(caps) = captures
                && !caps.positional.is_empty()
            {
                let expanded = crate::vm::vm_string_regex_ops::expand_capture_refs(
                    replacement_str,
                    &caps.positional,
                );
                return Ok(cased(expanded));
            }
            return Ok(cased(replacement_str.to_string()));
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
            self.env.insert_sym(*k, v.clone());
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
            self.env.insert("_".to_string(), match_obj.clone());
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
                && let Some(Value::Hash(named_hash)) = attributes.as_map().get("named")
            {
                for (k, v) in named_hash.iter() {
                    self.env.insert(format!("<{}>", k), v.clone());
                }
            }
        } else {
            let match_val = Value::str(matched_text.to_string());
            self.env.insert("/".to_string(), match_val.clone());
            self.env.insert("$_".to_string(), match_val.clone());
            self.env.insert("_".to_string(), match_val);
        }
        let result = self.eval_block_value(&sub_data.body).unwrap_or(Value::Nil);
        self.env = saved;
        Ok(cased(result.to_string_value()))
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
