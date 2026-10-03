use crate::value::{Value, ValueMap, make_rat};

use super::MAX_JSON_DEPTH;

/// Error from `from_json`: either a plain malformed-input message, or the
/// JSON::Fast `X::JSON::AdditionalContent` condition — the document parsed
/// cleanly but was followed by more non-whitespace text. Positions are in
/// characters (Raku `substr` semantics), not bytes.
pub(crate) enum FromJsonError {
    Parse(String),
    AdditionalContent {
        parsed: Value,
        parsed_length: usize,
        rest_position: usize,
    },
}

/// Parse a JSON string into a `Value`. With `immutable`, arrays decode as
/// `List` and objects as `Map` (JSON::Fast `:immutable`). With `allow_jsonc`,
/// `//` line and `/* */` block comments are skipped (JSONC).
// Cost: O(n), n = input bytes parsed into JSON values.
pub(crate) fn from_json(
    text: &str,
    immutable: bool,
    allow_jsonc: bool,
) -> Result<Value, FromJsonError> {
    let mut p = Parser {
        bytes: text.as_bytes(),
        chars: text,
        pos: 0,
        immutable,
        allow_jsonc,
    };
    p.skip_ws();
    let value = p.parse_value(0).map_err(FromJsonError::Parse)?;
    let parsed_length = text[..p.pos].chars().count();
    p.skip_ws();
    if p.pos != p.bytes.len() {
        return Err(FromJsonError::AdditionalContent {
            parsed: value,
            parsed_length,
            rest_position: text[..p.pos].chars().count(),
        });
    }
    Ok(value)
}

struct Parser<'a> {
    bytes: &'a [u8],
    chars: &'a str,
    pos: usize,
    immutable: bool,
    allow_jsonc: bool,
}

impl<'a> Parser<'a> {
    /// Wrap a decoded JSON object: mutable `Hash` by default, `Map` with
    /// `:immutable` (a Hash whose `declared_type` is "Map", mutsu's Map repr).
    fn finish_object(&self, map: ValueMap) -> Value {
        if self.immutable {
            let mut data: crate::value::HashData = map.into();
            data.declared_type = Some("Map".to_string());
            Value::hash_with_data(crate::gc::Gc::new(data))
        } else {
            // ADR-0040 slice 2: a decoded JSON object is a real `Hash`, so its
            // aggregate values are `Scalar` containers like any other stored
            // hash value -- `from-json('{"a":[1,2]}')<a>.raku` is `$[1, 2]`.
            // (The `:immutable` form is a `Map`, whose values are not
            // containers, so it is deliberately left alone.)
            let map: ValueMap = map
                .into_iter()
                .map(|(k, v)| (k, v.itemize_for_element_store()))
                .collect();
            Value::hash_with_data(Value::hash_arc(map))
        }
    }

    /// Wrap a decoded JSON array: mutable `Array` by default, `List` with
    /// `:immutable`.
    fn finish_array(&self, items: Vec<Value>) -> Value {
        if self.immutable {
            Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(items)),
                crate::value::ArrayKind::List,
            )
        } else {
            // ADR-0040 slice 2: see `finish_object` -- a decoded JSON array is
            // a real `Array`, so its aggregate elements itemize. (`:immutable`
            // decodes to a `List`, whose elements are not containers.)
            crate::runtime::utils::itemize_real_array_elements(Value::real_array(items))
        }
    }

    fn skip_ws(&mut self) {
        while self.pos < self.bytes.len() {
            match self.bytes[self.pos] {
                b' ' | b'\t' | b'\n' | b'\r' => self.pos += 1,
                b'/' if self.allow_jsonc => {
                    // JSONC comments: `// ...` to end of line, `/* ... */`
                    // (non-overlapping: `/*/` is NOT a complete comment). An
                    // invalid or unterminated comment leaves pos at the `/` so
                    // the value parser reports it as an unexpected character.
                    if self.bytes.get(self.pos + 1) == Some(&b'/') {
                        self.pos += 2;
                        while self.pos < self.bytes.len() && self.bytes[self.pos] != b'\n' {
                            self.pos += 1;
                        }
                    } else if self.bytes.get(self.pos + 1) == Some(&b'*') {
                        match self.chars[self.pos + 2..].find("*/") {
                            Some(end) => self.pos += 2 + end + 2,
                            None => break,
                        }
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
    }

    fn peek(&self) -> Option<u8> {
        self.bytes.get(self.pos).copied()
    }

    fn parse_value(&mut self, depth: usize) -> Result<Value, String> {
        self.skip_ws();
        match self.peek() {
            Some(b'{') => self.parse_object(depth),
            Some(b'[') => self.parse_array(depth),
            Some(b'"') => Ok(Value::str(self.parse_string()?)),
            Some(b't') => self.parse_literal("true", Value::TRUE),
            Some(b'f') => self.parse_literal("false", Value::FALSE),
            Some(b'n') => self.parse_literal("null", Value::package(crate::symbol::wk::any())),
            Some(c) if c == b'-' || c.is_ascii_digit() => self.parse_number(),
            Some(c) => Err(format!("Unexpected character in JSON: {}", c as char)),
            None => Err("Unexpected end of JSON".to_string()),
        }
    }

    fn parse_literal(&mut self, word: &str, val: Value) -> Result<Value, String> {
        if self.chars[self.pos..].starts_with(word) {
            self.pos += word.len();
            Ok(val)
        } else {
            Err(format!("Invalid JSON literal, expected '{word}'"))
        }
    }

    fn parse_object(&mut self, depth: usize) -> Result<Value, String> {
        if depth >= MAX_JSON_DEPTH {
            return Err(format!("JSON nesting exceeds {MAX_JSON_DEPTH} levels"));
        }
        self.pos += 1; // consume '{'
        let mut map = ValueMap::default();
        self.skip_ws();
        if self.peek() == Some(b'}') {
            self.pos += 1;
            return Ok(self.finish_object(map));
        }
        loop {
            self.skip_ws();
            if self.peek() != Some(b'"') {
                return Err("Expected string key in JSON object".to_string());
            }
            let key = self.parse_string()?;
            self.skip_ws();
            if self.peek() != Some(b':') {
                return Err("Expected ':' in JSON object".to_string());
            }
            self.pos += 1;
            let val = self.parse_value(depth + 1)?;
            map.insert(key, val);
            self.skip_ws();
            match self.peek() {
                Some(b',') => {
                    self.pos += 1;
                }
                Some(b'}') => {
                    self.pos += 1;
                    return Ok(self.finish_object(map));
                }
                _ => return Err("Expected ',' or '}' in JSON object".to_string()),
            }
        }
    }

    fn parse_array(&mut self, depth: usize) -> Result<Value, String> {
        if depth >= MAX_JSON_DEPTH {
            return Err(format!("JSON nesting exceeds {MAX_JSON_DEPTH} levels"));
        }
        self.pos += 1; // consume '['
        let mut items = Vec::new();
        self.skip_ws();
        if self.peek() == Some(b']') {
            self.pos += 1;
            return Ok(self.finish_array(items));
        }
        loop {
            let val = self.parse_value(depth + 1)?;
            items.push(val);
            self.skip_ws();
            match self.peek() {
                Some(b',') => {
                    self.pos += 1;
                }
                Some(b']') => {
                    self.pos += 1;
                    return Ok(self.finish_array(items));
                }
                _ => return Err("Expected ',' or ']' in JSON array".to_string()),
            }
        }
    }

    fn parse_string(&mut self) -> Result<String, String> {
        self.pos += 1; // consume opening '"'
        let mut result = String::new();
        while self.pos < self.bytes.len() {
            let c = self.bytes[self.pos];
            match c {
                b'"' => {
                    self.pos += 1;
                    return Ok(result);
                }
                b'\\' => {
                    self.pos += 1;
                    let esc = self.peek().ok_or("Unterminated escape in JSON string")?;
                    match esc {
                        b'"' => result.push('"'),
                        b'\\' => result.push('\\'),
                        b'/' => result.push('/'),
                        b'b' => result.push('\u{0008}'),
                        b'f' => result.push('\u{000c}'),
                        b'n' => result.push('\n'),
                        b'r' => result.push('\r'),
                        b't' => result.push('\t'),
                        b'u' => {
                            let cp = self.parse_unicode_escape()?;
                            // A high surrogate must be followed by an escaped low
                            // surrogate (JSON transports astral chars as pairs);
                            // anything else — including a lone low surrogate — is
                            // malformed (JSON::Fast rejects it too).
                            if (0xD800..=0xDBFF).contains(&cp) {
                                if !self.chars[self.pos..].starts_with("\\u") {
                                    return Err(
                                        "Lone surrogate \\u escape in JSON string".to_string()
                                    );
                                }
                                self.pos += 1; // consume '\\'; parse_unicode_escape eats the 'u'
                                let lo = self.parse_unicode_escape()?;
                                if !(0xDC00..=0xDFFF).contains(&lo) {
                                    return Err("Invalid surrogate pair in JSON string".to_string());
                                }
                                let combined = 0x10000 + ((cp - 0xD800) << 10) + (lo - 0xDC00);
                                match char::from_u32(combined) {
                                    Some(ch) => result.push(ch),
                                    None => {
                                        return Err(
                                            "Invalid surrogate pair in JSON string".to_string()
                                        );
                                    }
                                }
                            } else if (0xDC00..=0xDFFF).contains(&cp) {
                                return Err("Lone surrogate \\u escape in JSON string".to_string());
                            } else if let Some(ch) = char::from_u32(cp) {
                                result.push(ch);
                            }
                            continue;
                        }
                        _ => {
                            return Err(format!(
                                "Invalid backslash escape in JSON string at position {}",
                                self.pos
                            ));
                        }
                    }
                    self.pos += 1;
                }
                c if c < 0x20 => {
                    // Raw control characters (tab, newline, ...) must be escaped
                    // inside JSON strings.
                    return Err(format!(
                        "Unescaped control character in JSON string at position {}",
                        self.pos
                    ));
                }
                _ => {
                    // Copy the full UTF-8 character.
                    let ch_str = &self.chars[self.pos..];
                    let ch = ch_str
                        .chars()
                        .next()
                        .ok_or("Invalid UTF-8 in JSON string")?;
                    result.push(ch);
                    self.pos += ch.len_utf8();
                }
            }
        }
        Err("Unterminated JSON string".to_string())
    }

    /// Parse exactly 4 hex digits (the `\u` already consumed) and advance past them.
    fn parse_unicode_escape(&mut self) -> Result<u32, String> {
        self.pos += 1; // consume 'u'
        if self.pos + 4 > self.bytes.len() {
            return Err("Truncated \\u escape in JSON string".to_string());
        }
        // Work on bytes: slicing the &str could split a multi-byte character.
        let mut cp = 0u32;
        for &b in &self.bytes[self.pos..self.pos + 4] {
            let digit = (b as char)
                .to_digit(16)
                .ok_or_else(|| "Invalid \\u escape in JSON string".to_string())?;
            cp = cp * 16 + digit;
        }
        self.pos += 4;
        Ok(cp)
    }

    fn parse_number(&mut self) -> Result<Value, String> {
        let start = self.pos;
        if self.peek() == Some(b'-') {
            self.pos += 1;
        }
        while matches!(self.peek(), Some(c) if c.is_ascii_digit()) {
            self.pos += 1;
        }
        let mut is_rat = false;
        let mut is_num = false;
        if self.peek() == Some(b'.') {
            is_rat = true;
            self.pos += 1;
            // JSON's number grammar requires at least one digit after the
            // decimal point: `1.` / `2.e3` are malformed.
            if !matches!(self.peek(), Some(c) if c.is_ascii_digit()) {
                return Err("Missing digits after decimal point in JSON number".to_string());
            }
            while matches!(self.peek(), Some(c) if c.is_ascii_digit()) {
                self.pos += 1;
            }
        }
        if matches!(self.peek(), Some(b'e') | Some(b'E')) {
            is_num = true;
            self.pos += 1;
            if matches!(self.peek(), Some(b'+') | Some(b'-')) {
                self.pos += 1;
            }
            while matches!(self.peek(), Some(c) if c.is_ascii_digit()) {
                self.pos += 1;
            }
        }
        let num_str = &self.chars[start..self.pos];
        if is_num {
            // Exponential form -> Num.
            num_str
                .parse::<f64>()
                .map(Value::num)
                .map_err(|_| format!("Invalid JSON number: {num_str}"))
        } else if is_rat {
            Ok(decimal_to_rat(num_str))
        } else {
            // Plain integer -> Int (fall back to Num if it overflows i64).
            match num_str.parse::<i64>() {
                Ok(i) => Ok(Value::int(i)),
                Err(_) => num_str
                    .parse::<f64>()
                    .map(Value::num)
                    .map_err(|_| format!("Invalid JSON number: {num_str}")),
            }
        }
    }
}

/// Convert a decimal literal (e.g. "2.5", "-0.75") into a reduced `Rat`.
fn decimal_to_rat(s: &str) -> Value {
    let negative = s.starts_with('-');
    let abs = s.trim_start_matches('-');
    if let Some((int_part, frac_part)) = abs.split_once('.') {
        let frac_digits = frac_part.len() as u32;
        if let Some(denom) = 10i64.checked_pow(frac_digits) {
            let int_val = int_part.parse::<i64>().unwrap_or(0);
            let frac_val = if frac_part.is_empty() {
                0
            } else {
                frac_part.parse::<i64>().unwrap_or(0)
            };
            if let Some(scaled) = int_val.checked_mul(denom) {
                let mut numer = scaled + frac_val;
                if negative {
                    numer = -numer;
                }
                return make_rat(numer, denom);
            }
        }
    }
    // Fallback for overflow / unexpected shape.
    Value::num(s.parse::<f64>().unwrap_or(0.0))
}
