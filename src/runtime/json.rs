//! Native `to-json` / `from-json` (JSON::Fast / JSON::Tiny compatible).
//!
//! These are NOT Raku core builtins — they are provided by the `JSON::Fast` /
//! `JSON::Tiny` modules. The real `JSON::Fast` depends on ~50 `nqp::` ops mutsu
//! does not implement, so mutsu ships native Rust implementations gated behind
//! `use JSON::Fast` / `use JSON::Tiny`, mirroring how the `Test` functions are
//! gated on `use Test` (see `vm_native_test.rs`).
//!
//! Encoding follows JSON::Fast 0.19 semantics: `:pretty` defaults to True with a
//! 2-space indent, type objects / undefined values render as `null`, `Rat`s gain
//! a trailing `.0` when integral, and `Num`s gain a trailing `e0` when they carry
//! no exponent. Decoding maps `true`/`false` to `Bool`, `null` to the `Any` type
//! object, integers to `Int`, decimals to `Rat`, and exponential forms to `Num`.

use crate::value::{Value, make_rat};
use std::collections::HashMap;

/// Options controlling `to-json` rendering. Mirrors the JSON::Fast named params.
pub(crate) struct ToJsonOpts {
    pub pretty: bool,
    pub sorted_keys: bool,
    pub spacing: usize,
}

impl Default for ToJsonOpts {
    fn default() -> Self {
        ToJsonOpts {
            pretty: true,
            sorted_keys: false,
            spacing: 2,
        }
    }
}

/// Serialize a `Value` to a JSON string.
pub(crate) fn to_json(val: &Value, opts: &ToJsonOpts) -> String {
    let mut out = String::new();
    jsonify(val, opts, 0, &mut out);
    out
}

fn indent(out: &mut String, opts: &ToJsonOpts, level: usize) {
    if opts.pretty {
        for _ in 0..(level * opts.spacing) {
            out.push(' ');
        }
    }
}

fn jsonify(val: &Value, opts: &ToJsonOpts, level: usize, out: &mut String) {
    match val {
        Value::Bool(b) => out.push_str(if *b { "true" } else { "false" }),
        Value::Int(_) | Value::BigInt(_) => out.push_str(&val.to_string_value()),
        Value::Rat(..) | Value::FatRat(..) | Value::BigRat(..) => {
            // JSON::Fast: emit the Rat string, appending ".0" when it has no
            // decimal point so the value reads back as a Rat, not an Int.
            let s = val.to_string_value();
            out.push_str(&s);
            if !s.contains('.') {
                out.push_str(".0");
            }
        }
        Value::Num(f) => {
            if f.is_nan() || f.is_infinite() {
                // JSON has no NaN/Inf; JSON::Fast emits `null` unless the dynamic
                // var $*JSON_NAN_INF_SUPPORT is set (which mutsu does not model).
                out.push_str("null");
            } else {
                let s = val.to_string_value();
                out.push_str(&s);
                if !s.contains('e') && !s.contains('E') {
                    out.push_str("e0");
                }
            }
        }
        Value::Str(s) => {
            out.push('"');
            escape_str(s, out);
            out.push('"');
        }
        Value::Scalar(inner) => jsonify(inner, opts, level, out),
        Value::Mixin(inner, _) => jsonify(inner, opts, level, out),
        Value::Array(arr, _) => jsonify_seq(&arr.items, opts, level, out),
        Value::Seq(items) | Value::Slip(items) | Value::HyperSeq(items) | Value::RaceSeq(items) => {
            jsonify_seq(items, opts, level, out)
        }
        Value::Hash(h) => {
            let entries: Vec<(&String, &Value)> = h.map.iter().collect();
            jsonify_object(entries, opts, level, out);
        }
        Value::Pair(k, v) => {
            jsonify_object(vec![(k, v.as_ref())], opts, level, out);
        }
        Value::ValuePair(k, v) => {
            let key = k.to_string_value();
            jsonify_object(vec![(&key, v.as_ref())], opts, level, out);
        }
        // Type objects / undefined values render as JSON null.
        Value::Nil | Value::Package(_) | Value::Whatever | Value::HyperWhatever => {
            out.push_str("null");
        }
        // Anything else: fall back to a quoted stringification. Raku would die on
        // an unjsonifiable object; being lenient keeps web/template use working.
        other => {
            out.push('"');
            escape_str(&other.to_string_value(), out);
            out.push('"');
        }
    }
}

fn jsonify_seq(items: &[Value], opts: &ToJsonOpts, level: usize, out: &mut String) {
    if items.is_empty() {
        // JSON::Fast prints an empty array as "[\n]" in pretty mode.
        if opts.pretty {
            out.push_str("[\n");
            indent(out, opts, level);
            out.push(']');
        } else {
            out.push_str("[]");
        }
        return;
    }
    out.push('[');
    if opts.pretty {
        out.push('\n');
    }
    for (i, item) in items.iter().enumerate() {
        if i > 0 {
            out.push(',');
            if opts.pretty {
                out.push('\n');
            }
        }
        indent(out, opts, level + 1);
        jsonify(item, opts, level + 1, out);
    }
    if opts.pretty {
        out.push('\n');
        indent(out, opts, level);
    }
    out.push(']');
}

fn jsonify_object(
    entries: Vec<(&String, &Value)>,
    opts: &ToJsonOpts,
    level: usize,
    out: &mut String,
) {
    if entries.is_empty() {
        if opts.pretty {
            out.push_str("{\n");
            indent(out, opts, level);
            out.push('}');
        } else {
            out.push_str("{}");
        }
        return;
    }
    let mut entries = entries;
    if opts.sorted_keys {
        entries.sort_by(|a, b| a.0.cmp(b.0));
    }
    out.push('{');
    if opts.pretty {
        out.push('\n');
    }
    for (i, (k, v)) in entries.iter().enumerate() {
        if i > 0 {
            out.push(',');
            if opts.pretty {
                out.push('\n');
            }
        }
        indent(out, opts, level + 1);
        out.push('"');
        escape_str(k, out);
        out.push_str("\":");
        if opts.pretty {
            out.push(' ');
        }
        jsonify(v, opts, level + 1, out);
    }
    if opts.pretty {
        out.push('\n');
        indent(out, opts, level);
    }
    out.push('}');
}

/// Escape a string per JSON::Fast's `str-escape`: `\n`/`\r`/`\t` and `"`/`\\`
/// get short escapes, other control chars (< 0x20) become `\u00xx`, code points
/// above the BMP become surrogate pairs, everything else passes through literally.
fn escape_str(s: &str, out: &mut String) {
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 => {
                out.push_str(&format!("\\u{:04x}", c as u32));
            }
            c if (c as u32) >= 0x10000 => {
                let cp = c as u32 - 0x10000;
                let hi = 0xD800 + (cp >> 10);
                let lo = 0xDC00 + (cp & 0x3FF);
                out.push_str(&format!("\\u{:04x}\\u{:04x}", hi, lo));
            }
            c => out.push(c),
        }
    }
}

/// Parse a JSON string into a `Value`. Returns an error message on malformed input.
pub(crate) fn from_json(text: &str) -> Result<Value, String> {
    let mut p = Parser {
        bytes: text.as_bytes(),
        chars: text,
        pos: 0,
    };
    p.skip_ws();
    let value = p.parse_value()?;
    p.skip_ws();
    if p.pos != p.bytes.len() {
        return Err("JSON Input contained additional text after the document".to_string());
    }
    Ok(value)
}

struct Parser<'a> {
    bytes: &'a [u8],
    chars: &'a str,
    pos: usize,
}

impl<'a> Parser<'a> {
    fn skip_ws(&mut self) {
        while self.pos < self.bytes.len() {
            match self.bytes[self.pos] {
                b' ' | b'\t' | b'\n' | b'\r' => self.pos += 1,
                _ => break,
            }
        }
    }

    fn peek(&self) -> Option<u8> {
        self.bytes.get(self.pos).copied()
    }

    fn parse_value(&mut self) -> Result<Value, String> {
        self.skip_ws();
        match self.peek() {
            Some(b'{') => self.parse_object(),
            Some(b'[') => self.parse_array(),
            Some(b'"') => Ok(Value::str(self.parse_string()?)),
            Some(b't') => self.parse_literal("true", Value::Bool(true)),
            Some(b'f') => self.parse_literal("false", Value::Bool(false)),
            Some(b'n') => {
                self.parse_literal("null", Value::Package(crate::symbol::Symbol::intern("Any")))
            }
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

    fn parse_object(&mut self) -> Result<Value, String> {
        self.pos += 1; // consume '{'
        let mut map = HashMap::new();
        self.skip_ws();
        if self.peek() == Some(b'}') {
            self.pos += 1;
            return Ok(Value::Hash(Value::hash_arc(map)));
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
            let val = self.parse_value()?;
            map.insert(key, val);
            self.skip_ws();
            match self.peek() {
                Some(b',') => {
                    self.pos += 1;
                }
                Some(b'}') => {
                    self.pos += 1;
                    return Ok(Value::Hash(Value::hash_arc(map)));
                }
                _ => return Err("Expected ',' or '}' in JSON object".to_string()),
            }
        }
    }

    fn parse_array(&mut self) -> Result<Value, String> {
        self.pos += 1; // consume '['
        let mut items = Vec::new();
        self.skip_ws();
        if self.peek() == Some(b']') {
            self.pos += 1;
            return Ok(Value::real_array(items));
        }
        loop {
            let val = self.parse_value()?;
            items.push(val);
            self.skip_ws();
            match self.peek() {
                Some(b',') => {
                    self.pos += 1;
                }
                Some(b']') => {
                    self.pos += 1;
                    return Ok(Value::real_array(items));
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
                            // Handle surrogate pairs.
                            if (0xD800..=0xDBFF).contains(&cp) {
                                if self.chars[self.pos..].starts_with("\\u") {
                                    self.pos += 2;
                                    let lo = self.parse_unicode_escape()?;
                                    let combined = 0x10000 + ((cp - 0xD800) << 10) + (lo - 0xDC00);
                                    if let Some(ch) = char::from_u32(combined) {
                                        result.push(ch);
                                    }
                                } else if let Some(ch) = char::from_u32(cp) {
                                    result.push(ch);
                                }
                                continue;
                            } else if let Some(ch) = char::from_u32(cp) {
                                result.push(ch);
                            }
                            continue;
                        }
                        other => {
                            result.push('\\');
                            result.push(other as char);
                        }
                    }
                    self.pos += 1;
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
        let hex = &self.chars[self.pos..self.pos + 4];
        let cp = u32::from_str_radix(hex, 16)
            .map_err(|_| "Invalid \\u escape in JSON string".to_string())?;
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
                .map(Value::Num)
                .map_err(|_| format!("Invalid JSON number: {num_str}"))
        } else if is_rat {
            Ok(decimal_to_rat(num_str))
        } else {
            // Plain integer -> Int (fall back to Num if it overflows i64).
            match num_str.parse::<i64>() {
                Ok(i) => Ok(Value::Int(i)),
                Err(_) => num_str
                    .parse::<f64>()
                    .map(Value::Num)
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
    Value::Num(s.parse::<f64>().unwrap_or(0.0))
}
