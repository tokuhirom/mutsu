//! Free-function helpers shared across the `methods_distribution*` modules: a
//! dependency-free JSON parser/serializer plus small distribution utilities
//! (identity tuple, platform library naming, string hashing). Kept `pub(super)`
//! so the sibling `impl Interpreter` modules can call them.

use crate::value::Value;
use std::collections::HashMap;

// ---- Simple JSON parser (no serde_json dependency) ----

pub(super) fn parse_json_value(s: &str) -> Result<(Value, &str), String> {
    let s = s.trim_start();
    if s.is_empty() {
        return Err("Unexpected end of JSON".to_string());
    }
    match s.as_bytes()[0] {
        b'{' => parse_json_object(&s[1..]),
        b'[' => parse_json_array(&s[1..]),
        b'"' => parse_json_string(&s[1..]),
        b't' if s.starts_with("true") => Ok((Value::Bool(true), &s[4..])),
        b'f' if s.starts_with("false") => Ok((Value::Bool(false), &s[5..])),
        b'n' if s.starts_with("null") => Ok((Value::Nil, &s[4..])),
        b'-' | b'0'..=b'9' => parse_json_number(s),
        ch => Err(format!("Unexpected character in JSON: {}", ch as char)),
    }
}

pub(super) fn parse_json_object(s: &str) -> Result<(Value, &str), String> {
    let mut map = HashMap::new();
    let mut s = s.trim_start();
    if let Some(rest) = s.strip_prefix('}') {
        return Ok((Value::Hash(Value::hash_arc(map)), rest));
    }
    loop {
        let s2 = s.trim_start();
        if !s2.starts_with('"') {
            return Err("Expected string key in JSON object".to_string());
        }
        let (key_val, rest) = parse_json_string(&s2[1..])?;
        let key = key_val.to_string_value();
        let rest = rest.trim_start();
        if !rest.starts_with(':') {
            return Err("Expected ':' in JSON object".to_string());
        }
        let (val, rest) = parse_json_value(&rest[1..])?;
        map.insert(key, val);
        let rest = rest.trim_start();
        if let Some(after) = rest.strip_prefix('}') {
            return Ok((Value::Hash(Value::hash_arc(map)), after));
        }
        if let Some(after) = rest.strip_prefix(',') {
            s = after;
        } else {
            return Err("Expected ',' or '}' in JSON object".to_string());
        }
    }
}

pub(super) fn parse_json_array(s: &str) -> Result<(Value, &str), String> {
    let mut items = Vec::new();
    let mut s = s.trim_start();
    if let Some(rest) = s.strip_prefix(']') {
        return Ok((Value::array(items), rest));
    }
    loop {
        let (val, rest) = parse_json_value(s)?;
        items.push(val);
        let rest = rest.trim_start();
        if let Some(after) = rest.strip_prefix(']') {
            return Ok((Value::array(items), after));
        }
        if let Some(after) = rest.strip_prefix(',') {
            s = after.trim_start();
        } else {
            return Err("Expected ',' or ']' in JSON array".to_string());
        }
    }
}

pub(super) fn parse_json_string(s: &str) -> Result<(Value, &str), String> {
    let mut result = String::new();
    let mut chars = s.chars();
    loop {
        match chars.next() {
            None => return Err("Unterminated JSON string".to_string()),
            Some('"') => {
                return Ok((Value::str(result), chars.as_str()));
            }
            Some('\\') => match chars.next() {
                Some('"') => result.push('"'),
                Some('\\') => result.push('\\'),
                Some('/') => result.push('/'),
                Some('n') => result.push('\n'),
                Some('r') => result.push('\r'),
                Some('t') => result.push('\t'),
                Some('u') => {
                    let hex: String = chars.by_ref().take(4).collect();
                    if let Ok(cp) = u32::from_str_radix(&hex, 16)
                        && let Some(ch) = char::from_u32(cp)
                    {
                        result.push(ch);
                    }
                }
                Some(c) => {
                    result.push('\\');
                    result.push(c);
                }
                None => return Err("Unterminated escape in JSON string".to_string()),
            },
            Some(c) => result.push(c),
        }
    }
}

pub(super) fn parse_json_number(s: &str) -> Result<(Value, &str), String> {
    let mut end = 0;
    let bytes = s.as_bytes();
    if end < bytes.len() && bytes[end] == b'-' {
        end += 1;
    }
    while end < bytes.len() && bytes[end].is_ascii_digit() {
        end += 1;
    }
    let mut is_float = false;
    if end < bytes.len() && bytes[end] == b'.' {
        is_float = true;
        end += 1;
        while end < bytes.len() && bytes[end].is_ascii_digit() {
            end += 1;
        }
    }
    if end < bytes.len() && (bytes[end] == b'e' || bytes[end] == b'E') {
        is_float = true;
        end += 1;
        if end < bytes.len() && (bytes[end] == b'+' || bytes[end] == b'-') {
            end += 1;
        }
        while end < bytes.len() && bytes[end].is_ascii_digit() {
            end += 1;
        }
    }
    let num_str = &s[..end];
    if is_float {
        let f: f64 = num_str
            .parse()
            .map_err(|_| format!("Invalid JSON number: {num_str}"))?;
        Ok((Value::Num(f), &s[end..]))
    } else {
        let i: i64 = num_str
            .parse()
            .map_err(|_| format!("Invalid JSON number: {num_str}"))?;
        Ok((Value::Int(i), &s[end..]))
    }
}

pub(super) fn value_to_json_string(val: &Value) -> String {
    match val {
        Value::Hash(map) => {
            let parts: Vec<String> = map
                .iter()
                .map(|(k, v)| format!("  {:?}: {}", k, value_to_json_string(v)))
                .collect();
            format!("{{\n{}\n}}", parts.join(",\n"))
        }
        Value::Array(arr, _) => {
            let parts: Vec<String> = arr.iter().map(value_to_json_string).collect();
            format!("[{}]", parts.join(", "))
        }
        Value::Str(s) => format!("{:?}", s.to_string()),
        Value::Int(i) => format!("{i}"),
        Value::Bool(b) => format!("{b}"),
        Value::Nil => "null".to_string(),
        _ => format!("{:?}", val.to_string_value()),
    }
}

/// Identity tuple of a distribution, used to detect duplicate installs.
/// Two distributions are "the same" when name/version/auth/api all match.
pub(super) fn dist_identity(meta: &Value) -> (String, String, String, String) {
    let field = |key: &str, alt: Option<&str>| {
        meta.hash_get_str(key)
            .or_else(|| alt.and_then(|a| meta.hash_get_str(a)))
            .map(|v| v.to_string_value())
            .unwrap_or_default()
    };
    (
        field("name", None),
        field("ver", Some("version")),
        field("auth", None),
        field("api", None),
    )
}

pub(super) fn platform_library_name(name: &str) -> String {
    if cfg!(target_os = "macos") {
        format!("lib{name}.dylib")
    } else if cfg!(target_os = "windows") {
        format!("{name}.dll")
    } else {
        format!("lib{name}.so")
    }
}

pub(super) fn hash_strings(strings: &[&str]) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    for s in strings {
        s.hash(&mut hasher);
    }
    hasher.finish()
}
