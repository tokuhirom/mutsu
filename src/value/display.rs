use super::*;

/// Apply tclc (titlecase first char, lowercase rest) to a string.
pub fn tclc_str(s: &str) -> String {
    let mut result = String::new();
    let mut first = true;
    for ch in s.chars() {
        if first {
            result.push_str(&crate::builtins::unicode_titlecase_first(ch));
            first = false;
        } else {
            for c in ch.to_lowercase() {
                result.push(c);
            }
        }
    }
    result
}

/// Apply wordcase to a string: find words matching <ident>+ % <[ - ' ]>
/// and apply tclc to each word. Non-word characters pass through unchanged.
pub fn wordcase_str(s: &str) -> String {
    let chars: Vec<char> = s.chars().collect();
    let len = chars.len();
    let mut result = String::new();
    let mut i = 0;

    while i < len {
        if chars[i].is_alphabetic() || chars[i] == '_' {
            let word_start = i;
            // Consume first ident: [alpha|_] \w*
            i += 1;
            while i < len && (chars[i].is_alphanumeric() || chars[i] == '_') {
                i += 1;
            }
            // Try to consume more idents separated by - or '
            loop {
                if i < len
                    && (chars[i] == '-' || chars[i] == '\'')
                    && i + 1 < len
                    && (chars[i + 1].is_alphabetic() || chars[i + 1] == '_')
                {
                    i += 1; // consume separator
                    i += 1; // consume first char of next ident
                    while i < len && (chars[i].is_alphanumeric() || chars[i] == '_') {
                        i += 1;
                    }
                } else {
                    break;
                }
            }
            let word: String = chars[word_start..i].iter().collect();
            result.push_str(&tclc_str(&word));
        } else {
            result.push(chars[i]);
            i += 1;
        }
    }
    result
}

pub fn format_complex(r: f64, i: f64) -> String {
    fn fmt_num(v: f64) -> String {
        if v.fract() == 0.0 && v.is_finite() {
            format!("{}", v as i64)
        } else {
            format!("{}", v)
        }
    }
    if i == 0.0 {
        format!("{}+0i", fmt_num(r))
    } else if i < 0.0 {
        format!("{}{}i", fmt_num(r), fmt_num(i))
    } else {
        format!("{}+{}i", fmt_num(r), fmt_num(i))
    }
}

impl Value {
    pub(crate) fn to_string_value(&self) -> String {
        match self {
            Value::Int(i) => i.to_string(),
            Value::BigInt(n) => n.to_string(),
            Value::Num(f) => {
                if f.is_nan() {
                    "NaN".to_string()
                } else if f.is_infinite() {
                    if *f > 0.0 {
                        "Inf".to_string()
                    } else {
                        "-Inf".to_string()
                    }
                } else if *f == 0.0 && f.is_sign_negative() {
                    "-0".to_string()
                } else if f.fract() == 0.0 && f.is_finite() {
                    format!("{}", *f as i64)
                } else {
                    format!("{}", f)
                }
            }
            Value::Str(s) => s.clone(),
            Value::Bool(true) => "True".to_string(),
            Value::Bool(false) => "False".to_string(),
            Value::Range(a, b) => format!("{}..{}", a, b),
            Value::RangeExcl(a, b) => format!("{}..^{}", a, b),
            Value::RangeExclStart(a, b) => format!("^{}..{}", a, b),
            Value::RangeExclBoth(a, b) => format!("^{}..^{}", a, b),
            Value::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                // String ranges expand to space-separated elements
                if let (Value::Str(a), Value::Str(b)) = (start.as_ref(), end.as_ref())
                    && a.len() == 1
                    && b.len() == 1
                {
                    let s = a.chars().next().unwrap() as u32;
                    let e = b.chars().next().unwrap() as u32;
                    let s = if *excl_start { s + 1 } else { s };
                    let items: Vec<String> = if *excl_end {
                        (s..e)
                            .filter_map(char::from_u32)
                            .map(|c| c.to_string())
                            .collect()
                    } else {
                        (s..=e)
                            .filter_map(char::from_u32)
                            .map(|c| c.to_string())
                            .collect()
                    };
                    return items.join(" ");
                }
                let prefix = if *excl_start { "^" } else { "" };
                let sep = if *excl_end { "..^" } else { ".." };
                format!(
                    "{}{}{}{}",
                    prefix,
                    start.to_string_value(),
                    sep,
                    end.to_string_value()
                )
            }
            Value::Array(items) => items
                .iter()
                .map(|v| v.to_string_value())
                .collect::<Vec<_>>()
                .join(" "),
            Value::LazyList(_) => "LazyList".to_string(),
            Value::Hash(items) => items
                .iter()
                .map(|(k, v)| format!("{}\t{}", k, v.to_string_value()))
                .collect::<Vec<_>>()
                .join("\n"),
            Value::Rat(n, d) => {
                if *d == 0 {
                    if *n == 0 {
                        "NaN".to_string()
                    } else if *n > 0 {
                        "Inf".to_string()
                    } else {
                        "-Inf".to_string()
                    }
                } else if *n % *d == 0 {
                    // Exact integer: Rat(10, 2) => "5"
                    format!("{}", *n / *d)
                } else {
                    let whole = *n as f64 / *d as f64;
                    // Check if it can be represented as exact decimal
                    let mut dd = *d;
                    while dd % 2 == 0 {
                        dd /= 2;
                    }
                    while dd % 5 == 0 {
                        dd /= 5;
                    }
                    if dd == 1 {
                        // Exact decimal representation
                        let s = format!("{}", whole);
                        if s.contains('.') {
                            s
                        } else {
                            format!("{}.0", whole)
                        }
                    } else {
                        format!("{:.6}", whole)
                    }
                }
            }
            Value::FatRat(a, b) => {
                if *b == 0 {
                    if *a == 0 {
                        "NaN".to_string()
                    } else if *a > 0 {
                        "Inf".to_string()
                    } else {
                        "-Inf".to_string()
                    }
                } else if *a % *b == 0 {
                    format!("{}", *a / *b)
                } else {
                    let whole = *a as f64 / *b as f64;
                    let mut dd = *b;
                    while dd % 2 == 0 {
                        dd /= 2;
                    }
                    while dd % 5 == 0 {
                        dd /= 5;
                    }
                    if dd == 1 {
                        let s = format!("{}", whole);
                        if s.contains('.') {
                            s
                        } else {
                            format!("{}.0", whole)
                        }
                    } else {
                        format!("{:.6}", whole)
                    }
                }
            }
            Value::Complex(r, i) => format_complex(*r, *i),
            Value::Set(s) => {
                let mut keys: Vec<&String> = s.iter().collect();
                keys.sort();
                format!(
                    "set({})",
                    keys.iter()
                        .map(|k| k.as_str())
                        .collect::<Vec<_>>()
                        .join(" ")
                )
            }
            Value::Bag(b) => {
                let mut keys: Vec<(&String, &i64)> = b.iter().collect();
                keys.sort_by_key(|(k, _)| (*k).clone());
                format!(
                    "bag({})",
                    keys.iter()
                        .map(|(k, v)| format!("{}({})", k, v))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Value::Mix(m) => {
                let mut keys: Vec<(&String, &f64)> = m.iter().collect();
                keys.sort_by_key(|(k, _)| (*k).clone());
                format!(
                    "mix({})",
                    keys.iter()
                        .map(|(k, v)| format!("{}({})", k, v))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Value::Pair(k, v) => format!("{}\t{}", k, v.to_string_value()),
            Value::Enum { key, .. } => key.clone(),
            Value::CompUnitDepSpec { short_name } => {
                format!("CompUnit::DependencySpecification({})", short_name)
            }
            Value::Package(s) => format!("({})", s),
            Value::Routine { package, name } => format!("{}::{}", package, name),
            Value::Sub(data) => data.name.clone(),
            Value::WeakSub(weak) => match weak.upgrade() {
                Some(data) => data.name.clone(),
                None => String::new(),
            },
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "IO::Path" => attributes
                .get("path")
                .map(|v: &Value| v.to_string_value())
                .unwrap_or_else(|| format!("{}()", class_name)),
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Exception" => attributes
                .get("message")
                .map(|v: &Value| v.to_string_value())
                .unwrap_or_else(|| format!("{}()", class_name)),
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "ObjAt" => attributes
                .get("WHICH")
                .map(|v: &Value| v.to_string_value())
                .unwrap_or_else(|| format!("{}()", class_name)),
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Match" => attributes
                .get("str")
                .map(|v: &Value| v.to_string_value())
                .unwrap_or_default(),
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Proc" => attributes
                .get("exitcode")
                .map(|v: &Value| v.to_string_value())
                .unwrap_or_else(|| format!("{}()", class_name)),
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Buf" || class_name == "Blob" => {
                if let Some(Value::Array(bytes)) = attributes.get("bytes") {
                    if bytes.is_empty() {
                        format!("{}()", class_name)
                    } else {
                        let hex: Vec<String> = bytes
                            .iter()
                            .map(|b| match b {
                                Value::Int(i) => format!("{:02X}", *i as u8),
                                _ => "00".to_string(),
                            })
                            .collect();
                        format!("{}:0x<{}>", class_name, hex.join(" "))
                    }
                } else {
                    format!("{}()", class_name)
                }
            }
            Value::Instance { class_name, .. } => format!("{}()", class_name),
            Value::Junction { kind, values } => {
                let kind_str = match kind {
                    JunctionKind::Any => "any",
                    JunctionKind::All => "all",
                    JunctionKind::One => "one",
                    JunctionKind::None => "none",
                };
                let elems = values
                    .iter()
                    .map(|v| v.to_string_value())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", kind_str, elems)
            }
            Value::Regex(pattern) => format!("/{}/", pattern),
            Value::Version { parts, plus, minus } => {
                let s = Self::version_parts_to_string(parts);
                if *plus {
                    format!("{}+", s)
                } else if *minus {
                    format!("{}-", s)
                } else {
                    s
                }
            }
            Value::Slip(items) => items
                .iter()
                .map(|v| v.to_string_value())
                .collect::<Vec<_>>()
                .join(" "),
            Value::Promise(p) => format!("Promise({})", p.status()),
            Value::Channel(_) => "Channel".to_string(),
            Value::Nil => String::new(),
            Value::HyperWhatever => "**".to_string(),
            Value::Capture { positional, named } => {
                let mut parts = Vec::new();
                for v in positional {
                    parts.push(v.to_string_value());
                }
                for (k, v) in named {
                    if let Value::Bool(true) = v {
                        parts.push(format!(":{}", k));
                    } else if let Value::Bool(false) = v {
                        parts.push(format!(":!{}", k));
                    } else {
                        parts.push(format!(":{}({})", k, v.to_string_value()));
                    }
                }
                format!("\\({})", parts.join(", "))
            }
            Value::Mixin(inner, mixins) => {
                if let Some(str_val) = mixins.get("Str") {
                    str_val.to_string_value()
                } else {
                    inner.to_string_value()
                }
            }
        }
    }

    pub(crate) fn version_parts_to_string(parts: &[VersionPart]) -> String {
        parts
            .iter()
            .map(|p| match p {
                VersionPart::Num(n) => n.to_string(),
                VersionPart::Str(s) => s.clone(),
                VersionPart::Whatever => "*".to_string(),
            })
            .collect::<Vec<_>>()
            .join(".")
    }

    /// Parse a version string into parts, handling:
    /// - `.`, `-`, `+`, `/` as separators
    /// - `_` as a separator (becomes a separate part if alone)
    /// - Transitions between digits and non-digit/non-separator chars create implicit splits
    /// - Leading zeros stripped from numeric parts
    pub(crate) fn parse_version_string(s: &str) -> (Vec<VersionPart>, bool, bool) {
        let mut plus = false;
        let mut minus = false;
        let mut raw = s;
        if let Some(stripped) = raw.strip_suffix('+') {
            plus = true;
            raw = stripped;
        } else if let Some(stripped) = raw.strip_suffix('-') {
            minus = true;
            raw = stripped;
        }

        let mut parts = Vec::new();
        let mut current = String::new();
        let mut is_digit_run = false;

        for ch in raw.chars() {
            match ch {
                '.' | '-' | '+' | '/' => {
                    if !current.is_empty() {
                        parts.push(Self::make_version_part(&current, is_digit_run));
                        current.clear();
                    }
                    is_digit_run = false;
                }
                '_' => {
                    if !current.is_empty() {
                        parts.push(Self::make_version_part(&current, is_digit_run));
                        current.clear();
                    }
                    // _ by itself is a "whatever" separator part
                    parts.push(VersionPart::Str("_".to_string()));
                    is_digit_run = false;
                }
                c if c.is_ascii_digit() => {
                    if !current.is_empty() && !is_digit_run {
                        // Transition from alpha to digit
                        parts.push(Self::make_version_part(&current, false));
                        current.clear();
                    }
                    current.push(c);
                    is_digit_run = true;
                }
                c => {
                    if !current.is_empty() && is_digit_run {
                        // Transition from digit to alpha
                        parts.push(Self::make_version_part(&current, true));
                        current.clear();
                    }
                    current.push(c);
                    is_digit_run = false;
                }
            }
        }
        if !current.is_empty() {
            parts.push(Self::make_version_part(&current, is_digit_run));
        }

        if parts.is_empty() {
            parts.push(VersionPart::Num(0));
        }

        (parts, plus, minus)
    }

    fn make_version_part(s: &str, is_digit: bool) -> VersionPart {
        if s == "*" {
            VersionPart::Whatever
        } else if is_digit {
            VersionPart::Num(s.parse::<i64>().unwrap_or(0))
        } else {
            VersionPart::Str(s.to_string())
        }
    }
}
