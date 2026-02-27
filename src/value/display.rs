use super::*;
use num_bigint::BigInt as NumBigInt;
use num_traits::{Signed, ToPrimitive, Zero};

pub(crate) fn is_internal_anon_type_name(name: &str) -> bool {
    name.starts_with("__ANON_") && name.ends_with("__")
}

/// Format a Num in scientific notation matching Raku's output (e.g. `1e+40`, `-1e-05`).
fn format_num_scientific(f: f64) -> String {
    // Use Rust's {:e} format and ensure the exponent has an explicit sign
    let s = format!("{:e}", f);
    // Rust produces e.g. "1e40" or "1e-5"; Raku uses "1e+40" and "1e-05"
    if let Some(pos) = s.rfind('e') {
        let (mantissa, exp_part) = s.split_at(pos + 1); // exp_part is e.g. "40" or "-5"
        let exp_with_sign = if let Some(stripped) = exp_part.strip_prefix('-') {
            format!("-{:02}", stripped.parse::<i32>().unwrap_or(0).abs())
        } else {
            format!("+{}", exp_part)
        };
        format!("{}{}", mantissa, exp_with_sign)
    } else {
        s
    }
}

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
        if v.is_nan() {
            "NaN".to_string()
        } else if v.is_infinite() {
            if v > 0.0 {
                "Inf".to_string()
            } else {
                "-Inf".to_string()
            }
        } else if v.fract() == 0.0 {
            format!("{}", v as i64)
        } else {
            format!("{}", v)
        }
    }
    // Use \i notation when imaginary part is Inf, -Inf, or NaN
    let imag_special = i.is_infinite() || i.is_nan();
    let suffix = if imag_special { "\\i" } else { "i" };
    if i == 0.0 {
        format!("{}+0i", fmt_num(r))
    } else if i.is_nan() {
        format!("{}+NaN\\i", fmt_num(r))
    } else if i < 0.0 || (i.is_infinite() && i.is_sign_negative()) {
        format!("{}-{}{}", fmt_num(r), fmt_num(i.abs()), suffix)
    } else {
        format!("{}+{}{}", fmt_num(r), fmt_num(i), suffix)
    }
}

fn format_terminating_ratio_exact(
    numer: i64,
    denom: i64,
    append_dot_zero_for_integer: bool,
) -> String {
    if denom == 0 {
        if numer == 0 {
            return "NaN".to_string();
        }
        return if numer > 0 { "Inf" } else { "-Inf" }.to_string();
    }

    let sign = (numer < 0) ^ (denom < 0);
    let n = (numer as i128).abs();
    let d = (denom as i128).abs();
    let int_part = n / d;
    let mut rem = n % d;

    if rem == 0 {
        if append_dot_zero_for_integer {
            return format!("{}{}.0", if sign { "-" } else { "" }, int_part);
        }
        return format!("{}{}", if sign { "-" } else { "" }, int_part);
    }

    let mut frac = String::new();
    while rem != 0 {
        rem *= 10;
        let digit = rem / d;
        rem %= d;
        frac.push(char::from(b'0' + (digit as u8)));
    }

    format!("{}{}.{}", if sign { "-" } else { "" }, int_part, frac)
}

fn format_ratio_bigint_decimal(
    numer: &NumBigInt,
    denom: &NumBigInt,
    append_dot_zero_for_integer: bool,
    max_fraction_digits: Option<usize>,
) -> String {
    if denom.is_zero() {
        if numer.is_zero() {
            return "NaN".to_string();
        }
        return if numer.is_positive() {
            "Inf".to_string()
        } else {
            "-Inf".to_string()
        };
    }

    let sign = numer.is_negative() ^ denom.is_negative();
    let n = numer.abs();
    let d = denom.abs();
    let int_part = &n / &d;
    let mut rem = n % &d;

    if rem.is_zero() {
        if append_dot_zero_for_integer {
            return format!("{}{}.0", if sign { "-" } else { "" }, int_part);
        }
        return format!("{}{}", if sign { "-" } else { "" }, int_part);
    }

    let mut frac = String::new();
    while !rem.is_zero() {
        if max_fraction_digits.is_some_and(|limit| frac.len() >= limit) {
            break;
        }
        rem *= 10u8;
        let digit = &rem / &d;
        rem %= &d;
        let ch = digit.to_u8().unwrap_or(0);
        frac.push(char::from(b'0' + ch));
    }

    format!("{}{}.{}", if sign { "-" } else { "" }, int_part, frac)
}

fn has_terminating_decimal(denom: i64) -> bool {
    if denom == 0 {
        return false;
    }
    let mut dd = (denom as i128).abs();
    while dd % 2 == 0 {
        dd /= 2;
    }
    while dd % 5 == 0 {
        dd /= 5;
    }
    dd == 1
}

fn has_terminating_decimal_bigint(denom: &NumBigInt) -> bool {
    if denom.is_zero() {
        return false;
    }
    let mut dd = denom.abs();
    while (&dd % 2u8).is_zero() {
        dd /= 2u8;
    }
    while (&dd % 5u8).is_zero() {
        dd /= 5u8;
    }
    dd == NumBigInt::from(1u8)
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
                    let abs = f.abs();
                    if abs >= 1e15 || (abs != 0.0 && abs < 1e-4) {
                        // Scientific notation for very large/small integer-valued Nums
                        format_num_scientific(*f)
                    } else {
                        format!("{}", *f as i64)
                    }
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
            Value::Array(items, ..) => items
                .iter()
                .map(|v| v.to_string_value())
                .collect::<Vec<_>>()
                .join(" "),
            Value::LazyList(_) => "LazyList".to_string(),
            Value::Hash(items) => {
                let mut pairs: Vec<_> = items
                    .iter()
                    .map(|(k, v)| format!("{}\t{}", k, v.to_string_value()))
                    .collect();
                pairs.sort();
                pairs.join("\n")
            }
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
                } else if has_terminating_decimal(*d) {
                    // Exact decimal representation without f64 rounding.
                    format_terminating_ratio_exact(*n, *d, false)
                } else {
                    let whole = *n as f64 / *d as f64;
                    format!("{:.6}", whole)
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
                } else if has_terminating_decimal(*b) {
                    format_terminating_ratio_exact(*a, *b, false)
                } else {
                    let whole = *a as f64 / *b as f64;
                    format!("{:.6}", whole)
                }
            }
            Value::BigRat(n, d) => {
                if d.is_zero() {
                    if n.is_zero() {
                        "NaN".to_string()
                    } else if n.is_positive() {
                        "Inf".to_string()
                    } else {
                        "-Inf".to_string()
                    }
                } else if (n % d).is_zero() {
                    format!("{}", n / d)
                } else if has_terminating_decimal_bigint(d) {
                    format_ratio_bigint_decimal(n, d, false, None)
                } else {
                    format_ratio_bigint_decimal(n, d, false, Some(6))
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
            Value::ValuePair(k, v) => {
                format!("{}\t{}", k.to_string_value(), v.to_string_value())
            }
            Value::Enum { key, .. } => key.clone(),
            Value::CompUnitDepSpec { short_name } => {
                format!("CompUnit::DependencySpecification({})", short_name)
            }
            Value::Package(s) => {
                if is_internal_anon_type_name(s) {
                    "()".to_string()
                } else {
                    format!("({})", s)
                }
            }
            Value::ParametricRole {
                base_name,
                type_args,
            } => {
                let args: Vec<String> = type_args.iter().map(|a| a.to_string_value()).collect();
                format!("({}[{}])", base_name, args.join(","))
            }
            Value::Routine { package, name, .. } => format!("{}::{}", package, name),
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
            } if class_name == "Exception"
                || class_name.starts_with("X::")
                || class_name.starts_with("CX::") =>
            {
                attributes
                    .get("message")
                    .map(|v: &Value| v.to_string_value())
                    .unwrap_or_else(|| format!("{}()", class_name))
            }
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
                if let Some(Value::Array(bytes, ..)) = attributes.get("bytes") {
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
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Instant" => {
                let val = attributes.get("value").map(|v| v.to_f64()).unwrap_or(0.0);
                format!("Instant:{}", val)
            }
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Signature" => attributes
                .get("gist")
                .map(|v: &Value| v.to_string_value())
                .unwrap_or_else(|| format!("{}()", class_name)),
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Method" || class_name == "Sub" || class_name == "Routine" => {
                attributes
                    .get("name")
                    .map(|v: &Value| v.to_string_value())
                    .unwrap_or_else(|| format!("{}()", class_name))
            }
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Format" => attributes
                .get("format")
                .map(|v: &Value| v.to_string_value())
                .unwrap_or_else(|| format!("{}()", class_name)),
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
            Value::RegexWithAdverbs {
                pattern,
                exhaustive,
                repeat,
                perl5,
                ..
            } => {
                let mut prefix = String::new();
                if *exhaustive {
                    prefix.push_str(":ex");
                }
                if let Some(count) = repeat {
                    prefix.push_str(&format!(":x({count})"));
                }
                if *perl5 {
                    prefix.push_str(":P5");
                }
                format!("m{prefix}/{pattern}/")
            }
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
            Value::Seq(items) | Value::Slip(items) => items
                .iter()
                .map(|v| v.to_string_value())
                .collect::<Vec<_>>()
                .join(" "),
            Value::Promise(p) => format!("Promise({})", p.status()),
            Value::Channel(_) => "Channel".to_string(),
            Value::Nil => String::new(),
            Value::Whatever => "*".to_string(),
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
            Value::Uni { text, .. } => text.clone(),
            Value::Mixin(inner, mixins) => {
                if let Some(str_val) = mixins.get("Str") {
                    str_val.to_string_value()
                } else {
                    inner.to_string_value()
                }
            }
            Value::Proxy { .. } => "Proxy".to_string(),
            Value::CustomType { name, .. } => {
                if name.is_empty() {
                    "(CustomType)".to_string()
                } else {
                    format!("({})", name)
                }
            }
            Value::CustomTypeInstance { type_name, .. } => format!("{}()", type_name),
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
