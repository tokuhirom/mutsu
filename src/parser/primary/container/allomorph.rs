use crate::value::Value;

/// Unescape backslash sequences in a `<...>` word.
/// Per Raku spec, `<...>` is `q:w` quoting which only processes a small set of
/// backslash escapes: `\\` → `\`, `\<` → `<`, `\>` → `>`, `\ ` → space (allows
/// embedded spaces in a word), `\#` → `#`. Other backslash sequences (e.g. `\n`)
/// are kept literally.
fn unescape_angle_word(word: &str) -> String {
    let mut out = String::with_capacity(word.len());
    let mut chars = word.chars();
    while let Some(ch) = chars.next() {
        if ch == '\\'
            && let Some(next) = chars.next()
        {
            match next {
                '\\' => out.push('\\'),
                '<' => out.push('<'),
                '>' => out.push('>'),
                ' ' => out.push(' '),
                '#' => out.push('#'),
                other => {
                    out.push('\\');
                    out.push(other);
                }
            }
        } else {
            out.push(ch);
        }
    }
    out
}

pub(crate) fn angle_word_value(word: &str) -> Value {
    angle_word_value_impl(word, false)
}

/// Like `angle_word_value` but always produces allomorphic types for fractions.
/// Used for multi-element word lists where `<2/3>` becomes RatStr, not plain Rat.
pub(crate) fn angle_word_value_full_allomorphic(word: &str) -> Value {
    angle_word_value_impl(word, true)
}

fn angle_word_value_impl(word: &str, fraction_allomorphic: bool) -> Value {
    // Raku `<...>` words produce allomorphic types: numeric-looking words
    // become IntStr, RatStr, NumStr, or ComplexStr — values that smartmatch
    // against both their numeric type and Str.
    // We represent allomorphs as Mixin(numeric_value, {"Str": Str(word)}).
    // For single-element <2/3>, fraction notation produces a plain Rat, not RatStr.
    // For multi-element lists, fractions produce RatStr.

    // Process backslash escapes within the word: `\\` → `\`, `\<`/`\>` → `<`/`>`,
    // `\ ` → space (allows embedded spaces in a word).
    let unescaped_storage;
    let word: &str = if word.contains('\\') {
        unescaped_storage = unescape_angle_word(word);
        unescaped_storage.as_str()
    } else {
        word
    };

    // Normalize U+2212 MINUS SIGN to ASCII minus for numeric parsing.
    // The allomorphic Str part retains the original word spelling.
    let normalized;
    let parse_word = if word.contains('\u{2212}') {
        normalized = word.replace('\u{2212}', "-");
        normalized.as_str()
    } else {
        word
    };
    if let Some(rat) = parse_angle_rat_word(parse_word) {
        if fraction_allomorphic {
            return make_allomorphic_value(rat, word);
        }
        return rat;
    }
    if let Some(complex) = parse_angle_complex(parse_word) {
        return make_allomorphic_value(complex, word);
    }
    // The plain integer/decimal/Num parsers accept only unsigned digits (the
    // sign is normally a prefix operator), so a leading `+`/`-` is stripped here
    // and reapplied to the parsed value. This makes `<-3>` an IntStr and
    // `<-3.5>` a RatStr, matching Raku (rather than a bare Str). The allomorphic
    // Str component keeps the original signed spelling.
    let (negate, num_word) = match parse_word.strip_prefix('-') {
        Some(rest) if !rest.is_empty() => (true, rest),
        _ => (false, parse_word.strip_prefix('+').unwrap_or(parse_word)),
    };
    let apply = |val: Value| -> Value {
        let val = if negate {
            negate_angle_numeric(val)
        } else {
            val
        };
        make_allomorphic_value(val, word)
    };
    if let Ok((rest, crate::ast::Expr::Literal(val))) =
        crate::parser::primary::number::integer(num_word)
        && rest.is_empty()
    {
        return apply(val);
    }
    if let Ok((rest, crate::ast::Expr::Literal(val))) =
        crate::parser::primary::number::decimal(num_word)
        && rest.is_empty()
    {
        return apply(val);
    }
    if let Ok((rest, crate::ast::Expr::Literal(val))) =
        crate::parser::primary::number::dot_decimal(num_word)
        && rest.is_empty()
    {
        return apply(val);
    }
    if let Some(val) = parse_angle_num(num_word) {
        return apply(val);
    }
    Value::str(word.to_string())
}

/// Negate a numeric Value produced by the unsigned angle-word number parsers.
/// Non-numeric values are returned unchanged (the caller only passes numerics).
fn negate_angle_numeric(val: Value) -> Value {
    match val {
        Value::Int(n) => Value::Int(-n),
        Value::BigInt(n) => Value::BigInt(std::sync::Arc::new(-(&*n))),
        Value::Num(n) => Value::Num(-n),
        Value::Rat(n, d) => Value::Rat(-n, d),
        Value::FatRat(n, d) => Value::FatRat(-n, d),
        Value::BigRat(n, d) => Value::bigrat(-(*n), *d),
        other => other,
    }
}

fn make_allomorphic_value(val: Value, word: &str) -> Value {
    let mut mixins = std::collections::HashMap::new();
    mixins.insert("Str".to_string(), Value::str(word.to_string()));
    Value::mixin(val, mixins)
}

fn parse_angle_rat_word(word: &str) -> Option<Value> {
    let (lhs, rhs) = word.split_once('/')?;
    if lhs.is_empty() || rhs.is_empty() {
        return None;
    }
    // Don't parse negative denominators as Rat (Raku spec: <1/-3> is Str)
    if rhs.starts_with('-') {
        return None;
    }
    // Try i64 first, fall back to BigInt for large numbers
    if let (Some(n), Some(d)) = (parse_angle_int(lhs), parse_angle_int(rhs)) {
        return Some(crate::value::make_rat(n, d));
    }
    // BigInt fallback
    let numer = parse_angle_bigint(lhs)?;
    let denom = parse_angle_bigint(rhs)?;
    Some(crate::value::make_big_rat(numer, denom))
}

fn parse_angle_bigint(s: &str) -> Option<num_bigint::BigInt> {
    let (sign_neg, rest) = if let Some(rest) = s.strip_prefix('+') {
        (false, rest)
    } else if let Some(rest) = s.strip_prefix('-') {
        (true, rest)
    } else {
        (false, s)
    };
    if rest.is_empty() {
        return None;
    }
    let clean: String = rest.chars().filter(|c| *c != '_').collect();
    if clean.is_empty() {
        return None;
    }
    // Support 0x, 0b, 0o prefixes
    let val = if let Some(hex) = clean
        .strip_prefix("0x")
        .or_else(|| clean.strip_prefix("0X"))
    {
        num_bigint::BigInt::parse_bytes(hex.as_bytes(), 16)?
    } else if let Some(bin) = clean
        .strip_prefix("0b")
        .or_else(|| clean.strip_prefix("0B"))
    {
        num_bigint::BigInt::parse_bytes(bin.as_bytes(), 2)?
    } else if let Some(oct) = clean
        .strip_prefix("0o")
        .or_else(|| clean.strip_prefix("0O"))
    {
        num_bigint::BigInt::parse_bytes(oct.as_bytes(), 8)?
    } else {
        clean.parse::<num_bigint::BigInt>().ok()?
    };
    if sign_neg { Some(-val) } else { Some(val) }
}

/// Parse an integer that may have a 0x/0b/0o prefix, sign, or underscores.
fn parse_angle_int(s: &str) -> Option<i64> {
    let (sign, rest) = if let Some(rest) = s.strip_prefix('+') {
        (1i64, rest)
    } else if let Some(rest) = s.strip_prefix('-') {
        (-1i64, rest)
    } else {
        (1i64, s)
    };
    if rest.is_empty() {
        return None;
    }
    let clean: String = rest.chars().filter(|c| *c != '_').collect();
    if clean.is_empty() {
        return None;
    }
    if let Some(hex) = clean
        .strip_prefix("0x")
        .or_else(|| clean.strip_prefix("0X"))
    {
        return i64::from_str_radix(hex, 16).ok().map(|n| sign * n);
    }
    if let Some(bin) = clean
        .strip_prefix("0b")
        .or_else(|| clean.strip_prefix("0B"))
    {
        return i64::from_str_radix(bin, 2).ok().map(|n| sign * n);
    }
    if let Some(oct) = clean
        .strip_prefix("0o")
        .or_else(|| clean.strip_prefix("0O"))
    {
        return i64::from_str_radix(oct, 8).ok().map(|n| sign * n);
    }
    clean.parse::<i64>().ok().map(|n| sign * n)
}

/// Parse a complex number literal from an angle bracket word.
/// Handles forms like: 3+0i, -2+5i, 0+31337i, 3-3i, 5i, -3i, 3.5+2.1i, 2e0+0i
fn parse_angle_complex(word: &str) -> Option<Value> {
    let word = word.trim();
    // Must end with 'i'
    if !word.ends_with('i') {
        return None;
    }
    let without_i = &word[..word.len() - 1];

    // Pure imaginary: just "Ni" (e.g. "5i", "-3i")
    if let Ok(imag) = without_i.parse::<f64>() {
        return Some(Value::Complex(0.0, imag));
    }

    // Find the last '+' or '-' that splits real from imaginary.
    // Skip the first character to allow a leading sign on the real part.
    // Also skip 'e'/'E' followed by sign (scientific notation like 2e-3).
    let bytes = without_i.as_bytes();
    let mut split_pos = None;
    let mut i = 1;
    while i < bytes.len() {
        if (bytes[i] == b'+' || bytes[i] == b'-')
            && i > 0
            && bytes[i - 1] != b'e'
            && bytes[i - 1] != b'E'
        {
            split_pos = Some(i);
        }
        i += 1;
    }

    let split_pos = split_pos?;
    let real_str = &without_i[..split_pos];
    let imag_str = &without_i[split_pos..];

    let real: f64 = real_str.parse().ok()?;
    let imag: f64 = imag_str.parse().ok()?;
    Some(Value::Complex(real, imag))
}

/// Parse a Num (floating-point with exponent) from an angle bracket word.
/// Handles forms like: 2e0, 5e0, -8e0, 3.5e2
fn parse_angle_num(word: &str) -> Option<Value> {
    let word = word.trim();
    // Must contain 'e' or 'E' to be a Num (otherwise it would have been caught by decimal)
    if !word.contains('e') && !word.contains('E') {
        return None;
    }
    let val: f64 = word.parse().ok()?;
    Some(Value::Num(val))
}

/// Build an `Expr::Literal` from a single angle-bracket word, applying allomorphic
/// type inference (IntStr, RatStr, etc.).
pub(super) fn angle_word_expr(word: &str) -> crate::ast::Expr {
    crate::ast::Expr::Literal(angle_word_value(word))
}
