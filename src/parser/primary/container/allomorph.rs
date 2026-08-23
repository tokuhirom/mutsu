use crate::value::{Value, ValueView};

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

/// Build the value of one quote-word.
///
/// Quote-words *always* yield the allomorph for a number-shaped word — there is
/// no "plain numeric" mode here. Raku's plain `Rat`/`Complex` come from the
/// separate `rat_number` / `complex_number` *literal terms*, which only the
/// `<...>`-as-a-term parser recognises; it unwraps the allomorph built here
/// after consulting [`angle_word_is_numeric_literal`].
pub(crate) fn angle_word_value(word: &str) -> Value {
    // Raku `<...>` words produce allomorphic types: numeric-looking words
    // become IntStr, RatStr, NumStr, or ComplexStr — values that smartmatch
    // against both their numeric type and Str.
    // We represent allomorphs as Mixin(numeric_value, {"Str": Str(word)}).

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
        return make_allomorphic_value(rat, word);
    }
    if let Some(complex) = parse_angle_complex(parse_word) {
        return make_allomorphic_value(complex, word);
    }
    if let Some(val) = parse_angle_inf_nan(parse_word) {
        return make_allomorphic_value(val, word);
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
        crate::parser::primary::number::integer_no_warn(num_word)
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
    let negated = match val.view() {
        ValueView::Int(n) => Some(Value::int(-n)),
        ValueView::BigInt(n) => Some(Value::bigint_arc(std::sync::Arc::new(-(&**n)))),
        ValueView::Num(n) => Some(Value::num(-n)),
        ValueView::Rat(n, d) => Some(Value::rat_raw(-n, d)),
        ValueView::FatRat(n, d) => Some(Value::fat_rat_raw(-n, d)),
        ValueView::BigRat(n, d) => Some(Value::bigrat(-n, d.clone())),
        _ => None,
    };
    negated.unwrap_or(val)
}

fn make_allomorphic_value(val: Value, word: &str) -> Value {
    let mut mixins = std::collections::HashMap::new();
    mixins.insert("Str".to_string(), Value::str(word.to_string()));
    Value::mixin(val, mixins)
}

/// Parse the `Inf` / `NaN` word forms, which are allomorphic `NumStr`s.
///
/// Raku accepts a sign on `Inf` (`<-Inf>` is a `NumStr`) but not on `NaN`
/// (`<-NaN>` stays a plain `Str`), and both spellings are case-sensitive:
/// `<inf>` and `<nan>` are ordinary strings.
fn parse_angle_inf_nan(word: &str) -> Option<Value> {
    match word {
        "Inf" | "+Inf" => Some(Value::num(f64::INFINITY)),
        "-Inf" => Some(Value::num(f64::NEG_INFINITY)),
        "NaN" => Some(Value::num(f64::NAN)),
        _ => None,
    }
}

/// Does this `<...>` content read as one of Raku's numeric *literal terms*
/// rather than as quote-words?
///
/// Raku's grammar has dedicated `rat_number` (`<nu/de>`) and `complex_number`
/// (`<re±im i>`) terms that evaluate to a plain `Rat` / `Complex`. Everything
/// else between `<` and `>` is ordinary quote-words and therefore yields the
/// *allomorph*. The distinction is purely syntactic, so the very same number
/// changes type once it is padded with spaces:
///
/// ```text
/// <42/10>    Rat          < 42/10 >    RatStr
/// <1+42i>    Complex      < 1+42i >    ComplexStr
/// ```
///
/// A bare imaginary such as `<42i>` has no real part, so it never matches
/// `complex_number` and stays a `ComplexStr` even when tight.
pub(crate) fn angle_word_is_numeric_literal(content: &str) -> bool {
    // Padding whitespace, or any backslash escape, means the content went
    // through quote-words and cannot be a literal term.
    if content.is_empty() || content.contains('\\') || content.chars().any(char::is_whitespace) {
        return false;
    }
    // U+2212 MINUS SIGN is accepted in these literals (roast pins `<5−1i>` as a
    // plain Complex), so normalize it the same way the value parsers do.
    let normalized;
    let content = if content.contains('\u{2212}') {
        normalized = content.replace('\u{2212}', "-");
        normalized.as_str()
    } else {
        content
    };
    if content.ends_with('i') {
        return matches!(parse_angle_complex_parts(content), Some((_, true)));
    }
    is_angle_rat_literal(content)
}

/// Raku's `bare_rat_number` production is `signed-integer '/' integer`: the
/// numerator may carry a sign but the denominator may not, so `<+1/2>` is a
/// literal `Rat` while `<1/+3>` is a `RatStr`.
fn is_angle_rat_literal(word: &str) -> bool {
    let Some((nu, de)) = word.split_once('/') else {
        return false;
    };
    if de.starts_with('+') || de.starts_with('-') {
        return false;
    }
    is_angle_integer_literal(nu) && is_angle_integer_literal(de)
}

fn is_angle_integer_literal(s: &str) -> bool {
    !s.is_empty() && (parse_angle_int(s).is_some() || parse_angle_bigint(s).is_some())
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

/// Parse a complex number from an angle bracket word.
/// Handles forms like: 3+0i, -2+5i, 0+31337i, 3-3i, 5i, -3i, 3.5+2.1i, 2e0+0i
fn parse_angle_complex(word: &str) -> Option<Value> {
    parse_angle_complex_parts(word).map(|(value, _)| value)
}

/// As [`parse_angle_complex`], but also reports whether the word carried an
/// explicit **real part**. Only the full `re±im i` form is Raku's
/// `complex_number` literal term; a pure imaginary like `42i` parses to the
/// same `Complex` value but is quote-words, so it stays a `ComplexStr`.
fn parse_angle_complex_parts(word: &str) -> Option<(Value, bool)> {
    let word = word.trim();
    // Must end with 'i'
    if !word.ends_with('i') {
        return None;
    }
    let without_i = &word[..word.len() - 1];

    // Pure imaginary: just "Ni" (e.g. "5i", "-3i") — no real part.
    if let Ok(imag) = without_i.parse::<f64>() {
        return Some((Value::complex(0.0, imag), false));
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
    Some((Value::complex(real, imag), true))
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
    Some(Value::num(val))
}

/// Strip the `Str` mixin off an allomorph, leaving the bare numeric value.
/// Used by the `<...>`-as-a-term parser for Raku's `rat_number` /
/// `complex_number` literal terms, which are plain `Rat` / `Complex`.
pub(super) fn strip_allomorph(value: Value) -> Value {
    match value.view() {
        ValueView::Mixin(inner, _) => inner.as_ref().clone(),
        _ => value,
    }
}
