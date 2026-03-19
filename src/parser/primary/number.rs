use super::super::parse_result::{PError, PResult, parse_char, parse_tag};

use crate::ast::Expr;
use crate::value::Value;

fn decimal_digit_value(c: char) -> Option<u32> {
    crate::builtins::unicode::unicode_decimal_digit_value(c)
}

fn hex_alpha_value(c: char) -> Option<u32> {
    match c {
        'a'..='f' => Some(10 + (c as u32 - 'a' as u32)),
        'A'..='F' => Some(10 + (c as u32 - 'A' as u32)),
        'ａ'..='ｆ' => Some(10 + (c as u32 - 'ａ' as u32)),
        'Ａ'..='Ｆ' => Some(10 + (c as u32 - 'Ａ' as u32)),
        _ => None,
    }
}

fn radix_alpha_value(c: char) -> Option<u32> {
    match c {
        'a'..='z' => Some(10 + (c as u32 - 'a' as u32)),
        'A'..='Z' => Some(10 + (c as u32 - 'A' as u32)),
        'ａ'..='ｚ' => Some(10 + (c as u32 - 'ａ' as u32)),
        'Ａ'..='Ｚ' => Some(10 + (c as u32 - 'Ａ' as u32)),
        _ => None,
    }
}

fn starts_with_decimal_digit(input: &str) -> bool {
    input.chars().next().and_then(decimal_digit_value).is_some()
}

fn scan_decimal_digits(input: &str) -> Option<(&str, String)> {
    let mut end = 0;
    let mut clean = String::new();
    let mut saw_digit = false;
    for c in input.chars() {
        if c == '_' {
            end += c.len_utf8();
            continue;
        }
        if let Some(dv) = decimal_digit_value(c) {
            clean.push(char::from_digit(dv, 10).unwrap());
            saw_digit = true;
            end += c.len_utf8();
            continue;
        }
        break;
    }
    if saw_digit {
        Some((&input[end..], clean))
    } else {
        None
    }
}

fn parse_prefixed_radix<'a>(
    input: &'a str,
    lower_prefix: &'static str,
    upper_prefix: &'static str,
    radix: u32,
    digit_value: impl Fn(char) -> Option<u32>,
) -> Option<PResult<'a, Expr>> {
    let (rest, _) = if let Ok((r, _)) = parse_tag(input, lower_prefix) {
        (r, ())
    } else if let Ok((r, _)) = parse_tag(input, upper_prefix) {
        (r, ())
    } else {
        return None;
    };
    let mut end = 0;
    let mut clean = String::new();
    for c in rest.chars() {
        if c == '_' {
            end += c.len_utf8();
            continue;
        }
        if let Some(v) = digit_value(c) {
            if v < radix {
                clean.push(char::from_digit(v, 36).unwrap());
                end += c.len_utf8();
                continue;
            }
            break;
        }
        break;
    }
    if clean.is_empty() {
        return Some(Err(PError::expected("radix digits")));
    }
    // Reject if the next character is a Unicode numeric that wasn't a valid digit for this radix
    let remaining = &rest[end..];
    if let Some(next_ch) = remaining.chars().next()
        && next_ch.is_numeric()
        && !next_ch.is_ascii_digit()
    {
        return Some(Err(PError::expected(
            "confused by Unicode numeric character after radix literal",
        )));
    }
    Some(Ok((remaining, parse_int_radix(&clean, radix))))
}

/// Parse an integer string with given radix, using BigInt for overflow.
pub(super) fn parse_int_radix(clean: &str, radix: u32) -> Expr {
    if let Ok(n) = i64::from_str_radix(clean, radix) {
        Expr::Literal(Value::Int(n))
    } else if let Some(n) = num_bigint::BigInt::parse_bytes(clean.as_bytes(), radix) {
        Expr::Literal(Value::bigint(n))
    } else {
        Expr::Literal(Value::Int(0))
    }
}

/// Parse an integer literal (including underscore separators).
pub(super) fn integer(input: &str) -> PResult<'_, Expr> {
    // Hex: 0x...
    if let Some(result) = parse_prefixed_radix(input, "0x", "0X", 16, |c| {
        decimal_digit_value(c).or_else(|| hex_alpha_value(c))
    }) {
        return result;
    }
    // Octal: 0o...
    if let Some(result) = parse_prefixed_radix(input, "0o", "0O", 8, decimal_digit_value) {
        return result;
    }
    // Binary: 0b...
    if let Some(result) = parse_prefixed_radix(input, "0b", "0B", 2, decimal_digit_value) {
        return result;
    }
    // Decimal: 0d...
    if let Some(result) = parse_prefixed_radix(input, "0d", "0D", 10, decimal_digit_value) {
        return result;
    }
    let (rest, clean) = scan_decimal_digits(input).ok_or_else(|| PError::expected("digits"))?;
    // Don't consume if next char is '.' followed by digit (that's a decimal)
    if let Some(after_dot) = rest.strip_prefix('.')
        && starts_with_decimal_digit(after_dot)
    {
        return Err(PError::expected("integer (not decimal)"));
    }
    if input.starts_with('0') && clean.len() > 1 {
        let literal = &input[..input.len() - rest.len()];
        let suggested_digits = clean.trim_start_matches('0');
        let suggested = format!(
            "0o{}",
            if suggested_digits.is_empty() {
                "0"
            } else {
                suggested_digits
            }
        );
        super::super::add_parse_warning(format!(
            "Potential difficulties:\n    Leading 0 does not indicate octal in Raku; use {} for octal values (found {}).",
            suggested, literal
        ));
    }
    // Check for scientific notation on integer: 42e0 → Num(42.0)
    if rest.starts_with('e') || rest.starts_with('E') {
        let mut r = &rest[1..];
        let mut exp_part = String::from("e");
        if r.starts_with('+') || r.starts_with('-') {
            exp_part.push_str(&r[..1]);
            r = &r[1..];
        }
        if let Some((r2, exp_digits)) = scan_decimal_digits(r) {
            exp_part.push_str(&exp_digits);
            let full = format!("{}{}", clean, exp_part);
            let n: f64 = full.parse().unwrap_or(0.0);
            // Check for imaginary suffix
            if r2.starts_with('i')
                && !r2[1..].starts_with(|c: char| c.is_alphanumeric() || c == '_')
            {
                return Ok((&r2[1..], Expr::Literal(Value::Complex(0.0, n))));
            }
            return Ok((r2, Expr::Literal(Value::Num(n))));
        }
    }
    // Check for imaginary suffix: 4i
    if rest.starts_with('i') && !rest[1..].starts_with(|c: char| c.is_alphanumeric() || c == '_') {
        let n: i64 = clean.parse().unwrap_or(0);
        return Ok((&rest[1..], Expr::Literal(Value::Complex(0.0, n as f64))));
    }
    // Try i64 first, fall back to BigInt for large integers
    if let Ok(n) = clean.parse::<i64>() {
        Ok((rest, Expr::Literal(Value::Int(n))))
    } else if let Ok(n) = clean.parse::<num_bigint::BigInt>() {
        Ok((rest, Expr::Literal(Value::bigint(n))))
    } else {
        Ok((rest, Expr::Literal(Value::Int(0))))
    }
}

/// Parse a decimal number literal.
/// In Raku, decimal literals without exponent are Rat, with exponent are Num.
pub(super) fn decimal(input: &str) -> PResult<'_, Expr> {
    let (rest, int_clean) = scan_decimal_digits(input).ok_or_else(|| PError::expected("digits"))?;
    let (rest, _) = parse_char(rest, '.')?;
    let (rest, frac_clean) =
        scan_decimal_digits(rest).ok_or_else(|| PError::expected("fraction digits"))?;
    let num_str = format!("{}.{}", int_clean, frac_clean);

    // Check for scientific notation
    let (rest, exp_part) = if rest.starts_with('e') || rest.starts_with('E') {
        let mut r = &rest[1..];
        let mut exp = String::from("e");
        if r.starts_with('+') || r.starts_with('-') {
            exp.push_str(&r[..1]);
            r = &r[1..];
        }
        if let Some((r2, exp_digits)) = scan_decimal_digits(r) {
            exp.push_str(&exp_digits);
            (r2, Some(exp))
        } else {
            (rest, None)
        }
    } else {
        (rest, None)
    };

    // With scientific notation → Num; without → Rat
    if let Some(exp) = exp_part {
        let full = format!("{}{}", num_str, exp);
        let n: f64 = full.parse().unwrap_or(0.0);
        if rest.starts_with('i')
            && !rest[1..].starts_with(|c: char| c.is_alphanumeric() || c == '_')
        {
            return Ok((&rest[1..], Expr::Literal(Value::Complex(0.0, n))));
        }
        Ok((rest, Expr::Literal(Value::Num(n))))
    } else {
        // Check for imaginary suffix first
        let n: f64 = num_str.parse().unwrap_or(0.0);
        if rest.starts_with('i')
            && !rest[1..].starts_with(|c: char| c.is_alphanumeric() || c == '_')
        {
            return Ok((&rest[1..], Expr::Literal(Value::Complex(0.0, n))));
        }
        // Produce Rat: numerator = int_part * 10^frac_digits + frac_part, denominator = 10^frac_digits
        let frac_digits = frac_clean.len() as u32;
        if frac_digits > 18 {
            // Too many decimal places for i64 — use BigInt to produce an exact Rat
            use num_bigint::BigInt;
            let int_val: BigInt = int_clean.parse().unwrap_or_default();
            let frac_val: BigInt = frac_clean.parse().unwrap_or_default();
            let denom: BigInt = BigInt::from(10).pow(frac_digits);
            let numer = int_val * &denom + frac_val;
            return Ok((
                rest,
                Expr::Literal(crate::value::make_big_rat(numer, denom)),
            ));
        }
        let denom = 10i64.pow(frac_digits);
        let int_val: i64 = int_clean.parse().unwrap_or(0);
        let frac_val: i64 = frac_clean.parse().unwrap_or(0);
        let numer = int_val
            .checked_mul(denom)
            .and_then(|v| v.checked_add(frac_val));
        match numer {
            Some(numer) => Ok((rest, Expr::Literal(crate::value::make_rat(numer, denom)))),
            None => {
                // i64 overflow — use BigInt for exact Rat
                use num_bigint::BigInt;
                let int_val: BigInt = int_clean.parse().unwrap_or_default();
                let frac_val: BigInt = frac_clean.parse().unwrap_or_default();
                let denom_big: BigInt = BigInt::from(10).pow(frac_digits);
                let numer_big = int_val * &denom_big + frac_val;
                Ok((
                    rest,
                    Expr::Literal(crate::value::make_big_rat(numer_big, denom_big)),
                ))
            }
        }
    }
}

/// Parse a decimal literal starting with `.` (e.g., `.5`, `.5i`, `.123e2`).
/// Without exponent → Rat; with exponent → Num.
pub(super) fn dot_decimal(input: &str) -> PResult<'_, Expr> {
    let (rest, _) = parse_char(input, '.')?;
    // Must be followed by a digit
    let (rest, frac_clean) =
        scan_decimal_digits(rest).ok_or_else(|| PError::expected("fraction digits"))?;
    let num_str = format!("0.{}", frac_clean);

    // Check for scientific notation
    let (rest, exp_part) = if rest.starts_with('e') || rest.starts_with('E') {
        let mut r = &rest[1..];
        let mut exp = String::from("e");
        if r.starts_with('+') || r.starts_with('-') {
            exp.push_str(&r[..1]);
            r = &r[1..];
        }
        if let Some((r2, exp_digits)) = scan_decimal_digits(r) {
            exp.push_str(&exp_digits);
            (r2, Some(exp))
        } else {
            (rest, None)
        }
    } else {
        (rest, None)
    };

    if let Some(exp) = exp_part {
        let full = format!("{}{}", num_str, exp);
        let n: f64 = full.parse().unwrap_or(0.0);
        if rest.starts_with('i')
            && !rest[1..].starts_with(|c: char| c.is_alphanumeric() || c == '_')
        {
            return Ok((&rest[1..], Expr::Literal(Value::Complex(0.0, n))));
        }
        Ok((rest, Expr::Literal(Value::Num(n))))
    } else {
        let n: f64 = format!("0.{}", frac_clean).parse().unwrap_or(0.0);
        if rest.starts_with('i')
            && !rest[1..].starts_with(|c: char| c.is_alphanumeric() || c == '_')
        {
            return Ok((&rest[1..], Expr::Literal(Value::Complex(0.0, n))));
        }
        let frac_digits = frac_clean.len() as u32;
        let denom = 10i64.pow(frac_digits);
        let numer: i64 = frac_clean.parse().unwrap_or(0);
        Ok((rest, Expr::Literal(crate::value::make_rat(numer, denom))))
    }
}

/// Parse a generic radix literal: :36<Unicodez>
pub(super) fn generic_radix(input: &str) -> PResult<'_, Expr> {
    let Some(r) = input.strip_prefix(':') else {
        return Err(PError::expected("generic radix literal"));
    };
    let Some((r, base_clean)) = scan_decimal_digits(r) else {
        return Err(PError::expected("generic radix literal"));
    };
    let base: u32 = base_clean.parse().unwrap_or(0);
    if !(2..=36).contains(&base) {
        return Err(PError::expected("generic radix base 2..36"));
    }
    let Some(r) = r.strip_prefix('<') else {
        return Err(PError::expected("generic radix literal"));
    };
    let Some(close_pos) = r.find('>') else {
        return Err(PError::expected("closing '>' for generic radix literal"));
    };
    let body = &r[..close_pos];
    let body: String = body.chars().filter(|c| !c.is_whitespace()).collect();
    if body.is_empty() {
        return Err(PError::expected("generic radix digits"));
    }
    let (digits_body, exponent_scale) = if let Some((digits, exp_part)) = body.split_once("*10**") {
        let exp_part = exp_part.trim();
        if exp_part.is_empty() {
            return Err(PError::expected("generic radix exponent"));
        }
        let (sign, after_sign) = if let Some(rest) = exp_part.strip_prefix('+') {
            (1_i64, rest)
        } else if let Some(rest) = exp_part.strip_prefix('-') {
            (-1_i64, rest)
        } else {
            (1_i64, exp_part)
        };
        let Some((exp_rest, exp_clean)) = scan_decimal_digits(after_sign) else {
            return Err(PError::expected("generic radix exponent"));
        };
        if !exp_rest.is_empty() {
            return Err(PError::expected("generic radix exponent"));
        }
        let exp_abs: i64 = exp_clean
            .parse()
            .map_err(|_| PError::expected("generic radix exponent"))?;
        (digits.trim(), sign * exp_abs)
    } else {
        (body.trim(), 0_i64)
    };
    if digits_body.is_empty() {
        return Err(PError::expected("generic radix digits"));
    }

    let mut int_clean = String::new();
    let mut frac_clean = String::new();
    let mut saw_dot = false;
    let mut saw_digit = false;
    for c in digits_body.chars() {
        if c == '_' {
            continue;
        }
        if c == '.' {
            if saw_dot {
                return Err(PError::expected("generic radix digits"));
            }
            saw_dot = true;
            continue;
        }
        let Some(value) = decimal_digit_value(c).or_else(|| radix_alpha_value(c)) else {
            return Err(PError::expected("generic radix digits"));
        };
        if value >= base {
            return Err(PError::expected("generic radix digits"));
        }
        saw_digit = true;
        let digit = char::from_digit(value, 36).unwrap();
        if saw_dot {
            frac_clean.push(digit);
        } else {
            int_clean.push(digit);
        }
    }
    if !saw_digit {
        return Err(PError::expected("generic radix digits"));
    }

    // Fast path for plain integer generic radix literals.
    if !saw_dot && exponent_scale == 0 {
        return Ok((&r[close_pos + 1..], parse_int_radix(&int_clean, base)));
    }

    let base_big = num_bigint::BigInt::from(base);
    let int_value = if int_clean.is_empty() {
        num_bigint::BigInt::from(0_i64)
    } else {
        num_bigint::BigInt::parse_bytes(int_clean.as_bytes(), base)
            .unwrap_or_else(|| num_bigint::BigInt::from(0_i64))
    };
    let frac_value = if frac_clean.is_empty() {
        num_bigint::BigInt::from(0_i64)
    } else {
        num_bigint::BigInt::parse_bytes(frac_clean.as_bytes(), base)
            .unwrap_or_else(|| num_bigint::BigInt::from(0_i64))
    };
    let frac_scale = base_big.pow(frac_clean.len() as u32);
    let mut numerator = int_value * &frac_scale + frac_value;
    let mut denominator = frac_scale;

    if exponent_scale != 0 {
        let exp_abs = exponent_scale.unsigned_abs() as u32;
        let scale10 = num_bigint::BigInt::from(10_u32).pow(exp_abs);
        if exponent_scale > 0 {
            numerator *= scale10;
        } else {
            denominator *= scale10;
        }
    }

    Ok((
        &r[close_pos + 1..],
        Expr::Literal(crate::value::make_big_rat(numerator, denominator)),
    ))
}

/// Parse a single Unicode numeric literal (vulgar fractions and superscript digits).
pub(super) fn unicode_numeric_literal(input: &str) -> PResult<'_, Expr> {
    fn is_superscript_digit(c: char) -> bool {
        matches!(
            c,
            '\u{2070}'
                | '\u{00B9}'
                | '\u{00B2}'
                | '\u{00B3}'
                | '\u{2074}'
                | '\u{2075}'
                | '\u{2076}'
                | '\u{2077}'
                | '\u{2078}'
                | '\u{2079}'
        )
    }

    let first = input
        .chars()
        .next()
        .ok_or_else(|| PError::expected("unicode numeric literal"))?;
    let rest = &input[first.len_utf8()..];
    if let Some(next) = rest.chars().next()
        && (next.is_alphanumeric() || next == '_')
        && !is_superscript_digit(next)
    {
        return Err(PError::expected("unicode numeric literal"));
    }
    if let Some((n, d)) = crate::builtins::unicode::unicode_rat_value(first) {
        return Ok((rest, Expr::Literal(crate::value::make_rat(n, d))));
    }
    if let Some(n) = crate::builtins::unicode::unicode_numeric_int_value(first) {
        return Ok((rest, Expr::Literal(Value::Int(n))));
    }
    Err(PError::expected("unicode numeric literal"))
}
