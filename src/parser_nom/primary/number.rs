use super::super::parse_result::{PError, PResult, parse_char, parse_tag, take_while1};

use crate::ast::Expr;
use crate::value::Value;

/// Parse an integer string with given radix, using BigInt for overflow.
pub(super) fn parse_int_radix(clean: &str, radix: u32) -> Expr {
    if let Ok(n) = i64::from_str_radix(clean, radix) {
        Expr::Literal(Value::Int(n))
    } else if let Some(n) = num_bigint::BigInt::parse_bytes(clean.as_bytes(), radix) {
        Expr::Literal(Value::BigInt(n))
    } else {
        Expr::Literal(Value::Int(0))
    }
}

/// Parse an integer literal (including underscore separators).
pub(super) fn integer(input: &str) -> PResult<'_, Expr> {
    // Hex: 0x...
    if let Ok((rest, _)) = parse_tag(input, "0x") {
        let (rest, digits) = take_while1(rest, |c: char| c.is_ascii_hexdigit() || c == '_')?;
        let clean: String = digits.chars().filter(|c| *c != '_').collect();
        return Ok((rest, parse_int_radix(&clean, 16)));
    }
    // Octal: 0o...
    if let Ok((rest, _)) = parse_tag(input, "0o") {
        let (rest, digits) = take_while1(rest, |c: char| matches!(c, '0'..='7' | '_'))?;
        let clean: String = digits.chars().filter(|c| *c != '_').collect();
        return Ok((rest, parse_int_radix(&clean, 8)));
    }
    // Binary: 0b...
    if let Ok((rest, _)) = parse_tag(input, "0b") {
        let (rest, digits) = take_while1(rest, |c: char| c == '0' || c == '1' || c == '_')?;
        let clean: String = digits.chars().filter(|c| *c != '_').collect();
        return Ok((rest, parse_int_radix(&clean, 2)));
    }
    let (rest, digits) = take_while1(input, |c: char| c.is_ascii_digit() || c == '_')?;
    // Don't consume if next char is '.' followed by digit (that's a decimal)
    if rest.starts_with('.') && rest.len() > 1 && rest.as_bytes()[1].is_ascii_digit() {
        return Err(PError::expected("integer (not decimal)"));
    }
    let clean: String = digits.chars().filter(|c| *c != '_').collect();
    // Check for scientific notation on integer: 42e0 → Num(42.0)
    if (rest.starts_with('e') || rest.starts_with('E'))
        && rest.len() > 1
        && (rest.as_bytes()[1].is_ascii_digit()
            || ((rest.as_bytes()[1] == b'+' || rest.as_bytes()[1] == b'-')
                && rest.len() > 2
                && rest.as_bytes()[2].is_ascii_digit()))
    {
        let exp_start = rest;
        let r = &rest[1..];
        let r = if r.starts_with('+') || r.starts_with('-') {
            &r[1..]
        } else {
            r
        };
        if let Ok((r, _)) = take_while1(r, |c: char| c.is_ascii_digit()) {
            let exp_part = &exp_start[..exp_start.len() - r.len()];
            let full = format!("{}{}", clean, exp_part);
            let n: f64 = full.parse().unwrap_or(0.0);
            // Check for imaginary suffix
            if r.starts_with('i') && !r[1..].starts_with(|c: char| c.is_alphanumeric() || c == '_')
            {
                return Ok((&r[1..], Expr::Literal(Value::Complex(0.0, n))));
            }
            return Ok((r, Expr::Literal(Value::Num(n))));
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
        Ok((rest, Expr::Literal(Value::BigInt(n))))
    } else {
        Ok((rest, Expr::Literal(Value::Int(0))))
    }
}

/// Parse a decimal number literal.
pub(super) fn decimal(input: &str) -> PResult<'_, Expr> {
    let start = input;
    let (rest, _) = take_while1(input, |c: char| c.is_ascii_digit() || c == '_')?;
    let (rest, _) = parse_char(rest, '.')?;
    let (rest, _) = take_while1(rest, |c: char| c.is_ascii_digit() || c == '_')?;
    let num_str = &start[..start.len() - rest.len()];

    // Check for scientific notation
    let (rest, exp_part) = if rest.starts_with('e') || rest.starts_with('E') {
        let exp_start = rest;
        let r = &rest[1..];
        let r = if r.starts_with('+') || r.starts_with('-') {
            &r[1..]
        } else {
            r
        };
        if let Ok((r, _)) = take_while1(r, |c: char| c.is_ascii_digit()) {
            (r, Some(&exp_start[..exp_start.len() - r.len()]))
        } else {
            (rest, None)
        }
    } else {
        (rest, None)
    };

    let full = if let Some(exp) = exp_part {
        format!("{}{}", num_str, exp)
    } else {
        num_str.to_string()
    };
    let clean: String = full.chars().filter(|c| *c != '_').collect();
    let n: f64 = clean.parse().unwrap_or(0.0);
    // Check for imaginary suffix
    if rest.starts_with('i') && !rest[1..].starts_with(|c: char| c.is_alphanumeric() || c == '_') {
        return Ok((&rest[1..], Expr::Literal(Value::Complex(0.0, n))));
    }
    Ok((rest, Expr::Literal(Value::Num(n))))
}
