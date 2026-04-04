//! Comprehensive Raku string-to-numeric parsing.
//!
//! Handles: Int (with underscores, radix prefixes, generic radix `:base<digits>`),
//! Rat (`123.0`, `3/2`), Num (scientific notation with `e`/`E`),
//! Complex (`1+2i`), Inf, NaN, and U+2212 MINUS SIGN.

use crate::value::Value;

/// Result of attempting to parse a Raku numeric string.
/// Returns `Some(value)` on success, `None` on failure (invalid format).
pub(crate) fn parse_raku_str_to_numeric(input: &str) -> Option<Value> {
    let s = input.trim();
    if s.is_empty() {
        return Some(Value::Int(0));
    }

    // Normalize U+2212 MINUS SIGN to ASCII hyphen-minus
    let normalized = normalize_minus(s);
    let s = normalized.as_str();

    // Try Complex first (contains `i`)
    if let Some(v) = try_parse_complex(s) {
        return Some(v);
    }

    // Try Inf/NaN
    if let Some(v) = try_parse_inf_nan(s) {
        return Some(v);
    }

    // Strip leading sign
    let (sign, body) = strip_sign(s);

    // Try generic radix `:base<digits>`
    if body.starts_with(':') {
        return try_parse_generic_radix(body).map(|v| apply_sign(v, sign));
    }

    // Try 0-prefixed radix (0x, 0o, 0b, 0d)
    if body.starts_with('0')
        && body.len() >= 3
        && let Some(v) = try_parse_0_radix(body)
    {
        return Some(apply_sign(v, sign));
    }

    // Try fraction `N/D`
    if let Some(v) = try_parse_fraction(body, sign) {
        return Some(v);
    }

    // Try scientific notation (has e/E)
    if let Some(v) = try_parse_scientific(body, sign) {
        return Some(v);
    }

    // Try decimal with dot (Rat)
    if let Some(v) = try_parse_decimal_rat(body, sign) {
        return Some(v);
    }

    // Try plain integer (with underscores)
    try_parse_plain_int(body, sign)
}

/// Normalize U+2212 MINUS SIGN to ASCII hyphen-minus.
fn normalize_minus(s: &str) -> String {
    if s.contains('\u{2212}') {
        s.replace('\u{2212}', "-")
    } else {
        s.to_string()
    }
}

/// Strip leading sign, returning (sign_multiplier, remaining_body).
fn strip_sign(s: &str) -> (i32, &str) {
    if let Some(rest) = s.strip_prefix('-') {
        (-1, rest)
    } else if let Some(rest) = s.strip_prefix('+') {
        (1, rest)
    } else {
        (1, s)
    }
}

/// Apply sign to a Value.
fn apply_sign(v: Value, sign: i32) -> Value {
    if sign >= 0 {
        return v;
    }
    match v {
        Value::Int(i) => Value::Int(-i),
        Value::BigInt(n) => Value::BigInt(std::sync::Arc::new(-n.as_ref())),
        Value::Num(f) => Value::Num(-f),
        Value::Rat(n, d) => Value::Rat(-n, d),
        Value::FatRat(n, d) => Value::FatRat(-n, d),
        other => other,
    }
}

/// Validate underscore placement: no leading, trailing, or double underscores.
fn validate_underscores(s: &str) -> bool {
    if s.starts_with('_') || s.ends_with('_') || s.contains("__") {
        return false;
    }
    true
}

/// Strip underscores from a string, returning None if underscores are invalid.
fn strip_underscores(s: &str) -> Option<String> {
    if !validate_underscores(s) {
        return None;
    }
    Some(s.chars().filter(|&c| c != '_').collect())
}

fn try_parse_inf_nan(s: &str) -> Option<Value> {
    match s {
        "Inf" | "+Inf" => Some(Value::Num(f64::INFINITY)),
        "-Inf" => Some(Value::Num(f64::NEG_INFINITY)),
        "NaN" => Some(Value::Num(f64::NAN)),
        _ => None,
    }
}

/// Parse `0x`, `0o`, `0b`, `0d` prefixed integers.
fn try_parse_0_radix(body: &str) -> Option<Value> {
    let (base, digits) = match body.as_bytes().get(1)? {
        b'x' | b'X' => (16, &body[2..]),
        b'o' | b'O' => (8, &body[2..]),
        b'b' | b'B' => (2, &body[2..]),
        b'd' | b'D' => (10, &body[2..]),
        _ => return None,
    };

    // Allow leading underscore after 0b/0o/0x/0d prefix (Raku allows `0b_1`)
    let clean = strip_underscores_radix_body(digits)?;
    if clean.is_empty() {
        return None;
    }

    // Check all digits valid for base
    for c in clean.chars() {
        let dv = match c {
            '0'..='9' => c as u32 - '0' as u32,
            'a'..='f' => 10 + c as u32 - 'a' as u32,
            'A'..='F' => 10 + c as u32 - 'A' as u32,
            _ => return None,
        };
        if dv >= base {
            return None;
        }
    }

    if let Ok(n) = i64::from_str_radix(&clean, base) {
        Some(Value::Int(n))
    } else {
        use num_bigint::BigInt;
        use num_traits::Num;
        BigInt::from_str_radix(&clean, base)
            .ok()
            .map(|b| Value::BigInt(std::sync::Arc::new(b)))
    }
}

/// Strip underscores in radix body (allows leading underscore like `0b_1`).
fn strip_underscores_radix_body(s: &str) -> Option<String> {
    // No trailing underscore, no double underscores
    if s.ends_with('_') || s.contains("__") {
        return None;
    }
    let clean: String = s.chars().filter(|&c| c != '_').collect();
    Some(clean)
}

/// Parse `:base<digits>` generic radix notation.
fn try_parse_generic_radix(body: &str) -> Option<Value> {
    let rest = body.strip_prefix(':')?;

    // Parse base digits (must be plain ASCII digits, underscores allowed between)
    let (base_str, after_base) = scan_base_digits(rest)?;
    let base_clean = strip_underscores(&base_str)?;
    let base: u32 = base_clean.parse().ok()?;
    if !(2..=36).contains(&base) {
        return None;
    }

    let rest = after_base.strip_prefix('<')?;
    let close_pos = rest.find('>')?;
    if !rest[close_pos + 1..].trim().is_empty() {
        return None;
    }
    let digits_body = &rest[..close_pos];

    // Validate: no leading/trailing underscore in digit body
    if digits_body.is_empty() {
        return None;
    }
    if digits_body.starts_with('_') || digits_body.ends_with('_') {
        return None;
    }

    crate::runtime::utils::parse_radix_number_body(digits_body, base)
}

/// Scan base digits (ASCII digits + underscores) and return (scanned, rest).
fn scan_base_digits(s: &str) -> Option<(String, &str)> {
    let mut end = 0;
    let mut digits = String::new();
    let mut saw_digit = false;
    // No leading underscore in base
    if s.starts_with('_') {
        return None;
    }
    for (i, c) in s.char_indices() {
        if c.is_ascii_digit() {
            digits.push(c);
            saw_digit = true;
            end = i + 1;
        } else if c == '_' {
            digits.push(c);
            end = i + 1;
        } else {
            break;
        }
    }
    if saw_digit {
        Some((digits, &s[end..]))
    } else {
        None
    }
}

/// Parse fraction `N/D` as Rat.
fn try_parse_fraction(body: &str, sign: i32) -> Option<Value> {
    let slash_pos = body.find('/')?;
    let num_str = &body[..slash_pos];
    let den_str = &body[slash_pos + 1..];

    // Both parts must be plain integers (no dots, no e, no radix)
    let num_clean = strip_underscores(num_str)?;
    let den_clean = strip_underscores(den_str)?;

    // Must be pure digits
    if num_clean.is_empty()
        || den_clean.is_empty()
        || !num_clean.chars().all(|c| c.is_ascii_digit())
        || !den_clean.chars().all(|c| c.is_ascii_digit())
    {
        return None;
    }

    let n: i64 = num_clean.parse().ok()?;
    let d: i64 = den_clean.parse().ok()?;
    if d == 0 {
        return None;
    }
    let n = if sign < 0 { -n } else { n };
    Some(crate::value::make_rat(n, d))
}

/// Parse scientific notation: `123e0`, `123.0e2`, `1_2_3E0_0`, etc.
fn try_parse_scientific(body: &str, sign: i32) -> Option<Value> {
    // Must contain 'e' or 'E'
    let e_pos = body.find(['e', 'E'])?;
    let mantissa_str = &body[..e_pos];
    let exp_str = &body[e_pos + 1..];

    if exp_str.is_empty() {
        return None;
    }

    // Parse mantissa
    let mantissa = parse_mantissa(mantissa_str)?;

    // Parse exponent (may have sign)
    let (exp_sign, exp_body) = if let Some(rest) = exp_str.strip_prefix('+') {
        (1i32, rest)
    } else if let Some(rest) = exp_str.strip_prefix('-') {
        (-1i32, rest)
    } else {
        (1i32, exp_str)
    };

    let exp_clean = strip_underscores(exp_body)?;
    if exp_clean.is_empty() || !exp_clean.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }
    // If the exponent is too large for i32, the result overflows to Inf/0.0
    let exp_val: i32 = match exp_clean.parse() {
        Ok(v) => v,
        Err(_) => {
            // Exponent too large -- result is Inf or 0
            let result = if exp_sign > 0 {
                if sign < 0 {
                    f64::NEG_INFINITY
                } else {
                    f64::INFINITY
                }
            } else {
                // Huge negative exponent → 0.0
                if sign < 0 { -0.0 } else { 0.0 }
            };
            return Some(Value::Num(result));
        }
    };
    let exp = exp_sign * exp_val;

    let result = mantissa * 10f64.powi(exp);
    let result = if sign < 0 { -result } else { result };
    Some(Value::Num(result))
}

/// Parse mantissa for scientific notation (e.g., `123`, `123.01`, `1_2_3.0_1`).
fn parse_mantissa(s: &str) -> Option<f64> {
    if let Some(dot_pos) = s.find('.') {
        let int_str = &s[..dot_pos];
        let frac_str = &s[dot_pos + 1..];
        if frac_str.is_empty() {
            return None;
        }
        let int_clean = strip_underscores(int_str)?;
        let frac_clean = strip_underscores(frac_str)?;
        if int_clean.is_empty() || !int_clean.chars().all(|c| c.is_ascii_digit()) {
            return None;
        }
        if frac_clean.is_empty() || !frac_clean.chars().all(|c| c.is_ascii_digit()) {
            return None;
        }
        let combined = format!("{}.{}", int_clean, frac_clean);
        combined.parse().ok()
    } else {
        let clean = strip_underscores(s)?;
        if clean.is_empty() || !clean.chars().all(|c| c.is_ascii_digit()) {
            return None;
        }
        clean.parse().ok()
    }
}

/// Parse decimal number with dot as Rat (e.g., `123.0`, `1_2_3.0_0`).
fn try_parse_decimal_rat(body: &str, sign: i32) -> Option<Value> {
    let dot_pos = body.find('.')?;
    let int_str = &body[..dot_pos];
    let frac_str = &body[dot_pos + 1..];

    // Trailing dot not allowed (e.g., "123." fails)
    if frac_str.is_empty() {
        return None;
    }

    let int_clean = strip_underscores(int_str)?;
    let frac_clean = strip_underscores(frac_str)?;

    if int_clean.is_empty() || !int_clean.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }
    if frac_clean.is_empty() || !frac_clean.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }

    // Construct as Rat: integer_part * 10^frac_len + frac_part / 10^frac_len
    let int_val: i64 = int_clean.parse().ok()?;
    let frac_val: i64 = frac_clean.parse().ok()?;
    let denom: i64 = 10i64.checked_pow(frac_clean.len() as u32)?;
    let numer = int_val * denom + frac_val;
    let numer = if sign < 0 { -numer } else { numer };
    Some(crate::value::make_rat(numer, denom))
}

/// Parse plain integer with underscores.
fn try_parse_plain_int(body: &str, sign: i32) -> Option<Value> {
    let clean = strip_underscores(body)?;
    if clean.is_empty() || !clean.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }

    if let Ok(n) = clean.parse::<i64>() {
        let n = if sign < 0 { -n } else { n };
        Some(Value::Int(n))
    } else {
        // Try BigInt
        use num_bigint::BigInt;
        let mut n: BigInt = clean.parse().ok()?;
        if sign < 0 {
            n = -n;
        }
        Some(Value::from_bigint(n))
    }
}

/// Parse complex number strings like `1+2i`, `-1-2i`, `3+Inf\i`, etc.
fn try_parse_complex(s: &str) -> Option<Value> {
    // Must end with 'i' (or `\i`)
    if !s.ends_with('i') {
        return None;
    }

    // Check for `\i` suffix (backslash before i)
    let has_backslash_i = s.len() >= 2 && s.as_bytes()[s.len() - 2] == b'\\';
    let without_i = if has_backslash_i {
        &s[..s.len() - 2]
    } else {
        &s[..s.len() - 1]
    };

    // Find the split point between real and imaginary parts.
    // The imaginary part starts with a `+` or `-` that is NOT:
    // - at position 0
    // - right after an 'e' or 'E' (part of exponent)
    let split = find_complex_split(without_i)?;

    let real_str = &without_i[..split];
    let imag_str = &without_i[split..]; // includes the sign

    // Parse real part
    let real_val = parse_real_component(real_str)?;

    // Parse imaginary part (strip leading sign)
    let (imag_sign, imag_body) = strip_sign(imag_str);
    let imag_val = if imag_body.is_empty() {
        // bare +i or -i: not handled here (rakudo skips these too)
        return None;
    } else if imag_body == "Inf" || imag_body == "NaN" {
        // Inf/NaN as imaginary component requires `\i` suffix
        if !has_backslash_i {
            return None;
        }
        if imag_body == "Inf" {
            f64::INFINITY
        } else {
            f64::NAN
        }
    } else {
        parse_real_component_to_f64(imag_body)?
    };
    let imag_val = if imag_sign < 0 { -imag_val } else { imag_val };

    Some(Value::Complex(real_val, imag_val))
}

/// Find the index where the imaginary part starts (a `+` or `-` not part of exponent).
fn find_complex_split(s: &str) -> Option<usize> {
    let bytes = s.as_bytes();
    let mut i = 1; // skip position 0 (could be sign of real part)
    while i < bytes.len() {
        if (bytes[i] == b'+' || bytes[i] == b'-')
            && i > 0
            && bytes[i - 1] != b'e'
            && bytes[i - 1] != b'E'
        {
            return Some(i);
        }
        i += 1;
    }
    None
}

/// Parse a real component (for Complex) to a Value.
fn parse_real_component(s: &str) -> Option<f64> {
    if s == "Inf" || s == "+Inf" {
        return Some(f64::INFINITY);
    }
    if s == "-Inf" {
        return Some(f64::NEG_INFINITY);
    }
    parse_real_component_to_f64(s)
}

/// Parse a real component string to f64.
fn parse_real_component_to_f64(s: &str) -> Option<f64> {
    let (sign, body) = strip_sign(s);

    // Scientific notation
    if let Some(e_pos) = body.find(['e', 'E']) {
        let mantissa_str = &body[..e_pos];
        let exp_str = &body[e_pos + 1..];
        if exp_str.is_empty() {
            return None;
        }
        let mantissa = parse_mantissa(mantissa_str)?;
        let (exp_sign, exp_body) = strip_sign(exp_str);
        let exp_clean = strip_underscores(exp_body)?;
        if exp_clean.is_empty() {
            return None;
        }
        let exp_val: i32 = exp_clean.parse().ok()?;
        let exp = exp_sign * exp_val;
        let result = mantissa * 10f64.powi(exp);
        return Some(if sign < 0 { -result } else { result });
    }

    // Decimal
    if body.contains('.') {
        let dot_pos = body.find('.')?;
        let int_str = &body[..dot_pos];
        let frac_str = &body[dot_pos + 1..];
        if frac_str.is_empty() {
            return None;
        }
        let int_clean = strip_underscores(int_str)?;
        let frac_clean = strip_underscores(frac_str)?;
        let combined = format!("{}.{}", int_clean, frac_clean);
        let v: f64 = combined.parse().ok()?;
        return Some(if sign < 0 { -v } else { v });
    }

    // Plain integer
    let clean = strip_underscores(body)?;
    if clean.is_empty() || !clean.chars().all(|c| c.is_ascii_digit()) {
        // Try "Inf"
        if clean == "Inf" {
            return Some(if sign < 0 {
                f64::NEG_INFINITY
            } else {
                f64::INFINITY
            });
        }
        return None;
    }
    let v: f64 = clean.parse().ok()?;
    Some(if sign < 0 { -v } else { v })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check(input: &str, expected_type: &str, expected_f64: f64) {
        let result = parse_raku_str_to_numeric(input);
        assert!(result.is_some(), "Failed to parse '{}'", input);
        let val = result.unwrap();
        let type_name = match &val {
            Value::Int(_) | Value::BigInt(_) => "(Int)",
            Value::Num(_) => "(Num)",
            Value::Rat(_, _) | Value::FatRat(_, _) | Value::BigRat(_, _) => "(Rat)",
            Value::Complex(_, _) => "(Complex)",
            other => panic!("Unexpected type for '{}': {:?}", input, other),
        };
        assert_eq!(type_name, expected_type, "Type mismatch for '{}'", input);
        let actual_f64 = match &val {
            Value::Int(i) => *i as f64,
            Value::BigInt(n) => {
                use num_traits::ToPrimitive;
                n.to_f64().unwrap()
            }
            Value::Num(f) => *f,
            Value::Rat(n, d) => *n as f64 / *d as f64,
            _ => 0.0,
        };
        assert!(
            (actual_f64 - expected_f64).abs() < 1e-10 || actual_f64 == expected_f64,
            "Value mismatch for '{}': got {} expected {}",
            input,
            actual_f64,
            expected_f64
        );
    }

    fn f(input: &str) {
        let result = parse_raku_str_to_numeric(input);
        assert!(
            result.is_none(),
            "Expected '{}' to fail, got {:?}",
            input,
            result
        );
    }

    #[test]
    fn test_basic_int() {
        check("123", "(Int)", 123.0);
        check(" 123", "(Int)", 123.0);
        check("+123", "(Int)", 123.0);
        check("-123", "(Int)", -123.0);
        check("0000123", "(Int)", 123.0);
        check("1_2_3", "(Int)", 123.0);
    }

    #[test]
    fn test_invalid_int() {
        f("a+123");
        f("123foo");
        f("123+");
        f("1__2");
        f("123_");
        f("123 and stuff");
    }

    #[test]
    fn test_radix() {
        check("0b111", "(Int)", 7.0);
        check("0b1_1_1", "(Int)", 7.0);
        check("+0b111", "(Int)", 7.0);
        check("-0b111", "(Int)", -7.0);
        check("0b_1", "(Int)", 1.0);
        f("0b112");
        f("0b");
        check("0o77", "(Int)", 63.0);
        check("+0o77", "(Int)", 63.0);
        check("-0o77", "(Int)", -63.0);
        f("0o8");
        check("0d123", "(Int)", 123.0);
        check("-0d123", "(Int)", -123.0);
        f("0da");
        check("0x123", "(Int)", 291.0);
        check("-0x123", "(Int)", -291.0);
        check("0xa0", "(Int)", 160.0);
        check("-0xA0", "(Int)", -160.0);
        f("0xag");
        f("0xaf-");
    }

    #[test]
    fn test_generic_radix() {
        check(":10<42>", "(Int)", 42.0);
        check("-:10<42>", "(Int)", -42.0);
        check("-:1_0<4_2>", "(Int)", -42.0);
        check(":36<aZ>", "(Int)", 395.0);
        check(":2<11>", "(Int)", 3.0);
        f(":2<2>");
        f(":37<8>");
        f(":10<8_>");
        f(":10<_8>");
        f(":18<>");
        f(":10<8");
    }

    #[test]
    fn test_rat() {
        f("123.");
        check("123.0", "(Rat)", 123.0);
        check("-123.0", "(Rat)", -123.0);
        check("+123.0", "(Rat)", 123.0);
        check("+1_2_3.0_0", "(Rat)", 123.0);
        check("3/2", "(Rat)", 1.5);
        check("+3/2", "(Rat)", 1.5);
        check("-3/2", "(Rat)", -1.5);
    }

    #[test]
    fn test_scientific() {
        check("123e0", "(Num)", 123.0);
        check("-123e0", "(Num)", -123.0);
        check("+123e0", "(Num)", 123.0);
        check("+123.0e0", "(Num)", 123.0);
        check("+123.0_1e2", "(Num)", 12301.0);
        check("+123.0_1e0_2", "(Num)", 12301.0);
        check("123e-0", "(Num)", 123.0);
        check("-123e+0", "(Num)", -123.0);
        check("123E0", "(Num)", 123.0);
        check("1_2_3E0_0", "(Num)", 123.0);
        check("-123E0", "(Num)", -123.0);
        check("+123E0", "(Num)", 123.0);
        check("123E-0", "(Num)", 123.0);
        check("-123E+0_1", "(Num)", -1230.0);
        check("1230E-1", "(Num)", 123.0);
        check("-12E+1", "(Num)", -120.0);
        f("120e");
        f("120e2_");
    }

    #[test]
    fn test_unicode_minus() {
        check("\u{2212}42", "(Int)", -42.0);
        check("\u{2212}42.72", "(Rat)", -42.72);
        check("\u{2212}42e0", "(Num)", -42.0);
        check("42e\u{2212}10", "(Num)", 42e-10);
        check("\u{2212}42e\u{2212}10", "(Num)", -42e-10);
    }

    #[test]
    fn test_complex() {
        check("1+2i", "(Complex)", 0.0); // type check only
        f("3+3i+4i");
        f("3+3+4i");
        f("3+Infi"); // missing backslash
    }

    #[test]
    fn test_empty() {
        check("", "(Int)", 0.0);
        check(" ", "(Int)", 0.0);
        check("   ", "(Int)", 0.0);
    }
}
