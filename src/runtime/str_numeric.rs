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

    // If the string contains Unicode decimal digits (Nd category), normalize
    // them to their ASCII equivalents before further parsing.
    if s.chars().any(|c| {
        !c.is_ascii() && crate::builtins::unicode::unicode_decimal_digit_value(c).is_some()
    }) && let Some(ascii_str) = normalize_unicode_decimal_digits(s)
    {
        return parse_raku_str_to_numeric(&ascii_str);
    }

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

    // Try multiplicative notation `N*B**K` (e.g., `2*10**3`, `0b1111*10**3`)
    if let Some(v) = try_parse_multiplicative(body, sign) {
        return Some(v);
    }

    // Try 0-prefixed radix (0x, 0o, 0b, 0d) with optional fractional part
    if body.starts_with('0')
        && body.len() >= 3
        && let Some(v) = try_parse_0_radix(body)
    {
        return Some(apply_sign(v, sign));
    }

    // Try fraction `N/D` (with optional decimal parts and signed denominators)
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

/// Inspect a string that failed `parse_raku_str_to_numeric` and classify the
/// failure the way rakudo's X::Str::Numeric does: returns `(pos, reason)` where
/// `pos` is the 0-based character index at which parsing stopped and `reason`
/// is the human-readable explanation. Returns `None` when the string actually
/// IS a valid number (so callers can treat `None` as "no error").
pub(crate) fn str_numeric_failure(s: &str) -> Option<(usize, String)> {
    if parse_raku_str_to_numeric(s).is_some() {
        return None;
    }
    let end = numeric_prefix_end_byte(s);
    if end == 0 {
        // Nothing number-like at the front.
        Some((
            0,
            "base-10 number must begin with valid digits or '.'".to_string(),
        ))
    } else {
        // A valid number was parsed but extra non-whitespace follows it.
        let char_pos = s[..end].chars().count();
        Some((char_pos, "trailing characters after number".to_string()))
    }
}

/// Scan the leading numeric token (skipping leading whitespace) of `s` and
/// return the byte index just past it, or 0 when the string does not begin with
/// a number. Handles the common int/decimal/scientific/signed forms; exotic
/// forms (radix, complex, fractions) are not scanned precisely but still yield a
/// non-zero prefix when they start with digits, which is enough to distinguish
/// "trailing characters" from "no leading number".
fn numeric_prefix_end_byte(s: &str) -> usize {
    let bytes = s.as_bytes();
    let n = bytes.len();
    let mut i = 0;
    while i < n && (bytes[i] as char).is_ascii_whitespace() {
        i += 1;
    }
    if i < n && (bytes[i] == b'+' || bytes[i] == b'-') {
        i += 1;
    }
    let mut saw_digit = false;
    while i < n {
        let c = bytes[i];
        if c.is_ascii_digit() || c == b'_' {
            saw_digit = true;
            i += 1;
        } else if c == b'.' {
            i += 1;
        } else if (c == b'e' || c == b'E') && saw_digit {
            i += 1;
            if i < n && (bytes[i] == b'+' || bytes[i] == b'-') {
                i += 1;
            }
        } else {
            break;
        }
    }
    if saw_digit { i } else { 0 }
}

/// Normalize U+2212 MINUS SIGN to ASCII hyphen-minus.
fn normalize_minus(s: &str) -> String {
    if s.contains('\u{2212}') {
        s.replace('\u{2212}', "-")
    } else {
        s.to_string()
    }
}

/// Normalize a string of Unicode decimal digits (Nd category) to ASCII digits.
/// Returns None if any non-ASCII character is NOT a decimal digit (Nd).
fn normalize_unicode_decimal_digits(s: &str) -> Option<String> {
    let mut result = String::with_capacity(s.len());
    for ch in s.chars() {
        if ch.is_ascii() {
            result.push(ch);
        } else if let Some(d) = crate::builtins::unicode::unicode_decimal_digit_value(ch) {
            result.push(char::from_digit(d, 10).unwrap());
        } else {
            return None;
        }
    }
    Some(result)
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

/// Parse multiplicative notation: `N*B**K` (e.g., `2*10**3`, `42.25*10**4`)
/// The base B can have an optional sign: `N*+B**K`, `N*-B**K`
/// The exponent K can also have an optional sign: `N*B**+K`
fn try_parse_multiplicative(body: &str, sign: i32) -> Option<Value> {
    let star_pos = body.find('*')?;
    let left = &body[..star_pos];
    let right = &body[star_pos + 1..];

    // Right side must contain ** (power notation)
    let pow_pos = right.find("**")?;
    let base_str = &right[..pow_pos];
    let exp_str = &right[pow_pos + 2..];

    // Parse left (coefficient): can be integer, decimal, or 0-prefixed radix
    let coeff = parse_coefficient(left)?;

    // Parse base with optional sign
    let (base_sign, base_body) = strip_sign(base_str);
    let base_clean = strip_underscores(base_body)?;
    if base_clean.is_empty() || !base_clean.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }
    let base_int: i64 = base_clean.parse().ok()?;
    let base_int = if base_sign < 0 { -base_int } else { base_int };

    // Parse exponent with optional sign
    let (exp_sign, exp_body) = strip_sign(exp_str);
    let exp_clean = strip_underscores(exp_body)?;
    if exp_clean.is_empty() || !exp_clean.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }
    let exp_val: i32 = exp_clean.parse().ok()?;
    let exp_val = if exp_sign < 0 { -exp_val } else { exp_val };

    // Try to produce an exact integer or Rat result when possible
    if exp_val >= 0
        && let Some(power) = (base_int).checked_pow(exp_val as u32)
    {
        match &coeff {
            Value::Int(n) => {
                if let Some(result) = n.checked_mul(power) {
                    let result = if sign < 0 { -result } else { result };
                    return Some(Value::Int(result));
                }
            }
            Value::Rat(n, d) => {
                if let Some(result_n) = n.checked_mul(power) {
                    let result_n = if sign < 0 { -result_n } else { result_n };
                    return Some(crate::value::make_rat(result_n, *d));
                }
            }
            _ => {}
        }
    }

    let result = value_to_f64(&coeff) * (base_int as f64).powi(exp_val);
    let result = if sign < 0 { -result } else { result };
    Some(Value::Num(result))
}

/// Parse a coefficient for multiplicative notation.
/// Can be plain integer, decimal, or 0-prefixed radix number (with optional fractional part).
fn parse_coefficient(s: &str) -> Option<Value> {
    // Try 0-prefixed radix first
    if s.starts_with('0')
        && s.len() >= 3
        && let Some(v) = try_parse_0_radix(s)
    {
        return Some(v);
    }
    // Try decimal with dot
    if let Some(v) = try_parse_decimal_rat(s, 1) {
        return Some(v);
    }
    // Try plain integer
    try_parse_plain_int(s, 1)
}

/// Convert a Value to f64 for arithmetic operations.
fn value_to_f64(v: &Value) -> f64 {
    match v {
        Value::Int(i) => *i as f64,
        Value::Num(f) => *f,
        Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
        Value::FatRat(n, d) if *d != 0 => *n as f64 / *d as f64,
        Value::BigInt(n) => {
            use num_traits::ToPrimitive;
            n.to_f64().unwrap_or(f64::INFINITY)
        }
        _ => 0.0,
    }
}

/// Parse `0x`, `0o`, `0b`, `0d` prefixed numbers (with optional fractional part).
fn try_parse_0_radix(body: &str) -> Option<Value> {
    let (base, digits) = match body.as_bytes().get(1)? {
        b'x' | b'X' => (16, &body[2..]),
        b'o' | b'O' => (8, &body[2..]),
        b'b' | b'B' => (2, &body[2..]),
        b'd' | b'D' => (10, &body[2..]),
        _ => return None,
    };

    // Check for fractional part (e.g., 0b111.11)
    if let Some(dot_pos) = digits.find('.') {
        let int_part = &digits[..dot_pos];
        let frac_part = &digits[dot_pos + 1..];

        let int_clean = strip_underscores_radix_body(int_part)?;
        let frac_clean = strip_underscores_radix_body(frac_part)?;

        if int_clean.is_empty() || frac_clean.is_empty() {
            return None;
        }

        // Validate digits for base
        validate_radix_digits(&int_clean, base)?;
        validate_radix_digits(&frac_clean, base)?;

        // Calculate: int_part + frac_part / base^frac_len as Rat
        let int_val = i64::from_str_radix(&int_clean, base).ok()?;
        let frac_val = i64::from_str_radix(&frac_clean, base).ok()?;
        let denom = (base as i64).checked_pow(frac_clean.len() as u32)?;
        let numer = int_val.checked_mul(denom)?.checked_add(frac_val)?;
        return Some(crate::value::make_rat(numer, denom));
    }

    // Allow leading underscore after 0b/0o/0x/0d prefix (Raku allows `0b_1`)
    let clean = strip_underscores_radix_body(digits)?;
    if clean.is_empty() {
        return None;
    }

    // Check all digits valid for base
    validate_radix_digits(&clean, base)?;

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

/// Validate that all characters are valid digits for the given base.
fn validate_radix_digits(s: &str, base: u32) -> Option<()> {
    for c in s.chars() {
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
    Some(())
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

    // Support both <> and «» (French/guillemet) delimiters
    let (rest, close_char) = if let Some(r) = after_base.strip_prefix('<') {
        (r, '>')
    } else if let Some(r) = after_base.strip_prefix('\u{ab}') {
        // «
        (r, '\u{bb}') // »
    } else {
        return None;
    };
    let close_pos = rest.find(close_char)?;
    if !rest[close_pos + close_char.len_utf8()..].trim().is_empty() {
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

    // Parse numerator (can be integer or decimal with underscores)
    let (num_n, num_d) = parse_fraction_part(num_str, false)?;
    // Parse denominator (can have optional sign, can be integer or decimal)
    let (den_n, den_d) = parse_fraction_part(den_str, true)?;

    if den_n == 0 && den_d == 0 {
        return None;
    }

    // Result is (num_n/num_d) / (den_n/den_d) = (num_n * den_d) / (num_d * den_n)
    let result_n = num_n.checked_mul(den_d)?;
    let result_d = num_d.checked_mul(den_n.abs())?;
    if result_d == 0 {
        return None;
    }

    let result_n = if sign < 0 { -result_n } else { result_n };
    let result_n = if den_n < 0 { -result_n } else { result_n };
    Some(crate::value::make_rat(result_n, result_d))
}

/// Parse a numerator or denominator part of a fraction.
/// Returns (numerator, denominator) as a rational number.
/// When `allow_sign` is true, the part can have a leading +/- sign.
fn parse_fraction_part(s: &str, allow_sign: bool) -> Option<(i64, i64)> {
    let (part_sign, body) = if allow_sign { strip_sign(s) } else { (1, s) };

    if let Some(dot_pos) = body.find('.') {
        // Decimal part (e.g., "42.15" or "15.45")
        let int_str = &body[..dot_pos];
        let frac_str = &body[dot_pos + 1..];
        let int_clean = strip_underscores(int_str)?;
        let frac_clean = strip_underscores(frac_str)?;
        let int_clean = if int_clean.is_empty() {
            "0".to_string()
        } else {
            int_clean
        };
        if !int_clean.chars().all(|c| c.is_ascii_digit()) {
            return None;
        }
        if frac_clean.is_empty() || !frac_clean.chars().all(|c| c.is_ascii_digit()) {
            return None;
        }
        let int_val: i64 = int_clean.parse().ok()?;
        let frac_val: i64 = frac_clean.parse().ok()?;
        let denom = 10i64.checked_pow(frac_clean.len() as u32)?;
        let numer = int_val.checked_mul(denom)?.checked_add(frac_val)?;
        let numer = if part_sign < 0 { -numer } else { numer };
        Some((numer, denom))
    } else {
        // Plain integer
        let clean = strip_underscores(body)?;
        if clean.is_empty() || !clean.chars().all(|c| c.is_ascii_digit()) {
            return None;
        }
        let val: i64 = clean.parse().ok()?;
        let val = if part_sign < 0 { -val } else { val };
        Some((val, 1))
    }
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

    // Parse the full normalized string as f64 directly for best precision.
    // Computing mantissa * 10^exp compounds floating-point rounding errors.
    let mantissa_clean = mantissa_str.replace('_', "");
    let full_str = format!("{}e{}", mantissa_clean, exp);
    let result = if let Ok(f) = full_str.parse::<f64>() {
        if sign < 0 { -f } else { f }
    } else {
        // Fallback (should not normally happen)
        let r = mantissa * 10f64.powi(exp);
        if sign < 0 { -r } else { r }
    };
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

    // Allow empty integer part (e.g., ".13" → "0.13")
    let int_clean = if int_clean.is_empty() {
        "0".to_string()
    } else {
        int_clean
    };
    if !int_clean.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }
    if frac_clean.is_empty() || !frac_clean.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }

    // Construct as Rat: integer_part * 10^frac_len + frac_part / 10^frac_len
    // Use checked arithmetic to avoid overflow for large inputs; fall back to Num.
    if let (Ok(int_val), Ok(frac_val), Some(denom)) = (
        int_clean.parse::<i64>(),
        frac_clean.parse::<i64>(),
        10i64.checked_pow(frac_clean.len() as u32),
    ) && let Some(numer) = int_val
        .checked_mul(denom)
        .and_then(|v| v.checked_add(frac_val))
    {
        let numer = if sign < 0 { -numer } else { numer };
        return Some(crate::value::make_rat(numer, denom));
    }
    // Overflow or too many digits: fall back to f64 approximation
    let combined = format!("{}.{}", int_clean, frac_clean);
    let f: f64 = combined.parse().ok()?;
    let f = if sign < 0 { -f } else { f };
    Some(Value::Num(f))
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

/// Parse complex number strings like `1+2i`, `-1-2i`, `3+Inf\i`, `42i`, `42\i`, etc.
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

    if let Some(split) = find_complex_split(without_i) {
        // Has both real and imaginary parts: "42+34i", "-1-2i", etc.
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
    } else {
        // Pure imaginary: "42i", "-3.5i", "4_2i", "Inf\i", etc.
        let (imag_sign, imag_body) = strip_sign(without_i);
        let imag_val = if imag_body == "Inf" {
            if has_backslash_i {
                f64::INFINITY
            } else {
                return None;
            }
        } else if imag_body == "NaN" {
            if has_backslash_i {
                f64::NAN
            } else {
                return None;
            }
        } else {
            parse_real_component_to_f64(imag_body)?
        };
        let imag_val = if imag_sign < 0 { -imag_val } else { imag_val };
        Some(Value::Complex(0.0, imag_val))
    }
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
        // Parse the full normalized string for best precision.
        let mantissa_clean = mantissa_str.replace('_', "");
        let full_str = format!("{}e{}", mantissa_clean, exp);
        let result = if let Ok(f) = full_str.parse::<f64>() {
            f
        } else {
            mantissa * 10f64.powi(exp)
        };
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
