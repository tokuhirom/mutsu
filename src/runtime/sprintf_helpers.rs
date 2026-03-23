use crate::value::Value;
use num_bigint::BigInt;

/// Format a float value for %f/%F specifiers.
pub(super) fn format_float_fixed(f: f64, p: usize, plus_sign: bool, space_flag: bool) -> String {
    if f.is_infinite() || f.is_nan() {
        format_inf_nan(f, plus_sign, space_flag)
    } else {
        let is_neg = f.is_sign_negative() && (f != 0.0 || f.is_sign_negative());
        let prefix = sign_prefix(is_neg, plus_sign, space_flag);
        let abs = f.abs();
        format!("{}{:.*}", prefix, p, abs)
    }
}

/// Format a Rat/FatRat value for %f with exact precision (avoids f64 rounding).
/// Returns None if the argument is not a rational type.
pub(super) fn format_rat_fixed(
    arg: Option<&Value>,
    p: usize,
    plus_sign: bool,
    space_flag: bool,
) -> Option<String> {
    let (numer, denom) = match arg {
        Some(Value::Rat(n, d)) if *d != 0 => (BigInt::from(*n), BigInt::from(*d)),
        Some(Value::FatRat(n, d)) if *d != 0 => (BigInt::from(*n), BigInt::from(*d)),
        Some(Value::BigRat(n, d)) if *d != num_bigint::BigInt::from(0) => (n.clone(), d.clone()),
        _ => return None,
    };
    let is_neg = numer < BigInt::from(0);
    let prefix = sign_prefix(is_neg, plus_sign, space_flag);
    let abs_numer = if is_neg { -&numer } else { numer };
    // Compute integer and fractional parts using BigInt arithmetic
    let int_part = &abs_numer / &denom;
    let remainder = &abs_numer % &denom;
    if p == 0 {
        // Round to integer
        let doubled = &remainder * BigInt::from(2);
        let rounded = if doubled >= denom {
            &int_part + BigInt::from(1)
        } else {
            int_part
        };
        return Some(format!("{}{}", prefix, rounded));
    }
    // Compute p decimal digits of the fractional part
    let scale = num_traits::pow::pow(BigInt::from(10), p);
    let scaled_remainder = &remainder * &scale;
    // Divide and round
    let frac_digits = &scaled_remainder / &denom;
    let frac_remainder = &scaled_remainder % &denom;
    let doubled_frac = &frac_remainder * BigInt::from(2);
    let (final_int, final_frac) = if doubled_frac >= denom {
        let rounded_frac = &frac_digits + BigInt::from(1);
        if rounded_frac >= scale {
            (&int_part + BigInt::from(1), BigInt::from(0))
        } else {
            (int_part, rounded_frac)
        }
    } else {
        (int_part, frac_digits)
    };
    let frac_str = format!("{:0>width$}", final_frac, width = p);
    Some(format!("{}{}.{}", prefix, final_int, frac_str))
}

/// Format a non-negative float using %g/%G rules:
/// - Use %e/%E if exponent < -4 or >= precision
/// - Otherwise use %f
/// - Precision specifies total significant digits
/// - Trailing zeros are removed (unless # flag is set)
pub(super) fn format_g(abs: f64, prec: usize, upper: bool, hash_flag: bool) -> String {
    // Determine exponent
    let exp = if abs == 0.0 {
        0i32
    } else {
        abs.log10().floor() as i32
    };
    let use_sci = exp < -4 || exp >= prec as i32;
    if use_sci {
        // Scientific notation with (prec-1) decimal digits
        let decimal_digits = prec.saturating_sub(1);
        let raw = format!("{:.*e}", decimal_digits, abs);
        let normalized = normalize_sci_exponent(&raw);
        // Replace 'e' with correct case
        let normalized = if upper {
            normalized.replacen('e', "E", 1)
        } else {
            normalized
        };
        if hash_flag {
            normalized
        } else {
            strip_trailing_zeros_sci(&normalized)
        }
    } else {
        // Fixed notation with (prec - 1 - exp) decimal places
        let decimal_digits = if prec as i32 > exp + 1 {
            (prec as i32 - exp - 1) as usize
        } else {
            0
        };
        let raw = format!("{:.*}", decimal_digits, abs);
        if hash_flag {
            raw
        } else {
            strip_trailing_zeros_fixed(&raw)
        }
    }
}

/// Strip trailing zeros after decimal point in fixed notation.
/// "3.1400" -> "3.14", "3.0" -> "3", "100" -> "100"
fn strip_trailing_zeros_fixed(s: &str) -> String {
    if s.contains('.') {
        let trimmed = s.trim_end_matches('0');
        if let Some(without_dot) = trimmed.strip_suffix('.') {
            without_dot.to_string()
        } else {
            trimmed.to_string()
        }
    } else {
        s.to_string()
    }
}

/// Strip trailing zeros in scientific notation mantissa.
/// "3.1400e+02" -> "3.14e+02", "3.0000e+02" -> "3e+02"
fn strip_trailing_zeros_sci(s: &str) -> String {
    let e_pos = s.rfind('e').or_else(|| s.rfind('E'));
    if let Some(pos) = e_pos {
        let mantissa = &s[..pos];
        let exp_part = &s[pos..];
        if mantissa.contains('.') {
            let trimmed = mantissa.trim_end_matches('0');
            if let Some(without_dot) = trimmed.strip_suffix('.') {
                format!("{}{}", without_dot, exp_part)
            } else {
                format!("{}{}", trimmed, exp_part)
            }
        } else {
            s.to_string()
        }
    } else {
        s.to_string()
    }
}

/// Format Inf/NaN values consistently across all format specifiers.
/// Raku always uses "Inf", "-Inf", "NaN" regardless of %e/%E/%f/%g/%G.
pub(super) fn format_inf_nan(f: f64, plus_sign: bool, space_flag: bool) -> String {
    if f.is_nan() {
        "NaN".to_string()
    } else {
        let is_neg = f.is_sign_negative();
        let prefix = if is_neg {
            "-"
        } else if plus_sign {
            "+"
        } else if space_flag {
            " "
        } else {
            ""
        };
        format!("{}Inf", prefix)
    }
}

/// Returns the sign prefix for a numeric value.
pub(super) fn sign_prefix(is_neg: bool, plus_sign: bool, space_flag: bool) -> &'static str {
    if is_neg {
        "-"
    } else if plus_sign {
        "+"
    } else if space_flag {
        " "
    } else {
        ""
    }
}

/// Apply width formatting with proper zero-padding.
pub(super) fn apply_width(
    out: &mut String,
    rendered: &str,
    width_num: usize,
    left_align: bool,
    zero_pad: bool,
    plain_zero: bool,
) {
    let rendered_width = rendered.chars().count();
    if width_num > rendered_width {
        let pad_len = width_num - rendered_width;
        if left_align {
            out.push_str(rendered);
            for _ in 0..pad_len {
                out.push(' ');
            }
        } else if zero_pad {
            if plain_zero {
                for _ in 0..pad_len {
                    out.push('0');
                }
                out.push_str(rendered);
            } else {
                let prefix_len = zero_pad_prefix_len(rendered);
                if prefix_len > 0 {
                    out.push_str(&rendered[..prefix_len]);
                    for _ in 0..pad_len {
                        out.push('0');
                    }
                    out.push_str(&rendered[prefix_len..]);
                } else {
                    for _ in 0..pad_len {
                        out.push('0');
                    }
                    out.push_str(rendered);
                }
            }
        } else {
            for _ in 0..pad_len {
                out.push(' ');
            }
            out.push_str(rendered);
        }
    } else {
        out.push_str(rendered);
    }
}

/// Determine the length of the prefix (sign + base indicator) that should
/// appear before zero-padding.
fn zero_pad_prefix_len(s: &str) -> usize {
    let bytes = s.as_bytes();
    let mut pos = 0;
    if matches!(bytes.first(), Some(b'+' | b'-' | b' ')) {
        pos = 1;
    }
    if pos + 1 < bytes.len() && bytes[pos] == b'0' {
        let next = bytes[pos + 1];
        if next == b'b'
            || next == b'B'
            || next == b'x'
            || next == b'X'
            || next == b'o'
            || next == b'O'
        {
            pos += 2;
        }
    }
    pos
}

/// Handle Raku's quirky behavior when both '-' and '0' flags are present on float specifiers.
pub(super) fn apply_float_minus_zero(out: &mut String, rendered: &str, width: usize) {
    let len = rendered.chars().count();
    if width <= len {
        out.push_str(rendered);
        return;
    }
    let pad_len = width - len;
    let has_sign = matches!(
        rendered.as_bytes().first(),
        Some(b'+') | Some(b'-') | Some(b' ')
    );

    let mut padded = String::with_capacity(width);
    if has_sign {
        padded.push(rendered.chars().next().unwrap());
        for _ in 0..pad_len {
            padded.push('0');
        }
        padded.push_str(&rendered[1..]);
    } else {
        for _ in 0..pad_len {
            padded.push('0');
        }
        padded.push_str(rendered);
    }

    if !has_sign && should_shift_zero(&padded) {
        let mut shifted = String::with_capacity(width);
        shifted.push_str(&padded[1..]);
        shifted.push('0');
        out.push_str(&shifted);
        return;
    }

    out.push_str(&padded);
}

/// Check if a zero-padded float string has a leading zero that can be shifted.
fn should_shift_zero(padded: &str) -> bool {
    if let Some(dot_pos) = padded.find('.') {
        let int_part = &padded[..dot_pos];
        int_part.len() >= 2 && int_part.starts_with('0')
    } else {
        false
    }
}

/// Normalize Rust scientific notation to C-style.
pub(super) fn normalize_sci_exponent(s: &str) -> String {
    let e_marker = if s.contains('E') { 'E' } else { 'e' };
    if let Some(pos) = s.rfind(e_marker) {
        let (mantissa, exp_part) = s.split_at(pos);
        let exp_str = &exp_part[1..];
        let (sign, digits) = if let Some(d) = exp_str.strip_prefix('-') {
            ("-", d)
        } else if let Some(d) = exp_str.strip_prefix('+') {
            ("+", d)
        } else {
            ("+", exp_str)
        };
        let exp_num: i32 = digits.parse().unwrap_or(0);
        format!(
            "{}{}{}{:02}",
            mantissa,
            e_marker,
            sign,
            exp_num.unsigned_abs()
        )
    } else {
        s.to_string()
    }
}
