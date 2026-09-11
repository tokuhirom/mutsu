//! C99 hexadecimal floating-point literals (`0x1.8p+1`), per rakudo/rakudo#6524
//! and `roast/S02-literals/numeric.t`.
//!
//! The literal is `0x` / `0X`, a hex mantissa with an optional `.` fraction,
//! then a mandatory `p` / `P` binary exponent written in decimal. The exponent
//! is what makes it a float: without it, `0x1.abs` must keep parsing as a
//! method call on the hex integer `0x1`, and `0x1.8` must stay a syntax error.
//! So this scanner commits to the float reading only once it has seen the
//! `p`/`P` *and* its digits, and otherwise returns a non-fatal error so the
//! ordinary radix-integer path gets its turn.
//!
//! Rust's `f64` has no hexfloat parser, so the conversion is done here: the
//! mantissa digits become an exact `BigUint`, the fraction and the exponent
//! collapse into a single binary scale, and the exact value `m * 2^exp` is
//! rounded once to the nearest `f64` with ties to even. Doing it in one
//! rounding step is what makes the subnormal, overflow and underflow cases
//! come out right — a naive `mantissa * 2f64.powi(exp)` double-rounds.

use super::super::parse_result::{PError, PResult};
use crate::ast::Expr;
use crate::value::Value;

use num_bigint::BigUint;
use num_traits::{One, ToPrimitive, Zero};

/// Largest binary exponent worth tracking. Anything past this is unambiguously
/// an overflow to `Inf` or an underflow to zero, so clamping keeps the shift
/// arithmetic in range without changing any representable result.
const EXP_CLAMP: i128 = 1 << 40;

fn hex_digit_value(c: char) -> Option<u32> {
    c.to_digit(16)
}

/// Scan a run of hex digits, allowing a single `_` **between** two digits.
/// Returns the remaining input and the digits with the separators removed.
fn scan_hex_digits(input: &str) -> (&str, String) {
    let mut clean = String::new();
    let mut end = 0;
    let mut chars = input.char_indices().peekable();
    while let Some((i, c)) = chars.next() {
        if c == '_' {
            if clean.is_empty() {
                break;
            }
            let next_is_digit = chars
                .peek()
                .is_some_and(|(_, nc)| hex_digit_value(*nc).is_some());
            if !next_is_digit {
                break;
            }
            end = i + c.len_utf8();
            continue;
        }
        match hex_digit_value(c) {
            Some(_) => {
                clean.push(c);
                end = i + c.len_utf8();
            }
            None => break,
        }
    }
    (&input[end..], clean)
}

/// Scan the decimal exponent digits, allowing a single `_` between two digits.
fn scan_exponent_digits(input: &str) -> (&str, String) {
    let mut clean = String::new();
    let mut end = 0;
    let mut chars = input.char_indices().peekable();
    while let Some((i, c)) = chars.next() {
        if c == '_' {
            if clean.is_empty() {
                break;
            }
            let next_is_digit = chars.peek().is_some_and(|(_, nc)| nc.is_ascii_digit());
            if !next_is_digit {
                break;
            }
            end = i + c.len_utf8();
            continue;
        }
        if c.is_ascii_digit() {
            clean.push(c);
            end = i + c.len_utf8();
            continue;
        }
        break;
    }
    (&input[end..], clean)
}

/// Parse a C99 hexadecimal float literal, e.g. `0x1.8p+1`.
///
/// Returns a non-fatal `PError` whenever the input is not one, so that `0x1`,
/// `0x1.abs` and `0x1.8` all fall through to the radix-integer parser and keep
/// their existing meanings.
pub(super) fn hex_float(input: &str) -> PResult<'_, Expr> {
    // Deliberately the same expectation the integer parser reports, so that
    // merely trying a hexfloat first does not add a new alternative to the
    // "expected ... or ... or ..." list of every unrelated parse failure.
    let rest = input
        .strip_prefix("0x")
        .or_else(|| input.strip_prefix("0X"))
        .ok_or_else(|| PError::expected("digits"))?;

    let (rest, int_digits) = scan_hex_digits(rest);

    // A fraction is taken only when the `.` is actually followed by a hex
    // digit. `0x1.p3` therefore stays a method call, and so does `0x1.abs`
    // (which does scan a fraction `ab`, but then finds no `p` exponent and
    // rewinds wholesale by returning an error).
    let (rest, frac_digits) = match rest.strip_prefix('.') {
        Some(after_dot) if after_dot.starts_with(|c: char| hex_digit_value(c).is_some()) => {
            let (r, digits) = scan_hex_digits(after_dot);
            (r, digits)
        }
        _ => (rest, String::new()),
    };

    if int_digits.is_empty() && frac_digits.is_empty() {
        return Err(PError::expected("hexadecimal float mantissa"));
    }

    let after_p = rest
        .strip_prefix(['p', 'P'])
        .ok_or_else(|| PError::expected("binary exponent in hexadecimal float literal"))?;

    let (after_sign, negative_exp) = if let Some(r) = after_p.strip_prefix('+') {
        (r, false)
    } else if let Some(r) = after_p.strip_prefix('-') {
        (r, true)
    } else if let Some(r) = after_p.strip_prefix('\u{2212}') {
        // U+2212 MINUS SIGN, normalized to ASCII minus like the `e` exponent.
        (r, true)
    } else {
        (after_p, false)
    };

    let (rest, exp_digits) = scan_exponent_digits(after_sign);
    if exp_digits.is_empty() {
        return Err(PError::expected(
            "digits in the binary exponent of a hexadecimal float literal",
        ));
    }

    let magnitude: i128 = exp_digits
        .parse::<i128>()
        .unwrap_or(EXP_CLAMP)
        .min(EXP_CLAMP);
    let exponent = if negative_exp { -magnitude } else { magnitude };

    let mantissa_digits = format!("{}{}", int_digits, frac_digits);
    let mantissa =
        BigUint::parse_bytes(mantissa_digits.as_bytes(), 16).unwrap_or_else(BigUint::zero);
    // Each fraction digit is four binary places, folded into one scale.
    let scale = exponent - 4 * frac_digits.len() as i128;

    let value = scale_to_f64(&mantissa, scale);
    Ok((rest, Expr::Literal(Value::num(value))))
}

/// Round the exact value `mantissa * 2^exp` to the nearest `f64`, ties to even.
///
/// Correct for the whole range: normals, subnormals (where the quantum is
/// pinned at `2^-1074` rather than 53 significant bits), exponent overflow to
/// `Inf`, and exponent underflow to `0`.
fn scale_to_f64(mantissa: &BigUint, exp: i128) -> f64 {
    if mantissa.is_zero() {
        return 0.0;
    }
    let bits = mantissa.bits() as i128;
    // Binary exponent of the mantissa's leading one.
    let leading = bits - 1 + exp;
    if leading > 1023 {
        return f64::INFINITY;
    }
    // The value is rounded to a multiple of 2^quantum: 53 significant bits for
    // a normal, or the fixed 2^-1074 grid once it goes subnormal.
    let quantum = std::cmp::max(leading - 52, -1074);
    let shift = quantum - exp;

    let rounded: BigUint = if shift <= 0 {
        // `shift` is bounded below by `bits - 53 >= -52`, so this never
        // materializes a large number.
        mantissa << ((-shift) as usize)
    } else if shift > bits {
        // Strictly less than half a quantum: everything rounds away.
        BigUint::zero()
    } else {
        let s = shift as usize;
        let truncated = mantissa >> s;
        let half = BigUint::one() << (s - 1);
        let remainder = mantissa - (&truncated << s);
        match remainder.cmp(&half) {
            std::cmp::Ordering::Greater => truncated + BigUint::one(),
            std::cmp::Ordering::Less => truncated,
            std::cmp::Ordering::Equal => {
                if (&truncated & BigUint::one()).is_one() {
                    truncated + BigUint::one()
                } else {
                    truncated
                }
            }
        }
    };

    if rounded.is_zero() {
        return 0.0;
    }
    // After rounding to at most 53 bits (54 if a carry propagated) the
    // significand always fits a u64.
    let Some(significand) = rounded.to_u64() else {
        return f64::INFINITY;
    };
    let width = 64 - significand.leading_zeros() as i128;
    let leading = width - 1 + quantum;
    if leading > 1023 {
        return f64::INFINITY;
    }
    if leading < -1022 {
        // Subnormal: the quantum is 2^-1074, so the significand *is* the
        // fraction field.
        return f64::from_bits(significand);
    }
    // A carry out of the 53rd bit means the significand is exactly 2^53; drop
    // the (zero) low bit to renormalize, which leaves `leading` unchanged.
    let significand = if width == 54 {
        significand >> 1
    } else {
        significand
    };
    let fraction = significand & ((1u64 << 52) - 1);
    let biased = (leading + 1023) as u64;
    f64::from_bits((biased << 52) | fraction)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(src: &str) -> f64 {
        match hex_float(src) {
            Ok((rest, Expr::Literal(v))) => {
                assert_eq!(rest, "", "unconsumed input for {src}");
                match v.view() {
                    crate::value::ValueView::Num(n) => n,
                    other => panic!("{src} produced {other:?}, expected Num"),
                }
            }
            Ok((_, other)) => panic!("{src} produced {other:?}"),
            Err(e) => panic!("{src} failed to parse: {e:?}"),
        }
    }

    #[test]
    fn spec_table() {
        assert_eq!(parse("0x1.8p+1"), 3.0);
        assert_eq!(parse("0x1p-2"), 0.25);
        assert_eq!(parse("0x.8p+1"), 1.0);
        assert_eq!(parse("0x1.8P4"), 24.0);
        assert_eq!(parse("0x1.999999999999ap-4"), 0.1);
        assert_eq!(parse("0xde_ad.be_efp+0"), parse("0xdead.beefp0"));
        assert_eq!(parse("0x1p+1_0"), 1024.0);
        assert_eq!(parse("0x1.fffffffffffffp+1023"), f64::MAX);
        assert_eq!(parse("0x1p-1022"), f64::MIN_POSITIVE);
        assert_eq!(parse("0x1p-1074"), 5e-324);
        assert_eq!(parse("0x1.8p-1074"), 1e-323);
        assert_eq!(parse("0x1p+9999"), f64::INFINITY);
        assert_eq!(parse("0x1p-9999"), 0.0);
        assert!(parse("0x0p+0").is_sign_positive());
        assert_eq!(parse("0x0p+0"), 0.0);
    }

    #[test]
    fn ties_round_to_even() {
        // 2^-1074 * 1.5 sits exactly between two subnormals; the even one wins.
        assert_eq!(parse("0x1.8p-1074"), 2.0 * 5e-324);
        // 2^-1074 * 0.5 is exactly half the smallest subnormal: ties to even
        // means zero.
        assert_eq!(parse("0x0.8p-1074"), 0.0);
        // Just over half rounds up instead.
        assert_eq!(parse("0x0.81p-1074"), 5e-324);
    }

    #[test]
    fn not_a_hexfloat() {
        // No exponent at all: the integer parser must get these back.
        assert!(hex_float("0x1.abs").is_err());
        assert!(hex_float("0x1.8").is_err());
        assert!(hex_float("0x1").is_err());
        assert!(hex_float("0x1.pi").is_err());
        // `p` with no digits after it is not an exponent either.
        assert!(hex_float("0x1p").is_err());
        assert!(hex_float("0x1p+").is_err());
        // Not a hex literal to begin with.
        assert!(hex_float("42").is_err());
        assert!(hex_float("0b1p+1").is_err());
    }

    #[test]
    fn stops_at_the_end_of_the_literal() {
        let (rest, _) = hex_float("0x1p+1, 2").expect("parses");
        assert_eq!(rest, ", 2");
    }
}
