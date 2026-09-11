//! C99 hexadecimal-float rendering for the sprintf `%a` / `%A` directives.
//!
//! `%a` writes a `Num` as an exact binary value: a `0x` prefix, one hex digit
//! before the radix point, the mantissa in hex after it, then `p` and a
//! *decimal* signed binary exponent (`sprintf("%a", 1e0)` is `0x1p+0`). `%A`
//! is the same with uppercase hex digits and `0X`/`P`.
//!
//! Because an f64 mantissa is exactly 52 bits — 13 hex digits — the rendering
//! is exact whenever no precision is given; trailing zeroes are then omitted.
//! With a precision the mantissa is rounded to that many fractional hex digits,
//! ties-to-even, and a carry out of the leading digit is *not* renormalized
//! (C prints `0x2p+4`, not `0x1p+5`), matching glibc.

use super::sprintf_helpers::{format_inf_nan, sign_prefix};

/// Hex digits in an f64's 52-bit fraction field.
const FRACTION_HEX_DIGITS: usize = 13;

/// Render `f` for `%a` (`upper == false`) or `%A` (`upper == true`).
///
/// `prec` is the requested number of fractional hex digits; `None` means "as
/// many as the value needs", which is the exact representation with trailing
/// zeroes stripped. `hash_flag` forces the radix point even when the fraction
/// is empty.
pub(super) fn format_hexfloat(
    f: f64,
    prec: Option<usize>,
    plus_sign: bool,
    space_flag: bool,
    hash_flag: bool,
    upper: bool,
) -> String {
    if f.is_nan() || f.is_infinite() {
        // Raku spells the special values `Inf`/`-Inf`/`NaN` for every float
        // directive, and `%E` does not uppercase them; `%A` follows `%E`.
        return format_inf_nan(f, plus_sign, space_flag);
    }
    let bits = f.to_bits();
    let is_neg = (bits >> 63) != 0;
    let biased_exp = ((bits >> 52) & 0x7ff) as i32;
    let fraction = bits & ((1u64 << 52) - 1);
    // A normal number has an implicit leading 1 and an exponent of
    // `biased - 1023`. A subnormal has a leading 0 and, like C, is printed at
    // the fixed minimum exponent -1022 rather than being normalized. Zero is
    // printed as `0x0p+0`.
    let (leading_digit, exponent) = if biased_exp == 0 {
        (0u64, if fraction == 0 { 0 } else { -1022 })
    } else {
        (1u64, biased_exp - 1023)
    };

    let digits = mantissa_digits(leading_digit, fraction, prec);
    let (int_digit, frac_digits) = digits.split_at(1);
    let frac_digits = match prec {
        // Exact rendering: drop the trailing zeroes the 13-digit field pads with.
        None => frac_digits.trim_end_matches('0'),
        Some(_) => frac_digits,
    };

    let mut out = String::with_capacity(digits.len() + 8);
    out.push_str(sign_prefix(is_neg, plus_sign, space_flag));
    out.push_str(if upper { "0X" } else { "0x" });
    out.push_str(int_digit);
    if !frac_digits.is_empty() {
        out.push('.');
        out.push_str(frac_digits);
    } else if hash_flag {
        out.push('.');
    }
    if upper {
        out.make_ascii_uppercase();
        out.push('P');
    } else {
        out.push('p');
    }
    out.push(if exponent < 0 { '-' } else { '+' });
    out.push_str(&exponent.unsigned_abs().to_string());
    out
}

/// The mantissa as `1 + p` hex digits: the integer digit followed by exactly
/// `p` fractional digits (`p` defaults to the full 13-digit fraction field).
/// Rounding is half-to-even, and a carry into the integer digit is kept as-is
/// (`0x1.b...` at precision 0 becomes `2`, not a renormalized `1` at `exp + 1`).
fn mantissa_digits(leading_digit: u64, fraction: u64, prec: Option<usize>) -> String {
    let p = prec.unwrap_or(FRACTION_HEX_DIGITS);
    if p >= FRACTION_HEX_DIGITS {
        // No rounding needed: the whole fraction field fits, pad with zeroes.
        let mut s = format!("{leading_digit}{fraction:013x}");
        s.extend(std::iter::repeat_n('0', p - FRACTION_HEX_DIGITS));
        return s;
    }
    let shift = 4 * (FRACTION_HEX_DIGITS - p) as u32;
    let full = (leading_digit << 52) | fraction;
    let mut kept = full >> shift;
    let dropped = full & ((1u64 << shift) - 1);
    let half = 1u64 << (shift - 1);
    if dropped > half || (dropped == half && kept & 1 == 1) {
        kept += 1;
    }
    // `kept` needs at most `p + 1` hex digits even after a carry, because the
    // largest possible value is `0x2000...0`.
    format!("{kept:0width$x}", width = p + 1)
}

#[cfg(test)]
mod tests {
    use super::format_hexfloat;

    fn a(f: f64, prec: Option<usize>) -> String {
        format_hexfloat(f, prec, false, false, false, false)
    }

    #[test]
    fn exact_rendering_matches_c() {
        assert_eq!(a(0.0, None), "0x0p+0");
        assert_eq!(a(-0.0, None), "-0x0p+0");
        assert_eq!(a(1.0, None), "0x1p+0");
        assert_eq!(a(27.1, None), "0x1.b19999999999ap+4");
        assert_eq!(a(-2.71, None), "-0x1.5ae147ae147aep+1");
    }

    #[test]
    fn precision_rounds_ties_to_even_without_renormalizing() {
        assert_eq!(a(27.1, Some(0)), "0x2p+4");
        assert_eq!(a(-2.71, Some(0)), "-0x1p+1");
        assert_eq!(a(27.1, Some(3)), "0x1.b1ap+4");
        assert_eq!(a(-2.71, Some(3)), "-0x1.5aep+1");
        assert_eq!(a(0.0, Some(3)), "0x0.000p+0");
        assert_eq!(a(1.9999999, Some(1)), "0x2.0p+0");
        assert_eq!(a(27.1, Some(20)), "0x1.b19999999999a0000000p+4");
    }

    #[test]
    fn subnormals_use_the_minimum_exponent() {
        assert_eq!(a(5e-324, None), "0x0.0000000000001p-1022");
        assert_eq!(a(2.2250738585072014e-308, None), "0x1p-1022");
    }

    #[test]
    fn flags_and_case() {
        assert_eq!(
            format_hexfloat(0.0, None, false, false, true, false),
            "0x0.p+0"
        );
        assert_eq!(
            format_hexfloat(1.0, Some(0), false, false, true, false),
            "0x1.p+0"
        );
        assert_eq!(
            format_hexfloat(27.1, None, true, false, false, false),
            "+0x1.b19999999999ap+4"
        );
        assert_eq!(
            format_hexfloat(27.1, None, false, true, false, false),
            " 0x1.b19999999999ap+4"
        );
        assert_eq!(
            format_hexfloat(27.1, None, false, false, false, true),
            "0X1.B19999999999AP+4"
        );
    }

    #[test]
    fn inf_and_nan_use_raku_spelling() {
        assert_eq!(a(f64::INFINITY, None), "Inf");
        assert_eq!(a(f64::NEG_INFINITY, None), "-Inf");
        assert_eq!(a(f64::NAN, None), "NaN");
    }
}
