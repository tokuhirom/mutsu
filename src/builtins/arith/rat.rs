#![allow(clippy::result_large_err)]
//! Rational-number and BigNum helper utilities used by arith ops.

use crate::value::{Value, make_rat};
use num_bigint::BigInt as NumBigInt;
use num_traits::{Signed, ToPrimitive, Zero};

/// Perform Rat addition with overflow detection.
/// Returns Num if the result's denominator would exceed i64 range.
pub(crate) fn rat_add_checked(an: i64, ad: i64, bn: i64, bd: i64) -> Value {
    // Use i128 to detect overflow
    let n = an as i128 * bd as i128 + bn as i128 * ad as i128;
    let d = ad as i128 * bd as i128;
    rat_from_i128_or_num(n, d)
}

/// Perform Rat subtraction with overflow detection.
pub(crate) fn rat_sub_checked(an: i64, ad: i64, bn: i64, bd: i64) -> Value {
    let n = an as i128 * bd as i128 - bn as i128 * ad as i128;
    let d = ad as i128 * bd as i128;
    rat_from_i128_or_num(n, d)
}

/// Perform Rat multiplication with overflow detection.
pub(crate) fn rat_mul_checked(an: i64, ad: i64, bn: i64, bd: i64) -> Value {
    let n = an as i128 * bn as i128;
    let d = ad as i128 * bd as i128;
    rat_from_i128_or_num(n, d)
}

/// Perform Rat division with overflow detection.
pub(crate) fn rat_div_checked(an: i64, ad: i64, bn: i64, bd: i64) -> Value {
    let n = an as i128 * bd as i128;
    let d = ad as i128 * bn as i128;
    rat_from_i128_or_num(n, d)
}

/// Convert i128 numerator/denominator to Rat, or Num on overflow.
/// Raku spec: Rat denominators are limited to uint64 range.
/// When the denominator exceeds this after GCD reduction, degrade to Num.
pub(crate) fn rat_from_i128_or_num(n: i128, d: i128) -> Value {
    if d == 0 {
        return if n == 0 {
            Value::Rat(0, 0)
        } else if n > 0 {
            Value::Rat(1, 0)
        } else {
            Value::Rat(-1, 0)
        };
    }
    // Normalize sign
    let (mut n, mut d) = if d < 0 { (-n, -d) } else { (n, d) };
    // GCD
    fn gcd128(mut a: i128, mut b: i128) -> i128 {
        a = a.abs();
        b = b.abs();
        while b != 0 {
            let t = b;
            b = a % b;
            a = t;
        }
        a
    }
    let g = gcd128(n, d);
    if g != 0 {
        n /= g;
        d /= g;
    }
    // Check if result fits in i64 (denominator must also fit in uint64 per Raku spec)
    if let (Ok(n64), Ok(d64)) = (i64::try_from(n), i64::try_from(d)) {
        return Value::Rat(n64, d64);
    }
    // Overflow: degrade to Num
    Value::Num(n as f64 / d as f64)
}

/// Check if BigRat arithmetic is needed: at least one operand requires BigInt precision
/// (BigRat or BigInt) AND at least one operand is rational (Rat/FatRat/BigRat).
/// Plain Rat-vs-Rat operations use the i128-based overflow-to-Num path instead.
pub(crate) fn needs_bigrat_path(l: &Value, r: &Value) -> bool {
    let has_big = matches!(l, Value::BigRat(_, _) | Value::BigInt(_))
        || matches!(r, Value::BigRat(_, _) | Value::BigInt(_));
    let has_rat = matches!(
        l,
        Value::Rat(_, _) | Value::FatRat(_, _) | Value::BigRat(_, _)
    ) || matches!(
        r,
        Value::Rat(_, _) | Value::FatRat(_, _) | Value::BigRat(_, _)
    );
    has_big && has_rat
}

/// Check if a value should be treated as FatRat for arithmetic.
/// This includes Value::FatRat and Value::BigRat when the denominator exceeds u64,
/// since only FatRat operations can produce such values (regular Rat degrades to Num).
pub(crate) fn is_fat_rat_like(v: &Value) -> bool {
    match v {
        Value::FatRat(_, _) => true,
        Value::BigRat(_, d) => d.to_u64().is_none(),
        _ => false,
    }
}

/// Safely convert a BigInt ratio to f64, handling cases where both
/// numerator and denominator are too large for f64 individually.
pub(crate) fn bigint_ratio_to_f64(n: &NumBigInt, d: &NumBigInt) -> f64 {
    if d.is_zero() {
        if n.is_zero() {
            return f64::NAN;
        }
        return if n.is_negative() {
            f64::NEG_INFINITY
        } else {
            f64::INFINITY
        };
    }
    // Try direct conversion first
    if let (Some(nf), Some(df)) = (n.to_f64(), d.to_f64())
        && nf.is_finite()
        && df.is_finite()
        && df != 0.0
    {
        return nf / df;
    }
    // For very large numbers, use scaled division
    let sign = n.is_negative() ^ d.is_negative();
    let na = n.abs();
    let da = d.abs();
    // Scale to get enough precision
    let scaled = &na * NumBigInt::from(10u64).pow(20);
    let div = &scaled / &da;
    let val = div.to_f64().unwrap_or(0.0) / 1e20;
    if sign { -val } else { val }
}

/// Floored modulo for f64: result has the same sign as the divisor.
pub(crate) fn float_mod_floor(a: f64, b: f64) -> f64 {
    let r = a % b;
    if (r > 0.0 && b < 0.0) || (r < 0.0 && b > 0.0) {
        r + b
    } else {
        r
    }
}

pub(crate) fn as_bigint(value: &Value) -> Option<NumBigInt> {
    match value {
        Value::Int(i) => Some(NumBigInt::from(*i)),
        Value::BigInt(i) => Some((**i).clone()),
        _ => None,
    }
}

pub(crate) fn to_big_rat_parts(value: &Value) -> Option<(NumBigInt, NumBigInt)> {
    match value {
        Value::Int(i) => Some((NumBigInt::from(*i), NumBigInt::from(1))),
        Value::BigInt(i) => Some(((**i).clone(), NumBigInt::from(1))),
        Value::Rat(n, d) | Value::FatRat(n, d) => Some((NumBigInt::from(*n), NumBigInt::from(*d))),
        Value::BigRat(n, d) => Some(((**n).clone(), (**d).clone())),
        _ => None,
    }
}

pub(crate) fn make_fat_rat(num: i64, den: i64) -> Value {
    match make_rat(num, den) {
        Value::Rat(n, d) => Value::FatRat(n, d),
        other => other,
    }
}

/// Coerce a Real value to a Rational, matching Rakudo's Duration which always
/// stores its seconds as a Rat. Integers become exact Rats; existing rationals
/// are kept; Inf/NaN map to the degenerate Rats `1/0`/`0/0`; floats use the
/// standard epsilon conversion.
pub(crate) fn real_to_rat(v: &Value) -> Value {
    match v {
        Value::Rat(..) | Value::BigRat(..) | Value::FatRat(..) => v.clone(),
        Value::Int(i) => crate::value::make_rat(*i, 1),
        Value::Bool(b) => crate::value::make_rat(*b as i64, 1),
        // Preserve large integers exactly as a (big) Rat with denominator 1.
        Value::BigInt(b) => crate::value::make_big_rat_arith((**b).clone(), NumBigInt::from(1)),
        Value::Num(f) => {
            if f.is_nan() {
                Value::Rat(0, 0)
            } else if f.is_infinite() {
                if *f > 0.0 {
                    Value::Rat(1, 0)
                } else {
                    Value::Rat(-1, 0)
                }
            } else {
                crate::builtins::num_to_rat_with_epsilon(*f, 1e-6)
            }
        }
        // Anything else (e.g. Str, allomorphs): fall back through f64.
        _ => {
            let f = crate::runtime::to_float_value(v).unwrap_or(0.0);
            if f.is_nan() {
                Value::Rat(0, 0)
            } else if f.is_infinite() {
                if f > 0.0 {
                    Value::Rat(1, 0)
                } else {
                    Value::Rat(-1, 0)
                }
            } else {
                crate::builtins::num_to_rat_with_epsilon(f, 1e-6)
            }
        }
    }
}
