//! Rational-number and BigNum helper utilities used by arith ops.

use crate::value::{Value, ValueView, make_rat};
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
            Value::rat_raw(0, 0)
        } else if n > 0 {
            Value::rat_raw(1, 0)
        } else {
            Value::rat_raw(-1, 0)
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
        return Value::rat_raw(n64, d64);
    }
    // Overflow: degrade to Num
    Value::num(n as f64 / d as f64)
}

/// Check if BigRat arithmetic is needed: at least one operand requires BigInt precision
/// (BigRat or BigInt) AND at least one operand is rational (Rat/FatRat/BigRat).
/// Plain Rat-vs-Rat operations use the i128-based overflow-to-Num path instead.
pub(crate) fn needs_bigrat_path(l: &Value, r: &Value) -> bool {
    let has_big = matches!(l.view(), ValueView::BigRat(_, _) | ValueView::BigInt(_))
        || matches!(r.view(), ValueView::BigRat(_, _) | ValueView::BigInt(_));
    let has_rat = matches!(
        l.view(),
        ValueView::Rat(_, _) | ValueView::FatRat(_, _) | ValueView::BigRat(_, _)
    ) || matches!(
        r.view(),
        ValueView::Rat(_, _) | ValueView::FatRat(_, _) | ValueView::BigRat(_, _)
    );
    has_big && has_rat
}

/// Check if a value should be treated as FatRat for arithmetic.
/// This includes FatRat and BigRat values when the denominator exceeds u64,
/// since only FatRat operations can produce such values (regular Rat degrades to Num).
pub(crate) fn is_fat_rat_like(v: &Value) -> bool {
    match v.view() {
        ValueView::FatRat(_, _) => true,
        ValueView::BigRat(_, d) => d.to_u64().is_none(),
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
    match value.view() {
        ValueView::Int(i) => Some(NumBigInt::from(i)),
        ValueView::BigInt(i) => Some((**i).clone()),
        _ => None,
    }
}

pub(crate) fn to_big_rat_parts(value: &Value) -> Option<(NumBigInt, NumBigInt)> {
    match value.view() {
        ValueView::Int(i) => Some((NumBigInt::from(i), NumBigInt::from(1))),
        ValueView::BigInt(i) => Some(((**i).clone(), NumBigInt::from(1))),
        ValueView::Rat(n, d) | ValueView::FatRat(n, d) => {
            Some((NumBigInt::from(n), NumBigInt::from(d)))
        }
        ValueView::BigRat(n, d) => Some((n.clone(), d.clone())),
        _ => None,
    }
}

pub(crate) fn make_fat_rat(num: i64, den: i64) -> Value {
    let r = make_rat(num, den);
    if let ValueView::Rat(n, d) = r.view() {
        return Value::fat_rat_raw(n, d);
    }
    r
}

/// Coerce a Real value to a Rational, matching Rakudo's Duration which always
/// stores its seconds as a Rat. Integers become exact Rats; existing rationals
/// are kept; Inf/NaN map to the degenerate Rats `1/0`/`0/0`; floats use the
/// standard epsilon conversion.
pub(crate) fn real_to_rat(v: &Value) -> Value {
    match v.view() {
        ValueView::Rat(..) | ValueView::BigRat(..) | ValueView::FatRat(..) => v.clone(),
        ValueView::Int(i) => crate::value::make_rat(i, 1),
        ValueView::Bool(b) => crate::value::make_rat(b as i64, 1),
        // Preserve large integers exactly as a (big) Rat with denominator 1.
        ValueView::BigInt(b) => crate::value::make_big_rat_arith((**b).clone(), NumBigInt::from(1)),
        ValueView::Num(f) => {
            if f.is_nan() {
                Value::rat_raw(0, 0)
            } else if f.is_infinite() {
                if f > 0.0 {
                    Value::rat_raw(1, 0)
                } else {
                    Value::rat_raw(-1, 0)
                }
            } else {
                crate::builtins::num_to_rat_with_epsilon(f, 1e-6)
            }
        }
        // Anything else (e.g. Str, allomorphs): fall back through f64.
        _ => {
            let f = crate::runtime::to_float_value(v).unwrap_or(0.0);
            if f.is_nan() {
                Value::rat_raw(0, 0)
            } else if f.is_infinite() {
                if f > 0.0 {
                    Value::rat_raw(1, 0)
                } else {
                    Value::rat_raw(-1, 0)
                }
            } else {
                crate::builtins::num_to_rat_with_epsilon(f, 1e-6)
            }
        }
    }
}

/// Extract exact `(numerator, denominator)` i128 parts of a Real value, or `None`
/// when the value is inexact (Num/Complex) or too large to fit i128. Used to keep
/// `.round($scale)` exact when both operands are integers/rationals.
fn exact_rat_parts_i128(v: &ValueView<'_>) -> Option<(i128, i128)> {
    match v {
        ValueView::Int(i) => Some((*i as i128, 1)),
        ValueView::BigInt(bi) => bi.to_i128().map(|n| (n, 1)),
        ValueView::Rat(n, d) | ValueView::FatRat(n, d) if *d != 0 => Some((*n as i128, *d as i128)),
        ValueView::BigRat(n, d) => {
            let (n, d) = (n.to_i128()?, d.to_i128()?);
            (d != 0).then_some((n, d))
        }
        _ => None,
    }
}

/// Whether a Real value is an integer type (so `.round($scale)` yields an Int
/// rather than a Rat).
fn is_integer_scale(v: &ValueView<'_>) -> bool {
    matches!(v, ValueView::Int(_) | ValueView::BigInt(_))
}

fn value_from_i128(n: i128) -> Value {
    match i64::try_from(n) {
        Ok(i) => Value::int(i),
        Err(_) => Value::bigint(NumBigInt::from(n)),
    }
}

/// Exact `(self / $scale + 1/2).floor * $scale` per Rakudo's `Real.round(Real)`,
/// staying in exact Int/Rat arithmetic. Returns `None` when either operand is
/// inexact (Num/Complex), the scale is zero, or i128 overflow would occur — the
/// caller then falls back to the f64 path.
pub(crate) fn exact_round_scaled(target: &Value, scale: &Value) -> Option<Value> {
    // Fast path: exact i128 arithmetic. On any i128 overflow (large Ints, e.g.
    // `(17**1500).round(10)`), this yields `None` via the `checked_*` ops, and we
    // retry with the BigInt path below rather than degrading to a lossy f64
    // round (a huge BigInt numifies to `Inf`, so the scaled round returned `Inf`).
    if let Some(v) = exact_round_scaled_i128(target, scale) {
        return Some(v);
    }
    exact_round_scaled_bigint(target, scale)
}

fn exact_round_scaled_i128(target: &Value, scale: &Value) -> Option<Value> {
    let (tn, td) = exact_rat_parts_i128(&target.view())?;
    let (sn, sd) = exact_rat_parts_i128(&scale.view())?;
    if sn == 0 {
        return None;
    }
    // r = self/scale + 1/2 = (2*tn*sd + td*sn) / (2*td*sn)
    let num = tn
        .checked_mul(sd)?
        .checked_mul(2)?
        .checked_add(td.checked_mul(sn)?)?;
    let mut den = td.checked_mul(sn)?.checked_mul(2)?;
    let mut num = num;
    if den < 0 {
        num = num.checked_neg()?;
        den = den.checked_neg()?;
    }
    // floor(num/den) with den > 0
    let floor_q = num.div_euclid(den);
    // result = floor_q * scale = floor_q * sn / sd
    let rn = floor_q.checked_mul(sn)?;
    if is_integer_scale(&scale.view()) {
        // sd == 1 for an integer scale, so the result is an exact integer.
        Some(value_from_i128(rn.checked_div(sd)?))
    } else {
        Some(rat_from_i128_or_num(rn, sd))
    }
}

/// BigInt-precision `round($scale)` for rational targets/scales that overflow
/// the i128 fast path. Mirrors `exact_round_scaled_i128` exactly, using
/// arbitrary-precision arithmetic so `(17**1500).round(10)` stays exact.
fn exact_round_scaled_bigint(target: &Value, scale: &Value) -> Option<Value> {
    use num_integer::Integer;
    let (tn, td) = to_big_rat_parts(target)?;
    let (sn, sd) = to_big_rat_parts(scale)?;
    if sn.is_zero() {
        return None;
    }
    let two = NumBigInt::from(2);
    // r = self/scale + 1/2 = (2*tn*sd + td*sn) / (2*td*sn)
    let mut num = &two * &tn * &sd + &td * &sn;
    let mut den = &td * &sn * &two;
    if den.is_negative() {
        num = -num;
        den = -den;
    }
    // floor(num/den) with den > 0
    let floor_q = num.div_floor(&den);
    // result = floor_q * scale = floor_q * sn / sd
    let rn = floor_q * &sn;
    if is_integer_scale(&scale.view()) {
        // sd == 1 for an integer scale, so the result is an exact integer.
        Some(Value::from_bigint(rn / &sd))
    } else {
        Some(crate::value::make_big_rat_arith(rn, sd))
    }
}
