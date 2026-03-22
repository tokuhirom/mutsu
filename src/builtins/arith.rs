#![allow(clippy::result_large_err)]

use std::sync::Arc;

use crate::runtime;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, make_big_rat, make_rat};
use num_bigint::{BigInt as NumBigInt, Sign};
use num_traits::{Signed, ToPrimitive, Zero};

/// Perform Rat addition with overflow detection.
/// Returns Num if the result's denominator would exceed i64 range.
fn rat_add_checked(an: i64, ad: i64, bn: i64, bd: i64) -> Value {
    // Use i128 to detect overflow
    let n = an as i128 * bd as i128 + bn as i128 * ad as i128;
    let d = ad as i128 * bd as i128;
    rat_from_i128_or_num(n, d)
}

/// Perform Rat subtraction with overflow detection.
fn rat_sub_checked(an: i64, ad: i64, bn: i64, bd: i64) -> Value {
    let n = an as i128 * bd as i128 - bn as i128 * ad as i128;
    let d = ad as i128 * bd as i128;
    rat_from_i128_or_num(n, d)
}

/// Perform Rat multiplication with overflow detection.
fn rat_mul_checked(an: i64, ad: i64, bn: i64, bd: i64) -> Value {
    let n = an as i128 * bn as i128;
    let d = ad as i128 * bd as i128;
    rat_from_i128_or_num(n, d)
}

/// Perform Rat division with overflow detection.
fn rat_div_checked(an: i64, ad: i64, bn: i64, bd: i64) -> Value {
    let n = an as i128 * bd as i128;
    let d = ad as i128 * bn as i128;
    rat_from_i128_or_num(n, d)
}

/// Convert i128 numerator/denominator to Rat, or Num on overflow.
/// Raku spec: Rat denominators are limited to uint64 range.
/// When the denominator exceeds this after GCD reduction, degrade to Num.
fn rat_from_i128_or_num(n: i128, d: i128) -> Value {
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
fn needs_bigrat_path(l: &Value, r: &Value) -> bool {
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

/// Safely convert a BigInt ratio to f64, handling cases where both
/// numerator and denominator are too large for f64 individually.
fn bigint_ratio_to_f64(n: &NumBigInt, d: &NumBigInt) -> f64 {
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
fn float_mod_floor(a: f64, b: f64) -> f64 {
    let r = a % b;
    if (r > 0.0 && b < 0.0) || (r < 0.0 && b > 0.0) {
        r + b
    } else {
        r
    }
}

/// Check if a value is a Date, Instant, or Duration instance (temporal operand for arithmetic).
pub fn is_temporal_operand(value: &Value) -> bool {
    matches!(value, Value::Instance { class_name, .. }
        if class_name == "Date" || class_name == "Instant" || class_name == "Duration")
}

fn as_bigint(value: &Value) -> Option<NumBigInt> {
    match value {
        Value::Int(i) => Some(NumBigInt::from(*i)),
        Value::BigInt(i) => Some((**i).clone()),
        _ => None,
    }
}

fn to_big_rat_parts(value: &Value) -> Option<(NumBigInt, NumBigInt)> {
    match value {
        Value::Int(i) => Some((NumBigInt::from(*i), NumBigInt::from(1))),
        Value::BigInt(i) => Some(((**i).clone(), NumBigInt::from(1))),
        Value::Rat(n, d) | Value::FatRat(n, d) => Some((NumBigInt::from(*n), NumBigInt::from(*d))),
        Value::BigRat(n, d) => Some((n.clone(), d.clone())),
        _ => None,
    }
}

fn instance_days(value: &Value) -> Option<i64> {
    match value {
        Value::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Date" => match attributes.get("days") {
            Some(Value::Int(days)) => Some(*days),
            _ => None,
        },
        _ => None,
    }
}

fn instance_instant_value(value: &Value) -> Option<f64> {
    match value {
        Value::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Instant" => attributes.get("value").and_then(runtime::to_float_value),
        _ => None,
    }
}

fn instance_duration_value(value: &Value) -> Option<f64> {
    match value {
        Value::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Duration" => attributes.get("value").and_then(runtime::to_float_value),
        _ => None,
    }
}

fn instance_datetime_parts(value: &Value) -> Option<(i64, i64, i64, i64, i64, f64, i64)> {
    match value {
        Value::Instance { attributes, .. }
            if attributes.contains_key("year")
                && attributes.contains_key("month")
                && attributes.contains_key("day")
                && attributes.contains_key("hour")
                && attributes.contains_key("minute")
                && attributes.contains_key("second")
                && attributes.contains_key("timezone") =>
        {
            Some(crate::builtins::methods_0arg::temporal::datetime_attrs(
                attributes,
            ))
        }
        _ => None,
    }
}

pub fn make_duration_value(secs: f64) -> Value {
    make_duration(secs)
}

fn make_duration(secs: f64) -> Value {
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("value".to_string(), Value::Num(secs));
    Value::make_instance(Symbol::intern("Duration"), attrs)
}

fn make_fat_rat(num: i64, den: i64) -> Value {
    match make_rat(num, den) {
        Value::Rat(n, d) => Value::FatRat(n, d),
        other => other,
    }
}

// ── Arithmetic operators ─────────────────────────────────────────────
pub(crate) fn arith_add(left: Value, right: Value) -> Result<Value, RuntimeError> {
    // Range + Int: shift both bounds
    match (&left, &right) {
        (Value::Range(a, b), Value::Int(n)) => return Ok(Value::Range(a + n, b + n)),
        (Value::RangeExcl(a, b), Value::Int(n)) => return Ok(Value::RangeExcl(a + n, b + n)),
        (Value::RangeExclStart(a, b), Value::Int(n)) => {
            return Ok(Value::RangeExclStart(a + n, b + n));
        }
        (Value::RangeExclBoth(a, b), Value::Int(n)) => {
            return Ok(Value::RangeExclBoth(a + n, b + n));
        }
        (Value::Int(n), Value::Range(a, b)) => return Ok(Value::Range(a + n, b + n)),
        (Value::Int(n), Value::RangeExcl(a, b)) => return Ok(Value::RangeExcl(a + n, b + n)),
        (Value::Int(n), Value::RangeExclStart(a, b)) => {
            return Ok(Value::RangeExclStart(a + n, b + n));
        }
        (Value::Int(n), Value::RangeExclBoth(a, b)) => {
            return Ok(Value::RangeExclBoth(a + n, b + n));
        }
        _ => {}
    }
    // Date + Int: add days
    if let Some(days) = instance_days(&left)
        && let Value::Int(delta) = &right
    {
        use crate::builtins::methods_0arg::temporal;
        let new_days = days + delta;
        let (y, m, d) = temporal::epoch_days_to_civil(new_days);
        return Ok(temporal::make_date(y, m, d));
    }
    if let Some(days) = instance_days(&right)
        && let Value::Int(delta) = &left
    {
        use crate::builtins::methods_0arg::temporal;
        let new_days = days + delta;
        let (y, m, d) = temporal::epoch_days_to_civil(new_days);
        return Ok(temporal::make_date(y, m, d));
    }
    // Instant + Numeric => Instant (add to TAI value)
    if let Some(tai) = instance_instant_value(&left)
        && right.is_numeric()
    {
        let delta = runtime::to_float_value(&right).unwrap_or(0.0);
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("value".to_string(), Value::Num(tai + delta));
        return Ok(Value::make_instance(Symbol::intern("Instant"), attrs));
    }
    if let Some(tai) = instance_instant_value(&right)
        && left.is_numeric()
    {
        let delta = runtime::to_float_value(&left).unwrap_or(0.0);
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("value".to_string(), Value::Num(tai + delta));
        return Ok(Value::make_instance(Symbol::intern("Instant"), attrs));
    }
    // Duration + Numeric => Duration
    if let Some(dur) = instance_duration_value(&left)
        && right.is_numeric()
    {
        let delta = runtime::to_float_value(&right).unwrap_or(0.0);
        return Ok(make_duration(dur + delta));
    }
    if let Some(dur) = instance_duration_value(&right)
        && left.is_numeric()
    {
        let delta = runtime::to_float_value(&left).unwrap_or(0.0);
        return Ok(make_duration(dur + delta));
    }
    // DateTime + Duration => DateTime
    if let Some((y, m, d, h, mi, s, tz)) = instance_datetime_parts(&left)
        && let Some(delta) = instance_duration_value(&right)
    {
        use crate::builtins::methods_0arg::temporal;
        let instant = temporal::datetime_to_instant_leap_aware(y, m, d, h, mi, s, tz);
        let (ny, nm, nd, nh, nmi, ns) =
            temporal::instant_to_datetime_leap_aware(instant + delta, tz);
        return Ok(temporal::make_datetime(ny, nm, nd, nh, nmi, ns, tz));
    }
    if let Some(delta) = instance_duration_value(&left)
        && let Some((y, m, d, h, mi, s, tz)) = instance_datetime_parts(&right)
    {
        use crate::builtins::methods_0arg::temporal;
        let instant = temporal::datetime_to_instant_leap_aware(y, m, d, h, mi, s, tz);
        let (ny, nm, nd, nh, nmi, ns) =
            temporal::instant_to_datetime_leap_aware(instant + delta, tz);
        return Ok(temporal::make_datetime(ny, nm, nd, nh, nmi, ns, tz));
    }
    let (l, r) = runtime::coerce_numeric(left, right);
    Ok(arith_add_coerced(l, r))
}

fn arith_add_coerced(l: Value, r: Value) -> Value {
    if matches!(l, Value::Complex(_, _)) || matches!(r, Value::Complex(_, _)) {
        let (ar, ai) = runtime::to_complex_parts(&l).unwrap_or((0.0, 0.0));
        let (br, bi) = runtime::to_complex_parts(&r).unwrap_or((0.0, 0.0));
        Value::Complex(ar + br, ai + bi)
    } else if (matches!(l, Value::BigInt(_)) || matches!(r, Value::BigInt(_)))
        && let (Some(a), Some(b)) = (as_bigint(&l), as_bigint(&r))
    {
        Value::from_bigint(a + b)
    } else if let (Some((an, ad)), Some((bn, bd))) = (to_big_rat_parts(&l), to_big_rat_parts(&r))
        && needs_bigrat_path(&l, &r)
    {
        let has_fat_rat = matches!(l, Value::FatRat(_, _)) || matches!(r, Value::FatRat(_, _));
        match make_big_rat(an * bd.clone() + bn * ad.clone(), ad * bd) {
            Value::Rat(n, d) if has_fat_rat => Value::FatRat(n, d),
            other => other,
        }
    } else if let (Some((an, ad)), Some((bn, bd))) =
        (runtime::to_rat_parts(&l), runtime::to_rat_parts(&r))
    {
        let has_rat = matches!(l, Value::Rat(_, _) | Value::FatRat(_, _))
            || matches!(r, Value::Rat(_, _) | Value::FatRat(_, _));
        let has_fat_rat = matches!(l, Value::FatRat(_, _)) || matches!(r, Value::FatRat(_, _));
        if has_rat {
            if has_fat_rat {
                let n = an * bd + bn * ad;
                let d = ad * bd;
                make_fat_rat(n, d)
            } else {
                rat_add_checked(an, ad, bn, bd)
            }
        } else {
            match (l, r) {
                (Value::Int(a), Value::Int(b)) => match a.checked_add(b) {
                    Some(sum) => Value::Int(sum),
                    None => Value::from_bigint(
                        num_bigint::BigInt::from(a) + num_bigint::BigInt::from(b),
                    ),
                },
                (Value::Num(a), Value::Num(b)) => Value::Num(a + b),
                (Value::Int(a), Value::Num(b)) => Value::Num(a as f64 + b),
                (Value::Num(a), Value::Int(b)) => Value::Num(a + b as f64),
                _ => Value::Int(0),
            }
        }
    } else {
        let lf = runtime::to_float_value(&l);
        let rf = runtime::to_float_value(&r);
        if let (Some(a), Some(b)) = (lf, rf) {
            Value::Num(a + b)
        } else {
            match (l, r) {
                (Value::Int(a), Value::Int(b)) => match a.checked_add(b) {
                    Some(sum) => Value::Int(sum),
                    None => Value::from_bigint(
                        num_bigint::BigInt::from(a) + num_bigint::BigInt::from(b),
                    ),
                },
                (Value::Num(a), Value::Num(b)) => Value::Num(a + b),
                (Value::Int(a), Value::Num(b)) => Value::Num(a as f64 + b),
                (Value::Num(a), Value::Int(b)) => Value::Num(a + b as f64),
                _ => Value::Int(0),
            }
        }
    }
}

pub(crate) fn arith_sub(left: Value, right: Value) -> Value {
    if let (Some(a), Some(b)) = (
        instance_instant_value(&left),
        instance_instant_value(&right),
    ) {
        // Instant - Instant returns a Duration
        return make_duration(a - b);
    }
    if let Some(a) = instance_instant_value(&left)
        && right.is_numeric()
    {
        let delta = runtime::to_float_value(&right).unwrap_or(0.0);
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("value".to_string(), Value::Num(a - delta));
        return Value::make_instance(Symbol::intern("Instant"), attrs);
    }
    // Duration - Duration returns Duration
    if let (Some(a), Some(b)) = (
        instance_duration_value(&left),
        instance_duration_value(&right),
    ) {
        return make_duration(a - b);
    }
    // Duration - Numeric returns Duration
    if let Some(a) = instance_duration_value(&left)
        && right.is_numeric()
    {
        let delta = runtime::to_float_value(&right).unwrap_or(0.0);
        return make_duration(a - delta);
    }
    // DateTime - DateTime => Duration
    if let (Some((ly, lm, ld, lh, lmin, ls, ltz)), Some((ry, rm, rd, rh, rmin, rs, rtz))) = (
        instance_datetime_parts(&left),
        instance_datetime_parts(&right),
    ) {
        use crate::builtins::methods_0arg::temporal;
        let left_instant = temporal::datetime_to_instant_leap_aware(ly, lm, ld, lh, lmin, ls, ltz);
        let right_instant = temporal::datetime_to_instant_leap_aware(ry, rm, rd, rh, rmin, rs, rtz);
        let secs = ((left_instant - right_instant) * 1_000_000.0).round() / 1_000_000.0;
        return make_duration(secs);
    }
    // DateTime - Duration => DateTime
    if let Some((y, m, d, h, mi, s, tz)) = instance_datetime_parts(&left)
        && let Some(delta) = instance_duration_value(&right)
    {
        use crate::builtins::methods_0arg::temporal;
        let instant = temporal::datetime_to_instant_leap_aware(y, m, d, h, mi, s, tz);
        let (ny, nm, nd, nh, nmi, ns) =
            temporal::instant_to_datetime_leap_aware(instant - delta, tz);
        return temporal::make_datetime(ny, nm, nd, nh, nmi, ns, tz);
    }
    if let (Some(a), Some(b)) = (instance_days(&left), instance_days(&right)) {
        return Value::Int(a - b);
    }
    if let Some(days) = instance_days(&left)
        && let Value::Int(delta) = right
    {
        use crate::builtins::methods_0arg::temporal;
        let new_days = days - delta;
        let (y, m, d) = temporal::epoch_days_to_civil(new_days);
        return temporal::make_date(y, m, d);
    }
    match (&left, &right) {
        (Value::Range(a, b), Value::Int(n)) => return Value::Range(a - n, b - n),
        (Value::RangeExcl(a, b), Value::Int(n)) => return Value::RangeExcl(a - n, b - n),
        (Value::RangeExclStart(a, b), Value::Int(n)) => return Value::RangeExclStart(a - n, b - n),
        (Value::RangeExclBoth(a, b), Value::Int(n)) => return Value::RangeExclBoth(a - n, b - n),
        (Value::Range(a, b), rhs)
        | (Value::RangeExcl(a, b), rhs)
        | (Value::RangeExclStart(a, b), rhs)
        | (Value::RangeExclBoth(a, b), rhs)
            if rhs.is_numeric() =>
        {
            let excl_start = matches!(&left, Value::RangeExclStart(..) | Value::RangeExclBoth(..));
            let excl_end = matches!(&left, Value::RangeExcl(..) | Value::RangeExclBoth(..));
            return Value::GenericRange {
                start: Arc::new(arith_sub(Value::Int(*a), rhs.clone())),
                end: Arc::new(arith_sub(Value::Int(*b), rhs.clone())),
                excl_start,
                excl_end,
            };
        }
        _ => {}
    }
    let (l, r) = runtime::coerce_numeric(left, right);
    if matches!(l, Value::Complex(_, _)) || matches!(r, Value::Complex(_, _)) {
        let (ar, ai) = runtime::to_complex_parts(&l).unwrap_or((0.0, 0.0));
        let (br, bi) = runtime::to_complex_parts(&r).unwrap_or((0.0, 0.0));
        Value::Complex(ar - br, ai - bi)
    } else if (matches!(l, Value::BigInt(_)) || matches!(r, Value::BigInt(_)))
        && let (Some(a), Some(b)) = (as_bigint(&l), as_bigint(&r))
    {
        Value::from_bigint(a - b)
    } else if let (Some((an, ad)), Some((bn, bd))) = (to_big_rat_parts(&l), to_big_rat_parts(&r))
        && needs_bigrat_path(&l, &r)
    {
        let has_fat_rat = matches!(l, Value::FatRat(_, _)) || matches!(r, Value::FatRat(_, _));
        match make_big_rat(an * bd.clone() - bn * ad.clone(), ad * bd) {
            Value::Rat(n, d) if has_fat_rat => Value::FatRat(n, d),
            other => other,
        }
    } else if let (Some((an, ad)), Some((bn, bd))) =
        (runtime::to_rat_parts(&l), runtime::to_rat_parts(&r))
    {
        let has_rat = matches!(l, Value::Rat(_, _) | Value::FatRat(_, _))
            || matches!(r, Value::Rat(_, _) | Value::FatRat(_, _));
        let has_fat_rat = matches!(l, Value::FatRat(_, _)) || matches!(r, Value::FatRat(_, _));
        if has_rat {
            if has_fat_rat {
                let n = an * bd - bn * ad;
                let d = ad * bd;
                make_fat_rat(n, d)
            } else {
                rat_sub_checked(an, ad, bn, bd)
            }
        } else {
            match (l, r) {
                (Value::Int(a), Value::Int(b)) => match a.checked_sub(b) {
                    Some(diff) => Value::Int(diff),
                    None => Value::from_bigint(
                        num_bigint::BigInt::from(a) - num_bigint::BigInt::from(b),
                    ),
                },
                (Value::Num(a), Value::Num(b)) => Value::Num(a - b),
                (Value::Int(a), Value::Num(b)) => Value::Num(a as f64 - b),
                (Value::Num(a), Value::Int(b)) => Value::Num(a - b as f64),
                _ => Value::Int(0),
            }
        }
    } else if (matches!(l, Value::BigInt(_)) || matches!(r, Value::BigInt(_)))
        && let (Some(a), Some(b)) = (as_bigint(&l), as_bigint(&r))
    {
        Value::from_bigint(a - b)
    } else {
        let lf = runtime::to_float_value(&l);
        let rf = runtime::to_float_value(&r);
        if let (Some(a), Some(b)) = (lf, rf) {
            Value::Num(a - b)
        } else {
            match (l, r) {
                (Value::Int(a), Value::Int(b)) => match a.checked_sub(b) {
                    Some(diff) => Value::Int(diff),
                    None => Value::from_bigint(
                        num_bigint::BigInt::from(a) - num_bigint::BigInt::from(b),
                    ),
                },
                (Value::Num(a), Value::Num(b)) => Value::Num(a - b),
                (Value::Int(a), Value::Num(b)) => Value::Num(a as f64 - b),
                (Value::Num(a), Value::Int(b)) => Value::Num(a - b as f64),
                _ => Value::Int(0),
            }
        }
    }
}

pub(crate) fn arith_mul(left: Value, right: Value) -> Value {
    let (l, r) = runtime::coerce_numeric(left, right);
    if matches!(l, Value::Complex(_, _)) || matches!(r, Value::Complex(_, _)) {
        let (ar, ai) = runtime::to_complex_parts(&l).unwrap_or((0.0, 0.0));
        let (br, bi) = runtime::to_complex_parts(&r).unwrap_or((0.0, 0.0));
        Value::Complex(ar * br - ai * bi, ar * bi + ai * br)
    } else if (matches!(l, Value::BigInt(_)) || matches!(r, Value::BigInt(_)))
        && let (Some(a), Some(b)) = (as_bigint(&l), as_bigint(&r))
    {
        Value::from_bigint(a * b)
    } else if let (Some((an, ad)), Some((bn, bd))) = (to_big_rat_parts(&l), to_big_rat_parts(&r))
        && needs_bigrat_path(&l, &r)
    {
        let has_fat_rat = matches!(l, Value::FatRat(_, _)) || matches!(r, Value::FatRat(_, _));
        match make_big_rat(an * bn, ad * bd) {
            Value::Rat(n, d) if has_fat_rat => Value::FatRat(n, d),
            other => other,
        }
    } else if let (Some((an, ad)), Some((bn, bd))) =
        (runtime::to_rat_parts(&l), runtime::to_rat_parts(&r))
    {
        let has_rat = matches!(l, Value::Rat(_, _) | Value::FatRat(_, _))
            || matches!(r, Value::Rat(_, _) | Value::FatRat(_, _));
        let has_fat_rat = matches!(l, Value::FatRat(_, _)) || matches!(r, Value::FatRat(_, _));
        if has_rat {
            if has_fat_rat {
                let n = an * bn;
                let d = ad * bd;
                make_fat_rat(n, d)
            } else {
                rat_mul_checked(an, ad, bn, bd)
            }
        } else {
            match (l, r) {
                (Value::Int(a), Value::Int(b)) => {
                    if let Some(product) = a.checked_mul(b) {
                        Value::Int(product)
                    } else {
                        Value::bigint(NumBigInt::from(a) * NumBigInt::from(b))
                    }
                }
                (Value::Num(a), Value::Num(b)) => Value::Num(a * b),
                (Value::Int(a), Value::Num(b)) => Value::Num(a as f64 * b),
                (Value::Num(a), Value::Int(b)) => Value::Num(a * b as f64),
                _ => Value::Int(0),
            }
        }
    } else {
        // When mixing Num with Rat, convert to float
        let lf = runtime::to_float_value(&l);
        let rf = runtime::to_float_value(&r);
        if let (Some(a), Some(b)) = (lf, rf) {
            Value::Num(a * b)
        } else {
            match (l, r) {
                (Value::Int(a), Value::Int(b)) => {
                    if let Some(product) = a.checked_mul(b) {
                        Value::Int(product)
                    } else {
                        Value::bigint(NumBigInt::from(a) * NumBigInt::from(b))
                    }
                }
                (Value::Num(a), Value::Num(b)) => Value::Num(a * b),
                (Value::Int(a), Value::Num(b)) => Value::Num(a as f64 * b),
                (Value::Num(a), Value::Int(b)) => Value::Num(a * b as f64),
                _ => Value::Int(0),
            }
        }
    }
}

pub(crate) fn arith_div(left: Value, right: Value) -> Result<Value, RuntimeError> {
    let (l, r) = runtime::coerce_numeric(left, right);
    if matches!(l, Value::Complex(_, _)) || matches!(r, Value::Complex(_, _)) {
        let (ar, ai) = runtime::to_complex_parts(&l).unwrap_or((0.0, 0.0));
        let (br, bi) = runtime::to_complex_parts(&r).unwrap_or((0.0, 0.0));
        let denom = br * br + bi * bi;
        if denom == 0.0 {
            return Err(RuntimeError::numeric_divide_by_zero());
        }
        Ok(Value::Complex(
            (ar * br + ai * bi) / denom,
            (ai * br - ar * bi) / denom,
        ))
    } else {
        // When mixing Num with Rat/FatRat, convert to float
        let has_num = matches!(l, Value::Num(_)) || matches!(r, Value::Num(_));
        let has_rat = matches!(l, Value::Rat(_, _) | Value::FatRat(_, _))
            || matches!(r, Value::Rat(_, _) | Value::FatRat(_, _));
        if has_num && has_rat {
            let lf = runtime::to_float_value(&l).unwrap_or(0.0);
            let rf = runtime::to_float_value(&r).unwrap_or(1.0);
            if rf == 0.0 {
                return Ok(RuntimeError::divide_by_zero_failure(None, None));
            }
            return Ok(Value::Num(lf / rf));
        }
        let has_fat_rat = matches!(l, Value::FatRat(_, _)) || matches!(r, Value::FatRat(_, _));
        let has_big_rat = matches!(l, Value::BigRat(_, _)) || matches!(r, Value::BigRat(_, _));
        if (has_fat_rat || has_big_rat)
            && let (Some((an, ad)), Some((bn, bd))) = (to_big_rat_parts(&l), to_big_rat_parts(&r))
        {
            let result = make_big_rat(an * bd, ad * bn);
            return Ok(match result {
                Value::Rat(n, d) if has_fat_rat => Value::FatRat(n, d),
                other => other,
            });
        }
        if (matches!(l, Value::BigInt(_)) || matches!(r, Value::BigInt(_)))
            && let (Some(a), Some(b)) = (as_bigint(&l), as_bigint(&r))
        {
            if b.is_zero() {
                return Err(RuntimeError::numeric_divide_by_zero());
            }
            if (&a % &b).is_zero() {
                return Ok(Value::from_bigint(a / b));
            }
            return Ok(make_big_rat(a, b));
        }
        if let (Some((an, ad)), Some((bn, bd))) =
            (runtime::to_rat_parts(&l), runtime::to_rat_parts(&r))
        {
            let has_rat = matches!(l, Value::Rat(_, _) | Value::FatRat(_, _))
                || matches!(r, Value::Rat(_, _) | Value::FatRat(_, _));
            let has_fat_rat = matches!(l, Value::FatRat(_, _)) || matches!(r, Value::FatRat(_, _));
            if has_rat || matches!((&l, &r), (Value::Int(_), Value::Int(_))) {
                return Ok(if has_fat_rat {
                    if let (Some(n), Some(d)) = (an.checked_mul(bd), ad.checked_mul(bn)) {
                        make_fat_rat(n, d)
                    } else {
                        let n = NumBigInt::from(an) * NumBigInt::from(bd);
                        let d = NumBigInt::from(ad) * NumBigInt::from(bn);
                        let result = make_big_rat(n, d);
                        match result {
                            Value::Rat(n, d) => Value::FatRat(n, d),
                            other => other,
                        }
                    }
                } else {
                    rat_div_checked(an, ad, bn, bd)
                });
            }
        }
        Ok(match (&l, &r) {
            (Value::Num(_), Value::Num(b)) if *b == 0.0 => {
                return Err(RuntimeError::numeric_divide_by_zero());
            }
            (Value::Int(_), Value::Num(b)) if *b == 0.0 => {
                return Ok(RuntimeError::divide_by_zero_failure(None, None));
            }
            (Value::Num(_), Value::Int(b)) if *b == 0 => {
                return Ok(RuntimeError::divide_by_zero_failure(None, None));
            }
            (Value::Num(a), Value::Num(b)) => Value::Num(a / b),
            (Value::Int(a), Value::Num(b)) => Value::Num(*a as f64 / b),
            (Value::Num(a), Value::Int(b)) => Value::Num(a / *b as f64),
            (Value::Num(a), Value::BigInt(b)) => {
                let bf = runtime::to_float_value(&Value::BigInt(b.clone())).unwrap_or(1.0);
                if bf == 0.0 {
                    return Err(RuntimeError::numeric_divide_by_zero());
                }
                Value::Num(a / bf)
            }
            (Value::BigInt(a), Value::Num(b)) => {
                if *b == 0.0 {
                    return Err(RuntimeError::numeric_divide_by_zero());
                }
                let af = runtime::to_float_value(&Value::BigInt(a.clone())).unwrap_or(0.0);
                Value::Num(af / b)
            }
            _ => Value::Int(0),
        })
    }
}

pub(crate) fn arith_mod(left: Value, right: Value) -> Result<Value, RuntimeError> {
    let (mut l, mut r) = runtime::coerce_numeric(left, right);
    // Mixed Num/Rat modulo should use floating semantics; routing through
    // exact-rational reduction loses expected precision behavior for cases like
    // 1.01 % 0.2 (should be ~0.01).
    if matches!(l, Value::Num(_))
        && matches!(
            r,
            Value::Rat(_, _) | Value::FatRat(_, _) | Value::BigRat(_, _)
        )
    {
        r = Value::Num(runtime::to_float_value(&r).unwrap_or(f64::NAN));
    } else if matches!(r, Value::Num(_))
        && matches!(
            l,
            Value::Rat(_, _) | Value::FatRat(_, _) | Value::BigRat(_, _)
        )
    {
        l = Value::Num(runtime::to_float_value(&l).unwrap_or(f64::NAN));
    }
    if let (Some((an, ad)), Some((bn, bd))) = (runtime::to_rat_parts(&l), runtime::to_rat_parts(&r))
    {
        if matches!(l, Value::Rat(_, _) | Value::FatRat(_, _))
            || matches!(r, Value::Rat(_, _) | Value::FatRat(_, _))
        {
            if bn == 0 {
                return Err(RuntimeError::numeric_divide_by_zero());
            }
            // Rational modulo with divisor-sign semantics.
            let num = num_integer::Integer::mod_floor(&(an * bd), &(ad * bn));
            let den = ad * bd;
            let has_fat_rat = matches!(l, Value::FatRat(_, _)) || matches!(r, Value::FatRat(_, _));
            Ok(if has_fat_rat {
                make_fat_rat(num, den)
            } else {
                crate::value::make_rat(num, den)
            })
        } else {
            Ok(match (l, r) {
                (Value::Int(_), Value::Int(0)) => {
                    return Err(RuntimeError::numeric_divide_by_zero());
                }
                (Value::BigInt(_), Value::Int(0)) => {
                    return Err(RuntimeError::numeric_divide_by_zero());
                }
                (Value::Int(_), Value::BigInt(b)) if b.is_zero() => {
                    return Err(RuntimeError::numeric_divide_by_zero());
                }
                (Value::BigInt(_), Value::BigInt(b)) if b.is_zero() => {
                    return Err(RuntimeError::numeric_divide_by_zero());
                }
                (Value::Int(a), Value::Int(b)) => {
                    Value::Int(num_integer::Integer::mod_floor(&a, &b))
                }
                (Value::BigInt(a), Value::Int(b)) => {
                    let bb = num_bigint::BigInt::from(b);
                    Value::from_bigint(num_integer::Integer::mod_floor(a.as_ref(), &bb))
                }
                (Value::Int(a), Value::BigInt(b)) => {
                    let aa = num_bigint::BigInt::from(a);
                    Value::from_bigint(num_integer::Integer::mod_floor(&aa, b.as_ref()))
                }
                (Value::BigInt(a), Value::BigInt(b)) => {
                    Value::from_bigint(num_integer::Integer::mod_floor(a.as_ref(), b.as_ref()))
                }
                (Value::Num(a), Value::Num(b)) => Value::Num(float_mod_floor(a, b)),
                (Value::Int(a), Value::Num(b)) => Value::Num(float_mod_floor(a as f64, b)),
                (Value::Num(a), Value::Int(b)) => Value::Num(float_mod_floor(a, b as f64)),
                _ => Value::Int(0),
            })
        }
    } else {
        Ok(match (l, r) {
            (Value::Int(_), Value::Int(0)) => {
                return Err(RuntimeError::numeric_divide_by_zero());
            }
            (Value::BigInt(_), Value::Int(0)) => {
                return Err(RuntimeError::numeric_divide_by_zero());
            }
            (Value::Int(_), Value::BigInt(b)) if b.is_zero() => {
                return Err(RuntimeError::numeric_divide_by_zero());
            }
            (Value::BigInt(_), Value::BigInt(b)) if b.is_zero() => {
                return Err(RuntimeError::numeric_divide_by_zero());
            }
            (Value::Int(a), Value::Int(b)) => Value::Int(num_integer::Integer::mod_floor(&a, &b)),
            (Value::BigInt(a), Value::Int(b)) => {
                let bb = num_bigint::BigInt::from(b);
                Value::from_bigint(num_integer::Integer::mod_floor(a.as_ref(), &bb))
            }
            (Value::Int(a), Value::BigInt(b)) => {
                let aa = num_bigint::BigInt::from(a);
                Value::from_bigint(num_integer::Integer::mod_floor(&aa, b.as_ref()))
            }
            (Value::BigInt(a), Value::BigInt(b)) => {
                Value::from_bigint(num_integer::Integer::mod_floor(a.as_ref(), b.as_ref()))
            }
            (Value::Num(a), Value::Num(b)) => Value::Num(float_mod_floor(a, b)),
            (Value::Int(a), Value::Num(b)) => Value::Num(float_mod_floor(a as f64, b)),
            (Value::Num(a), Value::Int(b)) => Value::Num(float_mod_floor(a, b as f64)),
            _ => Value::Int(0),
        })
    }
}

pub(crate) fn arith_pow(left: Value, right: Value) -> Value {
    let (l, r) = runtime::coerce_numeric(left, right);
    let to_pow_f64 = |v: &Value| -> Option<f64> {
        runtime::to_float_value(v).or_else(|| match v {
            Value::BigRat(n, d) => {
                if d.is_zero() {
                    if n.is_zero() {
                        Some(f64::NAN)
                    } else if n.sign() == Sign::Minus {
                        Some(f64::NEG_INFINITY)
                    } else {
                        Some(f64::INFINITY)
                    }
                } else {
                    Some(n.to_f64()? / d.to_f64()?)
                }
            }
            _ => None,
        })
    };
    let powf_or_zero = |base: &Value, exp: &Value| -> Value {
        match (to_pow_f64(base), to_pow_f64(exp)) {
            (Some(a), Some(b)) => Value::Num(a.powf(b)),
            _ => Value::Int(0),
        }
    };
    if matches!(l, Value::Complex(_, _)) || matches!(r, Value::Complex(_, _)) {
        let (ar, ai) = runtime::to_complex_parts(&l).unwrap_or((0.0, 0.0));
        let (br, bi) = runtime::to_complex_parts(&r).unwrap_or((0.0, 0.0));
        // Special case: 0 ** anything = 0+0i (except 0**0 = 1+0i)
        if ar == 0.0 && ai == 0.0 {
            return if br == 0.0 && bi == 0.0 {
                Value::Complex(1.0, 0.0)
            } else {
                Value::Complex(0.0, 0.0)
            };
        }
        let ln_r = (ar * ar + ai * ai).sqrt().ln();
        let ln_i = ai.atan2(ar);
        let wr = br * ln_r - bi * ln_i;
        let wi = br * ln_i + bi * ln_r;
        let mag = wr.exp();
        Value::Complex(mag * wi.cos(), mag * wi.sin())
    } else {
        match (l, r) {
            (Value::Int(a), Value::Int(b)) if b >= 0 => {
                if let Some(result) = a.checked_pow(b as u32) {
                    Value::Int(result)
                } else {
                    // Overflow: use BigInt
                    use num_bigint::BigInt;
                    let base = BigInt::from(a);
                    Value::bigint(base.pow(b as u32))
                }
            }
            (Value::Int(a), Value::Int(b)) => {
                let pos = (-b) as u32;
                if let Some(base) = a.checked_pow(pos) {
                    make_rat(1, base)
                } else {
                    Value::Num(1.0 / (a as f64).powi(pos as i32))
                }
            }
            (Value::Rat(n, d), Value::Int(b)) if b >= 0 => {
                let p = b as u32;
                if let (Some(np), Some(dp)) = (n.checked_pow(p), d.checked_pow(p)) {
                    make_rat(np, dp)
                } else {
                    // Overflow in i64: use BigInt, then check if denom fits
                    let nn = NumBigInt::from(n).pow(p);
                    let dd = NumBigInt::from(d).pow(p);
                    match make_big_rat(nn, dd) {
                        // If make_big_rat reduced it to fit in i64, keep as Rat
                        Value::Rat(rn, rd) => Value::Rat(rn, rd),
                        // BigRat means denom exceeds i64: degrade to Num
                        Value::BigRat(bn, bd) => {
                            // Check if denom fits in i64 (numerator can be big)
                            if let Some(d64) = bd.to_i64() {
                                Value::BigRat(bn, NumBigInt::from(d64))
                            } else {
                                Value::Num(bigint_ratio_to_f64(&bn, &bd))
                            }
                        }
                        other => other,
                    }
                }
            }
            (Value::Rat(n, d), Value::Int(b)) => {
                let p = (-b) as u32;
                if let (Some(dp), Some(np)) = (d.checked_pow(p), n.checked_pow(p)) {
                    make_rat(dp, np)
                } else {
                    let nn = NumBigInt::from(d).pow(p);
                    let dd = NumBigInt::from(n).pow(p);
                    match make_big_rat(nn, dd) {
                        Value::Rat(rn, rd) => Value::Rat(rn, rd),
                        Value::BigRat(bn, bd) => {
                            if let Some(d64) = bd.to_i64() {
                                Value::BigRat(bn, NumBigInt::from(d64))
                            } else {
                                Value::Num(bigint_ratio_to_f64(&bn, &bd))
                            }
                        }
                        other => other,
                    }
                }
            }
            (Value::FatRat(n, d), Value::Int(b)) if b >= 0 => {
                let p = b as u32;
                if let (Some(np), Some(dp)) = (n.checked_pow(p), d.checked_pow(p)) {
                    make_fat_rat(np, dp)
                } else {
                    let nn = NumBigInt::from(n).pow(p);
                    let dd = NumBigInt::from(d).pow(p);
                    match make_big_rat(nn, dd) {
                        Value::Rat(nn, dd) => Value::FatRat(nn, dd),
                        other => other,
                    }
                }
            }
            (Value::FatRat(n, d), Value::Int(b)) => {
                let p = (-b) as u32;
                if let (Some(dp), Some(np)) = (d.checked_pow(p), n.checked_pow(p)) {
                    make_fat_rat(dp, np)
                } else {
                    let nn = NumBigInt::from(d).pow(p);
                    let dd = NumBigInt::from(n).pow(p);
                    match make_big_rat(nn, dd) {
                        Value::Rat(nn, dd) => Value::FatRat(nn, dd),
                        other => other,
                    }
                }
            }
            (Value::BigRat(n, d), Value::Int(b)) if b >= 0 => {
                let p = b as u32;
                make_big_rat(n.pow(p), d.pow(p))
            }
            (Value::BigRat(n, d), Value::Int(b)) => {
                let p = (-b) as u32;
                make_big_rat(d.pow(p), n.pow(p))
            }
            (Value::Num(a), Value::Int(b)) => Value::Num(a.powi(b as i32)),
            (Value::BigInt(a), Value::Int(b)) if b >= 0 => {
                if let Ok(exp_u) = u32::try_from(b) {
                    Value::bigint((*a).clone().pow(exp_u))
                } else {
                    Value::Num(a.as_ref().to_f64().unwrap_or(0.0).powf(b as f64))
                }
            }
            (Value::BigInt(a), Value::Int(b)) => {
                if *a == NumBigInt::from(1) {
                    Value::Int(1)
                } else if *a == NumBigInt::from(-1) {
                    if (-b) % 2 == 0 {
                        Value::Int(1)
                    } else {
                        Value::Int(-1)
                    }
                } else {
                    Value::Num(a.as_ref().to_f64().unwrap_or(0.0).powf(b as f64))
                }
            }
            (Value::Int(a), Value::Num(b)) => {
                if b.is_finite() && b.fract() == 0.0 && b >= i32::MIN as f64 && b <= i32::MAX as f64
                {
                    let exp_i = b as i32;
                    if exp_i >= 0 {
                        let exp_u = exp_i as u32;
                        if let Some(result) = a.checked_pow(exp_u) {
                            Value::Int(result)
                        } else {
                            use num_bigint::BigInt;
                            Value::bigint(BigInt::from(a).pow(exp_u))
                        }
                    } else {
                        let pos = (-exp_i) as u32;
                        if let Some(base) = a.checked_pow(pos) {
                            make_rat(1, base)
                        } else {
                            Value::Num(1.0 / (a as f64).powi(pos as i32))
                        }
                    }
                } else {
                    Value::Num((a as f64).powf(b))
                }
            }
            (Value::Int(a), Value::BigInt(b)) => {
                let is_even = (b.as_ref() % 2u8) == NumBigInt::from(0u8);
                if b.is_zero() || a == 1 {
                    Value::Int(1)
                } else if a == -1 {
                    Value::Int(if is_even { 1 } else { -1 })
                } else if a == 0 && b.sign() != Sign::Minus {
                    Value::Int(0)
                } else if let Some(exp_u) = b.as_ref().to_u32() {
                    if let Some(result) = a.checked_pow(exp_u) {
                        Value::Int(result)
                    } else {
                        Value::bigint(NumBigInt::from(a).pow(exp_u))
                    }
                } else {
                    Value::Num((a as f64).powf(b.as_ref().to_f64().unwrap_or(f64::INFINITY)))
                }
            }
            (Value::Num(a), Value::BigInt(b)) => {
                let is_even = (b.as_ref() % 2u8) == NumBigInt::from(0u8);
                if b.is_zero() || a == 1.0 {
                    Value::Num(1.0)
                } else if a == -1.0 {
                    Value::Num(if is_even { 1.0 } else { -1.0 })
                } else if a == 0.0 && b.sign() != Sign::Minus {
                    Value::Num(0.0)
                } else {
                    Value::Num(a.powf(b.as_ref().to_f64().unwrap_or(f64::INFINITY)))
                }
            }
            (Value::Num(a), Value::Num(b)) => Value::Num(a.powf(b)),
            (base, exp) => powf_or_zero(&base, &exp),
        }
    }
}

pub(crate) fn arith_negate(val: Value) -> Result<Value, RuntimeError> {
    match val {
        Value::Bool(b) => Ok(Value::Int(if b { -1 } else { 0 })),
        Value::Int(i) => {
            if let Some(neg) = i.checked_neg() {
                Ok(Value::Int(neg))
            } else {
                // i64::MIN overflow: promote to Num
                Ok(Value::Num(-(i as f64)))
            }
        }
        Value::BigInt(i) => Ok(Value::bigint(-(*i).clone())),
        Value::Num(f) => Ok(Value::Num(-f)),
        Value::Rat(n, d) => {
            if let Some(neg) = n.checked_neg() {
                Ok(Value::Rat(neg, d))
            } else {
                Ok(Value::Num(-(n as f64) / d as f64))
            }
        }
        Value::FatRat(n, d) => {
            if let Some(neg) = n.checked_neg() {
                Ok(make_fat_rat(neg, d))
            } else {
                Ok(Value::Num(-(n as f64) / d as f64))
            }
        }
        Value::BigRat(n, d) => Ok(crate::value::make_big_rat(-n, d)),
        Value::Complex(r, i) => Ok(Value::Complex(-r, -i)),
        Value::Str(ref s) => {
            if let Ok(i) = s.trim().parse::<i64>() {
                Ok(Value::Int(-i))
            } else if let Ok(f) = s.trim().parse::<f64>() {
                Ok(Value::Num(-f))
            } else {
                Ok(Value::Int(0))
            }
        }
        other => Ok(Value::Int(-runtime::to_int(&other))),
    }
}
