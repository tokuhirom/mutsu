#![allow(clippy::result_large_err)]
//! Addition and subtraction arithmetic operators.

use super::range::{mixin_range_arith, mixin_range_arith_val, range_offset};
use super::rat::{
    as_bigint, is_fat_rat_like, make_fat_rat, needs_bigrat_path, rat_add_checked, rat_sub_checked,
    to_big_rat_parts,
};
use super::temporal::{
    instance_datetime_parts, instance_days, instance_duration_value, instance_instant_raw,
    instance_instant_value, make_duration, value_sub,
};
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, make_big_fat_rat, make_big_rat_arith};

// ── Arithmetic operators ─────────────────────────────────────────────
pub(crate) fn arith_add(left: Value, right: Value) -> Result<Value, RuntimeError> {
    // Phase 2 element container: a `:=`-bound element cell may reach an arith op
    // directly (e.g. `@a.reduce(&[+])` folds raw items); read through the cell.
    let (left, right) = (left.into_deref(), right.into_deref());
    // A bare Whatever value reaching `+` is NOT a curry point (those are wrapped
    // into a WhateverCode at parse time). Numifying it dies in Raku, e.g.
    // `&infix:<+>(*, 42)` invokes `+` with a Whatever argument.
    if matches!(left, Value::Whatever | Value::HyperWhatever)
        || matches!(right, Value::Whatever | Value::HyperWhatever)
    {
        return Err(RuntimeError::new(
            "Cannot resolve caller Numeric(Whatever:D: ); none of these signatures matches:\n    (Mu:U \\v: *%_)".to_string(),
        ));
    }
    // Mixin-wrapped Range + Real (or Real + Mixin Range): perform Range arithmetic and re-wrap
    if let Some(result) = mixin_range_arith(left.clone(), right.clone(), arith_add)
        .or_else(|| mixin_range_arith(right.clone(), left.clone(), arith_add))
    {
        return result;
    }
    // Range + Real (commutative): shift both bounds, preserving exclusivity.
    let add_endpoint = |a: Value, b: Value| arith_add(a, b).unwrap_or(Value::Int(0));
    if let Some(range) = range_offset(&left, &right, add_endpoint)
        .or_else(|| range_offset(&right, &left, add_endpoint))
    {
        return Ok(range);
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
    // Instant + Instant is illegal
    if instance_instant_value(&left).is_some() && instance_instant_value(&right).is_some() {
        return Err(RuntimeError::new(
            "Cannot add two Instants together".to_string(),
        ));
    }
    // Instant + Duration => Instant
    if let Some(tai) = instance_instant_value(&left)
        && let Some(dur) = instance_duration_value(&right)
    {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("value".to_string(), Value::Num(tai + dur));
        return Ok(Value::make_instance(Symbol::intern("Instant"), attrs));
    }
    if let Some(tai) = instance_instant_value(&right)
        && let Some(dur) = instance_duration_value(&left)
    {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("value".to_string(), Value::Num(tai + dur));
        return Ok(Value::make_instance(Symbol::intern("Instant"), attrs));
    }
    // Instant + Numeric => Instant (add to TAI value)
    if let Some(tai) = instance_instant_value(&left)
        && right.is_numeric()
    {
        let delta = crate::runtime::to_float_value(&right).unwrap_or(0.0);
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("value".to_string(), Value::Num(tai + delta));
        return Ok(Value::make_instance(Symbol::intern("Instant"), attrs));
    }
    if let Some(tai) = instance_instant_value(&right)
        && left.is_numeric()
    {
        let delta = crate::runtime::to_float_value(&left).unwrap_or(0.0);
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("value".to_string(), Value::Num(tai + delta));
        return Ok(Value::make_instance(Symbol::intern("Instant"), attrs));
    }
    // Duration + Numeric => Duration
    if let Some(dur) = instance_duration_value(&left)
        && right.is_numeric()
    {
        let delta = crate::runtime::to_float_value(&right).unwrap_or(0.0);
        return Ok(make_duration(dur + delta));
    }
    if let Some(dur) = instance_duration_value(&right)
        && left.is_numeric()
    {
        let delta = crate::runtime::to_float_value(&left).unwrap_or(0.0);
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
    let (l, r) = crate::runtime::coerce_numeric(left, right);
    Ok(arith_add_coerced(l, r))
}

fn arith_add_coerced(l: Value, r: Value) -> Value {
    if matches!(l, Value::Complex(_, _)) || matches!(r, Value::Complex(_, _)) {
        let (ar, ai) = crate::runtime::to_complex_parts(&l).unwrap_or((0.0, 0.0));
        let (br, bi) = crate::runtime::to_complex_parts(&r).unwrap_or((0.0, 0.0));
        Value::Complex(ar + br, ai + bi)
    } else if (matches!(l, Value::BigInt(_)) || matches!(r, Value::BigInt(_)))
        && let (Some(a), Some(b)) = (as_bigint(&l), as_bigint(&r))
    {
        Value::from_bigint(a + b)
    } else if let (Some((an, ad)), Some((bn, bd))) = (to_big_rat_parts(&l), to_big_rat_parts(&r))
        && needs_bigrat_path(&l, &r)
    {
        let has_fat_rat = is_fat_rat_like(&l) || is_fat_rat_like(&r);
        if has_fat_rat {
            match make_big_fat_rat(an * bd.clone() + bn * ad.clone(), ad * bd) {
                Value::Rat(n, d) => Value::FatRat(n, d),
                other => other,
            }
        } else {
            make_big_rat_arith(an * bd.clone() + bn * ad.clone(), ad * bd)
        }
    } else if let (Some((an, ad)), Some((bn, bd))) = (
        crate::runtime::to_rat_parts(&l),
        crate::runtime::to_rat_parts(&r),
    ) {
        let has_rat = matches!(l, Value::Rat(_, _) | Value::FatRat(_, _))
            || matches!(r, Value::Rat(_, _) | Value::FatRat(_, _));
        let has_fat_rat = is_fat_rat_like(&l) || is_fat_rat_like(&r);
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
        let lf = crate::runtime::to_float_value(&l);
        let rf = crate::runtime::to_float_value(&r);
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
    let (left, right) = (left.into_deref(), right.into_deref());
    if let (Some(a), Some(b)) = (instance_instant_raw(&left), instance_instant_raw(&right)) {
        return make_duration(crate::runtime::to_float_value(&value_sub(a, b)).unwrap_or(0.0));
    }
    if let Some(a) = instance_instant_raw(&left)
        && let Some(dur) = instance_duration_value(&right)
    {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("value".to_string(), value_sub(a, Value::Num(dur)));
        return Value::make_instance(Symbol::intern("Instant"), attrs);
    }
    if let Some(a) = instance_instant_raw(&left)
        && right.is_numeric()
    {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("value".to_string(), value_sub(a, right));
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
        let delta = crate::runtime::to_float_value(&right).unwrap_or(0.0);
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
    // Mixin-wrapped Range - Real: perform Range arithmetic and re-wrap
    if let Some(result) = mixin_range_arith_val(left.clone(), right.clone(), arith_sub) {
        return result;
    }
    // Range - Real: shift both bounds (only when the Range is on the left;
    // `Real - Range` numifies the Range, matching Raku).
    if let Some(range) = range_offset(&left, &right, arith_sub) {
        return range;
    }
    let (l, r) = crate::runtime::coerce_numeric(left, right);
    if matches!(l, Value::Complex(_, _)) || matches!(r, Value::Complex(_, _)) {
        let (ar, ai) = crate::runtime::to_complex_parts(&l).unwrap_or((0.0, 0.0));
        let (br, bi) = crate::runtime::to_complex_parts(&r).unwrap_or((0.0, 0.0));
        Value::Complex(ar - br, ai - bi)
    } else if (matches!(l, Value::BigInt(_)) || matches!(r, Value::BigInt(_)))
        && let (Some(a), Some(b)) = (as_bigint(&l), as_bigint(&r))
    {
        Value::from_bigint(a - b)
    } else if let (Some((an, ad)), Some((bn, bd))) = (to_big_rat_parts(&l), to_big_rat_parts(&r))
        && needs_bigrat_path(&l, &r)
    {
        let has_fat_rat = is_fat_rat_like(&l) || is_fat_rat_like(&r);
        if has_fat_rat {
            match make_big_fat_rat(an * bd.clone() - bn * ad.clone(), ad * bd) {
                Value::Rat(n, d) => Value::FatRat(n, d),
                other => other,
            }
        } else {
            make_big_rat_arith(an * bd.clone() - bn * ad.clone(), ad * bd)
        }
    } else if let (Some((an, ad)), Some((bn, bd))) = (
        crate::runtime::to_rat_parts(&l),
        crate::runtime::to_rat_parts(&r),
    ) {
        let has_rat = matches!(l, Value::Rat(_, _) | Value::FatRat(_, _))
            || matches!(r, Value::Rat(_, _) | Value::FatRat(_, _));
        let has_fat_rat = is_fat_rat_like(&l) || is_fat_rat_like(&r);
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
        let lf = crate::runtime::to_float_value(&l);
        let rf = crate::runtime::to_float_value(&r);
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
