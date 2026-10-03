//! Addition and subtraction arithmetic operators.

use super::range::{mixin_range_arith, mixin_range_arith_val, range_offset};
use super::rat::{
    big_int_add, big_int_sub, is_fat_rat_like, make_fat_rat, needs_bigrat_path, rat_add_checked,
    rat_sub_checked, to_big_rat_parts,
};
use super::temporal::{
    instance_datetime_parts, instance_days, instance_duration_raw_value, instance_duration_value,
    instance_instant_raw, instance_instant_value, make_duration, make_duration_real,
    rebuild_date_like, rebuild_datetime_like, value_add, value_sub,
};
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView, make_big_fat_rat, make_big_rat_arith};
use num_bigint::BigInt as NumBigInt;

/// An Instant holding `tai` seconds, stored as Rakudo stores them (see
/// [`super::temporal::tai_rat`]).
// Cost: O(d), see `tai_rat`.
fn make_instant(tai: Value) -> Value {
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("value".to_string(), super::temporal::tai_rat(&tai));
    Value::make_instance(Symbol::intern("Instant"), attrs)
}

// ── Arithmetic operators ─────────────────────────────────────────────
pub(crate) fn arith_add(left: Value, right: Value) -> Result<Value, RuntimeError> {
    // Phase 2 element container: a `:=`-bound element cell may reach an arith op
    // directly (e.g. `@a.reduce(&[+])` folds raw items); read through the cell.
    let (left, right) = (left.into_deref(), right.into_deref());
    // Fast path: a pair of plain integers with at least one big operand. None
    // of the Whatever/Range/Date/Instant/Duration guards below can match a bare
    // Int/BigInt, and for a wide operand walking them costs more than the
    // arithmetic itself. (Int+Int keeps falling through: the VM fast-paths the
    // machine-word case before it ever reaches here, and an overflowing pair
    // still needs the widening below.)
    if let Some(sum) = big_int_add(&left, &right) {
        return Ok(sum);
    }
    // A bare Whatever value reaching `+` is NOT a curry point (those are wrapped
    // into a WhateverCode at parse time), and a bare Sub/Block has no `.Numeric`
    // candidate either. Numifying either dies in Raku, e.g. `&infix:<+>(*, 42)`
    // invokes `+` with a Whatever argument (#9791). Checked here (not just in
    // the VM opcode's coercion bridge) because a reduction fold
    // (`[+] $b, $c`, `$b Z+ $c`) calls this directly with 2+ elements.
    crate::runtime::require_numeric_candidate(&left)?;
    crate::runtime::require_numeric_candidate(&right)?;
    // Mixin-wrapped Range + Real (or Real + Mixin Range): perform Range arithmetic and re-wrap
    if let Some(result) = mixin_range_arith(left.clone(), right.clone(), arith_add)
        .or_else(|| mixin_range_arith(right.clone(), left.clone(), arith_add))
    {
        return result;
    }
    // Range + Real (commutative): shift both bounds, preserving exclusivity.
    let add_endpoint = |a: Value, b: Value| arith_add(a, b).unwrap_or(Value::int(0));
    if let Some(range) = range_offset(&left, &right, add_endpoint)
        .or_else(|| range_offset(&right, &left, add_endpoint))
    {
        return Ok(range);
    }
    // Date + Int: add days
    if let Some(days) = instance_days(&left)
        && let ValueView::Int(delta) = right.view()
    {
        return Ok(rebuild_date_like(&left, days + delta));
    }
    if let Some(days) = instance_days(&right)
        && let ValueView::Int(delta) = left.view()
    {
        return Ok(rebuild_date_like(&right, days + delta));
    }
    // Instant + Instant is illegal
    if instance_instant_value(&left).is_some() && instance_instant_value(&right).is_some() {
        return Err(RuntimeError::new(
            "Cannot add two Instants together".to_string(),
        ));
    }
    // Instant + Duration => Instant, exact when both hold Rats.
    if let Some(tai) = instance_instant_raw(&left)
        && let Some(dur) = instance_duration_raw_value(&right)
    {
        return Ok(make_instant(value_add(tai, dur)));
    }
    if let Some(tai) = instance_instant_raw(&right)
        && let Some(dur) = instance_duration_raw_value(&left)
    {
        return Ok(make_instant(value_add(dur, tai)));
    }
    // Instant + Numeric => Instant (add to TAI value)
    if let Some(tai) = instance_instant_raw(&left)
        && right.is_numeric()
    {
        return Ok(make_instant(value_add(tai, right)));
    }
    if let Some(tai) = instance_instant_raw(&right)
        && left.is_numeric()
    {
        return Ok(make_instant(value_add(left, tai)));
    }
    // Duration + Numeric => Duration
    if let Some(dur) = instance_duration_raw_value(&left)
        && right.is_numeric()
    {
        return Ok(make_duration_real(&value_add(dur, right)));
    }
    if let Some(dur) = instance_duration_raw_value(&right)
        && left.is_numeric()
    {
        return Ok(make_duration_real(&value_add(left, dur)));
    }
    // DateTime + Duration => DateTime
    if let Some((y, m, d, h, mi, s, tz)) = instance_datetime_parts(&left)
        && let Some(delta) = instance_duration_value(&right)
    {
        use crate::builtins::methods_0arg::temporal;
        let instant = temporal::datetime_to_instant_leap_aware(y, m, d, h, mi, s, tz);
        let (ny, nm, nd, nh, nmi, ns) =
            temporal::instant_to_datetime_leap_aware(instant + delta, tz);
        return Ok(rebuild_datetime_like(&left, (ny, nm, nd, nh, nmi, ns, tz)));
    }
    if let Some(delta) = instance_duration_value(&left)
        && let Some((y, m, d, h, mi, s, tz)) = instance_datetime_parts(&right)
    {
        use crate::builtins::methods_0arg::temporal;
        let instant = temporal::datetime_to_instant_leap_aware(y, m, d, h, mi, s, tz);
        let (ny, nm, nd, nh, nmi, ns) =
            temporal::instant_to_datetime_leap_aware(instant + delta, tz);
        return Ok(rebuild_datetime_like(&right, (ny, nm, nd, nh, nmi, ns, tz)));
    }
    let (l, r) = crate::runtime::coerce_numeric(left, right);
    Ok(arith_add_coerced(l, r))
}

fn arith_add_coerced(l: Value, r: Value) -> Value {
    if matches!(l.view(), ValueView::Complex(_, _)) || matches!(r.view(), ValueView::Complex(_, _))
    {
        let (ar, ai) = crate::runtime::to_complex_parts(&l).unwrap_or((0.0, 0.0));
        let (br, bi) = crate::runtime::to_complex_parts(&r).unwrap_or((0.0, 0.0));
        Value::complex(ar + br, ai + bi)
    } else if let Some(sum) = big_int_add(&l, &r) {
        sum
    } else if let (Some((an, ad)), Some((bn, bd))) = (to_big_rat_parts(&l), to_big_rat_parts(&r))
        && needs_bigrat_path(&l, &r)
    {
        let has_fat_rat = is_fat_rat_like(&l) || is_fat_rat_like(&r);
        if has_fat_rat {
            let tmp = make_big_fat_rat(an * bd.clone() + bn * ad.clone(), ad * bd);
            if let ValueView::Rat(n, d) = tmp.view() {
                Value::fat_rat_raw(n, d)
            } else {
                tmp
            }
        } else {
            make_big_rat_arith(an * bd.clone() + bn * ad.clone(), ad * bd)
        }
    } else if let (Some((an, ad)), Some((bn, bd))) = (
        crate::runtime::to_rat_parts(&l),
        crate::runtime::to_rat_parts(&r),
    ) {
        let has_rat = matches!(l.view(), ValueView::Rat(_, _) | ValueView::FatRat(_, _))
            || matches!(r.view(), ValueView::Rat(_, _) | ValueView::FatRat(_, _));
        let has_fat_rat = is_fat_rat_like(&l) || is_fat_rat_like(&r);
        if has_rat {
            if has_fat_rat {
                if let (Some(n), Some(d)) = (
                    an.checked_mul(bd).and_then(|left| {
                        bn.checked_mul(ad).and_then(|right| left.checked_add(right))
                    }),
                    ad.checked_mul(bd),
                ) {
                    make_fat_rat(n, d)
                } else {
                    let n = NumBigInt::from(an) * NumBigInt::from(bd)
                        + NumBigInt::from(bn) * NumBigInt::from(ad);
                    let d = NumBigInt::from(ad) * NumBigInt::from(bd);
                    let result = make_big_fat_rat(n, d);
                    if let ValueView::Rat(n, d) = result.view() {
                        Value::fat_rat_raw(n, d)
                    } else {
                        result
                    }
                }
            } else {
                rat_add_checked(an, ad, bn, bd)
            }
        } else {
            match (l.view(), r.view()) {
                (ValueView::Int(a), ValueView::Int(b)) => match a.checked_add(b) {
                    Some(sum) => Value::int(sum),
                    None => Value::from_bigint(
                        num_bigint::BigInt::from(a) + num_bigint::BigInt::from(b),
                    ),
                },
                (ValueView::Num(a), ValueView::Num(b)) => Value::num(a + b),
                (ValueView::Int(a), ValueView::Num(b)) => Value::num(a as f64 + b),
                (ValueView::Num(a), ValueView::Int(b)) => Value::num(a + b as f64),
                _ => Value::int(0),
            }
        }
    } else {
        let lf = crate::runtime::to_float_value(&l);
        let rf = crate::runtime::to_float_value(&r);
        if let (Some(a), Some(b)) = (lf, rf) {
            Value::num(a + b)
        } else {
            match (l.view(), r.view()) {
                (ValueView::Int(a), ValueView::Int(b)) => match a.checked_add(b) {
                    Some(sum) => Value::int(sum),
                    None => Value::from_bigint(
                        num_bigint::BigInt::from(a) + num_bigint::BigInt::from(b),
                    ),
                },
                (ValueView::Num(a), ValueView::Num(b)) => Value::num(a + b),
                (ValueView::Int(a), ValueView::Num(b)) => Value::num(a as f64 + b),
                (ValueView::Num(a), ValueView::Int(b)) => Value::num(a + b as f64),
                _ => Value::int(0),
            }
        }
    }
}

pub(crate) fn arith_sub(left: Value, right: Value) -> Value {
    let (left, right) = (left.into_deref(), right.into_deref());
    // Fast path: a pair of plain integers with at least one big operand -- see
    // the note in `arith_add`; no Range/Date/Instant/Duration guard below can
    // match a bare Int/BigInt either.
    if let Some(diff) = big_int_sub(&left, &right) {
        return diff;
    }
    if let (Some(a), Some(b)) = (instance_instant_raw(&left), instance_instant_raw(&right)) {
        return make_duration_real(&value_sub(a, b));
    }
    if let Some(a) = instance_instant_raw(&left)
        && let Some(dur) = instance_duration_raw_value(&right)
    {
        return make_instant(value_sub(a, dur));
    }
    if let Some(a) = instance_instant_raw(&left)
        && right.is_numeric()
    {
        return make_instant(value_sub(a, right));
    }
    // Duration - Duration returns Duration
    if let (Some(a), Some(b)) = (
        instance_duration_raw_value(&left),
        instance_duration_raw_value(&right),
    ) {
        return make_duration_real(&value_sub(a, b));
    }
    // Duration - Numeric returns Duration
    if let Some(a) = instance_duration_raw_value(&left)
        && right.is_numeric()
    {
        return make_duration_real(&value_sub(a, right));
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
        return rebuild_datetime_like(&left, (ny, nm, nd, nh, nmi, ns, tz));
    }
    if let (Some(a), Some(b)) = (instance_days(&left), instance_days(&right)) {
        return Value::int(a - b);
    }
    if let Some(days) = instance_days(&left)
        && let ValueView::Int(delta) = right.view()
    {
        return rebuild_date_like(&left, days - delta);
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
    if matches!(l.view(), ValueView::Complex(_, _)) || matches!(r.view(), ValueView::Complex(_, _))
    {
        let (ar, ai) = crate::runtime::to_complex_parts(&l).unwrap_or((0.0, 0.0));
        let (br, bi) = crate::runtime::to_complex_parts(&r).unwrap_or((0.0, 0.0));
        Value::complex(ar - br, ai - bi)
    } else if let Some(diff) = big_int_sub(&l, &r) {
        diff
    } else if let (Some((an, ad)), Some((bn, bd))) = (to_big_rat_parts(&l), to_big_rat_parts(&r))
        && needs_bigrat_path(&l, &r)
    {
        let has_fat_rat = is_fat_rat_like(&l) || is_fat_rat_like(&r);
        if has_fat_rat {
            let tmp = make_big_fat_rat(an * bd.clone() - bn * ad.clone(), ad * bd);
            if let ValueView::Rat(n, d) = tmp.view() {
                Value::fat_rat_raw(n, d)
            } else {
                tmp
            }
        } else {
            make_big_rat_arith(an * bd.clone() - bn * ad.clone(), ad * bd)
        }
    } else if let (Some((an, ad)), Some((bn, bd))) = (
        crate::runtime::to_rat_parts(&l),
        crate::runtime::to_rat_parts(&r),
    ) {
        let has_rat = matches!(l.view(), ValueView::Rat(_, _) | ValueView::FatRat(_, _))
            || matches!(r.view(), ValueView::Rat(_, _) | ValueView::FatRat(_, _));
        let has_fat_rat = is_fat_rat_like(&l) || is_fat_rat_like(&r);
        if has_rat {
            if has_fat_rat {
                if let (Some(n), Some(d)) = (
                    an.checked_mul(bd).and_then(|left| {
                        bn.checked_mul(ad).and_then(|right| left.checked_sub(right))
                    }),
                    ad.checked_mul(bd),
                ) {
                    make_fat_rat(n, d)
                } else {
                    let n = NumBigInt::from(an) * NumBigInt::from(bd)
                        - NumBigInt::from(bn) * NumBigInt::from(ad);
                    let d = NumBigInt::from(ad) * NumBigInt::from(bd);
                    let result = make_big_fat_rat(n, d);
                    if let ValueView::Rat(n, d) = result.view() {
                        Value::fat_rat_raw(n, d)
                    } else {
                        result
                    }
                }
            } else {
                rat_sub_checked(an, ad, bn, bd)
            }
        } else {
            match (l.view(), r.view()) {
                (ValueView::Int(a), ValueView::Int(b)) => match a.checked_sub(b) {
                    Some(diff) => Value::int(diff),
                    None => Value::from_bigint(
                        num_bigint::BigInt::from(a) - num_bigint::BigInt::from(b),
                    ),
                },
                (ValueView::Num(a), ValueView::Num(b)) => Value::num(a - b),
                (ValueView::Int(a), ValueView::Num(b)) => Value::num(a as f64 - b),
                (ValueView::Num(a), ValueView::Int(b)) => Value::num(a - b as f64),
                _ => Value::int(0),
            }
        }
    } else if let Some(diff) = big_int_sub(&l, &r) {
        diff
    } else {
        let lf = crate::runtime::to_float_value(&l);
        let rf = crate::runtime::to_float_value(&r);
        if let (Some(a), Some(b)) = (lf, rf) {
            Value::num(a - b)
        } else {
            match (l.view(), r.view()) {
                (ValueView::Int(a), ValueView::Int(b)) => match a.checked_sub(b) {
                    Some(diff) => Value::int(diff),
                    None => Value::from_bigint(
                        num_bigint::BigInt::from(a) - num_bigint::BigInt::from(b),
                    ),
                },
                (ValueView::Num(a), ValueView::Num(b)) => Value::num(a - b),
                (ValueView::Int(a), ValueView::Num(b)) => Value::num(a as f64 - b),
                (ValueView::Num(a), ValueView::Int(b)) => Value::num(a - b as f64),
                _ => Value::int(0),
            }
        }
    }
}
