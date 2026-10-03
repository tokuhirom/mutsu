//! Numeric operand coercion and exact rational comparison. Pure functions of
//! the values, so they live in `value` (#10779); `runtime::utils` re-exports them.

use crate::value::radix_numeric::coerce_to_numeric;
use crate::value::{Value, ValueView};
use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::{Signed, Zero};

pub(crate) fn coerce_numeric(left: Value, right: Value) -> (Value, Value) {
    // Unwrap allomorphic types (Mixin) to their inner numeric value
    let left = unwrap_mixin(left);
    let right = unwrap_mixin(right);
    let l = if matches!(
        left.view(),
        ValueView::Int(_)
            | ValueView::BigInt(_)
            | ValueView::Num(_)
            | ValueView::Rat(_, _)
            | ValueView::FatRat(_, _)
            | ValueView::BigRat(_, _)
            | ValueView::Complex(_, _)
    ) {
        left
    } else {
        coerce_to_numeric(left)
    };
    let r = if matches!(
        right.view(),
        ValueView::Int(_)
            | ValueView::BigInt(_)
            | ValueView::Num(_)
            | ValueView::Rat(_, _)
            | ValueView::FatRat(_, _)
            | ValueView::BigRat(_, _)
            | ValueView::Complex(_, _)
    ) {
        right
    } else {
        coerce_to_numeric(right)
    };
    (l, r)
}

/// Unwrap a Mixin (allomorphic type) to its inner value.
pub(crate) fn unwrap_mixin(val: Value) -> Value {
    if let ValueView::Mixin(inner, _) = val.view() {
        return inner.as_ref().clone();
    }
    val
}

pub(crate) fn to_rat_parts(val: &Value) -> Option<(i64, i64)> {
    match val.view() {
        ValueView::Mixin(inner, _) => to_rat_parts(inner),
        ValueView::Int(i) => Some((i, 1)),
        ValueView::Rat(n, d) => Some((n, d)),
        ValueView::FatRat(n, d) => Some((n, d)),
        _ => None,
    }
}

pub(crate) fn to_big_rat_parts(val: &Value) -> Option<(BigInt, BigInt)> {
    match val.view() {
        ValueView::Mixin(inner, _) => to_big_rat_parts(inner),
        ValueView::Int(i) => Some((BigInt::from(i), BigInt::from(1))),
        ValueView::BigInt(i) => Some(((**i).clone(), BigInt::from(1))),
        ValueView::Rat(n, d) | ValueView::FatRat(n, d) => Some((BigInt::from(n), BigInt::from(d))),
        ValueView::BigRat(n, d) => Some((n.clone(), d.clone())),
        _ => None,
    }
}

fn big_rat_parts_to_f64(num: &BigInt, den: &BigInt) -> f64 {
    if den.is_zero() {
        if num.is_zero() {
            f64::NAN
        } else if num.is_positive() {
            f64::INFINITY
        } else {
            f64::NEG_INFINITY
        }
    } else {
        crate::value::bigrat_to_f64(num, den)
    }
}

pub(crate) fn compare_big_rat_parts(
    a: (BigInt, BigInt),
    b: (BigInt, BigInt),
) -> Option<std::cmp::Ordering> {
    let (an, ad) = a;
    let (bn, bd) = b;
    if ad.is_zero() || bd.is_zero() {
        return big_rat_parts_to_f64(&an, &ad).partial_cmp(&big_rat_parts_to_f64(&bn, &bd));
    }
    Some((an * &bd).cmp(&(bn * &ad)))
}

pub(crate) fn big_rat_parts_equal(a: (BigInt, BigInt), b: (BigInt, BigInt)) -> bool {
    let (an, ad) = a;
    let (bn, bd) = b;
    if ad.is_zero() || bd.is_zero() {
        if (ad.is_zero() && an.is_zero()) || (bd.is_zero() && bn.is_zero()) {
            return false;
        }
        return big_rat_parts_to_f64(&an, &ad) == big_rat_parts_to_f64(&bn, &bd);
    }
    let ga = an.gcd(&ad);
    let gb = bn.gcd(&bd);
    let mut an = an / &ga;
    let mut ad = ad / ga;
    let mut bn = bn / &gb;
    let mut bd = bd / gb;
    if ad.is_negative() {
        an = -an;
        ad = -ad;
    }
    if bd.is_negative() {
        bn = -bn;
        bd = -bd;
    }
    an == bn && ad == bd
}

fn rat_parts_to_f64(num: i64, den: i64) -> f64 {
    if den != 0 {
        num as f64 / den as f64
    } else if num > 0 {
        f64::INFINITY
    } else if num < 0 {
        f64::NEG_INFINITY
    } else {
        f64::NAN
    }
}

pub(crate) fn compare_rat_parts(a: (i64, i64), b: (i64, i64)) -> std::cmp::Ordering {
    let (an, ad) = a;
    let (bn, bd) = b;
    if ad == 0 || bd == 0 {
        return rat_parts_to_f64(an, ad)
            .partial_cmp(&rat_parts_to_f64(bn, bd))
            .unwrap_or(std::cmp::Ordering::Equal);
    }
    let lhs = an as i128 * bd as i128;
    let rhs = bn as i128 * ad as i128;
    lhs.cmp(&rhs)
}
