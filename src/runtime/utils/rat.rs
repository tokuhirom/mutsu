use super::*;

/// Build an `X::Str::Numeric` error for a string that cannot be used as a
/// number, carrying rakudo's `source`, `pos` and `reason` attributes.
pub(crate) fn str_numeric_error(source: &str, pos: usize, reason: &str) -> RuntimeError {
    let msg = format!(
        "Cannot convert string to number: {} in '{}'",
        reason, source
    );
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("source".to_string(), Value::str(source.to_string()));
    attrs.insert("pos".to_string(), Value::Int(pos as i64));
    attrs.insert("reason".to_string(), Value::str(reason.to_string()));
    attrs.insert("target-name".to_string(), Value::str("Numeric".to_string()));
    attrs.insert(
        "source-indicator".to_string(),
        Value::str(crate::runtime::str_numeric::build_source_indicator(
            source, pos,
        )),
    );
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(crate::symbol::Symbol::intern("X::Str::Numeric"), attrs);
    let mut err = RuntimeError::new(&msg);
    err.exception = Some(Box::new(ex));
    err
}

/// Raise `X::Str::Numeric` if `value` is a `Str` (or allomorph wrapping one)
/// that cannot be parsed as a number. Used by the genuinely-numeric operators
/// (`+ - * / % **`, `== != < > <= >= <=>`) so `"5 foo" + 8` fails the way Raku
/// requires. The generic comparators (`cmp`, `before`/`after`, `min`/`max`) do
/// NOT call this — they compare strings as strings.
pub(crate) fn check_str_numeric(value: &Value) -> Result<(), RuntimeError> {
    // Hot path: only a bare or Mixin-wrapped Str can fail; everything else
    // (Int/Num/Rat/...) returns immediately without cloning or parsing.
    let s = match value {
        Value::Str(s) => s,
        Value::Mixin(inner, _) => match inner.as_ref() {
            Value::Str(s) => s,
            _ => return Ok(()),
        },
        _ => return Ok(()),
    };
    if let Some((pos, reason)) = crate::runtime::str_numeric::str_numeric_failure(s) {
        return Err(str_numeric_error(s, pos, &reason));
    }
    Ok(())
}

pub(crate) fn coerce_numeric(left: Value, right: Value) -> (Value, Value) {
    // Unwrap allomorphic types (Mixin) to their inner numeric value
    let left = unwrap_mixin(left);
    let right = unwrap_mixin(right);
    let l = match &left {
        Value::Int(_)
        | Value::BigInt(_)
        | Value::Num(_)
        | Value::Rat(_, _)
        | Value::FatRat(_, _)
        | Value::BigRat(_, _)
        | Value::Complex(_, _) => left,
        _ => coerce_to_numeric(left),
    };
    let r = match &right {
        Value::Int(_)
        | Value::BigInt(_)
        | Value::Num(_)
        | Value::Rat(_, _)
        | Value::FatRat(_, _)
        | Value::BigRat(_, _)
        | Value::Complex(_, _) => right,
        _ => coerce_to_numeric(right),
    };
    (l, r)
}

/// Unwrap a Mixin (allomorphic type) to its inner value.
pub(crate) fn unwrap_mixin(val: Value) -> Value {
    match val {
        Value::Mixin(inner, _) => inner.as_ref().clone(),
        other => other,
    }
}

pub(crate) fn to_rat_parts(val: &Value) -> Option<(i64, i64)> {
    match val {
        Value::Mixin(inner, _) => to_rat_parts(inner),
        Value::Int(i) => Some((*i, 1)),
        Value::Rat(n, d) => Some((*n, *d)),
        Value::FatRat(n, d) => Some((*n, *d)),
        _ => None,
    }
}

pub(crate) fn to_big_rat_parts(val: &Value) -> Option<(BigInt, BigInt)> {
    match val {
        Value::Mixin(inner, _) => to_big_rat_parts(inner),
        Value::Int(i) => Some((BigInt::from(*i), BigInt::from(1))),
        Value::BigInt(i) => Some(((**i).clone(), BigInt::from(1))),
        Value::Rat(n, d) | Value::FatRat(n, d) => Some((BigInt::from(*n), BigInt::from(*d))),
        Value::BigRat(n, d) => Some(((**n).clone(), (**d).clone())),
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
        num.to_f64().unwrap_or(0.0) / den.to_f64().unwrap_or(1.0)
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
