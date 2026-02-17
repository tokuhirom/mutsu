#![allow(clippy::result_large_err)]

use crate::runtime;
use crate::value::{RuntimeError, Value, make_rat};

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
    let (l, r) = runtime::coerce_numeric(left, right);
    Ok(arith_add_coerced(l, r))
}

fn arith_add_coerced(l: Value, r: Value) -> Value {
    if matches!(l, Value::Complex(_, _)) || matches!(r, Value::Complex(_, _)) {
        let (ar, ai) = runtime::to_complex_parts(&l).unwrap_or((0.0, 0.0));
        let (br, bi) = runtime::to_complex_parts(&r).unwrap_or((0.0, 0.0));
        Value::Complex(ar + br, ai + bi)
    } else if let (Some((an, ad)), Some((bn, bd))) =
        (runtime::to_rat_parts(&l), runtime::to_rat_parts(&r))
    {
        if matches!(l, Value::Rat(_, _)) || matches!(r, Value::Rat(_, _)) {
            make_rat(an * bd + bn * ad, ad * bd)
        } else {
            match (l, r) {
                (Value::Int(a), Value::Int(b)) => Value::Int(a.wrapping_add(b)),
                (Value::Num(a), Value::Num(b)) => Value::Num(a + b),
                (Value::Int(a), Value::Num(b)) => Value::Num(a as f64 + b),
                (Value::Num(a), Value::Int(b)) => Value::Num(a + b as f64),
                _ => Value::Int(0),
            }
        }
    } else {
        match (l, r) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a.wrapping_add(b)),
            (Value::Num(a), Value::Num(b)) => Value::Num(a + b),
            (Value::Int(a), Value::Num(b)) => Value::Num(a as f64 + b),
            (Value::Num(a), Value::Int(b)) => Value::Num(a + b as f64),
            _ => Value::Int(0),
        }
    }
}

pub(crate) fn arith_sub(left: Value, right: Value) -> Value {
    let (l, r) = runtime::coerce_numeric(left, right);
    if matches!(l, Value::Complex(_, _)) || matches!(r, Value::Complex(_, _)) {
        let (ar, ai) = runtime::to_complex_parts(&l).unwrap_or((0.0, 0.0));
        let (br, bi) = runtime::to_complex_parts(&r).unwrap_or((0.0, 0.0));
        Value::Complex(ar - br, ai - bi)
    } else if let (Some((an, ad)), Some((bn, bd))) =
        (runtime::to_rat_parts(&l), runtime::to_rat_parts(&r))
    {
        if matches!(l, Value::Rat(_, _)) || matches!(r, Value::Rat(_, _)) {
            make_rat(an * bd - bn * ad, ad * bd)
        } else {
            match (l, r) {
                (Value::Int(a), Value::Int(b)) => Value::Int(a.wrapping_sub(b)),
                (Value::Num(a), Value::Num(b)) => Value::Num(a - b),
                (Value::Int(a), Value::Num(b)) => Value::Num(a as f64 - b),
                (Value::Num(a), Value::Int(b)) => Value::Num(a - b as f64),
                _ => Value::Int(0),
            }
        }
    } else {
        match (l, r) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a.wrapping_sub(b)),
            (Value::Num(a), Value::Num(b)) => Value::Num(a - b),
            (Value::Int(a), Value::Num(b)) => Value::Num(a as f64 - b),
            (Value::Num(a), Value::Int(b)) => Value::Num(a - b as f64),
            _ => Value::Int(0),
        }
    }
}

pub(crate) fn arith_mul(left: Value, right: Value) -> Value {
    let (l, r) = runtime::coerce_numeric(left, right);
    if matches!(l, Value::Complex(_, _)) || matches!(r, Value::Complex(_, _)) {
        let (ar, ai) = runtime::to_complex_parts(&l).unwrap_or((0.0, 0.0));
        let (br, bi) = runtime::to_complex_parts(&r).unwrap_or((0.0, 0.0));
        Value::Complex(ar * br - ai * bi, ar * bi + ai * br)
    } else if let (Some((an, ad)), Some((bn, bd))) =
        (runtime::to_rat_parts(&l), runtime::to_rat_parts(&r))
    {
        if matches!(l, Value::Rat(_, _)) || matches!(r, Value::Rat(_, _)) {
            make_rat(an * bn, ad * bd)
        } else {
            match (l, r) {
                (Value::Int(a), Value::Int(b)) => Value::Int(a.wrapping_mul(b)),
                (Value::Num(a), Value::Num(b)) => Value::Num(a * b),
                (Value::Int(a), Value::Num(b)) => Value::Num(a as f64 * b),
                (Value::Num(a), Value::Int(b)) => Value::Num(a * b as f64),
                _ => Value::Int(0),
            }
        }
    } else {
        match (l, r) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a.wrapping_mul(b)),
            (Value::Num(a), Value::Num(b)) => Value::Num(a * b),
            (Value::Int(a), Value::Num(b)) => Value::Num(a as f64 * b),
            (Value::Num(a), Value::Int(b)) => Value::Num(a * b as f64),
            _ => Value::Int(0),
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
            return Err(RuntimeError::new("Division by zero"));
        }
        Ok(Value::Complex(
            (ar * br + ai * bi) / denom,
            (ai * br - ar * bi) / denom,
        ))
    } else {
        Ok(match (&l, &r) {
            (Value::Rat(_, _), _) | (_, Value::Rat(_, _)) | (Value::Int(_), Value::Int(_)) => {
                let (an, ad) = runtime::to_rat_parts(&l).unwrap_or((0, 1));
                let (bn, bd) = runtime::to_rat_parts(&r).unwrap_or((0, 1));
                let new_d = ad * bn;
                if new_d == 0 {
                    // Rat with zero denominator: Raku allows this (numifies to Inf/-Inf/NaN)
                    Value::Rat(an * bd, 0)
                } else {
                    make_rat(an * bd, new_d)
                }
            }
            (Value::Num(a), Value::Num(b)) => Value::Num(a / b),
            (Value::Int(a), Value::Num(b)) => Value::Num(*a as f64 / b),
            (Value::Num(a), Value::Int(b)) => Value::Num(a / *b as f64),
            _ => Value::Int(0),
        })
    }
}

pub(crate) fn arith_mod(left: Value, right: Value) -> Result<Value, RuntimeError> {
    let (l, r) = runtime::coerce_numeric(left, right);
    if let (Some((an, ad)), Some((bn, bd))) = (runtime::to_rat_parts(&l), runtime::to_rat_parts(&r))
    {
        if matches!(l, Value::Rat(_, _)) || matches!(r, Value::Rat(_, _)) {
            if bn == 0 {
                return Err(RuntimeError::new("Modulo by zero"));
            }
            let lf = an as f64 / ad as f64;
            let rf = bn as f64 / bd as f64;
            Ok(Value::Num(lf % rf))
        } else {
            Ok(match (l, r) {
                (Value::Int(_), Value::Int(0)) => {
                    return Err(RuntimeError::new("Modulo by zero"));
                }
                (Value::Int(a), Value::Int(b)) => Value::Int(a % b),
                (Value::Num(a), Value::Num(b)) => Value::Num(a % b),
                (Value::Int(a), Value::Num(b)) => Value::Num(a as f64 % b),
                (Value::Num(a), Value::Int(b)) => Value::Num(a % b as f64),
                _ => Value::Int(0),
            })
        }
    } else {
        Ok(match (l, r) {
            (Value::Int(_), Value::Int(0)) => {
                return Err(RuntimeError::new("Modulo by zero"));
            }
            (Value::Int(a), Value::Int(b)) => Value::Int(a % b),
            (Value::Num(a), Value::Num(b)) => Value::Num(a % b),
            (Value::Int(a), Value::Num(b)) => Value::Num(a as f64 % b),
            (Value::Num(a), Value::Int(b)) => Value::Num(a % b as f64),
            _ => Value::Int(0),
        })
    }
}

pub(crate) fn arith_pow(left: Value, right: Value) -> Value {
    let (l, r) = runtime::coerce_numeric(left, right);
    if matches!(l, Value::Complex(_, _)) || matches!(r, Value::Complex(_, _)) {
        let (ar, ai) = runtime::to_complex_parts(&l).unwrap_or((0.0, 0.0));
        let (br, bi) = runtime::to_complex_parts(&r).unwrap_or((0.0, 0.0));
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
                    Value::BigInt(base.pow(b as u32))
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
                    Value::Num((n as f64 / d as f64).powi(b as i32))
                }
            }
            (Value::Rat(n, d), Value::Int(b)) => {
                let p = (-b) as u32;
                if let (Some(dp), Some(np)) = (d.checked_pow(p), n.checked_pow(p)) {
                    make_rat(dp, np)
                } else {
                    Value::Num((d as f64 / n as f64).powi(p as i32))
                }
            }
            (Value::Num(a), Value::Int(b)) => Value::Num(a.powi(b as i32)),
            (Value::Int(a), Value::Num(b)) => Value::Num((a as f64).powf(b)),
            (Value::Num(a), Value::Num(b)) => Value::Num(a.powf(b)),
            _ => Value::Int(0),
        }
    }
}

pub(crate) fn arith_negate(val: Value) -> Result<Value, RuntimeError> {
    match val {
        Value::Int(i) => Ok(Value::Int(-i)),
        Value::Num(f) => Ok(Value::Num(-f)),
        Value::Rat(n, d) => Ok(Value::Rat(-n, d)),
        Value::Complex(r, i) => Ok(Value::Complex(-r, -i)),
        Value::Str(ref s) => {
            if let Ok(i) = s.trim().parse::<i64>() {
                Ok(Value::Int(-i))
            } else if let Ok(f) = s.trim().parse::<f64>() {
                Ok(Value::Num(-f))
            } else {
                Err(RuntimeError::new("Unary - expects numeric"))
            }
        }
        _ => Err(RuntimeError::new("Unary - expects numeric")),
    }
}
