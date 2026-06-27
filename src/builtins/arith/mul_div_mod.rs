#![allow(clippy::result_large_err)]
//! Multiplication, division, and modulo arithmetic operators.

use super::range::{mixin_range_arith, mixin_range_arith_val, range_divide, range_scale};
use super::rat::{
    as_bigint, float_mod_floor, is_fat_rat_like, make_fat_rat, needs_bigrat_path, rat_mul_checked,
    real_to_rat, to_big_rat_parts,
};
use super::temporal::{instance_duration_raw_value, make_duration_from_value};
use crate::value::{RuntimeError, Value, make_big_fat_rat, make_big_rat_arith};
use num_bigint::BigInt as NumBigInt;
use num_traits::Zero;

pub(crate) fn arith_mul(left: Value, right: Value) -> Value {
    let (left, right) = (left.into_deref(), right.into_deref());
    // Mixin-wrapped Range * Real: perform Range arithmetic and re-wrap
    if let Some(result) = mixin_range_arith_val(left.clone(), right.clone(), arith_mul)
        .or_else(|| mixin_range_arith_val(right.clone(), left.clone(), arith_mul))
    {
        return result;
    }
    // Range * Numeric: scale both bounds, preserving exclusivity
    if let Some(range) = range_scale(&left, &right).or_else(|| range_scale(&right, &left)) {
        return range;
    }
    let (l, r) = crate::runtime::coerce_numeric(left, right);
    if matches!(l, Value::Complex(_, _)) || matches!(r, Value::Complex(_, _)) {
        let (ar, ai) = crate::runtime::to_complex_parts(&l).unwrap_or((0.0, 0.0));
        let (br, bi) = crate::runtime::to_complex_parts(&r).unwrap_or((0.0, 0.0));
        Value::Complex(ar * br - ai * bi, ar * bi + ai * br)
    } else if (matches!(l, Value::BigInt(_)) || matches!(r, Value::BigInt(_)))
        && let (Some(a), Some(b)) = (as_bigint(&l), as_bigint(&r))
    {
        Value::from_bigint(a * b)
    } else if let (Some((an, ad)), Some((bn, bd))) = (to_big_rat_parts(&l), to_big_rat_parts(&r))
        && needs_bigrat_path(&l, &r)
    {
        let has_fat_rat = is_fat_rat_like(&l) || is_fat_rat_like(&r);
        if has_fat_rat {
            match make_big_fat_rat(an * bn, ad * bd) {
                Value::Rat(n, d) => Value::FatRat(n, d),
                other => other,
            }
        } else {
            make_big_rat_arith(an * bn, ad * bd)
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
        let lf = crate::runtime::to_float_value(&l);
        let rf = crate::runtime::to_float_value(&r);
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
    let (left, right) = (left.into_deref(), right.into_deref());
    // Mixin-wrapped Range / Real: perform Range arithmetic and re-wrap
    if let Some(result) = mixin_range_arith(left.clone(), right.clone(), arith_div) {
        return result;
    }
    // Range / Numeric: scale both bounds down
    if let Some(range) = range_divide(&left, &right) {
        return Ok(range);
    }
    let (l, r) = crate::runtime::coerce_numeric(left, right);
    if matches!(l, Value::Complex(_, _)) || matches!(r, Value::Complex(_, _)) {
        let (ar, ai) = crate::runtime::to_complex_parts(&l).unwrap_or((0.0, 0.0));
        let (br, bi) = crate::runtime::to_complex_parts(&r).unwrap_or((0.0, 0.0));
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
            let lf = crate::runtime::to_float_value(&l).unwrap_or(0.0);
            let rf = crate::runtime::to_float_value(&r).unwrap_or(1.0);
            if rf == 0.0 {
                return Ok(RuntimeError::divide_by_zero_failure(None, None));
            }
            return Ok(Value::Num(lf / rf));
        }
        let has_fat_rat = is_fat_rat_like(&l) || is_fat_rat_like(&r);
        let has_big_rat = matches!(l, Value::BigRat(_, _)) || matches!(r, Value::BigRat(_, _));
        if (has_fat_rat || has_big_rat)
            && let (Some((an, ad)), Some((bn, bd))) = (to_big_rat_parts(&l), to_big_rat_parts(&r))
        {
            let result = if has_fat_rat {
                make_big_fat_rat(an * bd, ad * bn)
            } else {
                make_big_rat_arith(an * bd, ad * bn)
            };
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
            // `/` on integers always yields a Rat, even when it divides evenly:
            // `555…555 / 5` is `Rat 111…111/1`, not an Int. Collapsing an
            // exact big quotient to a BigInt here lost the Rat-ness (its
            // `.WHAT` became `Int` and `.raku` dropped the trailing `.0`).
            return Ok(make_big_rat_arith(a, b));
        }
        if let (Some((an, ad)), Some((bn, bd))) = (
            crate::runtime::to_rat_parts(&l),
            crate::runtime::to_rat_parts(&r),
        ) {
            let has_rat = matches!(l, Value::Rat(_, _) | Value::FatRat(_, _))
                || matches!(r, Value::Rat(_, _) | Value::FatRat(_, _));
            let has_fat_rat = is_fat_rat_like(&l) || is_fat_rat_like(&r);
            if has_rat || matches!((&l, &r), (Value::Int(_), Value::Int(_))) {
                return Ok(if has_fat_rat {
                    if let (Some(n), Some(d)) = (an.checked_mul(bd), ad.checked_mul(bn)) {
                        make_fat_rat(n, d)
                    } else {
                        let n = NumBigInt::from(an) * NumBigInt::from(bd);
                        let d = NumBigInt::from(ad) * NumBigInt::from(bn);
                        let result = make_big_fat_rat(n, d);
                        match result {
                            Value::Rat(n, d) => Value::FatRat(n, d),
                            other => other,
                        }
                    }
                } else {
                    super::rat::rat_div_checked(an, ad, bn, bd)
                });
            }
        }
        Ok(match (&l, &r) {
            (Value::Num(a), Value::Num(b)) if *b == 0.0 => {
                return Ok(RuntimeError::divide_by_zero_failure(
                    Some(Value::Num(*a)),
                    Some("/"),
                ));
            }
            (Value::Int(a), Value::Num(b)) if *b == 0.0 => {
                return Ok(RuntimeError::divide_by_zero_failure(
                    Some(Value::Int(*a)),
                    Some("/"),
                ));
            }
            (Value::Num(_), Value::Int(b)) if *b == 0 => {
                return Ok(RuntimeError::divide_by_zero_failure(
                    Some(l.clone()),
                    Some("/"),
                ));
            }
            (Value::Num(a), Value::Num(b)) => Value::Num(a / b),
            (Value::Int(a), Value::Num(b)) => Value::Num(*a as f64 / b),
            (Value::Num(a), Value::Int(b)) => Value::Num(a / *b as f64),
            (Value::Num(a), Value::BigInt(b)) => {
                let bf = crate::runtime::to_float_value(&Value::BigInt(b.clone())).unwrap_or(1.0);
                if bf == 0.0 {
                    return Ok(RuntimeError::divide_by_zero_failure(
                        Some(Value::Num(*a)),
                        Some("/"),
                    ));
                }
                Value::Num(a / bf)
            }
            (Value::BigInt(a), Value::Num(b)) => {
                if *b == 0.0 {
                    return Ok(RuntimeError::divide_by_zero_failure(
                        Some(Value::BigInt(a.clone())),
                        Some("/"),
                    ));
                }
                let af = crate::runtime::to_float_value(&Value::BigInt(a.clone())).unwrap_or(0.0);
                Value::Num(af / b)
            }
            _ => Value::Int(0),
        })
    }
}

pub(crate) fn arith_mod(left: Value, right: Value) -> Result<Value, RuntimeError> {
    let (left, right) = (left.into_deref(), right.into_deref());
    if let Some(raw) = instance_duration_raw_value(&left)
        && right.is_numeric()
    {
        // Duration % Real => Duration. Compute with exact rational arithmetic
        // (the same code path as a plain modulo) so the result is eqv to
        // Duration.new($seconds % $real). arith_mod throws X::Numeric::DivideByZero
        // when the divisor is zero.
        let modded = arith_mod(real_to_rat(&raw), right)?;
        return Ok(make_duration_from_value(real_to_rat(&modded)));
    }
    let (mut l, mut r) = crate::runtime::coerce_numeric(left, right);
    // Mixed Num/Rat modulo should use floating semantics; routing through
    // exact-rational reduction loses expected precision behavior for cases like
    // 1.01 % 0.2 (should be ~0.01).
    if matches!(l, Value::Num(_))
        && matches!(
            r,
            Value::Rat(_, _) | Value::FatRat(_, _) | Value::BigRat(_, _)
        )
    {
        r = Value::Num(crate::runtime::to_float_value(&r).unwrap_or(f64::NAN));
    } else if matches!(r, Value::Num(_))
        && matches!(
            l,
            Value::Rat(_, _) | Value::FatRat(_, _) | Value::BigRat(_, _)
        )
    {
        l = Value::Num(crate::runtime::to_float_value(&l).unwrap_or(f64::NAN));
    }
    // Exact rational modulo (divisor-sign semantics) via big-rational parts so
    // that large operands such as `(7/1) % (2**66)` are handled exactly instead
    // of falling through to a 0 result.
    let l_is_rat = matches!(
        l,
        Value::Rat(_, _) | Value::FatRat(_, _) | Value::BigRat(_, _)
    );
    let r_is_rat = matches!(
        r,
        Value::Rat(_, _) | Value::FatRat(_, _) | Value::BigRat(_, _)
    );
    if (l_is_rat || r_is_rat)
        && let (Some((an, ad)), Some((bn, bd))) = (to_big_rat_parts(&l), to_big_rat_parts(&r))
    {
        if bn.is_zero() {
            return Err(RuntimeError::numeric_divide_by_zero());
        }
        let num = num_integer::Integer::mod_floor(&(&an * &bd), &(&ad * &bn));
        let den = &ad * &bd;
        let has_fat_rat = is_fat_rat_like(&l) || is_fat_rat_like(&r);
        return Ok(if has_fat_rat {
            // FatRat % ... stays a FatRat. make_big_fat_rat normalizes small
            // results down to Rat, so re-tag those as FatRat.
            match crate::value::make_big_fat_rat(num, den) {
                Value::Rat(n, d) => Value::FatRat(n, d),
                other => other,
            }
        } else {
            crate::value::make_big_rat_arith(num, den)
        });
    }
    {
        // Integer `%` by zero reports the dividend and `using %`, like Rakudo
        // (`Attempt to divide 7 by zero using %`).
        let mod_div0 = |dividend: &Value| {
            RuntimeError::numeric_divide_by_zero_full(Some(dividend.clone()), Some("%"))
        };
        Ok(match (l, r) {
            (Value::Int(a), Value::Int(0)) => {
                return Err(mod_div0(&Value::Int(a)));
            }
            (Value::BigInt(a), Value::Int(0)) => {
                return Err(mod_div0(&Value::from_bigint((*a).clone())));
            }
            (Value::Int(a), Value::BigInt(b)) if b.is_zero() => {
                return Err(mod_div0(&Value::Int(a)));
            }
            (Value::BigInt(a), Value::BigInt(b)) if b.is_zero() => {
                return Err(mod_div0(&Value::from_bigint((*a).clone())));
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
            (Value::Num(a), Value::Num(0.0)) => {
                return Ok(RuntimeError::divide_by_zero_failure(
                    Some(Value::Num(a)),
                    Some("%"),
                ));
            }
            (Value::Num(a), Value::Num(b)) => Value::Num(float_mod_floor(a, b)),
            (ref lv, Value::Num(0.0)) => {
                return Ok(RuntimeError::divide_by_zero_failure(
                    Some(lv.clone()),
                    Some("%"),
                ));
            }
            (Value::Int(a), Value::Num(b)) => Value::Num(float_mod_floor(a as f64, b)),
            (Value::Num(a), Value::Int(b)) => Value::Num(float_mod_floor(a, b as f64)),
            _ => Value::Int(0),
        })
    }
}
