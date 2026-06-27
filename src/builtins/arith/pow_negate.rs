#![allow(clippy::result_large_err)]
//! Power (exponentiation) and unary negation operators.

use super::rat::{bigint_ratio_to_f64, is_fat_rat_like, make_fat_rat};
use crate::value::{RuntimeError, Value, make_big_fat_rat, make_big_rat_arith, make_rat};
use num_bigint::{BigInt as NumBigInt, Sign};
use num_traits::{ToPrimitive, Zero};

pub(crate) fn arith_pow(left: Value, right: Value) -> Value {
    let (left, right) = (left.into_deref(), right.into_deref());
    let (l, r) = crate::runtime::coerce_numeric(left, right);
    let to_pow_f64 = |v: &Value| -> Option<f64> {
        crate::runtime::to_float_value(v).or_else(|| match v {
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
        let (ar, ai) = crate::runtime::to_complex_parts(&l).unwrap_or((0.0, 0.0));
        let (br, bi) = crate::runtime::to_complex_parts(&r).unwrap_or((0.0, 0.0));
        // Special case: 0 ** anything = 0+0i (except 0**0 = 1+0i)
        if ar == 0.0 && ai == 0.0 {
            return if br == 0.0 && bi == 0.0 {
                Value::Complex(1.0, 0.0)
            } else {
                Value::Complex(0.0, 0.0)
            };
        }
        // Integer real exponent: compute by repeated multiplication
        // (exponentiation by squaring) so the result stays exact, matching
        // raku. The polar `exp(b*ln a)` form below introduces floating-point
        // noise (e.g. `(0+1i)**2` would be `-1+1.2e-16i` instead of `-1+0i`).
        if bi == 0.0 && br.fract() == 0.0 && br.abs() <= 1024.0 {
            let cmul = |xr: f64, xi: f64, yr: f64, yi: f64| -> (f64, f64) {
                (xr * yr - xi * yi, xr * yi + xi * yr)
            };
            let mut e = br.abs() as u64;
            let (mut rr, mut ri) = (1.0_f64, 0.0_f64);
            let (mut pr, mut pi) = (ar, ai);
            while e > 0 {
                if e & 1 == 1 {
                    let (nr, ni) = cmul(rr, ri, pr, pi);
                    rr = nr;
                    ri = ni;
                }
                let (sr, si) = cmul(pr, pi, pr, pi);
                pr = sr;
                pi = si;
                e >>= 1;
            }
            if br < 0.0 {
                // Reciprocal: 1 / (rr + ri·i) = (rr - ri·i) / (rr² + ri²).
                let denom = rr * rr + ri * ri;
                return Value::Complex(rr / denom, -ri / denom);
            }
            return Value::Complex(rr, ri);
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
                    match make_big_rat_arith(nn, dd) {
                        // If make_big_rat reduced it to fit in i64, keep as Rat
                        Value::Rat(rn, rd) => Value::Rat(rn, rd),
                        // BigRat means denom exceeds i64: degrade to Num
                        Value::BigRat(bn, bd) => {
                            // Check if denom fits in i64 (numerator can be big)
                            if let Some(d64) = bd.to_i64() {
                                Value::bigrat(*bn, NumBigInt::from(d64))
                            } else {
                                Value::Num(bigint_ratio_to_f64(bn.as_ref(), bd.as_ref()))
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
                    match make_big_rat_arith(nn, dd) {
                        Value::Rat(rn, rd) => Value::Rat(rn, rd),
                        Value::BigRat(bn, bd) => {
                            if let Some(d64) = bd.to_i64() {
                                Value::bigrat(*bn, NumBigInt::from(d64))
                            } else {
                                Value::Num(bigint_ratio_to_f64(bn.as_ref(), bd.as_ref()))
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
                    match make_big_fat_rat(nn, dd) {
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
                    match make_big_fat_rat(nn, dd) {
                        Value::Rat(nn, dd) => Value::FatRat(nn, dd),
                        other => other,
                    }
                }
            }
            (Value::BigRat(n, d), Value::Int(b)) if b >= 0 => {
                let p = b as u32;
                make_big_rat_arith(n.pow(p), d.pow(p))
            }
            (Value::BigRat(n, d), Value::Int(b)) => {
                let p = (-b) as u32;
                make_big_rat_arith(d.pow(p), n.pow(p))
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
            (Value::Num(a), Value::Num(b)) => {
                let result = a.powf(b);
                if result == 0.0 && a != 0.0 && b.is_finite() && b < 0.0 {
                    RuntimeError::numeric_underflow_failure()
                } else {
                    Value::Num(result)
                }
            }
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
        Value::BigRat(ref n, ref d) => {
            if is_fat_rat_like(&val) {
                Ok(make_big_fat_rat(-(**n).clone(), (**d).clone()))
            } else {
                Ok(make_big_rat_arith(-(**n).clone(), (**d).clone()))
            }
        }
        Value::Complex(r, i) => {
            // Canonicalize -0.0 to 0.0 so that e.g. (-i).reals gives (0e0, -1e0)
            let nr = if r == 0.0 { 0.0 } else { -r };
            let ni = if i == 0.0 { 0.0 } else { -i };
            Ok(Value::Complex(nr, ni))
        }
        Value::Str(ref s) => {
            if let Ok(i) = s.trim().parse::<i64>() {
                Ok(Value::Int(-i))
            } else if let Ok(f) = s.trim().parse::<f64>() {
                Ok(Value::Num(-f))
            } else {
                Ok(Value::Int(0))
            }
        }
        other => Ok(Value::Int(-crate::runtime::to_int(&other))),
    }
}
