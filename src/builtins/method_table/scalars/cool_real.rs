//! `Cool`'s numeric methods: `abs`, `sign`, `floor`, `ceiling`, `truncate`,
//! `round` and `round($scale)`, plus `Bool.succ` and `Bool.pred`.
//!
//! Rakudo's `Cool` bodies are `self.Numeric.METHOD`, so each row reads its
//! receiver through [`numify`] and calls the numeric types' implementation
//! (`real::abs_of` and friends, which the `Int`/`Num`/`Rat`/`Complex` rows use
//! too). A `Str` parses, a `List`, `Array` or `Hash` is its element count, and
//! a non-numeric `Str` is the `X::Str::Numeric` `Failure`.

use super::numify::numify;
use super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! row {
    ($owner:literal, $name:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Pure($handler),
            flags: RowFlags::NONE,
            named: &[],
        }
    };
}

pub(super) static COOL_ROWS: &[MethodRow] = &[
    row!("Cool", "abs", abs),
    row!("Cool", "sign", sign),
    row!("Cool", "floor", floor),
    row!("Cool", "ceiling", ceiling),
    row!("Cool", "truncate", truncate),
    row!("Cool", "round", round),
    MethodRow {
        owner: "Cool",
        name: "round",
        arity: 1,
        handler: Handler::Narrow(round_to),
        flags: RowFlags::ANY_ARGS,
        named: &[],
    },
];

/// `Bool` declares `succ` and `pred` itself: `True` and `False`.
pub(super) static BOOL_ROWS: &[MethodRow] = &[
    row!("Bool", "succ", bool_succ),
    row!("Bool", "pred", bool_pred),
];

/// A numeric method of the numified receiver. `f` answers a numeric view and
/// declines (`None`) anything else.
// Cost: O(1) (O(n) for a Str receiver, n = chars of the parse; see `f`).
fn with_number(
    target: &Value,
    method: &str,
    f: fn(&Value) -> Option<Result<Value, RuntimeError>>,
) -> Result<Value, RuntimeError> {
    match numify(target) {
        Ok(number) => {
            f(&number).unwrap_or_else(|| Err(RuntimeError::new(format!("{method}: not a number"))))
        }
        Err(failure) => Ok(failure),
    }
}

fn abs(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    with_number(target, "abs", super::real::abs_of)
}

fn sign(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    with_number(target, "sign", super::real::sign_of)
}

fn floor(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    with_number(target, "floor", super::real::floor_of)
}

fn ceiling(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    with_number(target, "ceiling", super::real::ceiling_of)
}

fn truncate(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    with_number(target, "truncate", super::real::truncate_of)
}

fn round(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    with_number(target, "round", super::real::round_of)
}

/// `Bool.succ`: `True`.
// Cost: O(1).
fn bool_succ(_target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::TRUE)
}

/// `Bool.pred`: `False`.
// Cost: O(1).
fn bool_pred(_target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::FALSE)
}

/// How `round($scale)`'s result is typed: by the scale's type.
#[derive(Clone, Copy)]
enum ScaleType {
    Int,
    Num,
    Rat,
}

/// `Cool.round($scale)`: the receiver rounded to a multiple of the scale
/// (`Real.round(Real)`). The result takes the scale's type; a `Complex`
/// receiver rounds each part. An allomorph (`IntStr`, ...) is read as its
/// number. `None` for a scale or receiver that is not a number.
// Cost: O(1) for word-sized values; O(b) for big ones, b = size in bits (the
// exact path is O(b^2) for rationals).
pub(crate) fn round_to(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let [scale_arg] = args else {
        return None;
    };
    // A `Str` scale numifies like a `Str` receiver does.
    let scale_number = match scale_arg.view() {
        ValueView::Str(s) => match super::numify::numify_str(&s) {
            Some(number) => number,
            None => return None,
        },
        _ => scale_arg.clone(),
    };
    let scale_val = unwrap_allomorph(&scale_number);
    let number = match numify(target) {
        Ok(number) => number,
        Err(failure) => return Some(Ok(failure)),
    };
    let exact_target = unwrap_allomorph(&number);
    // Integer and rational rounding stays exact (Rakudo's `Real.round(Real)`):
    // no f64 precision loss for large Ints or Rat scales such as
    // `round(1000, 23.01)` (989.43, not 989.4300000000001).
    if let Some(exact) = crate::builtins::arith::exact_round_scaled(exact_target, scale_val) {
        return Some(Ok(exact));
    }
    let scale_type = match scale_val.view() {
        ValueView::Int(_) | ValueView::BigInt(_) => ScaleType::Int,
        ValueView::Num(_) | ValueView::Complex(..) => ScaleType::Num,
        ValueView::Rat(..) | ValueView::FatRat(..) | ValueView::BigRat(..) => ScaleType::Rat,
        _ => return None,
    };
    let scale = real_part(scale_val)?;
    // A `Complex` receiver always answers a `Complex`.
    if let ValueView::Complex(re, im) = exact_target.view() {
        return Some(Ok(Value::complex(
            round_real(re, scale, scale_val),
            round_real(im, scale, scale_val),
        )));
    }
    let x = real_part(exact_target)?;
    let result = round_real(x, scale, scale_val);
    Some(Ok(match scale_type {
        ScaleType::Int => {
            let r = result.floor();
            if r >= i64::MIN as f64 && r <= i64::MAX as f64 {
                Value::int(r as i64)
            } else {
                Value::num(r)
            }
        }
        ScaleType::Num => Value::num(result),
        ScaleType::Rat => {
            let (n, d) = crate::builtins::methods_narg::f64_to_rat(result);
            Value::rat_raw(n, d)
        }
    }))
}

/// The number inside an allomorph, or the value itself.
fn unwrap_allomorph(value: &Value) -> &Value {
    match value.view() {
        ValueView::Mixin(inner, _) => inner,
        _ => value,
    }
}

/// A real number's `f64` value (a `Complex` scale is its real part), `None`
/// for anything that is not numeric or has a zero denominator.
fn real_part(value: &Value) -> Option<f64> {
    Some(match value.view() {
        ValueView::Int(i) => i as f64,
        ValueView::Bool(b) => f64::from(u8::from(b)),
        ValueView::BigInt(n) => num_traits::ToPrimitive::to_f64(n.as_ref()).unwrap_or(0.0),
        ValueView::Num(f) => f,
        ValueView::Rat(n, d) | ValueView::FatRat(n, d) if d != 0 => crate::value::rat_to_f64(n, d),
        ValueView::BigRat(n, d) if !num_traits::Zero::is_zero(d) => {
            crate::builtins::arith::bigint_ratio_to_f64(n, d)
        }
        ValueView::Complex(re, _) => re,
        _ => return None,
    })
}

/// Round half up (`floor(v + 1/2)`), as `Real.round`.
fn raku_round(v: f64) -> f64 {
    (v + 0.5).floor()
}

/// `x` rounded to a multiple of `scale`. When the scale is an exact rational
/// the final multiplication is `k * num / den`, so a scale like 0.1 (1/10)
/// yields `k / 10` (the nearest double) instead of `k * 0.1`, which carries
/// float noise (`-39 * 0.1` is `-3.9000000000000004`).
fn round_real(x: f64, scale: f64, scale_val: &Value) -> f64 {
    if scale == 0.0 {
        return raku_round(x);
    }
    let k = raku_round(x / scale);
    match scale_val.view() {
        ValueView::Rat(n, d) | ValueView::FatRat(n, d) if d != 0 => k * n as f64 / d as f64,
        ValueView::BigRat(n, d) if !num_traits::Zero::is_zero(d) => {
            k * crate::builtins::arith::bigint_ratio_to_f64(n, d)
        }
        _ => k * scale,
    }
}
