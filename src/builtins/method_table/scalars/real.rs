//! The numeric types' `Real`/`Numeric` rows: `abs`, `sign`, `floor`,
//! `ceiling`, `round` and `truncate` with no arguments.
//!
//! Rakudo has a copy of each in the method table of `Int`, `Num`, `Rat`,
//! `FatRat` and `Complex` (some written on the type, some composed from the
//! `Real` and `Rational` roles), except `Complex.sign`, which comes from
//! `Cool` and so has no row here. Every owner's row points at one handler per
//! method. The `*_of` functions behind the handlers answer any numeric view,
//! word-sized or big; the cascade arms that still answer receivers with no
//! shape (`Bool`, enums, `Duration`) call them too.

use super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueView};
use num_bigint::BigInt;
use num_traits::{FromPrimitive, Signed, Zero};

/// Zero-argument rows owned by `$owner`.
macro_rules! rows {
    ($owner:literal: $($name:literal => $handler:ident),* $(,)?) => {
        &[$(MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Pure($handler),
            flags: RowFlags::NONE,
            named: &[],
        }),*]
    };
}

/// The rows every real type declares.
macro_rules! real_rows {
    ($owner:literal) => {
        rows![$owner:
            "abs" => abs,
            "sign" => sign,
            "floor" => floor,
            "ceiling" => ceiling,
            "round" => round,
            "truncate" => truncate,
        ]
    };
}

pub(super) static INT_ROWS: &[MethodRow] = real_rows!("Int");
pub(super) static NUM_ROWS: &[MethodRow] = real_rows!("Num");
pub(super) static RAT_ROWS: &[MethodRow] = real_rows!("Rat");
pub(super) static FAT_RAT_ROWS: &[MethodRow] = real_rows!("FatRat");
pub(super) static COMPLEX_ROWS: &[MethodRow] = rows!["Complex":
    "abs" => abs,
    "floor" => floor,
    "ceiling" => ceiling,
    "round" => round,
    "truncate" => truncate,
];

/// The error a row's handler raises for a receiver that is not a number. A
/// row is only resolved for a numeric shape, so this is never reached
/// through the table.
fn not_numeric(method: &str) -> RuntimeError {
    RuntimeError::new(format!("{method}: receiver is not a number"))
}

fn abs(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    abs_of(target).unwrap_or_else(|| Err(not_numeric("abs")))
}
fn sign(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    sign_of(target).unwrap_or_else(|| Err(not_numeric("sign")))
}
fn floor(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    floor_of(target).unwrap_or_else(|| Err(not_numeric("floor")))
}
fn ceiling(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    ceiling_of(target).unwrap_or_else(|| Err(not_numeric("ceiling")))
}
fn round(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    round_of(target).unwrap_or_else(|| Err(not_numeric("round")))
}
fn truncate(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    truncate_of(target).unwrap_or_else(|| Err(not_numeric("truncate")))
}

/// An integral `f64` as an `Int`, big when it does not fit a word.
// Cost: O(1) in range; O(b) otherwise, b = the result's size in bits.
pub(super) fn integral_num_to_int(f: f64) -> Value {
    if f >= -(2f64.powi(63)) && f < 2f64.powi(63) {
        return Value::int(f as i64);
    }
    BigInt::from_f64(f).map_or(Value::num(f), Value::from_bigint)
}

/// Whether an integer value is below zero.
fn value_is_negative(v: &Value) -> bool {
    match v.view() {
        ValueView::Int(i) => i < 0,
        ValueView::BigInt(n) => n.is_negative(),
        _ => false,
    }
}

/// The `Failure` a rational with a zero denominator answers a rounding method
/// with.
fn zero_denominator(method: &str) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(RuntimeError::divide_by_zero_failure_for_method(
        method, "Rational",
    )))
}

/// `Real.abs` / `Complex.abs` on a numeric view, `None` for anything else.
/// A rational keeps its type; a `Complex` answers its magnitude.
// Cost: O(1) for word-sized values; O(b) for big ones, b = size in bits.
pub(crate) fn abs_of(target: &Value) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(match target.view() {
        ValueView::Int(i) => crate::builtins::int_abs(i),
        // `Bool` is an `Int` enum: `True.abs` is the `Int` 1.
        ValueView::Bool(b) => Value::int(i64::from(b)),
        ValueView::BigInt(n) => Value::bigint(n.as_ref().abs()),
        ValueView::Num(f) => Value::num(f.abs()),
        // `arith_negate` promotes an i64::MIN numerator instead of overflowing.
        ValueView::Rat(n, _) | ValueView::FatRat(n, _) if n < 0 => {
            return Some(crate::builtins::arith_negate(target.clone()));
        }
        ValueView::Rat(..) | ValueView::FatRat(..) => target.clone(),
        ValueView::BigRat(n, d) if target.is_bigfatrat() => {
            Value::bigfatrat(n.magnitude().clone().into(), d.clone())
        }
        ValueView::BigRat(n, d) => Value::bigrat(n.magnitude().clone().into(), d.clone()),
        ValueView::Complex(r, i) => Value::num((r * r + i * i).sqrt()),
        _ => return None,
    }))
}

/// `Real.sign` on a numeric view, `None` for anything else. A `Complex`
/// answers only when its imaginary part is zero; otherwise it cannot be a
/// `Real` and throws `X::Numeric::Real`.
// Cost: O(1).
pub(crate) fn sign_of(target: &Value) -> Option<Result<Value, RuntimeError>> {
    let of = |s: i64| Some(Ok(Value::int(s)));
    match target.view() {
        ValueView::Int(i) => of(i.signum()),
        ValueView::Bool(b) => of(i64::from(b)),
        ValueView::BigInt(n) => of(bigint_sign(n.as_ref())),
        ValueView::Num(f) if f.is_nan() => Some(Ok(Value::num(f64::NAN))),
        ValueView::Num(f) => of(f64_sign(f)),
        ValueView::Rat(0, 0) | ValueView::FatRat(0, 0) => Some(Ok(Value::num(f64::NAN))),
        ValueView::Rat(n, d) | ValueView::FatRat(n, d) => of(if d == 0 {
            n.signum()
        } else {
            n.signum() * d.signum()
        }),
        ValueView::BigRat(n, d) if d.is_zero() && n.is_zero() => Some(Ok(Value::num(f64::NAN))),
        ValueView::BigRat(n, d) if d.is_zero() => of(bigint_sign(n)),
        ValueView::BigRat(n, d) => of(bigint_sign(n) * bigint_sign(d)),
        ValueView::Complex(re, 0.0) => of(f64_sign(re)),
        ValueView::Complex(re, im) => Some(Err(complex_not_real(target, re, im))),
        _ => None,
    }
}

fn bigint_sign(n: &BigInt) -> i64 {
    if n.is_positive() {
        1
    } else if n.is_negative() {
        -1
    } else {
        0
    }
}

fn f64_sign(f: f64) -> i64 {
    if f > 0.0 {
        1
    } else if f < 0.0 {
        -1
    } else {
        0
    }
}

/// `X::Numeric::Real` for a `Complex` with a non-zero imaginary part.
fn complex_not_real(target: &Value, re: f64, im: f64) -> RuntimeError {
    let rendered = if im >= 0.0 {
        format!("{re}+{im}i")
    } else {
        format!("{re}{im}i")
    };
    let mut attrs = std::collections::HashMap::new();
    attrs.insert(
        "message".to_string(),
        Value::str(format!(
            "Cannot convert {rendered} to Real: imaginary part not zero"
        )),
    );
    attrs.insert(
        "target".to_string(),
        Value::package(crate::symbol::Symbol::intern("Real")),
    );
    attrs.insert("source".to_string(), target.clone());
    let ex = Value::make_instance(crate::symbol::Symbol::intern("X::Numeric::Real"), attrs);
    let mut err = RuntimeError::new("Cannot convert Complex to Real: imaginary part not zero");
    err.exception = Some(Box::new(ex));
    err
}

/// How a rounding method maps a value to an integer.
#[derive(Clone, Copy)]
enum Rounding {
    Floor,
    Ceiling,
    /// Half up: `floor(x + 1/2)`, as Rakudo's `Real.round`.
    Round,
    Truncate,
}

impl Rounding {
    fn name(self) -> &'static str {
        match self {
            Rounding::Floor => "floor",
            Rounding::Ceiling => "ceiling",
            Rounding::Round => "round",
            Rounding::Truncate => "truncate",
        }
    }

    fn apply_f64(self, f: f64) -> f64 {
        match self {
            Rounding::Floor => f.floor(),
            Rounding::Ceiling => f.ceil(),
            Rounding::Round => (f + 0.5).floor(),
            Rounding::Truncate => f.trunc(),
        }
    }

    /// `n / d` rounded, exactly, through the one floored-division routine
    /// (`int_div`, ADR-0118): `ceiling` is `-((-n) div d)`, `round` is
    /// `(2n + d) div 2d`, and `truncate` is whichever of the two rounds
    /// towards zero.
    // Cost: O(1) for word-sized parts; O(b^2) for big ones, b = size in bits.
    fn apply_rational(self, n: Value, d: Value) -> Result<Value, RuntimeError> {
        use crate::builtins::{arith_add, arith_mul, arith_negate, int_div};
        let ceiling = |n: Value, d: &Value| -> Result<Value, RuntimeError> {
            arith_negate(int_div(&arith_negate(n)?, d))
        };
        match self {
            Rounding::Floor => Ok(int_div(&n, &d)),
            Rounding::Ceiling => ceiling(n, &d),
            Rounding::Round => {
                let twice_d = arith_mul(d.clone(), Value::int(2));
                Ok(int_div(
                    &arith_add(arith_mul(n, Value::int(2)), d)?,
                    &twice_d,
                ))
            }
            Rounding::Truncate if value_is_negative(&n) != value_is_negative(&d) => ceiling(n, &d),
            Rounding::Truncate => Ok(int_div(&n, &d)),
        }
    }

    /// The method on a numeric view, `None` for anything else.
    // Cost: O(1) for word-sized values; O(b) for big ones (O(b^2) for a big
    // rational's division), b = size in bits.
    fn of(self, target: &Value) -> Option<Result<Value, RuntimeError>> {
        Some(Ok(match target.view() {
            // An `Int` or a `Bool` (an `Int` enum, whose rounding is itself).
            ValueView::Int(_) | ValueView::BigInt(_) | ValueView::Bool(_) => target.clone(),
            ValueView::Num(f) if !f.is_finite() => Value::num(f),
            ValueView::Num(f) => integral_num_to_int(self.apply_f64(f)),
            ValueView::Rat(_, 0) | ValueView::FatRat(_, 0) => {
                return zero_denominator(self.name());
            }
            ValueView::Rat(n, d) | ValueView::FatRat(n, d) => {
                return Some(self.apply_rational(Value::int(n), Value::int(d)));
            }
            ValueView::BigRat(_, d) if d.is_zero() => return zero_denominator(self.name()),
            ValueView::BigRat(n, d) => {
                return Some(
                    self.apply_rational(
                        Value::from_bigint(n.clone()),
                        Value::from_bigint(d.clone()),
                    ),
                );
            }
            ValueView::Complex(re, im) => Value::complex(self.apply_f64(re), self.apply_f64(im)),
            _ => return None,
        }))
    }
}

/// `.floor` on a numeric view, `None` for anything else.
// Cost: see `Rounding::of`.
pub(crate) fn floor_of(target: &Value) -> Option<Result<Value, RuntimeError>> {
    Rounding::Floor.of(target)
}

/// `.ceiling` on a numeric view, `None` for anything else.
// Cost: see `Rounding::of`.
pub(crate) fn ceiling_of(target: &Value) -> Option<Result<Value, RuntimeError>> {
    Rounding::Ceiling.of(target)
}

/// `.round` on a numeric view, `None` for anything else.
// Cost: see `Rounding::of`.
pub(crate) fn round_of(target: &Value) -> Option<Result<Value, RuntimeError>> {
    Rounding::Round.of(target)
}

/// `.truncate` on a numeric view, `None` for anything else.
// Cost: see `Rounding::of`.
pub(crate) fn truncate_of(target: &Value) -> Option<Result<Value, RuntimeError>> {
    Rounding::Truncate.of(target)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rounding_a_word_sized_rational_is_exact() {
        // 2**53 + 1 halves to x.5, which an f64 cannot hold.
        let r = crate::value::make_rat(9_007_199_254_740_993, 2);
        assert_eq!(
            round_of(&r).unwrap().unwrap().as_int(),
            Some(4_503_599_627_370_497)
        );
        let neg = crate::value::make_rat(-5, 2);
        assert_eq!(round_of(&neg).unwrap().unwrap().as_int(), Some(-2));
        assert_eq!(
            floor_of(&crate::value::make_rat(-7, 2))
                .unwrap()
                .unwrap()
                .as_int(),
            Some(-4)
        );
        assert_eq!(
            ceiling_of(&crate::value::make_rat(-7, 2))
                .unwrap()
                .unwrap()
                .as_int(),
            Some(-3)
        );
        assert_eq!(
            truncate_of(&crate::value::make_rat(-7, 2))
                .unwrap()
                .unwrap()
                .as_int(),
            Some(-3)
        );
    }

    #[test]
    fn a_num_past_a_word_rounds_to_a_big_int() {
        let v = floor_of(&Value::num(1e30)).unwrap().unwrap();
        assert_eq!(
            crate::runtime::gist_value(&v),
            "1000000000000000019884624838656"
        );
    }
}
