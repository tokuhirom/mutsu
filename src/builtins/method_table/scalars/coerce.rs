//! The numeric types' coercion rows: `Int`, `Num` and `Bool` with no
//! arguments.
//!
//! Rakudo has a copy of each in the method table of `Int`, `Num`, `Rat`,
//! `FatRat` and `Complex`. `Complex.Int` and `Complex.Num` read `$*TOLERANCE`
//! (an imaginary part below it counts as zero), which needs the interpreter,
//! so `Complex` has only its `Bool` row here. Each method has one
//! implementation; the cascade's `.Int`/`.Num` arms and `Str.Int`'s
//! numify-then-truncate path call the same functions.

use super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueView};
use num_traits::Zero;

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
            "Int" => int,
            "Num" => num,
            "Bool" => bool,
        ]
    };
}

pub(super) static INT_ROWS: &[MethodRow] = real_rows!("Int");
pub(super) static NUM_ROWS: &[MethodRow] = real_rows!("Num");
pub(super) static RAT_ROWS: &[MethodRow] = real_rows!("Rat");
pub(super) static FAT_RAT_ROWS: &[MethodRow] = real_rows!("FatRat");
pub(super) static COMPLEX_ROWS: &[MethodRow] = rows!["Complex": "Bool" => bool];

fn int(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    int_of(target).ok_or_else(|| RuntimeError::new("Int: receiver is not a number"))
}

fn num(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    num_of(target).ok_or_else(|| RuntimeError::new("Num: receiver is not a number"))
}

/// `.Bool` on a number: whether it is non-zero (`NaN` is true).
// Cost: O(1).
fn bool(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::truth(target.truthy()))
}

/// `.Int` on a numeric view: truncation towards zero, `None` for anything
/// else. A non-finite `Num` or a zero-denominator rational answers the lazy
/// `Failure` Rakudo does. A `Complex` answers its real part's truncation; the
/// caller decides first whether its imaginary part lets it be a `Real`
/// (`Complex.Int` itself does, against `$*TOLERANCE`, in
/// `Interpreter::dispatch_complex_to_real`).
// Cost: O(1) for word-sized values; O(b) for big ones (O(b^2) for a big
// rational's division), b = size in bits.
pub(crate) fn int_of(target: &Value) -> Option<Value> {
    Some(match target.view() {
        ValueView::Int(_) | ValueView::BigInt(_) => target.clone(),
        ValueView::Num(f) if !f.is_finite() => cannot_convert_to_int_failure(target, f),
        ValueView::Num(f) => super::real::integral_num_to_int(f.trunc()),
        ValueView::Rat(_, 0) | ValueView::FatRat(_, 0) => {
            RuntimeError::divide_by_zero_failure_for_method("Int", "Rational")
        }
        ValueView::BigRat(_, d) if d.is_zero() => {
            RuntimeError::divide_by_zero_failure_for_method("Int", "Rational")
        }
        ValueView::Rat(..) | ValueView::FatRat(..) | ValueView::BigRat(..) => {
            super::real::truncate_of(target)?.ok()?
        }
        ValueView::Complex(re, _) => super::real::integral_num_to_int(re.trunc()),
        _ => return None,
    })
}

/// `.Num` on a real numeric view, `None` for anything else. A
/// zero-denominator rational is `NaN` or a signed infinity.
// Cost: O(1) for word-sized values; O(b) for big ones, b = size in bits.
pub(crate) fn num_of(target: &Value) -> Option<Value> {
    Some(Value::num(match target.view() {
        ValueView::Int(i) => i as f64,
        ValueView::BigInt(n) => {
            num_traits::ToPrimitive::to_f64(n.as_ref()).unwrap_or(f64::INFINITY)
        }
        ValueView::Num(f) => f,
        ValueView::Rat(n, 0) | ValueView::FatRat(n, 0) => {
            if n == 0 {
                f64::NAN
            } else if n > 0 {
                f64::INFINITY
            } else {
                f64::NEG_INFINITY
            }
        }
        ValueView::Rat(n, d) | ValueView::FatRat(n, d) => crate::value::rat_to_f64(n, d),
        // Correctly rounded: converting numerator and denominator to f64
        // separately loses the last bit.
        ValueView::BigRat(n, d) if !d.is_zero() => crate::value::bigrat_to_f64(n, d),
        _ => return None,
    }))
}

/// The `X::Numeric::CannotConvert` Failure a non-finite `Num` (`NaN`, `Inf`,
/// `-Inf`) answers `.Int` with.
pub(crate) fn cannot_convert_to_int_failure(source: &Value, f: f64) -> Value {
    let label = if f.is_nan() {
        "NaN"
    } else if f.is_sign_positive() {
        "Inf"
    } else {
        "-Inf"
    };
    let mut attrs = std::collections::HashMap::new();
    attrs.insert(
        "message".to_string(),
        Value::str(format!("Cannot convert {label} to Int")),
    );
    attrs.insert("source".to_string(), source.clone());
    attrs.insert("target".to_string(), Value::str_from("Int"));
    let ex = Value::make_instance(
        crate::symbol::Symbol::intern("X::Numeric::CannotConvert"),
        attrs,
    );
    let mut failure_attrs = std::collections::HashMap::new();
    failure_attrs.insert("exception".to_string(), ex);
    Value::make_instance(crate::symbol::Symbol::intern("Failure"), failure_attrs)
}
