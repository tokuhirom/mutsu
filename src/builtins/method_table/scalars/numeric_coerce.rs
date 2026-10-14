//! The numeric types' conversions to another numeric type: `Numeric`, `Real`,
//! `Rat`, `FatRat` and `Complex` (ADR-11276 §9.70).
//!
//! Rakudo declares each of these on `Int`, `Num` and `Rat` (and `Numeric` and
//! `Complex` on `Complex` too). `Complex` has no row for `Real`, `Rat` or
//! `FatRat`: a `Complex` is a `Real` only when its imaginary part is below
//! `$*TOLERANCE`, which needs the interpreter (`Interpreter::dispatch_complex_to_real`).
//!
//! Each conversion has one implementation, the `*_of` function: a row calls it
//! for a number, and the cascade arms (which still answer `Str`, the
//! aggregates, `Bool`-like enums and the type objects) call it for the numeric
//! views before they look at anything else. `Rat` and `FatRat` also take an
//! `epsilon` (`Real $epsilon = 1e-6`) that matters only to a `Num`.

use super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueView, make_rat};
use num_bigint::BigInt;

/// The default `epsilon` of `.Rat` and `.FatRat`.
const DEFAULT_EPSILON: f64 = 1e-6;

/// Zero-argument rows with a handler that answers every shape of its owner.
macro_rules! pure {
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

/// A one-argument (`epsilon`) row; the handler declines an argument that is
/// not a `Real` by raising the bind failure itself.
macro_rules! with_epsilon {
    ($owner:literal, $name:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 1,
            handler: Handler::Pure($handler),
            flags: RowFlags::ANY_ARGS,
            named: &[],
        }
    };
}

pub(super) static INT_ROWS: &[MethodRow] = &[
    pure!("Int", "Numeric", numeric),
    pure!("Int", "Real", real),
    pure!("Int", "Rat", rat),
    pure!("Int", "FatRat", fat_rat),
    pure!("Int", "Complex", complex),
    with_epsilon!("Int", "Rat", rat_with),
    with_epsilon!("Int", "FatRat", fat_rat_with),
];
pub(super) static NUM_ROWS: &[MethodRow] = &[
    pure!("Num", "Numeric", numeric),
    pure!("Num", "Real", real),
    pure!("Num", "Rat", rat),
    pure!("Num", "FatRat", fat_rat),
    pure!("Num", "Complex", complex),
    with_epsilon!("Num", "Rat", rat_with),
    with_epsilon!("Num", "FatRat", fat_rat_with),
];
pub(super) static RAT_ROWS: &[MethodRow] = &[
    pure!("Rat", "Numeric", numeric),
    pure!("Rat", "Real", real),
    pure!("Rat", "Rat", rat),
    pure!("Rat", "FatRat", fat_rat),
    pure!("Rat", "Complex", complex),
    with_epsilon!("Rat", "Rat", rat_with),
    with_epsilon!("Rat", "FatRat", fat_rat_with),
];
pub(super) static COMPLEX_ROWS: &[MethodRow] = &[
    pure!("Complex", "Numeric", numeric),
    // `method Complex() { self }`, so the type object answers itself.
    MethodRow {
        owner: "Complex",
        name: "Complex",
        arity: 0,
        handler: Handler::Pure(complex_self),
        flags: RowFlags::TYPE_OBJECT_OK,
        named: &[],
    },
];

fn not_numeric(method: &str) -> RuntimeError {
    RuntimeError::new(format!("{method}: receiver is not a number"))
}

fn numeric(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    numeric_of(target).ok_or_else(|| not_numeric("Numeric"))
}
fn real(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    real_of(target).ok_or_else(|| not_numeric("Real"))
}
fn rat(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    rat_of(target).ok_or_else(|| not_numeric("Rat"))
}
fn fat_rat(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    fat_rat_of(target).ok_or_else(|| not_numeric("FatRat"))
}
/// `Complex.Complex`: the receiver, a type object included.
// Cost: O(1).
fn complex_self(target: &Value, args: &[Value]) -> Result<Value, RuntimeError> {
    if let ValueView::Package(_) = target.view() {
        return Ok(target.clone());
    }
    complex(target, args)
}

fn complex(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    complex_of(target).ok_or_else(|| not_numeric("Complex"))
}
fn rat_with(target: &Value, args: &[Value]) -> Result<Value, RuntimeError> {
    rat_with_epsilon(target, &args[0]).ok_or_else(|| not_numeric("Rat"))?
}
fn fat_rat_with(target: &Value, args: &[Value]) -> Result<Value, RuntimeError> {
    fat_rat_with_epsilon(target, &args[0]).ok_or_else(|| not_numeric("FatRat"))?
}

/// `.Numeric` of a number: itself (a `Bool` is its `Int`). `None` for anything
/// else. Rationals stay exact: going through `f64` would lose a large
/// denominator.
// Cost: O(1) (a clone; O(b) for a big value, b = size in bits).
pub(crate) fn numeric_of(target: &Value) -> Option<Value> {
    Some(match target.view() {
        ValueView::Int(_)
        | ValueView::BigInt(_)
        | ValueView::Num(_)
        | ValueView::Rat(..)
        | ValueView::FatRat(..)
        | ValueView::BigRat(..)
        | ValueView::Complex(..) => target.clone(),
        ValueView::Bool(b) => Value::int(i64::from(b)),
        _ => return None,
    })
}

/// `.Real` of a real number: itself (a `Bool` is its `Int`). `None` for a
/// `Complex`, which is a `Real` only within `$*TOLERANCE`, and for anything
/// else.
// Cost: O(1) (O(b) for a big value, b = size in bits).
pub(crate) fn real_of(target: &Value) -> Option<Value> {
    Some(match target.view() {
        ValueView::Int(_)
        | ValueView::BigInt(_)
        | ValueView::Num(_)
        | ValueView::Rat(..)
        | ValueView::FatRat(..) => target.clone(),
        ValueView::BigRat(n, d) => Value::bigrat(n.clone(), d.clone()),
        ValueView::Bool(b) => Value::int(i64::from(b)),
        _ => return None,
    })
}

/// `.Rat` of a real number. A `Num` is the simplest rational within `1e-6`
/// (`NaN` is `0/0`, the infinities `±1/0`); a `FatRat` drops its precision
/// flag. `None` for a `Complex` (the interpreter judges its imaginary part) and
/// for anything that is not a number.
// Cost: O(log(1/epsilon)) for a Num (the continued fraction); O(1) otherwise
// (O(b) for a big value, b = size in bits).
pub(crate) fn rat_of(target: &Value) -> Option<Value> {
    Some(match target.view() {
        ValueView::Rat(..) => target.clone(),
        ValueView::BigRat(n, d) if target.is_bigfatrat() => Value::bigrat(n.clone(), d.clone()),
        ValueView::BigRat(..) => target.clone(),
        ValueView::Int(i) => make_rat(i, 1),
        ValueView::BigInt(n) => crate::value::make_big_rat((**n).clone(), BigInt::from(1)),
        ValueView::Bool(b) => make_rat(i64::from(b), 1),
        ValueView::Num(f) => num_rat_with_epsilon(f, DEFAULT_EPSILON),
        ValueView::FatRat(n, d) => make_rat(n, d),
        _ => return None,
    })
}

/// `.FatRat` of a real number: as [`rat_of`], but a `Rat` gains the precision
/// flag and a big `Rat` never degrades to a `Num`.
// Cost: as `rat_of`.
pub(crate) fn fat_rat_of(target: &Value) -> Option<Value> {
    Some(match target.view() {
        ValueView::FatRat(..) => target.clone(),
        ValueView::Rat(n, d) => Value::fat_rat_raw(n, d),
        ValueView::BigRat(_, _) if target.is_bigfatrat() => target.clone(),
        ValueView::BigRat(n, d) => Value::bigfatrat(n.clone(), d.clone()),
        ValueView::Int(i) => Value::fat_rat_raw(i, 1),
        ValueView::BigInt(n) => crate::value::make_big_fat_rat((**n).clone(), BigInt::from(1)),
        ValueView::Bool(b) => Value::fat_rat_raw(i64::from(b), 1),
        ValueView::Num(f) => fat_rat_from_rat(num_rat_with_epsilon(f, DEFAULT_EPSILON)),
        _ => return None,
    })
}

/// `.Complex` of a real number or a `Complex`: the number as the real part
/// (`.Num` of it, so a zero-denominator rational is `NaN` or an infinity).
/// `None` for anything else.
// Cost: O(1) (O(b) for a big value, b = size in bits).
pub(crate) fn complex_of(target: &Value) -> Option<Value> {
    if let ValueView::Complex(..) = target.view() {
        return Some(target.clone());
    }
    let real = super::coerce::num_of(target)?;
    Some(Value::complex(real.to_f64(), 0.0))
}

/// `.Rat($epsilon)`: only a `Num` uses the epsilon, but a `Rat` (which
/// binds it anonymously) and a `Num` check that it is a `Real`; an `Int` does
/// not, so `7.Rat('0.01')` is `7.0`. `None` for a receiver that is not a real
/// number.
// Cost: as `rat_of`.
pub(crate) fn rat_with_epsilon(
    target: &Value,
    epsilon: &Value,
) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(match target.view() {
        ValueView::Rat(..) | ValueView::BigRat(..) => {
            if let Err(e) = epsilon_arg(epsilon, "<anon>") {
                return Some(Err(e));
            }
            rat_of(target)?
        }
        ValueView::Num(f) => match epsilon_arg(epsilon, "epsilon") {
            Ok(epsilon) => num_rat_with_epsilon(f, epsilon),
            Err(e) => return Some(Err(e)),
        },
        _ => rat_of(target)?,
    }))
}

/// `.FatRat($epsilon)`: as [`rat_with_epsilon`], with the `FatRat`
/// candidates' parameter names (`$epsilon` for a `Num`).
// Cost: as `rat_of`.
pub(crate) fn fat_rat_with_epsilon(
    target: &Value,
    epsilon: &Value,
) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(match target.view() {
        ValueView::Rat(..) | ValueView::BigRat(..) => {
            if let Err(e) = epsilon_arg(epsilon, "<anon>") {
                return Some(Err(e));
            }
            fat_rat_of(target)?
        }
        ValueView::Num(f) => match epsilon_arg(epsilon, "$epsilon") {
            Ok(epsilon) => fat_rat_from_rat(num_rat_with_epsilon(f, epsilon)),
            Err(e) => return Some(Err(e)),
        },
        _ => fat_rat_of(target)?,
    }))
}

/// A `Rat` as the `FatRat` of the same value (`0/1` when it is not a `Rat`).
fn fat_rat_from_rat(rat: Value) -> Value {
    match rat.view() {
        ValueView::Rat(n, d) => Value::fat_rat_raw(n, d),
        _ => Value::fat_rat_raw(0, 1),
    }
}

/// `f.Rat(epsilon)`: the simplest rational within `epsilon` of `f` (a
/// continued fraction), with `NaN` as `0/0` and the infinities as `±1/0`.
// Cost: O(log(1/epsilon)) continued-fraction steps.
pub(crate) fn num_rat_with_epsilon(f: f64, epsilon: f64) -> Value {
    if f.is_nan() {
        Value::rat_raw(0, 0)
    } else if f.is_infinite() {
        Value::rat_raw(if f.is_sign_positive() { 1 } else { -1 }, 0)
    } else {
        crate::builtins::num_to_rat_with_epsilon(f, epsilon)
    }
}

/// Whether `arg` is a `Real`, the type of the `Real $epsilon` parameter of
/// `.Rat(eps)` / `.FatRat(eps)`: a number, a `Bool`, an enum value, an allomorph
/// (`<0.01>`), or an object that carries a numeric payload (`Duration`,
/// `Instant`). A `Str`, `Nil`, a type object, a `Complex` and every container
/// are not.
// Cost: O(1).
fn is_real_epsilon(arg: &Value) -> bool {
    match arg.view() {
        ValueView::Num(_)
        | ValueView::Int(_)
        | ValueView::BigInt(_)
        | ValueView::Rat(..)
        | ValueView::BigRat(..)
        | ValueView::FatRat(..)
        | ValueView::Bool(_)
        | ValueView::Enum { .. } => true,
        ValueView::Mixin(inner, _) => is_real_epsilon(inner),
        ValueView::Scalar(inner) => is_real_epsilon(inner),
        // `Duration` and `Instant` keep their number in `value`; an instance of
        // a user subclass of `Int` / `Num` / `Rat` keeps it in a payload.
        ValueView::Instance { class_name, .. } => {
            class_name == "Duration"
                || class_name == "Instant"
                || crate::value::numeric_payload::numeric_subclass_payload(arg).is_some()
        }
        _ => false,
    }
}

/// The epsilon `.Rat(eps)` / `.FatRat(eps)` was bound to. Rakudo's parameter is
/// `Real $epsilon`, so anything else (a `Str`, `Nil`, a type object, a
/// `Complex`) fails the bind with `X::TypeCheck::Binding::Parameter`, as
/// `Num.Rat('0.01')` does there (#12043). `param` is the parameter's name as
/// Rakudo reports it: the `Num` candidates name theirs `epsilon` (`Rat`) and
/// `$epsilon` (`FatRat`), the `Rat` candidates leave it anonymous (`<anon>`).
/// A `Real` numifies; a non-finite one has no usable value and keeps the
/// default `1e-6`.
// Cost: O(1).
pub(crate) fn epsilon_arg(arg: &Value, param: &str) -> Result<f64, RuntimeError> {
    if !is_real_epsilon(arg) {
        return Err(
            crate::runtime::utils::typecheck_binding_parameter_with_hint(
                param,
                "Real",
                arg,
                &crate::runtime::utils::value_short_repr(arg),
                None,
            ),
        );
    }
    let value = match arg.view() {
        // `Value::to_f64` reads the `Duration` / `Instant` / payload instances.
        ValueView::Instance { .. } => Some(arg.to_f64()),
        _ => crate::runtime::to_float_value(arg),
    };
    Ok(value.filter(|f| f.is_finite()).unwrap_or(DEFAULT_EPSILON))
}
