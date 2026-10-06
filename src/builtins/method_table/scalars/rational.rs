//! The `Rational` role's rows, composed into `Rat` and `FatRat`.
//!
//! Rakudo declares `numerator`, `denominator`, `nude`, `norm` and `isNaN` in
//! the `Rational` role, so each of `Rat` and `FatRat` has its own copy in its
//! `^method_table`. Here both owners' rows point at the same handlers: one
//! implementation per method, whichever rational type the receiver is and
//! whether its components fit a machine word (`Rat`/`FatRat`) or not
//! (`BigRat`).
//!
//! `Int` has none of these but `isNaN` (`5.numerator` is "No such method" in
//! Rakudo), so `Int` receivers get no row here.

use super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueView, make_big_fat_rat, make_big_rat, make_rat};

/// The five `Rational` rows for one owner.
macro_rules! rational_rows {
    ($owner:literal) => {
        [
            MethodRow {
                owner: $owner,
                name: "numerator",
                arity: 0,
                handler: Handler::Pure(numerator),
                flags: RowFlags::NONE,
                named: &[],
            },
            MethodRow {
                owner: $owner,
                name: "denominator",
                arity: 0,
                handler: Handler::Pure(denominator),
                flags: RowFlags::NONE,
                named: &[],
            },
            MethodRow {
                owner: $owner,
                name: "nude",
                arity: 0,
                handler: Handler::Pure(nude),
                flags: RowFlags::NONE,
                named: &[],
            },
            MethodRow {
                owner: $owner,
                name: "norm",
                arity: 0,
                handler: Handler::Pure(norm),
                flags: RowFlags::NONE,
                named: &[],
            },
            MethodRow {
                owner: $owner,
                name: "isNaN",
                arity: 0,
                handler: Handler::Pure(is_nan),
                flags: RowFlags::NONE,
                named: &[],
            },
        ]
    };
}

pub(super) static RAT_ROWS: &[MethodRow] = &rational_rows!("Rat");
pub(super) static FAT_RAT_ROWS: &[MethodRow] = &rational_rows!("FatRat");

fn not_rational(method: &str) -> RuntimeError {
    RuntimeError::new(format!(
        "Rational.{method}: receiver is not a Rat or FatRat"
    ))
}

/// `Rational.numerator`.
// Cost: O(1) for machine-word components; O(b) for big ones, b = the
// numerator's size in bits (it is copied out).
fn numerator(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    match target.view() {
        ValueView::Rat(n, _) | ValueView::FatRat(n, _) => Ok(Value::int(n)),
        ValueView::BigRat(n, _) => Ok(Value::bigint(n.clone())),
        _ => Err(not_rational("numerator")),
    }
}

/// `Rational.denominator`.
// Cost: O(1) for machine-word components; O(b) for big ones, b = the
// denominator's size in bits (it is copied out).
fn denominator(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    match target.view() {
        ValueView::Rat(_, d) | ValueView::FatRat(_, d) => Ok(Value::int(d)),
        ValueView::BigRat(_, d) => Ok(Value::bigint(d.clone())),
        _ => Err(not_rational("denominator")),
    }
}

/// `Rational.nude`: the numerator and the denominator.
// Cost: O(1) for machine-word components; O(b) for big ones, b = the
// components' size in bits.
fn nude(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let pair = match target.view() {
        ValueView::Rat(n, d) | ValueView::FatRat(n, d) => vec![Value::int(n), Value::int(d)],
        ValueView::BigRat(n, d) => vec![Value::bigint(n.clone()), Value::bigint(d.clone())],
        _ => return Err(not_rational("nude")),
    };
    Ok(Value::array(pair))
}

/// `Rational.norm`: the same value with its components reduced, of the same
/// type as the receiver.
// Cost: O(log(min(n, d))) for machine-word components (a gcd); O(b^2) for
// big ones, b = the components' size in bits.
fn norm(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(match target.view() {
        ValueView::Rat(n, d) => make_rat(n, d),
        ValueView::FatRat(n, d) => fat(make_rat(n, d)),
        ValueView::BigRat(n, d) if target.is_bigfatrat() => {
            fat(make_big_fat_rat(n.clone(), d.clone()))
        }
        ValueView::BigRat(n, d) => make_big_rat(n.clone(), d.clone()),
        _ => return Err(not_rational("norm")),
    })
}

/// A reduced rational as a `FatRat`: the reducers answer a `Rat` for the
/// zero-denominator values.
fn fat(reduced: Value) -> Value {
    match reduced.view() {
        ValueView::Rat(n, d) => Value::fat_rat_raw(n, d),
        _ => reduced,
    }
}

/// `Rational.isNaN`: `0/0`.
// Cost: O(1).
fn is_nan(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    match target.view() {
        ValueView::Rat(n, d) | ValueView::FatRat(n, d) => Ok(Value::truth(n == 0 && d == 0)),
        // The reducers never build a big rational with a zero denominator.
        ValueView::BigRat(..) => Ok(Value::FALSE),
        _ => Err(not_rational("isNaN")),
    }
}
