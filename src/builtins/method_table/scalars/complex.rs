//! `Complex`'s rows.

use super::{Handler, MethodRow};
use crate::value::{RuntimeError, Value, ValueView};

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "Complex",
        name: "isNaN",
        arity: 0,
        handler: Handler::Pure(is_nan),
    },
    MethodRow {
        owner: "Complex",
        name: "re",
        arity: 0,
        handler: Handler::Pure(re),
    },
    MethodRow {
        owner: "Complex",
        name: "im",
        arity: 0,
        handler: Handler::Pure(im),
    },
    MethodRow {
        owner: "Complex",
        name: "reals",
        arity: 0,
        handler: Handler::Pure(reals),
    },
    MethodRow {
        owner: "Complex",
        name: "conj",
        arity: 0,
        handler: Handler::Pure(conj),
    },
];

/// `Complex.isNaN`: either part is NaN (`(NaN+5i).isNaN` is `True`).
// Cost: O(1).
fn is_nan(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    match target.view() {
        ValueView::Complex(re, im) => Ok(Value::truth(re.is_nan() || im.is_nan())),
        _ => Err(RuntimeError::new(
            "Complex.isNaN: receiver is not a Complex",
        )),
    }
}

/// The real component of a Complex.
// Cost: O(1).
pub(crate) fn re(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    match target.view() {
        ValueView::Complex(real, _) => Ok(Value::num(real)),
        _ => Err(RuntimeError::new("Complex.re: receiver is not a Complex")),
    }
}

/// The imaginary component of a Complex.
// Cost: O(1).
pub(crate) fn im(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    match target.view() {
        ValueView::Complex(_, imag) => Ok(Value::num(imag)),
        _ => Err(RuntimeError::new("Complex.im: receiver is not a Complex")),
    }
}

/// Both components of a Complex in real-then-imaginary order.
// Cost: O(1), two values and one array allocation.
pub(crate) fn reals(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    match target.view() {
        ValueView::Complex(real, imag) => {
            Ok(Value::array(vec![Value::num(real), Value::num(imag)]))
        }
        _ => Err(RuntimeError::new(
            "Complex.reals: receiver is not a Complex",
        )),
    }
}

/// The conjugate of a Complex. Real numeric values are their own conjugates.
// Cost: O(1).
pub(crate) fn conj(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    match target.view() {
        ValueView::Complex(real, imag) => Ok(Value::complex(real, -imag)),
        ValueView::Int(_)
        | ValueView::BigInt(_)
        | ValueView::Num(_)
        | ValueView::Rat(_, _)
        | ValueView::FatRat(_, _)
        | ValueView::Bool(_) => Ok(target.clone()),
        _ => Err(RuntimeError::new("conj: receiver is not numeric")),
    }
}
