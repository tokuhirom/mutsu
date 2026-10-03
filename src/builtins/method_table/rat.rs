//! `Rat`'s rows.

use super::{Handler, MethodRow};
use crate::value::{RuntimeError, Value, ValueView};

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "Rat",
        name: "numerator",
        arity: 0,
        handler: Handler::Pure(numerator),
    },
    MethodRow {
        owner: "Rat",
        name: "denominator",
        arity: 0,
        handler: Handler::Pure(denominator),
    },
];

/// `Rat.numerator`. Also called by the `numerator` cascade arm for a `Rat`,
/// so the method has one implementation.
// Cost: O(1).
pub(crate) fn numerator(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    match target.view() {
        ValueView::Rat(n, _) => Ok(Value::int(n)),
        _ => Err(RuntimeError::new("Rat.numerator: receiver is not a Rat")),
    }
}

/// `Rat.denominator`. Also called by the `denominator` cascade arm for a
/// `Rat`, so the method has one implementation.
// Cost: O(1).
pub(crate) fn denominator(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    match target.view() {
        ValueView::Rat(_, d) => Ok(Value::int(d)),
        _ => Err(RuntimeError::new("Rat.denominator: receiver is not a Rat")),
    }
}
