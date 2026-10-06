//! `Str` with no arguments on `Str` and the numeric types.
//!
//! Rakudo has a `Str` method in the method table of `Str`, `Int`, `Num`,
//! `Rat`, `FatRat` and `Complex`. A `Str` is itself. A number renders the
//! way the rest of the runtime renders it (`Value::to_string_value`, also
//! behind `~$n` and interpolation). A rational with a zero denominator
//! cannot be rendered: its `.Str` throws `X::Numeric::DivideByZero` with the
//! interpreter's context, so that row declines it.

use super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueView};

/// One `Str` row per owner.
macro_rules! str_rows {
    ($($owner:literal),*) => {
        &[$(MethodRow {
            owner: $owner,
            name: "Str",
            arity: 0,
            handler: Handler::Narrow(str),
            flags: RowFlags::NONE,
            named: &[],
        }),*]
    };
}

pub(super) static ROWS: &[MethodRow] = str_rows!("Str", "Int", "Num", "Rat", "FatRat", "Complex");

/// `.Str`: the receiver itself for a `Str`, its rendering for a number, or
/// `None` for a zero-denominator rational.
// Cost: O(1) for a `Str` (the value is shared, not copied); O(n) for a
// number, n = chars of the rendering.
fn str(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Str(_) => Some(Ok(target.clone())),
        ValueView::Rat(_, 0) | ValueView::FatRat(_, 0) => None,
        ValueView::BigRat(_, d) if num_traits::Zero::is_zero(d) => None,
        _ => Some(Ok(Value::str(target.to_string_value()))),
    }
}
