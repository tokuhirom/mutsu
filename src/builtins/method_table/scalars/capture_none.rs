//! `Capture` of the scalars that have none: `Int`, `Num`, `Str` and `Version`
//! (ADR-11276 §9.71).
//!
//! `Mu.Capture` turns an object's public attributes into named arguments; these
//! four types have none, so Rakudo throws `X::Cannot::Capture` naming the value.
//! The error is [`cannot_capture_value`], which the cascade's `Capture` arm
//! raises for the same receivers (a `Bool` reaches `Int`'s row through its MRO).
//! `Rat`, `Complex` and the aggregates have attributes, and stay in the cascade's
//! `value_to_capture`.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::methods_0arg::coercion::cannot_capture_value;
use crate::value::{RuntimeError, Value};

macro_rules! row {
    ($owner:literal) => {
        MethodRow {
            owner: $owner,
            name: "Capture",
            arity: 0,
            handler: Handler::Pure(capture),
            // `Int.Capture` cannot capture the type object either.
            flags: RowFlags::TYPE_OBJECT_OK,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[row!("Int"), row!("Num"), row!("Str"), row!("Version")];

/// `.Capture`: always the `X::Cannot::Capture` error.
// Cost: O(n), n = chars of the value's gist.
fn capture(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Err(cannot_capture_value(target))
}
