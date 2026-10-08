//! `fmt` on the collections (ADR-11276 remainder: the rendering names).
//!
//! Rakudo declares `fmt` on `Array`, `List`, `Map`, `Pair`, `Range` and `Seq`
//! (and on each quant hash, whose `fmt` pairs an element with its weight). The
//! zero-, one- and two-argument forms are one implementation,
//! [`fmt_native`](crate::builtins::fmt_native), which the three arity cascades
//! call too. It declines what needs the interpreter: a `Format` object as the
//! format, and a format with a directive over an item that may carry a user
//! `.Str`/`.Int`/`.Numeric`.

use super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value};

macro_rules! rows {
    ($($owner:literal),*) => {
        &[$(
            MethodRow {
                owner: $owner,
                name: "fmt",
                arity: 0,
                handler: Handler::Narrow(fmt),
                flags: RowFlags::NONE,
                named: &[],
            },
            MethodRow {
                owner: $owner,
                name: "fmt",
                arity: 1,
                handler: Handler::Narrow(fmt),
                flags: RowFlags::NONE,
                named: &[],
            },
            MethodRow {
                owner: $owner,
                name: "fmt",
                arity: 2,
                handler: Handler::Narrow(fmt),
                flags: RowFlags::NONE,
                named: &[],
            },
        )*]
    };
}

pub(super) static ROWS: &[MethodRow] = rows!(
    "Array", "List", "Map", "Pair", "Range", "Seq", "Set", "SetHash", "Bag", "BagHash", "Mix",
    "MixHash"
);

/// `.fmt`, `.fmt($format)` and `.fmt($format, $separator)`.
// Cost: O(f + n), f = chars of the format, n = chars of the rendering.
fn fmt(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    crate::builtins::fmt_native(target, args)
}
