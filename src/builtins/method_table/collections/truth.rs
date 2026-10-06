//! `Bool` on the collection owners that declare it (ADR-11276 §10, slice 3A
//! proof rows for the `Range`, `Capture` and quant-hash shapes).
//!
//! Each is the one truthiness rule (`Value::truthy`): a set, bag or mix is
//! true when it has an element, a `Capture` when it has an argument, and a
//! `Range` always.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::truth::truthiness;

macro_rules! bool_rows {
    ($($owner:literal),* $(,)?) => {
        &[$(MethodRow {
            owner: $owner,
            name: "Bool",
            arity: 0,
            handler: Handler::Pure(truthiness),
            flags: RowFlags::NONE,
            named: &[],
        }),*]
    };
}

pub(super) static ROWS: &[MethodRow] = bool_rows![
    "Range", "Capture", "Set", "SetHash", "Bag", "BagHash", "Mix", "MixHash"
];
