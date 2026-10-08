//! `pick`, `roll` and `pickpairs` (ADR-11276 remainder: the sampling methods).
//!
//! Rakudo declares `pick` and `roll` on `List`, `Range`, `Map` and `Any`, and on
//! the six quant hashes, which also declare `pickpairs`. Every row calls the one
//! implementation in [`crate::builtins::sampling`], which the cascades' arms for
//! a `Seq`, a lazy list, a shaped array and an itemized hash (receivers with no
//! dispatch shape) call too. The answer is random, so every row is flagged
//! `RANDOM`: the debug cross-check does not re-run it. A `Callable` count is the
//! interpreter's to invoke, so the handler declines it.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::sampling;

macro_rules! rows {
    ($name:literal => $handler:path: $($owner:literal),*) => {
        &[$(
            MethodRow {
                owner: $owner,
                name: $name,
                arity: 0,
                handler: Handler::Narrow($handler),
                flags: RowFlags::RANDOM,
                named: &[],
            },
            MethodRow {
                owner: $owner,
                name: $name,
                arity: 1,
                handler: Handler::Narrow($handler),
                flags: RowFlags::RANDOM.or(RowFlags::ANY_ARGS),
                named: &[],
            },
        )*]
    };
}

pub(super) static PICK_ROWS: &[MethodRow] = rows!("pick" => sampling::pick:
    "Any", "List", "Range", "Map", "Set", "SetHash", "Bag", "BagHash", "Mix", "MixHash");
pub(super) static ROLL_ROWS: &[MethodRow] = rows!("roll" => sampling::roll:
    "Any", "List", "Range", "Map", "Set", "SetHash", "Bag", "BagHash", "Mix", "MixHash");
pub(super) static PICKPAIRS_ROWS: &[MethodRow] = rows!("pickpairs" => sampling::pickpairs:
    "Set", "SetHash", "Bag", "BagHash", "Mix", "MixHash");
