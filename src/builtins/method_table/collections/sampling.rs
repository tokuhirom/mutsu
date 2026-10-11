//! `pick`, `roll` and `pickpairs` (ADR-11276 remainder: the sampling methods).
//!
//! Rakudo declares `pick` and `roll` on `List`, `Range`, `Map`, `Bool` and `Any`, and on
//! the six quant hashes, which also declare `pickpairs`. Every row calls the one
//! implementation in [`crate::builtins::sampling`], which the cascades' arms for
//! a `Seq`, a lazy list, a shaped array and an itemized hash (receivers with no
//! dispatch shape) call too. The answer is random, so every row is flagged
//! `RANDOM`: the debug cross-check does not re-run it. A `Callable` count is the
//! interpreter's to invoke, so the handler declines it.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::NarrowFn;
use crate::builtins::sampling;
use crate::value::{RuntimeError, Value, ValueView};

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

/// `Bool.pick` and `Bool.roll` are declared for the type object only (`Bool:U`):
/// `Bool.pick` is `True` or `False`, and a defined `True.pick` has no candidate.
pub(super) static BOOL_ROWS: &[MethodRow] = &[
    bool_row("pick", 0, bool_pick),
    bool_row("pick", 1, bool_pick),
    bool_row("roll", 0, bool_roll),
    bool_row("roll", 1, bool_roll),
];

const fn bool_row(name: &'static str, arity: u8, handler: NarrowFn) -> MethodRow {
    MethodRow {
        owner: "Bool",
        name,
        arity,
        handler: Handler::Narrow(handler),
        flags: if arity == 0 {
            RowFlags::RANDOM.or(RowFlags::TYPE_OBJECT_OK)
        } else {
            RowFlags::RANDOM
                .or(RowFlags::TYPE_OBJECT_OK)
                .or(RowFlags::ANY_ARGS)
        },
        named: &[],
    }
}

/// The error a defined `Bool` gets: only `Bool:U` candidates exist.
fn bool_no_candidate(method: &str) -> RuntimeError {
    crate::runtime::methods_signature_errors::make_multi_no_match_error(method)
}

fn bool_pick(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if matches!(target.view(), ValueView::Bool(_)) {
        return Some(Err(bool_no_candidate("pick")));
    }
    sampling::pick(&bool_values(target), args)
}

fn bool_roll(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if matches!(target.view(), ValueView::Bool(_)) {
        return Some(Err(bool_no_candidate("roll")));
    }
    sampling::roll(&bool_values(target), args)
}

/// The values `Bool.pick`/`Bool.roll` sample: `False` and `True`, in enum order.
fn bool_values(_type_object: &Value) -> Value {
    Value::array(vec![Value::FALSE, Value::TRUE])
}
