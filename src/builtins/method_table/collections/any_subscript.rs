//! `Any`'s subscript protocol (ADR-11276 §9.106): `AT-POS`, `EXISTS-POS`,
//! `AT-KEY` and `EXISTS-KEY` for a value that is neither `Positional` nor
//! `Associative`.
//!
//! Under a positional subscript such a value is the one-element list holding
//! itself: index 0 is the value, any other index is out of range
//! (`5.AT-POS(1)`). A type object answers itself at 0 too, but holds no
//! element (`Int.EXISTS-POS(0)` is `False`). It has no keys: a defined value
//! fails the associative subscript, a type object autovivifies to `Any`.
//! The index is an `Int` ([`RowFlags::INDEX_ARG`]; `index_arg` binds the
//! others), a key any object. A type whose own row comes first in its MRO
//! (`List`, `Hash`, `Range`, ...) never reaches these.

use super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! row {
    ($name:literal, $handler:ident, $flags:expr) => {
        MethodRow {
            owner: "Any",
            name: $name,
            arity: 1,
            handler: Handler::Narrow($handler),
            flags: $flags.or(RowFlags::TYPE_OBJECT_OK),
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("AT-POS", at_pos, RowFlags::INDEX_ARG),
    row!("EXISTS-POS", exists_pos, RowFlags::INDEX_ARG),
    row!(
        "AT-KEY",
        at_key,
        RowFlags::ANY_ARGS.or(RowFlags::OBJECT_ARGS)
    ),
    row!(
        "EXISTS-KEY",
        exists_key,
        RowFlags::ANY_ARGS.or(RowFlags::OBJECT_ARGS)
    ),
];

/// The index an `INDEX_ARG` row was handed, or `None` for anything else.
// Cost: O(1).
fn index(arg: &Value) -> Option<i64> {
    match arg.view() {
        ValueView::Int(i) => Some(i),
        _ => None,
    }
}

/// `Any.AT-POS(Int:D $pos)`: the receiver at index 0, an `X::OutOfRange`
/// failure at any other.
// Cost: O(1).
pub(crate) fn at_pos(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let pos = index(&args[0])?;
    Some(Ok(if pos == 0 {
        target.descalarize().clone()
    } else {
        RuntimeError::out_of_range_failure("Index", Value::int(pos), "0..0")
    }))
}

/// `Any.EXISTS-POS(Int:D $pos)`: only index 0 of a defined value exists.
// Cost: O(1).
pub(crate) fn exists_pos(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let pos = index(&args[0])?;
    Some(Ok(Value::truth(pos == 0 && !is_type_object(target))))
}

/// `Any.AT-KEY($key)`: a type object autovivifies to `Any`; a defined value
/// fails, as it does not do `Associative`.
// Cost: O(1).
pub(crate) fn at_key(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(if is_type_object(target) {
        Value::package(crate::symbol::wk::any())
    } else {
        RuntimeError::assoc_indexing_failure(&crate::value::types::what_type_name(target))
    }))
}

/// `Any.EXISTS-KEY($key)`: a value that is not `Associative` has no keys.
// Cost: O(1).
pub(crate) fn exists_key(_target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(Value::FALSE))
}

/// Whether the receiver is a type object.
// Cost: O(1), a tag probe.
fn is_type_object(target: &Value) -> bool {
    matches!(target.descalarize().view(), ValueView::Package(_))
}
