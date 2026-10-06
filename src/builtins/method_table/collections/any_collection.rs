//! Zero-argument `Any` collection methods shared by the row and cascade paths.

use super::{Handler, MethodRow};
use crate::value::{RuntimeError, Value, ValueView};

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "Any",
        name: "elems",
        arity: 0,
        handler: Handler::Pure(elems),
    },
    MethodRow {
        owner: "Any",
        name: "end",
        arity: 0,
        handler: Handler::Pure(end),
    },
    MethodRow {
        owner: "Any",
        name: "keys",
        arity: 0,
        handler: Handler::Pure(keys),
    },
    MethodRow {
        owner: "Any",
        name: "values",
        arity: 0,
        handler: Handler::Pure(values),
    },
    MethodRow {
        owner: "Any",
        name: "kv",
        arity: 0,
        handler: Handler::Pure(kv),
    },
    MethodRow {
        owner: "Any",
        name: "pairs",
        arity: 0,
        handler: Handler::Pure(pairs),
    },
    MethodRow {
        owner: "Any",
        name: "antipairs",
        arity: 0,
        handler: Handler::Pure(antipairs),
    },
    MethodRow {
        owner: "Any",
        name: "reverse",
        arity: 0,
        handler: Handler::Narrow(reverse),
    },
];

/// Whether the value has Any's one-element collection semantics rather than a
/// more specific collection implementation.
pub(crate) fn scalar_like(target: &Value) -> bool {
    matches!(
        target.view(),
        ValueView::Bool(_)
            | ValueView::Str(_)
            | ValueView::Int(_)
            | ValueView::BigInt(_)
            | ValueView::Num(_)
            | ValueView::Rat(..)
            | ValueView::FatRat(..)
            | ValueView::BigRat(..)
            | ValueView::Complex(..)
    )
}

// Cost: O(1), Any's scalar invocant is one element.
pub(crate) fn elems(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    debug_assert!(scalar_like(target));
    Ok(Value::int(1))
}

// Cost: O(1), Any's scalar invocant has only index zero.
pub(crate) fn end(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    match target.view() {
        ValueView::Hash(items) => Ok(Value::int(items.len() as i64 - 1)),
        _ => {
            debug_assert!(scalar_like(target));
            Ok(Value::int(0))
        }
    }
}

// Cost: O(1), the single scalar index is produced eagerly.
pub(crate) fn keys(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    debug_assert!(scalar_like(target));
    Ok(Value::seq(vec![Value::int(0)]))
}

// Cost: O(1), the single scalar value is copied into the result Seq.
pub(crate) fn values(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    debug_assert!(scalar_like(target));
    Ok(Value::seq(vec![target.clone()]))
}

// Cost: O(1), one index/value pair is produced.
pub(crate) fn kv(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    debug_assert!(scalar_like(target));
    Ok(Value::seq(vec![Value::int(0), target.clone()]))
}

// Cost: O(1), one index/value Pair is produced.
pub(crate) fn pairs(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    debug_assert!(scalar_like(target));
    Ok(Value::seq(vec![Value::value_pair(
        Value::int(0),
        target.clone(),
    )]))
}

// Cost: O(1), one value/index Pair is produced.
pub(crate) fn antipairs(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    debug_assert!(scalar_like(target));
    Ok(Value::seq(vec![Value::value_pair(
        target.clone(),
        Value::int(0),
    )]))
}

// Cost: O(1), a scalar reverses to its one-element Seq.
pub(crate) fn reverse(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if scalar_like(target) {
        return Some(Ok(Value::seq(vec![target.clone()])));
    }
    if matches!(target.view(), ValueView::Hash(_)) {
        let mut items = crate::runtime::utils::value_to_list(target);
        items.reverse();
        return Some(Ok(Value::seq(items)));
    }
    None
}
