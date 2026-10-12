//! Zero-argument `Any` collection methods shared by the row and cascade paths.

use super::{Handler, MethodRow, RowFlags};
use crate::value::{DispatchShape, RuntimeError, Value, ValueView};

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "Any",
        name: "elems",
        arity: 0,
        handler: Handler::Pure(elems),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Any",
        name: "end",
        arity: 0,
        handler: Handler::Narrow(end),
        flags: RowFlags::TYPE_OBJECT_OK,
        named: &[],
    },
    MethodRow {
        owner: "Any",
        name: "keys",
        arity: 0,
        handler: Handler::Pure(keys),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Any",
        name: "values",
        arity: 0,
        handler: Handler::Pure(values),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Any",
        name: "kv",
        arity: 0,
        handler: Handler::Pure(kv),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Any",
        name: "pairs",
        arity: 0,
        handler: Handler::Pure(pairs),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Any",
        name: "antipairs",
        arity: 0,
        handler: Handler::Pure(antipairs),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Any",
        name: "reverse",
        arity: 0,
        handler: Handler::Narrow(reverse),
        flags: RowFlags::TYPE_OBJECT_OK,
        named: &[],
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
    ) || matches!(
        // `Date`, `DateTime`, `Instant` and `Duration` are neither `Positional`
        // nor `Associative`: `Any`'s one-element semantics are theirs.
        target.dispatch_shape(),
        Some(
            DispatchShape::Date
                | DispatchShape::DateTime
                | DispatchShape::Instant
                | DispatchShape::Duration
        )
    )
}

// Cost: O(1), Any's scalar invocant is one element.
pub(crate) fn elems(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    debug_assert!(scalar_like(target));
    Ok(Value::int(1))
}

/// `Any.end`: the last index of whatever the invocant reads as a list. A
/// scalar has only index zero; a lazy list has no last index (Rakudo throws
/// `X::Cannot::Lazy`); a `LazyList` still being produced declines, so the
/// interpreter forces it.
// Cost: O(1) on a scalar, a reified list/array, hash, set/bag/mix or buf (a
// length read).
pub(crate) fn end(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    use crate::value::value_buf::buf_len_or_zero;
    let last = |len: usize| Some(Ok(Value::int(len as i64 - 1)));
    if scalar_like(target) {
        return Some(Ok(Value::int(0)));
    }
    if crate::builtins::methods_0arg::is_lazy_count_source(target) {
        return Some(Ok(crate::runtime::utils::cannot_lazy_failure("elems")));
    }
    if let Some(items) = target.as_list_items() {
        return last(items.len());
    }
    match target.view() {
        ValueView::Hash(items) => last(items.len()),
        ValueView::Set(items, _) => last(items.len()),
        ValueView::Bag(items, _) => last(items.len()),
        ValueView::Mix(items, _) => last(items.len()),
        ValueView::Junction { values, .. } => last(values.len()),
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if crate::runtime::utils::is_native_elems_class(&class_name.resolve()) => {
            last(buf_len_or_zero(&attributes))
        }
        ValueView::LazyList(_) => None,
        // A buffer-backed instance of any class (upstream NativeCall's
        // `CArray[T]`, a mixin over `CArray`) counts its elements.
        _ if let Some((_, attributes)) = crate::value::value_buf::buf_target(target) => {
            last(buf_len_or_zero(&attributes))
        }
        _ => Some(Ok(Value::int(0))),
    }
}

// Cost: O(1), the single scalar index is produced eagerly.
pub(crate) fn keys(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    if matches!(target.view(), ValueView::Bool(_)) {
        return Ok(Value::seq(vec![
            Value::str("True".to_string()),
            Value::str("False".to_string()),
        ]));
    }
    debug_assert!(scalar_like(target));
    Ok(Value::seq(vec![Value::int(0)]))
}

// Cost: O(1), the single scalar value is copied into the result Seq.
pub(crate) fn values(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    if matches!(target.view(), ValueView::Bool(_)) {
        return Ok(Value::seq(vec![Value::int(1), Value::int(0)]));
    }
    debug_assert!(scalar_like(target));
    Ok(Value::seq(vec![target.clone()]))
}

// Cost: O(1), one index/value pair is produced.
pub(crate) fn kv(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    if matches!(target.view(), ValueView::Bool(_)) {
        return Ok(Value::seq(vec![
            Value::str("True".to_string()),
            Value::int(1),
            Value::str("False".to_string()),
            Value::int(0),
        ]));
    }
    debug_assert!(scalar_like(target));
    Ok(Value::seq(vec![Value::int(0), target.clone()]))
}

// Cost: O(1), one index/value Pair is produced.
pub(crate) fn pairs(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    if matches!(target.view(), ValueView::Bool(_)) {
        return Ok(Value::seq(vec![
            Value::pair("True".to_string(), Value::int(1)),
            Value::pair("False".to_string(), Value::int(0)),
        ]));
    }
    debug_assert!(scalar_like(target));
    Ok(Value::seq(vec![Value::value_pair(
        Value::int(0),
        target.clone(),
    )]))
}

// Cost: O(1), one value/index Pair is produced.
pub(crate) fn antipairs(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    if matches!(target.view(), ValueView::Bool(_)) {
        return Ok(Value::seq(vec![
            Value::value_pair(Value::int(1), Value::str("True".to_string())),
            Value::value_pair(Value::int(0), Value::str("False".to_string())),
        ]));
    }
    debug_assert!(scalar_like(target));
    Ok(Value::seq(vec![Value::value_pair(
        target.clone(),
        Value::int(0),
    )]))
}

/// `Any.reverse`: a scalar reverses to its one-element Seq, a hash to its
/// pairs reversed, and anything else to its list reversed (`List.reverse`'s
/// handler).
// Cost: O(1) for a scalar; O(e) otherwise, e = elements.
pub(crate) fn reverse(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if scalar_like(target) {
        return Some(Ok(Value::seq(vec![target.clone()])));
    }
    if matches!(target.view(), ValueView::Hash(_)) {
        let mut items = crate::runtime::utils::value_to_list(target);
        items.reverse();
        return Some(Ok(Value::seq(items)));
    }
    super::list::reverse(target, args)
}
