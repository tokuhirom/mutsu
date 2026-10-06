//! The laziness and iteration markers of the collections (ADR-11276 §10, slice
//! 3C): `hyper`, `race`, `lazy`, `item` and `is-lazy` on `List`, `Map` and
//! `Range` (an `Array` reaches `List`'s, a `Hash` `Map`'s).

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::methods_0arg::{is_infinite_range, is_value_lazy};
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! row {
    ($owner:literal, $name:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Narrow($handler),
            flags: RowFlags::NONE,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("List", "hyper", hyper),
    row!("Map", "hyper", hyper),
    row!("Range", "hyper", hyper),
    row!("List", "race", race),
    row!("Map", "race", race),
    row!("Range", "race", race),
    row!("List", "lazy", lazy),
    row!("Map", "lazy", lazy),
    row!("Range", "lazy", lazy),
    row!("Map", "item", item),
    row!("Range", "item", item),
    row!("Range", "is-lazy", is_lazy),
];

/// `.hyper`: the elements as a `HyperSeq` (single-threaded: materialized).
// Cost: O(e), e = elements.
pub(crate) fn hyper(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if matches!(target.view(), ValueView::LazyList(_)) {
        return None;
    }
    Some(Ok(Value::hyper_seq(crate::runtime::value_to_list(target))))
}

/// `.race`: the elements as a `RaceSeq` (single-threaded: materialized).
// Cost: O(e), e = elements.
pub(crate) fn race(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if matches!(target.view(), ValueView::LazyList(_)) {
        return None;
    }
    Some(Ok(Value::race_seq(crate::runtime::value_to_list(target))))
}

/// `.lazy`: an already-lazy value stays itself (an unbounded range becomes a
/// lazy `Seq`, `(1..*).lazy.raku` is `(1, 2, ...).lazy.Seq`); an eager list
/// or finite range becomes a lazy list over its elements, marked so that
/// assigning it to an array keeps it lazy; `lazy { ... }` is a thunk that
/// runs its block on first access. A `Hash` or `Map` becomes a lazy `Seq` of
/// its pairs. A `LazyList` is the interpreter's.
// Cost: O(e) for an eager list, a finite range or a hash (its elements are
// copied once), e = elements; O(1) otherwise.
pub(crate) fn lazy(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if is_value_lazy(target) {
        // An unbounded range is lazy already, but `.lazy` still makes it a
        // `Seq`.
        if crate::runtime::unbounded_range::first(target).is_some() {
            return Some(Ok(Value::lazy_list(crate::gc::Gc::new(
                crate::value::LazyList::new_index_pipe(
                    target.clone(),
                    crate::value::IndexTransform::Identity,
                ),
            ))));
        }
        return Some(Ok(target.clone()));
    }
    let items = if let Some(items) = target.as_list_items() {
        items.to_vec()
    } else if target.is_range() || matches!(target.view(), ValueView::Hash(_)) {
        // A Hash (`Map`'s `list`) is its pairs.
        crate::runtime::utils::value_to_list(target)
    } else if matches!(target.view(), ValueView::Sub(..)) {
        // lazy { block } -- create a lazy thunk that evaluates the block on
        // first access
        return Some(Ok(Value::lazy_thunk(std::sync::Arc::new(
            crate::value::LazyThunkData {
                thunk: target.clone(),
                cache: std::sync::Mutex::new(None),
            },
        ))));
    } else {
        return Some(Ok(target.clone()));
    };
    let mut env = crate::env::Env::new();
    env.insert(
        "__mutsu_preserve_lazy_on_array_assign".to_string(),
        Value::TRUE,
    );
    Some(Ok(Value::lazy_list(crate::gc::Gc::new(
        crate::value::LazyList {
            body: vec![],
            env,
            cache: std::sync::Mutex::new(Some(items)),
            generation_state: std::sync::Mutex::new(None),
            compiled_code: None,
            compiled_fns: None,
            elems_count: None,
            scan_spec: None,
            sequence_spec: None,
            coroutine: None,
            lazy_pipe: None,
            closure_seq: None,
            walk_pending: None,
            cat_pull: None,
            array_context: false,
            list_context: false,
            cached_no_sink: false,
            itemized: false,
        },
    ))))
}

/// `.item`: the value as one item. `Value::item` is the one place that
/// decides how a value records its `$` container: an Array/List flips its
/// `ArrayKind` over the shared `Gc`, a Hash sets its itemized flag over the
/// same `HashData`, a Slip records the `$` as a flag too, and any other
/// aggregate is wrapped in a `Scalar`. A `LazyList` is the interpreter's.
// Cost: O(1) -- flips a tag/flag over the shared payload, or boxes the value
// in a `Scalar`; never copies the aggregate.
pub(crate) fn item(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::LazyList(_) => None,
        _ => Some(Ok(target.clone().item())),
    }
}

/// `Range.is-lazy`: whether an end is open.
// Cost: O(1).
pub(crate) fn is_lazy(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    target
        .is_range()
        .then(|| Ok(Value::truth(is_infinite_range(target))))
}
