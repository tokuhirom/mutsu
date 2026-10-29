//! The laziness and iteration markers of the collections (ADR-11276 §10, slice
//! 3C): `hyper`, `race`, `lazy`, `item` and `is-lazy` on `List`, `Map` and
//! `Range` (an `Array` reaches `List`'s, a `Hash` `Map`'s).

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::methods_0arg::{is_infinite_range, is_value_lazy};
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! row {
    ($owner:literal, $name:literal, $handler:ident) => {
        row!($owner, $name, $handler, RowFlags::NONE)
    };
    ($owner:literal, $name:literal, $handler:ident, $flags:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Narrow($handler),
            flags: $flags,
            named: &[],
        }
    };
}

/// A one-argument row.
macro_rules! row1 {
    ($owner:literal, $name:literal, $handler:ident, $flags:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 1,
            handler: Handler::Narrow($handler),
            flags: $flags,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row1!(
        "List",
        "lazy-if",
        lazy_if,
        RowFlags::TYPE_OBJECT_OK.or(RowFlags::ANY_ARGS)
    ),
    row1!(
        "Map",
        "lazy-if",
        lazy_if,
        RowFlags::TYPE_OBJECT_OK.or(RowFlags::ANY_ARGS)
    ),
    row1!("Range", "lazy-if", lazy_if, RowFlags::ANY_ARGS),
    // `Any.lazy-if` ignores the flag: only an `Iterable` can be made lazy.
    row1!(
        "Any",
        "lazy-if",
        any_lazy_if,
        RowFlags::TYPE_OBJECT_OK.or(RowFlags::ANY_ARGS)
    ),
    row!("List", "hyper", hyper),
    row!("Map", "hyper", hyper),
    row!("Range", "hyper", hyper),
    row!("List", "race", race),
    row!("Map", "race", race),
    row!("Range", "race", race),
    // A `List`/`Array`/`Hash` type object lists as itself: `Array.lazy` is
    // `(Array).lazy.Seq`.
    row!("List", "lazy", lazy, RowFlags::TYPE_OBJECT_OK),
    row!("Map", "lazy", lazy, RowFlags::TYPE_OBJECT_OK),
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
/// lazy `Seq`, `(1..*).lazy.raku` is `(1, 2, ...).lazy.Seq`; a lazy
/// `LazyList` is re-tagged to stay lazy when assigned); an eager list
/// or finite range becomes a lazy list over its elements, marked so that
/// assigning it to an array keeps it lazy; `lazy { ... }` is a thunk that
/// runs its block on first access. A `Hash` or `Map` becomes a lazy `Seq` of
/// its pairs. A `LazyList` that is not lazy is the interpreter's.
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
        if let ValueView::LazyList(list) = target.view() {
            return Some(Ok(retag_lazy_list(&list)));
        }
        return Some(Ok(target.clone()));
    }
    let items = if matches!(target.view(), ValueView::Package(_)) {
        vec![target.clone()]
    } else if let Some(items) = target.as_list_items() {
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
            hyper_kind: Default::default(),
            hyper_config: None,
            cached_no_sink: false,
            deleted_indices: Default::default(),
            descriptor: None,
            generator_origin: None,
            itemized: false,
        },
    ))))
}

/// `Iterable.lazy-if($flag)`: `self.lazy` when the flag is true, `self`
/// otherwise.
// Cost: O(1) for a false flag, else as `lazy`.
pub(crate) fn lazy_if(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match args {
        [flag] if flag.truthy() => lazy(target, &[]),
        [_] => Some(Ok(target.clone())),
        _ => None,
    }
}

/// `Any.lazy-if($flag)`: the receiver itself.
// Cost: O(1).
fn any_lazy_if(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    matches!(args, [_]).then(|| Ok(target.clone()))
}

/// A lazy `LazyList` re-tagged so that assigning it to an array keeps it
/// lazy: the same list (its body, cache and producer state), marked with
/// `__mutsu_preserve_lazy_on_array_assign`.
// Cost: O(c), c = elements already cached (the cache is copied).
fn retag_lazy_list(list: &crate::value::LazyList) -> Value {
    let mut env = list.env.clone();
    env.insert(
        "__mutsu_preserve_lazy_on_array_assign".to_string(),
        Value::TRUE,
    );
    let cache = list.cache.lock().unwrap().clone();
    Value::lazy_list(crate::gc::Gc::new(crate::value::LazyList {
        body: list.body.clone(),
        env,
        cache: std::sync::Mutex::new(cache),
        // The producer's own trailing history: a closure sequence generates its
        // next element from it, so a retagged copy must carry it along.
        generation_state: std::sync::Mutex::new(
            list.generation_state
                .lock()
                .unwrap_or_else(|e| e.into_inner())
                .clone(),
        ),
        compiled_code: list.compiled_code.clone(),
        compiled_fns: list.compiled_fns.clone(),
        elems_count: list.elems_count.clone(),
        scan_spec: list
            .scan_spec
            .as_ref()
            .map(|s| std::sync::Mutex::new(s.lock().unwrap().clone())),
        sequence_spec: list.sequence_spec.clone(),
        coroutine: list
            .coroutine
            .as_ref()
            .map(|c| std::sync::Mutex::new(c.lock().unwrap().clone())),
        lazy_pipe: list
            .lazy_pipe
            .as_ref()
            .map(|p| std::sync::Mutex::new(p.lock().unwrap().clone())),
        closure_seq: list
            .closure_seq
            .as_ref()
            .map(|c| std::sync::Mutex::new(c.lock().unwrap().clone())),
        walk_pending: list
            .walk_pending
            .as_ref()
            .map(|w| std::sync::Mutex::new(w.lock().unwrap().clone())),
        cat_pull: list
            .cat_pull
            .as_ref()
            .map(|c| std::sync::Mutex::new(c.lock().unwrap().clone())),
        array_context: list.array_context,
        list_context: list.list_context,
        hyper_kind: list.hyper_kind,
        hyper_config: list.hyper_config,
        cached_no_sink: list.cached_no_sink,
        deleted_indices: Default::default(),
        descriptor: None,
        generator_origin: None,
        itemized: list.itemized,
    }))
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
