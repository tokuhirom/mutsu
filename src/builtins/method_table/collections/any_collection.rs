//! Zero-argument `Any` collection methods shared by the row and cascade paths.

use super::any_enum_views::View;
use super::{Handler, MethodRow, RowFlags};
use crate::value::{DispatchShape, RuntimeError, Value, ValueView};

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "Any",
        name: "elems",
        arity: 0,
        handler: Handler::Narrow(elems),
        flags: RowFlags::TYPE_OBJECT_OK,
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

/// The `Bool` enumeration's `view`.
// Cost: O(1), two values.
fn enum_view(view: View) -> Value {
    super::any_enum_views::view(view, &super::any_enum_views::bool_variants())
}

/// `Any.elems`: the number of elements of whatever the invocant reads as a
/// list. A scalar (and a type object) is one element; a genuinely lazy list
/// has no count (a `Failure` of `X::Cannot::Lazy`, as in Rakudo); a lazy list
/// that is not lazy declines, so the interpreter reifies and counts it. A
/// receiver whose own type declares `elems` (a `List`, a `Range`, a quant hash)
/// answers its own row first; this one answers the rest: a `Junction`'s
/// eigenstates, a native `CArray`/`Buf` instance, a `Channel`, a `Stash`.
// Cost: O(1) on a scalar, a reified list/array, hash or integer Range; a
// finite LazyList is forced first (O(e), deferred to the runtime).
pub(crate) fn elems(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if scalar_like(target) || matches!(target.view(), ValueView::Package(_)) {
        return Some(Ok(Value::int(1)));
    }
    if let ValueView::LazyList(list) = target.view() {
        // Only a GENUINELY lazy list refuses `.elems`; a plain `gather {...}`
        // Seq, a `.map`/`.grep` pipe over a finite source and an
        // `IO::CatHandle` pull are not lazy (`is_genuinely_lazy`).
        if !list.is_genuinely_lazy() {
            return None;
        }
        return Some(Ok(crate::runtime::utils::cannot_lazy_failure("elems")));
    }
    // A lazy (infinite-backed) array cannot report its element count; checked
    // before `as_list_items`, which would answer the capped backing length.
    if let ValueView::Array(_, kind) = target.view()
        && kind.is_lazy()
    {
        return Some(Ok(crate::runtime::utils::cannot_lazy_failure("elems")));
    }
    if let Some(items) = target.as_list_items() {
        return Some(Ok(Value::int(items.len() as i64)));
    }
    Some(Ok(match target.view() {
        ValueView::Hash(items) => Value::int(items.len() as i64),
        ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..) => {
            return super::quanthash::elems(target, &[]);
        }
        ValueView::Junction { values, .. } => Value::int(values.len() as i64),
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if crate::runtime::utils::is_native_elems_class(&class_name.resolve()) => {
            // An unmanaged `CArray` (a `nativecast`ed handle: a bare C address
            // and no storage) has no length, and Rakudo throws rather than
            // answering 0 (`NativeHelpers::Blob`'s `dies-ok { $au.elems }`).
            if !crate::value::value_buf::has_buf_elems(&attributes)
                && attributes.contains_key("address")
            {
                return Some(Err(RuntimeError::new(
                    "Don't know how many elements a C array returned from a library",
                )));
            }
            Value::int(crate::value::value_buf::buf_len_or_zero(&attributes) as i64)
        }
        ValueView::Channel(_) => {
            return crate::builtins::method_table::promise_channel::channel_elems(target, &[]);
        }
        _ if target.is_range() => return super::range::elems(target, &[]),
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if crate::value::types::is_stash_class_name(class_name.as_str()) => {
            match attributes.as_map().get("symbols").map(Value::view) {
                Some(ValueView::Hash(map)) => Value::int(map.len() as i64),
                _ => Value::int(0),
            }
        }
        _ => Value::int(1),
    }))
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
    // A `Match` is `self.list.end`: its positional captures (ADR-11276 §9.96).
    if target.is_match_instance() {
        let list = target.match_list();
        return last(match list.as_ref().map(Value::view) {
            Some(ValueView::Array(items, _)) => items.len(),
            _ => 0,
        });
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
    // `Any(Bool)`: a `Bool` lists the `Bool` enumeration (ADR-11276 §9.145).
    if matches!(target.view(), ValueView::Bool(_)) {
        return Ok(enum_view(View::Keys));
    }
    debug_assert!(scalar_like(target));
    Ok(Value::seq(vec![Value::int(0)]))
}

// Cost: O(1), the single scalar value is copied into the result Seq.
pub(crate) fn values(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    // `Any(Bool)`: a `Bool` lists the `Bool` enumeration (ADR-11276 §9.145).
    if matches!(target.view(), ValueView::Bool(_)) {
        return Ok(enum_view(View::Values));
    }
    debug_assert!(scalar_like(target));
    Ok(Value::seq(vec![target.clone()]))
}

// Cost: O(1), one index/value pair is produced.
pub(crate) fn kv(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    // `Any(Bool)`: a `Bool` lists the `Bool` enumeration (ADR-11276 §9.145).
    if matches!(target.view(), ValueView::Bool(_)) {
        return Ok(enum_view(View::Kv));
    }
    debug_assert!(scalar_like(target));
    Ok(Value::seq(vec![Value::int(0), target.clone()]))
}

// Cost: O(1), one index/value Pair is produced.
pub(crate) fn pairs(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    // `Any(Bool)`: a `Bool` lists the `Bool` enumeration (ADR-11276 §9.145).
    if matches!(target.view(), ValueView::Bool(_)) {
        return Ok(enum_view(View::Pairs));
    }
    debug_assert!(scalar_like(target));
    Ok(Value::seq(vec![Value::value_pair(
        Value::int(0),
        target.clone(),
    )]))
}

// Cost: O(1), one value/index Pair is produced.
pub(crate) fn antipairs(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    // `Any(Bool)`: a `Bool` lists the `Bool` enumeration (ADR-11276 §9.145).
    if matches!(target.view(), ValueView::Bool(_)) {
        return Ok(enum_view(View::Antipairs));
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
