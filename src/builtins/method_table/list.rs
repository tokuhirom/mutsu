//! `List`'s rows (`Array` inherits them through its MRO).

use super::{Handler, MethodRow};
use crate::value::{RuntimeError, Value, ValueView};

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "List",
        name: "elems",
        arity: 0,
        handler: Handler::Pure(elems),
    },
    MethodRow {
        owner: "List",
        name: "end",
        arity: 0,
        handler: Handler::Pure(end),
    },
    MethodRow {
        owner: "List",
        name: "Bool",
        arity: 0,
        handler: Handler::Pure(bool),
    },
    MethodRow {
        owner: "List",
        name: "join",
        arity: 0,
        handler: Handler::Narrow(join),
    },
    MethodRow {
        owner: "List",
        name: "join",
        arity: 1,
        handler: Handler::Narrow(join),
    },
];

fn len(target: &Value) -> i64 {
    target.as_list_items().map_or(0, |items| items.len() as i64)
}

// Cost: O(1), a length read on the reified items.
fn elems(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::int(len(target)))
}

// Cost: O(1), a length read on the reified items.
fn end(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::int(len(target) - 1))
}

// Cost: O(1), an emptiness test.
fn bool(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::truth(target.truthy()))
}

/// `List.join($sep = "")` on a plain list or array, or `None` when an element
/// needs more than the pure stringification (see [`join_items`]) or is
/// undefined: Rakudo warns for each undefined element, which only the
/// interpreter path can do (#11838).
// Cost: O(e + t), e = elements, t = total chars of the result.
fn join(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    // `.join` stringifies every element, so a zero-denominator Rational among
    // them dies like its own `.Str` (GH #9621).
    if let Err(err) = crate::runtime::utils::check_str_coercion_zero_denominator(target) {
        return Some(Err(err));
    }
    let sep = args.first().map(Value::to_string_value).unwrap_or_default();
    let items = join_source_items(target)?;
    if items.iter().any(|v| {
        v.with_deref(|inner| {
            matches!(
                inner.descalarize().view(),
                ValueView::Nil | ValueView::Package(_)
            )
        })
    }) {
        return None;
    }
    join_items(&items, &sep).map(Ok)
}

/// The elements `.join` reads from a list-like receiver: a hole in an array
/// with an `is default(...)` value reads as that value, not the `Any` marker
/// the slot holds. `None` for a receiver with no reified items.
// Cost: O(1) when the receiver has no default; O(e) otherwise, e = elements.
pub(crate) fn join_source_items(target: &Value) -> Option<std::borrow::Cow<'_, [Value]>> {
    if let ValueView::Array(data, _) = target.view()
        && let std::borrow::Cow::Owned(resolved) = data.items_with_default()
    {
        return Some(std::borrow::Cow::Owned(resolved));
    }
    target.as_list_items().map(std::borrow::Cow::Borrowed)
}

/// Join already-reified elements with `sep`, or `None` when an element needs
/// the interpreter to stringify it: an instance or mixin (a user `Str` may
/// apply, also when nested in an inner list), a Junction (the whole `join`
/// threads over its eigenstates), or a deferred Seq whose callback has not
/// run. The one implementation the `List.join` row and the cascade arms share.
// Cost: O(e + t), e = elements, t = total chars of the result.
pub(crate) fn join_items(items: &[Value], sep: &str) -> Option<Value> {
    if items.iter().any(|v| {
        v.with_deref(|inner| {
            let inner = inner.descalarize();
            matches!(
                inner.view(),
                ValueView::Instance { .. }
                    | ValueView::Mixin(..)
                    | ValueView::Junction { .. }
                    | ValueView::LazyList(_)
                    | ValueView::LazyThunk(_)
            ) || matches!(inner.view(), ValueView::Seq(s) if s.awaits_vm_reify())
        }) || crate::value::gist::str_needs_dispatch(v)
    }) {
        return None;
    }
    Some(Value::str(
        items
            .iter()
            .map(Value::to_str_context)
            .collect::<Vec<_>>()
            .join(sep),
    ))
}
