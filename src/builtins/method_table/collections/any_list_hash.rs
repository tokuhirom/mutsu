//! `Any.list` and `Any.hash` (ADR-11276 slice 3C remainder, #12389 item 4).
//!
//! Every owner below `Any` that has a shape of its own (`List`, `Range`,
//! `Seq`, `Map`, the quant hashes, `Capture`, `Uni`, `Blob`) has its rows
//! already; these two answer the rest: a scalar is a one-element list, and a
//! one-element list is an odd hash initializer. The cascade's arms call the
//! same [`list_of`] and [`hash_of`].

use super::{Handler, MethodRow, RowFlags};
use crate::value::{DispatchShape, RuntimeError, Value, ValueView};

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "Any",
        name: "list",
        arity: 0,
        handler: Handler::Narrow(list),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Any",
        name: "hash",
        arity: 0,
        handler: Handler::Narrow(hash),
        flags: RowFlags::NONE,
        named: &[],
    },
];

/// Whether `target` is a value `.list`/`.hash` read as one element: a scalar,
/// or one of the built-in temporal classes (never a subclass, whose
/// attributes may be named `list`).
// Cost: O(1).
fn is_plain_scalar(target: &Value) -> bool {
    matches!(
        target.view(),
        ValueView::Str(_)
            | ValueView::Int(_)
            | ValueView::BigInt(_)
            | ValueView::Num(_)
            | ValueView::Rat(..)
            | ValueView::FatRat(..)
            | ValueView::BigRat(..)
            | ValueView::Complex(..)
            | ValueView::Bool(_)
    ) || matches!(
        target.dispatch_shape(),
        Some(
            DispatchShape::Date
                | DispatchShape::DateTime
                | DispatchShape::Instant
                | DispatchShape::Duration
        )
    )
}

/// The elements `.list` / `.Array` of a value with no list shape reads: a hash
/// is its pairs, a set/bag/mix its pairs, anything else the one element.
// Cost: O(e), e = entries of a hash or quant hash; O(1) otherwise.
pub(crate) fn list_of(target: &Value) -> Vec<Value> {
    match target.view() {
        ValueView::Hash(map) => super::map::list_pairs(&map),
        ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..) => {
            crate::runtime::utils::value_to_list(target)
        }
        _ => vec![target.clone()],
    }
}

/// The `Any.list` row.
// Cost: O(1).
fn list(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    is_plain_scalar(target).then(|| Ok(Value::array(list_of(target))))
}

/// `Any.hash`: the receiver's elements as a hash initializer. An `Array`,
/// `Seq` or `Slip` is read even when itemized (`$(:a, :b).hash`).
// Cost: O(e), e = elements of the invocant.
pub(crate) fn hash_of(target: &Value) -> Result<Value, RuntimeError> {
    let items = match target.view() {
        ValueView::Array(items, _) => items.iter().cloned().collect(),
        ValueView::Seq(items) => items.iter().cloned().collect(),
        ValueView::Slip(items) => items.iter().cloned().collect(),
        _ => crate::runtime::utils::value_to_list(target),
    };
    crate::runtime::utils::build_hash_from_items(items)
}

/// The `Any.hash` row. It declines what the cascade routes elsewhere: a type
/// object, a `Nil`, an instance (a user class may have its own `hash`
/// accessor, and a stash is a Map of its symbols), a hash, a quant hash (their
/// own rows) and a lazy list (forced by the interpreter).
// Cost: O(e), e = elements of the invocant (O(1) for a scalar: an odd-element error).
fn hash(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let readable = match target.view() {
        ValueView::Package(_)
        | ValueView::Nil
        | ValueView::Hash(_)
        | ValueView::Set(..)
        | ValueView::Bag(..)
        | ValueView::Mix(..)
        | ValueView::LazyList(_) => false,
        ValueView::Instance { .. } => is_plain_scalar(target),
        _ => true,
    };
    readable.then(|| hash_of(target))
}
