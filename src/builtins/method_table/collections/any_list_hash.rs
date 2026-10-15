//! `Any.list` and `Any.hash` (ADR-11276 slice 3C remainder, #12389 item 4).
//!
//! Every owner below `Any` that has a shape of its own (`List`, `Range`,
//! `Seq`, `Map`, the quant hashes, `Capture`, `Uni`, `Blob`) has its rows
//! already; these two answer the rest: a scalar is a one-element list, and a
//! one-element list is an odd hash initializer. The cascade's arms call the
//! same [`list_of`] and [`hash_of`].

use super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueMap, ValueView};

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "Any",
        name: "hash",
        arity: 0,
        handler: Handler::Narrow(hash),
        flags: RowFlags::TYPE_OBJECT_OK,
        named: &[],
    },
];

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

/// The `Any.hash` row. A type object has no contents, so its `.hash` is the
/// empty hash (`Hash.hash` is `Hash` itself, and `Mu`, which is no `Any`, never
/// reaches the row); `Nil` is the empty hash too; a hash is itself, de-itemized;
/// a package stash is the Map of its symbols. It declines an instance of any
/// other class (a user class may have its own `hash` accessor) and a lazy list
/// (forced by the interpreter).
// Cost: O(e), e = elements of the invocant (O(1) for a scalar: an odd-element error).
fn hash(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Package(name) if name == "Hash" => Some(Ok(target.clone())),
        ValueView::Package(_) | ValueView::Nil => Some(Ok(Value::hash(ValueMap::default()))),
        ValueView::Hash(_) => super::map::hash(target, &[]),
        ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..) => {
            super::quanthash::hash(target, &[])
        }
        ValueView::Instance { .. }
            if crate::builtins::map_hash_coerce::stash_symbols(target).is_some() =>
        {
            Some(crate::builtins::map_hash_coerce::to_hash(
                target.clone(),
                false,
            ))
        }
        // A built-in class with a shape (`IO::Path`, a `Date`) is one element,
        // an odd initializer, as in Rakudo; a user class's instance may have
        // its own `hash` accessor.
        ValueView::Instance { .. } => target.dispatch_shape().is_some().then(|| hash_of(target)),
        ValueView::LazyList(_) => None,
        _ => Some(hash_of(target)),
    }
}
