//! Borrowing access to a list-like value's own items (#9162).
//!
//! `value_to_list` / `value_to_list_for_receiver` answer with an owned `Vec`,
//! which is an O(e) copy even when the caller only reads one slot (`.pick`),
//! the last k (`.tail(k)`), a suffix (`.skip(n)`) or the length (`==`).
//! These helpers hand the caller the items the value already holds, and only
//! decompose into a fresh `Vec` for the shapes that do not store them reified.

use super::{value_to_list, value_to_list_for_receiver};
use crate::value::{Value, ValueView};

/// Run `f` over `val`'s items in LIST context -- exactly the items
/// [`value_to_list`] would return, but borrowed where `val` is a plain
/// `Array`/`List` or a reified `Seq`.
///
/// The slice borrows `val`'s storage, so `f` must not run user code that
/// could mutate `val` (no callbacks, no user methods).
// Cost: O(1) + f on a non-itemized Array/List or a Seq; O(e) + f otherwise,
// e = elements (the `value_to_list` fallback copy).
pub(crate) fn with_list_items<R>(val: &Value, f: impl FnOnce(&[Value]) -> R) -> R {
    match val.view() {
        ValueView::Array(items, kind) if !kind.is_itemized() => f(items.as_slice()),
        ValueView::Seq(items) | ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => {
            f(items.as_slice())
        }
        _ => f(&value_to_list(val)),
    }
}

/// [`with_list_items`] for a method-call RECEIVER: the receiver's own items,
/// ignoring its own itemization -- exactly what
/// [`value_to_list_for_receiver`] would return, borrowed where possible.
///
/// The same no-user-code restriction on `f` applies.
// Cost: O(1) + f on an Array/List (itemized or not) or a Seq; O(e) + f
// otherwise, e = elements (the `value_to_list_for_receiver` fallback copy).
pub(crate) fn with_receiver_items<R>(val: &Value, f: impl FnOnce(&[Value]) -> R) -> R {
    let bare = val.descalarize();
    match bare.view() {
        ValueView::Array(items, _) => f(items.as_slice()),
        ValueView::Seq(items) | ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => {
            f(items.as_slice())
        }
        _ => f(&value_to_list_for_receiver(val)),
    }
}

/// The number of items [`value_to_list`] would return, without copying them
/// for the shapes that hold their items reified.
// Cost: O(1) on a non-itemized Array/List or a Seq; O(e) otherwise.
pub(crate) fn list_items_len(val: &Value) -> usize {
    with_list_items(val, <[Value]>::len)
}
