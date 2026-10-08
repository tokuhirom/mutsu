//! The receiver-mutating methods' rows (ADR-11276 §10, slice 3F).
//!
//! `push`, `pop`, `shift`, `unshift`, `append`, `prepend`, `splice`, the hash
//! and quant-hash mutators, `subst-mutate`, `substr-rw`: rows whose handler
//! writes through the receiver's container ([`Handler::Mut`](super::Handler::Mut),
//! which takes the receiver's [`ReceiverPlace`](super::ReceiverPlace)). A slice adds a family
//! module here and lists it in [`FAMILIES`]; no other file names it.
//!
//! A `Mut` row is registered by its owner only and reached through
//! [`invoke_mut`](super::invoke_mut), which asks [`owners_of`] for the owner
//! chain of the receiver's value kind.

use super::MethodRow;
use crate::value::{DispatchShape, Value, ValueView};

pub(crate) mod array;
pub(crate) mod baghash;
pub(crate) mod buf;
pub(crate) mod hash;
pub(crate) mod quanthash;
pub(crate) mod subscript;
pub(crate) mod text;

/// Every family of this group.
pub(super) static FAMILIES: &[&[MethodRow]] = &[
    array::ROWS,
    baghash::ROWS,
    buf::ROWS,
    hash::ROWS,
    quanthash::ROWS,
    subscript::ROWS,
    text::ROWS,
];

/// The owners whose rows a mutating call on `value` may dispatch to, most
/// derived first, or `None` when no mutating row can answer a value of that
/// kind. `has_name` says whether the receiver is a named binding. A `Scalar`
/// container is seen through. The owner is decided by what
/// the value is (a `Bag` is a `BagHash` only when it is mutable), exactly the
/// distinction each cascade arm used to make with its own receiver probe.
// Cost: O(1), a tag probe.
pub(crate) fn owners_of(value: &Value, has_name: bool) -> Option<&'static [&'static str]> {
    match value.descalarize().view() {
        // A method that replaces the value (`subst-mutate`) or writes back under
        // the binding (`substr-rw`) has nothing to write to without a name.
        ValueView::Str(_) if has_name => Some(&["Str"]),
        // An immutable `List` is the same shape: its rows refuse the mutators.
        ValueView::Array(..) => Some(&["Array", "List"]),
        ValueView::Hash(_) => Some(&["Hash", "Map"]),
        ValueView::Set(_, true) => Some(&["SetHash"]),
        ValueView::Set(_, false) => Some(&["Set"]),
        ValueView::Bag(_, true) => Some(&["BagHash"]),
        ValueView::Bag(_, false) => Some(&["Bag"]),
        ValueView::Mix(_, true) => Some(&["MixHash"]),
        ValueView::Mix(_, false) => Some(&["Mix"]),
        // A mutable byte buffer; a `Blob` and the encoding buffers have no
        // mutators (Rakudo declares them on `Buf` only).
        ValueView::Instance { .. }
            if value.descalarize().dispatch_shape() == Some(DispatchShape::Buf) =>
        {
            Some(&["Buf"])
        }
        _ => None,
    }
}
