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

pub(crate) mod any;
pub(crate) mod array;
pub(crate) mod autoviv;
pub(crate) mod baghash;
pub(crate) mod buf;
pub(crate) mod buf_write;
pub(crate) mod hash;
pub(crate) mod quanthash;
pub(crate) mod store;
pub(crate) mod subscript;
pub(crate) mod subscript_bind;
pub(crate) mod subscript_pos;
pub(crate) mod text;

/// Every family of this group.
pub(super) static FAMILIES: &[&[MethodRow]] = &[
    any::ROWS,
    array::ROWS,
    autoviv::ROWS,
    baghash::ROWS,
    buf::ROWS,
    buf_write::ROWS,
    hash::ROWS,
    quanthash::ROWS,
    store::ROWS,
    subscript::ROWS,
    subscript_bind::ROWS,
    subscript_pos::ROWS,
    text::ROWS,
];

/// The owners whose rows a mutating call on `value` may dispatch to, most
/// derived first, or `None` when no mutating row can answer a value of that
/// kind. A `Scalar` container is seen through. The owner is decided by what
/// the value is (a `Bag` is a `BagHash` only when it is mutable), exactly the
/// distinction each cascade arm used to make with its own receiver probe.
// Cost: O(1), a tag probe; a type object's name is matched against the
// `Buf` type names, O(n) in its length, and so is an instance's once a user
// buffer class exists.
pub(crate) fn owners_of(value: &Value) -> Option<&'static [&'static str]> {
    match value.descalarize().view() {
        // A method that replaces the value (`subst-mutate`) or writes back under
        // the binding (`substr-rw`) has nothing to write to without a name, so
        // `Str`'s rows refuse or decline an unbound receiver.
        ValueView::Str(_) => Some(&["Str"]),
        // `Range` refuses the mutators it declares (`X::Immutable`).
        ValueView::Range(..)
        | ValueView::RangeExcl(..)
        | ValueView::RangeExclStart(..)
        | ValueView::RangeExclBoth(..)
        | ValueView::GenericRange { .. } => Some(&["Range"]),
        // No mutator of its own: `Any`'s rows, which end every chain, answer
        // an undefined receiver (autovivification) and refuse a defined
        // scalar (`5.push`).
        ValueView::Nil
        | ValueView::Int(_)
        | ValueView::BigInt(_)
        | ValueView::Num(_)
        | ValueView::Bool(_)
        | ValueView::Rat(..)
        | ValueView::FatRat(..)
        | ValueView::BigRat(..)
        | ValueView::Complex(..) => Some(&[]),
        ValueView::Package(name) if name == "Any" || name == "Mu" => Some(&[]),
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
        // mutators (Rakudo declares them on `Buf` only), and `Blob`'s rows
        // refuse the calls its protos name.
        // A user class composing a buffer role over a `VMArray` body
        // (`class C does Buf[uint8] is repr('VMArray')`) is the buffer it
        // composes.
        ValueView::Instance { class_name, .. } => match value.descalarize().dispatch_shape() {
            Some(DispatchShape::Buf) => Some(&["Buf"]),
            Some(DispatchShape::Blob) => Some(&["Blob"]),
            _ if crate::value::value_buf::any_buffer_class() => {
                match crate::value::value_buf::buffer_class_type(&class_name.resolve()) {
                    Some(kind) if kind.starts_with("Buf") => Some(&["Buf"]),
                    Some(_) => Some(&["Blob"]),
                    None => None,
                }
            }
            _ => None,
        },
        // A `Buf` type object: its `write-*` rows answer a fresh buffer.
        ValueView::Package(name) if crate::runtime::utils::is_buf_like_class(&name.resolve()) => {
            Some(&["Buf"])
        }
        _ => None,
    }
}

/// The owner chain of a container *type object* (`Array`, `SetHash`, ...),
/// for the refusal step of [`invoke_mut`](super::invoke_mut) (#13881): a row
/// Rakudo declares for a defined invocant only refuses the type object with
/// its concreteness or no-match error. `None` for any other receiver, which
/// [`owners_of`] answers.
// Cost: O(1), a tag probe and a short name compare.
pub(crate) fn type_object_owners(value: &Value) -> Option<&'static [&'static str]> {
    let ValueView::Package(name) = value.descalarize().view() else {
        return None;
    };
    Some(match name.resolve().as_str() {
        "Array" => &["Array", "List"],
        "List" => &["List"],
        "Hash" => &["Hash", "Map"],
        "Map" => &["Map"],
        "SetHash" => &["SetHash"],
        "BagHash" => &["BagHash"],
        "MixHash" => &["MixHash"],
        "Set" => &["Set"],
        "Bag" => &["Bag"],
        "Mix" => &["Mix"],
        _ => return None,
    })
}
