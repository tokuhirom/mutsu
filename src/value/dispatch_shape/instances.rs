//! The built-in classes whose instances have a dispatch shape, and the type
//! objects that have one, by interned name (`DispatchShape::from_instance_class`,
//! `DispatchShape::from_type_symbol`).

use super::DispatchShape;
use crate::symbol::Symbol;
use rustc_hash::FxHashMap;
use std::sync::OnceLock;

/// The built-in classes whose `Instance`s have a shape that is not an opaque
/// one (those are found by their type name, `opaque.rs`).
const INSTANCE_CLASSES: [(&str, DispatchShape); 41] = [
    ("Date", DispatchShape::Date),
    ("Label", DispatchShape::Label),
    ("Collation", DispatchShape::Collation),
    ("Attribute", DispatchShape::Attribute),
    ("Variable", DispatchShape::Variable),
    ("Format", DispatchShape::Format),
    (
        "CompUnit::DependencySpecification",
        DispatchShape::CompUnitDepSpec,
    ),
    ("IO::Path::Parts", DispatchShape::IoPathParts),
    ("Backtrace", DispatchShape::Backtrace),
    ("Backtrace::Frame", DispatchShape::BacktraceFrame),
    ("DateTime", DispatchShape::DateTime),
    ("Instant", DispatchShape::Instant),
    ("Duration", DispatchShape::Duration),
    ("Match", DispatchShape::Match),
    ("IO::Path", DispatchShape::IoPath),
    ("IO::Path::Unix", DispatchShape::IoPath),
    ("IO::Path::Win32", DispatchShape::IoPath),
    ("IO::Path::Cygwin", DispatchShape::IoPath),
    ("IO::Path::QNX", DispatchShape::IoPath),
    ("IO::Handle", DispatchShape::IoHandle),
    ("Blob", DispatchShape::Blob),
    ("Buf", DispatchShape::Buf),
    ("utf8", DispatchShape::Blob),
    ("utf16", DispatchShape::Blob),
    ("utf32", DispatchShape::Blob),
    ("Blob[uint8]", DispatchShape::Blob),
    ("Blob[uint16]", DispatchShape::Blob),
    ("Blob[uint32]", DispatchShape::Blob),
    ("Blob[uint64]", DispatchShape::Blob),
    ("Blob[int8]", DispatchShape::Blob),
    ("Blob[int16]", DispatchShape::Blob),
    ("Blob[int32]", DispatchShape::Blob),
    ("Blob[int64]", DispatchShape::Blob),
    ("Buf[uint8]", DispatchShape::Buf),
    ("Buf[uint16]", DispatchShape::Buf),
    ("Buf[uint32]", DispatchShape::Buf),
    ("Buf[uint64]", DispatchShape::Buf),
    ("Buf[int8]", DispatchShape::Buf),
    ("Buf[int16]", DispatchShape::Buf),
    ("Buf[int32]", DispatchShape::Buf),
    ("Buf[int64]", DispatchShape::Buf),
];

/// The shapes whose values are `Instance`s of a built-in class, by interned
/// class name: one hash lookup, whatever the number of shapes.
pub(super) fn instance_shapes() -> &'static FxHashMap<Symbol, DispatchShape> {
    static SHAPES: OnceLock<FxHashMap<Symbol, DispatchShape>> = OnceLock::new();
    SHAPES.get_or_init(|| {
        let opaque = DispatchShape::ALL
            .into_iter()
            .filter(|shape| shape.is_opaque())
            .map(|shape| (shape.type_name(), shape));
        INSTANCE_CLASSES
            .into_iter()
            .chain(super::opaque::ALIASES)
            .chain(opaque)
            .map(|(name, shape)| (Symbol::intern(name), shape))
            .collect()
    })
}

/// Every shape by its type's interned name, for a type object
/// (`Package("Int")`).
pub(super) fn type_shapes() -> &'static FxHashMap<Symbol, DispatchShape> {
    static SHAPES: OnceLock<FxHashMap<Symbol, DispatchShape>> = OnceLock::new();
    SHAPES.get_or_init(|| {
        DispatchShape::ALL
            .into_iter()
            .map(|shape| (Symbol::intern(shape.type_name()), shape))
            .collect()
    })
}

/// The classes [`DispatchShape::from_instance_class`] last answered `None` for,
/// by `symbol id % len`, holding `id + 1` (`0` is an empty slot).
static SHAPELESS_CLASSES: [std::sync::atomic::AtomicU32; 256] =
    [const { std::sync::atomic::AtomicU32::new(0) }; 256];

/// Whether `class` was just answered shapeless. The answer is a pure function
/// of the symbol, and a user class is asked on every call of its methods and
/// never has a shape; this table of last answers makes the repeat ask one load
/// instead of a hash lookup and a prefix compare. A racing store only
/// overwrites a slot, which at worst sends a later ask down the slow path.
// Cost: O(1), one relaxed load.
#[inline]
pub(super) fn is_known_shapeless(class: Symbol) -> bool {
    let slot = &SHAPELESS_CLASSES[class.id() as usize % SHAPELESS_CLASSES.len()];
    slot.load(std::sync::atomic::Ordering::Relaxed) == class.id().wrapping_add(1)
}

/// Remember that `class` has no instance shape.
// Cost: O(1), one relaxed store.
#[inline]
pub(super) fn note_shapeless(class: Symbol) {
    let slot = &SHAPELESS_CLASSES[class.id() as usize % SHAPELESS_CLASSES.len()];
    slot.store(class.id().wrapping_add(1), std::sync::atomic::Ordering::Relaxed);
}
