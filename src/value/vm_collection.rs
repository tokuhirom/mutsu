//! Raw nqp VM collections — `VMArray` / `VMHash` (ADR-12666).
//!
//! `nqp::list` / `nqp::hash` make the raw VM objects Rakudo calls `BOOTArray`
//! / `BOOTHash`; `nqp::hllize` turns them into a high-level `List` / `Hash`
//! that is a *different object sharing the same element storage*. mutsu keeps
//! the high-level/raw distinction in the 8-byte word's kind tag, over the same
//! `Gc<ArrayData>` / `Gc<HashData>` node:
//!
//! - a raw value views as `ValueView::Array(_, ArrayKind::List)` /
//!   `ValueView::Hash`, so every native builtin receives it the way Rakudo's
//!   argument binder would hand it over (hllized);
//! - the places where Rakudo tells the two apart (`islist`/`ishash`, `.WHAT`,
//!   method dispatch, `eqaddr`, binding a Raku parameter) ask
//!   [`Value::is_vm_array`] / [`Value::is_vm_hash`].

use super::*;

impl Value {
    /// A fresh raw `VMArray` holding `items` (`nqp::list`).
    // Cost: O(1) beyond moving `items`.
    pub(crate) fn vm_array(items: Vec<Value>) -> Value {
        Value::from_repr(ValueRepr::VmArray(Gc::new(ArrayData::new(items))))
    }

    /// A raw `VMArray` over an existing storage node.
    // Cost: O(1).
    pub(crate) fn vm_array_from_gc(data: Gc<ArrayData>) -> Value {
        Value::from_repr(ValueRepr::VmArray(data))
    }

    /// A fresh raw `VMHash` holding `map` (`nqp::hash`).
    // Cost: O(1) beyond moving `map`.
    pub(crate) fn vm_hash(map: impl Into<HashData>) -> Value {
        Value::from_repr(ValueRepr::VmHash(Gc::new(map.into())))
    }

    /// Whether this is a raw `VMArray` (of any element kind). A tag probe.
    #[inline]
    pub(crate) fn is_vm_array(&self) -> bool {
        self.0.is_vm_array()
    }

    /// Whether this is a raw `VMHash`. A tag probe.
    #[inline]
    pub(crate) fn is_vm_hash(&self) -> bool {
        self.0.is_vm_hash()
    }

    /// Whether this is a raw `VMArray` or `VMHash`. A tag probe.
    #[inline]
    pub(crate) fn is_vm_collection(&self) -> bool {
        self.0.is_vm_array() || self.0.is_vm_hash()
    }

    /// The `BOOT*` type a raw VM collection is an instance of, by its element
    /// kind; `None` for every other value.
    // Cost: O(1).
    pub(crate) fn vm_boot_type_name(&self) -> Option<&'static str> {
        if self.is_vm_hash() {
            return Some("BOOTHash");
        }
        if !self.is_vm_array() {
            return None;
        }
        let ValueView::Array(data, _) = self.view() else {
            return None;
        };
        Some(match data.nqp_elem {
            NqpElemKind::Object => "BOOTArray",
            NqpElemKind::Int => "BOOTIntArray",
            NqpElemKind::Num => "BOOTNumArray",
            NqpElemKind::Str => "BOOTStrArray",
        })
    }

    /// `nqp::hllize` of a raw collection: the high-level `List` / `Hash` view
    /// of the same storage node (a different object, shared elements). A
    /// native-element `VMArray` has no high-level mapping and stays raw, as in
    /// Rakudo; every other value is returned unchanged.
    // Cost: O(1).
    pub(crate) fn hllize_vm_collection(self) -> Value {
        match self.into_repr() {
            ValueRepr::VmArray(data) if data.nqp_elem == NqpElemKind::Object => {
                Value::Array(data, ArrayKind::List)
            }
            ValueRepr::VmHash(data) => Value::Hash(data, false),
            other => Value::from_repr(other),
        }
    }

    /// Whether `self` and `other` are the same raw-or-high-level object over
    /// the same storage node: `Some(true/false)` when both are collections,
    /// `None` when either is not (the caller falls back to its general rule).
    // Cost: O(1).
    pub(crate) fn same_collection_object(&self, other: &Value) -> Option<bool> {
        let node = |v: &Value| -> Option<usize> {
            match v.view() {
                ValueView::Array(data, _) => Some(Gc::as_ptr(&data) as usize),
                ValueView::Hash(data) => Some(Gc::as_ptr(&data) as usize),
                _ => None,
            }
        };
        if !self.is_vm_collection() && !other.is_vm_collection() {
            return None;
        }
        let (a, b) = (node(self)?, node(other)?);
        Some(a == b && self.is_vm_collection() == other.is_vm_collection())
    }
}
