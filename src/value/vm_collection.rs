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

    /// A raw `VMHash` over an existing storage node.
    // Cost: O(1).
    pub(crate) fn vm_hash_from_gc(data: Gc<HashData>) -> Value {
        Value::from_repr(ValueRepr::VmHash(data))
    }

    /// The storage view of a collection's own node — what Rakudo's
    /// `$!reified` / `$!storage` attribute holds (ADR-12666 §3.6): an
    /// `IterationBuffer` for an `Array`, a raw `VMArray` for a `List`, a raw
    /// `VMHash` for a `Map`/`Hash`. `None` for a value that is not a list or
    /// hash.
    // Cost: O(1).
    pub(crate) fn raw_storage_view(&self) -> Option<Value> {
        match self.view() {
            ValueView::Array(data, kind)
                if !self.is_vm_collection()
                    && !matches!(kind, ArrayKind::List | ArrayKind::ItemList) =>
            {
                Some(Value::iteration_buffer_view((*data).clone()))
            }
            ValueView::Array(data, _) => Some(Value::vm_array_from_gc((*data).clone())),
            ValueView::Hash(data) => Some(Value::vm_hash_from_gc((*data).clone())),
            _ => None,
        }
    }

    /// Whether this is a raw `VMArray` (of any element kind). A tag probe.
    #[inline]
    pub(crate) fn is_vm_array(&self) -> bool {
        self.0.is_vm_array()
    }

    /// An `IterationBuffer` over an existing storage node.
    // Cost: O(1).
    pub(crate) fn iteration_buffer_view(data: Gc<ArrayData>) -> Value {
        Value::from_repr(ValueRepr::VmIterBuf(data))
    }

    /// Whether this is an `IterationBuffer` storage view. A tag probe.
    #[inline]
    pub(crate) fn is_iteration_buffer_view(&self) -> bool {
        self.0.is_iteration_buffer_view()
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
    /// Rakudo; every other value is returned unchanged -- the same object,
    /// never re-encoded, since re-packing an `Arc`-backed value (a RakuAST
    /// node, say) allocates a fresh box and breaks its `===` identity.
    // Cost: O(1).
    pub(crate) fn hllize_vm_collection(self) -> Value {
        if !self.is_vm_collection() {
            return self;
        }
        match self.into_repr() {
            ValueRepr::VmArray(data) if data.nqp_elem == NqpElemKind::Object => {
                Value::Array(data, ArrayKind::List)
            }
            ValueRepr::VmHash(data) => Value::Hash(data, false),
            other => Value::from_repr(other),
        }
    }

    /// Which object over a storage node this word is: 0 the high-level
    /// collection, 1 a raw `VMArray`/`VMHash`, 2 an `IterationBuffer` view.
    // Cost: O(1).
    fn storage_view_class(&self) -> u8 {
        if self.is_vm_collection() {
            1
        } else if self.is_iteration_buffer_view() {
            2
        } else {
            0
        }
    }

    /// Whether `self` and `other` are the same object over the same storage
    /// node, telling the high-level collection, its raw `VMArray`/`VMHash`
    /// and its `IterationBuffer` apart: `Some(..)` when either is a raw or
    /// buffer view, `None` otherwise (the caller falls back to its general
    /// rule).
    // Cost: O(1).
    pub(crate) fn same_collection_object(&self, other: &Value) -> Option<bool> {
        let node = |v: &Value| -> Option<usize> {
            match v.view() {
                ValueView::Array(data, _) => Some(Gc::as_ptr(&data) as usize),
                ValueView::Hash(data) => Some(Gc::as_ptr(&data) as usize),
                _ => None,
            }
        };
        let (ca, cb) = (self.storage_view_class(), other.storage_view_class());
        if ca == 0 && cb == 0 {
            return None;
        }
        let (Some(a), Some(b)) = (node(self), node(other)) else {
            return Some(false);
        };
        Some(a == b && ca == cb)
    }
}
