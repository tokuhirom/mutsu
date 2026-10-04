//! Reference-element `is repr('CArray')` storage: ADR-0015 P3c (#11209).
//!
//! A `CArray` whose element type is a **reference** -- `Str`, `Pointer`, a
//! CStruct/CUnion/CPointer class, a nested `CArray` -- is, in C, an array of
//! addresses (`char**`, `void**`, `struct s**`). MoarVM's CArray REPR keeps two
//! parallel tables for it: the addresses C sees (`storage`) and the Raku object
//! each slot was bound to or last read as (`child_objs`). This module is that
//! REPR for the classes [`carray_repr`] allocates:
//!
//! - **The address table** is the same [`BufData`](crate::value::BufData) node a
//!   native numeric `CArray` keeps its elements in, at pointer width. So a native
//!   call is handed the table itself (`marshal_carray_arg` passes any storage
//!   node through), nothing is copied in or out, and a callee that writes an
//!   address into a slot (`strtol`'s `endptr`) is seen by the next read.
//! - **The child table** holds, per slot, the object the slot answers and what
//!   keeps its address valid: for a `Str` bound from Raku, the NUL-terminated
//!   copy C points at (a [`byte_block`](crate::value::value_buf::byte_block),
//!   owned here so it lives exactly as long as the array references it); for
//!   anything else, the address the object stood for.
//!
//! A read answers the cached child only while the slot still holds that
//! address. When C has rewritten the slot, the child is stale and the object
//! is materialised again from the new address -- what MoarVM's
//! `nativecallrefresh` does after each call, done at read time so that no call
//! path can miss it. A NULL slot, or one past the end, reads as the element
//! type object.

use super::*;
use crate::symbol::Symbol;
use crate::value::InstanceAttrs;
use crate::value::value_buf;
use std::sync::LazyLock;

/// The attribute holding the per-slot objects (`NIL` where none is cached).
// Cost: O(1).
fn children_key() -> Symbol {
    static KEY: LazyLock<Symbol> = LazyLock::new(|| Symbol::intern("carray-children"));
    *KEY
}

/// The attribute holding, per slot, what keeps that slot's address valid: a
/// byte block this array owns, or the address as an `Int`.
// Cost: O(1).
fn keep_key() -> Symbol {
    static KEY: LazyLock<Symbol> = LazyLock::new(|| Symbol::intern("carray-keep"));
    *KEY
}

/// The attribute holding the element type object.
// Cost: O(1).
fn of_key() -> Symbol {
    static KEY: LazyLock<Symbol> = LazyLock::new(|| Symbol::intern("carray-of"));
    *KEY
}

/// Give `attrs` empty reference-element storage of element type `elem`.
// Cost: O(1).
pub(crate) fn install_ref_storage(attrs: &InstanceAttrs, elem: Value) {
    let width = std::mem::size_of::<usize>() as u8;
    value_buf::install_empty_storage(attrs, width, crate::value::ElemKind::Uint);
    attrs.insert(children_key(), Value::array(Vec::new()));
    attrs.insert(keep_key(), Value::array(Vec::new()));
    attrs.insert(of_key(), elem);
}

/// The attribute marking reference-element storage as a **view** of C memory:
/// an unmanaged CArray `nativecast` made over an address (see
/// `carray_view`). Its slots are the pointer-width words at the object's
/// `address`, not an owned address table.
// Cost: O(1).
fn view_key() -> Symbol {
    static KEY: LazyLock<Symbol> = LazyLock::new(|| Symbol::intern("carray-view"));
    *KEY
}

/// Make `attrs` a reference-element view of element type `elem` over the C
/// array at `addr` (the instance's `address`).
// Cost: O(1).
pub(crate) fn install_ref_view(attrs: &InstanceAttrs, elem: Value, addr: usize) {
    attrs.insert(Symbol::intern("address"), Value::int(addr as i64));
    attrs.insert(view_key(), Value::TRUE);
    attrs.insert(children_key(), Value::array(Vec::new()));
    attrs.insert(keep_key(), Value::array(Vec::new()));
    attrs.insert(of_key(), elem);
}

/// The base address of a reference-element view, `None` for owned storage.
// Cost: O(1).
fn view_base(attrs: &InstanceAttrs) -> Option<usize> {
    let map = attrs.as_map();
    map.get(view_key())?;
    usize::try_from(map.get("address")?.as_int()?).ok()
}

/// The address slot `idx` of a view holds, read from the C memory.
// Cost: O(1).
fn view_slot(base: usize, idx: usize) -> usize {
    // SAFETY: `nativecast` vouched that an array of pointers lives at `base`;
    // an index past its end is undefined behaviour in Rakudo too, which is
    // the trust every NativeCall cast gets (`carray_view` module docs).
    unsafe { std::ptr::read_unaligned((base as *const usize).wrapping_add(idx)) }
}

/// Store `addr` in slot `idx` of a view, in the C memory.
// Cost: O(1).
fn set_view_slot(base: usize, idx: usize, addr: usize) {
    // SAFETY: as in `view_slot`; the memory is the C array the cast named.
    unsafe { std::ptr::write_unaligned((base as *mut usize).wrapping_add(idx), addr) }
}

/// The attributes of `target` when it is a reference-element CArray.
// Cost: O(1).
pub(crate) fn ref_attrs(target: &Value) -> Option<crate::gc::Gc<InstanceAttrs>> {
    match target.view() {
        ValueView::Instance { attributes, .. } if attributes.as_map().contains_key(of_key()) => {
            Some((*attributes).clone())
        }
        ValueView::Mixin(inner, _) => ref_attrs(inner),
        ValueView::Scalar(inner) => ref_attrs(inner),
        _ => None,
    }
}

/// Slot `idx` of the table under `key`, `NIL` when the table is shorter.
// Cost: O(1).
fn table_at(attrs: &InstanceAttrs, key: Symbol, idx: usize) -> Value {
    let map = attrs.as_map();
    match map.get(key).map(Value::view) {
        Some(ValueView::Array(items, _)) => items.get(idx).cloned().unwrap_or(Value::NIL),
        _ => Value::NIL,
    }
}

/// Set slot `idx` of the table under `key` to `val`, growing it with `NIL`.
// Cost: O(1) amortized when the table is unshared; O(n) when it is forked, n = slots.
fn table_set(attrs: &InstanceAttrs, key: Symbol, idx: usize, val: Value) {
    let put = |items: &mut Vec<Value>| {
        if items.len() <= idx {
            items.resize(idx + 1, Value::NIL);
        }
        items[idx] = val;
    };
    let forked = {
        let map = attrs.as_map();
        match map.get(key).map(Value::view) {
            Some(ValueView::Array(items, _)) if items.strong_count() == 1 => {
                // SAFETY: audited aliased in-place container write (see
                // `value::aliased_mut`), the same one
                // `value_buf::with_buf_storage_mut` performs on the address
                // table beside it. The table is unshared (no Raku value can
                // hold it: it is reachable only through this attribute), the
                // edit is a pure slot store that never re-enters the
                // interpreter, and the read guard covers only the attribute
                // map, which is not what is being mutated.
                let data = unsafe { crate::value::gc_contents_mut(&items) };
                put(data.items_mut());
                return;
            }
            Some(ValueView::Array(items, _)) => items.to_vec(),
            _ => Vec::new(),
        }
    };
    let mut items = forked;
    put(&mut items);
    attrs.insert(key, Value::array(items));
}

/// The address a `keep` entry stands for.
// Cost: O(1).
fn keep_address(keep: &Value) -> Option<usize> {
    value_buf::byte_block_address(keep).or_else(|| keep.as_int().map(|i| i as usize))
}

/// Whether `elem` is the `Str` type: its slots are `char*` read as strings.
// Cost: O(1).
fn is_str_type(elem: &Value) -> bool {
    matches!(elem.view(), ValueView::Package(name) if name == "Str")
}

impl Interpreter {
    /// `nqp::atpos` on a reference-element CArray: the object slot `idx`
    /// holds. See the module docs for when a cached child answers.
    // Cost: O(1) for a cached slot; O(n) to materialise a `Str`, n = bytes.
    pub(crate) fn carray_ref_at(
        &mut self,
        attrs: &InstanceAttrs,
        idx: i64,
    ) -> Result<Value, RuntimeError> {
        let elem = attrs.as_map().get(of_key()).cloned().unwrap_or(Value::NIL);
        let Ok(idx) = usize::try_from(idx) else {
            return Err(RuntimeError::new(format!(
                "Cannot access negative index {idx} of a CArray"
            )));
        };
        let addr = match view_base(attrs) {
            Some(base) => view_slot(base, idx),
            None => match value_buf::buf_elem_at(attrs, idx) {
                Some(slot) => crate::runtime::to_int(&slot) as usize,
                None => return Ok(elem),
            },
        };
        let child = table_at(attrs, children_key(), idx);
        if !child.is_nil() && keep_address(&table_at(attrs, keep_key(), idx)) == Some(addr) {
            return Ok(child);
        }
        if addr == 0 {
            return Ok(elem);
        }
        let obj = if is_str_type(&elem) {
            // SAFETY: the array is declared to hold `char*`; C (or this
            // module) stored a NUL-terminated string's address in the slot.
            // That is the trust every NativeCall declaration gets.
            let cstr = unsafe { std::ffi::CStr::from_ptr(addr as *const std::ffi::c_char) };
            Value::str(cstr.to_string_lossy().into_owned())
        } else {
            let name = match elem.view() {
                ValueView::Package(name) => name.resolve(),
                _ => crate::runtime::value_type_name(&elem).to_string(),
            };
            self.nativecast_address(&name, addr)
        };
        table_set(attrs, children_key(), idx, obj.clone());
        table_set(attrs, keep_key(), idx, Value::int(addr as i64));
        Ok(obj)
    }

    /// `nqp::bindpos` on a reference-element CArray: store `val`'s address in
    /// slot `idx` (growing the array with NULL slots) and remember `val` as
    /// the slot's object. An undefined `val` stores NULL.
    // Cost: O(1) amortized; O(n) for a `Str`, n = bytes copied.
    pub(crate) fn carray_ref_bind(
        &mut self,
        attrs: &InstanceAttrs,
        idx: i64,
        val: Value,
    ) -> Result<Value, RuntimeError> {
        let Ok(idx) = usize::try_from(idx) else {
            return Err(RuntimeError::new(format!(
                "Cannot bind to negative index {idx} of a CArray"
            )));
        };
        let val = crate::runtime::types::unwrap_varref_value(val);
        let (addr, keep, child) = if !crate::runtime::types::value_is_defined(&val) {
            (0, Value::NIL, Value::NIL)
        } else if let ValueView::Str(s) = val.view() {
            let mut bytes = s.as_str().as_bytes().to_vec();
            if bytes.contains(&0) {
                return Err(RuntimeError::new(
                    "Cannot store a Str containing a NUL byte in a CArray",
                ));
            }
            bytes.push(0);
            let block = value_buf::byte_block(bytes);
            let addr = value_buf::byte_block_address(&block).unwrap_or(0);
            (addr, block, val.clone())
        } else {
            let addr = self.carray_element_address(&val);
            (addr, Value::int(addr as i64), val.clone())
        };
        match view_base(attrs) {
            Some(base) => set_view_slot(base, idx, addr),
            None => {
                value_buf::set_buf_elem(attrs, idx, &Value::int(addr as i64));
            }
        }
        table_set(attrs, keep_key(), idx, keep);
        table_set(attrs, children_key(), idx, child);
        Ok(val)
    }

    /// The C address a reference element stands for: a CArray-REPR array's
    /// own element storage, otherwise whatever address the object carries
    /// (`Pointer`, a CStruct handle, a native `CArray[T]`).
    // Cost: O(1).
    pub(crate) fn carray_element_address(&self, v: &Value) -> usize {
        let v = &crate::runtime::types::unwrap_varref_value(v.clone()).deref_container();
        let class = match v.view() {
            ValueView::Instance { class_name, .. } => Some(class_name),
            ValueView::Mixin(inner, _) => match inner.view() {
                ValueView::Instance { class_name, .. } => Some(class_name),
                _ => None,
            },
            _ => None,
        };
        if let Some(class) = class
            && self.is_carray_repr_class(class.as_str())
            && let Some((_, attrs)) = value_buf::buf_target(v)
        {
            return crate::value::value_carray::carray_storage_address(&attrs).unwrap_or(0);
        }
        crate::runtime::nativecall::value_c_address(v)
    }
}
