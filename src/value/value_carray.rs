//! Native-backed `CArray[T]` element storage (ADR-0015 P3).
//!
//! A `CArray[T]` used to be a `Value::Array` of boxed elements, and every native
//! call **copied** it into a fresh C block on the way in and copied the block
//! back out on the way out. That copy is correct only when the callee writes
//! into the buffer *during* the call: `NativeHelpers::Blob`'s
//! `carray-from-blob(:managed)` builds a `CArray`, takes `BODY_OF(arr).storage`
//! and `memcpy`s into it later, with no call boundary in between and so no point
//! at which a copy could be synced back. The array also had no address to hand
//! out at all — its `.REPR` was `P6opaque` and its `.WHERE` was the identity
//! hash.
//!
//! So a `CArray[T]` whose elements are a **native numeric type** is now the same
//! thing a `Buf` is: a `Value::Instance` whose storage attribute holds a
//! [`BufData`](super::BufData) node of contiguous bytes. It shares that node,
//! its accessor layer ([`super::value_buf`]) and its element encode/decode
//! verbatim — this module adds only what is specific to `CArray`: the class-name
//! filter, construction, and the `CArray` REPR body.
//!
//! **Element types that are references keep the boxed representation.**
//! `CArray[Str]`, `CArray[Pointer]`, a nested `CArray[CArray[…]]` and a CStruct
//! element are addresses of other objects, and reading one back means
//! materialising the object it points at — which contiguous bytes alone cannot
//! do (MoarVM keeps a parallel `child` table for exactly this reason). Those stay
//! `Value::Array`, keep the per-call `char**` marshalling, and go on
//! under-reporting `.REPR` as `P6opaque`, which is the safe direction: ADR-0015
//! §2.1's ordering rule is that an honest `.REPR` is a promise that a body exists
//! behind `.WHERE`.

use super::{InstanceAttrs, Value};
use crate::symbol::Symbol;

/// Whether `class_name` is a `CArray` parameterised with a native numeric
/// element type — the arrays that get native storage.
///
/// The name arrives in several spellings (`CArray[uint8]`, and
/// package-qualified as `NativeCall::Types::CArray[uint8]` or
/// `Foo::CArray[uint8]` when the declaration was read inside a module), so the
/// base is matched on its last `::` component, the same "one class, several
/// spellings" problem `cstruct_layout::short_base_name` documents.
pub(crate) fn is_native_carray_class(class_name: &str) -> bool {
    carray_elem_type_name(class_name)
        .is_some_and(|elem| super::value_buf::native_elem_type(elem).is_some())
}

/// The element type name of a `CArray[T]` class name (`CArray[uint8]` →
/// `uint8`), or `None` if this is not a parameterised `CArray` at all.
pub(crate) fn carray_elem_type_name(class_name: &str) -> Option<&str> {
    let (base, rest) = class_name.split_once('[')?;
    let base = base.rsplit("::").next().unwrap_or(base);
    if base != "CArray" {
        return None;
    }
    rest.strip_suffix(']')
}

/// A fresh native-backed `CArray[T]` holding `elems`.
///
/// The elements are encoded at the class's element width by the same
/// [`super::value_buf`] path a `Buf` uses, so an out-of-range or non-`Int`
/// element is coerced exactly as it would be there.
pub(crate) fn make_carray(class_name: Symbol, elems: Vec<Value>) -> Value {
    super::value_buf::make_buf(class_name, elems)
}

/// The address of this array's synthesised `CArray` REPR body, which is what its
/// `.WHERE` answers.
///
/// `None` for an instance with no element storage, whose `.REPR` therefore stays
/// `P6opaque` — under-reporting is safe, claiming `CArray` without a body behind
/// it is not (ADR-0015 §2.1).
pub(crate) fn carray_repr_body_address(attrs: &InstanceAttrs) -> Option<usize> {
    super::value_buf::with_storage_node(attrs, |node| node.body.carray_address(node))
}

/// The address of this array's **elements** — the pointer a C function receives,
/// and what `nativecast(Pointer[T], $carray)` reinterprets.
///
/// Not the same address as [`carray_repr_body_address`]: that one is the
/// `CArrayB` block *describing* the storage, and this is the storage itself
/// (the block's `storage` word). Valid until the array is resized or dies —
/// ADR-0015 §2 contract 3, the same guarantee Rakudo's `VMArray` offers.
pub(crate) fn carray_storage_address(attrs: &InstanceAttrs) -> Option<usize> {
    super::value_buf::with_storage_node(attrs, |node| node.bytes.as_ptr() as usize)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn only_native_element_types_get_storage() {
        for name in [
            "CArray[uint8]",
            "CArray[int32]",
            "CArray[num64]",
            "NativeCall::Types::CArray[uint16]",
        ] {
            assert!(is_native_carray_class(name), "{name} should be native");
        }
        // Reference-typed elements, and things that are not a `CArray` at all.
        for name in [
            "CArray[Str]",
            "CArray[Pointer]",
            "CArray[MyStruct]",
            "CArray",
            "Buf[uint8]",
            "Array[Int]",
        ] {
            assert!(!is_native_carray_class(name), "{name} should not be native");
        }
    }

    #[test]
    fn elements_round_trip_through_the_node() {
        let a = make_carray(Symbol::intern("CArray[int32]"), vec![Value::int(-7)]);
        let attrs = match a.view() {
            super::super::ValueView::Instance { attributes, .. } => attributes,
            _ => panic!("expected an instance"),
        };
        assert_eq!(
            super::super::value_buf::buf_elems(&attrs),
            Some(vec![Value::int(-7)])
        );
        assert!(carray_repr_body_address(&attrs).is_some());
    }
}
