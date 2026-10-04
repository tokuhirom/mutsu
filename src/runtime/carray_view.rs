//! `nativecast` to a mixin type: upstream NativeCall's `Pointer[T]` and
//! `CArray[T]` (#11209).
//!
//! Upstream builds those types in `^parameterize` as
//! `Pointer.^mixin(TypedPointer[T])` / `CArray.^mixin(IntTypedCArray[T])`,
//! so `nativecast(CArray[int32], $ptr)` hands `nqp::nativecallcast` a mixin
//! type object, not a class. MoarVM answers an object of exactly that type
//! over the address:
//!
//! - a CPointer-REPR base gives a pointer object holding the address, with
//!   the type's roles (`.of`, `.deref` of `TypedPointer`);
//! - a CArray-REPR base gives an **unmanaged** CArray: its elements are the C
//!   memory at the address, read and written in place, and nothing is freed
//!   when it goes away.
//!
//! The unmanaged CArray keeps the address under the `address` attribute (what
//! [`value_c_address`](crate::runtime::nativecall::value_c_address) passes
//! back to C) and the element encoding of the type's `.^array_type` under
//! [`VIEW_ATTR`]. The `nqp::` element ops reach it through [`CArrayView`].
//! It has no length: like MoarVM's, `nqp::elems` on it dies, and a negative
//! index has no end to count from. A reference-element CArray (`CArray[Str]`,
//! `CArray[Pointer]`) views an array of addresses instead, through
//! `carray_ref`'s slot logic (`install_ref_view`).

use super::*;
use crate::value::ElemKind;

/// The element encoding of an unmanaged CArray: `width * 4 + kind`.
const VIEW_ATTR: &str = "__mutsu_carray_view";

// Cost: O(1).
fn kind_code(kind: ElemKind) -> i64 {
    match kind {
        ElemKind::Int => 0,
        ElemKind::Uint => 1,
        ElemKind::Float => 2,
    }
}

// Cost: O(1).
fn kind_of_code(code: i64) -> Option<ElemKind> {
    Some(match code {
        0 => ElemKind::Int,
        1 => ElemKind::Uint,
        2 => ElemKind::Float,
        _ => return None,
    })
}

/// An unmanaged CArray: C memory at `addr`, `width`-byte `kind` elements.
pub(crate) struct CArrayView {
    addr: usize,
    width: u8,
    kind: ElemKind,
}

impl CArrayView {
    /// The view `target` is, or `None` for any other value.
    // Cost: O(1).
    pub(crate) fn of(target: &Value) -> Option<Self> {
        let attributes = match target.view() {
            ValueView::Instance { attributes, .. } => attributes,
            ValueView::Mixin(inner, _) => match inner.view() {
                ValueView::Instance { attributes, .. } => attributes,
                _ => return None,
            },
            _ => return None,
        };
        let (code, addr) = {
            let map = attributes.as_map();
            let code = map.get(VIEW_ATTR)?.as_int()?;
            let addr = map.get("address")?.as_int()?;
            (code, addr)
        };
        Some(CArrayView {
            addr: usize::try_from(addr).ok()?,
            width: u8::try_from(code / 4).ok()?,
            kind: kind_of_code(code % 4)?,
        })
    }

    /// Element `idx`, read from the C memory.
    // Cost: O(1).
    pub(crate) fn elem_at(&self, idx: usize) -> Option<Value> {
        // SAFETY: `nativecast` vouched that a C array of this element type
        // lives at `addr`; an index past its end is undefined behaviour in
        // Rakudo too (see the module docs).
        unsafe { crate::value::value_buf::read_raw_elem(self.addr, idx, self.width, self.kind) }
    }

    /// Store `v` as element `idx`, in the C memory.
    // Cost: O(1).
    pub(crate) fn bind(&self, idx: usize, v: &Value) -> Option<()> {
        // SAFETY: as in `elem_at`; the memory is the C array the cast named.
        unsafe { crate::value::value_buf::write_raw_elem(self.addr, idx, self.width, self.kind, v) }
    }
}

impl Interpreter {
    /// The object of type `ty` that lives at C address `addr`, as MoarVM's
    /// CPointer and CArray REPRs box one: an instance of a CPointer-REPR class
    /// holding the address, or of a mixin of a CPointer- or CArray-REPR class
    /// (upstream's `Pointer[T]` / `CArray[T]`, see the module docs) carrying
    /// its roles. `None` when `ty` is none of those, leaving the caller's
    /// name-based handling in place. NULL is the caller's to decide: a
    /// `Pointer.new(0)` is a defined object, a NULL return is the type object.
    // Cost: O(r), r = roles of a mixin `ty`; O(1) for a class.
    pub(crate) fn native_object_of_type(
        &mut self,
        ty: &Value,
        addr: usize,
    ) -> Option<Result<Value, RuntimeError>> {
        let (class, mixins) = match ty.view() {
            ValueView::Package(class) => (class, None),
            ValueView::Mixin(inner, mixins) => match inner.view() {
                ValueView::Package(class) => (class, Some(mixins)),
                _ => return None,
            },
            _ => return None,
        };
        let is_carray = mixins.is_some() && self.is_carray_repr_class(class.as_str());
        if !is_carray && !self.registry().cpointer_classes.contains(class.as_str()) {
            return None;
        }
        let instance = self.create_instance(class);
        if let ValueView::Instance { attributes, .. } = instance.view() {
            attributes.insert("address".to_string(), Value::int(addr as i64));
            if is_carray {
                let elem = match self.type_array_type(ty) {
                    Ok(elem) => elem,
                    Err(e) => return Some(Err(e)),
                };
                let Some(elem) = elem else {
                    return Some(Err(RuntimeError::new(
                        "nativecast: a CArray type without an element type cannot view C memory",
                    )));
                };
                let Some((width, kind)) = self.native_elem_encoding(&elem) else {
                    // A reference element (`CArray[Str]`, `CArray[Pointer]`,
                    // a CStruct class): the C memory is an array of
                    // addresses, read and written through `carray_ref`'s
                    // slot logic.
                    crate::runtime::carray_ref::install_ref_view(&attributes, elem, addr);
                    return Some(match mixins {
                        Some(mixins) => self.compose_mixin_type_roles_unbuilt(instance, mixins),
                        None => Ok(instance),
                    });
                };
                attributes.insert(
                    VIEW_ATTR.to_string(),
                    Value::int(width as i64 * 4 + kind_code(kind)),
                );
            }
        }
        Some(match mixins {
            Some(mixins) => self.compose_mixin_type_roles_unbuilt(instance, mixins),
            None => Ok(instance),
        })
    }
}
