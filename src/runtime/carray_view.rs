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
//! MoarVM's unmanaged array has no length either: its `elems` is the highest
//! index touched plus one, which [`ELEMS_ATTR`] tracks.

use super::*;
use crate::value::ElemKind;

/// The element encoding of an unmanaged CArray: `width * 4 + kind`.
const VIEW_ATTR: &str = "__mutsu_carray_view";
/// How many elements of an unmanaged CArray have been touched.
const ELEMS_ATTR: &str = "__mutsu_carray_view_elems";

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
    attrs: crate::gc::Gc<crate::value::InstanceAttrs>,
    addr: usize,
    width: u8,
    kind: ElemKind,
}

impl CArrayView {
    /// The view `target` is, or `None` for any other value.
    // Cost: O(1).
    pub(crate) fn of(target: &Value) -> Option<Self> {
        let target = match target.view() {
            ValueView::Mixin(inner, _) => Value::clone(inner),
            _ => target.clone(),
        };
        let ValueView::Instance { attributes, .. } = target.view() else {
            return None;
        };
        let (code, addr) = {
            let map = attributes.as_map();
            let code = map.get(VIEW_ATTR)?.as_int()?;
            let addr = map.get("address")?.as_int()?;
            (code, addr)
        };
        Some(CArrayView {
            attrs: attributes.clone(),
            addr: usize::try_from(addr).ok()?,
            width: u8::try_from(code / 4).ok()?,
            kind: kind_of_code(code % 4)?,
        })
    }

    /// The elements touched so far (MoarVM's `elems` of an unmanaged array).
    // Cost: O(1).
    pub(crate) fn elems(&self) -> usize {
        self.attrs
            .as_map()
            .get(ELEMS_ATTR)
            .and_then(Value::as_int)
            .and_then(|n| usize::try_from(n).ok())
            .unwrap_or(0)
    }

    /// Record that element `idx` was touched.
    // Cost: O(1).
    fn touch(&self, idx: usize) {
        if idx >= self.elems() {
            self.attrs
                .insert(ELEMS_ATTR.to_string(), Value::int(idx as i64 + 1));
        }
    }

    /// Element `idx`, read from the C memory.
    // Cost: O(1).
    pub(crate) fn elem_at(&self, idx: usize) -> Option<Value> {
        self.touch(idx);
        // SAFETY: `nativecast` vouched that a C array of this element type
        // lives at `addr`; an index past its end is undefined behaviour in
        // Rakudo too (see the module docs).
        unsafe { crate::value::value_buf::read_raw_elem(self.addr, idx, self.width, self.kind) }
    }

    /// Store `v` as element `idx`, in the C memory.
    // Cost: O(1).
    pub(crate) fn bind(&self, idx: usize, v: &Value) -> Option<()> {
        self.touch(idx);
        // SAFETY: as in `elem_at`; the memory is the C array the cast named.
        unsafe { crate::value::value_buf::write_raw_elem(self.addr, idx, self.width, self.kind, v) }
    }
}

impl Interpreter {
    /// `nativecast($type, $source)` for a mixin type object `ty`
    /// (`Base.^mixin(R)`): an object of exactly that type over `addr` (see the
    /// module docs). `None` when `ty` is not a mixin of a CPointer- or
    /// CArray-REPR class, leaving the name-based cast in place. A NULL address
    /// answers the type object, as MoarVM does.
    // Cost: O(r), r = roles of the mixin.
    pub(crate) fn nativecast_mixin(
        &mut self,
        ty: &Value,
        addr: usize,
    ) -> Option<Result<Value, RuntimeError>> {
        let ValueView::Mixin(inner, mixins) = ty.view() else {
            return None;
        };
        let ValueView::Package(class) = inner.view() else {
            return None;
        };
        let is_carray = self.is_carray_repr_class(class.as_str());
        if !is_carray && !self.registry().cpointer_classes.contains(class.as_str()) {
            return None;
        }
        if addr == 0 {
            return Some(Ok(ty.clone()));
        }
        Some(self.nativecast_mixin_instance(ty, class, mixins, is_carray, addr))
    }

    // Cost: O(r), r = roles of the mixin.
    fn nativecast_mixin_instance(
        &mut self,
        ty: &Value,
        class: Symbol,
        mixins: &crate::value::MixinOverrides,
        is_carray: bool,
        addr: usize,
    ) -> Result<Value, RuntimeError> {
        let instance = self.create_instance(class);
        if let ValueView::Instance { attributes, .. } = instance.view() {
            attributes.insert("address".to_string(), Value::int(addr as i64));
            if is_carray {
                let elem = self.type_array_type(ty)?;
                let Some((width, kind)) = elem.and_then(|e| self.native_elem_encoding(&e)) else {
                    return Err(RuntimeError::new(format!(
                        "nativecast to {}: only a CArray of native numbers can view C memory",
                        crate::value::type_name::value_type_name(ty)
                    )));
                };
                attributes.insert(
                    VIEW_ATTR.to_string(),
                    Value::int(width as i64 * 4 + kind_code(kind)),
                );
            }
        }
        self.compose_mixin_type_roles_unbuilt(instance, mixins)
    }
}
