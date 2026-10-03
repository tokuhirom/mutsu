//! `is repr('CArray')`: the REPR selected by the declaration, not by the
//! class name (#11209, ADR-11203 §2.4).
//!
//! Upstream `NativeCall::Types` declares its C array as an ordinary class,
//! `our class CArray is repr('CArray') is array_type(Pointer)`, and builds
//! each typed array in `^parameterize` by mixing a role into it
//! (`array.^mixin(IntTypedCArray[int32])`, whose role says
//! `is array_type(TValue)`). Every element access is then nqp code on `self`:
//! `nqp::create(self)`, `nqp::bindpos_i`, `nqp::atposref_i`, `nqp::elems`.
//!
//! So what MoarVM's CArray REPR does at allocation is what this module does:
//! `nqp::create` of a class declared `is repr('CArray')` (or of a mixin of
//! one) gives the instance native element storage -- the same
//! [`BufData`](crate::value::BufData) node a `Buf` and the native provider's
//! `CArray[T]` keep their elements in (ADR-0015 P3a) -- typed by the type's
//! `.^array_type`. The element type is read from the declaration
//! (`is array_type`, a role's trait, a `native` type's `is nativesize` /
//! `is ctype` / `is unsigned`), never from the spelling of the class name.
//!
//! An array whose element type is a reference (`Pointer`, `Str`, a CStruct
//! class) is a `CArray` of addresses that read back as objects, which bytes
//! alone cannot hold (MoarVM keeps a parallel `child` table); its storage is
//! ADR-0015's P3c and still open, so such an instance gets no element storage
//! yet.

use super::*;
use crate::value::ElemKind;

/// Bytes of a C type given by MoarVM's negative `.^nativesize` code (see
/// `native_decl::ctype_nativesize`), on this platform.
// Cost: O(1).
fn ctype_width(code: i64, is_num: bool) -> Option<u8> {
    let width = if is_num {
        match code {
            -1 => 4,
            -2 => 8,
            // `long double` has no element encoding here.
            _ => return None,
        }
    } else {
        match code {
            -1 | -7 => 1,
            -2 => 2,
            -3 => 4,
            -4 => std::mem::size_of::<std::ffi::c_long>(),
            -5 => 8,
            -6 | -8 => std::mem::size_of::<usize>(),
            _ => return None,
        }
    };
    Some(width as u8)
}

/// The element encoding of a `native` declaration: its width, from
/// `is nativesize(bits)` or `is ctype<...>`, and its signedness.
// Cost: O(1).
fn native_decl_elem(decl: &super::native_decl::NativeDecl) -> Option<(u8, ElemKind)> {
    let is_num = decl.repr.as_deref() == Some("P6num");
    let width = match decl.nativesize? {
        bits if bits > 0 => u8::try_from(bits / 8).ok()?,
        code => ctype_width(code, is_num)?,
    };
    let kind = if is_num {
        ElemKind::Float
    } else if decl.unsigned {
        ElemKind::Uint
    } else {
        ElemKind::Int
    };
    let valid = match kind {
        ElemKind::Float => matches!(width, 4 | 8),
        _ => matches!(width, 1 | 2 | 4 | 8),
    };
    valid.then_some((width, kind))
}

impl Interpreter {
    /// Record a class declared `is repr('CArray')`.
    // Cost: O(n), n = chars of the name.
    pub(crate) fn register_carray_class(&mut self, name: &str) {
        self.registry_mut().carray_classes.insert(name.to_string());
    }

    /// Whether `class` was declared `is repr('CArray')`. A REPR is not
    /// inherited: `class Sub is CA { }` is `P6opaque` in rakudo, while a
    /// mixin of `CA` (what `^parameterize` builds) keeps `CA` as its class.
    // Cost: O(n), n = chars of the name.
    pub(crate) fn is_carray_repr_class(&self, class: &str) -> bool {
        self.registry().carray_classes.contains(class)
    }

    /// How a native element of type `elem` is stored: bytes per element and
    /// how it reads back. `None` for anything that is not a native numeric
    /// type -- a reference element (`Pointer`, `Str`, a CStruct class).
    // Cost: O(n), n = chars of the type name.
    pub(crate) fn native_elem_encoding(&self, elem: &Value) -> Option<(u8, ElemKind)> {
        let ValueView::Package(name) = elem.view() else {
            return None;
        };
        // A `native` declaration (`NativeCall::Types`' `long`, `size_t`, ...)
        // says what it is; the core's own native types are in the table.
        match self.native_decl(name.as_str()) {
            Some(decl) => native_decl_elem(&decl),
            None => crate::value::value_buf::native_elem_type(name.as_str()),
        }
    }

    /// `nqp::create` of `class`, a CArray-REPR class, reached as type `ty`
    /// (the class itself, or a mixin of it whose roles may say what the
    /// elements are). The instance gets empty element storage of the type's
    /// `.^array_type` when that is a native numeric type.
    // Cost: O(a + r), a = attributes of the class, r = roles of a mixin `ty`.
    pub(crate) fn create_carray_instance(
        &mut self,
        class: Symbol,
        ty: &Value,
    ) -> Result<Value, RuntimeError> {
        let instance = self.create_instance(class);
        let encoding = match self.type_array_type(ty)? {
            Some(elem) => self.native_elem_encoding(&elem),
            None => None,
        };
        if let Some((width, kind)) = encoding
            && let ValueView::Instance { attributes, .. } = instance.view()
        {
            crate::value::value_buf::install_empty_storage(&attributes, width, kind);
        }
        Ok(instance)
    }

    /// `nqp::create` of a mixin type object (`Base.^mixin(R)`, a
    /// `Mixin(Package, ..)`) or of an object with roles mixed in: an instance
    /// of the base class -- with CArray storage when the base is a
    /// CArray-REPR class -- carrying the same roles, so `nqp::create(self)`
    /// inside a role method keeps the role's methods. `None` when `ty` is not
    /// a mixin of a class.
    // Cost: O(a + r), a = attributes of the base class, r = roles mixed in.
    pub(crate) fn nqp_create_mixin(&mut self, ty: &Value) -> Option<Result<Value, RuntimeError>> {
        let ValueView::Mixin(inner, mixins) = ty.view() else {
            return None;
        };
        let class = match inner.view() {
            ValueView::Package(class) => class,
            ValueView::Instance { class_name, .. } => class_name,
            _ => return None,
        };
        let base = if self.is_carray_repr_class(class.as_str()) {
            self.create_carray_instance(class, ty)
        } else {
            self.nqp_create(Value::package(class))
        };
        Some(base.and_then(|base| self.compose_mixin_type_roles_unbuilt(base, &mixins)))
    }
}

#[cfg(test)]
mod tests {
    use super::super::native_decl::NativeDecl;
    use super::*;

    fn decl(repr: &str, nativesize: i64, unsigned: bool) -> NativeDecl {
        NativeDecl {
            repr: Some(repr.to_string()),
            nativesize: Some(nativesize),
            unsigned,
        }
    }

    #[test]
    fn native_decls_give_their_element_encoding() {
        assert_eq!(
            native_decl_elem(&decl("P6int", 16, false)),
            Some((2, ElemKind::Int))
        );
        assert_eq!(
            native_decl_elem(&decl("P6int", -6, true)),
            Some((std::mem::size_of::<usize>() as u8, ElemKind::Uint))
        );
        assert_eq!(
            native_decl_elem(&decl("P6int", -7, false)),
            Some((1, ElemKind::Int))
        );
        assert_eq!(
            native_decl_elem(&decl("P6num", -2, false)),
            Some((8, ElemKind::Float))
        );
        // `long double` and odd widths have no encoding.
        assert_eq!(native_decl_elem(&decl("P6num", -3, false)), None);
        assert_eq!(native_decl_elem(&decl("P6int", 24, false)), None);
        assert_eq!(
            native_decl_elem(&NativeDecl {
                repr: Some("P6int".to_string()),
                nativesize: None,
                unsigned: false,
            }),
            None
        );
    }
}
