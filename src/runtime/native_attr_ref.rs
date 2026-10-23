//! `IntAttrRef` / `UIntAttrRef` / `NumAttrRef` / `StrAttrRef`: the lvalue an
//! `is rw` accessor of a native-typed `CStruct` field answers (#14744).
//!
//! A CStruct handle keeps no Raku attributes: its fields live in the C struct
//! its `address` points at, so there is no slot to promote to a shared cell.
//! Rakudo answers with a native attribute reference whose read decodes the
//! field and whose write encodes into it, so `$!content.top = 5e0` and an
//! `is rw` routine returning `$rect.top` reach the struct's memory.
//!
//! mutsu's container with code on read and write is `Proxy`, so the reference
//! is a `Proxy` subclass named after Rakudo's type, whose FETCH and STORE are
//! the two routines below and whose attributes are the handle and the field
//! name (the same shape as the native positional references of
//! `native_pos_ref.rs`). Both read and write the live struct on every access:
//! nothing is copied, so the reference never goes stale.

use super::cstruct_layout::FieldType;
use super::*;
use crate::symbol::Symbol;

/// The routine every native attribute reference FETCHes through.
const FETCH: &str = "__mutsu_native_attrref_fetch";
/// The routine every native attribute reference STOREs through.
const STORE: &str = "__mutsu_native_attrref_store";

/// Whether `name` is one of the two routines a native attribute reference calls.
// Cost: O(1).
pub(crate) fn is_native_attr_ref_routine(name: &str) -> bool {
    name == FETCH || name == STORE
}

/// Rakudo's reference type for a field of C type `ty`, `None` for a field that
/// is no native scalar (a pointer, an inline struct), which reads as a handle.
// Cost: O(1).
fn ref_type_name(ty: FieldType) -> Option<&'static str> {
    Some(match ty {
        FieldType::I8 | FieldType::I16 | FieldType::I32 | FieldType::I64 => "IntAttrRef",
        FieldType::U8 | FieldType::U16 | FieldType::U32 | FieldType::U64 => "UIntAttrRef",
        FieldType::F32 | FieldType::F64 => "NumAttrRef",
        FieldType::Str => "StrAttrRef",
        FieldType::Pointer | FieldType::Embedded { .. } => return None,
    })
}

/// A reference to field `field` of the struct `handle` points at, of Rakudo
/// reference type `type_name`.
// Cost: O(1).
fn native_attr_ref(handle: Value, field: &str, type_name: &'static str) -> Value {
    let mut attrs = crate::value::AttrMap::new();
    attrs.insert("target".to_string(), handle);
    attrs.insert("field".to_string(), Value::str(field.to_string()));
    let routine =
        |name: &str| Value::routine_parts(Symbol::intern(""), Symbol::intern(name), false);
    Value::proxy_parts(
        routine(FETCH),
        routine(STORE),
        Some((
            Symbol::intern(type_name),
            crate::value::proxy_subclass_attrs(Symbol::intern(type_name), attrs),
        )),
        false,
    )
}

/// The struct handle and field name a native attribute reference addresses.
// Cost: O(1).
fn parts(proxy: &Value) -> Result<(Value, String), RuntimeError> {
    if let ValueView::Proxy {
        subclass: Some((_, attrs)),
        ..
    } = proxy.view()
    {
        let attrs = attrs.as_map();
        if let (Some(target), Some(field)) = (attrs.get("target"), attrs.get("field")) {
            return Ok((target.clone(), field.to_string_value()));
        }
    }
    Err(RuntimeError::new(
        "a native attribute reference was invoked without its field",
    ))
}

impl Interpreter {
    /// A reference to the native-typed field `name` of the CStruct handle
    /// `target`, `None` when `target` is no such handle or the field is not a
    /// native scalar.
    // Cost: O(f), f = fields of the struct (the layout lookup).
    pub(crate) fn cstruct_field_ref(&mut self, target: &Value, name: &str) -> Option<Value> {
        let ValueView::Instance { class_name, .. } = target.view() else {
            return None;
        };
        let registered = self.cstruct_class_name(&class_name.resolve())?;
        let layout = self.cstruct_layout(&registered)?;
        let field = layout.iter().find(|f| f.name == name)?;
        let type_name = ref_type_name(field.ty)?;
        Some(native_attr_ref(target.clone(), name, type_name))
    }

    /// Dispatch one of the two routines a native attribute reference calls;
    /// `None` for any other name.
    // Cost: O(f) per access, f = fields of the struct (one layout lookup).
    pub(crate) fn try_native_attr_ref_routine(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !is_native_attr_ref_routine(name) {
            return None;
        }
        let (handle, field) = match parts(args.first().unwrap_or(&Value::NIL)) {
            Ok(parts) => parts,
            Err(err) => return Some(Err(err)),
        };
        if name == FETCH {
            return Some(self.cstruct_field_value(&handle, &field).ok_or_else(|| {
                RuntimeError::new(format!("the struct has no field '{field}' to read"))
            }));
        }
        let value = args
            .get(1)
            .cloned()
            .unwrap_or(Value::NIL)
            .deref_container();
        Some(match self.cstruct_field_assign(&handle, &field, &value) {
            Ok(true) => Ok(value),
            Ok(false) => Err(RuntimeError::new(format!(
                "the struct has no field '{field}' to write"
            ))),
            Err(err) => Err(err),
        })
    }
}
