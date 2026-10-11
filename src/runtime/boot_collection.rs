//! The raw VM collection REPRs at the nqp level (ADR-12666): the five `BOOT*`
//! collection type objects, `nqp::create` of them, the `islist` / `ishash`
//! REPR tests, and what a raw `VMArray` / `VMHash` answers to a method call.
//!
//! The values themselves are `Value::vm_array` / `Value::vm_hash`
//! (`value::vm_collection`): a high-level view of the same storage with a raw
//! tag. A raw value reaches native builtins as if hllized; only the places
//! below tell it apart.

use super::*;
use crate::symbol::Symbol;
use crate::value::NqpElemKind;

/// The element kind behind a `BOOT*Array` type name.
// Cost: O(1).
fn boot_array_elem(name: &str) -> Option<NqpElemKind> {
    Some(match name {
        "BOOTArray" => NqpElemKind::Object,
        "BOOTIntArray" => NqpElemKind::Int,
        "BOOTNumArray" => NqpElemKind::Num,
        "BOOTStrArray" => NqpElemKind::Str,
        _ => return None,
    })
}

/// The type name an `nqp::boot*` collection op answers.
// Cost: O(1).
fn boot_collection_op_type(op: &str) -> Option<&'static str> {
    Some(match op {
        "bootarray" => "BOOTArray",
        "boothash" => "BOOTHash",
        "bootintarray" => "BOOTIntArray",
        "bootnumarray" => "BOOTNumArray",
        "bootstrarray" => "BOOTStrArray",
        _ => return None,
    })
}

/// The `BOOT*` collection type a type object names, if any.
// Cost: O(1).
pub(crate) fn boot_type_object_name(v: &Value) -> Option<Symbol> {
    match v.view() {
        ValueView::Package(name)
            if name.as_str() == "BOOTHash" || boot_array_elem(name.as_str()).is_some() =>
        {
            Some(name)
        }
        _ => None,
    }
}

/// `nqp::bootarray()` / `boothash()` / `bootintarray()` / `bootnumarray()` /
/// `bootstrarray()`: the type object.
// Cost: O(1).
pub(crate) fn boot_collection_type(op: &str) -> Option<Value> {
    boot_collection_op_type(op).map(|name| Value::package(Symbol::intern(name)))
}

/// `nqp::islist`: 1 exactly for the `VMArray` REPR — a raw `VMArray` of any
/// element kind, a `BOOT*Array` type object, a native `array[T]` and a
/// `Buf`/`Blob`. A Raku `List`, `Array`, `Slip` or `Seq` is P6opaque: 0.
// Cost: O(1) (O(n) in the length of a buffer class name).
pub(crate) fn repr_is_vm_array(v: &Value) -> bool {
    if v.is_vm_array() {
        return true;
    }
    match v.view() {
        ValueView::Package(name) => boot_array_elem(name.as_str()).is_some(),
        ValueView::Array(data, _) => {
            data.has_native_backing()
                || data
                    .declared_type
                    .as_deref()
                    .is_some_and(|t| t.starts_with("array"))
        }
        ValueView::Instance { class_name, .. } => {
            crate::runtime::utils::is_buf_or_blob_class(class_name.as_str())
        }
        _ => false,
    }
}

/// `nqp::ishash`: 1 exactly for the `VMHash` REPR — a raw `VMHash` or the
/// `BOOTHash` type object. A Raku `Hash`, `Map` or stash is P6opaque: 0.
// Cost: O(1).
pub(crate) fn repr_is_vm_hash(v: &Value) -> bool {
    v.is_vm_hash() || matches!(v.view(), ValueView::Package(name) if name.as_str() == "BOOTHash")
}

impl Interpreter {
    /// `nqp::create` of a `BOOT*` collection type object (or of a raw
    /// collection, whose type it takes): an empty raw value of that kind.
    // Cost: O(1).
    pub(crate) fn create_boot_collection(ty: &Value) -> Option<Value> {
        let name = match ty.vm_boot_type_name() {
            Some(name) => Symbol::intern(name),
            None => boot_type_object_name(ty)?,
        };
        Some(match boot_array_elem(name.as_str()) {
            Some(kind) => Value::nqp_typed_list(Vec::new(), kind),
            None => Value::vm_hash(crate::value::ValueMap::default()),
        })
    }

    /// A method call on a raw `VMArray` / `VMHash` or a `BOOT*` collection type
    /// object: Rakudo gives them a `KnowHOW` and no methods, so only the
    /// meta-level questions are answered and every other name dies with
    /// `X::Method::NotFound` naming the `BOOT*` type. `None` for any other
    /// invocant.
    // Cost: O(1).
    pub(crate) fn vm_collection_method(
        target: &Value,
        method: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        let name = match target.vm_boot_type_name() {
            Some(name) => name,
            None => boot_type_object_name(target)?.as_str(),
        };
        Some(match method {
            "WHAT" => Ok(Value::package(Symbol::intern(name))),
            "^name" => Ok(Value::str_from(name)),
            "REPR" => Ok(Value::str_from(if name == "BOOTHash" {
                "VMHash"
            } else {
                "VMArray"
            })),
            "HOW" => Ok(Value::package(Symbol::intern("KnowHOW"))),
            "VAR" => Ok(target.clone()),
            // Any other meta-method asks the `KnowHOW`, which has no such
            // method either.
            _ if let Some(meta) = method.strip_prefix('^') => Err(
                crate::runtime::did_you_mean::method_not_found(meta, "KnowHOW"),
            ),
            _ => Err(crate::runtime::did_you_mean::method_not_found(
                method, name,
            )),
        })
    }
}

/// Whether a method call on `target` must go through
/// [`Interpreter::vm_collection_method`]: a raw collection or a `BOOT*`
/// collection type object. Two tag probes on the common path.
// Cost: O(1).
#[inline]
pub(crate) fn is_boot_collection_invocant(target: &Value) -> bool {
    target.is_vm_collection()
        || (target.is_package_value() && boot_type_object_name(target).is_some())
}
