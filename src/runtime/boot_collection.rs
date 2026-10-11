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

/// Whether `name` is one of the BOOT types: a collection (`BOOTArray`,
/// `BOOTHash`, `BOOT*Array`) or a scalar box type (`BOOTInt`, `BOOTNum`,
/// `BOOTStr`).
// Cost: O(1).
fn is_boot_type_name(name: &str) -> bool {
    name == "BOOTHash"
        || boot_array_elem(name).is_some()
        || super::boot_scalar::boot_scalar_kind(name).is_some()
}

/// The `BOOT*` type a type object names, if any.
// Cost: O(1).
pub(crate) fn boot_type_object_name(v: &Value) -> Option<Symbol> {
    match v.view() {
        ValueView::Package(name) if is_boot_type_name(name.as_str()) => Some(name),
        _ => None,
    }
}

/// The `BOOT*` type of a raw value or BOOT type object: a raw `VMArray` /
/// `VMHash`, a `BOOT*` type object, or a BOOT scalar box
/// (`nqp::box_i($i, nqp::bootint())`). `None` for anything else.
// Cost: O(1).
pub(crate) fn boot_type_of(v: &Value) -> Option<Symbol> {
    if let Some(name) = v.vm_boot_type_name() {
        return Some(Symbol::intern(name));
    }
    match v.view() {
        ValueView::Package(name) | ValueView::Instance {
            class_name: name, ..
        } if is_boot_type_name(name.as_str()) => Some(name),
        _ => None,
    }
}

/// The REPR a `BOOT*` type has.
// Cost: O(1).
fn boot_repr(name: &str) -> &'static str {
    match name {
        "BOOTHash" => "VMHash",
        "BOOTInt" => "P6int",
        "BOOTNum" => "P6num",
        "BOOTStr" => "P6str",
        _ => "VMArray",
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
    if v.is_vm_array() || v.is_iteration_buffer_view() {
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
            class_name == "IterationBuffer"
                || crate::runtime::utils::is_buf_or_blob_class(class_name.as_str())
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
    /// `nqp::create` of a `BOOT*` type object (or of a raw value, whose type
    /// it takes): an empty raw collection, or a scalar box holding zero.
    // Cost: O(1).
    pub(crate) fn create_boot_collection(ty: &Value) -> Option<Value> {
        let name = boot_type_of(ty)?;
        if let Some(boxed) = Self::create_boot_scalar(name.as_str()) {
            return Some(boxed);
        }
        Some(match boot_array_elem(name.as_str()) {
            Some(kind) => Value::nqp_typed_list(Vec::new(), kind),
            None => Value::vm_hash(crate::value::ValueMap::default()),
        })
    }

    /// A method call on a raw `VMArray` / `VMHash`, a BOOT scalar box or a
    /// `BOOT*` type object: Rakudo gives them a `KnowHOW` and no methods, so
    /// only the meta-level questions are answered and every other name dies
    /// with `X::Method::NotFound` naming the `BOOT*` type. `None` for any
    /// other invocant.
    // Cost: O(1).
    pub(crate) fn vm_collection_method(
        target: &Value,
        method: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        // An `IterationBuffer` view is a high-level `Any`: it answers its own
        // type and leaves every other method to the ordinary dispatch.
        if target.is_iteration_buffer_view() {
            return match method {
                "WHAT" => Some(Ok(Value::package(Symbol::intern("IterationBuffer")))),
                "^name" => Some(Ok(Value::str_from("IterationBuffer"))),
                "REPR" => Some(Ok(Value::str_from("VMArray"))),
                // `IterationBuffer` derives from `Any` alone, not from the
                // `List` whose storage it views (#12700).
                "^mro" => Some(Ok(Value::array(
                    ["IterationBuffer", "Any", "Mu"]
                        .map(|n| Value::package(Symbol::intern(n)))
                        .to_vec(),
                ))),
                _ => None,
            };
        }
        let name = boot_type_of(target)?.as_str();
        Some(match method {
            "WHAT" => Ok(Value::package(Symbol::intern(name))),
            "^name" => Ok(Value::str_from(name)),
            "REPR" => Ok(Value::str_from(boot_repr(name))),
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
/// [`Interpreter::vm_collection_method`]: a raw collection, an
/// `IterationBuffer` view, a BOOT scalar box or a `BOOT*` type object. Tag
/// probes on the common path; an object pays one class-name match.
// Cost: O(1).
#[inline]
pub(crate) fn is_boot_collection_invocant(target: &Value) -> bool {
    target.is_vm_collection()
        || target.is_iteration_buffer_view()
        || (target.is_package_value() && boot_type_object_name(target).is_some())
        || matches!(target.view(), ValueView::Instance { class_name, .. }
            if super::boot_scalar::boot_scalar_kind(class_name.as_str()).is_some())
}

impl Interpreter {
    /// `nqp::p6bindattrinvres($obj, T, '$!reified' | '$!storage', $raw)` with a
    /// raw `VMArray` / `VMHash` `$raw`: Rakudo installs `$raw` as the
    /// wrapper's storage, so the object handed back and `$raw` share every
    /// element from then on. mutsu's high-level collection *is* its storage
    /// node, so the answer is the high-level view (the wrapper's kind) of
    /// `$raw`'s node — O(1), where copying the elements in was O(e) and lost
    /// every later write through `$raw` (ADR-12666 §3.6). The wrapper's own
    /// container metadata (element type, declared type, default) moves onto
    /// the adopted node.
    ///
    /// `None` when this is not such a bind (another attribute, a non-raw
    /// value, a shaped or lazy wrapper, an instance): the caller binds as
    /// before. A plain `nqp::bindattr` onto an existing object cannot adopt —
    /// every holder of the object points at its own node (#12672).
    // Cost: O(1).
    pub(crate) fn adopt_raw_storage(obj: &Value, attr: &str, raw: &Value) -> Option<Value> {
        let bare = attr
            .trim_start_matches(['$', '@', '%', '&'])
            .trim_start_matches(['!', '.']);
        if !matches!(bare, "reified" | "storage")
            || obj.is_vm_collection()
            || obj.is_iteration_buffer_view()
        {
            return None;
        }
        match (obj.view(), raw.view()) {
            (ValueView::Array(old, kind), ValueView::Array(new, _))
                if (raw.is_vm_array() || raw.is_iteration_buffer_view())
                    && matches!(
                        kind,
                        crate::value::ArrayKind::List | crate::value::ArrayKind::Array
                    ) =>
            {
                if !crate::gc::Gc::ptr_eq(&old, &new) {
                    // SAFETY: audited aliased in-place container write (see
                    // value::aliased_mut) — a metadata copy onto the adopted
                    // node, no other borrow into it live across the write.
                    let data = unsafe { crate::value::gc_contents_mut(&new) };
                    if old.value_type.is_some() {
                        data.value_type.clone_from(&old.value_type);
                    }
                    if old.declared_type.is_some() {
                        data.declared_type.clone_from(&old.declared_type);
                    }
                    if old.default.is_some() {
                        data.default.clone_from(&old.default);
                    }
                }
                Some(Value::array_with_kind((*new).clone(), kind))
            }
            (ValueView::Hash(old), ValueView::Hash(new)) if raw.is_vm_hash() => {
                if !crate::gc::Gc::ptr_eq(&old, &new) {
                    // SAFETY: as above.
                    let data = unsafe { crate::value::gc_contents_mut(&new) };
                    if old.value_type.is_some() {
                        data.value_type.clone_from(&old.value_type);
                    }
                    if old.key_type.is_some() {
                        data.key_type.clone_from(&old.key_type);
                    }
                    if old.declared_type.is_some() {
                        data.declared_type.clone_from(&old.declared_type);
                    }
                    if old.default.is_some() {
                        data.default.clone_from(&old.default);
                    }
                    data.bare_values |= old.bare_values;
                }
                Some(Value::hash_with_data_itemized(
                    (*new).clone(),
                    obj.hash_is_itemized(),
                ))
            }
            _ => None,
        }
    }
}

