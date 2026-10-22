//! MoarVM's `BOOTInt` / `BOOTNum` / `BOOTStr` boxes (ADR-11553 §3.1).
//!
//! `nqp::box_i($i, nqp::bootint())` makes an object of the `P6int` REPR, which
//! is *not* a Raku `Int`: `nqp::isint` answers 1 for it and 0 for `1`, and
//! `nqp::hllize` turns it into the high-level `Int`. mutsu's `Int`, `Num` and
//! `Str` are the values themselves, so the raw box is an object named after
//! the BOOT type, holding the native value in one hidden attribute. `unbox_*`,
//! `isint`/`isnum`/`isstr`, `hllize` and the `boot*` type objects all read that
//! one representation.
//!
//! Like the BOOT collection types (ADR-12666), these are `KnowHOW` types
//! outside `Mu`: no class is registered for them, and what a box or type
//! object answers to a method call or a type check is decided in
//! `runtime::boot_collection` for every BOOT type alike (#12670).

use super::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

/// The hidden attribute holding the native value of a BOOT scalar box.
const BOOT_VALUE: &str = "__mutsu_boot_value";

/// The BOOT scalar type behind `name`, as the `is*` test spells it.
// Cost: O(1).
pub(super) fn boot_scalar_kind(name: &str) -> Option<&'static str> {
    match name {
        "BOOTInt" => Some("isint"),
        "BOOTNum" => Some("isnum"),
        "BOOTStr" => Some("isstr"),
        _ => None,
    }
}

/// A box of BOOT scalar type `name` holding `value`.
// Cost: O(1).
fn boot_box(name: Symbol, value: Value) -> Value {
    let mut attrs = std::collections::HashMap::new();
    attrs.insert(BOOT_VALUE.to_string(), value);
    Value::make_instance(name, attrs)
}

impl Interpreter {
    /// Convert an NQP value at a Raku HLL boundary, sharing the conversion
    /// used by `nqp::hllize` and method-call results.
    // Cost: O(1).
    pub(crate) fn hllize_value(value: Value) -> Value {
        Self::unbox_boot_scalar(&value).unwrap_or_else(|| value.hllize_vm_collection())
    }

    /// `nqp::bootint()` / `bootnum()` / `bootstr()`: the type object.
    // Cost: O(1).
    pub(crate) fn boot_scalar_type(op: &str) -> Option<Value> {
        let name = match op {
            "bootint" => "BOOTInt",
            "bootnum" => "BOOTNum",
            "bootstr" => "BOOTStr",
            _ => return None,
        };
        Some(Value::package(Symbol::intern(name)))
    }

    /// `nqp::box_*($value, $type)` when `$type` is a BOOT scalar type.
    // Cost: O(1).
    pub(crate) fn box_boot_scalar(ty: &Value, value: &Value) -> Option<Value> {
        let ValueView::Package(name) = ty.view() else {
            return None;
        };
        boot_scalar_kind(name.as_str())?;
        Some(boot_box(name, value.clone()))
    }

    /// A `BOOTInt` box holding `i`, as MoarVM boxes the integers an async
    /// event pushes onto its queue (a signal number, a byte count).
    // Cost: O(1).
    pub(crate) fn boot_int(i: i64) -> Value {
        boot_box(Symbol::intern("BOOTInt"), Value::int(i))
    }

    /// A `BOOTStr` box holding `s` (a watched file's name, an error).
    // Cost: O(len(s)).
    pub(crate) fn boot_str(s: &str) -> Value {
        boot_box(Symbol::intern("BOOTStr"), Value::str_from(s))
    }

    /// `nqp::create` of a BOOT scalar type: a box holding the type's zero.
    // Cost: O(1).
    pub(crate) fn create_boot_scalar(name: &str) -> Option<Value> {
        let zero = match boot_scalar_kind(name)? {
            "isint" => Value::int(0),
            "isnum" => Value::num(0.0),
            _ => Value::str_from(""),
        };
        Some(boot_box(Symbol::intern(name), zero))
    }

    /// The native value inside a BOOT scalar box (`None` for any other value).
    // Cost: O(1).
    pub(crate) fn unbox_boot_scalar(obj: &Value) -> Option<Value> {
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = obj.view()
        else {
            return None;
        };
        boot_scalar_kind(class_name.as_str())?;
        attributes.as_map().get(BOOT_VALUE).cloned()
    }

    /// `nqp::isint` / `isnum` / `isstr`: 1 for a BOOT box (or type object) of
    /// that REPR, 0 for anything else, a Raku `Int`/`Num`/`Str` included.
    // Cost: O(1).
    pub(crate) fn boot_scalar_test(op: &str, v: &Value) -> Option<bool> {
        if !matches!(op, "isint" | "isnum" | "isstr") {
            return None;
        }
        Some(match v.view() {
            ValueView::Instance { class_name, .. } | ValueView::Package(class_name) => {
                boot_scalar_kind(class_name.as_str()) == Some(op)
            }
            _ => false,
        })
    }
}
