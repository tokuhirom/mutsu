//! MoarVM's `BOOTInt` / `BOOTNum` / `BOOTStr` boxes (ADR-11553 §3.1).
//!
//! `nqp::box_i($i, nqp::bootint())` makes an object of the `P6int` REPR, which
//! is *not* a Raku `Int`: `nqp::isint` answers 1 for it and 0 for `1`, and
//! `nqp::hllize` turns it into the high-level `Int`. mutsu's `Int`, `Num` and
//! `Str` are the values themselves, so the raw box is an instance of a
//! built-in class named after the BOOT type, holding the native value in one
//! hidden attribute. `unbox_*`, `isint`/`isnum`/`isstr`, `hllize` and the
//! `boot*` type objects all read that one representation.

use super::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

/// The hidden attribute holding the native value of a BOOT scalar box.
const BOOT_VALUE: &str = "__mutsu_boot_value";

/// The BOOT scalar type behind `name`, as the `is*` test spells it.
// Cost: O(1).
fn boot_scalar_kind(name: &str) -> Option<&'static str> {
    match name {
        "BOOTInt" => Some("isint"),
        "BOOTNum" => Some("isnum"),
        "BOOTStr" => Some("isstr"),
        _ => None,
    }
}

impl Interpreter {
    /// Register `name` (a `BOOT*` type) as a class on first use, so `.WHAT`,
    /// `.^name` and `nqp::create` see a real type.
    // Cost: O(1) after the first call.
    fn ensure_boot_scalar_class(&mut self, name: &str) {
        if self.registry().classes.contains_key(name) {
            return;
        }
        self.registry_mut().classes.insert(
            name.to_string(),
            ClassDef {
                parents: vec!["Any".to_string()],
                mro: [Symbol::intern(name), Symbol::intern("Any"), Symbol::intern("Mu")].into(),
                ..ClassDef::default()
            }
            .into(),
        );
    }

    /// `nqp::bootint()` / `bootnum()` / `bootstr()`: the type object.
    // Cost: O(1).
    pub(crate) fn boot_scalar_type(&mut self, op: &str) -> Option<Value> {
        let name = match op {
            "bootint" => "BOOTInt",
            "bootnum" => "BOOTNum",
            "bootstr" => "BOOTStr",
            _ => return None,
        };
        self.ensure_boot_scalar_class(name);
        Some(Value::package(Symbol::intern(name)))
    }

    /// `nqp::box_*($value, $type)` when `$type` is a BOOT scalar type.
    // Cost: O(1).
    pub(crate) fn box_boot_scalar(&mut self, ty: &Value, value: &Value) -> Option<Value> {
        let ValueView::Package(name) = ty.view() else {
            return None;
        };
        let name = name.resolve();
        boot_scalar_kind(&name)?;
        self.ensure_boot_scalar_class(&name);
        let mut attrs = std::collections::HashMap::new();
        attrs.insert(BOOT_VALUE.to_string(), value.clone());
        Some(Value::make_instance(Symbol::intern(&name), attrs))
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
        boot_scalar_kind(&class_name.resolve())?;
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
                boot_scalar_kind(&class_name.resolve()) == Some(op)
            }
            _ => false,
        })
    }
}
