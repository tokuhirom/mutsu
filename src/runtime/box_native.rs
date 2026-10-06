//! `nqp::box_*` into a class, and `nqp::unbox_*` out of one (#11209).
//!
//! MoarVM's `box_i`/`box_n`/`box_s`/`box_u` allocate an object of the type
//! operand and put the native value wherever that type keeps one, and the
//! `unbox_*` ops read it back:
//!
//! - a class with an `is box_target` attribute (`has int $!value is box_target`)
//!   stores the value in that attribute ([`super::box_target`]);
//! - an object of a `CStr` REPR class owns a C string ([`super::cstr_repr`]).
//!
//! mutsu's `Int`, `Num` and `Str` are the values themselves, so an op whose type
//! operand is one of those, or a class with no boxing of its own, keeps
//! answering the plain value: both functions here answer `None` for it and the
//! op carries on with what it always did. Each op keeps only that one fall
//! through; the rule itself lives here once.

use super::*;

impl Interpreter {
    /// `nqp::box_{i,n,s,u}($value, $type)` when `$type` is a class that boxes
    /// into its own storage. `None` leaves the op to its plain-value answer.
    // Cost: O(1) while no `is box_target` attribute or `CStr` class is declared;
    // else O(a), a = attributes of the class (one `nqp::create`), plus the
    // MRO walk of `box_target_attr_of_class`.
    pub(crate) fn box_native_into_class(
        &mut self,
        ty: Option<&Value>,
        value: Value,
    ) -> Option<Result<Value, RuntimeError>> {
        let ty = ty?;
        let ValueView::Package(name) = ty.view() else {
            return None;
        };
        {
            let reg = self.registry();
            if reg.cstr_classes.is_empty() && reg.box_target_attrs.is_empty() {
                return None;
            }
        }
        if self.registry().cstr_classes.contains(name.as_str()) {
            return Some(match value.view() {
                ValueView::Str(_) => self.box_str_into_cstr(ty, &value.to_string_value()),
                // MoarVM's CStr REPR boxes strings only.
                _ => Err(RuntimeError::new(format!(
                    "{} is a CStr representation and boxes only a str",
                    name.as_str()
                ))),
            });
        }
        let attr = self.box_target_attr_of_class(name.as_str())?;
        Some(self.nqp_create(ty.clone()).and_then(|object| {
            Self::nqp_bindattr_value("bindattr", &object, &attr, value)?;
            Ok(object)
        }))
    }

    /// What `nqp::unbox_*` reads out of `obj` when it boxes a native value of
    /// its own: its `is box_target` attribute's value, or the string a CStr
    /// object holds (`Nil` for a NULL one). `None` for any other value.
    // Cost: O(1) for a value that is not an object; else O(m + r) like
    // `box_target_attr_name`, plus O(n) to decode a C string of n bytes.
    pub(crate) fn unbox_native_through(&mut self, obj: &Value) -> Option<Value> {
        if !matches!(obj.view(), ValueView::Instance { .. } | ValueView::Mixin(..)) {
            return None;
        }
        {
            let reg = self.registry();
            if reg.cstr_classes.is_empty() && reg.box_target_attrs.is_empty() {
                return None;
            }
        }
        if let Some(text) = self.cstr_object_string(obj) {
            return Some(text);
        }
        let attr = self.box_target_attr_name(obj)?;
        Self::nqp_attr_value(obj, &attr)
            .map(|v| crate::runtime::types::unwrap_varref_value(v).deref_container())
    }
}
