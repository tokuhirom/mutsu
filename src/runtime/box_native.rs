//! `nqp::box_*` into a class, and `nqp::unbox_*` out of one (#11209).
//!
//! MoarVM's `box_i`/`box_n`/`box_s`/`box_u` allocate an object of the type
//! operand and put the native value wherever that type keeps one, and the
//! `unbox_*` ops read it back:
//!
//! - a class with an `is box_target` attribute (`has int $!value is box_target`)
//!   stores the value in that attribute ([`super::box_target`]);
//! - an object of a `CStr` REPR class owns a C string ([`super::cstr_repr`]);
//! - a user class inheriting `Int`/`Num`/`Str` keeps it in the payload
//!   attribute its `.new` seeds.
//!
//! mutsu's `Int`, `Num` and `Str` are the values themselves, so an op whose type
//! operand is one of those, or a class with no boxing of its own, keeps
//! answering the plain value: both functions here answer `None` for it and the
//! op carries on with what it always did. Each op keeps only that one fall
//! through; the rule itself lives here once.

use super::*;
use crate::value::numeric_payload::{INT_PAYLOAD, NUM_PAYLOAD, numeric_payload_of};

/// The reserved attribute an `is Str` subclass instance keeps its payload in
/// (seeded by `runtime::seed_native_subclass_payloads`).
const STR_PAYLOAD: &str = "__mutsu_str_value";

impl Interpreter {
    /// `nqp::box_{i,n,s,u}($value, $type)` when `$type` is a class that boxes
    /// into its own storage. `None` leaves the op to its plain-value answer.
    /// A declared box target (`is box_target`, `CStr`) wins over the payload
    /// of an inherited native scalar, as MoarVM's REPR does.
    // Cost: O(1) for a built-in type operand while no `is box_target` attribute
    // or `CStr` class is declared; else O(d + a), d = MRO length (cached),
    // a = attributes of the class (one `nqp::create`).
    pub(crate) fn box_native_into_class(
        &mut self,
        ty: Option<&Value>,
        value: Value,
    ) -> Option<Result<Value, RuntimeError>> {
        let ty = ty?;
        if let Some(boxed) = Self::box_boot_scalar(ty, &value) {
            return Some(Ok(boxed));
        }
        if let Some(boxed) = self.box_into_declared_storage(ty, &value) {
            return Some(boxed);
        }
        self.box_into_scalar_subclass(ty, &value)
    }

    /// The `is box_target` attribute or `CStr` C string of `ty`, if it has one.
    // Cost: O(1) while no `is box_target` attribute or `CStr` class is declared;
    // else O(a), a = attributes of the class (one `nqp::create`), plus the
    // MRO walk of `box_target_attr_of_class`.
    fn box_into_declared_storage(
        &mut self,
        ty: &Value,
        value: &Value,
    ) -> Option<Result<Value, RuntimeError>> {
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
            Self::nqp_bindattr_value("bindattr", &object, &attr, value.clone())?;
            Ok(object)
        }))
    }

    /// `nqp::box_{i,n,s,u}` into a user class inheriting the matching native
    /// scalar (`class ValueObjAt is Str`, `class MyInt is Int`): MoarVM
    /// allocates the subclass with the value in its P6int/P6num/P6str body,
    /// which in mutsu is the payload attribute `.new` seeds
    /// (`runtime::seed_native_subclass_payloads`). The type operand may be
    /// the class or an instance of it. `None` for any other operand, or when
    /// the class inherits a different scalar than the value's.
    // Cost: O(1) for a built-in or unregistered type operand; else O(d + a),
    // d = MRO length (cached), a = attributes of the class (one `nqp::create`).
    fn box_into_scalar_subclass(
        &mut self,
        ty: &Value,
        value: &Value,
    ) -> Option<Result<Value, RuntimeError>> {
        let class = match ty.view() {
            ValueView::Package(name) => name,
            ValueView::Instance { class_name, .. } => class_name,
            _ => return None,
        };
        // `ObjAt` / `ValueObjAt` are core `is Str` classes mutsu builds
        // natively, around their `WHICH` string (`ValueMap.WHICH` boxes into
        // `ValueObjAt`).
        if matches!(class.as_str(), "ObjAt" | "ValueObjAt") {
            return matches!(value.view(), ValueView::Str(_))
                .then(|| Self::build_native_objat_value(class, std::slice::from_ref(value)));
        }
        let payload_key = match (
            self.native_scalar_backing_parent(class.as_str())?,
            value.view(),
        ) {
            ("Int", ValueView::Int(_) | ValueView::BigInt(_)) => INT_PAYLOAD,
            ("Num", ValueView::Num(_)) => NUM_PAYLOAD,
            ("Str", ValueView::Str(_)) => STR_PAYLOAD,
            // MoarVM's P6opaque boxes only the native its parent scalar holds.
            ("Int" | "Num" | "Str", _) => {
                let native = match value.view() {
                    ValueView::Int(_) | ValueView::BigInt(_) => "integer",
                    ValueView::Num(_) => "number",
                    _ => "string",
                };
                return Some(Err(RuntimeError::new(format!(
                    "This type cannot box a native {native}: P6opaque, {}",
                    crate::value::user_facing_type_name(&class.resolve())
                ))));
            }
            _ => return None,
        };
        Some(self.nqp_create(Value::package(class)).inspect(|object| {
            if let ValueView::Instance { attributes, .. } = object.view() {
                attributes.insert(payload_key, value.clone());
            }
        }))
    }

    /// What `nqp::unbox_*` reads out of `obj` when it boxes a native value of
    /// its own: its `is box_target` attribute's value, the string a CStr
    /// object holds (`Nil` for a NULL one), or the payload of a user subclass
    /// of `Int`/`Num`/`Str`. `None` for any other value.
    // Cost: O(1) for a value that is not an object; else O(m + r) like
    // `box_target_attr_name`, plus O(n) to decode a C string of n bytes.
    pub(crate) fn unbox_native_through(&mut self, obj: &Value) -> Option<Value> {
        if let Some(inner) = Self::unbox_boot_scalar(obj) {
            return Some(inner);
        }
        if !matches!(obj.view(), ValueView::Instance { .. } | ValueView::Mixin(..)) {
            return None;
        }
        self.unbox_declared_storage(obj)
            .or_else(|| Self::scalar_subclass_payload(obj))
    }

    /// The `is box_target` attribute's value or the CStr string of `obj`.
    // Cost: O(1) while no `is box_target` attribute or `CStr` class is declared;
    // else as `unbox_native_through`.
    fn unbox_declared_storage(&mut self, obj: &Value) -> Option<Value> {
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

    /// The payload `box_into_scalar_subclass` (or the class's `.new`) put in
    /// an instance of a user subclass of `Int`/`Num`/`Str`.
    // Cost: O(1), at most four attribute-map probes.
    fn scalar_subclass_payload(obj: &Value) -> Option<Value> {
        let ValueView::Instance { attributes, .. } = obj.view() else {
            return None;
        };
        numeric_payload_of(&attributes).or_else(|| attributes.as_map().get(STR_PAYLOAD).cloned())
    }
}
