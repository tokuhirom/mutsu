//! `is box_target` and the `NativeCall` REPR (#11209, ADR-11203 §2.4).
//!
//! Upstream `NativeCall.rakumod` keeps a routine's built call in an attribute
//! of the role it mixes into the routine:
//!
//! ```raku
//! my class Callsite is repr<NativeCall> { }
//! our role Native[...] {
//!     has Callsite $!call is box_target;
//!     ...
//!     method !setup() { ... return if nqp::unbox_i($!call); ... nqp::buildnativecall(self, ...) }
//! }
//! ```
//!
//! `is box_target` names the attribute whose body the object *is* as far as
//! MoarVM's native ops go: `nqp::buildnativecall(self, ...)` builds the
//! callsite in `$!call`, and `nqp::nativecall($rettype, self, $args)` calls the
//! one in `$!call`. The routine itself is never the callsite, so `!setup` runs
//! once and `nqp::unbox_i($!call)` answers non-zero from then on.
//!
//! mutsu records the attribute per declaring class or role
//! ([`Registry::box_target_attrs`](super::registry::Registry)); a declaration
//! of a class-typed box target also gets a concrete instance of its type at
//! construction (the parser seeds `Type.CREATE`, the REPR allocating its body,
//! as MoarVM inlines it into the object). Native ops on an object with no
//! such attribute are unaffected.
//!
//! The family is only wired for the FFI ops so far: `nqp::box_*` into a class
//! with a box target and `unbox_*` through one are not delegated.

use super::*;
use crate::meta_ns::MetaNs;

/// The role name without its type arguments (`Native[Routine, Str]` -> `Native`).
// Cost: O(n), n = chars of the name.
fn role_base_name(name: &str) -> &str {
    name.split_once('[').map_or(name, |(base, _)| base)
}

impl Interpreter {
    /// Record a class declared `is repr<NativeCall>`.
    // Cost: O(n), n = chars of the name.
    pub(crate) fn register_nativecall_class(&mut self, name: &str) {
        self.registry_mut()
            .nativecall_classes
            .insert(name.to_string());
    }

    /// Record that `owner` (a class, or a role by name) declared `attr`
    /// `is box_target`. One box target per type, as in MoarVM; a second
    /// declaration replaces the first.
    // Cost: O(n), n = chars of the names.
    pub(crate) fn register_box_target(&mut self, owner: &str, attr: &str) {
        self.registry_mut()
            .box_target_attrs
            .insert(role_base_name(owner).to_string(), attr.to_string());
    }

    /// Whether a `has` declaration carries the core `is box_target` trait.
    // Cost: O(t), t = unknown traits on the declaration.
    pub(crate) fn declares_box_target(decl: &crate::opcode::CompiledAttrDecl) -> bool {
        decl.unknown_traits
            .iter()
            .any(|(kind, name, _)| kind == "is" && name == "box_target")
    }

    /// The bare name of the box-target attribute `obj` has, from its class,
    /// the roles that class composes, or the roles mixed into the value.
    // Cost: O(1) while no `is box_target` attribute is declared; else O(r + m),
    // r = roles mixed in, m = classes in the MRO.
    pub(crate) fn box_target_attr_name(&mut self, obj: &Value) -> Option<String> {
        if self.registry().box_target_attrs.is_empty() {
            return None;
        }
        match obj.view() {
            ValueView::Scalar(inner) => self.box_target_attr_name(inner),
            ValueView::Mixin(inner, mixins) => {
                let reg = self.registry();
                let from_roles = mixins
                    .keys()
                    .filter_map(|key| key.strip_prefix(MetaNs::Role.prefix()))
                    .find_map(|role| reg.box_target_attrs.get(role_base_name(role)).cloned());
                drop(reg);
                from_roles.or_else(|| self.box_target_attr_name(inner))
            }
            ValueView::Instance { class_name, .. } => {
                self.box_target_attr_of_class(class_name.as_str())
            }
            _ => None,
        }
    }

    /// The bare name of the box-target attribute `class` has, declared by the
    /// class itself, a class it inherits from, or a role any of them composes.
    // Cost: O(1) while no `is box_target` attribute is declared; else O(m + r),
    // m = classes in the MRO, r = roles they compose.
    pub(crate) fn box_target_attr_of_class(&mut self, class: &str) -> Option<String> {
        if self.registry().box_target_attrs.is_empty() {
            return None;
        }
        let mro = self.class_mro(class);
        let reg = self.registry();
        mro.iter().find_map(|class| {
            let class = class.as_str();
            reg.box_target_attrs.get(class).cloned().or_else(|| {
                reg.class_composed_roles.get(class).and_then(|roles| {
                    roles
                        .iter()
                        .find_map(|role| reg.box_target_attrs.get(role_base_name(role)))
                        .cloned()
                })
            })
        })
    }

    /// Whether `class` was declared with a REPR whose instance is its own body
    /// (`is repr<NativeCall>`, `is repr<CStr>`): `.REPR` can report it for an
    /// instance as well as for the type object, since there is no separate
    /// body for it to under-report.
    // Cost: O(n), n = chars of the name.
    pub(crate) fn is_bodied_repr_class(&self, class: &str) -> bool {
        let reg = self.registry();
        reg.nativecall_classes.contains(class) || reg.cstr_classes.contains(class)
    }

    /// The object native ops on `obj` apply to: the value of `obj`'s
    /// `is box_target` attribute, or `obj` itself when it has none.
    ///
    /// A box target that was never allocated (the attribute holds a type
    /// object) is an error rather than a silent fallback to `obj`: the op would
    /// build or call a callsite no later read of the attribute could see.
    // Cost: O(1) when no `is box_target` attribute exists; see `box_target_attr_name`.
    pub(crate) fn box_target_operand(
        &mut self,
        op: &str,
        obj: Value,
    ) -> Result<Value, RuntimeError> {
        let Some(attr) = self.box_target_attr_name(&obj) else {
            return Ok(obj);
        };
        let target = Self::nqp_attr_value(&obj, &attr)
            .map(|v| crate::runtime::types::unwrap_varref_value(v).deref_container());
        match target {
            Some(value) if matches!(value.view(), ValueView::Instance { .. }) => Ok(value),
            _ => Err(RuntimeError::new(format!(
                "nqp::{op}: the box target attribute '{attr}' of {} has no body to operate on",
                crate::value::type_name::value_type_name(&obj)
            ))),
        }
    }
}
