//! The native default constructor for the pun of a parametric role
//! (`R[Int].^pun.new(...)`, Monad's `Maybe[T]` constructors, #17331).
//!
//! A pun carries its type arguments in `Registry::class_role_param_bindings`,
//! which the full constructor installs into the env for the attribute
//! initializers and checks to read. When every argument is a type object and
//! every initializer is a literal, the only thing the bindings change is the
//! attributes' declared types (`has T $.value` is `has Int $.value`), so the
//! native constructor serves the pun with those types substituted into its
//! plan.

use super::*;

impl Interpreter {
    /// Whether the parametric-role pun `cn` may be built natively: it is a
    /// registered class whose role bindings qualify
    /// ([`Self::role_bindings_native_ok`]).
    // Cost: O(b + a), b = role bindings, a = the class's attributes.
    pub(crate) fn role_pun_native_constructible(&self, cn: &str) -> bool {
        let registry = self.registry();
        registry.class_role_param_bindings.contains_key(cn)
            && registry
                .classes
                .get(cn)
                .is_some_and(|class_def| Self::role_bindings_native_ok(&registry, cn, class_def))
    }

    /// The role bindings of class `cls` leave its construction plain data
    /// assembly: every attribute initializer is a literal or the type seed
    /// the declaration implies (an expression may read a binding by name); a constraint that names a binding names it
    /// bare (no smiley or parameterization to rewrite) and the binding is a
    /// type object; and the class declares no BUILD/TWEAK,
    /// whose bodies may read the bindings too.
    // Cost: O(b + a), b = role bindings, a = the class's attributes.
    pub(crate) fn role_bindings_native_ok(
        registry: &super::Registry,
        cls: &str,
        class_def: &ClassDef,
    ) -> bool {
        let Some(bindings) = registry.class_role_param_bindings.get(cls) else {
            return true;
        };
        let is_type = |v: &Value| matches!(v.view(), crate::value::ValueView::Package(_));
        class_def
            .attributes
            .iter()
            .all(|a| {
                a.default_is_seed || a.default.as_ref().is_none_or(|d| d.literal().is_some())
            })
            && class_def.attribute_types.values().all(|tc| match Self::role_binding_of(bindings, tc) {
                Some(v) => is_type(v),
                None => !bindings.keys().any(|k| {
                    let k = crate::qualified::type_capture_name(k).unwrap_or(k);
                    !k.is_empty() && !k.starts_with("__") && tc.contains(k)
                }),
            })
            && registry.user_method_overloads(cls, "BUILD").is_none()
            && registry.user_method_overloads(cls, "TWEAK").is_none()
    }

    /// `constraints` (attribute name -> declared type) with each constraint
    /// that names one of `cn`'s role type parameters replaced by the bound
    /// type object's name. A class without bindings gets them unchanged.
    // Cost: O(a), a = attributes, when `cn` has bindings; O(1) otherwise.
    pub(crate) fn role_bound_attribute_constraints(
        &self,
        cn: &str,
        mut constraints: HashMap<String, String>,
    ) -> HashMap<String, String> {
        let registry = self.registry();
        let Some(bindings) = registry.class_role_param_bindings.get(cn) else {
            return constraints;
        };
        for tc in constraints.values_mut() {
            if let Some(crate::value::ValueView::Package(name)) =
                Self::role_binding_of(bindings, tc).map(Value::view)
            {
                *tc = name.as_str().to_string();
            }
        }
        constraints
    }

    /// The binding of the role type parameter a constraint spells `tc`: a
    /// type capture is recorded as `::T` and written `T` in the constraint.
    // Cost: O(b), b = bindings (one hash probe, then a scan on a miss).
    fn role_binding_of<'a>(
        bindings: &'a HashMap<String, Value, rustc_hash::FxBuildHasher>,
        tc: &str,
    ) -> Option<&'a Value> {
        bindings.get(tc).or_else(|| {
            bindings
                .iter()
                .find(|(k, _)| crate::qualified::type_capture_name(k) == Some(tc))
                .map(|(_, v)| v)
        })
    }
}
