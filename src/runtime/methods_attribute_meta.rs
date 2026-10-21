//! The `Attribute` metaobject's methods that need the interpreter: `set_build`,
//! `build`, `get_value` and `set_value` (ADR-11276 §9.121). The rows are in
//! `builtins/method_table/ctors_mop/attribute.rs`; each calls one method here.

use super::*;
use crate::value::InstanceAttrs;

/// The attribute `key` of an `Attribute` metaobject, as a string; empty when
/// it is not set.
// Cost: O(1), one hash probe.
fn meta_str(meta: &InstanceAttrs, key: &str) -> String {
    meta.as_map()
        .get(key)
        .map(|v| v.to_string_value())
        .unwrap_or_default()
}

/// The `(owner, attribute name)` key `set_build` records an override under.
// Cost: O(1), three hash probes.
fn build_override_key(meta: &InstanceAttrs) -> (String, String) {
    let map = meta.as_map();
    let owner = map
        .get("__mutsu_attr_owner")
        .map(|v| v.to_string_value())
        .unwrap_or_default();
    let attr_name = map
        .get("__mutsu_attr_name")
        .or_else(|| map.get("name"))
        .map(|v| v.to_string_value())
        .unwrap_or_default();
    (owner, attr_name)
}

/// The instance-map key an `Attribute` meta object reads or writes. A private
/// declaration whose bare name collides with another sigil's declaration is
/// stored under `<sigil>name` (see `attribute_storage_key`); the public one
/// and every non-colliding attribute keep the bare name.
// Cost: O(1), two hash probes of the instance's attribute map.
fn attribute_meta_storage_key(
    meta: &InstanceAttrs,
    obj_attrs: &InstanceAttrs,
    attr_name: &str,
) -> Symbol {
    let is_public = meta.as_map().get("is_public").is_some_and(|v| v.truthy());
    let sigil = meta_str(meta, "sigil");
    if !is_public && matches!(sigil.as_str(), "@" | "%" | "&" | "$") {
        let sigil_key = Symbol::intern(&format!("{sigil}{attr_name}"));
        if obj_attrs.as_map().contains_key(sigil_key) {
            return sigil_key;
        }
    }
    Symbol::intern(attr_name)
}

impl Interpreter {
    /// `Attribute.set_build(&build)`: the callback that builds the attribute's
    /// value replaces its initializer.
    // Cost: O(1) amortized: one hash insert and a cache clear.
    pub(crate) fn attribute_meta_set_build(
        &mut self,
        target: &Value,
        meta: &InstanceAttrs,
        build: &Value,
    ) -> Result<Value, RuntimeError> {
        let (owner, attr_name) = build_override_key(meta);
        if owner.is_empty() || attr_name.is_empty() {
            return Err(RuntimeError::new(
                "Attribute.set_build missing owner or attribute name",
            ));
        }
        self.registry_mut()
            .attribute_build_overrides
            .insert((owner, attr_name), build.clone());
        // A build override disqualifies the class from the native
        // default constructor — drop any cached plan for it.
        self.caches.native_ctor_plan_cache.clear();
        Ok(target.clone())
    }

    /// `Attribute.build`: what builds the attribute's value (`set_build`
    /// replaces it); the type object `Mu` when it has no initializer.
    // Cost: O(1), a few hash probes.
    pub(crate) fn attribute_meta_build(&self, meta: &InstanceAttrs) -> Value {
        if let Some(over) = self
            .registry()
            .attribute_build_overrides
            .get(&build_override_key(meta))
        {
            return over.clone();
        }
        if let Some(build_val) = meta.as_map().get("build") {
            return build_val.clone();
        }
        if meta
            .as_map()
            .get("__mutsu_has_build")
            .is_some_and(|v| v.truthy())
        {
            return Value::TRUE;
        }
        // No initializer: the type object `Mu` (rakudo).
        Value::package(Symbol::intern("Mu"))
    }

    /// `Attribute.get_value($obj)`: the attribute's value in `$obj`.
    // Cost: O(1), a few hash probes.
    pub(crate) fn attribute_meta_get_value(
        &self,
        meta: &InstanceAttrs,
        obj: &Value,
    ) -> Result<Value, RuntimeError> {
        // The object may arrive wrapped in a Scalar container
        // (a `$`-bound alias of the instance); read through it.
        let obj = obj.clone().deref_container();
        let attr_name = meta_str(meta, "__mutsu_attr_name");
        // An attribute a mixed-in role declares lives in the
        // mixin's own role cell, not in the wrapped object.
        if let ValueView::Mixin(_, mixins) = obj.view() {
            let owner = meta_str(meta, "__mutsu_attr_owner");
            let bare = attr_name.trim_start_matches(['$', '@', '%', '&', '!', '.']);
            if let Some(v) = mixins.role_attribute(&owner, bare) {
                return Ok(v.deref_container());
            }
        }
        // A `$obj but Role` mixin keeps the object's attributes
        // on the value it wraps.
        let obj = match obj.view() {
            ValueView::Mixin(inner, _) => Value::clone(inner),
            _ => obj.clone(),
        };
        if let ValueView::Instance {
            attributes: obj_attrs,
            ..
        } = obj.view()
        {
            let key = attribute_meta_storage_key(meta, &obj_attrs, &attr_name);
            // The read first: an ABSENT slot is recorded as a pending lazy
            // initializer (`set_attribute_value`'s race rule), and answers
            // `Nil` rather than a container the slot does not have yet.
            let value = obj_attrs.get_attribute_value(key);
            // A scalar attribute's value is its Scalar container (Rakudo's
            // `get_value` hands that out, so `nqp::iscont` holds and a store
            // through it is the attribute's own); a Proxy bound there, or a
            // list/hash slot, is already its own container.
            if !attr_name.starts_with(['@', '%', '&'])
                && obj_attrs.as_map().get(key).is_some()
                && !matches!(
                    value.view(),
                    ValueView::Array(..)
                        | ValueView::Hash(_)
                        | ValueView::Mixin(..)
                        | ValueView::Proxy { .. }
                )
            {
                return Ok(obj_attrs.promote_attr_to_container(key));
            }
            return Ok(value);
        }
        // An attribute has no value on a type object.
        if let ValueView::Package(name) = obj.view()
            && self.registry().classes.contains_key(name.as_str())
        {
            return Err(RuntimeError::new(format!(
                "Cannot look up attributes in a {} type object. Did you forget a '.new'?",
                name.resolve()
            )));
        }
        Ok(Value::NIL)
    }

    /// `Attribute.set_value($obj, $value)`: writes the attribute's value in
    /// `$obj`.
    // Cost: O(1), a few hash probes.
    pub(crate) fn attribute_meta_set_value(
        &self,
        meta: &InstanceAttrs,
        obj: &Value,
        new_val: &Value,
    ) -> Result<Value, RuntimeError> {
        let attr_name = meta_str(meta, "__mutsu_attr_name");
        // Write through a Scalar container wrapper: the shared
        // attribute cell of the underlying instance is what the
        // object's methods read (a silent no-op here loses e.g.
        // OO::Monitors' lock installation).
        let obj = obj.clone().deref_container();
        // An attribute a mixed-in role declares lives in the
        // mixin's own role cell, not in the wrapped object.
        if let ValueView::Mixin(_, mixins) = obj.view() {
            let owner = meta_str(meta, "__mutsu_attr_owner");
            let bare = attr_name.trim_start_matches(['$', '@', '%', '&', '!', '.']);
            if mixins.role_attribute(&owner, bare).is_some()
                && mixins.set_role_attribute(&owner, bare, new_val.clone())
            {
                return Ok(new_val.clone());
            }
        }
        let obj = match obj.view() {
            ValueView::Mixin(inner, _) => Value::clone(inner),
            _ => obj.clone(),
        };
        if let ValueView::Instance {
            attributes: obj_attrs,
            ..
        } = obj.view()
        {
            let key = attribute_meta_storage_key(meta, &obj_attrs, &attr_name);
            let stored = obj_attrs.set_attribute_value(key, new_val.clone());
            // An `is repr('CStruct')` object that owns a native body keeps the
            // field in C memory too (ADR-11209).
            self.cstruct_store_through(&obj_attrs, key, new_val);
            return Ok(stored);
        }
        Ok(Value::NIL)
    }
}
