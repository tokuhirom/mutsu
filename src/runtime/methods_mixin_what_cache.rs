//! Composition-keyed anonymous type object cache for a role-mixed value's
//! `.WHAT` (ADR-0060). Rakudo builds and permanently caches one anonymous
//! type object per (base type, role set, role type-arguments) composition —
//! not per instance, and not shared with the base type. `dispatch_what()`
//! (`methods_introspect.rs`) and the `.^set_name`/`.^name` handlers
//! (`methods_classhow_dispatch.rs`) both consult the same cache through the
//! helpers here so a rename made through either path is visible through the
//! other.

use super::*;

impl Interpreter {
    /// Build the `.WHAT` value for a role-mixed (`Mixin`) value: the base
    /// value's own `.WHAT`, wrapped with the shared, composition-keyed
    /// `overrides` node every value with the same composition uses (see
    /// [`Self::mixin_composition_overrides`]).
    pub(super) fn mixin_what_value(
        &mut self,
        inner: &Arc<Value>,
        mixins: &crate::value::MixinOverrides,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let base_what =
            self.call_method_with_values(inner.as_ref().clone(), "WHAT", args.to_vec())?;
        let base_type_name = base_what.to_string_value();
        let key = crate::value::types::mixin_composition_key(&base_type_name, mixins);
        let overrides = self.mixin_composition_overrides(key, mixins);
        Ok(Value::mixin_parts(Arc::new(base_what), overrides))
    }

    /// Get-or-create the shared `Gc<MixinOverrides>` node for a composition
    /// key. Every `.WHAT` call (and every `.^set_name`/`.^name` on a `Mixin`
    /// value, direct or via `.WHAT`) for the same composition resolves to
    /// this exact node, so an in-place write through
    /// [`crate::gc::gc_contents_mut`] on one alias is visible through every
    /// other — including instances constructed AFTER the write (verified
    /// against `raku`, see ADR-0060).
    ///
    /// On a cache miss, the new node is seeded with `mixins`' filtered
    /// composition markers ([`crate::value::types::filter_composition_markers`])
    /// rather than left empty: `.^name`/`what_type_name` on the `.WHAT`
    /// value itself then synthesizes the right `Base+{Role,...}` display
    /// straight from the shared node, and two different compositions get
    /// content-different (not merely differently-keyed) overrides maps.
    pub(super) fn mixin_composition_overrides(
        &self,
        key: String,
        mixins: &crate::value::MixinOverrides,
    ) -> crate::gc::Gc<crate::value::MixinOverrides> {
        self.registry_mut()
            .mixin_what_cache
            .entry(key)
            .or_insert_with(|| {
                crate::gc::Gc::new(crate::value::types::filter_composition_markers(mixins))
            })
            .clone()
    }

    /// Derive a `Mixin` instance's composition key and resolve it to the
    /// shared cache entry, computing the base type name the same way
    /// [`Self::mixin_what_value`] does (`inner`'s own `.WHAT`, stringified).
    /// Used by direct `$obj.^set_name(...)`/`.^name` on a mixin instance, so
    /// they read/write the same composition-keyed identity `.WHAT` uses
    /// rather than the instance's own (per-instance) `overrides` map.
    pub(super) fn mixin_instance_composition_overrides(
        &mut self,
        inner: &Arc<Value>,
        mixins: &crate::value::MixinOverrides,
    ) -> Result<crate::gc::Gc<crate::value::MixinOverrides>, RuntimeError> {
        let base_what = self.call_method_with_values(inner.as_ref().clone(), "WHAT", Vec::new())?;
        let base_type_name = base_what.to_string_value();
        let key = crate::value::types::mixin_composition_key(&base_type_name, mixins);
        Ok(self.mixin_composition_overrides(key, mixins))
    }

    /// Build the composition-keyed punned-class type object for a role
    /// (ADR-0060 naming) — the exact value `R.new.WHAT` produces for an
    /// instance of `role_name`'s pun, computed WITHOUT constructing an
    /// instance. `ensure_role_punned_to_class` registers the pun's
    /// `ClassDef` under the role's own name, so a bare `Value::package`
    /// would be ambiguous between "the role group" and "the punned class"
    /// (`.HOW` cannot tell them apart, `todo/tickets/role-pun-metamethod-
    /// returns-role-group.md`); wrapping it in the same `Mixin` shape the
    /// role's own instances carry (`__mutsu_role__{name}` /
    /// `__mutsu_role_id__{name}`, mirroring `mark_punned_role_instance`
    /// in `methods_object_dispatch_new.rs`) disambiguates it the same way
    /// ADR-0060 disambiguates any other role composition's `.WHAT`.
    ///
    /// Used by the `^pun` metamethod (`methods_classhow_dispatch.rs`) and by
    /// MRO emission (`methods_classhow_mro.rs`) for a level that is itself a
    /// punned role (`class C is SomeRole { }`), so `R.^pun === R.new.WHAT`
    /// and an MRO entry for a punned role `eqv`s `R.^pun`
    /// (`roast/6.c/S12-class/mro-6c.t`).
    pub(super) fn punned_role_type_object(
        &mut self,
        role_name: &str,
    ) -> Result<Value, RuntimeError> {
        self.ensure_role_punned_to_class(role_name)?;
        let mut mixins: crate::value::MixinOverrides = HashMap::new();
        mixins.insert(format!("__mutsu_role__{role_name}"), Value::TRUE);
        // Mirrors `mark_punned_role_instance`'s own role-id lookup so a
        // punned instance's `.WHAT` and `^pun`'s return value key to the
        // SAME cache entry (both omit the marker when a role carries no
        // minted id, e.g. mutsu's natively-modelled core roles).
        let role_id = self
            .registry()
            .roles
            .get(role_name)
            .map_or(0, |r| r.role_id);
        if role_id != 0 {
            mixins.insert(
                format!("__mutsu_role_id__{role_name}"),
                Value::int(role_id as i64),
            );
        }
        let base_what = Value::package(Symbol::intern(role_name));
        let base_type_name = base_what.to_string_value();
        let key = crate::value::types::mixin_composition_key(&base_type_name, &mixins);
        let overrides = self.mixin_composition_overrides(key, &mixins);
        Ok(Value::mixin_parts(Arc::new(base_what), overrides))
    }
}
