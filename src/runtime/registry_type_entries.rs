//! Copying one type's entries between two [`Registry`]s (ADR-17697).
//!
//! A type minted by `Metamodel::*HOW.new_type` on one thread is published
//! process-wide once it is composed, and a sibling thread that meets its name
//! copies its registry entries into its own copy-on-write registry
//! (`crate::runtime::published_minted_types`). A type's state is spread over
//! many class-keyed tables, and ADR-11456 rejected entry copying for mixins
//! because a copy that misses one table fails silently.
//! [`Registry::copy_type_entries`] answers that: it destructures [`Registry`]
//! exhaustively, so adding a field does not compile until it is classified
//! here as type-keyed (copied) or not (skipped, with the reason).
//!
//! The same routine serves both directions. Publication copies the minting
//! thread's entries into an otherwise empty bundle `Registry`; adoption copies
//! the bundle's entries into the adopting thread's registry, for the names that
//! registry does not know yet. An entry already present in the destination is
//! never overwritten.

use super::*;

/// The type names whose entries are copied, as strings and as symbols (the
/// tables are keyed either way).
pub(crate) struct TypeNames {
    strs: HashSet<String>,
    syms: HashSet<Symbol>,
    /// `class:<name>:` — the prefix of a [`Registry::composed_role_bodies`]
    /// key of one of the types.
    role_body_prefixes: Vec<String>,
    /// `<name>::&role:` — the prefix of a
    /// [`Registry::role_composition_scopes`] symbol of one of the types.
    composition_scope_prefixes: Vec<String>,
}

impl TypeNames {
    // Cost: O(n), n = total length of `names`.
    pub(crate) fn new<'a>(names: impl IntoIterator<Item = &'a str>) -> Self {
        let mut out = Self {
            strs: HashSet::default(),
            syms: HashSet::default(),
            role_body_prefixes: Vec::new(),
            composition_scope_prefixes: Vec::new(),
        };
        for name in names {
            if out.strs.insert(name.to_string()) {
                out.syms.insert(Symbol::intern(name));
                out.role_body_prefixes.push(format!("class:{name}:"));
                out.composition_scope_prefixes.push(format!("{name}::&role:"));
            }
        }
        out
    }

    // Cost: O(1).
    pub(crate) fn is_empty(&self) -> bool {
        self.strs.is_empty()
    }

    // Cost: O(n), n = the names.
    pub(crate) fn iter(&self) -> impl Iterator<Item = &String> {
        self.strs.iter()
    }
}

/// A key that belongs to one type: the type name itself, or a tuple whose
/// first component is the type name.
trait TypeKey: Clone + Eq + std::hash::Hash {
    /// The exact keys of `names`, when a key IS a type name; `None` when keys
    /// only start with one, and the table has to be scanned.
    fn exact(names: &TypeNames) -> Option<Vec<Self>>;
    /// Whether this key belongs to one of `names`.
    fn owned_by(&self, names: &TypeNames) -> bool;
}

impl TypeKey for String {
    // Cost: O(n), n = the names.
    fn exact(names: &TypeNames) -> Option<Vec<Self>> {
        Some(names.strs.iter().cloned().collect())
    }
    // Cost: O(|self|).
    fn owned_by(&self, names: &TypeNames) -> bool {
        names.strs.contains(self)
    }
}

impl TypeKey for Symbol {
    // Cost: O(n), n = the names.
    fn exact(names: &TypeNames) -> Option<Vec<Self>> {
        Some(names.syms.iter().copied().collect())
    }
    // Cost: O(1).
    fn owned_by(&self, names: &TypeNames) -> bool {
        names.syms.contains(self)
    }
}

impl<B: Clone + Eq + std::hash::Hash> TypeKey for (String, B) {
    // Cost: O(1).
    fn exact(_: &TypeNames) -> Option<Vec<Self>> {
        None
    }
    // Cost: O(|self.0|).
    fn owned_by(&self, names: &TypeNames) -> bool {
        names.strs.contains(&self.0)
    }
}

impl TypeKey for (String, String, usize) {
    // Cost: O(1).
    fn exact(_: &TypeNames) -> Option<Vec<Self>> {
        None
    }
    // Cost: O(|self.0|).
    fn owned_by(&self, names: &TypeNames) -> bool {
        names.strs.contains(&self.0)
    }
}

impl TypeKey for (Symbol, Symbol) {
    // Cost: O(1).
    fn exact(_: &TypeNames) -> Option<Vec<Self>> {
        None
    }
    // Cost: O(1).
    fn owned_by(&self, names: &TypeNames) -> bool {
        names.syms.contains(&self.0)
    }
}

/// A table some of whose entries belong to types.
trait TypeTable {
    type Item;
    /// The entries of `names`.
    fn entries_of(&self, names: &TypeNames) -> Vec<Self::Item>;
    /// Insert `items`, keeping any entry already present.
    fn insert_absent(&mut self, items: Vec<Self::Item>);
}

impl<K: TypeKey, V: Clone, S: std::hash::BuildHasher> TypeTable
    for std::collections::HashMap<K, V, S>
{
    type Item = (K, V);
    // Cost: O(n) for a table keyed by type name, n = the names; O(t) for a
    // table keyed by `(type, ...)`, t = table size.
    fn entries_of(&self, names: &TypeNames) -> Vec<(K, V)> {
        match K::exact(names) {
            Some(keys) => keys
                .into_iter()
                .filter_map(|k| self.get(&k).map(|v| (k, v.clone())))
                .collect(),
            None => self
                .iter()
                .filter(|(k, _)| k.owned_by(names))
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect(),
        }
    }
    // Cost: O(i), i = items.
    fn insert_absent(&mut self, items: Vec<(K, V)>) {
        for (k, v) in items {
            self.entry(k).or_insert(v);
        }
    }
}

impl<K: TypeKey, S: std::hash::BuildHasher> TypeTable for std::collections::HashSet<K, S> {
    type Item = K;
    // Cost: as for a map.
    fn entries_of(&self, names: &TypeNames) -> Vec<K> {
        match K::exact(names) {
            Some(keys) => keys.into_iter().filter(|k| self.contains(k)).collect(),
            None => self.iter().filter(|k| k.owned_by(names)).cloned().collect(),
        }
    }
    // Cost: O(i), i = items.
    fn insert_absent(&mut self, items: Vec<K>) {
        self.extend(items);
    }
}

impl<T: TypeTable + Clone> TypeTable for CowTable<T> {
    type Item = T::Item;
    // Cost: as for the inner table.
    fn entries_of(&self, names: &TypeNames) -> Vec<T::Item> {
        (**self).entries_of(names)
    }
    // Cost: as for the inner table, plus the copy-on-write clone when shared.
    fn insert_absent(&mut self, items: Vec<T::Item>) {
        (**self).insert_absent(items);
    }
}

impl<T: TypeTable + Clone> TypeTable for Arc<T> {
    type Item = T::Item;
    // Cost: as for the inner table.
    fn entries_of(&self, names: &TypeNames) -> Vec<T::Item> {
        (**self).entries_of(names)
    }
    // Cost: as for the inner table, plus the copy-on-write clone when shared.
    fn insert_absent(&mut self, items: Vec<T::Item>) {
        Arc::make_mut(self).insert_absent(items);
    }
}

impl<V: Clone> TypeTable for super::super::class_attr_table::ClassAttrTable<V> {
    type Item = (String, String, V);
    // Cost: O(a), a = the attribute entries of the names.
    fn entries_of(&self, names: &TypeNames) -> Vec<(String, String, V)> {
        names
            .strs
            .iter()
            .flat_map(|class| {
                self.class_entries(class).into_iter().flat_map(move |attrs| {
                    attrs
                        .iter()
                        .map(move |(attr, v)| (class.clone(), attr.clone(), v.clone()))
                })
            })
            .collect()
    }
    // Cost: O(i), i = items.
    fn insert_absent(&mut self, items: Vec<(String, String, V)>) {
        for (class, attr, v) in items {
            self.insert_if_absent(&class, &attr, v);
        }
    }
}

/// Copy `src`'s entries of `names` in `table` into `dst`'s; touches `dst`
/// (and so a copy-on-write table) only when there is something to copy.
// Cost: that of `entries_of` + `insert_absent`.
fn copy<T: TypeTable>(src: &T, dst: &mut T, names: &TypeNames) {
    let items = src.entries_of(names);
    if !items.is_empty() {
        dst.insert_absent(items);
    }
}

/// Copy every entry of a set that is a whole-program prefilter (a stale
/// member costs one slow-path check, a missing one a wrong answer).
// Cost: O(s), s = the source set's size.
fn union<T: Clone + Eq + std::hash::Hash, S: std::hash::BuildHasher>(
    src: &std::collections::HashSet<T, S>,
    dst: &mut std::collections::HashSet<T, S>,
) {
    if src.iter().any(|m| !dst.contains(m)) {
        dst.extend(src.iter().cloned());
    }
}

impl Registry {
    /// Copy into `dst` every entry of `src` that belongs to one of `names`,
    /// keeping the entries `dst` already has. See the module documentation.
    ///
    /// The destructuring below is deliberately exhaustive: a new `Registry`
    /// field must be classified here before the crate compiles again.
    // Cost: O(n + t), n = the names, t = the total size of the tables keyed by
    // `(type, member)` tuples, which are scanned.
    pub(crate) fn copy_type_entries(src: &Registry, dst: &mut Registry, names: &TypeNames) {
        let Registry {
            // A cache, re-verified against `classes` on every hit.
            mixin_display_index: _,
            who_overrides,
            frozen_dispatchers,
            // Routine-local declarations and per-unit pragmas: not type state.
            routine_local_subs: _,
            routine_local_subsets: _,
            monkey_eval_units: _,
            method_entries,
            method_value_prototypes,
            // Bumped once the copy is done (`bump_method_generation`).
            method_generation: _,
            any_raw_invocant_method,
            any_container_binding_method_param,
            owner_method_names,
            owner_accessor_names,
            method_wrap_chains,
            method_renames,
            dispatcher_wrapped_methods,
            candidate_wrapped_methods,
            enum_types,
            how_enums,
            subsets,
            classes,
            cunion_classes,
            cstruct_classes,
            cppstruct_classes,
            cpointer_classes,
            carray_classes,
            uninstantiable_classes,
            nativecall_classes,
            cstr_classes,
            box_target_attrs,
            native_decls,
            array_types,
            role_array_type_args,
            // Keyed by short name and filled only by an `is repr('VMArray')`
            // / `is repr('VMHash')` declaration, which a minted type is not.
            vmarray_classes: _,
            async_repr_classes,
            vmhash_classes: _,
            hidden_classes,
            lexical_classes,
            // The re-entry stamp of a static declaration site.
            class_site_stamps: _,
            namespaced_lexical_types,
            classes_with_method_type_decls,
            compound_declared_types,
            compound_name_overrides,
            compound_identity_bindings,
            compound_binding_identities,
            class_stubs,
            package_stubs,
            // Diagnostics already reported on the creating thread.
            reported_stub_errors: _,
            package_kinds,
            hidden_defer_parents,
            class_trusts,
            class_how_values,
            // In-flight declaration state of the creating thread.
            roles_in_trait_dispatch: _,
            declared_native_how,
            grammar_custom_how,
            class_find_method_how,
            // In-flight declaration state of the creating thread.
            pending_class_compose: _,
            composed_classes,
            minted_type_names,
            // In-flight: cleared for a type when it composes.
            pending_multi_methods: _,
            attr_trait_composed,
            declaring_role_ids,
            // In-flight declaration state of the creating thread.
            pending_role_specialize: _,
            has_metamodel_how_classes,
            class_composed_roles,
            class_direct_composed_roles,
            class_does_only_roles,
            class_enum_roles,
            class_subs,
            role_composition_scopes,
            buildplans,
            attribute_build_overrides,
            class_attribute_defaults,
            role_attribute_default_exprs,
            role_class_level_attrs,
            class_level_attr_names,
            class_attribute_default_exprs,
            declare_attribute_classes,
            class_attribute_is_types,
            role_attribute_is_types,
            role_attribute_types,
            role_attribute_smileys,
            class_attribute_does_roles,
            class_attribute_container_mixins,
            class_attribute_deprecated,
            class_attribute_trait_objects,
            role_attribute_pending_traits,
            role_attribute_trait_fingerprints,
            // Keyed by a parametric role declaration's id, which every fork
            // shares from its snapshot.
            param_role_attribute_traits: _,
            role_registered_sub_protos,
            role_evaluated_constants,
            roles,
            user_declared_roles,
            role_candidates,
            role_parents,
            role_hides,
            role_type_params,
            role_body_blocks,
            composed_role_bodies,
            // Keyed by a declaration's id, which every fork shares.
            nested_shell_traits_applied: _,
            class_role_param_bindings,
            // A cache of `Gc` handles owned by the creating thread.
            mixin_what_cache: _,
            // Subs, tokens and their protos are package-scoped routines, not
            // a type's state: `new_type` and the MOP calls on its result
            // never install one.
            functions: _,
            fn_transitions: _,
            our_scoped_restore_memo: _,
            module_functions_present: _,
            our_scoped_functions: _,
            proto_functions: _,
            token_defs: _,
            proto_subs: _,
            proto_gen: _,
            proto_tokens: _,
            proto_token_params: _,
            has_proto_methods,
            proto_method_names,
            // A compile cache, refilled on demand.
            proto_compiled_cache: _,
            // Derived caches, dropped by the write guard (`DerefMut`).
            lexical_type_names: _,
            class_short_names: _,
            lexical_key_scans: _,
            type_stamp: _,
        } = src;

        // The method table: the user and accessor columns of each type's rows,
        // found through the per-owner name indices instead of a scan.
        let mut rows = Vec::new();
        for owner in &names.syms {
            let user = owner_method_names.get(owner).into_iter().flatten();
            let acc = owner_accessor_names.get(owner).into_iter().flatten();
            for name in user.chain(acc) {
                let key = MethodEntryKey {
                    owner: *owner,
                    name: *name,
                };
                if let Some(entry) = method_entries.get(&key) {
                    rows.push((key, entry.clone()));
                }
            }
        }
        if !rows.is_empty() {
            let table = &mut *dst.method_entries;
            for (key, entry) in rows {
                table.entry(key).or_insert(entry);
            }
        }
        copy(owner_method_names, &mut dst.owner_method_names, names);
        copy(owner_accessor_names, &mut dst.owner_accessor_names, names);
        copy(method_value_prototypes, &mut dst.method_value_prototypes, names);
        dst.any_raw_invocant_method |= *any_raw_invocant_method;
        dst.any_container_binding_method_param |= *any_container_binding_method_param;
        dst.has_metamodel_how_classes |= *has_metamodel_how_classes;
        dst.has_proto_methods |= *has_proto_methods;
        union(dispatcher_wrapped_methods, &mut dst.dispatcher_wrapped_methods);
        union(candidate_wrapped_methods, &mut dst.candidate_wrapped_methods);
        union(class_level_attr_names, &mut dst.class_level_attr_names);
        union(proto_method_names, &mut dst.proto_method_names);

        copy(who_overrides, &mut dst.who_overrides, names);
        copy(frozen_dispatchers, &mut dst.frozen_dispatchers, names);
        copy(method_wrap_chains, &mut dst.method_wrap_chains, names);
        copy(method_renames, &mut dst.method_renames, names);
        copy(enum_types, &mut dst.enum_types, names);
        copy(how_enums, &mut dst.how_enums, names);
        copy(subsets, &mut dst.subsets, names);
        copy(classes, &mut dst.classes, names);
        copy(cunion_classes, &mut dst.cunion_classes, names);
        copy(cstruct_classes, &mut dst.cstruct_classes, names);
        copy(cppstruct_classes, &mut dst.cppstruct_classes, names);
        copy(cpointer_classes, &mut dst.cpointer_classes, names);
        copy(carray_classes, &mut dst.carray_classes, names);
        copy(uninstantiable_classes, &mut dst.uninstantiable_classes, names);
        copy(nativecall_classes, &mut dst.nativecall_classes, names);
        copy(cstr_classes, &mut dst.cstr_classes, names);
        copy(box_target_attrs, &mut dst.box_target_attrs, names);
        copy(native_decls, &mut dst.native_decls, names);
        copy(array_types, &mut dst.array_types, names);
        copy(role_array_type_args, &mut dst.role_array_type_args, names);
        copy(async_repr_classes, &mut dst.async_repr_classes, names);
        copy(hidden_classes, &mut dst.hidden_classes, names);
        copy(lexical_classes, &mut dst.lexical_classes, names);
        copy(namespaced_lexical_types, &mut dst.namespaced_lexical_types, names);
        copy(
            classes_with_method_type_decls,
            &mut dst.classes_with_method_type_decls,
            names,
        );
        copy(compound_declared_types, &mut dst.compound_declared_types, names);
        copy(compound_name_overrides, &mut dst.compound_name_overrides, names);
        copy(compound_identity_bindings, &mut dst.compound_identity_bindings, names);
        copy(compound_binding_identities, &mut dst.compound_binding_identities, names);
        copy(class_stubs, &mut dst.class_stubs, names);
        copy(package_stubs, &mut dst.package_stubs, names);
        copy(package_kinds, &mut dst.package_kinds, names);
        copy(hidden_defer_parents, &mut dst.hidden_defer_parents, names);
        copy(class_trusts, &mut dst.class_trusts, names);
        copy(class_how_values, &mut dst.class_how_values, names);
        copy(declared_native_how, &mut dst.declared_native_how, names);
        copy(grammar_custom_how, &mut dst.grammar_custom_how, names);
        copy(class_find_method_how, &mut dst.class_find_method_how, names);
        copy(composed_classes, &mut dst.composed_classes, names);
        copy(minted_type_names, &mut dst.minted_type_names, names);
        copy(attr_trait_composed, &mut dst.attr_trait_composed, names);
        copy(declaring_role_ids, &mut dst.declaring_role_ids, names);
        copy(class_composed_roles, &mut dst.class_composed_roles, names);
        copy(class_direct_composed_roles, &mut dst.class_direct_composed_roles, names);
        copy(class_does_only_roles, &mut dst.class_does_only_roles, names);
        copy(class_enum_roles, &mut dst.class_enum_roles, names);
        copy(class_subs, &mut dst.class_subs, names);
        copy(buildplans, &mut dst.buildplans, names);
        copy(attribute_build_overrides, &mut dst.attribute_build_overrides, names);
        copy(class_attribute_defaults, &mut dst.class_attribute_defaults, names);
        copy(role_attribute_default_exprs, &mut dst.role_attribute_default_exprs, names);
        copy(role_class_level_attrs, &mut dst.role_class_level_attrs, names);
        copy(class_attribute_default_exprs, &mut dst.class_attribute_default_exprs, names);
        copy(declare_attribute_classes, &mut dst.declare_attribute_classes, names);
        copy(class_attribute_is_types, &mut dst.class_attribute_is_types, names);
        copy(role_attribute_is_types, &mut dst.role_attribute_is_types, names);
        copy(role_attribute_types, &mut dst.role_attribute_types, names);
        copy(role_attribute_smileys, &mut dst.role_attribute_smileys, names);
        copy(class_attribute_does_roles, &mut dst.class_attribute_does_roles, names);
        copy(
            class_attribute_container_mixins,
            &mut dst.class_attribute_container_mixins,
            names,
        );
        copy(class_attribute_deprecated, &mut dst.class_attribute_deprecated, names);
        copy(class_attribute_trait_objects, &mut dst.class_attribute_trait_objects, names);
        copy(role_attribute_pending_traits, &mut dst.role_attribute_pending_traits, names);
        copy(
            role_attribute_trait_fingerprints,
            &mut dst.role_attribute_trait_fingerprints,
            names,
        );
        copy(role_registered_sub_protos, &mut dst.role_registered_sub_protos, names);
        copy(role_evaluated_constants, &mut dst.role_evaluated_constants, names);
        copy(roles, &mut dst.roles, names);
        copy(user_declared_roles, &mut dst.user_declared_roles, names);
        copy(role_candidates, &mut dst.role_candidates, names);
        copy(role_parents, &mut dst.role_parents, names);
        copy(role_hides, &mut dst.role_hides, names);
        copy(role_type_params, &mut dst.role_type_params, names);
        copy(role_body_blocks, &mut dst.role_body_blocks, names);
        copy(class_role_param_bindings, &mut dst.class_role_param_bindings, names);

        // Keys that embed a type name: `class:<type>:<role>` and
        // `<type>::&role:<role>`.
        let bodies: Vec<String> = composed_role_bodies
            .iter()
            .filter(|k| names.role_body_prefixes.iter().any(|p| k.starts_with(p.as_str())))
            .cloned()
            .collect();
        dst.composed_role_bodies.extend(bodies);
        let scopes: Vec<Symbol> = role_composition_scopes
            .iter()
            .filter(|s| {
                let s = s.as_str();
                names
                    .composition_scope_prefixes
                    .iter()
                    .any(|p| s.starts_with(p.as_str()))
            })
            .copied()
            .collect();
        dst.role_composition_scopes.extend(scopes);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn copies_only_the_named_types_and_keeps_existing_entries() {
        let mut src = Registry::default();
        src.classes.insert("M".into(), Default::default());
        src.classes.insert("Other".into(), Default::default());
        src.composed_classes.insert("M".into());
        src.declared_native_how
            .insert("M".into(), "Perl6::Metamodel::ClassHOW".into());
        src.class_attribute_is_types
            .insert(("M".into(), "$!a".into()), "rw".into());
        src.class_attribute_is_types
            .insert(("Other".into(), "$!a".into()), "rw".into());
        src.composed_role_bodies.insert("class:M:R".into());
        src.composed_role_bodies.insert("class:Other:R".into());

        let mut dst = Registry::default();
        dst.declared_native_how
            .insert("M".into(), "Perl6::Metamodel::Kept".into());
        Registry::copy_type_entries(&src, &mut dst, &TypeNames::new(["M"]));

        assert!(dst.classes.contains_key("M"));
        assert!(!dst.classes.contains_key("Other"));
        assert!(dst.composed_classes.contains("M"));
        assert_eq!(dst.declared_native_how["M"], "Perl6::Metamodel::Kept");
        assert!(
            dst.class_attribute_is_types
                .contains_key(&("M".to_string(), "$!a".to_string()))
        );
        assert_eq!(dst.class_attribute_is_types.len(), 1);
        assert!(dst.composed_role_bodies.contains("class:M:R"));
        assert!(!dst.composed_role_bodies.contains("class:Other:R"));
    }
}
