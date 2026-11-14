//! Publishing a composed `new_type`-minted type, and adopting one another
//! thread published (#17697, ADR-17697). The table and its invariants are
//! [`crate::runtime::published_minted_types`]; the per-table copy is
//! [`crate::runtime::registry::Registry::copy_type_entries`].

use super::*;
use crate::runtime::published_minted_types::MintedTypeEntries;
use crate::runtime::registry::type_entries::TypeNames;

impl Interpreter {
    /// Publish the just-composed type `name` when another thread could meet
    /// it without having it in its registry: it was minted by `new_type`, and
    /// a thread has been forked from this interpreter's lineage.
    // Cost: O(1) unless publishing; then O(n + t), n = the type's MRO and
    // composed roles, t = the size of the registry tables keyed by
    // `(type, member)` tuples (see `Registry::copy_type_entries`).
    pub(crate) fn publish_composed_minted_type(&mut self, name: &str) {
        if !self.types.published_minted_types.publishing() {
            return;
        }
        let minted = crate::runtime::metamodel_new_type::is_minted_type_name(name)
            || self.registry().minted_type_names.contains(name);
        if !minted || self.types.published_minted_types.contains(name) {
            return;
        }
        // The type, and what it was built from: a parent or role the minting
        // thread created at run time (a role specialization, another minted
        // type) is missing on the sibling too.
        let mut names: Vec<String> = vec![name.to_string()];
        for parent in self.class_mro(name).iter() {
            if !names.iter().any(|n| n == parent.as_str()) {
                names.push(parent.to_string());
            }
        }
        if let Some(roles) = self.registry().class_composed_roles.get(name) {
            for role in roles {
                if !names.contains(role) {
                    names.push(role.clone());
                }
            }
        }
        let type_names = TypeNames::new(names.iter().map(String::as_str));
        let mut registry = Registry::default();
        Registry::copy_type_entries(&self.registry(), &mut registry, &type_names);
        let method_fallbacks = names
            .iter()
            .filter_map(|n| {
                self.types
                    .method_fallbacks
                    .get(n)
                    .map(|handlers| (n.clone(), handlers.clone()))
            })
            .collect();
        let package_lexicals = names
            .iter()
            .filter_map(|n| {
                self.lexicals.package_lexicals.get(n).map(|store| {
                    let entries = store.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                    (n.clone(), entries)
                })
            })
            .collect();
        self.types.published_minted_types.publish(
            name,
            MintedTypeEntries {
                names,
                registry,
                method_fallbacks,
                package_lexicals,
            },
        );
    }

    /// Copy the minted type `name` into this interpreter's registry when
    /// another thread published it and this registry does not have it.
    /// Returns whether the type is registered afterwards. Runs no user code.
    // Cost: O(1) when nothing was ever published; O(|name|) when the name is
    // registered or unpublished; otherwise O(n + t) as for
    // `publish_composed_minted_type`.
    pub(crate) fn adopt_published_minted_type(&mut self, name: &str) -> bool {
        if !self.types.published_minted_types.may_hold() {
            return false;
        }
        if self.registry().classes.contains_key(name) {
            return false;
        }
        let Some(entries) = self
            .types
            .published_minted_types
            .lookup(name, &mut self.types.minted_type_misses)
        else {
            return false;
        };
        // Only the names this thread lacks: a parent or role it already has is
        // its own declaration and stays as it is.
        let missing: Vec<&str> = {
            let registry = self.registry();
            entries
                .names
                .iter()
                .map(String::as_str)
                .filter(|n| !registry.classes.contains_key(*n) && !registry.roles.contains_key(*n))
                .collect()
        };
        let type_names = TypeNames::new(missing.iter().copied());
        if type_names.is_empty() {
            return false;
        }
        {
            let mut registry = self.registry_mut();
            Registry::copy_type_entries(&entries.registry, &mut registry, &type_names);
            registry.bump_method_generation();
        }
        let fallbacks: Vec<_> = entries
            .method_fallbacks
            .iter()
            .filter(|(n, _)| type_names.iter().any(|m| m == n))
            .collect();
        if !fallbacks.is_empty() {
            let table = crate::runtime::cow_table_mut(&mut self.types.method_fallbacks);
            for (n, handlers) in fallbacks {
                table.entry(n.clone()).or_insert_with(|| handlers.clone());
            }
        }
        for (n, store) in &entries.package_lexicals {
            if !type_names.iter().any(|m| m == n) {
                continue;
            }
            let dst = self.package_lexicals_cow_mut().entry(n.clone()).or_default();
            for (bare, value) in store {
                if dst.get(bare).is_none() {
                    dst.insert(bare.clone(), value.clone());
                }
            }
        }
        self.caches.native_ctor_plan_cache.clear();
        self.caches.native_ctor_plan_builtin.clear();
        self.registry().classes.contains_key(name)
    }
}
