//! Namespace owners of a qualified package declaration.

use super::*;

impl Interpreter {
    // Cost: O(n), n = declaration name bytes; two hash operations.
    pub(super) fn record_compound_type_binding(&mut self, identity: Symbol, written: Symbol) {
        let binding = Symbol::intern(Self::strip_pseudo_packages(written.as_str()));
        if crate::qualified::is_qualified(binding) && identity != binding {
            if self.registry().compound_identity_bindings.get(&identity) != Some(&binding) {
                self.registry_mut()
                    .compound_identity_bindings
                    .insert(identity, binding);
            }
        } else if self
            .registry()
            .compound_identity_bindings
            .contains_key(&identity)
        {
            self.registry_mut()
                .compound_identity_bindings
                .remove(&identity);
        }
    }

    /// A qualified declaration also declares its enclosing package objects.
    /// Existing types and scalar bindings keep their identities.
    // Cost: O(d + n), d = ancestor packages, n = their total name bytes.
    pub(super) fn declare_package_ancestors(&mut self, name: Symbol) {
        if let Some(parent) = crate::qualified::package_parent(name) {
            self.declare_package_namespace(parent);
        }
    }

    // Cost: O(d + n), d = namespace and ancestor packages, n = their total name bytes.
    pub(crate) fn declare_package_namespace(&mut self, name: Symbol) {
        for parent in crate::qualified::package_ancestors(name) {
            if crate::qualified::is_global_package(parent) {
                continue;
            }
            if self.is_known_package(parent.as_str()) {
                if self.module.module_load_stack.is_empty() {
                    self.release_foreign_provenance(parent.as_str());
                }
                continue;
            }
            self.registry_mut()
                .package_kinds
                .insert(parent.as_str().to_owned(), crate::ast::PackageKind::Package);
            if !self.module.module_load_stack.is_empty() {
                let unit = self.executing_unit_sym_for_module_load();
                crate::runtime::cow_table_mut(&mut self.module.package_declaring_units)
                    .entry(parent.as_str().to_owned())
                    .or_insert(unit);
            }
            if !self.env().contains_key_sym(parent) {
                self.env_mut().insert_sym(parent, Value::package(parent));
            }
            crate::runtime::cow_table_mut(&mut self.module.chain_declared_packages)
                .insert(parent.as_str().to_owned());
        }
    }

    // Cost: O(t * (d + n)), t = export tags, d = package depth, n = ancestor name bytes.
    pub(crate) fn declare_export_namespaces(&mut self, package: &str, tags: &[String]) {
        let owner = Symbol::intern(package);
        let exports = if crate::qualified::is_global_package(owner) {
            Symbol::intern("EXPORT")
        } else {
            crate::qualified::qualified(owner, Symbol::intern("EXPORT"))
        };
        let all = crate::qualified::qualified(exports, Symbol::intern("ALL"));
        self.declare_package_namespace(all);
        if tags.is_empty() {
            self.declare_package_namespace(crate::qualified::qualified(
                exports,
                Symbol::intern("DEFAULT"),
            ));
        }
        for tag in tags {
            self.declare_package_namespace(crate::qualified::qualified(
                exports,
                Symbol::intern(tag),
            ));
        }
    }
}
