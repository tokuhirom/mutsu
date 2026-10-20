//! Namespace owners of a qualified package declaration.

use super::*;

impl Interpreter {
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
            if crate::qualified::is_global_package(parent) || self.is_known_package(parent.as_str())
            {
                continue;
            }
            self.registry_mut()
                .package_kinds
                .insert(parent.as_str().to_owned(), crate::ast::PackageKind::Package);
            if !self.env().contains_key_sym(parent) {
                self.env_mut().insert_sym(parent, Value::package(parent));
            }
            crate::runtime::cow_table_mut(&mut self.module.chain_declared_packages)
                .insert(parent.as_str().to_owned());
        }
    }

    // Cost: O(t * (d + n)), t = export tags, d = package depth, n = ancestor name bytes.
    pub(crate) fn declare_export_namespaces(&mut self, package: &str, tags: &[String]) {
        let exports =
            crate::qualified::qualified(Symbol::intern(package), Symbol::intern("EXPORT"));
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
