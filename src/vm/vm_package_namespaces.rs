//! Namespace owners of a qualified package declaration.

use super::*;

impl Interpreter {
    // Cost: O(d + n), d = declaring package depth, n = declaration name bytes.
    pub(super) fn record_compound_type_binding(&mut self, identity: Symbol, written: Symbol) {
        let written = Symbol::intern(Self::strip_pseudo_packages(written.as_str()));
        let binding = if let Some((head, tail)) = crate::qualified::split_first(written) {
            let (head, tail) = (Symbol::intern(head), Symbol::intern(tail));
            let value = self
                .lexical_type_binding_sym(head)
                .or_else(|| self.term_binding_sym(head));
            let owner = match value.as_ref().map(Value::view) {
                Some(ValueView::Package(owner)) => Some(owner),
                _ if self.is_known_package(head.as_str())
                    || crate::core_type_names::contains(head.as_str()) =>
                {
                    Some(head)
                }
                _ => self
                    .resolve_bareword_type_name(head.as_str())
                    .map(|name| Symbol::intern(&name)),
            };
            owner
                .map(|owner| crate::qualified::qualified(owner, tail))
                .unwrap_or_else(|| {
                    if self.current_package_is_global() {
                        written
                    } else {
                        crate::qualified::qualified(self.current_package_sym(), written)
                    }
                })
        } else {
            identity
        };
        if crate::qualified::is_qualified(written) && identity != written && identity != binding {
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
            if self.is_known_package(parent.as_str())
                || self.types.user_declared_classes.contains(parent.as_str())
            {
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
}
