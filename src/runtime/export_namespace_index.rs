//! Generated export stashes are unit symbols, not user package declarations.

use super::*;

impl Interpreter {
    // Cost: O(n + d), n = namespace name bytes, d = EVAL ancestry (at most 64).
    pub(crate) fn export_namespace_exists(&self, name: &str) -> bool {
        let name = Symbol::intern(Self::strip_pseudo_packages(name));
        if self.module.package_export_namespaces.contains(&name) {
            return true;
        }
        for anchor in [self.executing_unit_sym_for_module_load(), self.current_unit] {
            let mut unit = Some(anchor);
            for _ in 0..64 {
                let Some(owner) = unit else { break };
                if self.module.unit_export_namespaces.contains(&(owner, name)) {
                    return true;
                }
                unit = crate::runtime::eval_unit_parent(owner);
            }
        }
        false
    }

    // Cost: O(t * n), t = export tags, n = package and tag name bytes.
    pub(crate) fn declare_export_namespaces(&mut self, package: &str, tags: &[String]) {
        let unit = self.executing_unit_sym_for_module_load();
        let owner = Symbol::intern(package);
        let export = Symbol::intern("EXPORT");
        self.record_export_namespace(unit, owner, export);
        self.record_export_namespace(
            unit,
            owner,
            crate::qualified::qualified(export, Symbol::intern("ALL")),
        );
        if tags.is_empty() {
            self.record_export_namespace(
                unit,
                owner,
                crate::qualified::qualified(export, Symbol::intern("DEFAULT")),
            );
        }
        for tag in tags {
            self.record_export_namespace(
                unit,
                owner,
                crate::qualified::qualified(export, Symbol::intern(tag)),
            );
        }
    }

    fn record_export_namespace(&mut self, unit: Symbol, owner: Symbol, name: Symbol) {
        if !self.module.unit_export_namespaces.contains(&(unit, name)) {
            cow_table_mut(&mut self.module.unit_export_namespaces).insert((unit, name));
        }
        if !crate::qualified::is_global_package(owner) {
            let public = crate::qualified::qualified(owner, name);
            if !self.module.package_export_namespaces.contains(&public) {
                cow_table_mut(&mut self.module.package_export_namespaces).insert(public);
            }
        }
    }
}
