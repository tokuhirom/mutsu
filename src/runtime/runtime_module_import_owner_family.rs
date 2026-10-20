//! A body-level `use` does not hide the routines its own package declares.

use super::*;

impl Interpreter {
    /// Whether `target_single` (`Pkg::name`) names a routine family that the
    /// package a role or class body imports into (`import_target_package`)
    /// declared itself.
    ///
    /// An imported proto hides the candidates visible at its target, because
    /// they belong to an outer scope. The routines a role or class body
    /// declares are in that body's own scope, so a `use` in the body does not
    /// hide them. `role CSS::Units { use Color; ... }` used to drop the
    /// `multi sub infix:<+> is export(:ops)` that the module's other
    /// `CSS::Units[...]` candidate had declared under the same package.
    /// `Color`'s imported `infix:<+>` proto removed that family from the
    /// registry, and importers of `:ops` got Color's candidates instead
    /// (#14161).
    // Cost: O(c), c = the family's candidates (the family index).
    pub(super) fn import_target_declares_family(&self, target_single: &str) -> bool {
        let Some(owner) = self.module.import_target_package.as_deref() else {
            return false;
        };
        let owner = Symbol::intern(owner);
        let registry = self.registry();
        let functions = &registry.functions;
        functions
            .family_keys(target_single)
            .iter()
            .chain(std::iter::once(&Symbol::intern(target_single)))
            .filter_map(|key| functions.get(key))
            .any(|def| def.package == owner)
    }
}
