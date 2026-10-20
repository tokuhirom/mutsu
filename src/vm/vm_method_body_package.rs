//! The package a method body runs in.

use crate::runtime::Interpreter;
use crate::symbol::Symbol;

impl Interpreter {
    /// The package to make current while the body of `method_def`, declared
    /// by `owner_class`, runs; `None` keeps the caller's.
    ///
    /// Keyed on the owner (the class or role that lexically declares the
    /// body), not on the receiver's dynamic class: a method's lexical scope is
    /// fixed by where it is written. Using the receiver broke an inherited
    /// method whose own body declares a recursive nested `sub` (#9008).
    ///
    /// A mixin type (`C+{M::R}`, made by `$obj does M::R`) is declared
    /// nowhere: its methods come from the roles it composes, so a role method
    /// runs in its role's package, as `$?PACKAGE` says. The `::` inside the
    /// braces is no package nesting; anchoring the package to the mixin type
    /// registered a `sub` the method declares where a `samewith`, run from the
    /// role's package, could not find it (AttrX::Mooish's `set-option`).
    // Cost: O(1) amortized: memoized registry probes.
    pub(crate) fn method_body_package(
        &mut self,
        owner_class: &str,
        owner_sym: Symbol,
        method_def: &crate::runtime::MethodDef,
    ) -> Option<Symbol> {
        // A role method composed into a class that shadows one of the role
        // body's subs runs in the role's composition scope, where it finds the
        // role's sub first and the class's package next (#14177).
        if let Some(role) = method_def.role_origin.as_deref()
            && role != owner_class
            && !self.registry().role_composition_scopes.is_empty()
        {
            let scope = crate::runtime::registration_role_composition_scope::role_composition_scope(
                owner_sym,
                Symbol::intern(role),
            );
            if self.registry().role_composition_scopes.contains(&scope) {
                return Some(scope);
            }
        }
        if self.types.published_mixins.may_hold(owner_class) {
            return method_def
                .original_role
                .as_deref()
                .or(method_def.role_origin.as_deref())
                .map(Symbol::intern);
        }
        // The class body declared subs, method-local types or `my` statics,
        // which a method read resolves through the owner's package; or the
        // class is declared inside a package (`class Searcher` inside `unit
        // module NL` registers as `NL::Searcher`), or it is a flat role/class
        // whose own deferred `use` imported a routine under its bare name
        // (#8646 shape 1) -- either way, bare-name lookup walks outwards from
        // the owner (`bare_name_packages`).
        (self.has_class_scoped_subs(owner_class)
            || self.class_has_method_type_decls(owner_class)
            || self.class_has_package_lexicals(owner_class)
            || crate::qualified::is_qualified(owner_sym)
            || self.package_has_deferred_use_imports(owner_class))
        .then_some(owner_sym)
    }
}
