//! Routines the main program declares under a core routine's name (#17433).
//!
//! A `sub item` in a script is lexical to the script: rakudo resolves a module's
//! own `item(...)` against the setting, whatever the program that loaded it
//! declares. mutsu registers a script's package-less routine under the shared
//! `GLOBAL::name` key the module's by-name call also reaches, so a test file's
//! `sub item` replaced the `item` that `Test.rakumod`'s `_push_vars` calls, and
//! a block's `sub any` the `any` of a module routine it called.
//!
//! The main unit's routine stays registered (the program's own calls, `&name`,
//! introspection read it); it is only hidden from code running in another
//! compunit, through the same visibility gate ADR-11136 uses for a module's
//! `our sub` ([`Interpreter::module_routine_visible_here`]). Only names a core
//! routine also carries are recorded: for any other name a module's by-name
//! call finds nothing in the setting either, and the unit-keyed treatment costs
//! the name-keyed caches their hit.

use super::*;

impl Interpreter {
    /// Note that `package::name` was just installed: a package-less routine the
    /// main program declares under a core routine's name is the program's own.
    // Cost: O(1) (two hash probes) unless `name` is a core routine of the main
    // unit; then a copy-on-write insert into a small set.
    pub(crate) fn note_main_unit_routine(&mut self, package: &str, name: &str) {
        if !crate::qualified::is_global_name(package)
            || crate::qualified::is_qualified_str(name)
            || !(crate::builtin_functions::is_builtin_function(name)
                || !crate::builtins::rakudo_candidates::core_sub_candidates(name).is_empty())
            || !self.is_main_lineage(self.executing_unit_sym())
        {
            return;
        }
        let sym = Symbol::intern(name);
        if self
            .module
            .module_visibility
            .main_shadowing_routines
            .contains(&sym)
        {
            return;
        }
        crate::runtime::cow_table_mut(&mut self.module.module_visibility.main_shadowing_routines)
            .insert(sym);
        self.invalidate_fn_resolution();
    }

    /// Whether the registry routine under `key` is visible to the code running
    /// right now as far as the main program's lexical routines go: a recorded
    /// `GLOBAL::name` is the main program's, which only it (and the `EVAL`s it
    /// runs) sees.
    // Cost: O(1) when the program declared no core routine name; otherwise a
    // set probe plus the executing unit's `EVAL` chain.
    pub(crate) fn main_routine_visible_here(&self, key: Symbol) -> bool {
        if self
            .module
            .module_visibility
            .main_shadowing_routines
            .is_empty()
        {
            return true;
        }
        let Some(name) =
            key.with_str(|k| Self::toplevel_global_routine_name(k).and_then(Symbol::lookup))
        else {
            return true;
        };
        if !self
            .module
            .module_visibility
            .main_shadowing_routines
            .contains(&name)
        {
            return true;
        }
        // `current_unit` is what the compiled-sub call paths enter; the frame's
        // recorded file answers for a method or a callback block (the same two
        // anchors `unit_private_routine` consults).
        self.is_main_lineage(self.current_unit) && self.is_main_lineage(self.executing_unit_sym())
    }

    /// Whether `unit` is the main program, or an `EVAL` compiled inside it.
    // Cost: O(e), e = nesting of `EVAL`s (bounded).
    fn is_main_lineage(&self, unit: Symbol) -> bool {
        let mut unit = Some(unit);
        for _ in 0..64 {
            let Some(sym) = unit else {
                return false;
            };
            if sym == crate::runtime::main_unit() {
                return true;
            }
            unit = crate::runtime::eval_unit_parent(sym);
        }
        false
    }

    /// Whether the main program declared a routine under the core name `name`
    /// (its resolution then depends on the asking compunit).
    // Cost: O(1).
    pub(crate) fn is_main_shadowing_routine(&self, name: Symbol) -> bool {
        !self
            .module
            .module_visibility
            .main_shadowing_routines
            .is_empty()
            && self
                .module
                .module_visibility
                .main_shadowing_routines
                .contains(&name)
    }
}
