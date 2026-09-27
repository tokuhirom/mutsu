//! Putting a loaded module's own state back after a scope rollback.
//!
//! A scope restore (block exit, `EVAL` rollback, import-scope pop) resets the
//! routine registry and `env` to a snapshot taken before the scope ran, but a
//! module first loaded inside that scope stays in `loaded_modules`, so a later
//! `use` of it is a no-op that cannot re-register anything. These helpers put
//! back what such a load owns.
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Put back the routines a module load introduced that `snapshot` predates.
    ///
    /// Restoring `registry.functions` to a snapshot taken before a `use` drops
    /// the used module's own routines, but NOT its entry in `loaded_modules` —
    /// so the module is left half-loaded: code that still holds one of its subs
    /// (a `&name` the scope returned, or an export hoisted into the caller) runs
    /// with the module's file-scoped helpers gone, and re-`use`ing it is a no-op
    /// that cannot restore them. `'use File::Temp; &tempfile'.EVAL` hits exactly
    /// this: the returned `&tempfile` then dies with `Unknown function:
    /// make-temp`.
    ///
    /// Only package-qualified keys are tracked (see `module_registered_functions`),
    /// so the importing scope's bare aliases still go out of scope normally.
    ///
    /// `include_global_aliases` decides whether the `GLOBAL::`-prefixed members of
    /// that set are put back too, and only the **`EVAL` rollback** passes `true`.
    /// The distinction is forced by a key collision the set cannot see through: a
    /// file with no `unit module` declaration runs its body at
    /// `current_package() == GLOBAL`, so its own `sub foo is export` registers
    /// `GLOBAL::foo` — the same shape as an alias installed *for the importing
    /// scope*. Reinstating those on an ordinary block exit made
    /// `{ require NoModule <&bar>; }` leak `&bar` past the block
    /// (`roast/S11-modules/require.t` test 10). An `EVAL` is the one caller that
    /// must reinstate them: it rolls the whole registry back while
    /// `loaded_modules` keeps the module recorded as loaded, so without this the
    /// module is left permanently unable to resolve its own imports.
    ///
    /// A `GLOBAL::` key whose routine name no loaded module exports is not
    /// subject to that collision -- no import could have installed it -- so it
    /// comes back on a block exit too. In practice that is a package-less
    /// module's own `my multi` family (`GLOBAL::build/<sig>`): unlike a single
    /// `my sub`, which [`Interpreter::seclude_private_toplevel_routines`] moves
    /// out of the registry for good, its candidates stay here, and dropping
    /// them when the block holding the module's first `use` exited left the
    /// module's own `sub EXPORT` and subs unable to see the family ever after
    /// (Identity::Utils' `use Identity::Utils 'build'` in a bare block).
    ///
    /// Takes the snapshot as the copy-on-write `Arc` the registry itself holds
    /// (see `Registry::functions`): the reinstatement is collected first and
    /// the map copied only if there is actually something to put back, so the
    /// common "nothing was declared" caller pays no copy at all.
    pub(crate) fn reinstate_module_functions(
        &self,
        functions: &mut std::sync::Arc<crate::runtime::function_table::FunctionTable>,
        include_global_aliases: bool,
    ) {
        if self.module_registered_functions.is_empty() {
            return;
        }
        let mut missing: Vec<(Symbol, std::sync::Arc<FunctionDef>)> = Vec::new();
        // Built on first need only: a key is missing from `functions` just
        // after a scope that loaded a module exits, not on an ordinary one.
        let mut exported: Option<std::collections::HashSet<String>> = None;
        let registry = self.registry();
        for key in self.module_registered_functions.iter() {
            if functions.contains_key(key) {
                continue;
            }
            // A `GLOBAL::` key is normally left out unless the EVAL rollback
            // asked for it, because it may be an alias installed for the
            // importing scope. A PRELUDE splice is never that (see
            // `prelude_registered_functions`), so it comes back either way,
            // and neither does a routine no module exports (see above).
            let key_str = key.resolve();
            if !include_global_aliases
                && let Some(tail) = key_str.strip_prefix("GLOBAL::")
                && !self.prelude_registered_functions.contains(key)
            {
                let name = crate::runtime::dispatch_resolve::function_key_strip_arity_suffix(tail);
                let exported = exported.get_or_insert_with(|| self.exported_routine_names());
                if exported.contains(name) {
                    continue;
                }
            }
            if let Some(def) = registry.functions.get(key) {
                missing.push((*key, def.clone()));
            }
        }
        drop(registry);
        if !missing.is_empty() {
            crate::runtime::cow_table_mut(functions)
                .map_mut()
                .extend(missing);
        }
    }

    /// Put back any `our` package global of `module` that has gone missing from
    /// `env` since the module was loaded. See `module_package_globals`.
    pub(crate) fn reinstate_module_package_globals(&mut self, module: &str) {
        let Some(globals) = self.module_package_globals.get(module) else {
            return;
        };
        let missing: Vec<(Symbol, Value)> = globals
            .iter()
            .filter(|(key, _)| self.env.get_sym(*key).is_none())
            .cloned()
            .collect();
        for (key, value) in missing {
            self.env.insert_sym(key, value);
        }
    }
}
