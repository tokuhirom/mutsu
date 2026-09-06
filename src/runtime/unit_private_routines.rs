//! Compunit-private top-level routines.
//!
//! Raku scopes a package-less top-level `sub name {...}` **lexically to its own
//! compilation unit**: it is not installed as a shared `GLOBAL::name` stash
//! entry the way an `our sub` / package-scoped routine is. mutsu registers it
//! that way regardless, so before this module a module's own private helper
//! stayed permanently callable, bare, from whatever scope `use`d or `require`d
//! it (`todo/deep/module-toplevel-private-sub-leak-cleanup.md`).
//!
//! The fix is a *move*, not a delete. Deleting the entry outright is what every
//! earlier attempt did, and it cannot work: mutsu resolves a bare routine name
//! through the flat registry, so the module's OWN bodies -- its exported subs,
//! its classes' methods, its `sub EXPORT` -- reach their private helpers by the
//! same `GLOBAL::name` key the importer does. Instead the entry is moved into
//! `Interpreter::unit_private_routines`, keyed by the declaring compunit, and
//! [`Interpreter::unit_private_routine`] hands it back to code compiled in that
//! same unit (or in an `EVAL` nested inside it) and to nobody else. That is the
//! same lexical-by-compilation-unit scoping `user_declared_infix_ops` already
//! uses for user-declared operators, keyed off the `current_unit` the VM
//! already maintains across every compiled-routine call.
//!
//! ## The ambient `GLOBAL::` installers this must not touch
//!
//! Only routines *the loaded compunit's own body registered* are candidates,
//! which [`Interpreter::hide_toplevel_global_routines`] makes exact: it empties
//! the package-less single-routine namespace before the body runs, so whatever
//! is in it afterwards came from that body. On top of that, the same predicate
//! the hide uses ([`Interpreter::is_toplevel_global_routine_key`]) excludes:
//!
//! - **`sub EXPORT`** (`GLOBAL::EXPORT`), a per-load magic name that
//!   `apply_module_export` calls and deletes *after* the load
//!   (`remove_export_routine`), long after this runs.
//! - **multi candidates** (`GLOBAL::name/<sig>`), which are additive across
//!   compunits by design (several modules legitimately contribute candidates to
//!   one package-less `multi trait_mod:<is>`).
//! - **package-qualified** entries (`GLOBAL::Foo::bar`), which are real shared
//!   stash entries.
//!
//! Seclusion additionally skips:
//!
//! - **exported routines** of any kind -- the very thing that is *supposed* to
//!   reach the importer. Checked against the union of `exported_subs`,
//!   `unit_module_exported_subs` and `module_owned_exports`, so a name any
//!   module has ever exported is left alone (conservative: it can only
//!   under-fix, never over-reap).
//! - **prelude routines** (`PRELUDE_SUB_TRAIT`: NativeCall's `nativecast`,
//!   `nativesizeof`, `cglobal`, ...), which are deliberately spliced as ambient
//!   package-less `GLOBAL::` routines into every compunit that uses NativeCall
//!   and are never `is export`ed. Recorded by name at their registration site
//!   (`registration_sub::register_sub_decl_with_metadata`).
//! - **`MAIN`**, whose leak has its own, narrower removal
//!   (`remove_leaked_main_routines`) that must keep deciding it.

use super::*;

impl Interpreter {
    /// Move the package-less top-level routines the compunit at `source_path`
    /// just declared, but did not export, out of the shared registry and into
    /// that compunit's private table.
    ///
    /// Call it while the loaded compunit's registrations are still the only
    /// occupants of the package-less namespace -- i.e. after its `run_block`
    /// and *before* [`Self::restore_toplevel_global_routines`] puts the loading
    /// scope's own entries back.
    pub(crate) fn seclude_private_toplevel_routines(&mut self, source_path: &str) {
        let unit = self.unit_of_source(Some(source_path));
        let candidates: Vec<(Symbol, String)> = self
            .registry()
            .functions
            .keys()
            .filter_map(|k| {
                let ks = k.resolve();
                let name = Self::toplevel_global_routine_name(&ks)?;
                Some((*k, name.to_string()))
            })
            .collect();
        if candidates.is_empty() {
            return;
        }
        let exported = self.exported_routine_names();
        let mut secluded: Vec<(Symbol, Arc<FunctionDef>)> = Vec::new();
        for (key, name) in candidates {
            if name == "MAIN" || exported.contains(&name) {
                continue;
            }
            let name_sym = Symbol::intern(&name);
            if self.prelude_sub_names.contains(&name_sym) {
                continue;
            }
            let Some(def) = self.registry_mut().functions.remove(&key) else {
                continue;
            };
            secluded.push((name_sym, def));
        }
        if secluded.is_empty() {
            return;
        }
        let table = self.unit_private_routines.entry(unit).or_default();
        for (name_sym, def) in secluded {
            table.insert(name_sym, def);
        }
        let names: Vec<Symbol> = self.unit_private_routines[&unit].keys().copied().collect();
        for name_sym in names {
            self.unit_private_names.insert(name_sym);
            // The bare `&name` env binding is the other way the routine stayed
            // reachable from the loading scope (`say &helper`); it is written by
            // the same registration and has to travel with the registry entry.
            self.env.remove(&format!("&{}", name_sym.resolve()));
        }
        // Invalidate name-keyed resolution caches: these names now resolve
        // differently depending on the unit asking.
        self.fn_resolve_gen += 1;
    }

    /// Every routine name any loaded module has exported, in any form.
    fn exported_routine_names(&self) -> std::collections::HashSet<String> {
        let mut names = std::collections::HashSet::new();
        for table in self.exported_subs.values() {
            names.extend(table.keys().cloned());
        }
        for table in self.unit_module_exported_subs.values() {
            names.extend(table.keys().cloned());
        }
        for table in self.module_owned_exports.values() {
            names.extend(table.keys().cloned());
        }
        names
    }

    /// The bare routine name of a package-less top-level registry key, or
    /// `None` when `key` is not one. See
    /// [`Self::is_toplevel_global_routine_key`], whose predicate this is.
    pub(crate) fn toplevel_global_routine_name(key: &str) -> Option<&str> {
        key.strip_prefix("GLOBAL::")
            .filter(|tail| !tail.contains("::") && !tail.contains('/') && *tail != "EXPORT")
    }

    /// A compunit-private routine named `name` visible to the code running
    /// right now, if any. Visible means: declared by the unit currently
    /// executing, or by a unit an enclosing `EVAL` was compiled in (an `EVAL`
    /// compiles in its caller's lexical scope, exactly as
    /// `user_infix_override` treats a user-declared operator).
    pub(crate) fn unit_private_routine(&self, name: &str) -> Option<Arc<FunctionDef>> {
        if self.unit_private_names.is_empty() {
            return None;
        }
        let name_sym = Symbol::lookup(name)?;
        if !self.unit_private_names.contains(&name_sym) {
            return None;
        }
        if let Some(def) = self.unit_private_routine_from(self.current_unit, name_sym) {
            return Some(def);
        }
        // `current_unit` is entered by the compiled-*sub* call paths
        // (`enter_compilation_unit`); a method body reaches its own compiled
        // code through the method-dispatch paths, which do not. The innermost
        // routine frame records the file its body lives in (`def_file`, what a
        // backtrace renders), so it answers the same question for those.
        let def_file = self.routine_stack.last().and_then(|f| f.def_file)?;
        let unit = self.unit_of_source(Some(&def_file.resolve()));
        self.unit_private_routine_from(unit, name_sym)
    }

    /// A private routine named `name_sym` declared by `unit`, or by a unit
    /// `unit` is an `EVAL` of (an `EVAL` compiles in its caller's lexical
    /// scope).
    fn unit_private_routine_from(
        &self,
        unit: Symbol,
        name_sym: Symbol,
    ) -> Option<Arc<FunctionDef>> {
        let mut unit = Some(unit);
        // An EVAL nested in an EVAL nested in ... is bounded in practice; the
        // cap only stops a cycle from hanging the VM.
        for _ in 0..64 {
            let sym = unit?;
            if let Some(def) = self
                .unit_private_routines
                .get(&sym)
                .and_then(|table| table.get(&name_sym))
            {
                return Some(def.clone());
            }
            unit = crate::runtime::eval_unit_parent(sym);
        }
        None
    }

    /// True when `name` resolves differently depending on which compilation
    /// unit is asking, and therefore must not be answered from (or written to)
    /// the name-keyed resolution caches, which are not keyed by unit.
    #[inline]
    pub(crate) fn is_unit_scoped_routine_name(&self, name: &str) -> bool {
        if self.unit_private_names.is_empty() {
            return false;
        }
        Symbol::lookup(name).is_some_and(|s| self.unit_private_names.contains(&s))
    }

    /// [`Self::is_unit_scoped_routine_name`] for an already-interned name.
    #[inline]
    pub(crate) fn is_unit_scoped_routine_sym(&self, name: Symbol) -> bool {
        !self.unit_private_names.is_empty() && self.unit_private_names.contains(&name)
    }
}
