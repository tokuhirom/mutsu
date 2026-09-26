//! Compunit-private top-level routines.
//!
//! Raku scopes a package-less top-level `sub name {...}` **lexically to its own
//! compilation unit**: it is not installed as a shared `GLOBAL::name` stash
//! entry the way an `our sub` / package-scoped routine is. mutsu registers it
//! that way regardless, so before this module a module's own private helper
//! stayed permanently callable, bare, from whatever scope `use`d or `require`d
//! it (GH #7558).
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
//! - **`our sub name {...}`**, which in a package-less compunit really IS a
//!   `GLOBAL` stash entry and is legitimately reachable by bare name from the
//!   loading scope (`roast/6.c/MISC/bug-coverage.t`'s `our sub
//!   module-transform`). The `my_scoped_package_items` marker the registration
//!   path already maintains is exactly this distinction -- see
//!   `qualified_name_hidden_here` -- so it is the positive test, not a guess.
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
    /// Move a package-less lexical helper declared while a parameterized role
    /// body is being re-run into the role's compilation-unit table.
    ///
    /// Role composition can happen inside a method. The role body's `sub`
    /// declaration is installed in the flat registry while that method runs,
    /// but the method's routine-scope restore quite correctly removes ordinary
    /// lexical declarations afterwards. Composed role methods still need the
    /// helper on later calls, so give it the same compunit lifetime as a
    /// top-level lexical helper loaded from a module.
    pub(crate) fn seclude_role_lexical_routine(
        &mut self,
        package: &str,
        name: &str,
        source_path: Option<&str>,
    ) {
        let key = Symbol::intern(&format!("{package}::{name}"));
        let Some(def) = self.registry_mut().functions_mut().remove(&key) else {
            return;
        };
        let unit = self.unit_of_source(source_path.or(def.source_file.as_deref()));
        crate::runtime::cow_table_mut(&mut self.unit_private_routines)
            .entry(unit)
            .or_default()
            .insert(Symbol::intern(name), def);
        crate::runtime::cow_table_mut(&mut self.unit_private_names).insert(Symbol::intern(name));
        // The declaration's code-variable binding is only the live-scope
        // record. Calls resolve through the unit-private table after the
        // composition scope has ended.
        self.env.remove(&format!("&{name}"));
        self.env.remove(&format!("&{package}::{name}"));
        self.invalidate_fn_resolution_for_keys([key]);
    }

    /// Move the package-less top-level routines the compunit at `source_path`
    /// just declared, but did not export, out of the shared registry and into
    /// that compunit's private table.
    ///
    /// Call it while the loaded compunit's registrations are still the only
    /// occupants of the package-less namespace -- i.e. after its `run_block`
    /// and *before* [`Self::restore_toplevel_global_routines`] puts the loading
    /// scope's own entries back.
    ///
    /// `module` names the loaded module when known: only *its own* exports
    /// then stay shared. Another module's export of the same name (one this
    /// compunit perhaps also imported) does not make this compunit's own
    /// `sub name` public (#9587). A `require` of a path has no module name and
    /// keeps the conservative every-module union.
    pub(crate) fn seclude_private_toplevel_routines(
        &mut self,
        source_path: &str,
        module: Option<&str>,
    ) {
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
        let exported = match module {
            Some(module) => self.own_exported_routine_names(module),
            None => self.exported_routine_names(),
        };
        let mut secluded: Vec<(Symbol, Arc<FunctionDef>)> = Vec::new();
        for (key, name) in candidates {
            if name == "MAIN" || exported.contains(&name) {
                continue;
            }
            // An export an earlier module load installed stays visible through
            // `hide_toplevel_global_routines` (it skips these keys), so it is
            // still here without having been declared by this body.
            if self.module_registered_functions.contains(&key) {
                continue;
            }
            // `our sub name {...}` in a package-less compunit IS a GLOBAL stash
            // entry, which the loading scope legitimately reaches by bare name
            // (`roast/6.c/MISC/bug-coverage.t`'s `our sub module-transform`).
            // Only a lexical `sub`/`my sub` is compunit-private -- exactly the
            // distinction `my_scoped_package_items` already records at
            // registration time (see `qualified_name_hidden_here`).
            if !self.is_my_scoped_package_item(&key.resolve()) {
                continue;
            }
            let name_sym = Symbol::intern(&name);
            if self.prelude_sub_names.contains(&name_sym) {
                continue;
            }
            let Some(def) = self.registry_mut().functions_mut().remove(&key) else {
                continue;
            };
            secluded.push((name_sym, def));
        }
        if secluded.is_empty() {
            return;
        }
        let mut names: Vec<Symbol> = Vec::with_capacity(secluded.len());
        for (name_sym, def) in secluded {
            // Key by the routine's OWN declaring file, not by the module whose
            // load surfaced it. They differ whenever the loaded module's body
            // ran code that belongs to another compunit: composing a role
            // declared elsewhere re-runs that role's body, and a lexical `sub`
            // in it (zef's `sub DEBUG` inside `role Zef::Pluggable`) registers
            // during THIS load while remaining lexical to the role's file --
            // which is exactly where the role's methods look for it.
            let unit = self.unit_of_source(Some(def.source_file.as_deref().unwrap_or(source_path)));
            crate::runtime::cow_table_mut(&mut self.unit_private_routines)
                .entry(unit)
                .or_default()
                .insert(name_sym, def);
            names.push(name_sym);
        }
        for name_sym in names {
            crate::runtime::cow_table_mut(&mut self.unit_private_names).insert(name_sym);
            // The bare `&name` env binding is the other way the routine stayed
            // reachable from the loading scope (`say &helper`); it is written by
            // the same registration and has to travel with the registry entry.
            self.env.remove(&format!("&{}", name_sym.resolve()));
        }
        // Invalidate name-keyed resolution caches: these names now resolve
        // differently depending on the unit asking.
        self.invalidate_fn_resolution();
    }

    /// Move the package-less routine aliases the compunit at `source_path`
    /// imported with its own `use` statements out of the shared registry and
    /// into that compunit's private table (#9587).
    ///
    /// A `use` is lexical to the compunit that says it. `import_module`
    /// installs a plain routine under the runtime package the module body runs
    /// in, which is `GLOBAL` for a file with no `unit` declarator and for the
    /// pre-`unit class` window -- the very package the importer shares. Left
    /// there, `use E; ex(1)` reaches a routine only `E` imported, and a later
    /// compunit's own `sub ex` is rejected as a redeclaration. The module's own
    /// bodies still resolve the name through [`Self::unit_private_routine`],
    /// exactly like a private helper [`Self::seclude_private_toplevel_routines`]
    /// moved.
    ///
    /// `aliases` is the `imported_routine_aliases` set the module body built;
    /// only its `GLOBAL::name` members are candidates. Multi imports are left
    /// in place (the private table holds one routine per name, and multi
    /// candidates are additive across compunits by design), as are keys a
    /// module load owns (`module_registered_functions`).
    // Cost: O(a + f), a = the module's import aliases, f = registry functions
    // (one scan for multi candidates, only when there is a candidate alias).
    pub(crate) fn seclude_module_import_aliases(
        &mut self,
        aliases: &std::collections::HashSet<Symbol>,
        source_path: &str,
    ) {
        let candidates: Vec<(Symbol, Symbol)> = aliases
            .iter()
            .filter_map(|key| {
                let ks = key.resolve();
                let name = Self::toplevel_global_routine_name(&ks)?;
                if name == "MAIN" || name.contains(":<") {
                    return None;
                }
                Some((*key, Symbol::intern(name)))
            })
            .filter(|(key, _)| {
                self.registry().functions.contains_key(key)
                    && !self.module_registered_functions.contains(key)
            })
            .collect();
        if candidates.is_empty() {
            return;
        }
        let multi_names: std::collections::HashSet<String> = self
            .registry()
            .functions
            .keys()
            .filter_map(|k| {
                let ks = k.resolve();
                let (head, _) = ks.split_once('/')?;
                Self::toplevel_global_routine_name(head).map(str::to_string)
            })
            .collect();
        let unit = self.unit_of_source(Some(source_path));
        let mut moved = false;
        for (key, name_sym) in candidates {
            if multi_names.contains(name_sym.resolve().as_str())
                || self.prelude_sub_names.contains(&name_sym)
            {
                continue;
            }
            let Some(def) = self.registry_mut().functions_mut().remove(&key) else {
                continue;
            };
            self.registry_mut().our_scoped_functions.remove(&key);
            crate::runtime::cow_table_mut(&mut self.unit_private_routines)
                .entry(unit)
                .or_default()
                .insert(name_sym, def);
            crate::runtime::cow_table_mut(&mut self.unit_private_names).insert(name_sym);
            moved = true;
        }
        if moved {
            self.invalidate_fn_resolution();
        }
    }

    /// Every routine name `module` itself exports, in any form.
    fn own_exported_routine_names(&self, module: &str) -> std::collections::HashSet<String> {
        let mut names = std::collections::HashSet::new();
        for table in [
            &self.exported_subs,
            &self.unit_module_exported_subs,
            &self.module_owned_exports,
        ] {
            if let Some(exports) = table.get(module) {
                names.extend(exports.keys().cloned());
            }
        }
        names
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
        // (`enter_compilation_unit`) only. Two other shapes reach a module's own
        // code without going through one, and both are answered by the frame's
        // recorded `def_file` instead:
        //
        // - a **method body**, which arrives through the method-dispatch paths;
        // - a **block handed to a native callback taker** — `.tap`, and hence
        //   every `supply`/`whenever` body — which the supply machinery invokes
        //   through the generic code-object entry (`call_sub_value`).
        //
        // [`Self::executing_unit_sym`] is the same anchor `prelude_visible_here`
        // uses for the identical question about prelude splices: it walks out
        // through inlined bare blocks (which record no `def_file` of their own)
        // to the innermost frame that does. A block frame DOES record one — it
        // is stamped from the closure's `SubData::source_file` at
        // `push_block_routine_with_location` — so a tap callback declared in a
        // module names that module's unit however it was invoked.
        self.unit_private_routine_from(self.executing_unit_sym(), name_sym)
    }

    /// Every compunit-private routine visible to the code running right now —
    /// the enumerating twin of [`Self::unit_private_routine`], answering from
    /// the same unit that one would resolve a name against (see the precedence
    /// note in the body).
    ///
    /// A lexical pseudo-stash (`MY::`, `UNIT::`) needs the whole set rather
    /// than one name: `sub EXPORT`'s standard "export everything I declared"
    /// idiom is `UNIT::.grep: { .key.starts-with('&') }`, which can only see
    /// what the stash enumerates. Asking the env instead would be
    /// order-dependent — registration deliberately REMOVES the `&name` binding
    /// when it seclusion-moves a routine here, and only a call through the
    /// name puts one back.
    pub(crate) fn visible_unit_private_routines(&self) -> Vec<(Symbol, Arc<FunctionDef>)> {
        if self.unit_private_names.is_empty() {
            return Vec::new();
        }
        // The SAME precedence `unit_private_routine` applies to one name:
        // `current_unit` first and, only when that chain holds nothing,
        // the frame's own unit. Never the union of the two — they can be
        // different compunits (a module's `sub EXPORT` runs with the module
        // as `current_unit` while the importing script is still the executing
        // frame's unit), and unioning them put the IMPORTER's routines into
        // the module's `UNIT::`. The module then exported them back, and the
        // importer's next `sub` declaration was rejected as a redeclaration
        // of itself.
        for anchor in [self.current_unit, self.executing_unit_sym()] {
            let mut out: Vec<(Symbol, Arc<FunctionDef>)> = Vec::new();
            let mut unit = Some(anchor);
            for _ in 0..64 {
                let Some(sym) = unit else { break };
                if let Some(table) = self.unit_private_routines.get(&sym) {
                    out.extend(table.iter().map(|(name, def)| (*name, def.clone())));
                }
                unit = crate::runtime::eval_unit_parent(sym);
            }
            if !out.is_empty() {
                return out;
            }
        }
        Vec::new()
    }

    /// A private routine named `name_sym` declared by `unit`, or by a unit
    /// `unit` is an `EVAL` of (an `EVAL` compiles in its caller's lexical
    /// scope).
    pub(super) fn unit_private_routine_from(
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
