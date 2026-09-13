//! Custom module export via `sub EXPORT`.
//!
//! A module may define `sub EXPORT(...)` which Raku calls with the `use`
//! arguments and whose return `Map` (or list of `Map`s) names the symbols to
//! install into the importing scope. mutsu calls it at module-load time (see
//! `load_module`), after the module body has run and its subs are registered.
use super::*;
use crate::value::ValueView;

/// A module's EXPORT, remembered across the first load so a re-`use` of the
/// already-loaded module can run it again with the new import's arguments
/// (Raku runs `sub EXPORT` on every import, not once per process).
#[derive(Clone)]
pub(crate) enum ModuleExportDef {
    /// The module's own `sub EXPORT`, together with the module-scope env it
    /// was first called in. Raku's `EXPORT` is a closure over its own
    /// compunit, so a re-`use` of an already-loaded module has to run it there
    /// too: the new importer's scope holds none of the module's lexicals, and
    /// `NativeLibs`' `Map.new('NativeCall' => NativeCall, ...)` quietly
    /// degraded to the *bareword string* `"NativeCall"` when re-run against it
    /// -- which then shadowed the real package for the importer.
    Sub(Arc<FunctionDef>, crate::env::Env),
    /// An `&EXPORT` the module imported from another module's EXPORT map
    /// (the Slangify pattern).
    Value(Value),
}

impl Interpreter {
    /// Bind `$*LANG` for the duration of a `sub EXPORT` call.
    ///
    /// In Rakudo `EXPORT` runs at *compile* time of the importing unit, where
    /// `$*LANG` is the live language object; a module that adds a slang reads
    /// it there (`$*LANG.define_slang`, `$*LANG.set_how`). mutsu runs `EXPORT`
    /// at module-load time instead, so nothing would otherwise bind it and the
    /// whole registration died on `Nil`. Bind the same minimal `CompLang`
    /// handle the parse-time activation sub-interpreter uses (ADR-0026 §4), so
    /// one EXPORT body works in both. Only ever *adds* the binding: an
    /// activation run has already put its own there.
    fn bind_compile_time_lang(&mut self) {
        if self.env.get("*LANG").is_none() {
            self.env.insert(
                "*LANG".to_string(),
                crate::runtime::slang_activation::comp_lang_instance(),
            );
        }
    }

    /// Put `caller_env` back as the current env after module code ran in
    /// `self.env`, but keep that code's writes to dynamic variables the
    /// caller already owned.
    ///
    /// A `$*x` belongs to the dynamic scope that *declared* it, not to the
    /// module that assigns it, so a `sub EXPORT { $*PACKAGE_LOADED++ }` has to
    /// leave the importer's counter incremented — that is how a module reports
    /// a load-time fact, and how `modules/if/`'s own suite counts loads
    /// (#8229). The wholesale restore this replaces dropped those writes along
    /// with EXPORT's params and locals, which is the only thing it is actually
    /// there to drop (see `apply_module_export`).
    ///
    /// Only keys the caller already had are carried over, so a dynamic the
    /// module declared for itself still dies with the load.
    fn restore_caller_env_keeping_dynamics(&mut self, mut caller_env: crate::env::Env) {
        for (key, value) in &self.env {
            if key.is_dynamic_var_env_key()
                && caller_env.contains_key_sym(*key)
                && caller_env.get_sym(*key) != Some(value)
            {
                caller_env.insert_sym(*key, value.clone());
            }
        }
        self.env = caller_env;
    }

    /// Overlay the importer's live dynamic variables onto a remembered
    /// module-scope env, before `sub EXPORT` is re-run in it.
    ///
    /// [`ModuleExportDef::Sub`] carries the module's own scope as it stood at
    /// its FIRST load, so a re-`use` would otherwise run EXPORT against that
    /// load's `$*x` values. A `$*PACKAGE_LOADED` already incremented to 1 then
    /// reads as 0, increments back to 1, and
    /// [`Interpreter::restore_caller_env_keeping_dynamics`] sees no change to
    /// carry back — the second load goes uncounted (#8229). Dynamics are
    /// dynamic-scope: the live binding belongs to whoever is importing now,
    /// not to the scope that happened to load the module first.
    fn overlay_caller_dynamics(env: &mut crate::env::Env, caller_env: &crate::env::Env) {
        for (key, value) in caller_env {
            if key.is_dynamic_var_env_key() {
                env.insert_sym(*key, value.clone());
            }
        }
    }

    /// If the just-loaded module defined `sub EXPORT`, call it with the `use`
    /// arguments and install the symbols from its returned `Map`(s) into the
    /// caller's scope. `EXPORT` itself is special (never an export), so it is
    /// removed from the registry afterwards to avoid leaking as a callable.
    ///
    /// `module_env` is a snapshot of `self.env` taken right after the module's
    /// own body finished running, before the load's env restoration (dropping
    /// the module's transitively-`use`d packages that don't belong to the
    /// importer, restoring the loading scope's own plain bindings, etc.)
    /// stripped it back down. `sub EXPORT` is part of the module's own
    /// closure, so it must see everything the mainline could see while it
    /// runs -- e.g. NativeLibs' `Map.new('NativeCall' => NativeCall, ...)`
    /// needs `NativeCall` still bound to its package, not gone the way
    /// `leaked_packages` (`run_modules.rs`) already dropped it by the time
    /// this is called. Running EXPORT against the (already-restored) current
    /// `self.env` instead degrades that bareword to the plain string
    /// `"NativeCall"`, which then shadows the real package for every importer
    /// (#7806).
    pub(super) fn apply_module_export(
        &mut self,
        export_args: Vec<Value>,
        module_env: crate::env::Env,
    ) -> Result<(), RuntimeError> {
        // An `&EXPORT` this module imported from another module's EXPORT map
        // (the Slangify pattern) becomes this module's own EXPORT. Consume the
        // record either way so it cannot go stale; the module's own
        // `sub EXPORT` wins when both exist.
        let inherited = self
            .module_load_stack
            .last()
            .cloned()
            .and_then(|m| self.pending_inner_export_subs.remove(&m));
        // This module's exports go to the importer *below* it on the load
        // stack (None when a user script is the importer).
        let importer = self.module_load_stack.iter().rev().nth(1).cloned();
        // The module body runs under GLOBAL, so `sub EXPORT` registers as
        // `GLOBAL::EXPORT`. Only participate when it is actually present.
        let Some(def) = self.resolve_function("EXPORT") else {
            if let Some(export_sub) = inherited {
                // Same env discipline as the compiled path below: the imported
                // EXPORT's effects are its return value, not caller-env writes.
                let caller_env = self.env.clone();
                self.env = module_env;
                self.bind_compile_time_lang();
                let result = self.call_sub_value(export_sub.clone(), export_args, false)?;
                self.restore_caller_env_keeping_dynamics(caller_env);
                self.install_export_map(&result, importer.as_deref());
                if let Some(m) = self.module_load_stack.last().cloned() {
                    self.module_export_defs
                        .insert(m, ModuleExportDef::Value(export_sub));
                }
            }
            return Ok(());
        };
        // Run EXPORT through the compiled call path (not the tree-walk
        // `call_function` slow path): its params become real local slots, so a
        // sub the EXPORT returns can capture a use-argument (`sub EXPORT($x)
        // { Map.new: '&f' => sub { ...$x... } }`).
        //
        // Snapshot the CALLER's env (not the module's) so it can be restored
        // after the call: the call's scalar return-merge writes EXPORT's own
        // params/locals (`$x`, a `my $y`) back into whatever `self.env` is at
        // return time as their post-return values. A sub EXPORT returns that
        // closes over such a lexical carries the correct captured value, but a
        // later bareword call of it merges the caller env with `merge_all`
        // (keep-existing) semantics — so the leaked stale entry would shadow
        // the capture. Dropping EXPORT's env writes keeps the caller env
        // clean; EXPORT's real effects are its return value and control flow
        // (die/note/exit), not caller-env mutation.
        let empty_fns = crate::opcode::CompiledFns::default();
        let caller_env = self.env.clone();
        self.env = module_env.clone();
        self.bind_compile_time_lang();
        // Anchor the call to the module's own compunit. `EXPORT` is the
        // module's code, so a prelude splice made into the module's unit (the
        // NativeCall `trait_mod:<is>` candidates `NativeLibs` introspects) has
        // to be visible while it runs -- `prelude_visible_here` otherwise hides
        // it as soon as the importer's frames are what the unit walk reaches.
        let saved_unit = self.current_unit;
        self.current_unit = self.unit_of_declaring_file(def.source_file.as_deref());
        let result = self.compile_and_call_function_def(&def, export_args, &empty_fns);
        self.current_unit = saved_unit;
        let result = result?;
        self.restore_caller_env_keeping_dynamics(caller_env);
        // `EXPORT` must not itself become a callable in (or leak from) the
        // module; drop every registered `EXPORT` routine now that it has run.
        self.remove_export_routine();
        self.install_export_map(&result, importer.as_deref());
        if let Some(m) = self.module_load_stack.last().cloned() {
            // `module_env` is the module's own scope: a re-`use` of an
            // already-loaded module re-runs EXPORT there too (see
            // `rerun_module_export`), since the new importer's own scope
            // holds none of the module's lexicals.
            self.module_export_defs
                .insert(m, ModuleExportDef::Sub(def, module_env));
        }
        Ok(())
    }

    /// Re-run an already-loaded module's remembered EXPORT for a new import
    /// (its returned map may depend on the `use` arguments). No-op for modules
    /// without one.
    pub(super) fn rerun_module_export(&mut self, module: &str) -> Result<(), RuntimeError> {
        let Some(def) = self.module_export_defs.get(module).cloned() else {
            return Ok(());
        };
        let export_args = self.pending_use_export_args.take().unwrap_or_default();
        let saved_env = self.env.clone();
        let result = match def {
            ModuleExportDef::Sub(d, mut module_env) => {
                let empty_fns = crate::opcode::CompiledFns::default();
                Self::overlay_caller_dynamics(&mut module_env, &saved_env);
                self.env = module_env;
                self.bind_compile_time_lang();
                // Same compunit anchoring as the first-load path above.
                let saved_unit = self.current_unit;
                self.current_unit = self.unit_of_declaring_file(d.source_file.as_deref());
                let r = self.compile_and_call_function_def(&d, export_args, &empty_fns);
                self.current_unit = saved_unit;
                r?
            }
            ModuleExportDef::Value(v) => {
                self.bind_compile_time_lang();
                self.call_sub_value(v, export_args, false)?
            }
        };
        // Same discipline as the first-load path: EXPORT's own lexicals go,
        // the importer's dynamics keep whatever EXPORT wrote (#8229). Raku
        // runs `sub EXPORT` on every import, so a re-`use` must report its
        // load the same way the first one did.
        self.restore_caller_env_keeping_dynamics(saved_env);
        let importer = self.module_load_stack.last().cloned();
        self.install_export_map(&result, importer.as_deref());
        Ok(())
    }

    /// Take every currently-registered `EXPORT` routine out of the registry
    /// before a compunit's own body runs, returning them for
    /// [`Interpreter::restore_export_routines`].
    ///
    /// `sub EXPORT` is per-compunit in Raku: each file may declare one, and two
    /// files' hooks never see each other. mutsu registers it under the single
    /// `GLOBAL::EXPORT` key (the module body runs under `GLOBAL`), so without
    /// this two modules in one load chain that each declare `sub EXPORT`
    /// compete for that one key. Sub declarations are hoisted, so entering the
    /// outer module's body registers `GLOBAL::EXPORT` before its `use` of the
    /// inner one runs; the inner module's hoisted declaration then hit the
    /// redeclaration check in `registration_sub.rs` and the whole load died
    /// with `X::Redeclaration` (#7947 -- the dominant lizmat "re-export a
    /// dependency under another name" idiom, 10 distributions in the ecosystem
    /// ledger). Hiding the enclosing compunit's hook while the nested one loads
    /// restores the per-compunit invariant with the same mechanism
    /// `hide_toplevel_global_routines` already uses for ordinary package-less
    /// top-level routines.
    ///
    /// The one thing that must not be copied from that mechanism is its
    /// restore point. `EXPORT` is deliberately excluded from
    /// `is_toplevel_global_routine_key` because that restore runs *before*
    /// `apply_module_export`, which would put an outer module's stale hook back
    /// over the inner module's fresh one before it is ever read (`t/sub-export.t`).
    /// So this pair brackets `apply_module_export` instead -- see its caller in
    /// `run_modules.rs`.
    pub(super) fn hide_export_routines(
        &mut self,
    ) -> Vec<(crate::symbol::Symbol, Arc<FunctionDef>)> {
        let keys: Vec<crate::symbol::Symbol> = self
            .registry()
            .functions
            .keys()
            .filter(|k| Self::is_export_routine_key(&k.resolve()))
            .copied()
            .collect();
        if keys.is_empty() {
            return Vec::new();
        }
        let mut hidden = Vec::with_capacity(keys.len());
        for key in keys {
            if let Some(def) = self.registry_mut().functions_mut().remove(&key) {
                hidden.push((key, def));
            }
        }
        // Invalidate name-keyed resolution caches.
        self.fn_resolve_gen += 1;
        hidden
    }

    /// Put back what [`Interpreter::hide_export_routines`] hid, once this
    /// compunit's own `sub EXPORT` has been called and dropped.
    pub(super) fn restore_export_routines(
        &mut self,
        hidden: Vec<(crate::symbol::Symbol, Arc<FunctionDef>)>,
    ) {
        if hidden.is_empty() {
            return;
        }
        for (key, def) in hidden {
            self.registry_mut().functions_mut().insert(key, def);
        }
        // Invalidate name-keyed resolution caches.
        self.fn_resolve_gen += 1;
    }

    /// Whether a registry key names the magic `EXPORT` hook. The module body
    /// runs under GLOBAL, so the key is normally `GLOBAL::EXPORT`; be liberal
    /// in case a package prefix was used.
    fn is_export_routine_key(key: &str) -> bool {
        key == "EXPORT" || key.ends_with("::EXPORT")
    }

    /// Remove any `EXPORT` routine registered by the module body (it runs under
    /// GLOBAL, so the key is `GLOBAL::EXPORT`; be liberal in case a package
    /// prefix was used) so it does not leak into the caller as `EXPORT()`.
    fn remove_export_routine(&mut self) {
        self.registry_mut()
            .functions_mut()
            .retain(|key, _| !Self::is_export_routine_key(&key.resolve()));
        // Invalidate name-keyed resolution caches.
        self.fn_resolve_gen += 1;
    }

    /// Install the symbols named by an `EXPORT` return value. Accepts a single
    /// `Map`/`Hash` (`'&name' => sub {...}`, `'$name' => value`) or a list of
    /// them (recursing), matching `sub EXPORT { Map.new: ... }` and the
    /// multi-tag `%(...)` form.
    fn install_export_map(&mut self, val: &Value, inner_export_importer: Option<&str>) {
        match val.view() {
            ValueView::Hash(gc) => {
                let pairs: Vec<(String, Value)> =
                    gc.map.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                for (key, value) in pairs {
                    self.install_export_symbol(key, value, inner_export_importer);
                }
            }
            ValueView::Array(items, ..) => {
                let items: Vec<Value> = items.iter().cloned().collect();
                for item in items {
                    self.install_export_map(&item, inner_export_importer);
                }
            }
            _ => {}
        }
    }

    /// Install one exported symbol under its sigilled name (`&greet`, `$foo`,
    /// `@bar`, `%baz`) into the current (caller's) scope. An exported operator
    /// sub is also registered with the parser so runtime-parsed code (EVAL)
    /// recognizes the new operator symbol.
    fn install_export_symbol(
        &mut self,
        key: String,
        value: Value,
        inner_export_importer: Option<&str>,
    ) {
        // An exported `&EXPORT` imported *by a module being loaded* becomes
        // that module's own EXPORT for its importers (the Slangify pattern),
        // not an env-visible callable — EXPORT is special and never leaks.
        if key == "&EXPORT"
            && let Some(importer) = inner_export_importer
        {
            self.pending_inner_export_subs
                .insert(importer.to_string(), value);
            return;
        }
        let sigil = key.chars().next();
        if let Some('&') = sigil {
            let op = &key[1..];
            if matches!(
                op.split_once(":<").map(|(c, _)| c),
                Some("prefix" | "postfix" | "infix" | "circumfix" | "postcircumfix")
            ) {
                crate::runtime::cow_table_mut(&mut self.imported_operator_names)
                    .insert(op.to_string());
            }
            if op.starts_with("infix:<") {
                // Exported: visible in the importing unit, so no
                // declaring-file restriction (see the field's doc comment:
                // an empty file set means "visible everywhere"). This must
                // force the set empty, not merely fill it in when absent
                // (`.entry().or_default()`): the declaring module's OWN
                // decl-time registration (`registration_sub.rs`) already
                // populated a non-empty entry scoped to ITS unit, so a
                // fill-if-absent here was a no-op, leaving the operator
                // invisible to every importer — including the declaring
                // module's own body once that decl-time entry correctly
                // named it instead of (by a since-fixed bug, #8008) the
                // unit that had triggered the module's load.
                crate::runtime::cow_table_mut(&mut self.user_declared_infix_ops)
                    .insert(op.to_string(), std::collections::HashSet::new());
                crate::vm::vm_jit::note_user_infix_decl();
            }
        }
        // Install into env under the key the reader looks up. A `$scalar` read
        // compiles to a bare (sigil-stripped) `GetGlobal` — the same key an
        // `our $x` module global lands under — so the sigil must be dropped for
        // scalars. Arrays/hashes/subs are read under their sigilled name.
        self.unsuppress_name(&key);
        let env_key = match sigil {
            Some('$') => key[1..].to_string(),
            _ => key,
        };
        self.env.insert(env_key, value);
        self.fn_resolve_gen += 1;
    }
}
