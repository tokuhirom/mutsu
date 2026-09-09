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
    /// If the just-loaded module defined `sub EXPORT`, call it with the `use`
    /// arguments and install the symbols from its returned `Map`(s) into the
    /// caller's scope. `EXPORT` itself is special (never an export), so it is
    /// removed from the registry afterwards to avoid leaking as a callable.
    pub(super) fn apply_module_export(
        &mut self,
        export_args: Vec<Value>,
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
                let saved_env = self.env.clone();
                let result = self.call_sub_value(export_sub.clone(), export_args, false)?;
                self.env = saved_env;
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
        // Snapshot env across the call and restore it afterwards: the call's
        // scalar return-merge writes EXPORT's own params/locals (`$x`, a `my $y`)
        // back into this (the caller's) env as their post-return values. A sub
        // EXPORT returns that closes over such a lexical carries the correct
        // captured value, but a later bareword call of it merges the caller env
        // with `merge_all` (keep-existing) semantics — so the leaked stale entry
        // would shadow the capture. Dropping EXPORT's env writes keeps the
        // caller env clean; EXPORT's real effects are its return value and
        // control flow (die/note/exit), not caller-env mutation.
        let empty_fns = crate::opcode::CompiledFns::default();
        let saved_env = self.env.clone();
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
        self.env = saved_env.clone();
        // `EXPORT` must not itself become a callable in (or leak from) the
        // module; drop every registered `EXPORT` routine now that it has run.
        self.remove_export_routine();
        self.install_export_map(&result, importer.as_deref());
        if let Some(m) = self.module_load_stack.last().cloned() {
            // `saved_env` is the module's own scope: `apply_module_export` runs
            // right after the module body, before the load unwinds.
            self.module_export_defs
                .insert(m, ModuleExportDef::Sub(def, saved_env));
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
            ModuleExportDef::Sub(d, module_env) => {
                let empty_fns = crate::opcode::CompiledFns::default();
                self.env = module_env;
                // Same compunit anchoring as the first-load path above.
                let saved_unit = self.current_unit;
                self.current_unit = self.unit_of_declaring_file(d.source_file.as_deref());
                let r = self.compile_and_call_function_def(&d, export_args, &empty_fns);
                self.current_unit = saved_unit;
                r?
            }
            ModuleExportDef::Value(v) => self.call_sub_value(v, export_args, false)?,
        };
        self.env = saved_env;
        let importer = self.module_load_stack.last().cloned();
        self.install_export_map(&result, importer.as_deref());
        Ok(())
    }

    /// Remove any `EXPORT` routine registered by the module body (it runs under
    /// GLOBAL, so the key is `GLOBAL::EXPORT`; be liberal in case a package
    /// prefix was used) so it does not leak into the caller as `EXPORT()`.
    fn remove_export_routine(&mut self) {
        self.registry_mut().functions.retain(|key, _| {
            let ks = key.resolve();
            ks != "EXPORT" && !ks.ends_with("::EXPORT")
        });
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
                // declaring-file restriction (see the field's doc comment).
                crate::runtime::cow_table_mut(&mut self.user_declared_infix_ops)
                    .entry(op.to_string())
                    .or_default();
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
