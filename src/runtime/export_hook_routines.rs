//! The registry side of a module's `sub EXPORT` hook: finding the candidate a
//! `use` dispatches to, and keeping each compunit's hook out of every other
//! compunit's way (see `runtime_module_export_sub` for the call itself).
use super::*;

impl Interpreter {
    /// The `EXPORT` routine a `use` with `export_args` calls: the multi
    /// candidate its arguments select, else the plain hook.
    pub(super) fn resolve_export_hook(
        &mut self,
        export_args: &[Value],
    ) -> Option<Arc<FunctionDef>> {
        self.resolve_function_with_types("EXPORT", export_args)
            .or_else(|| self.resolve_function("EXPORT"))
    }

    /// Every registered `EXPORT` routine (a plain hook, or each candidate of a
    /// multi), keyed as the registry holds it.
    // Cost: O(F), F = registered routines.
    pub(super) fn export_routine_candidates(
        &self,
    ) -> Vec<(crate::symbol::Symbol, Arc<FunctionDef>)> {
        self.registry()
            .functions
            .iter()
            .filter(|(k, _)| Self::is_export_routine_key(&k.resolve()))
            .map(|(k, def)| (*k, def.clone()))
            .collect()
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
        self.invalidate_fn_resolution();
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
        self.invalidate_fn_resolution();
    }

    /// Whether a registry key names the magic `EXPORT` hook. The module body
    /// runs under GLOBAL, so the key is normally `GLOBAL::EXPORT`; be liberal
    /// in case a package prefix was used. Multi `EXPORT` declarations are
    /// stored under arity-suffixed keys such as `GLOBAL::EXPORT/0`, so they
    /// must be hidden too when a nested module loads.
    pub(super) fn is_export_routine_key(key: &str) -> bool {
        key == "EXPORT" || key.ends_with("::EXPORT") || key.contains("::EXPORT/")
    }

    /// Remove any `EXPORT` routine registered by the module body (it runs under
    /// GLOBAL, so the key is `GLOBAL::EXPORT`; be liberal in case a package
    /// prefix was used) so it does not leak into the caller as `EXPORT()`.
    pub(super) fn remove_export_routine(&mut self) {
        self.registry_mut()
            .functions_mut()
            .retain(|key, _| !Self::is_export_routine_key(&key.resolve()));
        // Invalidate name-keyed resolution caches.
        self.invalidate_fn_resolution();
    }
}
