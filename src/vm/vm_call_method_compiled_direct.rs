use super::*;

impl Interpreter {
    /// Resolve and dispatch a method call directly through the VM's cached
    /// compiled-method path (`resolve_method_cached`, `check_method_wrap_chain`,
    /// and `dispatch_compiled_method`), without going through
    /// `call_method_with_values` / `call_method_mut_with_values`. Returns
    /// `None` when no compiled resolution is available: either no user method
    /// was found, or the resolved method still lacks compiled bytecode after
    /// an on-demand compile attempt. The caller should fall back to
    /// `run_instance_method_at` (or its own existing native handling) then.
    ///
    /// ADR-0019 F6: `call_method_with_values` and `call_method_mut_with_values`
    /// each call several `run_instance_method`-family fallback sites
    /// (`dispatch_instance_and_fallback`, `dispatch_new`, their own
    /// native-lever-A/general fallback branches, ...) from *within* their own
    /// ~3900/~2800-line bodies. Swapping one of those sites' carrier call for
    /// a call back into `call_method_with_values`/`call_method_mut_with_values`
    /// itself recurses whenever the modern resolver falls through to the same
    /// fallback again for the same `(target, method)` — confirmed as a real,
    /// reproducible stack overflow, not a theoretical concern (see
    /// `todo/deep/adr0019-f6-vm-level-dispatch-helper-needed.md`). This helper
    /// exists so those sites can migrate off the carrier without that hazard:
    /// unlike `call_method_with_values`, it performs no accessor-vs-method
    /// arbitration and never calls either dispatch entry point, so it cannot
    /// re-enter its own caller's call graph.
    pub(crate) fn try_dispatch_compiled_method_direct(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let class_sym = match target.view() {
            ValueView::Instance { class_name, .. } => class_name,
            ValueView::Package(name) => name,
            _ => return None,
        };
        self.refresh_method_caches_for_generation();
        let cn = class_sym.as_str();
        let method_sym = crate::symbol::Symbol::intern(method);
        let (owner_class, method_def) =
            self.resolve_method_cached(cn, method, class_sym, method_sym, args, target)?;
        if let Some(result) = self.check_method_wrap_chain(
            cn,
            owner_class.as_str(),
            method,
            &method_def,
            target,
            args,
        ) {
            return Some(result);
        }
        let (owner_class, method_def) = if method_def.compiled_code.is_some() {
            (owner_class, method_def)
        } else if !method_def.body.is_empty()
            && let Some((owner, def)) =
                self.populate_uncompiled_method(cn, owner_class.as_str(), method, args, target)
        {
            // Refresh the resolve caches so future calls take the
            // already-compiled fast path without re-resolving (mirrors
            // `try_compiled_method_or_interpret_inner`'s own on-demand-compile
            // cache refresh).
            let cache_key = (class_sym, method_sym);
            self.method_resolve_cache
                .insert(cache_key, Some((owner, def.clone())));
            if !def.is_multi {
                self.last_method_resolve = Some((class_sym, method_sym, owner, def.clone()));
            }
            (owner, def)
        } else {
            return None;
        };
        let cc = method_def
            .compiled_code
            .clone()
            .expect("compiled_code set above");
        Some(self.dispatch_compiled_method(
            cn,
            owner_class.as_str(),
            method,
            &method_def,
            &cc,
            target.clone(),
            args.to_vec(),
            None,
        ))
    }
}
