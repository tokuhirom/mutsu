//! The `GetLocal` env cell-adoption probe (ADR-0097 §15).
//!
//! A slot can hold a plain value while this frame's env overlay holds a
//! `ContainerRef` or `Proxy` under the same symbol. The slow `GetLocal` chain
//! adopts that container into the slot. ADR-0097 §15 makes the divergence an
//! invariant violation — every site that installs an overlay cell installs it
//! into the slot too — so the fast path, which never runs this probe, asserts
//! in debug builds that the probe would have found nothing.

use super::*;

impl Interpreter {
    /// The container the cell-adoption probe would copy into slot `idx`, if
    /// any: a `ContainerRef` or `Proxy` in this frame's own env overlay under
    /// the slot's symbol, while the slot holds neither.
    ///
    /// Overlay-only lookup (`overlay_get`/`overlay_get_sym`), NOT `get`/`get_sym`:
    /// this frame's own local slot must never adopt a same-named ANCESTOR call
    /// frame's container. `call_compiled_function_positional_light` (and the
    /// other scoped-env call paths) chain the callee's env as a *scoped child*
    /// of the live caller env for perf (no per-call flatten/clone); when a
    /// recursive call's own by-name env mirror is skipped for this param
    /// (`needs_env_sync` false — the common case for a plain scalar param only
    /// ever read via its slot), a plain `get`/`get_sym` here falls through the
    /// parent chain and can find the CALLER's own same-named variable instead —
    /// e.g. a recursive `sub rec($n) { my @v = ($n,); ... rec($n - 1) ... }`
    /// where the trailing-comma list literal boxes `$n`'s slot into a shared
    /// `ContainerRef` (so `@v`'s element aliases `$n`'s container) and mirrors
    /// it into env: the callee's fresh `$n = 0` binding got silently replaced
    /// by the caller's own boxed `$n` cell (still holding `1`), which never
    /// decremented — a Raku-level infinite recursion that overflowed the
    /// native Rust stack. `overlay_get`/`overlay_get_sym` read only this
    /// frame's own overlay (a plain function/method body runs under a single
    /// env tier — nested blocks do not push their own `scoped_child`), so an
    /// ancestor frame's container can never be picked up here.
    ///
    /// Type objects and complex values (packages, arrays, hashes, subs,
    /// instances, a lazy `Match`) are never replaced.
    // Cost: O(1) — one overlay hash probe.
    pub(super) fn local_cell_adoption_target(
        &self,
        code: &CompiledCode,
        idx: usize,
    ) -> Option<Value> {
        let slot = &self.locals[idx];
        if slot.is_container_ref()
            || slot.is_proxy_value()
            // A lazy Match counts as an Instance here — probed by tag so this
            // per-GetLocal check cannot materialize it.
            || slot.is_lazy_match_value()
            || matches!(
                slot.view(),
                ValueView::Package(_)
                    | ValueView::Array(..)
                    | ValueView::Hash(..)
                    | ValueView::Sub(..)
                    | ValueView::Instance { .. }
            )
        {
            return None;
        }
        // Probe via the pre-interned Symbol (this read runs on every slow
        // GetLocal — a by-name lookup would re-intern per read).
        let env_hit = match code.locals_sym.get(idx) {
            Some(sym) => self.env().overlay_get_sym(*sym),
            None => {
                let name = code.locals.get(idx).map(|s| s.as_str()).unwrap_or("");
                self.env().overlay_get(name)
            }
        }?;
        match env_hit.view() {
            ValueView::ContainerRef(arc) => Some(Value::container_ref(arc.clone())),
            ValueView::Proxy { .. } => Some(env_hit.clone()),
            _ => None,
        }
    }
}
