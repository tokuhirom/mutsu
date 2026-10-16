//! Carrying runtime-named writes across a light call's return.
//!
//! A write whose target name exists only at run time (`$::($n) = v`,
//! `CALLER::CALLER::.BIND-KEY($name, ...)`, an `EVAL`'d `$a = 32`) lands in
//! the callee's env and is invisible to `free_var_writes`. The named path
//! carries it into the caller env with `propagate_pending_caller_writes`; the
//! light paths used to rely on their plain-lexical return merge to do it.
//! These helpers carry it explicitly, so it does not depend on that merge
//! (ADR-12529 §3, slice 36).

use super::*;
use crate::opcode::CompiledFunction;

impl Interpreter {
    /// The pending runtime-named writes visible from the callee frame, read
    /// before the light return merge drops the frame. A name the callee
    /// declares itself is its own: it is dropped from the pending list.
    // Cost: O(w * d), w = pending runtime-named writes (usually 0),
    // d = env tiers walked to find each.
    pub(super) fn light_runtime_name_writes(
        &mut self,
        cf: &CompiledFunction,
    ) -> Vec<(Symbol, Value)> {
        if self.pending_runtime_name_writes.is_empty() {
            return Vec::new();
        }
        // A name the callee declares was its own: the write has reached its
        // binding, so it is not carried any further.
        self.pending_runtime_name_writes
            .retain(|n| !cf.is_callee_local_sym(Symbol::intern(n)));
        self.pending_runtime_name_writes
            .iter()
            .map(|name| Symbol::intern(name))
            .filter_map(|sym| {
                self.env()
                    .get_through_callers(sym.as_str())
                    .cloned()
                    .map(|v| (sym, v))
            })
            .collect()
    }

    /// Store the writes [`Self::light_runtime_name_writes`] read into the
    /// caller env the light return merge restored.
    // Cost: O(w), w = carried writes.
    pub(super) fn restore_light_runtime_name_writes(&mut self, carried: Vec<(Symbol, Value)>) {
        for (sym, value) in carried {
            self.env_mut().insert_sym(sym, value);
        }
    }

    /// Finish a positional light call's env: carry the runtime-named writes
    /// across the return, around the merge in
    /// [`Self::merge_positional_light_env`].
    pub(super) fn finish_positional_light_env(
        &mut self,
        cf: &CompiledFunction,
        caller_env: Option<crate::env::Env>,
    ) {
        let carried = self.light_runtime_name_writes(cf);
        self.merge_positional_light_env(cf, caller_env);
        self.restore_light_runtime_name_writes(carried);
    }

    /// [`Self::finish_positional_light_env`] for the typed light path, around
    /// [`Self::merge_light_env`].
    pub(super) fn finish_light_env(
        &mut self,
        cf: &CompiledFunction,
        caller_env: Option<crate::env::Env>,
    ) {
        let carried = self.light_runtime_name_writes(cf);
        self.merge_light_env(cf, caller_env);
        self.restore_light_runtime_name_writes(carried);
    }

    /// Carry a by-name write or `:=` of a dynamic the running code does not
    /// declare (`$*x = v` reaching a caller's `my $*x`, or its `$*x`
    /// parameter) back across every return, as a runtime-named write is
    /// (ADR-12529 §3, slice 40). The running code's own declaration is not
    /// recorded: its binding is right here.
    // Cost: O(1) for a non-dynamic name; else O(w), w = pending runtime-named
    // writes (the de-duplicating push).
    pub(super) fn record_foreign_dynamic_write(&mut self, code: &CompiledCode, sym: Symbol) {
        if sym.flags() & crate::symbol::flags::DYNAMIC_VAR_ENV_KEY == 0
            || code.dynamic_declared_sym.contains(&sym)
        {
            return;
        }
        sym.with_str(|name| self.record_runtime_name_write(name));
    }
}
