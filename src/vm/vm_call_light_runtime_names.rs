//! Carrying runtime-named writes across a light call's return.
//!
//! A write whose target name exists only at run time (`$::($n) = v`,
//! `CALLER::CALLER::.BIND-KEY($name, ...)`, an `EVAL`'d `$a = 32`) lands in
//! the callee's env and is invisible to `free_var_writes`. The named path
//! carries it into the caller env with `propagate_pending_caller_writes`; the
//! light paths carry it here (ADR-12529 §3, slice 36). No return copies the
//! callee's other bindings back (slice 41).

use super::*;
use crate::opcode::CompiledFunction;

impl Interpreter {
    /// The pending runtime-named writes visible from the callee frame, read
    /// before the light return drops the frame. A name the callee
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
    /// caller env the light return restored.
    // Cost: O(w), w = carried writes.
    pub(super) fn restore_light_runtime_name_writes(&mut self, carried: Vec<(Symbol, Value)>) {
        for (sym, value) in carried {
            self.env_mut().insert_sym(sym, value);
        }
    }

    /// Finish a light call's env: drop the callee frame's bindings and carry
    /// the runtime-named writes across the return.
    ///
    /// Nothing the callee bound is copied back (ADR-12529 §3, slice 41): an
    /// outer lexical is reached through the static link and written through
    /// its shared cell, and a write whose name exists only at run time (or a
    /// rebind of a caller's dynamic) is carried by
    /// [`Self::light_runtime_name_writes`]. The swap path restores the
    /// caller's env; a reused frame (the caller's overlay was the shared
    /// empty singleton at entry) drops what the call wrote in place, through
    /// the flatten's write log when the body flattened the env (#7630).
    /// Shared by the normal completion and the panic-unwind recovery arm of
    /// both light paths (`news/2026-08/light-call-state-leak-on-panic-unwind.md`).
    // Cost: O(w + c), w = pending runtime-named writes, c = names the call
    // wrote into a reused frame's env.
    pub(super) fn finish_light_env(
        &mut self,
        cf: &CompiledFunction,
        caller_env: Option<crate::env::Env>,
    ) {
        let carried = self.light_runtime_name_writes(cf);
        match caller_env {
            Some(caller_env) => *self.env_mut() = caller_env,
            None => {
                if !self.env_mut().retain_frame_writes(|_| false)
                    && !self.env().overlay_is_shared_empty()
                {
                    self.env_mut().retain_overlay(|_, _| false);
                }
            }
        }
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
