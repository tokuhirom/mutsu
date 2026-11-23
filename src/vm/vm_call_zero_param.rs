//! A call of a routine with no parameter at all (`sub f() { }`, or `sub f { }`
//! whose body reads no `@_`/`%_`), through `call_compiled_function_fast`.
//!
//! Two dispatch sites share this: the general path's resolution-cache hit,
//! which also installs the name-keyed `pos_light_call_cache` entry, and that
//! cache's own hit path, which tries TRIR first (#18080). The routine gets no
//! positional-light bind: `call_compiled_function_fast` is the path that
//! serves what a parameterless body may still need from its frame (a
//! `$CALLER::` read, the backtrace frame, placeholder-free legacy shapes).
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Whether a routine the name-keyed light cache handed back is a
    /// parameterless one this module serves, not a positional-light one.
    /// The light gate admits only a non-empty `param_defs`, so the first test
    /// already separates the two; the rest re-states the install condition.
    // Cost: O(1).
    #[inline]
    pub(super) fn is_cached_zero_param_routine(cf: &CompiledFunction, name: &str) -> bool {
        cf.param_defs.is_empty() && !cf.is_raw && Self::is_fast_call_eligible(cf, name)
    }

    /// Call the parameterless routine `cf` with the `arity` values on top of
    /// the stack. A parenthesized call carries only the parser's callsite-line
    /// marker, whose line is recorded; any other value is a surplus positional
    /// and raises the general binder's "Too many positionals" error.
    // Cost: O(a) plus the callee's body, a = arity.
    pub(super) fn call_zero_param_routine(
        &mut self,
        code: &CompiledCode,
        cf: &CompiledFunction,
        name_str: &str,
        name_sym: Symbol,
        arity: usize,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        if self.stack.len() >= arity && arity > 0 {
            let start = self.stack.len() - arity;
            let popped: Vec<Value> = self.stack.drain(start..).collect();
            let cl = Self::peek_callsite_line(&popped);
            if cl.is_some() {
                loan_env!(self, set_pending_callsite_line(cl));
            } else {
                // A popped value that is NOT the synthetic callsite-line
                // marker is a genuine over-supplied positional argument.
                // Raise the same "Too many positionals passed" wording the
                // general binder produces (binding_signature.rs) -- several
                // call sites pattern-match on this exact message.
                return Err(RuntimeError::typed(
                    "X::TypeCheck::Argument",
                    Self::type_check_argument_attrs(
                        name_str,
                        &cf.param_defs,
                        &popped,
                        format!(
                            "Too many positionals passed; expected 0 arguments but got {}",
                            popped.len()
                        ),
                    ),
                ));
            }
        }
        let result = match self.call_compiled_function_fast(cf, name_str, name_sym, compiled_fns) {
            Ok(v) => v,
            Err(e) => {
                // Slice F (exception-escape coherence): an exceptional exit
                // (`die`/`fail`) still ran the callee's UNDO/LEAVE phasers,
                // which can mutate a captured-outer variable (e.g. `UNDO {
                // $ng ~= "U" }`). The body recorded those writes into
                // `pending_rw_writeback_sources`; drain them to this caller's
                // local slots *before* propagating the error, exactly as the
                // Ok path does, so the reverse `sync_locals_from_env` pull is
                // not required for coherence.
                self.apply_pending_rw_writeback(code);
                return Err(e);
            }
        };
        self.stack.push(result);
        // Slice F: write any captured-outer variables the callee mutated
        // straight through to this caller's local slots, so they stay
        // coherent without the reverse `sync_locals_from_env` pull. The
        // env_dirty-gated reconcile additionally catches a *nested* callee's
        // captured-outer write (`via()` -> `bump-outer()` -> `$acc`), which
        // the single-frame drain discards one frame too deep, so
        // `via(); via()` accumulates correctly. No blanket env_dirty:
        // `call_compiled_function_fast` signals it precisely.
        self.drain_and_reconcile_after_cached_call(code);
        Ok(())
    }
}
