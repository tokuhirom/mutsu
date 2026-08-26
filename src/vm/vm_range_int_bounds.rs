//! `Range.int-bounds($from is rw, $to is rw --> Bool)`.
//!
//! The zero-argument candidate is a pure value operation and lives in the
//! native arity cascade (`builtins/methods_0arg`). This two-argument candidate
//! cannot: it writes the bounds into the *caller's* containers and answers a
//! `Bool` saying whether the Range had integer bounds at all, so it needs the
//! call site's argument-source names (`pending_call_arg_sources`, the same
//! metadata an `is rw` parameter writeback uses) and a `&mut Interpreter`.
//! It is therefore served here, in the VM's native dispatch, rather than
//! through a `runtime/methods.rs` slow path.

use super::*;

impl Interpreter {
    /// True when this receiver/arity pair is the two-argument `int-bounds`.
    pub(super) fn is_range_int_bounds_rw(target: &Value, method: &str, args: &[Value]) -> bool {
        method == "int-bounds"
            && args.len() == 2
            && matches!(
                target.view(),
                ValueView::Range(..)
                    | ValueView::RangeExcl(..)
                    | ValueView::RangeExclStart(..)
                    | ValueView::RangeExclBoth(..)
                    | ValueView::GenericRange { .. }
            )
    }

    /// Bind the Range's integer bounds into the two caller-supplied containers
    /// and answer whether it has any. A Range without integer bounds leaves the
    /// arguments untouched (raku leaves them `Any`) and answers `False`.
    pub(super) fn range_int_bounds_rw(&mut self, target: &Value) -> Result<Value, RuntimeError> {
        let Some((from, to)) = crate::builtins::range_bounds_int::range_int_bounds(target) else {
            return Ok(Value::FALSE);
        };
        let sources = self.pending_call_arg_sources().cloned().unwrap_or_default();
        for (idx, value) in [from, to].into_iter().enumerate() {
            let Some(Some(name)) = sources.get(idx) else {
                continue;
            };
            // `insert_through` assigns to the container the name already
            // denotes, so an aliased slot (`$b := $a`) keeps its identity —
            // the same rule `is rw` parameter writeback follows.
            self.env_mut().insert_through(name.clone(), value);
            // The env write alone leaves the caller frame's local slot stale;
            // queue the name for the call site's writeback drain
            // (`drain_and_reconcile_after_cached_call`), which refreshes the
            // owning slot in whichever frame actually holds it.
            self.record_caller_var_writeback(name);
        }
        Ok(Value::TRUE)
    }
}
