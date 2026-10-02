//! Calling a subset's `where` predicate as a closure.

use super::*;

impl Interpreter {
    /// Run a callable subset predicate on `arg` and report whether it accepts
    /// it. A predicate that dies, or returns an unhandled `Failure` (`fail
    /// "msg"` in its body), rejects the value and records why in
    /// `subset_where_fail` so the type-check error can report it.
    // Cost: O(1) plus the predicate's own body.
    pub(super) fn call_subset_predicate(&mut self, callable: Value, arg: Value) -> bool {
        let compiled =
            matches!(callable.view(), ValueView::Sub(ref data) if data.compiled_code.is_some());
        let called = if compiled {
            self.vm_call_on_value(callable, vec![arg], None)
        } else {
            self.call_sub_value(callable, vec![arg], false)
        };
        match called {
            Ok(v) => {
                if let Some(e) = self.failure_to_runtime_error_if_unhandled(&v) {
                    self.record_subset_where_fail(e);
                    false
                } else {
                    v.truthy()
                }
            }
            Err(e) => {
                self.record_subset_where_fail(e);
                false
            }
        }
    }
}
