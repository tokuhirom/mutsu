//! Closing out a method call: which signals are the method's own result, and
//! the return-type check that applies to that result.

use super::*;

impl Interpreter {
    /// Turn the outcome of a finished method body into the call's result.
    ///
    /// A `return` signal that still names another routine (`CATCH { return }`
    /// reaching out of a bare block, `EVAL ..., context =>`) was declined by
    /// this boundary; it is not this method's return value, so it leaves
    /// untouched, never meeting this method's `--> T` check. A `succeed` that
    /// [`CompiledCode::lets_succeed_through`] unwinds past the method likewise.
    /// Any other `return` signal is the method's own explicit return: it is
    /// checked against `return_spec` when there is one, else absorbed.
    // Cost: O(1), plus the return-type check `finalize_return_with_spec` runs.
    pub(super) fn finalize_method_result(
        &mut self,
        cc: &CompiledCode,
        outcome: Result<Value, RuntimeError>,
        return_spec: Option<&str>,
    ) -> Result<Value, RuntimeError> {
        if let Err(e) = &outcome
            && (cc.lets_succeed_through(e) || e.is_targeted_return())
        {
            return outcome;
        }
        match return_spec {
            Some(spec) => self.finalize_return_with_spec(outcome, Some(spec)),
            None => match outcome {
                Err(e) if e.return_value.is_some() => Ok(e.return_value.unwrap()),
                other => other,
            },
        }
    }
}
