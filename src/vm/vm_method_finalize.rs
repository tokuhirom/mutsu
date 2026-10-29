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

    /// [`Self::finalize_method_result`] for a call whose `--> T` names a plain
    /// core type that resolves to itself ([`Self::method_return_fast`]): an `Ok`
    /// value that is neither a container nor a Proxy and satisfies `T` is the
    /// result as is. Anything else -- an error outcome, a container, a value
    /// the type refuses -- takes the general path with `return_type` as the
    /// resolved spec (it names itself, so resolution is the identity).
    // Cost: O(1) for a fast return type, else as `finalize_method_result`.
    pub(super) fn finalize_method_result_fast(
        &mut self,
        cc: &CompiledCode,
        outcome: Result<Value, RuntimeError>,
        return_type: Option<&str>,
        effective_spec: Option<&str>,
        fast: Option<(crate::opcode::FastParamType, crate::symbol::Symbol)>,
    ) -> Result<Value, RuntimeError> {
        let Some((kind, name_sym)) = fast else {
            return self.finalize_method_result(cc, outcome, effective_spec);
        };
        if let Ok(v) = &outcome
            && !v.is_container_ref()
            && !v.is_proxy_value()
            && self.light_return_check(v, kind, name_sym)
        {
            return outcome;
        }
        self.finalize_method_result(cc, outcome, return_type)
    }
}
