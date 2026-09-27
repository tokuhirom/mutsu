use super::*;

impl Interpreter {
    /// The core (non-mutating) implementation of a single-argument prefix
    /// operator, bypassing user `multi prefix:<op>` candidates.
    ///
    /// Shared by the call-fallback path (a user `multi prefix:<->` made `-$x`
    /// a call but none of its candidates took the argument) and by the final
    /// leg of `callsame`/`nextsame`/`callwith`/`nextwith` from a user prefix
    /// candidate: core operators are not `FunctionDef`s in the multi list, yet
    /// Raku exposes them as the next candidate (`multi prefix:<->(UInt $n) {
    /// callsame() mod $*modulus }`, the FiniteFields dist). Returns `None` for
    /// an operator this table does not cover.
    // Cost: O(1) dispatch, plus the operator's own cost on `arg`.
    pub(crate) fn core_prefix_op(
        &mut self,
        op: &str,
        arg: &Value,
    ) -> Option<Result<Value, RuntimeError>> {
        // An lvalue argument arrives tagged as a `VarRef` (the call site
        // cannot know no user candidate wants `is rw`); the core operators
        // read its value. Without this `-$x` computed `-(VarRef)`, i.e. 0.
        let arg = arg.unwrap_varref();
        Some(match op {
            "!" => Ok(Value::truth(!arg.truthy())),
            "+" => Ok(crate::runtime::coerce_to_numeric(arg.clone())),
            "-" | "−" => crate::builtins::arith_negate(arg.clone()),
            "~" => {
                if let Some(err) = self.failure_to_runtime_error_if_unhandled(arg) {
                    return Some(Err(err));
                }
                Ok(Value::str(crate::runtime::utils::coerce_to_str(arg)))
            }
            "?" | "so" => Ok(Value::truth(arg.truthy())),
            "not" => Ok(Value::truth(!arg.truthy())),
            _ => return None,
        })
    }
}
