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
            "!" => Ok(Value::truth(!self.eval_truthy(arg))),
            "+" => crate::runtime::require_numeric_candidate(arg)
                .map(|()| crate::runtime::coerce_to_numeric(arg.clone())),
            "-" | "−" => crate::builtins::arith_negate(arg.clone()),
            "~" => {
                if let Some(err) = self.failure_to_runtime_error_if_unhandled(arg) {
                    return Some(Err(err));
                }
                Ok(Value::str(crate::runtime::utils::coerce_to_str(arg)))
            }
            "?" | "so" => Ok(Value::truth(self.eval_truthy(arg))),
            "not" | "?^" => Ok(Value::truth(!self.eval_truthy(arg))),
            "+^" => crate::runtime::require_numeric_candidate(arg)
                .map(|()| crate::builtins::int_bitneg(&crate::runtime::coerce_to_numeric(arg.clone()))),
            "~^" => Ok(Self::str_bit_neg(arg)),
            "^" => Self::build_range_excl_value(Value::int(0), arg.clone()),
            _ => return None,
        })
    }

    /// The core implementation of a non-mutating postfix operator, bypassing
    /// user `multi postfix:<op>` candidates: the call fallback, and a
    /// captured-candidate dispatcher none of whose candidates took the
    /// argument (ADR-8751 stage 2). Returns `None` for an operator this table
    /// does not cover.
    // Cost: O(1) dispatch, plus the selected operator's cost on arg.
    pub(crate) fn core_postfix_op(
        &mut self,
        op: &str,
        arg: &Value,
    ) -> Option<Result<Value, RuntimeError>> {
        match op {
            "i" => Some(self.core_postfix_i(arg.unwrap_varref())),
            _ => None,
        }
    }

    // Cost: O(m) plus Numeric coercion, m = an instance's MRO depth.
    fn core_postfix_i(&mut self, arg: &Value) -> Result<Value, RuntimeError> {
        if !self.type_matches_value("Numeric", arg) && !self.type_matches_value("Cool", arg) {
            return Err(RuntimeError::typed_msg("X::Multi::NoMatch", format!(
                "Cannot resolve caller postfix:<i>({}); none of these signatures matches",
                crate::value::types::what_type_name(arg)
            )));
        }
        // An instance or type object coerces through its `.Numeric` first.
        let coerced = if matches!(
            arg.view(),
            ValueView::Instance { .. } | ValueView::Package(..)
        ) {
            self.call_method_with_values(arg.clone(), "Numeric", vec![])
                ?
        } else {
            arg.clone()
        };
        // Applying `i` to an existing Complex (directly, or via a `.Numeric`
        // coercion that returns one) rotates it 90° (multiplies by i):
        // `(r+ei)\i == -e+ri`.
        if let ValueView::Complex(r, i) = coerced.view() {
            return Ok(Value::complex(-i, r));
        }
        let n = crate::runtime::coerce_to_numeric(coerced);
        let num_val = match n.view() {
            ValueView::Int(i) => i as f64,
            ValueView::Num(f) => f,
            ValueView::Rat(n, d) => crate::value::rat_to_f64(n, d),
            _ => {
                return Err(RuntimeError::new(
                    "Cannot coerce to Numeric for postfix:<i>".to_string(),
                ));
            }
        };
        Ok(Value::complex(0.0, num_val))
    }
}
