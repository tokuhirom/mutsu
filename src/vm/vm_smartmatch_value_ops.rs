use super::*;

impl Interpreter {
    // Cost: O(1).
    pub(super) fn exec_scalarize_regex_match_result_op(&mut self) -> Result<(), RuntimeError> {
        let value = self.stack.pop().unwrap_or(Value::NIL);
        let scalarized = match value.view() {
            ValueView::Nil => Value::int(0),
            ValueView::Array(items, _) => Value::int(items.len() as i64),
            ValueView::Seq(items) | ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => {
                Value::int(items.len() as i64)
            }
            ValueView::Slip(items) => Value::int(items.len() as i64),
            ValueView::Capture { positional, .. } => Value::int(positional.len() as i64),
            ValueView::Instance { .. } if value.is_match_instance() => {
                let list_v = value.match_list();
                if let Some(ValueView::Array(items, _)) = list_v.as_ref().map(Value::view) {
                    Value::int(items.len() as i64)
                } else {
                    Value::int(1)
                }
            }
            _ if value.truthy() => Value::int(1),
            _ => Value::int(0),
        };
        self.stack.push(scalarized);
        Ok(())
    }

    // Cost: O(d + c), d = modulo cost, c = operator dispatch and any user call.
    pub(super) fn exec_divisible_by_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        // Thread over junctions
        if matches!(left.view(), ValueView::Junction { .. })
            || matches!(right.view(), ValueView::Junction { .. })
        {
            let result = self
                .eval_binary_with_junctions(left, right, |vm, l, r| vm.divisible_by_values(l, r))?;
            self.stack.push(result);
            return Ok(());
        }
        let result = self.divisible_by_values(left, right)?;
        self.stack.push(result);
        Ok(())
    }

    /// `$a %% $b` is `$a % $b == 0`. Compute the modulo with the exact-rational
    /// semantics of `arith_mod` (so Rat/Num/BigInt operands work, not just Int),
    /// then test the remainder for zero. A zero divisor is a soft `Failure`
    /// (dividend and `infix:<%%>`, matching Rakudo) — it is returned, not
    /// thrown, so `grep: 10 %% *` treats it as false instead of dying.
    fn divisible_by_values(&mut self, left: Value, right: Value) -> Result<Value, RuntimeError> {
        if let Some(value) = self.try_user_infix("infix:<%%>", &left, &right)? {
            return Ok(value);
        }
        let (l, r) = runtime::coerce_numeric(left.clone(), right);
        if !r.truthy() {
            return Ok(RuntimeError::divide_by_zero_failure(
                Some(left),
                Some("infix:<%%>"),
            ));
        }
        let remainder = crate::builtins::arith_mod(l, r)?;
        if Self::is_failure_value(&remainder) {
            return Ok(remainder);
        }
        Ok(Value::truth(!remainder.truthy()))
    }

    // Cost: O(d + c), d = modulo cost, c = operator dispatch and any user call.
    pub(super) fn exec_not_divisible_by_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        // Thread over junctions
        if matches!(left.view(), ValueView::Junction { .. })
            || matches!(right.view(), ValueView::Junction { .. })
        {
            let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
                vm.not_divisible_by_values(l, r)
            })?;
            self.stack.push(result);
            return Ok(());
        }
        let result = self.not_divisible_by_values(left, right)?;
        self.stack.push(result);
        Ok(())
    }

    /// `$a !%% $b` is `not ($a %% $b)`. Negating a `Failure` in a boolean
    /// context marks it handled and reads as `False` (so `!%%` itself never
    /// dies on a zero divisor), matching Rakudo's `6 !%% 0` => `True`.
    fn not_divisible_by_values(
        &mut self,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        let result = self.divisible_by_values(left, right)?;
        if Self::is_failure_value(&result) {
            result.mark_failure_handled();
        }
        Ok(Value::truth(!result.truthy()))
    }
}
