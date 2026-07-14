//! Integer division/mod, gcd/lcm, infix min/max, string/list repeat (`xx`),
//! and function composition ops.
use super::*;
use std::sync::Arc;
use unicode_normalization::UnicodeNormalization;

impl Interpreter {
    fn is_xx_reeval_thunk(data: &crate::value::SubData) -> bool {
        if !data.params.is_empty()
            || !data.param_defs.is_empty()
            || !data.assumed_positional.is_empty()
            || !data.assumed_named.is_empty()
            || data.body.len() != 1
        {
            return false;
        }
        match &data.body[0] {
            crate::ast::Stmt::Expr(expr) => matches!(
                expr,
                crate::ast::Expr::Call { .. }
                    | crate::ast::Expr::MethodCall { .. }
                    | crate::ast::Expr::DynamicMethodCall { .. }
                    | crate::ast::Expr::HyperMethodCall { .. }
                    | crate::ast::Expr::HyperMethodCallDynamic { .. }
                    | crate::ast::Expr::CallOn { .. }
                    | crate::ast::Expr::Block(_)
                    | crate::ast::Expr::DoBlock { .. }
                    | crate::ast::Expr::AnonSub { .. }
                    | crate::ast::Expr::AnonSubParams { .. }
                    | crate::ast::Expr::Lambda { .. }
            ),
            _ => false,
        }
    }

    /// Fast path for `@var.shift xx N` and `@var.pop xx N`.
    /// Drains N elements in bulk instead of running a thunk N times.
    fn try_bulk_shift_pop(
        &mut self,
        data: &crate::value::SubData,
        n: usize,
    ) -> Result<Option<Vec<Value>>, RuntimeError> {
        use crate::ast::{Expr, Stmt};
        if data.body.len() != 1 {
            return Ok(None);
        }
        let (var_name, is_shift) = match &data.body[0] {
            Stmt::Expr(Expr::MethodCall {
                target, name, args, ..
            }) if args.is_empty() && (name.resolve() == "shift" || name.resolve() == "pop") => {
                let vname = match target.as_ref() {
                    Expr::Var(v) => v.clone(),
                    Expr::ArrayVar(v) => format!("@{}", v),
                    _ => return Ok(None),
                };
                (vname, name.resolve() == "shift")
            }
            _ => return Ok(None),
        };
        // Look up the array in env
        let arr_key = if var_name.starts_with('@') {
            var_name.clone()
        } else {
            format!("@{}", var_name)
        };
        let target_val = self
            .env()
            .get(&arr_key)
            .or_else(|| self.env().get(&var_name))
            .cloned();
        let Some((arc_items, kind)) = target_val.unwrap_or(Value::NIL).into_array() else {
            return Ok(None);
        };
        if !kind.is_real_array() {
            return Ok(None);
        }
        let mut items = (*arc_items).clone();
        let actual_n = n.min(items.len());
        let result = if is_shift {
            let rest = items.split_off(actual_n);
            let shifted = items;
            items = crate::value::ArrayData::new(rest);
            shifted
        } else {
            // pop
            let split_at = items.len().saturating_sub(actual_n);
            let popped: Vec<Value> = items.drain(split_at..).rev().collect();
            crate::value::ArrayData::new(popped)
        };
        // Write the mutated array back
        let lookup_key = if self.env().contains_key(&arr_key) {
            &arr_key
        } else {
            &var_name
        };
        self.env_mut().insert(
            lookup_key.to_string(),
            Value::array_with_kind(crate::gc::Gc::new(items), kind),
        );
        Ok(Some(result.items))
    }

    /// Interpreter-native implementation of xx-repeat for thunks.
    /// Compiles the thunk body once and runs it N times via `run_reuse`,
    /// avoiding the interpreter roundtrip through `eval_xx_repeat_thunk`.
    fn vm_xx_repeat_thunk(
        &mut self,
        data: &crate::value::SubData,
        n: usize,
    ) -> Result<Vec<Value>, RuntimeError> {
        // Save env keys that the thunk captures
        let touched_keys: Vec<String> = data.env.keys().map(|k| k.resolve()).collect();
        let saved: Vec<(String, Option<Value>)> = touched_keys
            .iter()
            .map(|k| (k.clone(), self.env().get(k).cloned()))
            .collect();

        // Install captured env
        for (k, v) in data.env.iter() {
            if matches!(
                self.env().get_sym(*k).map(Value::view),
                Some(ValueView::Array(..))
            ) && matches!(v.view(), ValueView::Array(..))
            {
                continue;
            }
            self.env_mut().insert_sym(*k, v.clone());
        }

        // Compile the thunk body
        let compiler = crate::compiler::Compiler::new();
        let (code, compiled_fns) = compiler.compile(&data.body);
        let code = Arc::new(code);

        // Run N times, collecting results
        let mut out = Vec::with_capacity(n);
        let saved_stack = std::mem::take(&mut self.stack);
        let saved_locals = std::mem::take(&mut self.locals);

        for _ in 0..n {
            match self.run_reuse(&code, &compiled_fns) {
                Ok(()) => {
                    let val = self.env().get("_").cloned().unwrap_or(Value::NIL);
                    out.push(val);
                }
                Err(e) => {
                    self.stack = saved_stack;
                    self.locals = saved_locals;
                    // Restore env
                    for (k, orig) in saved {
                        match orig {
                            Some(v) => {
                                self.env_mut().insert(k, v);
                            }
                            None => {
                                self.env_mut().remove(&k);
                            }
                        }
                    }
                    return Err(e);
                }
            }
        }

        self.stack = saved_stack;
        self.locals = saved_locals;
        // Restore env, but keep mutations to arrays (e.g. shift/pop
        // inside the thunk should persist).
        for (k, orig) in saved {
            // If the thunk mutated an array variable (e.g. via .shift),
            // keep the mutated version instead of restoring the original.
            let current = self.env().get(&k).cloned();
            let should_keep_current = matches!(
                (
                    orig.as_ref().map(Value::view),
                    current.as_ref().map(Value::view)
                ),
                (Some(ValueView::Array(..)), Some(ValueView::Array(..)))
            );
            if should_keep_current {
                continue;
            }
            match orig {
                Some(v) => {
                    self.env_mut().insert(k, v);
                }
                None => {
                    self.env_mut().remove(&k);
                }
            }
        }
        Ok(out)
    }

    pub(super) fn exec_int_div_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            let val = match (l.view(), r.view()) {
                (ValueView::Int(a), ValueView::Int(b)) if b != 0 => {
                    Value::int(num_integer::Integer::div_floor(&a, &b))
                }
                (ValueView::Int(a), ValueView::Int(_)) => {
                    RuntimeError::divide_by_zero_failure(Some(Value::int(a)), Some("div"))
                }
                (ValueView::BigInt(a), ValueView::BigInt(b))
                    if **b != num_bigint::BigInt::from(0i64) =>
                {
                    Value::from_bigint(num_integer::Integer::div_floor(a.as_ref(), b.as_ref()))
                }
                (ValueView::BigInt(a), ValueView::Int(b)) if b != 0 => {
                    let bb = num_bigint::BigInt::from(b);
                    Value::from_bigint(num_integer::Integer::div_floor(a.as_ref(), &bb))
                }
                (ValueView::Int(a), ValueView::BigInt(b))
                    if **b != num_bigint::BigInt::from(0i64) =>
                {
                    let aa = num_bigint::BigInt::from(a);
                    Value::from_bigint(num_integer::Integer::div_floor(&aa, b.as_ref()))
                }
                _ => {
                    let a = runtime::to_int(&l);
                    let b = runtime::to_int(&r);
                    if b == 0 {
                        return Ok(RuntimeError::divide_by_zero_failure(
                            Some(Value::int(a)),
                            Some("div"),
                        ));
                    }
                    Value::int(num_integer::Integer::div_floor(&a, &b))
                }
            };
            Ok(val)
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_int_mod_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            let val = match (l.view(), r.view()) {
                (ValueView::Int(a), ValueView::Int(b)) if b != 0 => {
                    Value::int(num_integer::Integer::mod_floor(&a, &b))
                }
                (ValueView::Int(a), ValueView::Int(_)) => {
                    RuntimeError::divide_by_zero_failure(Some(Value::int(a)), Some("%"))
                }
                (ValueView::BigInt(a), ValueView::BigInt(b)) if !b.is_zero() => {
                    Value::from_bigint(num_integer::Integer::mod_floor(a.as_ref(), b.as_ref()))
                }
                (ValueView::BigInt(a), ValueView::Int(b)) if b != 0 => {
                    let bb = num_bigint::BigInt::from(b);
                    Value::from_bigint(num_integer::Integer::mod_floor(a.as_ref(), &bb))
                }
                (ValueView::Int(a), ValueView::BigInt(b)) if !b.is_zero() => {
                    let aa = num_bigint::BigInt::from(a);
                    Value::from_bigint(num_integer::Integer::mod_floor(&aa, b.as_ref()))
                }
                _ => {
                    let a = runtime::to_int(&l);
                    let b = runtime::to_int(&r);
                    if b == 0 {
                        return Ok(RuntimeError::divide_by_zero_failure(
                            Some(Value::int(a)),
                            Some("%"),
                        ));
                    }
                    Value::int(num_integer::Integer::mod_floor(&a, &b))
                }
            };
            Ok(val)
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_gcd_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let a = left.to_bigint().abs();
        let b = right.to_bigint().abs();
        let g = num_integer::Integer::gcd(&a, &b);
        self.stack.push(Value::from_bigint(g));
    }

    pub(super) fn exec_lcm_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let a = left.to_bigint().abs();
        let b = right.to_bigint().abs();
        let result = if a.is_zero() && b.is_zero() {
            Value::int(0)
        } else {
            let g = num_integer::Integer::gcd(&a, &b);
            Value::from_bigint(&a / &g * &b)
        };
        self.stack.push(result);
    }

    pub(super) fn exec_infix_min_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        if matches!(left.view(), ValueView::Package(name) if name == "Any") {
            self.stack.push(right);
            return;
        }
        if matches!(right.view(), ValueView::Package(name) if name == "Any") {
            self.stack.push(left);
            return;
        }
        let left_is_failure = matches!(left.view(), ValueView::Instance { class_name, .. } if class_name == "Failure");
        let right_is_failure = matches!(right.view(), ValueView::Instance { class_name, .. } if class_name == "Failure");
        if left_is_failure {
            self.stack.push(left);
            return;
        }
        if right_is_failure {
            self.stack.push(right);
            return;
        }
        let ord = cmp_values(&left, &right);
        self.stack.push(if ord.is_le() { left } else { right });
    }

    pub(super) fn exec_infix_max_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        if matches!(left.view(), ValueView::Package(name) if name == "Any") {
            self.stack.push(right);
            return;
        }
        if matches!(right.view(), ValueView::Package(name) if name == "Any") {
            self.stack.push(left);
            return;
        }
        let left_is_failure = matches!(left.view(), ValueView::Instance { class_name, .. } if class_name == "Failure");
        let right_is_failure = matches!(right.view(), ValueView::Instance { class_name, .. } if class_name == "Failure");
        if left_is_failure {
            self.stack.push(left);
            return;
        }
        if right_is_failure {
            self.stack.push(right);
            return;
        }
        let ord = cmp_values(&left, &right);
        self.stack.push(if ord.is_ge() { left } else { right });
    }

    pub(super) fn exec_string_repeat_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();

        // Warn on uninitialized type object used as repeat count
        if let ValueView::Package(name) = right.view()
            && name == "Int"
            && !self.warning_suppressed()
        {
            self.write_warn_to_stderr(&format!(
                "Use of uninitialized value of type {} in numeric context",
                name
            ));
        }

        // Whatever on RHS produces a WhateverCode closure
        if matches!(right.view(), ValueView::Whatever) {
            self.stack.push(self.make_x_whatevercode(left));
            return Ok(());
        }

        let Some(n_raw) = crate::runtime::Interpreter::parse_repeat_count(&right)? else {
            return Err(crate::runtime::Interpreter::repeat_error(
                "X::Numeric::CannotConvert",
                "Cannot convert Inf to Int".to_string(),
            ));
        };
        let n = n_raw.max(0) as usize;
        let src = crate::runtime::utils::coerce_to_str(&left);
        // Guard the allocation: `str::repeat` aborts the process via
        // `handle_alloc_error` on an absurd count (e.g. `"x" x 1e15`), which
        // `try {}` cannot recover from. Reserve fallibly first so the same
        // input yields a catchable `X::` instead. (raku aborts here too.)
        let total = src
            .len()
            .checked_mul(n)
            .ok_or_else(|| RuntimeError::new("Cannot repeat string: length overflow"))?;
        // Build by doubling (`extend_from_within` = one memcpy per doubling)
        // instead of `n` per-copy `push_str` calls: the roast A01-limits test
        // declares `"a" x 2**32-1` (a 4 GiB string), which must complete in
        // seconds, not minutes.
        let mut buf: Vec<u8> = Vec::new();
        buf.try_reserve_exact(total).map_err(|_| {
            RuntimeError::new(format!(
                "Cannot repeat string to {total} bytes: memory allocation failed"
            ))
        })?;
        if total > 0 {
            buf.extend_from_slice(src.as_bytes());
            while buf.len() < total {
                let take = (total - buf.len()).min(buf.len());
                buf.extend_from_within(..take);
            }
        }
        // SAFETY: `buf` is `src.as_bytes()` (valid UTF-8) repeated whole times;
        // a concatenation of valid UTF-8 strings is valid UTF-8. Skipping the
        // validation scan matters at this size (multi-GiB).
        let repeated = unsafe { String::from_utf8_unchecked(buf) };
        let result = if repeated.is_ascii() {
            Value::str(repeated)
        } else {
            Value::str(repeated.nfc().collect::<String>())
        };
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_list_repeat_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();

        // Fast path for xx-reeval thunks (compiler-generated repeat blocks)
        let thunk = match left.view() {
            ValueView::Sub(data) => Some(data.clone()),
            ValueView::WeakSub(weak) => weak.upgrade(),
            _ => None,
        };
        if let Some(data) = thunk.filter(|data| Self::is_xx_reeval_thunk(data.as_ref()))
            && let ValueView::Int(n) = right.view()
        {
            let n = n.max(0) as usize;
            // Fast path: @var.shift xx N or @var.pop xx N — bulk drain
            if let Some(items) = self.try_bulk_shift_pop(&data, n)? {
                self.stack.push(Value::seq(items));
                return Ok(());
            }
            let items = self.vm_xx_repeat_thunk(data.as_ref(), n)?;
            self.stack.push(Value::seq(items));
            return Ok(());
        }

        // Warn on uninitialized type object used as repeat count
        if let ValueView::Package(name) = right.view()
            && name == "Int"
            && !self.warning_suppressed()
        {
            self.write_warn_to_stderr(&format!(
                "Use of uninitialized value of type {} in numeric context",
                name
            ));
        }

        const EAGER_LIMIT: usize = 10_000;
        const LAZY_CACHE: usize = 4_096;
        const LAZY_CACHE_CALLABLE: usize = 256;

        let is_callable = matches!(
            left.view(),
            ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
        );
        let lazy_cache = if is_callable {
            LAZY_CACHE_CALLABLE
        } else {
            LAZY_CACHE
        };

        let count = crate::runtime::Interpreter::parse_repeat_count(&right)?;
        let (repeat, lazy) = match count {
            Some(n) if n <= 0 => (0usize, false),
            Some(n) if (n as usize) <= EAGER_LIMIT => (n as usize, false),
            Some(n) => ((n as usize).min(lazy_cache), true),
            None => (lazy_cache, true),
        };

        let mut items = Vec::with_capacity(repeat);
        if let ValueView::Slip(slip_items) = left.view() {
            if slip_items.is_empty() {
                items.extend(std::iter::repeat_n(Value::NIL, repeat));
            } else {
                for _ in 0..repeat {
                    items.extend(slip_items.iter().cloned());
                }
            }
        } else {
            for _ in 0..repeat {
                items.push(self.repeat_lhs_once(&left)?);
            }
        }

        let result = if lazy {
            let count = crate::runtime::Interpreter::repeat_logical_count(&right);
            crate::runtime::Interpreter::make_repeat_lazy_cache_counted(items, count)
        } else {
            Value::seq(items)
        };
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_function_compose_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let composed = self.compose_callables(left, right);
        self.stack.push(composed);
    }
}
