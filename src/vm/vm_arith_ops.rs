use super::*;
use num_traits::ToPrimitive;
use std::sync::Arc;
use unicode_normalization::UnicodeNormalization;

impl VM {
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

    /// VM-native implementation of xx-repeat for thunks.
    /// Compiles the thunk body once and runs it N times via `run_reuse`,
    /// avoiding the interpreter roundtrip through `eval_xx_repeat_thunk`.
    fn vm_xx_repeat_thunk(
        &mut self,
        data: &crate::value::SubData,
        n: usize,
    ) -> Result<Vec<Value>, RuntimeError> {
        // Save env keys that the thunk captures
        let touched_keys: Vec<String> = data.env.keys().cloned().collect();
        let saved: Vec<(String, Option<Value>)> = touched_keys
            .iter()
            .map(|k| (k.clone(), self.interpreter.env().get(k).cloned()))
            .collect();

        // Install captured env
        for (k, v) in &data.env {
            if matches!(self.interpreter.env().get(k), Some(Value::Array(..)))
                && matches!(v, Value::Array(..))
            {
                continue;
            }
            self.interpreter.env_mut().insert(k.clone(), v.clone());
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
                    let val = self
                        .interpreter
                        .env()
                        .get("_")
                        .cloned()
                        .unwrap_or(Value::Nil);
                    out.push(val);
                }
                Err(e) => {
                    self.stack = saved_stack;
                    self.locals = saved_locals;
                    // Restore env
                    for (k, orig) in saved {
                        match orig {
                            Some(v) => {
                                self.interpreter.env_mut().insert(k, v);
                            }
                            None => {
                                self.interpreter.env_mut().remove(&k);
                            }
                        }
                    }
                    return Err(e);
                }
            }
        }

        self.stack = saved_stack;
        self.locals = saved_locals;
        // Restore env
        for (k, orig) in saved {
            match orig {
                Some(v) => {
                    self.interpreter.env_mut().insert(k, v);
                }
                None => {
                    self.interpreter.env_mut().remove(&k);
                }
            }
        }
        Ok(out)
    }

    pub(super) fn exec_add_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            // Try user-defined infix:<+> before any coercion so typed multi
            // candidates (e.g. `multi sub infix:<+>(Foo $x, Foo $y)`) can match.
            if let Some(result) = vm.try_user_infix("infix:<+>", &l, &r)? {
                return Ok(result);
            }
            // Handle Date/Instant arithmetic before numeric coercion
            if crate::builtins::arith::is_temporal_operand(&l)
                || crate::builtins::arith::is_temporal_operand(&r)
            {
                return crate::builtins::arith_add(l, r);
            }
            let (l, r) = vm.coerce_numeric_bridge_pair(l, r)?;
            crate::builtins::arith_add(l, r)
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_sub_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            // Try user-defined infix:<-> before any coercion so typed multi
            // candidates can match.
            if let Some(result) = vm.try_user_infix("infix:<->", &l, &r)? {
                return Ok(result);
            }
            // Handle Date/Instant arithmetic before numeric coercion
            if crate::builtins::arith::is_temporal_operand(&l)
                || crate::builtins::arith::is_temporal_operand(&r)
            {
                return Ok(crate::builtins::arith_sub(l, r));
            }
            let (l, r) = vm.coerce_numeric_bridge_pair(l, r)?;
            Ok(crate::builtins::arith_sub(l, r))
        })?;
        self.stack.push(result);
        Ok(())
    }

    /// Try to dispatch a binary operation to a user-defined infix operator.
    /// Returns Some(result) if a user-defined candidate matched, None otherwise.
    pub(super) fn try_user_infix(
        &mut self,
        op_name: &str,
        left: &Value,
        right: &Value,
    ) -> Result<Option<Value>, RuntimeError> {
        let args = vec![left.clone(), right.clone()];
        if let Some(def) = self.interpreter.resolve_function_with_types(op_name, &args) {
            let empty_fns = HashMap::new();
            let result = self.compile_and_call_function_def(&def, args, &empty_fns)?;
            return Ok(Some(result));
        }
        Ok(None)
    }

    pub(super) fn exec_mul_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            let (l, r) = vm.coerce_numeric_bridge_pair(l, r)?;
            Ok(crate::builtins::arith_mul(l, r))
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_div_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            let (l, r) = vm.coerce_numeric_bridge_pair(l, r)?;
            crate::builtins::arith_div(l, r)
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_mod_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            let (l, r) = vm.coerce_numeric_bridge_pair(l, r)?;
            crate::builtins::arith_mod(l, r)
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_pow_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            let (l, r) = vm.coerce_numeric_bridge_pair(l, r)?;
            Ok(crate::builtins::arith_pow(l, r))
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_negate_op(&mut self) -> Result<(), RuntimeError> {
        let val = self.stack.pop().unwrap();
        // Type objects (Mu, Any, etc.) cannot be negated
        if let Value::Package(name) = &val
            && matches!(name.resolve().as_str(), "Mu" | "Any")
        {
            return Err(RuntimeError::new(format!(
                "Cannot resolve caller prefix:<->({}:U); none of these signatures matches:\n    (\\a)",
                name.resolve()
            )));
        }
        let val = self.coerce_numeric_bridge_value(val)?;
        self.stack.push(crate::builtins::arith_negate(val)?);
        Ok(())
    }

    pub(super) fn exec_int_bit_neg_op(&mut self) {
        let val = self.stack.pop().unwrap();
        match val {
            Value::Int(n) => self.stack.push(Value::Int(!n)),
            other => {
                let n = other.to_bigint();
                self.stack.push(Value::from_bigint(!n));
            }
        }
    }

    pub(super) fn exec_bool_bit_neg_op(&mut self) {
        let val = self.stack.pop().unwrap();
        self.stack.push(Value::Bool(!val.truthy()));
    }

    pub(super) fn exec_str_bit_neg_op(&mut self) {
        let val = self.stack.pop().unwrap();
        if Self::is_buf_value(&val) {
            let bytes = Self::extract_buf_bytes(&val);
            let negated: Vec<Value> = bytes.iter().map(|b| Value::Int((!b) as i64)).collect();
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("bytes".to_string(), Value::array(negated));
            // Determine result type: if input is utf8, result is utf8; otherwise Buf
            let result_type = if let Value::Instance { class_name, .. } = &val {
                class_name.resolve().to_string()
            } else {
                "Buf".to_string()
            };
            self.stack
                .push(Value::make_instance(Symbol::intern(&result_type), attrs));
        } else {
            let s = crate::runtime::utils::coerce_to_str(&val);
            let negated: Vec<u8> = s.as_bytes().iter().map(|b| !b).collect();
            let result = String::from_utf8_lossy(&negated).into_owned();
            self.stack.push(Value::str(result));
        }
    }

    pub(super) fn exec_decont_op(&mut self) {
        let val = self.stack.pop().unwrap();
        let new_val = match val {
            Value::Scalar(inner) => *inner,
            other => other,
        };
        self.stack.push(new_val);
    }

    pub(super) fn exec_make_slip_op(&mut self) {
        let val = self.stack.pop().unwrap();
        let items = match &val {
            Value::Array(items, ..) => (*items).to_vec(),
            Value::Slip(items) => (*items).to_vec(),
            Value::Seq(items) => (*items).to_vec(),
            Value::Capture { positional, named } => {
                let mut items = positional.clone();
                for (k, v) in named {
                    items.push(Value::Pair(k.clone(), Box::new(v.clone())));
                }
                items
            }
            Value::Hash(map) => map
                .iter()
                .map(|(k, v)| Value::Pair(k.clone(), Box::new(v.clone())))
                .collect(),
            Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. } => crate::runtime::utils::value_to_list(&val),
            _ => vec![val],
        };
        self.stack.push(Value::slip(items));
    }

    pub(super) fn exec_not_op(&mut self) {
        let val = self.stack.pop().unwrap();
        // Boolifying a Failure marks it as handled
        val.mark_failure_handled();
        let t = self.eval_truthy(&val);
        self.stack.push(Value::Bool(!t));
    }

    pub(super) fn exec_bool_coerce_op(&mut self) {
        let val = self.stack.pop().unwrap();
        let out = match &val {
            Value::Regex(_)
            | Value::RegexWithAdverbs { .. }
            | Value::Routine { is_regex: true, .. } => {
                let topic = self
                    .interpreter
                    .env()
                    .get("_")
                    .cloned()
                    .unwrap_or(Value::Nil);
                Value::Bool(self.vm_smart_match(&topic, &val))
            }
            _ => {
                // Boolifying a Failure marks it as handled
                val.mark_failure_handled();
                Value::Bool(val.truthy())
            }
        };
        self.stack.push(out);
    }

    pub(super) fn exec_concat_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        // Thread over junctions
        if matches!(left, Value::Junction { .. }) || matches!(right, Value::Junction { .. }) {
            let result = self
                .eval_binary_with_junctions(left, right, |vm, l, r| Ok(vm.concat_values(l, r)))
                .unwrap_or(Value::Nil);
            self.stack.push(result);
            return;
        }
        let result = self.concat_values(left, right);
        self.stack.push(result);
    }

    fn concat_values(&self, left: Value, right: Value) -> Value {
        // Buf ~ Buf → Buf (byte concatenation, preserving LHS type)
        if Self::is_buf_value(&left) && Self::is_buf_value(&right) {
            let result_class = if let Value::Instance { class_name, .. } = &left {
                *class_name
            } else {
                crate::symbol::Symbol::intern("Buf")
            };
            let mut bytes = Self::extract_buf_bytes(&left);
            bytes.extend(Self::extract_buf_bytes(&right));
            let byte_vals: Vec<Value> = bytes.into_iter().map(|b| Value::Int(b as i64)).collect();
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("bytes".to_string(), Value::array(byte_vals));
            return Value::make_instance(result_class, attrs);
        }
        // Buf ~ non-Buf or non-Buf ~ Buf: decode the Buf and produce a Str
        if Self::is_buf_value(&left) || Self::is_buf_value(&right) {
            let left_str = if Self::is_buf_value(&left) {
                let bytes = Self::extract_buf_bytes(&left);
                String::from_utf8_lossy(&bytes).into_owned()
            } else {
                crate::runtime::utils::coerce_to_str(&left)
            };
            let right_str = if Self::is_buf_value(&right) {
                let bytes = Self::extract_buf_bytes(&right);
                String::from_utf8_lossy(&bytes).into_owned()
            } else {
                crate::runtime::utils::coerce_to_str(&right)
            };
            let concatenated = format!("{}{}", left_str, right_str);
            let normalized: String = concatenated.nfc().collect();
            return Value::str(normalized);
        }
        let concatenated = format!(
            "{}{}",
            crate::runtime::utils::coerce_to_str(&left),
            crate::runtime::utils::coerce_to_str(&right)
        );
        let normalized: String = concatenated.nfc().collect();
        Value::str(normalized)
    }

    pub fn is_buf_value(val: &Value) -> bool {
        if let Value::Instance { class_name, .. } = val {
            let cn = class_name.resolve();
            cn == "Buf"
                || cn == "Blob"
                || cn == "utf8"
                || cn == "utf16"
                || cn.starts_with("Buf[")
                || cn.starts_with("Blob[")
                || cn.starts_with("buf")
                || cn.starts_with("blob")
        } else {
            false
        }
    }

    pub fn extract_buf_bytes(val: &Value) -> Vec<u8> {
        if let Value::Instance { attributes, .. } = val
            && let Some(Value::Array(items, ..)) = attributes.get("bytes")
        {
            return items
                .iter()
                .map(|v| match v {
                    Value::Int(i) => *i as u8,
                    _ => 0,
                })
                .collect();
        }
        Vec::new()
    }

    pub(super) fn exec_int_div_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            let val = match (&l, &r) {
                (Value::Int(a), Value::Int(b)) if *b != 0 => {
                    Value::Int(num_integer::Integer::div_floor(a, b))
                }
                (Value::Int(a), Value::Int(_)) => {
                    RuntimeError::divide_by_zero_failure(Some(Value::Int(*a)), Some("div"))
                }
                (Value::BigInt(a), Value::BigInt(b)) if **b != num_bigint::BigInt::from(0i64) => {
                    Value::from_bigint(num_integer::Integer::div_floor(a.as_ref(), b.as_ref()))
                }
                (Value::BigInt(a), Value::Int(b)) if *b != 0 => {
                    let bb = num_bigint::BigInt::from(*b);
                    Value::from_bigint(num_integer::Integer::div_floor(a.as_ref(), &bb))
                }
                (Value::Int(a), Value::BigInt(b)) if **b != num_bigint::BigInt::from(0i64) => {
                    let aa = num_bigint::BigInt::from(*a);
                    Value::from_bigint(num_integer::Integer::div_floor(&aa, b.as_ref()))
                }
                _ => {
                    let a = runtime::to_int(&l);
                    let b = runtime::to_int(&r);
                    if b == 0 {
                        return Ok(RuntimeError::divide_by_zero_failure(
                            Some(Value::Int(a)),
                            Some("div"),
                        ));
                    }
                    Value::Int(num_integer::Integer::div_floor(&a, &b))
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
            let val = match (&l, &r) {
                (Value::Int(a), Value::Int(b)) if *b != 0 => {
                    Value::Int(num_integer::Integer::mod_floor(a, b))
                }
                (Value::Int(a), Value::Int(_)) => {
                    RuntimeError::divide_by_zero_failure(Some(Value::Int(*a)), Some("%"))
                }
                (Value::BigInt(a), Value::BigInt(b)) if !b.is_zero() => {
                    Value::from_bigint(num_integer::Integer::mod_floor(a.as_ref(), b.as_ref()))
                }
                (Value::BigInt(a), Value::Int(b)) if *b != 0 => {
                    let bb = num_bigint::BigInt::from(*b);
                    Value::from_bigint(num_integer::Integer::mod_floor(a.as_ref(), &bb))
                }
                (Value::Int(a), Value::BigInt(b)) if !b.is_zero() => {
                    let aa = num_bigint::BigInt::from(*a);
                    Value::from_bigint(num_integer::Integer::mod_floor(&aa, b.as_ref()))
                }
                _ => {
                    let a = runtime::to_int(&l);
                    let b = runtime::to_int(&r);
                    if b == 0 {
                        return Ok(RuntimeError::divide_by_zero_failure(
                            Some(Value::Int(a)),
                            Some("%"),
                        ));
                    }
                    Value::Int(num_integer::Integer::mod_floor(&a, &b))
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
            Value::Int(0)
        } else {
            let g = num_integer::Integer::gcd(&a, &b);
            Value::from_bigint(&a / &g * &b)
        };
        self.stack.push(result);
    }

    pub(super) fn exec_infix_min_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        if matches!(&left, Value::Package(name) if name == "Any") {
            self.stack.push(right);
            return;
        }
        if matches!(&right, Value::Package(name) if name == "Any") {
            self.stack.push(left);
            return;
        }
        let left_is_failure =
            matches!(&left, Value::Instance { class_name, .. } if class_name == "Failure");
        let right_is_failure =
            matches!(&right, Value::Instance { class_name, .. } if class_name == "Failure");
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
        if matches!(&left, Value::Package(name) if name == "Any") {
            self.stack.push(right);
            return;
        }
        if matches!(&right, Value::Package(name) if name == "Any") {
            self.stack.push(left);
            return;
        }
        let left_is_failure =
            matches!(&left, Value::Instance { class_name, .. } if class_name == "Failure");
        let right_is_failure =
            matches!(&right, Value::Instance { class_name, .. } if class_name == "Failure");
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
        if let Value::Package(name) = &right
            && name == "Int"
            && !self.interpreter.warning_suppressed()
        {
            self.interpreter.write_warn_to_stderr(&format!(
                "Use of uninitialized value of type {} in numeric context",
                name
            ));
        }

        // Whatever on RHS produces a WhateverCode closure
        if matches!(&right, Value::Whatever) {
            self.stack.push(self.interpreter.make_x_whatevercode(left));
            return Ok(());
        }

        let Some(n_raw) = crate::runtime::Interpreter::parse_repeat_count(&right)? else {
            return Err(crate::runtime::Interpreter::repeat_error(
                "X::Numeric::CannotConvert",
                "Cannot convert Inf to Int".to_string(),
            ));
        };
        let n = n_raw.max(0) as usize;
        let repeated = crate::runtime::utils::coerce_to_str(&left).repeat(n);
        // NFC-normalize after repetition
        let result = Value::str(repeated.nfc().collect::<String>());
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_list_repeat_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();

        // Fast path for xx-reeval thunks (compiler-generated repeat blocks)
        let thunk = match &left {
            Value::Sub(data) => Some(data.clone()),
            Value::WeakSub(weak) => weak.upgrade(),
            _ => None,
        };
        if let Some(data) = thunk.filter(|data| Self::is_xx_reeval_thunk(data.as_ref()))
            && let Value::Int(n) = right
        {
            let n = n.max(0) as usize;
            let items = self.vm_xx_repeat_thunk(data.as_ref(), n)?;
            self.stack.push(Value::Seq(Arc::new(items)));
            return Ok(());
        }

        // Warn on uninitialized type object used as repeat count
        if let Value::Package(name) = &right
            && name == "Int"
            && !self.interpreter.warning_suppressed()
        {
            self.interpreter.write_warn_to_stderr(&format!(
                "Use of uninitialized value of type {} in numeric context",
                name
            ));
        }

        const EAGER_LIMIT: usize = 10_000;
        const LAZY_CACHE: usize = 4_096;
        const LAZY_CACHE_CALLABLE: usize = 256;

        let is_callable = matches!(
            left,
            Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
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
        if let Value::Slip(slip_items) = &left {
            if slip_items.is_empty() {
                items.extend(std::iter::repeat_n(Value::Nil, repeat));
            } else {
                for _ in 0..repeat {
                    items.extend(slip_items.iter().cloned());
                }
            }
        } else {
            for _ in 0..repeat {
                items.push(self.interpreter.repeat_lhs_once(&left)?);
            }
        }

        let result = if lazy {
            crate::runtime::Interpreter::make_repeat_lazy_cache(items)
        } else {
            Value::Seq(Arc::new(items))
        };
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_function_compose_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let composed = self.interpreter.compose_callables(left, right);
        self.stack.push(composed);
    }

    pub(super) fn exec_but_mixin_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let role_composed = match &right {
            Value::Pair(name, boxed)
                if self.interpreter.has_role(name)
                    && matches!(boxed.as_ref(), Value::Array(..)) =>
            {
                Some(
                    self.interpreter
                        .eval_does_values(left.clone(), right.clone()),
                )
            }
            Value::Package(name) if self.interpreter.has_role(&name.resolve()) => Some(
                self.interpreter
                    .eval_does_values(left.clone(), right.clone()),
            ),
            Value::Str(name) if self.interpreter.has_role(name) => Some(
                self.interpreter
                    .eval_does_values(left.clone(), right.clone()),
            ),
            _ => None,
        };
        if let Some(composed) = role_composed {
            self.stack.push(composed?);
            return Ok(());
        }
        let result = Self::apply_but_mixin(left, right);
        self.stack.push(result);
        Ok(())
    }

    /// Apply a `but`-style mixin: wrap the left value with the right value
    /// keyed by its type name.
    fn apply_but_mixin(left: Value, right: Value) -> Value {
        // Determine the mixin type from the right-hand value
        let mixin_type = match &right {
            Value::Bool(_) => "Bool".to_string(),
            Value::Int(_) | Value::BigInt(_) => "Int".to_string(),
            Value::Num(_) => "Num".to_string(),
            Value::Str(_) => "Str".to_string(),
            Value::Package(name) => name.resolve(),
            Value::Enum { enum_type, .. } => enum_type.resolve(),
            Value::Instance { class_name, .. } => class_name.resolve(),
            _ => "Any".to_string(),
        };
        // If left is already a Mixin, add to existing mixins
        match left {
            Value::Mixin(inner, existing_mixins) => {
                let mut mixins = (*existing_mixins).clone();
                mixins.insert(mixin_type, right);
                Value::mixin((*inner).clone(), mixins)
            }
            other => {
                let mut mixins = std::collections::HashMap::new();
                mixins.insert(mixin_type, right);
                Value::mixin(other, mixins)
            }
        }
    }

    pub(super) fn exec_isa_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let type_name = right.to_string_value();
        let result = left.isa_check(&type_name);
        self.stack.push(Value::Bool(result));
    }

    /// VM-native `does` check. Inlines the pure `does_check` path and
    /// only falls back to the interpreter for actual role composition.
    fn vm_does_values(&mut self, left: Value, right: Value) -> Result<Value, RuntimeError> {
        // Check if the RHS is a role that needs to be composed onto the value.
        // If so, delegate to the interpreter which manages role state.
        if self.interpreter.is_role_application(&right) {
            return self.interpreter.eval_does_values(left, right);
        }
        // When the RHS is an enum value, `does` acts as a mixin (like `but`).
        if matches!(&right, Value::Enum { .. }) {
            return Ok(Self::apply_but_mixin(left, right));
        }
        // When the RHS is a concrete value (Str, Int, Bool, etc.), `does` acts
        // as a mutating mixin — it overrides the corresponding type method.
        // For example, `$o does "modded"` makes `$o.Str` return "modded".
        if matches!(
            &right,
            Value::Str(_)
                | Value::Int(_)
                | Value::BigInt(_)
                | Value::Num(_)
                | Value::Bool(_)
                | Value::Rat(..)
        ) {
            return Ok(Self::apply_but_mixin(left, right));
        }
        // When the RHS is an Instance, mix it in so that calling .$ClassName
        // on the result returns that instance.
        if matches!(&right, Value::Instance { .. }) {
            return Ok(Self::apply_but_mixin(left, right));
        }
        // Pure check: does the value conform to the named role/type?
        let role_name = right.to_string_value();
        Ok(Value::Bool(left.does_check(&role_name)))
    }

    pub(super) fn exec_does_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.vm_does_values(left, right)?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_does_var_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let updated = self.vm_does_values(left, right)?;
        let name = Self::const_str(code, name_idx).to_string();
        self.interpreter
            .env_mut()
            .insert(name.clone(), updated.clone());
        self.update_local_if_exists(code, &name, &updated);
        self.stack.push(updated);
        Ok(())
    }

    pub(super) fn exec_make_pair_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        // Preserve the original key type for `.key` to return the correct type.
        // Use ValuePair for non-string keys, Pair for string keys.
        match &left {
            Value::Str(_) => {
                let key = left.to_string_value();
                self.stack.push(Value::Pair(key, Box::new(right)));
            }
            _ => {
                self.stack
                    .push(Value::ValuePair(Box::new(left), Box::new(right)));
            }
        }
    }

    pub(super) fn exec_bit_and_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let (l, r) = runtime::coerce_numeric(left, right);
        let result = match (l, r) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a & b),
            (Value::BigInt(a), Value::BigInt(b)) => Value::from_bigint(&*a & &*b),
            (Value::BigInt(a), Value::Int(b)) => {
                Value::from_bigint(&*a & &num_bigint::BigInt::from(b))
            }
            (Value::Int(a), Value::BigInt(b)) => {
                Value::from_bigint(&num_bigint::BigInt::from(a) & &*b)
            }
            _ => Value::Int(0),
        };
        self.stack.push(result);
    }

    pub(super) fn exec_bit_or_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let (l, r) = runtime::coerce_numeric(left, right);
        let result = match (l, r) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a | b),
            (Value::BigInt(a), Value::BigInt(b)) => Value::from_bigint(&*a | &*b),
            (Value::BigInt(a), Value::Int(b)) => {
                Value::from_bigint(&*a | &num_bigint::BigInt::from(b))
            }
            (Value::Int(a), Value::BigInt(b)) => {
                Value::from_bigint(&num_bigint::BigInt::from(a) | &*b)
            }
            _ => Value::Int(0),
        };
        self.stack.push(result);
    }

    pub(super) fn exec_bit_xor_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let (l, r) = runtime::coerce_numeric(left, right);
        let result = match (l, r) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a ^ b),
            (Value::BigInt(a), Value::BigInt(b)) => Value::from_bigint(&*a ^ &*b),
            (Value::BigInt(a), Value::Int(b)) => {
                Value::from_bigint(&*a ^ &num_bigint::BigInt::from(b))
            }
            (Value::Int(a), Value::BigInt(b)) => {
                Value::from_bigint(&num_bigint::BigInt::from(a) ^ &*b)
            }
            _ => Value::Int(0),
        };
        self.stack.push(result);
    }

    pub(super) fn exec_bit_shift_left_op(&mut self) {
        fn shift_left_i64(a: i64, b: i64) -> Value {
            if b < 0 {
                let shift = b.unsigned_abs();
                let shifted = if shift >= i64::BITS as u64 {
                    if a < 0 { -1 } else { 0 }
                } else {
                    a >> (shift as u32)
                };
                return Value::Int(shifted);
            }
            let shift = b as u64;
            if shift >= i64::BITS as u64 {
                return Value::from_bigint(num_bigint::BigInt::from(a) << (shift as usize));
            }
            // Use BigInt for the shift to avoid i64 overflow (Raku integers are arbitrary precision)
            Value::from_bigint(num_bigint::BigInt::from(a) << (shift as usize))
        }

        fn shift_left_bigint(a: num_bigint::BigInt, b: i64) -> Value {
            if b < 0 {
                Value::from_bigint(a >> (b.unsigned_abs() as usize))
            } else {
                Value::from_bigint(a << (b as usize))
            }
        }

        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let (l, r) = runtime::coerce_numeric(left, right);
        let result = match (l, r) {
            (Value::Int(a), Value::Int(b)) => shift_left_i64(a, b),
            (l, r) => {
                let a = l.to_bigint();
                let b_big = r.to_bigint();
                let b = b_big.to_i64().unwrap_or_else(|| {
                    if b_big.sign() == num_bigint::Sign::Minus {
                        i64::MIN
                    } else {
                        i64::MAX
                    }
                });
                shift_left_bigint(a, b)
            }
        };
        self.stack.push(result);
    }

    pub(super) fn exec_bit_shift_right_op(&mut self) {
        fn shift_right_i64(a: i64, b: i64) -> Value {
            if b < 0 {
                let shift = b.unsigned_abs();
                if shift >= i64::BITS as u64 {
                    return Value::from_bigint(num_bigint::BigInt::from(a) << (shift as usize));
                }
                if let Some(v) = a.checked_shl(shift as u32) {
                    Value::Int(v)
                } else {
                    Value::from_bigint(num_bigint::BigInt::from(a) << (shift as usize))
                }
            } else {
                let shift = b as u64;
                let shifted = if shift >= i64::BITS as u64 {
                    if a < 0 { -1 } else { 0 }
                } else {
                    a >> (shift as u32)
                };
                Value::Int(shifted)
            }
        }

        fn shift_right_bigint(a: num_bigint::BigInt, b: i64) -> Value {
            if b < 0 {
                Value::from_bigint(a << (b.unsigned_abs() as usize))
            } else {
                Value::from_bigint(a >> (b as usize))
            }
        }

        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let (l, r) = runtime::coerce_numeric(left, right);
        let result = match (l, r) {
            (Value::Int(a), Value::Int(b)) => shift_right_i64(a, b),
            (l, r) => {
                let a = l.to_bigint();
                let b_big = r.to_bigint();
                let b = b_big.to_i64().unwrap_or_else(|| {
                    if b_big.sign() == num_bigint::Sign::Minus {
                        i64::MIN
                    } else {
                        i64::MAX
                    }
                });
                shift_right_bigint(a, b)
            }
        };
        self.stack.push(result);
    }

    pub(super) fn exec_bool_bit_or_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack.push(Value::Bool(left.truthy() | right.truthy()));
    }

    pub(super) fn exec_bool_bit_and_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack.push(Value::Bool(left.truthy() & right.truthy()));
    }

    pub(super) fn exec_bool_bit_xor_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack.push(Value::Bool(left.truthy() ^ right.truthy()));
    }

    /// String bitwise AND (~&): AND corresponding bytes of two strings.
    pub(super) fn exec_str_bit_and_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let l = left.to_string_value();
        let r = right.to_string_value();
        let lb = l.as_bytes();
        let rb = r.as_bytes();
        let len = lb.len().max(rb.len());
        let result: Vec<u8> = (0..len)
            .map(|i| {
                let a = lb.get(i).copied().unwrap_or(0);
                let b = rb.get(i).copied().unwrap_or(0);
                a & b
            })
            .collect();
        self.stack
            .push(Value::str(String::from_utf8_lossy(&result).into_owned()));
    }

    /// String bitwise OR (~|): OR corresponding bytes of two strings.
    pub(super) fn exec_str_bit_or_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let l = left.to_string_value();
        let r = right.to_string_value();
        let lb = l.as_bytes();
        let rb = r.as_bytes();
        let len = lb.len().max(rb.len());
        let result: Vec<u8> = (0..len)
            .map(|i| {
                let a = lb.get(i).copied().unwrap_or(0);
                let b = rb.get(i).copied().unwrap_or(0);
                a | b
            })
            .collect();
        self.stack
            .push(Value::str(String::from_utf8_lossy(&result).into_owned()));
    }

    /// String bitwise XOR (~^): XOR corresponding bytes of two strings.
    pub(super) fn exec_str_bit_xor_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let l = left.to_string_value();
        let r = right.to_string_value();
        let lb = l.as_bytes();
        let rb = r.as_bytes();
        let len = lb.len().max(rb.len());
        let result: Vec<u8> = (0..len)
            .map(|i| {
                let a = lb.get(i).copied().unwrap_or(0);
                let b = rb.get(i).copied().unwrap_or(0);
                a ^ b
            })
            .collect();
        self.stack
            .push(Value::str(String::from_utf8_lossy(&result).into_owned()));
    }

    /// String bitwise shift left (~<): not yet implemented in Raku either.
    /// TODO: Implement when Raku spec is finalized for this operator.
    pub(super) fn exec_str_shift_left_op(&mut self) {
        let _right = self.stack.pop().unwrap();
        let _left = self.stack.pop().unwrap();
        self.stack.push(Value::Nil);
    }

    /// String bitwise shift right (~>): not yet implemented in Raku either.
    /// TODO: Implement when Raku spec is finalized for this operator.
    pub(super) fn exec_str_shift_right_op(&mut self) {
        let _right = self.stack.pop().unwrap();
        let _left = self.stack.pop().unwrap();
        self.stack.push(Value::Nil);
    }
}
