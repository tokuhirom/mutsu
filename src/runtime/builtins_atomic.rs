use super::*;
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use std::sync::atomic::{AtomicU64, Ordering};

pub(super) static ATOMIC_VAR_KEY_COUNTER: AtomicU64 = AtomicU64::new(1);

impl Interpreter {
    /// VM-native dispatch for the atomic var/element RMW markers (`__mutsu_atomic_*`
    /// / `__mutsu_cas_*`) the parser emits for the `⚛`-operators (`⚛=`/`⚛+=`/`⚛++`/
    /// `--⚛`/`cas`). These reached the interpreter only through the generic
    /// `call_function` name-match fallback; the `builtin_atomic_*` / `builtin_cas_*`
    /// impls already own the VM-shared `shared_vars` store and the per-attribute
    /// cell-CAS state, so the VM dispatches them directly here without the
    /// `call_function` round-trip (§D state ownership — byte-identical, the
    /// `call_function` arms call these exact impls on the same `self`). Returns
    /// `None` for every other name so the caller falls through to the rest of native
    /// dispatch.
    pub(crate) fn try_native_atomic_function(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let r = match name {
            "__mutsu_atomic_fetch_var" => self.builtin_atomic_fetch_var(args),
            "__mutsu_atomic_store_var" => self.builtin_atomic_store_var(args),
            "__mutsu_atomic_add_var" => self.builtin_atomic_add_var(args),
            "__mutsu_atomic_fetch_add_var" => self.builtin_atomic_fetch_add_var(args),
            "__mutsu_atomic_post_inc_var" => self.builtin_atomic_post_inc_var(args),
            "__mutsu_atomic_pre_inc_var" => self.builtin_atomic_pre_inc_var(args),
            "__mutsu_atomic_post_dec_var" => self.builtin_atomic_post_dec_var(args),
            "__mutsu_atomic_pre_dec_var" => self.builtin_atomic_pre_dec_var(args),
            "__mutsu_cas_var" => self.builtin_cas_var(args.to_vec()),
            "__mutsu_cas_array_elem" => self.builtin_cas_array_elem(args.to_vec()),
            "__mutsu_cas_array_multidim" => self.builtin_cas_array_multidim(args.to_vec()),
            "__mutsu_cas_hash_elem" => self.builtin_cas_hash_elem(args.to_vec()),
            _ => return None,
        };
        Some(r)
    }

    pub(super) fn canonical_atomic_var_name(
        &self,
        raw_name: &str,
        target_value: Option<&Value>,
    ) -> String {
        let is_user_visible = |name: &str| {
            name != "_"
                && name != "@_"
                && name != "?LINE"
                && !name.starts_with("__mutsu_")
                && !name.starts_with('&')
        };
        if is_user_visible(raw_name) && self.env.contains_key(raw_name) {
            return raw_name.to_string();
        }

        let current = target_value
            .cloned()
            .or_else(|| self.env.get(raw_name).cloned());
        if let Some(current) = current {
            for (key, value) in &self.env {
                let key_s = key.resolve();
                if !is_user_visible(&key_s) {
                    continue;
                }
                if crate::runtime::values_identical(value, &current) {
                    return key_s;
                }
            }
        }
        raw_name.to_string()
    }

    pub(super) fn atomic_var_name_arg(&self, args: &[Value]) -> Result<String, RuntimeError> {
        let Some(name) = args.first() else {
            return Err(RuntimeError::new(
                "atomic variable operation requires variable name",
            ));
        };
        let raw_name = name.to_string_value();
        Ok(self.canonical_atomic_var_name(&raw_name, Some(name)))
    }

    pub(super) fn atomic_shared_value_key(id: u64) -> String {
        format!("__mutsu_atomic_value::{id}")
    }

    pub(super) fn atomic_shared_name_key(name: &str) -> String {
        format!("__mutsu_atomic_name::{name}")
    }

    pub(super) fn atomic_assign_coerced_value(
        &mut self,
        name: &str,
        mut value: Value,
    ) -> Result<Value, RuntimeError> {
        self.check_readonly_for_modify(name)?;
        if let Some(constraint) = self.var_type_constraint(name)
            && !name.starts_with('%')
            && !name.starts_with('@')
        {
            if matches!(value, Value::Nil) {
                value = Value::Package(Symbol::intern(&constraint));
            } else if !self.type_matches_value(&constraint, &value) {
                return Err(crate::runtime::utils::type_check_assignment_typed_error(
                    name,
                    &constraint,
                    &value,
                ));
            }
            if !matches!(value, Value::Nil | Value::Package(_)) {
                value = self.try_coerce_value_for_constraint(&constraint, value)?;
            }
        }
        Ok(value)
    }

    pub(super) fn atomic_value_key_for_name(&mut self, name: &str) -> String {
        self.mark_atomic_var_seen();
        let name_key = Self::atomic_shared_name_key(name);
        if let Some(Value::Str(existing)) = self.env.get(&name_key) {
            return existing.to_string();
        }
        let value_key = {
            let mut shared = self.shared_vars.write().unwrap();
            if let Some(Value::Str(existing)) = shared.get(&name_key) {
                existing.to_string()
            } else {
                let id = ATOMIC_VAR_KEY_COUNTER.fetch_add(1, Ordering::Relaxed);
                let value_key = Self::atomic_shared_value_key(id);
                shared.insert(name_key.clone(), Value::str(value_key.clone()));
                value_key
            }
        };
        self.env.insert(name_key, Value::str(value_key.clone()));
        value_key
    }

    pub(super) fn atomic_current_value(
        &self,
        shared: &std::collections::HashMap<String, Value>,
        name: &str,
        value_key: &str,
    ) -> Value {
        let current = shared
            .get(value_key)
            .cloned()
            .or_else(|| self.env.get(name).cloned())
            .unwrap_or(Value::Nil);
        if let Some(constraint) = self.var_type_constraint(name) {
            let is_untyped_placeholder = matches!(current, Value::Nil)
                || matches!(current, Value::Package(sym) if sym.resolve() == "Any");
            if is_untyped_placeholder {
                return Value::Package(Symbol::intern(&constraint));
            }
        }
        current
    }

    pub(super) fn cas_retry_matches(expected_seen: &Value, latest_seen: &Value) -> bool {
        match (expected_seen, latest_seen) {
            (Value::Instance { id: a, .. }, Value::Instance { id: b, .. }) => a == b,
            (Value::Array(a, ..), Value::Array(b, ..)) => std::sync::Arc::ptr_eq(a, b),
            (Value::Hash(a), Value::Hash(b)) => std::sync::Arc::ptr_eq(a, b),
            (Value::Seq(a), Value::Seq(b)) => std::sync::Arc::ptr_eq(a, b),
            (Value::Slip(a), Value::Slip(b)) => std::sync::Arc::ptr_eq(a, b),
            _ => expected_seen == latest_seen,
        }
    }

    pub(crate) fn builtin_atomic_fetch_var(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let name = self.atomic_var_name_arg(args)?;
        // Phase 3 cell-CAS: attribute targets read the receiver's shared cell.
        if let Some((attrs, key)) = self.self_attr_cell_target(&name) {
            let val = attrs.as_map().get(&key).cloned().unwrap_or(Value::Nil);
            return Ok(val);
        }
        let value_key = self.atomic_value_key_for_name(&name);
        let shared = self.shared_vars.read().unwrap();
        Ok(self.atomic_current_value(&shared, &name, &value_key))
    }

    pub(super) fn builtin_atomic_store_var(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 2 {
            return Err(RuntimeError::new(
                "atomic store requires variable name and value",
            ));
        }
        let raw_name = args[0].to_string_value();
        let name = self.canonical_atomic_var_name(&raw_name, args.first());
        let value = self.atomic_assign_coerced_value(&name, args[1].clone())?;
        // Phase 3 cell-CAS: attribute targets store into the receiver's cell.
        if let Some((attrs, key)) = self.self_attr_cell_target(&name) {
            attrs.insert(key, value.clone());
            self.env.insert(name, value.clone());
            return Ok(value);
        }
        let value_key = self.atomic_value_key_for_name(&name);
        self.env.insert(name.clone(), value.clone());
        self.shared_vars
            .write()
            .unwrap()
            .insert(value_key.clone(), value.clone());
        if let Ok(mut dirty) = self.shared_vars_dirty.write() {
            dirty.insert(value_key);
            dirty.insert(name);
        }
        Ok(value)
    }

    pub(super) fn builtin_atomic_add_var(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() < 2 {
            return Err(RuntimeError::new(
                "atomic add requires variable name and increment value",
            ));
        }
        let raw_name = args[0].to_string_value();
        let name = self.canonical_atomic_var_name(&raw_name, args.first());
        let delta = args[1].clone();
        self.check_readonly_for_modify(&name)?;
        // Phase 3 cell-CAS: attribute targets RMW the receiver's shared cell.
        if let Some((attrs, key)) = self.self_attr_cell_target(&name) {
            let (_, next) = attrs.fetch_update(&key, |cur| {
                let base = match cur {
                    Value::Nil | Value::Package(_) => Value::Int(0),
                    other => other.clone(),
                };
                crate::builtins::arith_add(base, delta.clone())
            })?;
            self.env.insert(name, next.clone());
            return Ok(next);
        }
        let value_key = self.atomic_value_key_for_name(&name);
        let mut shared = self.shared_vars.write().unwrap();
        let current = self.atomic_current_value(&shared, &name, &value_key);
        let next = crate::builtins::arith_add(current, delta)?;
        self.env.insert(name.clone(), next.clone());
        shared.insert(value_key.clone(), next.clone());
        drop(shared);
        if let Ok(mut dirty) = self.shared_vars_dirty.write() {
            dirty.insert(value_key);
            dirty.insert(name);
        }
        Ok(next)
    }

    /// Like `builtin_atomic_add_var` but returns the OLD value (before adding).
    pub(super) fn builtin_atomic_fetch_add_var(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 2 {
            return Err(RuntimeError::new(
                "atomic-fetch-add requires variable name and increment value",
            ));
        }
        let raw_name = args[0].to_string_value();
        let name = self.canonical_atomic_var_name(&raw_name, args.first());
        let delta = args[1].clone();
        self.check_readonly_for_modify(&name)?;
        // Phase 3 cell-CAS: attribute targets RMW the receiver's shared cell.
        if let Some((attrs, key)) = self.self_attr_cell_target(&name) {
            let (old, next) = attrs.fetch_update(&key, |cur| {
                let base = match cur {
                    Value::Nil | Value::Package(_) => Value::Int(0),
                    other => other.clone(),
                };
                crate::builtins::arith_add(base, delta.clone())
            })?;
            self.env.insert(name, next);
            return Ok(old);
        }
        let value_key = self.atomic_value_key_for_name(&name);
        let mut shared = self.shared_vars.write().unwrap();
        let current = self.atomic_current_value(&shared, &name, &value_key);
        let next = crate::builtins::arith_add(current.clone(), delta)?;
        self.env.insert(name.clone(), next.clone());
        shared.insert(value_key.clone(), next.clone());
        drop(shared);
        if let Ok(mut dirty) = self.shared_vars_dirty.write() {
            dirty.insert(value_key);
            dirty.insert(name);
        }
        Ok(current)
    }

    pub(super) fn builtin_atomic_update_unit(
        &mut self,
        args: &[Value],
        delta: i64,
        return_old: bool,
    ) -> Result<Value, RuntimeError> {
        let name = self.atomic_var_name_arg(args)?;
        self.check_readonly_for_modify(&name)?;
        // Phase 3 cell-CAS: instance attribute variables (private `!attr` or
        // public `.attr`) read-modify-write the receiver's shared attribute
        // cell under its write lock — atomic across every alias and thread.
        if name.starts_with('!') || name.starts_with('.') {
            if let Some((attrs, key)) = self.self_attr_cell_target(&name) {
                let (old, next) = attrs.fetch_update(&key, |cur| {
                    let base = match cur {
                        Value::Nil | Value::Package(_) => Value::Int(0),
                        other => other.clone(),
                    };
                    crate::builtins::arith_add(base, Value::Int(delta))
                })?;
                let old = match old {
                    Value::Nil | Value::Package(_) => Value::Int(0),
                    other => other,
                };
                self.env.insert(name, next.clone());
                return if return_old { Ok(old) } else { Ok(next) };
            }
            // Fallback for non-instance context: use env directly
            let current = self.env.get(&name).cloned().unwrap_or(Value::Int(0));
            let next = crate::builtins::arith_add(current.clone(), Value::Int(delta))?;
            self.env.insert(name, next.clone());
            return if return_old { Ok(current) } else { Ok(next) };
        }
        let value_key = self.atomic_value_key_for_name(&name);
        let mut shared = self.shared_vars.write().unwrap();
        let current = self.atomic_current_value(&shared, &name, &value_key);
        let next = crate::builtins::arith_add(current.clone(), Value::Int(delta))?;
        self.env.insert(name.clone(), next.clone());
        shared.insert(value_key.clone(), next.clone());
        drop(shared);
        if let Ok(mut dirty) = self.shared_vars_dirty.write() {
            dirty.insert(value_key);
            dirty.insert(name);
        }
        if return_old { Ok(current) } else { Ok(next) }
    }

    pub(super) fn builtin_atomic_post_inc_var(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        self.builtin_atomic_update_unit(args, 1, true)
    }

    pub(super) fn builtin_atomic_pre_inc_var(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        self.builtin_atomic_update_unit(args, 1, false)
    }

    pub(super) fn builtin_atomic_post_dec_var(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        self.builtin_atomic_update_unit(args, -1, true)
    }

    pub(super) fn builtin_atomic_pre_dec_var(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        self.builtin_atomic_update_unit(args, -1, false)
    }

    /// Implements cas (compare-and-swap).
    /// 3-arg form: cas($var_name, $expected, $new) - swap if current == expected, return old
    /// 2-arg form: cas($var_name, &code) - read old, compute new = code(old), store new, return new
    pub(super) fn builtin_cas_var(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        if args.len() < 2 {
            return Err(RuntimeError::new(
                "cas requires 2 or 3 arguments: cas($var, $expected, $new) or cas($var, &code)",
            ));
        }
        let raw_name = args[0].to_string_value();
        let name = self.canonical_atomic_var_name(&raw_name, args.first());
        self.check_readonly_for_modify(&name)?;
        // Phase 3 cell-CAS: an instance attribute target operates directly on
        // the receiver's shared attribute cell — its write lock is the atomic
        // primitive and every alias shares the cell, so the swap is visible to
        // all frames and threads without the legacy shared_vars side channel.
        let attr_cell = self.self_attr_cell_target(&name);
        let value_key = if attr_cell.is_none() {
            self.atomic_value_key_for_name(&name)
        } else {
            String::new()
        };

        if args.len() == 3 {
            // 3-arg form: cas($var, $expected, $new)
            let expected = &args[1];
            let new_val = args[2].clone();
            let coerced = self.atomic_assign_coerced_value(&name, new_val)?;
            if let Some((attrs, key)) = attr_cell {
                let (current, swapped) = attrs.compare_and_swap(
                    &key,
                    |cur| Self::cas_retry_matches(cur, expected),
                    coerced.clone(),
                );
                // Keep the frame's env copy coherent for legacy env readers.
                let env_val = if swapped { coerced } else { current.clone() };
                self.env.insert(name, env_val);
                return Ok(current);
            }
            let mut did_swap = false;
            let current = {
                let mut shared = self.shared_vars.write().unwrap();
                let current = self.atomic_current_value(&shared, &name, &value_key);
                if Self::cas_retry_matches(&current, expected) {
                    shared.insert(value_key.clone(), coerced.clone());
                    did_swap = true;
                }
                current
            };
            if did_swap {
                self.env.insert(name.clone(), coerced.clone());
                if let Ok(mut dirty) = self.shared_vars_dirty.write() {
                    dirty.insert(value_key);
                    dirty.insert(name.clone());
                }
            } else {
                self.env.insert(name.clone(), current.clone());
            }
            Ok(current)
        } else {
            // 2-arg form: cas($var, &code)
            let code = args[1].clone();
            // Skip leading SetLine statements (inserted by pointy block parsing)
            // to find the effective body for the optimization check.
            let effective_body: Vec<&Stmt> = if let Value::Sub(sub) = &code {
                sub.body
                    .iter()
                    .filter(|s| !matches!(s, Stmt::SetLine(_)))
                    .collect()
            } else {
                Vec::new()
            };
            if let Value::Sub(sub) = &code
                && sub.params.len() == 1
                && effective_body.len() == 1
                && let Stmt::Expr(Expr::Binary { left, op, right }) = effective_body[0]
                && *op == TokenKind::Plus
            {
                let param = &sub.params[0];
                let delta_expr = match (left.as_ref(), right.as_ref()) {
                    (Expr::Var(lhs), rhs) if lhs == param => Some(rhs.clone()),
                    (lhs, Expr::Var(rhs)) if rhs == param => Some(lhs.clone()),
                    _ => None,
                };
                if let Some(delta_expr) = delta_expr {
                    let delta = match delta_expr {
                        Expr::Var(var_name) => {
                            self.env.get(&var_name).cloned().unwrap_or(Value::Nil)
                        }
                        Expr::Literal(v) => v,
                        other => self.eval_block_value(&[Stmt::Expr(other)])?,
                    };
                    return self.builtin_atomic_add_var(&[Value::str(name.clone()), delta]);
                }
            }
            // Optimization: {.succ} or {.pred} on $_ -> atomic add +/-1
            if let Value::Sub(sub) = &code
                && sub.params.is_empty()
                && effective_body.len() == 1
                && let Stmt::Expr(Expr::MethodCall {
                    target,
                    name: method_name,
                    args: method_args,
                    ..
                }) = effective_body[0]
                && method_args.is_empty()
                && matches!(target.as_ref(), Expr::Var(v) if v == "_" || v == "$_")
            {
                let method_str = method_name.resolve();
                if method_str == "succ" {
                    return self.builtin_atomic_add_var(&[Value::str(name.clone()), Value::Int(1)]);
                } else if method_str == "pred" {
                    return self
                        .builtin_atomic_add_var(&[Value::str(name.clone()), Value::Int(-1)]);
                }
            }
            // Optimization: { $_ + N } with zero params -> atomic add N
            if let Value::Sub(sub) = &code
                && sub.params.is_empty()
                && effective_body.len() == 1
                && let Stmt::Expr(Expr::Binary { left, op, right }) = effective_body[0]
                && *op == TokenKind::Plus
            {
                let delta_expr = match (left.as_ref(), right.as_ref()) {
                    (Expr::Var(lhs), rhs) if lhs == "_" || lhs == "$_" => Some(rhs.clone()),
                    (lhs, Expr::Var(rhs)) if rhs == "_" || rhs == "$_" => Some(lhs.clone()),
                    _ => None,
                };
                if let Some(delta_expr) = delta_expr {
                    let delta = match delta_expr {
                        Expr::Var(var_name) => {
                            self.env.get(&var_name).cloned().unwrap_or(Value::Nil)
                        }
                        Expr::Literal(v) => v,
                        other => self.eval_block_value(&[Stmt::Expr(other)])?,
                    };
                    return self.builtin_atomic_add_var(&[Value::str(name.clone()), delta]);
                }
            }
            loop {
                let current = if let Some((attrs, key)) = &attr_cell {
                    attrs
                        .as_map()
                        .get(key.as_str())
                        .cloned()
                        .unwrap_or(Value::Nil)
                } else {
                    let shared = self.shared_vars.read().unwrap();
                    self.atomic_current_value(&shared, &name, &value_key)
                };
                self.env.insert(name.clone(), current.clone());
                let new_val = {
                    let bind_dollar_topic =
                        matches!(&code, Value::Sub(sub) if sub.params.is_empty());
                    let saved_topic = self.env.get("_").cloned();
                    let saved_dollar_topic = if bind_dollar_topic {
                        self.env.get("$_").cloned()
                    } else {
                        None
                    };
                    self.env.insert("_".to_string(), current.clone());
                    if bind_dollar_topic {
                        self.env.insert("$_".to_string(), current.clone());
                    }
                    let call_args = if let Value::Sub(sub) = &code {
                        if sub.params.is_empty() {
                            Vec::new()
                        } else {
                            vec![current.clone()]
                        }
                    } else {
                        vec![current.clone()]
                    };
                    // env_dirty substrate (docs/captured-outer-cell-sharing.md §10):
                    // the CAS block (`cas $var, { $was = $_; … }`) can mutate a
                    // captured-outer caller scalar by name. The write reaches env but
                    // the owning caller slot is refreshed only by the call site's
                    // blanket pull (no-op once env_dirty is removed). Snapshot the env
                    // scalars before the block and record the names it changes into
                    // the retain-on-miss caller-var writeback, drained at the cas call
                    // site (`apply_pending_rw_writeback`).
                    let cas_pre_env: Option<
                        std::collections::HashMap<crate::symbol::Symbol, Value>,
                    > = Some(
                        self.env
                            .iter()
                            .filter(|(_, v)| Self::is_writeback_safe_scalar(v))
                            .map(|(k, v)| (*k, v.clone()))
                            .collect(),
                    );
                    let result = self.call_sub_value(code.clone(), call_args, bind_dollar_topic)?;
                    if let Some(cas_pre_env) = cas_pre_env {
                        let changed: Vec<String> = self
                            .env
                            .iter()
                            .filter(|(k, v)| {
                                let kn = k.resolve();
                                kn != "_"
                                    && kn != "$_"
                                    && kn != name
                                    && Self::is_writeback_safe_scalar(v)
                                    && cas_pre_env.get(*k).map(|p| p != *v).unwrap_or(true)
                            })
                            .map(|(k, _)| k.resolve())
                            .collect();
                        for n in changed {
                            self.record_caller_var_writeback(&n);
                        }
                    }
                    match saved_topic {
                        Some(v) => {
                            self.env.insert("_".to_string(), v);
                        }
                        None => {
                            self.env.remove("_");
                        }
                    }
                    if bind_dollar_topic {
                        match saved_dollar_topic {
                            Some(v) => {
                                self.env.insert("$_".to_string(), v);
                            }
                            None => {
                                self.env.remove("$_");
                            }
                        }
                    }
                    result
                };
                // If the block returned a value equal to the old value or Nil
                // but the variable itself was modified (e.g., `{ $x = expr }`),
                // use the variable's current value from the env. This handles
                // blocks that modify the variable as a side effect.
                let effective_new = if new_val == current || matches!(new_val, Value::Nil) {
                    self.env.get(&name).cloned().unwrap_or(new_val)
                } else {
                    new_val
                };
                let coerced = self.atomic_assign_coerced_value(&name, effective_new)?;
                // Restore the env variable to `current` before the CAS comparison,
                // so that atomic_current_value (which falls back to env) reads the
                // pre-block snapshot rather than the value the block may have modified.
                self.env.insert(name.clone(), current.clone());

                let updated = if let Some((attrs, key)) = &attr_cell {
                    let (seen, swapped) = attrs.compare_and_swap(
                        key,
                        |cur| Self::cas_retry_matches(&current, cur),
                        coerced.clone(),
                    );
                    if !swapped {
                        self.env.insert(name.clone(), seen);
                    }
                    swapped
                } else {
                    let mut shared = self.shared_vars.write().unwrap();
                    let seen = self.atomic_current_value(&shared, &name, &value_key);
                    if Self::cas_retry_matches(&current, &seen) {
                        shared.insert(value_key.clone(), coerced.clone());
                        true
                    } else {
                        drop(shared);
                        self.env.insert(name.clone(), seen);
                        false
                    }
                };
                if updated {
                    self.env.insert(name.clone(), coerced.clone());
                    if attr_cell.is_none()
                        && let Ok(mut dirty) = self.shared_vars_dirty.write()
                    {
                        dirty.insert(value_key.clone());
                        dirty.insert(name.clone());
                    }
                    return Ok(coerced);
                }
            }
        }
    }

    /// CAS on an array element: cas(@arr[idx], $expected, $new)
    /// Args: [array_name_str, index, expected, new_val]
    /// Uses shared_vars to store the whole array for atomic cross-thread access.
    /// The array in shared_vars is the single source of truth — reads via
    /// `GetLocal` pick it up through the `get_shared_var` fallback path.
    pub(super) fn builtin_cas_array_elem(
        &mut self,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if args.len() != 4 {
            return Err(RuntimeError::new(
                "__mutsu_cas_array_elem requires 4 arguments",
            ));
        }
        let arr_name = args[0].to_string_value();
        let index = match &args[1] {
            Value::Int(i) => *i,
            other => other.to_string_value().parse::<i64>().unwrap_or(0),
        };
        let expected = &args[2];
        let new_val = args[3].clone();

        // Use an internal key to avoid interference with set_shared_var
        // which would overwrite our atomic array with stale local values.
        let atomic_key = format!("__mutsu_atomic_arr::{arr_name}");

        // Initialize shared_vars with the array if not yet set
        {
            let shared = self.shared_vars.read().unwrap();
            if !shared.contains_key(&atomic_key) {
                drop(shared);
                let arr = self.env.get(&arr_name).cloned().unwrap_or(Value::Array(
                    std::sync::Arc::new(crate::value::ArrayData::new(Vec::new())),
                    crate::value::ArrayKind::Array,
                ));
                let mut shared = self.shared_vars.write().unwrap();
                if !shared.contains_key(&atomic_key) {
                    shared.insert(atomic_key.clone(), arr);
                }
            }
        }

        let mut did_swap = false;
        let current;
        {
            let mut shared = self.shared_vars.write().unwrap();
            let arr = shared.get(&atomic_key).cloned().unwrap_or(Value::Array(
                std::sync::Arc::new(crate::value::ArrayData::new(Vec::new())),
                crate::value::ArrayKind::Array,
            ));
            if let Value::Array(ref elements, kind) = arr {
                let idx = if index < 0 {
                    (elements.len() as i64 + index) as usize
                } else {
                    index as usize
                };
                current = elements.get(idx).cloned().unwrap_or(Value::Int(0));
                if Self::cas_retry_matches(&current, expected) {
                    let mut new_elements = (**elements).clone();
                    while new_elements.len() <= idx {
                        new_elements.push(Value::Int(0));
                    }
                    new_elements[idx] = new_val.clone();
                    shared.insert(
                        atomic_key.clone(),
                        Value::Array(std::sync::Arc::new(new_elements), kind),
                    );
                    did_swap = true;
                }
            } else {
                current = Value::Int(0);
            }
        }

        if did_swap && let Ok(mut dirty) = self.shared_vars_dirty.write() {
            dirty.insert(arr_name.clone());
        }
        // Note: don't update env["@values"] here — a later locals restore could
        // overwrite it with a stale value. Instead, GetLocal checks the atomic
        // shared key directly.
        Ok(current)
    }

    /// Thread-safe `@arr.push(...)` (and `.unshift`) in shared (threaded)
    /// context.
    ///
    /// A plain `.push` in a thread reads `@arr` from the thread's *local* env
    /// snapshot, pushes, and only writes back later via `set_shared_var` — so
    /// concurrent threads each start from the same stale snapshot and clobber
    /// each other's pushes (lost update). Route the mutation through the same
    /// `__mutsu_atomic_arr::` shared store the CAS array ops use: a single
    /// lock-protected read-modify-write under the `shared_vars` write lock
    /// serializes all threads, and `set_shared_var` already refuses to
    /// overwrite a key that has an active atomic entry. `prepend` inserts the
    /// items at the front (preserving order) for `unshift`. Returns the new
    /// array.
    pub(crate) fn shared_array_extend(
        &mut self,
        arr_name: &str,
        items: Vec<Value>,
        prepend: bool,
    ) -> Value {
        let atomic_key = format!("__mutsu_atomic_arr::{arr_name}");
        let updated = {
            let mut shared = self.shared_vars.write().unwrap();
            // Seed from the authoritative source: the atomic entry if present
            // (another thread already pushed), else the shared base key, else
            // this thread's local snapshot, else an empty array.
            let mut elements: Vec<Value> = match shared.get(&atomic_key) {
                Some(Value::Array(elems, _)) => elems.to_vec(),
                _ => match shared.get(arr_name).or_else(|| self.env.get(arr_name)) {
                    Some(Value::Array(elems, _)) => elems.to_vec(),
                    _ => Vec::new(),
                },
            };
            if prepend {
                for (i, it) in items.into_iter().enumerate() {
                    elements.insert(i, it);
                }
            } else {
                elements.extend(items);
            }
            let new_arr = Value::Array(
                std::sync::Arc::new(crate::value::ArrayData::new(elements)),
                crate::value::ArrayKind::Array,
            );
            shared.insert(atomic_key, new_arr.clone());
            new_arr
        };
        // Mark the user-visible name dirty so `sync_shared_vars_to_env`
        // propagates the merged array back to the parent thread.
        if let Ok(mut dirty) = self.shared_vars_dirty.write() {
            dirty.insert(arr_name.to_string());
        }
        // Update the local env so this thread observes its own push immediately.
        self.env.insert(arr_name.to_string(), updated.clone());
        updated
    }

    /// Thread-safe `@arr[$i] = $v` in shared (threaded) context.
    ///
    /// Mirrors `shared_array_extend`: a single lock-protected read-modify-write
    /// through the `__mutsu_atomic_arr::` shared store, so concurrent
    /// `start { @a[...] = ... }` blocks each writing a different index all land
    /// instead of clobbering a stale snapshot via `set_shared_var`. Grows the
    /// array with `Nil` holes up to `idx`. Returns the assigned element value.
    pub(crate) fn shared_array_elem_set(
        &mut self,
        arr_name: &str,
        idx: usize,
        value: Value,
    ) -> Value {
        let atomic_key = format!("__mutsu_atomic_arr::{arr_name}");
        let updated = {
            let mut shared = self.shared_vars.write().unwrap();
            let mut elements: Vec<Value> = match shared.get(&atomic_key) {
                Some(Value::Array(elems, _)) => elems.to_vec(),
                _ => match shared.get(arr_name).or_else(|| self.env.get(arr_name)) {
                    Some(Value::Array(elems, _)) => elems.to_vec(),
                    _ => Vec::new(),
                },
            };
            if idx >= elements.len() {
                elements.resize(idx + 1, Value::Nil);
            }
            elements[idx] = value.clone();
            let new_arr = Value::Array(
                std::sync::Arc::new(crate::value::ArrayData::new(elements)),
                crate::value::ArrayKind::Array,
            );
            shared.insert(atomic_key, new_arr.clone());
            new_arr
        };
        if let Ok(mut dirty) = self.shared_vars_dirty.write() {
            dirty.insert(arr_name.to_string());
        }
        self.env.insert(arr_name.to_string(), updated);
        value
    }

    /// Thread-safe `%h{$k} = $v` in shared (threaded) context.
    ///
    /// The hash analogue of `shared_array_elem_set`: a single lock-protected
    /// read-modify-write through the `__mutsu_atomic_hash::` shared store, so
    /// concurrent `start { %h{...} = ... }` blocks each writing a different key
    /// all land. Returns the assigned element value.
    pub(crate) fn shared_hash_elem_set(
        &mut self,
        hash_name: &str,
        elem_key: String,
        value: Value,
    ) -> Value {
        let atomic_key = format!("__mutsu_atomic_hash::{hash_name}");
        let updated = {
            let mut shared = self.shared_vars.write().unwrap();
            let mut map = match shared.get(&atomic_key) {
                Some(Value::Hash(h)) => h.as_ref().clone(),
                _ => match shared.get(hash_name).or_else(|| self.env.get(hash_name)) {
                    Some(Value::Hash(h)) => h.as_ref().clone(),
                    _ => crate::value::HashData::default(),
                },
            };
            Value::hash_insert_through(&mut map.map, elem_key, value.clone());
            let new_hash = Value::Hash(std::sync::Arc::new(map));
            shared.insert(atomic_key, new_hash.clone());
            new_hash
        };
        if let Ok(mut dirty) = self.shared_vars_dirty.write() {
            dirty.insert(hash_name.to_string());
        }
        self.env.insert(hash_name.to_string(), updated);
        value
    }

    /// CAS on a multi-dimensional array element: cas(@arr[d1;d2;...], $expected, $new)
    /// Args: [array_name_str, dimensions_list, expected, new_val]
    pub(super) fn builtin_cas_array_multidim(
        &mut self,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if args.len() != 4 {
            return Err(RuntimeError::new(
                "__mutsu_cas_array_multidim requires 4 arguments",
            ));
        }
        let arr_name = args[0].to_string_value();
        let dims: Vec<i64> = match &args[1] {
            Value::Array(elems, ..) => elems
                .iter()
                .map(|v| match v {
                    Value::Int(i) => *i,
                    other => other.to_string_value().parse::<i64>().unwrap_or(0),
                })
                .collect(),
            _ => vec![0],
        };
        let expected = &args[2];
        let new_val = args[3].clone();

        let atomic_key = format!("__mutsu_atomic_arr::{arr_name}");

        // Initialize shared_vars with the array if not yet set
        {
            let shared = self.shared_vars.read().unwrap();
            if !shared.contains_key(&atomic_key) {
                drop(shared);
                let arr = self.env.get(&arr_name).cloned().unwrap_or(Value::Array(
                    std::sync::Arc::new(crate::value::ArrayData::new(Vec::new())),
                    crate::value::ArrayKind::Array,
                ));
                let mut shared = self.shared_vars.write().unwrap();
                if !shared.contains_key(&atomic_key) {
                    shared.insert(atomic_key.clone(), arr);
                }
            }
        }

        let mut did_swap = false;
        let current;
        {
            let mut shared = self.shared_vars.write().unwrap();
            let arr = shared.get(&atomic_key).cloned().unwrap_or(Value::Array(
                std::sync::Arc::new(crate::value::ArrayData::new(Vec::new())),
                crate::value::ArrayKind::Array,
            ));
            // Navigate to the element using the dimension indices
            current = Self::multidim_get(&arr, &dims);
            if Self::cas_retry_matches(&current, expected) {
                let updated = Self::multidim_set(&arr, &dims, new_val);
                shared.insert(atomic_key.clone(), updated);
                did_swap = true;
            }
        }

        if did_swap && let Ok(mut dirty) = self.shared_vars_dirty.write() {
            dirty.insert(arr_name.clone());
        }
        Ok(current)
    }

    /// Get an element from a multi-dimensional array by navigating nested arrays.
    fn multidim_get(arr: &Value, dims: &[i64]) -> Value {
        let mut current = arr.clone();
        for &dim in dims {
            if let Value::Array(ref elements, ..) = current {
                let idx = if dim < 0 {
                    (elements.len() as i64 + dim) as usize
                } else {
                    dim as usize
                };
                current = elements.get(idx).cloned().unwrap_or(Value::Int(0));
            } else {
                return Value::Int(0);
            }
        }
        current
    }

    /// Set an element in a multi-dimensional array by navigating nested arrays.
    /// Returns the updated top-level array.
    fn multidim_set(arr: &Value, dims: &[i64], value: Value) -> Value {
        if dims.is_empty() {
            return value;
        }
        if let Value::Array(elements, kind) = arr {
            let idx = if dims[0] < 0 {
                (elements.len() as i64 + dims[0]) as usize
            } else {
                dims[0] as usize
            };
            let mut new_elements = (**elements).clone();
            while new_elements.len() <= idx {
                new_elements.push(Value::Int(0));
            }
            if dims.len() == 1 {
                new_elements[idx] = value;
            } else {
                new_elements[idx] = Self::multidim_set(&new_elements[idx], &dims[1..], value);
            }
            Value::Array(std::sync::Arc::new(new_elements), *kind)
        } else {
            arr.clone()
        }
    }

    /// After CAS updates an attribute variable (`!attr_name`), update the
    /// corresponding Instance object in env ("self") and store the updated
    /// Instance in shared_vars so the main thread can pick it up after await.
    /// Phase 3 cell-CAS: resolve an attribute-twigil atomic target (`!x`/`.x`)
    /// to `self`'s shared attribute cell and the map key, preferring the method
    /// owner class's qualified private key (Parent/Child same-named `$!priv`
    /// disambiguation, matching the VM's cell-direct access). Returns `None`
    /// when not in an instance method context, falling back to the shared_vars
    /// atomic machinery for plain variables.
    fn self_attr_cell_target(
        &self,
        name: &str,
    ) -> Option<(std::sync::Arc<crate::value::InstanceAttrs>, String)> {
        let bare = name.strip_prefix('!').or_else(|| name.strip_prefix('.'))?;
        if !bare
            .chars()
            .next()
            .is_some_and(|c| c.is_alphabetic() || c == '_')
        {
            return None;
        }
        let Some(Value::Instance { attributes, .. }) = self.env.get("self") else {
            return None;
        };
        let attrs = attributes.clone();
        let key = {
            let map = attrs.as_map();
            match self.method_class_stack.last() {
                Some(owner) => {
                    let qualified = format!("{}\0{}", owner, bare);
                    if map.contains_key(&qualified) {
                        qualified
                    } else {
                        bare.to_string()
                    }
                }
                None => bare.to_string(),
            }
        };
        Some((attrs, key))
    }

    /// CAS on a hash element: cas(%hash{key}, &code)
    /// Args: [hash_name_str, key, code]
    /// Uses shared_vars with an atomic key for cross-thread safety.
    pub(super) fn builtin_cas_hash_elem(
        &mut self,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if args.len() != 3 {
            return Err(RuntimeError::new(
                "__mutsu_cas_hash_elem requires 3 arguments (hash_name, key, code)",
            ));
        }
        let hash_name = args[0].to_string_value();
        let key = args[1].to_string_value();
        let code = args[2].clone();
        let atomic_key = format!("__mutsu_atomic_hash::{hash_name}");

        // Initialize shared_vars with the hash if not yet set
        {
            let shared = self.shared_vars.read().unwrap();
            if !shared.contains_key(&atomic_key) {
                drop(shared);
                let hash = self
                    .env
                    .get(&hash_name)
                    .cloned()
                    .unwrap_or_else(|| Value::Hash(Value::hash_arc(HashMap::new())));
                let mut shared = self.shared_vars.write().unwrap();
                if !shared.contains_key(&atomic_key) {
                    shared.insert(atomic_key.clone(), hash);
                }
            }
        }

        // Check if code is {.succ} or {.pred} for fast path
        if let Value::Sub(ref sub) = code {
            let effective_body: Vec<&Stmt> = sub
                .body
                .iter()
                .filter(|s| !matches!(s, Stmt::SetLine(_)))
                .collect();
            if sub.params.is_empty()
                && effective_body.len() == 1
                && let Stmt::Expr(Expr::MethodCall {
                    target,
                    name: method_name,
                    args: method_args,
                    ..
                }) = effective_body[0]
                && method_args.is_empty()
                && matches!(target.as_ref(), Expr::Var(v) if v == "_" || v == "$_")
            {
                let method_str = method_name.resolve();
                let delta = if method_str == "succ" {
                    Some(1i64)
                } else if method_str == "pred" {
                    Some(-1i64)
                } else {
                    None
                };
                if let Some(d) = delta {
                    let mut shared = self.shared_vars.write().unwrap();
                    let hash = shared
                        .get(&atomic_key)
                        .cloned()
                        .unwrap_or_else(|| Value::Hash(Value::hash_arc(HashMap::new())));
                    if let Value::Hash(ref map) = hash {
                        let current = map.get(&key).cloned().unwrap_or(Value::Int(0));
                        let new_val = crate::builtins::arith_add(current, Value::Int(d))?;
                        let mut new_map = (**map).clone();
                        new_map.insert(key, new_val);
                        let updated = Value::Hash(Value::hash_arc(new_map));
                        shared.insert(atomic_key.clone(), updated.clone());
                        shared.insert(hash_name.clone(), updated.clone());
                        drop(shared);
                        self.env.insert(hash_name.clone(), updated);
                        if let Ok(mut dirty) = self.shared_vars_dirty.write() {
                            dirty.insert(atomic_key);
                            dirty.insert(hash_name);
                        }
                        return Ok(Value::Nil);
                    }
                }
            }
        }

        // General CAS loop for hash elements
        loop {
            let current = {
                let shared = self.shared_vars.read().unwrap();
                let hash = shared
                    .get(&atomic_key)
                    .cloned()
                    .unwrap_or_else(|| Value::Hash(Value::hash_arc(HashMap::new())));
                if let Value::Hash(ref map) = hash {
                    map.get(&key).cloned().unwrap_or(Value::Int(0))
                } else {
                    Value::Int(0)
                }
            };
            let new_val = {
                let call_args = if let Value::Sub(ref sub) = code {
                    if sub.params.is_empty() {
                        self.env.insert("_".to_string(), current.clone());
                        self.env.insert("$_".to_string(), current.clone());
                        Vec::new()
                    } else {
                        vec![current.clone()]
                    }
                } else {
                    vec![current.clone()]
                };
                self.call_sub_value(code.clone(), call_args, true)?
            };
            // CAS: check if value is still `current`, if so store `new_val`
            let mut shared = self.shared_vars.write().unwrap();
            let hash = shared
                .get(&atomic_key)
                .cloned()
                .unwrap_or_else(|| Value::Hash(Value::hash_arc(HashMap::new())));
            if let Value::Hash(ref map) = hash {
                let seen = map.get(&key).cloned().unwrap_or(Value::Int(0));
                if Self::cas_retry_matches(&current, &seen) {
                    let mut new_map = (**map).clone();
                    new_map.insert(key, new_val);
                    let updated = Value::Hash(Value::hash_arc(new_map));
                    shared.insert(atomic_key.clone(), updated.clone());
                    shared.insert(hash_name.clone(), updated.clone());
                    drop(shared);
                    self.env.insert(hash_name.clone(), updated);
                    if let Ok(mut dirty) = self.shared_vars_dirty.write() {
                        dirty.insert(atomic_key);
                        dirty.insert(hash_name);
                    }
                    return Ok(Value::Nil);
                }
            }
            drop(shared);
        }
    }
}
