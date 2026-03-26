use super::*;
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use std::sync::atomic::{AtomicU64, Ordering};

pub(super) static ATOMIC_VAR_KEY_COUNTER: AtomicU64 = AtomicU64::new(1);

impl Interpreter {
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
                if !is_user_visible(key) {
                    continue;
                }
                if crate::runtime::values_identical(value, &current) {
                    return key.clone();
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
                return Err(RuntimeError::new(
                    crate::runtime::utils::type_check_assignment_error(name, &constraint, &value),
                ));
            }
            if !matches!(value, Value::Nil | Value::Package(_)) {
                value = self.try_coerce_value_for_constraint(&constraint, value)?;
            }
        }
        Ok(value)
    }

    pub(super) fn atomic_value_key_for_name(&mut self, name: &str) -> String {
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

    pub(super) fn builtin_atomic_update_unit(
        &mut self,
        args: &[Value],
        delta: i64,
        return_old: bool,
    ) -> Result<Value, RuntimeError> {
        let name = self.atomic_var_name_arg(args)?;
        self.check_readonly_for_modify(&name)?;
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
        let value_key = self.atomic_value_key_for_name(&name);

        if args.len() == 3 {
            // 3-arg form: cas($var, $expected, $new)
            let expected = &args[1];
            let new_val = args[2].clone();
            let coerced = self.atomic_assign_coerced_value(&name, new_val)?;
            let mut did_swap = false;
            let current = {
                let mut shared = self.shared_vars.write().unwrap();
                let current = self.atomic_current_value(&shared, &name, &value_key);
                if current == *expected {
                    shared.insert(value_key.clone(), coerced.clone());
                    did_swap = true;
                }
                current
            };
            if did_swap {
                self.env.insert(name.clone(), coerced);
                if let Ok(mut dirty) = self.shared_vars_dirty.write() {
                    dirty.insert(value_key);
                    dirty.insert(name);
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
            loop {
                let current = {
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
                    let result = self.call_sub_value(code.clone(), call_args, bind_dollar_topic)?;
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
                let coerced = self.atomic_assign_coerced_value(&name, new_val)?;

                let mut updated = false;
                {
                    let mut shared = self.shared_vars.write().unwrap();
                    let seen = self.atomic_current_value(&shared, &name, &value_key);
                    if Self::cas_retry_matches(&current, &seen) {
                        shared.insert(value_key.clone(), coerced.clone());
                        updated = true;
                    } else {
                        self.env.insert(name.clone(), seen);
                    }
                }
                if updated {
                    self.env.insert(name.clone(), coerced.clone());
                    if let Ok(mut dirty) = self.shared_vars_dirty.write() {
                        dirty.insert(value_key.clone());
                        dirty.insert(name.clone());
                    }
                    return Ok(coerced);
                }
            }
        }
    }
}
