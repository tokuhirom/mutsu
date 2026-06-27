//! Compare-and-swap on atomic scalar variables (`cas($var, $expected, $new)` and
//! the `cas($var, &code)` block form) and on single array elements. Shares the
//! name/value-key machinery and `cas_retry_matches` from `builtins_atomic`; the
//! shared-container and multidim/hash CAS helpers live in
//! `builtins_atomic_shared`.

use super::*;
use crate::token_kind::TokenKind;

impl Interpreter {
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
}
