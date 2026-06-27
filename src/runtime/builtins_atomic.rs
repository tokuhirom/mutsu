//! Atomic scalar-variable operations for the `⚛`-operators (`⚛=`/`⚛+=`/`⚛++`/
//! `--⚛`): name canonicalization, the shared-store value-key machinery, and the
//! fetch/store/add/inc/dec primitives. Compare-and-swap lives in
//! `builtins_atomic_cas` (vars/array elements) and `builtins_atomic_shared`
//! (shared-container helpers + multidim/hash CAS).

use super::*;
use crate::symbol::Symbol;
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
}
