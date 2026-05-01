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
        // For instance attribute variables (private `!attr` or public `.attr`),
        // use a per-instance shared_vars key so that multiple method calls on the
        // same object (via different Value clones, e.g. in Junction gist) see
        // consistent state. The key includes the instance ID for isolation.
        if name.starts_with('!') || name.starts_with('.') {
            let instance_id = match self.env.get("self") {
                Some(Value::Instance { id, .. }) => Some(*id),
                _ => None,
            };
            if let Some(id) = instance_id {
                // Use shared_vars keyed by instance ID + attr name for atomicity
                let instance_key = format!("__mutsu_atomic_attr::{}::{}", id, name);
                let mut shared = self.shared_vars.write().unwrap();
                let current = shared
                    .get(&instance_key)
                    .cloned()
                    .or_else(|| self.env.get(&name).cloned())
                    .unwrap_or(Value::Int(0));
                let next = crate::builtins::arith_add(current.clone(), Value::Int(delta))?;
                self.env.insert(name, next.clone());
                shared.insert(instance_key, next.clone());
                drop(shared);
                return if return_old { Ok(current) } else { Ok(next) };
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
        // For instance attributes, use an instance-specific key to avoid
        // stale data when a new instance reuses the same attribute name.
        let effective_name = if name.starts_with('!') {
            if let Some(Value::Instance { id, .. }) = self.env.get("self") {
                format!("{}::{}", name, id)
            } else {
                name.clone()
            }
        } else {
            name.clone()
        };
        let value_key = self.atomic_value_key_for_name(&effective_name);

        if args.len() == 3 {
            // 3-arg form: cas($var, $expected, $new)
            let expected = &args[1];
            let new_val = args[2].clone();
            let coerced = self.atomic_assign_coerced_value(&name, new_val)?;
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
                // If the variable is an instance attribute (!attr_name),
                // also update the Instance in env and shared_vars so the
                // main thread can pick up the change after await.
                if let Some(attr_name) = name.strip_prefix('!') {
                    self.sync_atomic_attribute_to_instance(attr_name, &coerced);
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
                    std::sync::Arc::new(Vec::new()),
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
                std::sync::Arc::new(Vec::new()),
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
        // Note: don't update env["@values"] here — it would be overwritten
        // by ensure_env_synced with stale locals. Instead, GetLocal checks
        // the atomic shared key directly.
        Ok(current)
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
                    std::sync::Arc::new(Vec::new()),
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
                std::sync::Arc::new(Vec::new()),
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
    fn sync_atomic_attribute_to_instance(&mut self, attr_name: &str, new_val: &Value) {
        if let Some(Value::Instance {
            class_name,
            attributes,
            id,
        }) = self.env.get("self").cloned()
        {
            let mut new_attrs = (*attributes).clone();
            new_attrs.insert(attr_name.to_string(), new_val.clone());
            let updated_instance = Value::make_instance_with_id(class_name, new_attrs, id);
            self.env
                .insert("self".to_string(), updated_instance.clone());
            self.env
                .insert("__ANON_STATE__".to_string(), updated_instance.clone());
            // Store the updated instance in shared_vars keyed by instance id
            let instance_key = format!("__mutsu_instance::{id}");
            {
                let mut shared = self.shared_vars.write().unwrap();
                shared.insert(instance_key.clone(), updated_instance);
            }
            if let Ok(mut dirty) = self.shared_vars_dirty.write() {
                dirty.insert(instance_key);
            }
        }
    }
}
