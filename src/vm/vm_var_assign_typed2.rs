use super::*;

impl Interpreter {
    pub(super) fn coerce_typed_container_assignment(
        &mut self,
        var_name: &str,
        value: Value,
        explicit_initializer: bool,
    ) -> Result<Value, RuntimeError> {
        let coercion_target = |constraint: &str| -> Option<String> {
            if let Some(open) = constraint.find('(')
                && constraint.ends_with(')')
            {
                let target = &constraint[..open];
                if !target.is_empty() {
                    return Some(target.to_string());
                }
            }
            None
        };
        if var_name.starts_with('@')
            && let Some(constraint) = loan_env!(self, var_type_constraint(var_name))
            && let Value::Array(items, kind) = value
        {
            // Native typed arrays cannot store lazy sequences
            if kind.is_lazy()
                && crate::runtime::native_types::is_native_array_element_type(&constraint)
            {
                let declared = format!("array[{}]", constraint);
                return Err(RuntimeError::typed(
                    "X::Cannot::Lazy",
                    [
                        (
                            "message".to_string(),
                            Value::str(format!(
                                "Cannot initialize an array of {} with a lazy list",
                                constraint
                            )),
                        ),
                        ("action".to_string(), Value::str_from("initialize")),
                        ("what".to_string(), Value::str(declared)),
                    ]
                    .into_iter()
                    .collect(),
                ));
            }
            let coerced_items = self.coerce_typed_array_elements(
                var_name,
                &constraint,
                &items,
                kind,
                &coercion_target,
                explicit_initializer,
            )?;
            return Ok(Value::Array(
                Arc::new(crate::value::ArrayData::new(coerced_items)),
                kind,
            ));
        }

        if var_name.starts_with('%')
            && let Value::Hash(map) = value
        {
            // Preserve original keys from the source hash before coercion
            // (coercion creates a new Arc, losing the registration).
            let saved_original_keys =
                runtime::utils::hash_original_keys_snapshot(&Value::Hash(map.clone()));
            let value_constraint = loan_env!(self, var_type_constraint(var_name));
            let key_constraint = loan_env!(self, var_hash_key_constraint(var_name));
            let mut coerced_map = std::collections::HashMap::with_capacity(map.len());
            for (key, val) in map.iter() {
                let coerced_key = if let Some(constraint) = &key_constraint {
                    // Hash keys are always stored as strings internally, but for
                    // key-constrained hashes (e.g. %h{Int}), we need to check
                    // that the key can be interpreted as the constraint type.
                    // Since hash construction stringifies pair keys (e.g. 1 => 2
                    // becomes "1" => 2), we try to reconstruct the typed value
                    // from the string key before checking the constraint.
                    let target_type =
                        coercion_target(constraint).unwrap_or_else(|| constraint.clone());
                    let key_as_typed_value = Self::try_reconstruct_typed_key(key, &target_type);
                    if !loan_env!(self, type_matches_value(&target_type, &key_as_typed_value)) {
                        return Err(runtime::utils::type_check_element_typed_error(
                            var_name,
                            constraint,
                            &Value::str(key.clone()),
                        ));
                    }
                    key.clone()
                } else {
                    key.clone()
                };
                let coerced_val = if let Some(constraint) = &value_constraint {
                    if matches!(val, Value::Nil) {
                        if let Some(default) = self.var_default(var_name) {
                            default.clone()
                        } else if explicit_initializer && self.is_definite_constraint(constraint) {
                            return Err(runtime::utils::type_check_element_typed_error(
                                var_name, constraint, val,
                            ));
                        } else {
                            val.clone()
                        }
                    } else {
                        let target_type =
                            coercion_target(constraint).unwrap_or_else(|| constraint.clone());
                        let coerced = if self.type_matches_value(&target_type, val) {
                            val.clone()
                        } else {
                            loan_env!(
                                self,
                                try_coerce_value_for_constraint(constraint, val.clone())
                            )?
                        };
                        if !self.type_matches_value(&target_type, &coerced) {
                            return Err(runtime::utils::type_check_element_typed_error(
                                var_name, constraint, val,
                            ));
                        }
                        coerced
                    }
                } else {
                    val.clone()
                };
                coerced_map.insert(coerced_key, coerced_val);
            }
            let mut result = Value::hash(coerced_map);
            // Re-embed original keys on the new (coerced) hash.
            if let Some(orig) = saved_original_keys {
                result = runtime::utils::set_hash_original_keys(result, orig);
            }
            return Ok(result);
        }

        Ok(value)
    }

    /// Coerce/check elements of a typed array, recursing into shaped sub-arrays.
    fn coerce_typed_array_elements(
        &mut self,
        var_name: &str,
        constraint: &str,
        items: &[Value],
        kind: crate::value::ArrayKind,
        coercion_target: &dyn Fn(&str) -> Option<String>,
        explicit_initializer: bool,
    ) -> Result<Vec<Value>, RuntimeError> {
        let native_constraint =
            crate::runtime::native_types::is_native_array_element_type(constraint);
        let mut coerced_items = Vec::with_capacity(items.len());
        for item in items.iter() {
            // A lazy/infinite Range (e.g. `-Inf..0e0`, `0e0..Inf`) cannot initialize
            // a native array, regardless of which end is unbounded.
            if native_constraint && crate::builtins::methods_0arg::is_value_lazy(item) {
                return Err(RuntimeError::typed(
                    "X::Cannot::Lazy",
                    [
                        (
                            "message".to_string(),
                            Value::str(format!(
                                "Cannot initialize an array of {} with a lazy list",
                                constraint
                            )),
                        ),
                        ("action".to_string(), Value::str_from("initialize")),
                        (
                            "what".to_string(),
                            Value::str(format!("array[{constraint}]")),
                        ),
                    ]
                    .into_iter()
                    .collect(),
                ));
            }
            // A type-object hole (e.g. an `Any` gap from `@a[1] = x`) assigned to a
            // native array becomes that array's default (`0`/`0e0`/`""`).
            if native_constraint && matches!(item, Value::Package(_)) {
                coerced_items.push(Self::native_fill_for_constraint(Some(constraint)));
                continue;
            }
            if matches!(item, Value::Nil) {
                if let Some(default) = self.var_default(var_name) {
                    coerced_items.push(default.clone());
                } else if explicit_initializer && self.is_definite_constraint(constraint) {
                    return Err(runtime::utils::type_check_element_typed_error(
                        var_name, constraint, item,
                    ));
                } else {
                    coerced_items.push(item.clone());
                }
                continue;
            }
            // For shaped arrays, sub-arrays are structural — recurse into them
            if kind == crate::value::ArrayKind::Shaped
                && let Value::Array(sub_items, sub_kind) = item
            {
                let sub_coerced = self.coerce_typed_array_elements(
                    var_name,
                    constraint,
                    sub_items,
                    *sub_kind,
                    coercion_target,
                    explicit_initializer,
                )?;
                coerced_items.push(Value::Array(
                    Arc::new(crate::value::ArrayData::new(sub_coerced)),
                    *sub_kind,
                ));
                continue;
            }
            let target_type = coercion_target(constraint).unwrap_or_else(|| constraint.to_string());
            // Whether the constraint is an explicit coercion type like `Array()`.
            // A plain type constraint (e.g. `Array`) must type-check elements
            // strictly without coercing scalars into containers.
            let is_coercion = coercion_target(constraint).is_some();
            let coerced = if self.type_matches_value(&target_type, item) {
                item.clone()
            } else if target_type == "Array" {
                match item {
                    Value::Array(items, kind) if !kind.is_real_array() => {
                        Value::Array(items.clone(), crate::value::ArrayKind::Array)
                    }
                    Value::Scalar(inner) => match inner.as_ref() {
                        Value::Array(items, kind) if !kind.is_real_array() => {
                            Value::Array(items.clone(), crate::value::ArrayKind::Array)
                        }
                        Value::Array(..) => inner.as_ref().clone(),
                        _ if is_coercion => loan_env!(
                            self,
                            try_coerce_value_for_constraint("Array()", item.clone())
                        )?,
                        _ => item.clone(),
                    },
                    _ if is_coercion => loan_env!(
                        self,
                        try_coerce_value_for_constraint("Array()", item.clone())
                    )?,
                    _ => item.clone(),
                }
            } else if matches!(target_type.as_str(), "Array" | "List" | "Hash") && is_coercion {
                self.try_coerce_value_for_constraint(&format!("{target_type}()"), item.clone())?
            } else {
                loan_env!(
                    self,
                    try_coerce_value_for_constraint(constraint, item.clone())
                )?
            };
            if !self.type_matches_value(&target_type, &coerced) {
                return Err(runtime::utils::type_check_element_typed_error(
                    var_name, constraint, item,
                ));
            }
            // Wrap/check native integer overflow for native typed arrays
            let coerced = Self::wrap_native_int_by_constraint(&target_type, coerced)?;
            coerced_items.push(coerced);
        }
        Ok(coerced_items)
    }

    /// Resolve a GenericRange with WhateverCode endpoints into a concrete range
    /// by evaluating the lambda endpoints against the given array length.
    pub(super) fn resolve_generic_range_for_assign(
        &mut self,
        idx: &Value,
        array_len: usize,
    ) -> Option<Value> {
        if let Value::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } = idx
        {
            let len = array_len as i64;
            let resolve_endpoint = |vm: &mut Self, val: &Value| -> i64 {
                match val {
                    Value::Int(i) => *i,
                    Value::Sub(data) => {
                        let mut sub_env = data.env.clone();
                        for p in &data.params {
                            sub_env.insert(p.to_string(), Value::Int(len));
                        }
                        let saved_env = std::mem::take(vm.env_mut());
                        *vm.env_mut() = sub_env;
                        let result = vm.eval_block_value(&data.body).unwrap_or(Value::Nil);
                        *vm.env_mut() = saved_env;
                        match result {
                            Value::Int(i) => i,
                            _ => 0,
                        }
                    }
                    Value::Num(f) => *f as i64,
                    _ => 0,
                }
            };
            let s = resolve_endpoint(self, start);
            let e = resolve_endpoint(self, end);
            let resolved = if *excl_start && *excl_end {
                Value::RangeExclBoth(s, e)
            } else if *excl_start {
                Value::RangeExclStart(s, e)
            } else if *excl_end {
                Value::RangeExcl(s, e)
            } else {
                Value::Range(s, e)
            };
            Some(resolved)
        } else {
            None
        }
    }

    pub(super) fn slice_indices_from_index(idx: &Value) -> Option<Vec<usize>> {
        match idx {
            Value::Range(a, b) => {
                let start = (*a).max(0);
                let end = (*b).min(start.saturating_add(Self::MAX_ASSIGN_SLICE_EXPAND));
                if end < start {
                    return Some(Vec::new());
                }
                Some(
                    (start..=end)
                        .filter_map(|i| usize::try_from(i).ok())
                        .collect(),
                )
            }
            Value::RangeExcl(a, b) => {
                let start = (*a).max(0);
                let end_excl = (*b)
                    .max(0)
                    .min(start.saturating_add(Self::MAX_ASSIGN_SLICE_EXPAND));
                if start >= end_excl {
                    return Some(Vec::new());
                }
                Some(
                    (start..end_excl)
                        .filter_map(|i| usize::try_from(i).ok())
                        .collect(),
                )
            }
            Value::RangeExclStart(a, b) => {
                let start = a.saturating_add(1).max(0);
                let end = (*b).min(start.saturating_add(Self::MAX_ASSIGN_SLICE_EXPAND));
                if end < start {
                    return Some(Vec::new());
                }
                Some(
                    (start..=end)
                        .filter_map(|i| usize::try_from(i).ok())
                        .collect(),
                )
            }
            Value::RangeExclBoth(a, b) => {
                let start = a.saturating_add(1).max(0);
                let end_excl = (*b)
                    .max(0)
                    .min(start.saturating_add(Self::MAX_ASSIGN_SLICE_EXPAND));
                if start >= end_excl {
                    return Some(Vec::new());
                }
                Some(
                    (start..end_excl)
                        .filter_map(|i| usize::try_from(i).ok())
                        .collect(),
                )
            }
            _ => None,
        }
    }

    /// Create an X::OutOfRange RuntimeError for negative index assignment
    pub(super) fn make_out_of_range_error(effective_index: i64) -> RuntimeError {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert(
            "message".to_string(),
            Value::str_from(&format!(
                "Effective index out of range. Is: {}, should be in 0..^Inf",
                effective_index
            )),
        );
        attrs.insert("got".to_string(), Value::Int(effective_index));
        attrs.insert("range".to_string(), Value::str_from("0..^Inf"));
        RuntimeError::typed("X::OutOfRange", attrs)
    }
}
