use super::*;
use crate::symbol::Symbol;
use std::collections::HashMap;
use std::sync::Arc;
use unicode_normalization::UnicodeNormalization;

impl VM {
    pub(super) fn delegated_mixin_attr_key(
        &self,
        mixins: &std::collections::HashMap<String, Value>,
        method_name: &str,
    ) -> Option<String> {
        self.interpreter
            .delegated_role_attr_key_from_mixins(mixins, method_name)
    }

    fn assign_mixin_container_slot(
        attr_value: &mut Value,
        idx: &Value,
        val: &Value,
        range_slice: &Option<(Vec<usize>, Vec<Value>)>,
    ) -> bool {
        match attr_value {
            Value::Array(items, kind) if !matches!(idx, Value::Str(_)) => {
                let mut updated = (**items).clone();
                if let Some((slice_indices, vals)) = range_slice {
                    if let Some(max_idx) = slice_indices.last().copied()
                        && max_idx >= updated.len()
                    {
                        updated.resize(max_idx + 1, Value::Package(Symbol::intern("Any")));
                    }
                    for (offset, i) in slice_indices.iter().enumerate() {
                        updated[*i] = vals.get(offset).cloned().unwrap_or(Value::Nil);
                    }
                } else if let Some(i) = Self::index_to_usize(idx) {
                    if i >= updated.len() {
                        updated.resize(i + 1, Value::Package(Symbol::intern("Any")));
                    }
                    updated[i] = val.clone();
                } else {
                    return false;
                }
                *attr_value = Value::Array(Arc::new(updated), *kind);
                true
            }
            Value::Hash(hash) if matches!(idx, Value::Str(_)) => {
                let mut updated = (**hash).clone();
                updated.insert(idx.to_string_value(), val.clone());
                *attr_value = Value::Hash(Arc::new(updated));
                true
            }
            Value::Nil if !matches!(idx, Value::Str(_)) => {
                let mut updated = Vec::new();
                if let Some((slice_indices, vals)) = range_slice {
                    if let Some(max_idx) = slice_indices.last().copied() {
                        updated.resize(max_idx + 1, Value::Package(Symbol::intern("Any")));
                    }
                    for (offset, i) in slice_indices.iter().enumerate() {
                        updated[*i] = vals.get(offset).cloned().unwrap_or(Value::Nil);
                    }
                } else if let Some(i) = Self::index_to_usize(idx) {
                    updated.resize(i + 1, Value::Package(Symbol::intern("Any")));
                    updated[i] = val.clone();
                } else {
                    return false;
                }
                *attr_value = Value::real_array(updated);
                true
            }
            Value::Nil if matches!(idx, Value::Str(_)) => {
                let mut updated = std::collections::HashMap::new();
                updated.insert(idx.to_string_value(), val.clone());
                *attr_value = Value::Hash(Arc::new(updated));
                true
            }
            _ => false,
        }
    }

    fn varref_target(value: &Value) -> Option<(String, Option<usize>)> {
        if let Value::Capture { positional, named } = value
            && positional.is_empty()
            && let Some(Value::Str(name)) = named.get("__mutsu_varref_name")
        {
            let source_index = match named.get("__mutsu_varref_index") {
                Some(Value::Int(i)) if *i >= 0 => Some(*i as usize),
                _ => None,
            };
            return Some((name.to_string(), source_index));
        }
        None
    }

    fn make_varref_value(name: String, value: Value, source_index: Option<usize>) -> Value {
        let mut named = std::collections::HashMap::new();
        named.insert("__mutsu_varref_name".to_string(), Value::str(name));
        named.insert("__mutsu_varref_value".to_string(), value);
        if let Some(i) = source_index {
            named.insert("__mutsu_varref_index".to_string(), Value::Int(i as i64));
        }
        Value::Capture {
            positional: Vec::new(),
            named,
        }
    }

    fn assign_varref_target(
        &mut self,
        source_name: &str,
        source_index: Option<usize>,
        value: Value,
    ) -> Result<(), RuntimeError> {
        if let Some(i) = source_index {
            let Some(container) = self.interpreter.env_mut().get_mut(source_name) else {
                return Err(RuntimeError::assignment_ro(None));
            };
            let Value::Array(items, ..) = container else {
                return Err(RuntimeError::assignment_ro(None));
            };
            let arr = Arc::make_mut(items);
            if i >= arr.len() {
                arr.resize(i + 1, Value::Package(Symbol::intern("Any")));
            }
            arr[i] = value;
            return Ok(());
        }
        self.interpreter
            .env_mut()
            .insert(source_name.to_string(), value);
        Ok(())
    }

    fn resolve_whatever_index_for_target(&mut self, idx: Value, target: Option<&Value>) -> Value {
        let len = match target {
            Some(Value::Array(items, ..)) => items.len() as i64,
            _ => 0,
        };
        // Bare Whatever (*) in array subscript means all indices: 0, 1, ..., len-1
        if matches!(idx, Value::Whatever) {
            let indices: Vec<Value> = (0..len).map(Value::Int).collect();
            return Value::Array(Arc::new(indices), crate::value::ArrayKind::List);
        }
        if let Value::Sub(ref data) = idx {
            let param = data.params.first().map(|s| s.as_str()).unwrap_or("_");
            let mut sub_env = data.env.clone();
            sub_env.insert(param.to_string(), Value::Int(len));
            let saved_env = std::mem::take(self.interpreter.env_mut());
            *self.interpreter.env_mut() = sub_env;
            let result = self
                .interpreter
                .eval_block_value(&data.body)
                .unwrap_or(Value::Nil);
            *self.interpreter.env_mut() = saved_env;
            return result;
        }
        // Resolve Array of WhateverCode indices: @a[*-3, *-2, *-1]
        if let Value::Array(ref items, kind) = idx {
            let mut needs_resolve = false;
            for item in items.iter() {
                if matches!(item, Value::Sub(_)) {
                    needs_resolve = true;
                    break;
                }
            }
            if needs_resolve {
                let mut resolved = Vec::with_capacity(items.len());
                for item in items.iter() {
                    if let Value::Sub(data) = item {
                        let param = data.params.first().map(|s| s.as_str()).unwrap_or("_");
                        let mut sub_env = data.env.clone();
                        sub_env.insert(param.to_string(), Value::Int(len));
                        let saved_env = std::mem::take(self.interpreter.env_mut());
                        *self.interpreter.env_mut() = sub_env;
                        let result = self
                            .interpreter
                            .eval_block_value(&data.body)
                            .unwrap_or(Value::Nil);
                        *self.interpreter.env_mut() = saved_env;
                        resolved.push(result);
                    } else {
                        resolved.push(item.clone());
                    }
                }
                return Value::Array(Arc::new(resolved), kind);
            }
        }
        idx
    }

    pub(super) fn quant_hash_trait_from_constraint(constraint: &str) -> Option<&'static str> {
        let base = constraint
            .split_once('[')
            .map(|(head, _)| head)
            .unwrap_or(constraint);
        match base {
            "Mix" => Some("Mix"),
            "MixHash" => Some("MixHash"),
            _ => None,
        }
    }

    /// After assigning to a hash variable, check if any values in the new hash
    /// reference the old hash Arc (captured on the RHS before assignment).
    /// If so, replace them with the new hash's Arc to create a true circular
    /// reference, matching Raku's container semantics for `%h = :b(%h)`.
    pub(super) fn fixup_circular_hash_refs(new_val: &mut Value, old_ptr: &Option<usize>) {
        let Some(old_ptr) = old_ptr else { return };
        if let Value::Hash(new_arc) = new_val {
            // Check if any values in the hash reference the old Arc.
            let has_old_ref = new_arc.values().any(|v| {
                if let Value::Hash(inner_arc) = v {
                    Arc::as_ptr(inner_arc) as usize == *old_ptr
                } else {
                    false
                }
            });
            if !has_old_ref {
                return;
            }
            // Build a new map where old-hash references are replaced with
            // a placeholder, then wrap it in an Arc and fix up the placeholder.
            let mut new_map = HashMap::new();
            let mut circular_keys = Vec::new();
            for (k, v) in new_arc.iter() {
                if let Value::Hash(inner_arc) = v
                    && Arc::as_ptr(inner_arc) as usize == *old_ptr
                {
                    circular_keys.push(k.clone());
                    // Placeholder - will be replaced below
                    new_map.insert(k.clone(), Value::Nil);
                    continue;
                }
                new_map.insert(k.clone(), v.clone());
            }
            // Create the new Arc with the map
            let result_arc = Arc::new(new_map);
            // Now fix up the circular references: set the placeholder values
            // to point to the result_arc itself.
            let map = Arc::as_ptr(&result_arc) as *mut HashMap<String, Value>;
            for key in &circular_keys {
                // SAFETY: We just created result_arc and hold the only reference,
                // so no other thread can access it.
                unsafe {
                    (*map).insert(key.clone(), Value::Hash(result_arc.clone()));
                }
            }
            *new_arc = result_arc;
        }
    }

    fn coerce_hash_var_value(&mut self, name: &str, value: Value) -> Result<Value, RuntimeError> {
        if let Some(constraint) = self.interpreter.var_type_constraint_fast(name).cloned()
            && let Some(trait_name) = Self::quant_hash_trait_from_constraint(&constraint)
        {
            return self.try_compiled_method_or_interpret(value, trait_name, vec![]);
        }
        if self.interpreter.check_readonly_for_modify(name).is_err()
            && matches!(
                value,
                Value::Set(_, _) | Value::Bag(_, _) | Value::Mix(_, _)
            )
        {
            return Ok(value);
        }
        if let Some(constraint) = self.interpreter.var_type_constraint(name)
            && constraint.starts_with("SetHash")
        {
            let result = runtime::utils::coerce_value_to_quanthash(&value);
            // SetHash should be mutable
            if let Value::Set(items, _) = result {
                return Ok(Value::Set(items, true));
            }
            return Ok(result);
        }
        // For Array/Seq/Slip values, use `build_hash_from_items` which
        // raises "Odd number of elements" when appropriate. Hash values from
        // scalar containers (`$h`) are NOT pre-flattened, so they appear as
        // opaque items (triggering the odd-number check when expected).
        let hash_val = match value {
            Value::Array(ref items, kind) => {
                // When a hash variable (%h) appears in a list being assigned to
                // another hash (%m = %h, a => 42), the hash should be flattened
                // into its pairs. However, a hash in a scalar ($hashitem) should
                // stay opaque. We can distinguish: if the array has >1 element
                // and contains Hash values alongside Pair values, flatten them.
                let needs_flatten = !kind.is_itemized()
                    && items.len() > 1
                    && items.iter().any(|v| matches!(v, Value::Hash(_)));
                if needs_flatten {
                    let mut flattened = Vec::new();
                    for item in items.iter() {
                        if let Value::Hash(h) = item {
                            for (k, v) in h.as_ref() {
                                flattened.push(Value::Pair(k.clone(), Box::new(v.clone())));
                            }
                        } else {
                            flattened.push(item.clone());
                        }
                    }
                    runtime::utils::build_hash_from_items(flattened)?
                } else {
                    runtime::utils::build_hash_from_items(items.iter().cloned().collect())?
                }
            }
            Value::Seq(ref items) | Value::Slip(ref items) => {
                runtime::utils::build_hash_from_items(items.iter().cloned().collect())?
            }
            _ => runtime::coerce_to_hash(value),
        };
        // Resolve hash sentinel entries (bound variable refs) when assigning
        // to a new hash variable. Assignment creates new containers.
        if let Value::Hash(ref items) = hash_val
            && Self::hash_has_sentinels(items)
        {
            return Ok(self.resolve_hash_for_iteration(items));
        }
        Ok(hash_val)
    }

    fn mix_assignment_weight(value: &Value) -> f64 {
        match value {
            Value::Int(i) => *i as f64,
            Value::Num(n) => *n,
            Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
            Value::Bool(flag) => {
                if *flag {
                    1.0
                } else {
                    0.0
                }
            }
            _ => {
                if value.truthy() {
                    1.0
                } else {
                    0.0
                }
            }
        }
    }

    const LAZY_ASSIGN_PRESERVE_MARKER: &str = "__mutsu_preserve_lazy_on_array_assign";
    const MAX_ASSIGN_SLICE_EXPAND: i64 = 100_000;

    fn assignment_rhs_values(&mut self, val: &Value) -> Result<Vec<Value>, RuntimeError> {
        Ok(match val {
            Value::Array(v, ..) => v.as_ref().clone(),
            Value::Seq(v) | Value::Slip(v) => v.iter().cloned().collect(),
            Value::Range(a, b) => {
                let end = (*b).min(a.saturating_add(Self::MAX_ASSIGN_SLICE_EXPAND));
                if end < *a {
                    Vec::new()
                } else {
                    (*a..=end).map(Value::Int).collect()
                }
            }
            Value::RangeExcl(a, b) => {
                let end = (*b).min(a.saturating_add(Self::MAX_ASSIGN_SLICE_EXPAND));
                if end <= *a {
                    Vec::new()
                } else {
                    (*a..end).map(Value::Int).collect()
                }
            }
            Value::RangeExclStart(a, b) => {
                let start = a.saturating_add(1);
                let end = (*b).min(start.saturating_add(Self::MAX_ASSIGN_SLICE_EXPAND));
                if end < start {
                    Vec::new()
                } else {
                    (start..=end).map(Value::Int).collect()
                }
            }
            Value::RangeExclBoth(a, b) => {
                let start = a.saturating_add(1);
                let end = (*b).min(start.saturating_add(Self::MAX_ASSIGN_SLICE_EXPAND));
                if end <= start {
                    Vec::new()
                } else {
                    (start..end).map(Value::Int).collect()
                }
            }
            Value::LazyList(list) => self.force_lazy_list_vm(list)?,
            _ => vec![val.clone()],
        })
    }

    pub(super) fn normalize_scalar_assignment_value(val: Value) -> Value {
        let is_nilish = |v: &Value| match v {
            Value::Nil => true,
            Value::Package(sym) => sym.resolve() == "Any",
            _ => false,
        };
        match val {
            Value::Array(items, kind) if items.len() == 1 => {
                if items.first().is_some_and(is_nilish) {
                    Value::Nil
                } else {
                    Value::Array(items, kind)
                }
            }
            Value::Seq(items) if items.len() == 1 && items.first().is_some_and(is_nilish) => {
                Value::Nil
            }
            Value::Slip(items) if items.len() == 1 && items.first().is_some_and(is_nilish) => {
                Value::Nil
            }
            other => other,
        }
    }

    fn extract_varref_binding(raw_val: Value) -> (Value, Option<String>) {
        if let Value::Capture { positional, named } = &raw_val
            && positional.is_empty()
            && let Some(Value::Str(name)) = named.get("__mutsu_varref_name")
            && let Some(inner) = named.get("__mutsu_varref_value")
        {
            return (inner.clone(), Some(name.to_string()));
        }
        (raw_val, None)
    }

    fn resolve_sigilless_alias_source_name(&self, source_name: &str) -> String {
        let mut resolved = source_name.to_string();
        let mut seen = std::collections::HashSet::new();
        while seen.insert(resolved.clone()) {
            let key = format!("__mutsu_sigilless_alias::{}", resolved);
            let Some(Value::Str(next)) = self.interpreter.env().get(&key) else {
                break;
            };
            resolved = next.to_string();
        }
        resolved
    }

    /// Try to reconstruct a typed value from a stringified hash key.
    /// Hash keys are always stored as strings, but for key-constrained hashes
    /// (e.g. `%h{Int}`), we need to check the original type. If the string
    /// looks like a valid value of the target type, return that typed value.
    fn try_reconstruct_typed_key(key: &str, target_type: &str) -> Value {
        match target_type {
            "Int" => {
                if let Ok(n) = key.parse::<i64>() {
                    return Value::Int(n);
                }
            }
            "Num" => {
                if let Ok(n) = key.parse::<f64>() {
                    return Value::Num(n);
                }
            }
            "Numeric" | "Real" | "Cool" | "Any" | "Mu" => {
                // These accept strings, so just return the string
                return Value::str(key.to_string());
            }
            _ => {}
        }
        Value::str(key.to_string())
    }

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
            && let Some(constraint) = self.interpreter.var_type_constraint(var_name)
            && let Value::Array(items, kind) = value
        {
            let coerced_items = self.coerce_typed_array_elements(
                var_name,
                &constraint,
                &items,
                kind,
                &coercion_target,
                explicit_initializer,
            )?;
            return Ok(Value::Array(Arc::new(coerced_items), kind));
        }

        if var_name.starts_with('%')
            && let Value::Hash(map) = value
        {
            let value_constraint = self.interpreter.var_type_constraint(var_name);
            let key_constraint = self.interpreter.var_hash_key_constraint(var_name);
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
                    if !self
                        .interpreter
                        .type_matches_value(&target_type, &key_as_typed_value)
                    {
                        return Err(RuntimeError::new(runtime::utils::type_check_element_error(
                            var_name,
                            constraint,
                            &Value::str(key.clone()),
                        )));
                    }
                    key.clone()
                } else {
                    key.clone()
                };
                let coerced_val = if let Some(constraint) = &value_constraint {
                    if matches!(val, Value::Nil) {
                        if let Some(default) = self.interpreter.var_default(var_name) {
                            default.clone()
                        } else if explicit_initializer
                            && self.interpreter.is_definite_constraint(constraint)
                        {
                            return Err(RuntimeError::new(
                                runtime::utils::type_check_element_error(var_name, constraint, val),
                            ));
                        } else {
                            val.clone()
                        }
                    } else {
                        let target_type =
                            coercion_target(constraint).unwrap_or_else(|| constraint.clone());
                        let coerced = if self.interpreter.type_matches_value(&target_type, val) {
                            val.clone()
                        } else {
                            self.interpreter
                                .try_coerce_value_for_constraint(constraint, val.clone())?
                        };
                        if !self.interpreter.type_matches_value(&target_type, &coerced) {
                            return Err(RuntimeError::new(
                                runtime::utils::type_check_element_error(var_name, constraint, val),
                            ));
                        }
                        coerced
                    }
                } else {
                    val.clone()
                };
                coerced_map.insert(coerced_key, coerced_val);
            }
            return Ok(Value::hash(coerced_map));
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
        let mut coerced_items = Vec::with_capacity(items.len());
        for item in items.iter() {
            if matches!(item, Value::Nil) {
                if let Some(default) = self.interpreter.var_default(var_name) {
                    coerced_items.push(default.clone());
                } else if explicit_initializer
                    && self.interpreter.is_definite_constraint(constraint)
                {
                    return Err(RuntimeError::new(runtime::utils::type_check_element_error(
                        var_name, constraint, item,
                    )));
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
                coerced_items.push(Value::Array(Arc::new(sub_coerced), *sub_kind));
                continue;
            }
            let target_type = coercion_target(constraint).unwrap_or_else(|| constraint.to_string());
            let coerced = if self.interpreter.type_matches_value(&target_type, item) {
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
                        _ => self
                            .interpreter
                            .try_coerce_value_for_constraint("Array()", item.clone())?,
                    },
                    _ => self
                        .interpreter
                        .try_coerce_value_for_constraint("Array()", item.clone())?,
                }
            } else if matches!(target_type.as_str(), "Array" | "List" | "Hash") {
                self.interpreter
                    .try_coerce_value_for_constraint(&format!("{target_type}()"), item.clone())?
            } else {
                self.interpreter
                    .try_coerce_value_for_constraint(constraint, item.clone())?
            };
            if !self.interpreter.type_matches_value(&target_type, &coerced) {
                return Err(RuntimeError::new(runtime::utils::type_check_element_error(
                    var_name, constraint, item,
                )));
            }
            coerced_items.push(coerced);
        }
        Ok(coerced_items)
    }

    /// Resolve a GenericRange with WhateverCode endpoints into a concrete range
    /// by evaluating the lambda endpoints against the given array length.
    fn resolve_generic_range_for_assign(&mut self, idx: &Value, array_len: usize) -> Option<Value> {
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
                        let param = data.params.first().map(|s| s.as_str()).unwrap_or("_");
                        let mut sub_env = data.env.clone();
                        sub_env.insert(param.to_string(), Value::Int(len));
                        let saved_env = std::mem::take(vm.interpreter.env_mut());
                        *vm.interpreter.env_mut() = sub_env;
                        let result = vm
                            .interpreter
                            .eval_block_value(&data.body)
                            .unwrap_or(Value::Nil);
                        *vm.interpreter.env_mut() = saved_env;
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

    fn slice_indices_from_index(idx: &Value) -> Option<Vec<usize>> {
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
    fn make_out_of_range_error(effective_index: i64) -> RuntimeError {
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

    pub(super) fn exec_string_concat_op(&mut self, n: u32) -> Result<(), RuntimeError> {
        let n = n as usize;
        let start = self.stack.len() - n;
        let values: Vec<Value> = self.stack.drain(start..).collect();
        let mut result = String::new();
        for v in values {
            // Buf/Blob instances with "bytes" attribute: call .Str which throws X::Buf::AsStr
            // Blob type objects (no "bytes" attr, e.g. $*DISTRO.signature) stringify to ""
            if let Value::Instance { attributes, .. } = &v
                && Self::is_buf_value(&v)
                && attributes.contains_key("bytes")
            {
                let str_result = self.try_compiled_method_or_interpret(v, "Str", Vec::new())?;
                result.push_str(&str_result.to_string_value());
                continue;
            }
            // For non-Buf instances, try .Stringy() for string context (Raku spec:
            // string interpolation calls .Str which delegates to .Stringy).
            if let Value::Instance { .. } = &v {
                if let Ok(str_result) =
                    self.try_compiled_method_or_interpret(v.clone(), "Stringy", Vec::new())
                {
                    result.push_str(&str_result.to_string_value());
                    continue;
                }
                // Fall back to .Str() if .Stringy() is not defined
                if let Ok(str_result) =
                    self.try_compiled_method_or_interpret(v.clone(), "Str", Vec::new())
                {
                    result.push_str(&str_result.to_string_value());
                    continue;
                }
                // Fall through to default stringification
                result.push_str(&crate::runtime::utils::coerce_to_str(&v));
                continue;
            }
            result.push_str(&crate::runtime::utils::coerce_to_str(&v));
        }
        let normalized: String = result.nfc().collect();
        self.stack.push(Value::str(normalized));
        Ok(())
    }

    /// Increment a value, calling .succ() on Instance values with custom methods.
    pub(super) fn increment_value_smart(&mut self, val: &Value) -> Result<Value, RuntimeError> {
        if let Value::Instance { .. } = val
            && let Ok(result) =
                self.interpreter
                    .call_method_with_values(val.clone(), "succ", vec![])
        {
            return Ok(result);
        }
        Ok(Self::increment_value(val))
    }

    /// Decrement a value, calling .pred() on Instance values with custom methods.
    pub(super) fn decrement_value_smart(&mut self, val: &Value) -> Result<Value, RuntimeError> {
        if let Value::Instance { .. } = val
            && let Ok(result) =
                self.interpreter
                    .call_method_with_values(val.clone(), "pred", vec![])
        {
            return Ok(result);
        }
        Ok(Self::decrement_value(val))
    }

    pub(super) fn exec_post_increment_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx);
        // Handle $CALLER::varname++ — increment through caller scope
        if let Some((bare_name, depth)) = crate::compiler::Compiler::parse_caller_prefix(name) {
            let raw_val = self.interpreter.get_caller_var(&bare_name, depth)?;
            let val = Self::normalize_incdec_source(raw_val);
            let new_val = self.increment_value_smart(&val)?;
            self.interpreter
                .set_caller_var(&bare_name, depth, new_val)?;
            self.stack.push(val);
            return Ok(());
        }
        self.interpreter.check_readonly_for_increment(name)?;
        let raw_val = self
            .get_env_with_main_alias(name)
            .or_else(|| self.anon_state_value(name))
            .unwrap_or(Value::Int(0));
        // If the value is a Proxy, FETCH → increment → STORE
        if let Value::Proxy { storer, .. } = &raw_val
            && !matches!(storer.as_ref(), Value::Nil)
        {
            let fetched = self.interpreter.auto_fetch_proxy(&raw_val)?;
            let val = Self::normalize_incdec_source(fetched);
            let new_val = self.increment_value_smart(&val)?;
            self.interpreter.assign_proxy_lvalue(raw_val, new_val)?;
            self.stack.push(val);
            return Ok(());
        }
        let val = self.normalize_incdec_source_with_type(name, raw_val);
        let new_val = self.increment_value_smart(&val)?;
        let new_val = Self::maybe_wrap_native_int(&self.interpreter, name, new_val);
        self.set_env_with_main_alias(name, new_val.clone());
        self.sync_anon_state_value(name, &new_val);
        self.update_local_if_exists(code, name, &new_val);
        let alias_key = format!("__mutsu_sigilless_alias::{}", name);
        let mut alias_name = self.interpreter.env().get(&alias_key).and_then(|v| {
            if let Value::Str(name) = v {
                Some(name.to_string())
            } else {
                None
            }
        });
        let mut seen_aliases = std::collections::HashSet::new();
        while let Some(current_alias) = alias_name {
            if !seen_aliases.insert(current_alias.clone()) {
                break;
            }
            self.set_env_with_main_alias(&current_alias, new_val.clone());
            self.update_local_if_exists(code, &current_alias, &new_val);
            let next_key = format!("__mutsu_sigilless_alias::{}", current_alias);
            alias_name = self.interpreter.env().get(&next_key).and_then(|v| {
                if let Value::Str(name) = v {
                    Some(name.to_string())
                } else {
                    None
                }
            });
        }
        // Write back to source variable when incrementing $_ bound to a container
        if name == "_"
            && let Some(ref source_var) = self.topic_source_var
            && !source_var.starts_with('@')
            && !source_var.starts_with('%')
        {
            let sv = source_var.clone();
            self.set_env_with_main_alias(&sv, new_val.clone());
            self.update_local_if_exists(code, &sv, &new_val);
        }
        self.stack.push(val);
        Ok(())
    }

    pub(super) fn exec_post_decrement_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx);
        self.interpreter.check_readonly_for_increment(name)?;
        let raw_val = self
            .get_env_with_main_alias(name)
            .or_else(|| self.anon_state_value(name))
            .unwrap_or(Value::Int(0));
        let val = self.normalize_incdec_source_with_type(name, raw_val);
        let new_val = self.decrement_value_smart(&val)?;
        let new_val = Self::maybe_wrap_native_int(&self.interpreter, name, new_val);
        self.set_env_with_main_alias(name, new_val.clone());
        self.sync_anon_state_value(name, &new_val);
        self.update_local_if_exists(code, name, &new_val);
        let alias_key = format!("__mutsu_sigilless_alias::{}", name);
        let mut alias_name = self.interpreter.env().get(&alias_key).and_then(|v| {
            if let Value::Str(name) = v {
                Some(name.to_string())
            } else {
                None
            }
        });
        let mut seen_aliases = std::collections::HashSet::new();
        while let Some(current_alias) = alias_name {
            if !seen_aliases.insert(current_alias.clone()) {
                break;
            }
            self.set_env_with_main_alias(&current_alias, new_val.clone());
            self.update_local_if_exists(code, &current_alias, &new_val);
            let next_key = format!("__mutsu_sigilless_alias::{}", current_alias);
            alias_name = self.interpreter.env().get(&next_key).and_then(|v| {
                if let Value::Str(name) = v {
                    Some(name.to_string())
                } else {
                    None
                }
            });
        }
        self.stack.push(val);
        Ok(())
    }

    pub(super) fn exec_post_increment_index_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        self.exec_inc_dec_index_op(code, name_idx, true, false)
    }

    pub(super) fn exec_post_decrement_index_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        self.exec_inc_dec_index_op(code, name_idx, false, false)
    }

    pub(super) fn exec_pre_increment_index_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        self.exec_inc_dec_index_op(code, name_idx, true, true)
    }

    pub(super) fn exec_pre_decrement_index_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        self.exec_inc_dec_index_op(code, name_idx, false, true)
    }

    fn exec_inc_dec_index_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        increment: bool,
        return_new: bool,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx).to_string();
        let declared_type_incdec = self
            .interpreter
            .env()
            .get(&name)
            .and_then(|v| self.interpreter.container_type_metadata(v))
            .and_then(|info| info.declared_type);
        let _target_is_mixhash_incdec = declared_type_incdec
            .as_deref()
            .is_some_and(|t| t == "MixHash");
        let _target_is_baghash_incdec = declared_type_incdec
            .as_deref()
            .is_some_and(|t| t == "BagHash");
        let _target_is_sethash_incdec = declared_type_incdec
            .as_deref()
            .is_some_and(|t| t == "SetHash");
        let idx_val = self.stack.pop().unwrap_or(Value::Nil);
        let key = idx_val.to_string_value();
        let mut container = self.get_env_with_main_alias(&name);
        let current = if let Some(container_value) = container.as_ref() {
            match container_value {
                Value::Hash(h) => h.get(&key).cloned().unwrap_or(Value::Nil),
                Value::Array(arr, ..) => {
                    if let Ok(i) = key.parse::<usize>() {
                        arr.get(i).cloned().unwrap_or(Value::Nil)
                    } else {
                        Value::Nil
                    }
                }
                Value::Mix(mix, _) => mix.get(&key).map_or(Value::Int(0), |w| {
                    if (*w - (*w as i64 as f64)).abs() < f64::EPSILON {
                        Value::Int(*w as i64)
                    } else {
                        Value::Num(*w)
                    }
                }),
                Value::Set(set, _) => {
                    if set.contains(&key) {
                        Value::Bool(true)
                    } else {
                        Value::Bool(false)
                    }
                }
                Value::Bag(bag, _) => Value::Int(*bag.get(&key).unwrap_or(&0)),
                _ => Value::Nil,
            }
        } else {
            Value::Nil
        };
        let effective = match &current {
            Value::Nil => Value::Int(0),
            other => other.clone(),
        };
        let effective = Self::normalize_incdec_source(effective);
        let new_val = if increment {
            self.increment_value_smart(&effective)?
        } else {
            self.decrement_value_smart(&effective)?
        };
        if let Some(container_value) = container.as_mut() {
            match container_value {
                Value::Hash(h) => {
                    Arc::make_mut(h).insert(key, new_val.clone());
                }
                Value::Array(arr, ..) => {
                    if let Ok(i) = idx_val.to_string_value().parse::<usize>() {
                        let a = Arc::make_mut(arr);
                        while a.len() <= i {
                            a.push(Value::Nil);
                        }
                        a[i] = new_val.clone();
                    }
                }
                Value::Mix(mix, is_mutable) => {
                    if !*is_mutable {
                        return Err(RuntimeError::assignment_ro(Some("Mix")));
                    }
                    let m = Arc::make_mut(mix);
                    if new_val.truthy() {
                        m.insert(key, Self::mix_assignment_weight(&new_val));
                    } else {
                        m.remove(&key);
                    }
                }
                Value::Set(set, is_mutable) => {
                    if !*is_mutable {
                        return Err(RuntimeError::assignment_ro(Some("Set")));
                    }
                    let s = Arc::make_mut(set);
                    if new_val.truthy() {
                        s.insert(key);
                    } else {
                        s.remove(&key);
                    }
                }
                Value::Bag(bag, is_mutable) => {
                    if !*is_mutable {
                        return Err(RuntimeError::assignment_ro(Some("Bag")));
                    }
                    let b = Arc::make_mut(bag);
                    let n = match &new_val {
                        Value::Int(i) => *i,
                        _ => 0,
                    };
                    if n > 0 {
                        b.insert(key, n);
                    } else {
                        b.remove(&key);
                    }
                }
                _ => {}
            }
            self.set_env_with_main_alias(&name, container_value.clone());
            self.update_local_if_exists(code, &name, container_value);
        }
        if return_new {
            self.stack.push(new_val);
        } else {
            self.stack.push(effective);
        }
        Ok(())
    }

    pub(super) fn exec_index_assign_expr_named_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let original_var_name = Self::const_str(code, name_idx).to_string();
        // Resolve sigilless alias: if `h` is a sigilless alias for `%a`,
        // operate on `%a` directly so in-place mutations are visible.
        let sigilless_alias_target = {
            let alias_key = format!("__mutsu_sigilless_alias::{}", original_var_name);
            self.interpreter.env().get(&alias_key).and_then(|v| {
                if let Value::Str(target) = v {
                    Some(target.to_string())
                } else {
                    None
                }
            })
        };
        let var_name = sigilless_alias_target
            .as_deref()
            .unwrap_or(&original_var_name)
            .to_string();
        let declared_type = self
            .interpreter
            .env()
            .get(&var_name)
            .and_then(|v| self.interpreter.container_type_metadata(v))
            .and_then(|info| info.declared_type);
        let _target_is_mixhash = declared_type.as_deref().is_some_and(|t| t == "MixHash");
        let _target_is_baghash = declared_type.as_deref().is_some_and(|t| t == "BagHash");
        let _target_is_sethash = declared_type.as_deref().is_some_and(|t| t == "SetHash");
        let declared_shape_key = format!("__mutsu_shaped_array_dims::{var_name}");
        let has_declared_shape = self.interpreter.env().contains_key(&declared_shape_key);
        let idx = self.stack.pop().unwrap_or(Value::Nil);
        let index_target = self.interpreter.env().get(&var_name).cloned();
        let idx = self.resolve_whatever_index_for_target(idx, index_target.as_ref());
        // Normalize Seq index to Array for uniform handling in assignment
        let idx = if let Value::Seq(items) = idx {
            Value::Array(items, crate::value::ArrayKind::List)
        } else {
            idx
        };
        let raw_val = self.stack.pop().unwrap_or(Value::Nil);
        let (val, bind_mode, bind_sources) = match raw_val {
            Value::Pair(name, payload) if name == "__mutsu_bind_index_value" => match *payload {
                Value::Array(items, ..) if items.len() >= 2 => {
                    let value = items.first().cloned().unwrap_or(Value::Nil);
                    let sources = match items.get(1) {
                        Some(Value::Array(srcs, ..)) => srcs
                            .iter()
                            .map(|src| match src {
                                Value::Str(s) if !s.is_empty() => Some((**s).clone()),
                                _ => None,
                            })
                            .collect(),
                        _ => Vec::new(),
                    };
                    (value, true, sources)
                }
                other => (other, true, Vec::new()),
            },
            other => (other, false, Vec::new()),
        };
        // For typed container elements, explicit `is default(...)` wins over
        // the nominal type-object fallback when Nil is assigned.
        let val = if matches!(val, Value::Nil) {
            if let Some(default) = self.interpreter.var_default(&var_name) {
                default.clone()
            } else if let Some(constraint) = self.interpreter.var_type_constraint(&var_name) {
                let nominal = self
                    .interpreter
                    .nominal_type_object_name_for_constraint(&constraint);
                Value::Package(Symbol::intern(&nominal))
            } else {
                val
            }
        } else {
            val
        };
        // Map containers are immutable - prevent assignment and binding to keys
        if declared_type.as_deref().is_some_and(|t| t == "Map") {
            if bind_mode {
                return Err(RuntimeError::typed(
                    "X::Bind",
                    [("target".to_string(), Value::str(var_name.clone()))]
                        .into_iter()
                        .collect(),
                ));
            } else {
                return Err(RuntimeError::new(
                    "Cannot modify an immutable Map (Map)".to_string(),
                ));
            }
        }
        let encoded_idx = Self::encode_bound_index(&idx);
        let is_bound_index = if bind_mode {
            self.is_bound_index(&var_name, &encoded_idx)
        } else {
            false
        };
        if !bind_mode && self.is_bound_index(&var_name, &encoded_idx) {
            // Check if the bound element still has a BOUND_ARRAY_REF_SENTINEL.
            // If yes, allow writes (they propagate to the source variable).
            // If no (sentinel gone due to array reset/delete), the metadata is
            // stale — clean it up and allow the write.
            let sentinel_status = if let Some(Value::Array(items, ..)) =
                self.interpreter.env().get(&var_name)
                && let Some(i) = Self::index_to_usize(&idx)
            {
                match items.get(i) {
                    Some(Value::Pair(marker, _))
                        if marker == super::vm_var_ops::BOUND_ARRAY_REF_SENTINEL =>
                    {
                        Some(true) // sentinel present
                    }
                    Some(_) => Some(false), // index exists but no sentinel
                    None => None,           // index out of bounds (array was reset/shrunk)
                }
            } else {
                None // array doesn't exist or index not a usize
            };
            match sentinel_status {
                Some(true) => {
                    // Sentinel present — write will propagate, allow it
                }
                Some(false) => {
                    // No sentinel but bound metadata exists — this is a
                    // non-bound readonly index, reject the write.
                    return Err(RuntimeError::assignment_ro(None));
                }
                None => {
                    // Array was reset or index is gone — stale metadata, allow write
                }
            }
        }
        if !self.interpreter.env().contains_key(&var_name)
            && let Some(slot) = self.find_local_slot(code, &var_name)
        {
            self.set_env_with_main_alias(&var_name, self.locals[slot].clone());
        }
        match &idx {
            Value::Array(keys, ..) => {
                let mut vals = self.assignment_rhs_values(&val)?;
                if let Some(container) = self.interpreter.env_mut().get_mut(&var_name)
                    && matches!(container, Value::Array(..))
                {
                    let is_shaped =
                        has_declared_shape || crate::runtime::utils::is_shaped_array(container);
                    let mut initialized_marks: Vec<String> = Vec::new();
                    if is_shaped {
                        if bind_mode && is_bound_index {
                            return Err(RuntimeError::assignment_ro(None));
                        }
                        let depth = Self::array_depth(container);
                        if depth <= 1 && keys.len() > 1 {
                            // 1D shaped array with multiple indices: slice assignment
                            for (i, key) in keys.iter().enumerate() {
                                let v = vals.get(i).cloned().unwrap_or(Value::Nil);
                                Self::assign_array_multidim(
                                    container,
                                    std::slice::from_ref(key),
                                    v,
                                )?;
                                initialized_marks.push(Self::encode_bound_index(key));
                            }
                        } else {
                            // Multidimensional indexing: @arr[0;0] = 'x'
                            Self::assign_array_multidim(container, keys.as_ref(), val.clone())?;
                            initialized_marks.push(encoded_idx.clone());
                        }
                    } else if keys.is_empty() {
                        // Empty slice assignment (e.g. @n[*] on empty array): no-op
                    } else {
                        // Flat slice assignment: @a[2,3,4,6] = <foo bar foo bar>
                        // Auto-extend the array to accommodate all indices
                        let max_idx = keys
                            .iter()
                            .filter_map(Self::index_to_usize)
                            .max()
                            .unwrap_or(0);
                        if let Value::Array(items, ..) = container {
                            let arr = Arc::make_mut(items);
                            if max_idx >= arr.len() {
                                arr.resize(max_idx + 1, Value::Package(Symbol::intern("Any")));
                            }
                        }
                        // Assign each value to the corresponding index
                        for (i, key) in keys.iter().enumerate() {
                            let v = vals.get(i).cloned().unwrap_or(Value::Nil);
                            Self::assign_array_multidim(container, std::slice::from_ref(key), v)?;
                            initialized_marks.push(Self::encode_bound_index(key));
                        }
                    }
                    for encoded in initialized_marks {
                        self.mark_initialized_index(&var_name, encoded);
                    }
                    if bind_mode {
                        self.mark_bound_index(&var_name, encoded_idx);
                    }
                    self.stack.push(val);
                    return Ok(());
                }
                if vals.is_empty() {
                    vals.push(Value::Nil);
                }
                // Check value type constraint for hash slice assignment
                if let Some(constraint) = self.interpreter.var_type_constraint(&var_name) {
                    for v in &vals {
                        if !matches!(v, Value::Nil)
                            && !self.interpreter.type_matches_value(&constraint, v)
                        {
                            return Err(RuntimeError::new(
                                runtime::utils::type_check_element_error(&var_name, &constraint, v),
                            ));
                        }
                    }
                }
                // Check key type constraint for hash slice assignment
                if let Some(key_constraint) = self.interpreter.var_hash_key_constraint(&var_name) {
                    for key in keys.iter() {
                        if !self.interpreter.type_matches_value(&key_constraint, key) {
                            return Err(RuntimeError::new(
                                runtime::utils::type_check_element_error(
                                    &var_name,
                                    &key_constraint,
                                    key,
                                ),
                            ));
                        }
                    }
                }
                if !matches!(self.interpreter.env().get(&var_name), Some(Value::Hash(_))) {
                    self.interpreter.env_mut().insert(
                        var_name.clone(),
                        Value::hash(std::collections::HashMap::new()),
                    );
                }
                let mut pending_source_updates: Vec<(String, Value)> = Vec::new();
                if let Some(Value::Hash(hash)) = self.interpreter.env_mut().get_mut(&var_name) {
                    let h = Arc::make_mut(hash);
                    for (i, key) in keys.iter().enumerate() {
                        let k = key.to_string_value();
                        let v = if bind_mode {
                            vals.get(i).cloned().unwrap_or(Value::Nil)
                        } else {
                            vals[i % vals.len()].clone()
                        };
                        if bind_mode && let Some(Some(source_name)) = bind_sources.get(i) {
                            pending_source_updates.push((source_name.clone(), v.clone()));
                            h.insert(
                                k,
                                Value::Pair(
                                    super::vm_var_ops::BOUND_HASH_REF_SENTINEL.to_string(),
                                    Box::new(Value::str(source_name.clone())),
                                ),
                            );
                        } else {
                            h.insert(k, v);
                        }
                    }
                }
                for (source_name, source_value) in pending_source_updates {
                    self.interpreter.env_mut().insert(source_name, source_value);
                }
            }
            _ => {
                // Check if the target is an array variable — use numeric index assignment
                let key = idx.to_string_value();
                let array_elem_constraint = self.interpreter.var_type_constraint(&var_name);
                if let Some(constraint) = array_elem_constraint
                    && !matches!(val, Value::Nil)
                    && !self.interpreter.type_matches_value(&constraint, &val)
                    // Container type constraints (Hash, Array, etc.) apply to the
                    // whole container, not individual elements. Skip element-level
                    // type checking for these types.
                    && !matches!(
                        constraint.as_str(),
                        "Hash" | "Array" | "Map" | "List" | "Bag" | "Set" | "Mix"
                            | "BagHash" | "SetHash" | "MixHash" | "Seq"
                    )
                {
                    return Err(RuntimeError::new(runtime::utils::type_check_element_error(
                        &var_name,
                        &constraint,
                        &val,
                    )));
                }
                // Check key type constraint for single-key hash element assignment
                if var_name.starts_with('%')
                    && let Some(key_constraint) =
                        self.interpreter.var_hash_key_constraint(&var_name)
                    && !self.interpreter.type_matches_value(&key_constraint, &idx)
                {
                    return Err(RuntimeError::new(runtime::utils::type_check_element_error(
                        &var_name,
                        &key_constraint,
                        &idx,
                    )));
                }
                // Resolve GenericRange with WhateverCode endpoints (e.g. @a[*-4 .. *-1] = ...)
                let resolved_idx;
                let idx_for_slice = if let Value::GenericRange { .. } = &idx {
                    let array_len = if let Some(Value::Array(items, ..)) =
                        self.interpreter.env().get(&var_name)
                    {
                        items.len()
                    } else {
                        0
                    };
                    resolved_idx = self.resolve_generic_range_for_assign(&idx, array_len);
                    resolved_idx.as_ref().unwrap_or(&idx)
                } else {
                    &idx
                };
                let range_slice =
                    if let Some(indices) = Self::slice_indices_from_index(idx_for_slice) {
                        Some((indices, self.assignment_rhs_values(&val)?))
                    } else {
                        None
                    };
                if let Some(current) = self.interpreter.env().get(&var_name).cloned()
                    && let Value::Mixin(inner, mixins) = current
                {
                    let mut updated_mixins = (*mixins).clone();
                    let mut assigned_object_slot = false;
                    let delegated_attr_key = if matches!(&idx, Value::Str(_)) {
                        self.delegated_mixin_attr_key(&updated_mixins, "ASSIGN-KEY")
                    } else {
                        self.delegated_mixin_attr_key(&updated_mixins, "ASSIGN-POS")
                    };
                    if let Some(attr_key) = delegated_attr_key
                        && let Some(attr_value) = updated_mixins.get_mut(&attr_key)
                    {
                        assigned_object_slot =
                            Self::assign_mixin_container_slot(attr_value, &idx, &val, &range_slice);
                    }
                    if !assigned_object_slot {
                        for (key, attr_value) in updated_mixins.iter_mut() {
                            if !key.starts_with("__mutsu_attr__") {
                                continue;
                            }
                            if Self::assign_mixin_container_slot(
                                attr_value,
                                &idx,
                                &val,
                                &range_slice,
                            ) {
                                assigned_object_slot = true;
                                break;
                            }
                        }
                    }
                    if assigned_object_slot {
                        self.interpreter.env_mut().insert(
                            var_name.clone(),
                            Value::Mixin(inner, Arc::new(updated_mixins)),
                        );
                        self.stack.push(val);
                        return Ok(());
                    }
                }
                let mut range_initialized_marks: Vec<String> = Vec::new();
                let mut pending_source_update: Option<(String, Value)> = None;
                let mut pending_varref_update: Option<(String, Option<usize>, Value)> = None;
                // Pre-compute whether this %-sigiled variable was bound via `:=`.
                // Bound hash variables are marked readonly, so we use that as a signal
                // to allow in-place mutation (preserving shared identity).
                let is_bound_hash_var = var_name.starts_with('%')
                    && self.interpreter.readonly_vars().contains(&var_name);
                if let Some(container) = self.interpreter.env_mut().get_mut(&var_name) {
                    match *container {
                        Value::Hash(ref mut hash) => {
                            let is_self_hash_ref = matches!(
                                &val,
                                Value::Hash(source_hash) if Arc::ptr_eq(hash, source_hash)
                            );
                            // Use in-place mutation instead of Arc::make_mut when the
                            // hash is shared (strong_count > 1) AND the variable is a
                            // scalar ($) container.  This preserves Raku's object
                            // identity semantics: when a hash is stored in an array
                            // slot via `$arr[i] = $hash`, mutations through the original
                            // variable must be visible through the array.
                            // SAFETY: mutsu is single-threaded, so exclusive access is
                            // guaranteed even though the Arc is shared.
                            // For %-sigiled hash variables (e.g. `%h is copy`),
                            // normally use COW.  For scalar ($) variables holding
                            // hashes, use in-place mutation to preserve identity.
                            // Exception: when a %-sigiled variable is bound via
                            // `:=` (marked readonly), use in-place mutation so
                            // modifications propagate to the bound source.
                            // %-sigiled vars have names like "%h", scalar vars
                            // have names without a sigil prefix (e.g. "bar").
                            let use_inplace = Arc::strong_count(hash) > 1
                                && (!var_name.starts_with('%') || is_bound_hash_var);
                            let h: &mut std::collections::HashMap<String, Value> = if use_inplace {
                                unsafe { &mut *(Arc::as_ptr(hash) as *mut _) }
                            } else {
                                Arc::make_mut(hash)
                            };
                            if bind_mode && let Some(Some(source_name)) = bind_sources.first() {
                                pending_source_update = Some((source_name.clone(), val.clone()));
                                h.insert(
                                    key.clone(),
                                    Value::Pair(
                                        super::vm_var_ops::BOUND_HASH_REF_SENTINEL.to_string(),
                                        Box::new(Value::str(source_name.clone())),
                                    ),
                                );
                            } else if let Some(Value::Pair(marker, source_name)) = h.get(&key)
                                && marker == super::vm_var_ops::BOUND_HASH_REF_SENTINEL
                            {
                                let source_name = source_name.to_string_value();
                                pending_source_update = Some((source_name, val.clone()));
                            } else if is_self_hash_ref {
                                h.insert(key.clone(), Self::self_hash_ref_marker());
                            } else {
                                h.insert(key.clone(), val.clone());
                            }
                        }
                        Value::Array(..) => {
                            if has_declared_shape
                                || crate::runtime::utils::is_shaped_array(container)
                            {
                                if bind_mode && is_bound_index {
                                    return Err(RuntimeError::assignment_ro(None));
                                }
                                Self::assign_array_multidim(
                                    container,
                                    std::slice::from_ref(&idx),
                                    val.clone(),
                                )?;
                            } else if let Some((slice_indices, vals)) = &range_slice {
                                if let Value::Array(items, ..) = container {
                                    let arr = Arc::make_mut(items);
                                    if let Some(max_idx) = slice_indices.last().copied()
                                        && max_idx >= arr.len()
                                    {
                                        arr.resize(
                                            max_idx + 1,
                                            Value::Package(Symbol::intern("Any")),
                                        );
                                    }
                                }
                                for (offset, i) in slice_indices.iter().enumerate() {
                                    let key = Value::Int(*i as i64);
                                    let v = vals.get(offset).cloned().unwrap_or(Value::Nil);
                                    Self::assign_array_multidim(
                                        container,
                                        std::slice::from_ref(&key),
                                        v,
                                    )?;
                                    range_initialized_marks.push(Self::encode_bound_index(&key));
                                }
                            } else if let Some(i) = Self::index_to_usize(&idx) {
                                if let Value::Array(items, ..) = container {
                                    let is_self_array_ref = matches!(
                                        &val,
                                        Value::Array(source_items, ..) if Arc::ptr_eq(items, source_items)
                                    );
                                    let arr = Arc::make_mut(items);
                                    if i >= arr.len() {
                                        arr.resize(i + 1, Value::Package(Symbol::intern("Any")));
                                    }
                                    if bind_mode
                                        && let Some(Some(source_name)) = bind_sources.first()
                                    {
                                        // Store a sentinel so reads follow the
                                        // source variable (two-way binding).
                                        arr[i] = Value::Pair(
                                            super::vm_var_ops::BOUND_ARRAY_REF_SENTINEL.to_string(),
                                            Box::new(Value::str(source_name.clone())),
                                        );
                                    } else if let Some(Value::Pair(marker, source)) = arr.get(i)
                                        && marker == super::vm_var_ops::BOUND_ARRAY_REF_SENTINEL
                                    {
                                        // Writing to a bound element propagates
                                        // to the source variable.
                                        let source_name = source.to_string_value();
                                        pending_varref_update =
                                            Some((source_name, None, val.clone()));
                                    } else if let Some((source_name, source_index)) =
                                        Self::varref_target(&arr[i])
                                    {
                                        pending_varref_update =
                                            Some((source_name.clone(), source_index, val.clone()));
                                        arr[i] = Self::make_varref_value(
                                            source_name,
                                            val.clone(),
                                            source_index,
                                        );
                                    } else {
                                        arr[i] = if is_self_array_ref {
                                            Self::self_array_ref_marker()
                                        } else {
                                            val.clone()
                                        };
                                    }
                                }
                            } else if let Value::Int(i) = &idx
                                && *i < 0
                            {
                                // Negative index from WhateverCode resolution
                                // (e.g. @arr[*-1] = 42 on empty array)
                                return Err(Self::make_out_of_range_error(*i));
                            } else {
                                return Err(RuntimeError::new("Index out of bounds"));
                            }
                            self.mark_initialized_index(&var_name, encoded_idx.clone());
                            if bind_mode {
                                self.mark_bound_index(&var_name, encoded_idx.clone());
                            }
                        }
                        Value::Set(ref mut set, is_mutable) => {
                            if !is_mutable {
                                return Err(RuntimeError::assignment_ro(Some("Set")));
                            }
                            let s = Arc::make_mut(set);
                            if val.truthy() {
                                s.insert(key.clone());
                            } else {
                                s.remove(&key);
                            }
                        }
                        Value::Bag(ref mut bag, is_mutable) => {
                            if !is_mutable {
                                return Err(RuntimeError::assignment_ro(Some("Bag")));
                            }
                            let b = Arc::make_mut(bag);
                            let count = match &val {
                                Value::Int(i) => *i,
                                Value::Num(n) => *n as i64,
                                Value::Bool(flag) => i64::from(*flag),
                                _ => {
                                    if val.truthy() {
                                        1
                                    } else {
                                        0
                                    }
                                }
                            };
                            if count == 0 {
                                b.remove(&key);
                            } else {
                                b.insert(key.clone(), count);
                            }
                        }
                        Value::Mix(ref mut mix, is_mutable) => {
                            if !is_mutable {
                                return Err(RuntimeError::assignment_ro(Some("Mix")));
                            }
                            let m = Arc::make_mut(mix);
                            let weight = Self::mix_assignment_weight(&val);
                            if weight == 0.0 {
                                m.remove(&key);
                            } else {
                                m.insert(key.clone(), weight);
                            }
                        }
                        _ => {
                            let mut hash = std::collections::HashMap::new();
                            hash.insert(key.clone(), val.clone());
                            *container = Value::hash(hash);
                        }
                    }
                } else {
                    let mut hash = std::collections::HashMap::new();
                    hash.insert(key.clone(), val.clone());
                    self.interpreter
                        .env_mut()
                        .insert(var_name.clone(), Value::hash(hash));
                }
                if let Some((source_name, source_value)) = pending_source_update {
                    self.interpreter.env_mut().insert(source_name, source_value);
                }
                if let Some((source_name, source_index, source_value)) = pending_varref_update {
                    self.assign_varref_target(&source_name, source_index, source_value)?;
                }
                for encoded in range_initialized_marks {
                    self.mark_initialized_index(&var_name, encoded);
                }
                // Sync OS environment when %*ENV is modified
                #[cfg(not(target_family = "wasm"))]
                if var_name == "%*ENV" {
                    // SAFETY: mutsu is single-threaded
                    unsafe {
                        std::env::set_var(&key, val.to_string_value());
                    }
                }
                // Sync $*HOME when %*ENV<HOME> changes
                if var_name == "%*ENV" && key == "HOME" {
                    let home_str = val.to_string_value();
                    let home_val = self.interpreter.make_io_path_instance(&home_str);
                    self.interpreter
                        .env_mut()
                        .insert("$*HOME".to_string(), home_val.clone());
                    self.interpreter
                        .env_mut()
                        .insert("*HOME".to_string(), home_val);
                }
            }
        }
        if let Some(updated) = self.get_env_with_main_alias(&var_name) {
            self.update_local_if_exists(code, &var_name, &updated);
            if var_name == "_"
                && let Some(ref source_var) = self.topic_source_var
            {
                let source_name = source_var.clone();
                self.set_env_with_main_alias(&source_name, updated.clone());
                self.update_local_if_exists(code, &source_name, &updated);
            }
            // Re-register container default for `is default(...)` after mutation,
            // since Arc::make_mut may have changed the pointer identity.
            if let Some(def) = self.interpreter.var_default(&var_name).cloned() {
                self.interpreter.set_container_default(&updated, def);
            }
        }
        // When operating through a sigilless alias (e.g., `h` → `%a`),
        // sync the modified container back to the alias variable so reads
        // of the sigilless variable see the updated value.
        if let Some(ref alias_target) = sigilless_alias_target
            && let Some(updated_container) = self.interpreter.env().get(alias_target).cloned()
        {
            self.interpreter
                .env_mut()
                .insert(original_var_name.clone(), updated_container.clone());
            self.update_local_if_exists(code, &original_var_name, &updated_container);
        }
        self.stack.push(val);
        Ok(())
    }

    pub(super) fn exec_index_assign_pseudo_stash_named_op(
        &mut self,
        code: &CompiledCode,
        stash_name_idx: u32,
        key_name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let stash_name = Self::const_str(code, stash_name_idx);
        if stash_name != "MY::" {
            return Err(RuntimeError::new(format!(
                "Unsupported pseudo-stash assignment target {stash_name}"
            )));
        }

        let raw_key = Self::const_str(code, key_name_idx);
        let resolved_name = if let Some(name) = raw_key.strip_prefix('$') {
            name.to_string()
        } else {
            raw_key.to_string()
        };

        let val = self.stack.pop().unwrap_or(Value::Nil);
        if let Some(slot) = self.find_local_slot(code, &resolved_name) {
            self.stack.push(val);
            self.exec_assign_expr_local_op(code, slot as u32)
        } else {
            let mut val = if resolved_name.starts_with('@') {
                runtime::coerce_to_array(val)
            } else if resolved_name.starts_with('%') {
                self.coerce_hash_var_value(&resolved_name, val)?
            } else {
                Self::normalize_scalar_assignment_value(val)
            };

            self.interpreter.check_readonly_for_modify(&resolved_name)?;
            if let Some(default) = self.interpreter.var_default(&resolved_name)
                && matches!(val, Value::Nil)
            {
                val = default.clone();
            }
            if resolved_name.starts_with('@') || resolved_name.starts_with('%') {
                val = self.coerce_typed_container_assignment(&resolved_name, val, false)?;
            }
            if let Some(constraint) = self.interpreter.var_type_constraint(&resolved_name)
                && !resolved_name.starts_with('@')
                && !resolved_name.starts_with('%')
            {
                if matches!(val, Value::Nil) {
                    if constraint != "Mu" {
                        let nominal = self
                            .interpreter
                            .nominal_type_object_name_for_constraint(&constraint);
                        val = Value::Package(Symbol::intern(&nominal));
                    }
                } else if !self.interpreter.type_matches_value(&constraint, &val) {
                    return Err(RuntimeError::new(
                        runtime::utils::type_check_assignment_error(
                            &resolved_name,
                            &constraint,
                            &val,
                        ),
                    ));
                }
                if !matches!(val, Value::Nil | Value::Package(_)) {
                    val = self
                        .interpreter
                        .try_coerce_value_for_constraint(&constraint, val)?;
                }
            }

            self.set_env_with_main_alias(&resolved_name, val.clone());
            self.stack.push(val);
            Ok(())
        }
    }

    pub(super) fn exec_index_assign_expr_nested_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let var_name = Self::const_str(code, name_idx).to_string();
        let inner_idx = self.stack.pop().unwrap_or(Value::Nil);
        let outer_idx = self.stack.pop().unwrap_or(Value::Nil);
        let val = self.stack.pop().unwrap_or(Value::Nil);
        let inner_key = inner_idx.to_string_value();
        let outer_key = outer_idx.to_string_value();

        // If the outer hash has a value type constraint, nested autovivification
        // would need to create a Hash value which won't match the constraint
        // (e.g. `my Int %h; %h<z><t> = 3` should die because %h<z> can't be a Hash).
        if var_name.starts_with('%')
            && let Some(constraint) = self.interpreter.var_type_constraint(&var_name)
        {
            let inner_hash = Value::hash(std::collections::HashMap::new());
            if !self
                .interpreter
                .type_matches_value(&constraint, &inner_hash)
            {
                return Err(RuntimeError::new(format!(
                    "Type check failed in assignment to {var_name}; expected {constraint} but got Hash (autovivification)"
                )));
            }
        }

        if !self.interpreter.env().contains_key(&var_name) {
            self.interpreter.env_mut().insert(
                var_name.clone(),
                Value::hash(std::collections::HashMap::new()),
            );
        }

        // Handle Array-in-Array: $a[outer][inner] = val
        if let Some(Value::Array(outer_arr, _kind)) = self.interpreter.env_mut().get_mut(&var_name)
            && let Ok(outer_i) = outer_key.parse::<usize>()
        {
            let arr = Arc::make_mut(outer_arr);
            if outer_i < arr.len()
                && let Value::Array(ref mut inner_arr, _) = arr[outer_i]
                && let Ok(inner_i) = inner_key.parse::<usize>()
            {
                let inner = Arc::make_mut(inner_arr);
                if inner_i < inner.len() {
                    inner[inner_i] = val.clone();
                } else {
                    inner.resize(inner_i + 1, Value::Nil);
                    inner[inner_i] = val.clone();
                }
                if let Some(updated) = self.get_env_with_main_alias(&var_name) {
                    self.update_local_if_exists(code, &var_name, &updated);
                }
                self.stack.push(val);
                return Ok(());
            }
        }

        // Hash-in-Hash auto-vivification (original behavior)
        // Drop the locals copy first so the Arc refcount is 1.
        // This avoids unnecessary cloning in Arc::make_mut which would
        // change the pointer and break .WHICH identity stability.
        if let Some(slot) = self.find_local_slot(code, &var_name) {
            self.locals[slot] = Value::Nil;
        }
        if let Some(Value::Hash(outer_hash)) = self.interpreter.env_mut().get_mut(&var_name) {
            let oh = Arc::make_mut(outer_hash);
            let inner_hash = oh
                .entry(inner_key)
                .or_insert_with(|| Value::hash(std::collections::HashMap::new()));
            if let Value::Hash(ref mut h) = *inner_hash {
                Arc::make_mut(h).insert(outer_key, val.clone());
            }
        }
        if let Some(updated) = self.get_env_with_main_alias(&var_name) {
            self.update_local_if_exists(code, &var_name, &updated);
        }
        self.stack.push(val);
        Ok(())
    }

    /// Generic index assignment on a stack-computed target.
    /// Stack order: target (bottom), index, value (top).
    /// If the target hash has `__callframe_depth`, routes through set_caller_var.
    pub(super) fn exec_index_assign_generic_op(&mut self) -> Result<(), RuntimeError> {
        let val = self.stack.pop().unwrap_or(Value::Nil);
        let idx = self.stack.pop().unwrap_or(Value::Nil);
        let target = self.stack.pop().unwrap_or(Value::Nil);
        let key = idx.to_string_value();

        match &target {
            Value::Hash(h) => {
                // Check for callframe .my hash with depth marker
                if let Some(Value::Int(depth)) = h.get("__callframe_depth") {
                    let depth = *depth as usize;
                    // Strip sigil from key to get bare variable name
                    let bare_name =
                        if key.starts_with('$') || key.starts_with('@') || key.starts_with('%') {
                            &key[1..]
                        } else {
                            &key
                        };
                    self.interpreter
                        .set_caller_var(bare_name, depth, val.clone())?;
                    self.stack.push(val);
                    return Ok(());
                }
                // Regular hash: just do the assignment (on a temporary — won't persist)
                self.stack.push(val);
            }
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Stash" => {
                let package = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                if !package.is_empty() {
                    let key_name = if key.starts_with('$')
                        || key.starts_with('@')
                        || key.starts_with('%')
                        || key.starts_with('&')
                    {
                        key
                    } else {
                        format!("${key}")
                    };
                    let pkg = package.trim_end_matches("::");
                    let fq = if pkg.is_empty() || pkg == "GLOBAL" {
                        key_name
                    } else {
                        format!("{pkg}::{key_name}")
                    };
                    self.interpreter.env_mut().insert(fq, val.clone());
                }
                self.stack.push(val);
            }
            _ => {
                self.stack.push(val);
            }
        }
        Ok(())
    }

    pub(super) fn exec_get_local_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        self.ensure_locals_synced(code);
        let idx = idx as usize;
        // Check if this variable has a binding alias (e.g. from $CALLER::foo := $other_var)
        let name = code.locals.get(idx).cloned().unwrap_or_default();
        if let Some(bound_to) = self.interpreter.resolve_binding(&name) {
            let bound_to = bound_to.to_string();
            if let Some(val) = self.interpreter.env().get(&bound_to).cloned() {
                self.stack.push(val);
                return Ok(());
            }
        }
        let atomic_name = name.strip_prefix('$').unwrap_or(&name);
        let atomic_name_key = format!("__mutsu_atomic_name::{atomic_name}");
        // Only use the scalar atomic fast path for scalar ($) variables.
        // Array (@) variables with `atomicint` constraint are element-wise
        // atomic and should go through the normal array read path.
        let is_atomic_int = !name.starts_with('@')
            && (self.interpreter.var_type_constraint(&name).as_deref() == Some("atomicint")
                || self.interpreter.var_type_constraint(atomic_name).as_deref()
                    == Some("atomicint")
                || self.interpreter.get_shared_var(&atomic_name_key).is_some());
        if is_atomic_int {
            let fetched = self
                .interpreter
                .builtin_atomic_fetch_var(&[Value::str(atomic_name.to_string())])?;
            self.locals[idx] = fetched.clone();
            self.stack.push(fetched);
            return Ok(());
        }
        // Atomic array CAS stores the authoritative array under an internal
        // shared key.  Check it first so reads pick up the latest CAS'd value.
        if name.starts_with('@') {
            let atomic_key = format!("__mutsu_atomic_arr::{name}");
            if let Some(shared_val) = self.interpreter.get_shared_var(&atomic_key) {
                self.locals[idx] = shared_val.clone();
                self.stack.push(shared_val);
                return Ok(());
            }
        }
        // Shared @/% variables may be mutated by sibling threads while this VM
        // still holds an old local snapshot. Prefer the shared copy so reads
        // observe the latest value without forcing array COW on every push.
        if (name.starts_with('@') || name.starts_with('%'))
            && let Some(shared_val) = self.interpreter.get_shared_var(&name)
        {
            self.stack.push(shared_val);
            return Ok(());
        }
        let val = self.locals[idx].clone();
        // Force lazy thunks transparently on access
        if let Value::LazyThunk(ref thunk_data) = val {
            let forced = self.force_lazy_thunk(thunk_data)?;
            self.stack.push(forced);
            return Ok(());
        }
        // Fast path: non-Nil values are always valid — skip env lookup
        if matches!(val, Value::Nil) {
            if let Some(shared_val) = self.interpreter.get_shared_var(&name) {
                self.stack.push(shared_val);
                return Ok(());
            }
            let is_internal = name.starts_with("__");
            let is_special = matches!(name.as_str(), "_" | "/" | "!" | "¢");
            if !is_internal && !is_special && !self.interpreter.env().contains_key(&name) {
                return Err(RuntimeError::new(format!(
                    "X::Undeclared::Symbols: Variable '{name}' is not declared"
                )));
            }
            // `is default(...)`: return the default value instead of Nil.
            if let Some(def) = self.interpreter.var_default(&name) {
                self.stack.push(def.clone());
                return Ok(());
            }
            if let Some(constraint) = self.interpreter.var_type_constraint_fast(&name).cloned() {
                let nominal = self
                    .interpreter
                    .nominal_type_object_name_for_constraint(&constraint);
                // Nil type constraint: the type object for Nil is Value::Nil itself,
                // not Value::Package("Nil").
                if nominal == "Nil" {
                    self.stack.push(Value::Nil);
                } else {
                    self.stack.push(Value::Package(Symbol::intern(&nominal)));
                }
                return Ok(());
            }
        }
        self.stack.push(val);
        Ok(())
    }

    pub(super) fn exec_set_local_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let idx = idx as usize;
        let raw_popped = self.stack.pop().unwrap_or(Value::Nil);
        let (raw_popped, bind_source) = Self::extract_varref_binding(raw_popped);
        let is_bind = self.bind_context || bind_source.is_some();
        let is_constant = self.constant_context;
        let has_explicit_initializer = self.explicit_initializer_context;
        self.bind_context = false;
        self.constant_context = false;
        self.explicit_initializer_context = false;

        // Fast path for simple scalar variables — skip all metadata checks
        if code.simple_locals[idx] && bind_source.is_none() && !is_bind {
            let mut val = raw_popped;
            let name = &code.locals[idx];
            // If the current value is a Proxy, invoke STORE instead of overwriting
            if let Value::Proxy { storer, .. } = &self.locals[idx]
                && !matches!(storer.as_ref(), Value::Nil)
            {
                let proxy_val = self.locals[idx].clone();
                self.interpreter.assign_proxy_lvalue(proxy_val, val)?;
                return Ok(());
            }
            // If the current value is a HashSlotRef, write back to the parent hash
            if let Value::HashSlotRef { .. } = &self.locals[idx] {
                self.locals[idx].hash_slot_write(val);
                self.locals_dirty = true;
                return Ok(());
            }
            if !name.starts_with('@') && !name.starts_with('%') {
                val = Self::normalize_scalar_assignment_value(val);
            }
            if matches!(val, Value::Nil)
                && let Some(def) = self.interpreter.var_default(name)
            {
                val = def.clone();
            }
            if let Some(constraint) = self.interpreter.var_type_constraint_fast(name).cloned() {
                if matches!(val, Value::Nil) && self.interpreter.is_definite_constraint(&constraint)
                {
                    if has_explicit_initializer {
                        return Err(RuntimeError::new(
                            runtime::utils::type_check_assignment_error(name, &constraint, &val),
                        ));
                    }
                    return Err(RuntimeError::new(format!(
                        "X::Syntax::Variable::MissingInitializer: Variable definition of type {} needs to be given an initializer",
                        constraint
                    )));
                }
                if !matches!(val, Value::Nil)
                    && !self.interpreter.type_matches_value(&constraint, &val)
                {
                    return Err(RuntimeError::new(
                        runtime::utils::type_check_assignment_error(name, &constraint, &val),
                    ));
                }
                let val = if !matches!(val, Value::Nil) {
                    self.interpreter
                        .try_coerce_value_for_constraint(&constraint, val)?
                } else {
                    val
                };
                // Wrap native integer values on assignment (overflow wrapping)
                let val = Self::wrap_native_int_by_constraint(&constraint, val)?;
                self.locals[idx] = val;
            } else {
                self.locals[idx] = val;
            }
            // Track lazy-thunk readonly: mark when storing a LazyThunk,
            // unmark when overwriting a LazyThunk with a non-LazyThunk (rebinding).
            if matches!(self.locals[idx], Value::LazyThunk(..)) {
                self.interpreter.mark_readonly(name);
            }
            if self.interpreter.fatal_mode
                && !name.contains("__mutsu_")
                && let Some(err) = self
                    .interpreter
                    .failure_to_runtime_error_if_unhandled(&self.locals[idx])
            {
                return Err(err);
            }
            // Also update env (and shared_vars if active) immediately so stale
            // references don't keep old Instance values alive (preventing DESTROY
            // from firing).
            self.interpreter
                .set_shared_var(name, self.locals[idx].clone());
            // Track topic mutations for map rw writeback
            if name == "_" {
                self.interpreter.env_mut().insert(
                    "__mutsu_rw_map_topic__".to_string(),
                    self.locals[idx].clone(),
                );
            }
            self.locals_dirty = true;
            return Ok(());
        }

        let name = &code.locals[idx];
        // Capture the old hash Arc before assignment for circular reference fixup.
        let old_hash_arc = if name.starts_with('%') {
            if let Value::Hash(arc) = &self.locals[idx] {
                Some(Arc::as_ptr(arc) as usize)
            } else {
                None
            }
        } else {
            None
        };
        let mut val = if name.starts_with('%') {
            if has_explicit_initializer
                && !is_constant
                && !is_bind
                && matches!(raw_popped, Value::Nil)
                && let Some(constraint) = self.interpreter.var_type_constraint(name)
            {
                return Err(RuntimeError::new(
                    runtime::utils::type_check_assignment_error(name, &constraint, &Value::Nil),
                ));
            }
            if is_constant || is_bind {
                // `:=` binding or `constant %x` preserves containers — skip coercion.
                // `constant %x = :42foo` keeps the Pair; `constant %x = bag(...)` keeps the Bag.
                raw_popped
            } else {
                self.coerce_hash_var_value(name, raw_popped)?
            }
        } else if name.starts_with('@') {
            if has_explicit_initializer
                && !is_constant
                && !is_bind
                && matches!(raw_popped, Value::Nil)
                && let Some(constraint) = self.interpreter.var_type_constraint(name)
            {
                return Err(RuntimeError::new(
                    runtime::utils::type_check_assignment_error(name, &constraint, &Value::Nil),
                ));
            }
            let mut assigned = if is_constant {
                // `constant @x` stores a List, not an Array.
                // Explicit Arrays ([1,2,3]) are preserved.
                match raw_popped {
                    Value::Array(items, kind) if kind.is_real_array() => Value::Array(items, kind),
                    Value::Array(items, _) => Value::Array(items, crate::value::ArrayKind::List),
                    Value::Seq(items) => Value::Array(
                        std::sync::Arc::new(items.to_vec()),
                        crate::value::ArrayKind::List,
                    ),
                    Value::LazyList(list) => {
                        let items = self.force_lazy_list_vm(&list)?;
                        Value::Array(std::sync::Arc::new(items), crate::value::ArrayKind::List)
                    }
                    Value::LazyIoLines { .. } => {
                        let forced = self.force_if_lazy_io_lines(raw_popped)?;
                        let items = runtime::value_to_list(&forced);
                        Value::Array(std::sync::Arc::new(items), crate::value::ArrayKind::List)
                    }
                    other => Value::Array(
                        std::sync::Arc::new(vec![other]),
                        crate::value::ArrayKind::List,
                    ),
                }
            } else if is_bind {
                // `:=` binding preserves the container type (e.g. List stays List).
                match raw_popped {
                    Value::LazyList(list) => Value::real_array(self.force_lazy_list_vm(&list)?),
                    Value::LazyIoLines { .. } => {
                        let forced = self.force_if_lazy_io_lines(raw_popped)?;
                        Value::real_array(runtime::value_to_list(&forced))
                    }
                    other => other,
                }
            } else {
                match raw_popped {
                    Value::LazyList(list) => {
                        match list.env.get(Self::LAZY_ASSIGN_PRESERVE_MARKER) {
                            Some(Value::Bool(true)) => Value::LazyList(list),
                            _ => Value::real_array(self.force_lazy_list_vm(&list)?),
                        }
                    }
                    Value::LazyIoLines { .. } => {
                        let forced = self.force_if_lazy_io_lines(raw_popped)?;
                        runtime::coerce_to_array(forced)
                    }
                    other => runtime::coerce_to_array(other),
                }
            };
            // Preserve shaped array property on re-assignment
            if let Some(shape) = crate::runtime::utils::shaped_array_shape(&self.locals[idx])
                && shape.len() == 1
            {
                let items = runtime::value_to_list(&assigned);
                let item_count = items.len();
                let mut shaped_items: Vec<Value> = items.into_iter().take(shape[0]).collect();
                if item_count < shape[0] {
                    shaped_items.resize(shape[0], Value::Nil);
                }
                assigned = Value::Array(
                    std::sync::Arc::new(shaped_items),
                    crate::value::ArrayKind::Shaped,
                );
                crate::runtime::utils::mark_shaped_array(&assigned, Some(&shape));
                // Preserve container type metadata
                if let Some(info) = self.interpreter.container_type_metadata(&self.locals[idx]) {
                    self.interpreter
                        .register_container_type_metadata(&assigned, info);
                }
            }
            let class_name = match &self.locals[idx] {
                Value::Instance { class_name, .. } => Some(*class_name),
                Value::Package(class_name) => Some(*class_name),
                _ => None,
            };
            if let Some(class_name) = class_name {
                let class = class_name.resolve();
                if class == "Blob" || class.starts_with("blob") {
                    return Err(RuntimeError::assignment_ro(None));
                }
                if class == "Buf" || class.starts_with("buf") {
                    let items = runtime::value_to_list(&assigned)
                        .into_iter()
                        .map(|v| Value::Int(runtime::to_int(&v)))
                        .collect::<Vec<_>>();
                    assigned = self.try_compiled_method_or_interpret(
                        Value::Package(class_name),
                        "new",
                        items,
                    )?;
                }
            }
            assigned
        } else {
            Self::normalize_scalar_assignment_value(raw_popped)
        };
        if matches!(val, Value::Nil)
            && !matches!(self.locals[idx], Value::Nil)
            && let Some(def) = self.interpreter.var_default(name)
        {
            val = def.clone();
        }
        // For array variables with `is default(X)`, replace Nil elements
        // with the default value (Raku container semantics).
        if name.starts_with('@')
            && let Some(def) = self.interpreter.var_default(name).cloned()
            && let Value::Array(ref items, kind) = val
        {
            let is_hole =
                |v: &Value| matches!(v, Value::Nil) || matches!(v, Value::Package(n) if n == "Any");
            let has_holes = items.iter().any(is_hole);
            if has_holes {
                let replaced: Vec<Value> = items
                    .iter()
                    .map(|v| if is_hole(v) { def.clone() } else { v.clone() })
                    .collect();
                val = Value::Array(Arc::new(replaced), kind);
            }
        }
        // Skip typed container coercion for `:=` binding — it would create
        // a new Arc and lose container identity (e.g. Map metadata).
        if !is_bind && (name.starts_with('@') || name.starts_with('%')) {
            val = self.coerce_typed_container_assignment(name, val, has_explicit_initializer)?;
        }
        if let Some(constraint) = self.interpreter.var_type_constraint(name)
            && !name.starts_with('%')
            && !name.starts_with('@')
        {
            if matches!(val, Value::Nil) && self.interpreter.is_definite_constraint(&constraint) {
                if has_explicit_initializer {
                    return Err(RuntimeError::new(
                        runtime::utils::type_check_assignment_error(name, &constraint, &val),
                    ));
                }
                return Err(RuntimeError::new(format!(
                    "X::Syntax::Variable::MissingInitializer: Variable definition of type {} needs to be given an initializer",
                    constraint
                )));
            }
            if !matches!(val, Value::Nil) && !self.interpreter.type_matches_value(&constraint, &val)
            {
                return Err(RuntimeError::new(
                    runtime::utils::type_check_assignment_error(name, &constraint, &val),
                ));
            }
            if !matches!(val, Value::Nil) {
                val = self
                    .interpreter
                    .try_coerce_value_for_constraint(&constraint, val)?;
            }
            // Wrap native integer values on assignment (overflow wrapping)
            val = Self::wrap_native_int_by_constraint(&constraint, val)?;
        }
        let readonly_key = format!("__mutsu_sigilless_readonly::{}", name);
        let alias_key = format!("__mutsu_sigilless_alias::{}", name);
        if matches!(
            self.interpreter.env().get(&readonly_key),
            Some(Value::Bool(true))
        ) && !matches!(self.interpreter.env().get(&alias_key), Some(Value::Str(_)))
        {
            return Err(RuntimeError::assignment_ro(None));
        }
        if !name.starts_with('@') && !name.starts_with('%') && !name.starts_with('&') {
            self.interpreter.reset_atomic_var_key(name);
        }
        if let Some(source_name) = bind_source {
            let resolved_source = self.resolve_sigilless_alias_source_name(&source_name);
            // TODO: compile to bytecode - implement local slot aliasing for `:=` binding
            // so that `my $y := $x; my $x = 3; say $y` correctly prints 3.
            // The write propagation approach needs careful handling to avoid
            // breaking S12-class/mro-6e.t's role concretization tests.
            self.interpreter
                .env_mut()
                .insert(alias_key.clone(), Value::str(resolved_source));
            self.interpreter
                .env_mut()
                .insert(readonly_key, Value::Bool(false));
        }
        // If the current value is a Proxy, invoke STORE instead of overwriting
        if let Value::Proxy { storer, .. } = &self.locals[idx]
            && !matches!(storer.as_ref(), Value::Nil)
        {
            let proxy_val = self.locals[idx].clone();
            self.interpreter.assign_proxy_lvalue(proxy_val, val)?;
            return Ok(());
        }
        // If the current value is a HashSlotRef, write back to the parent hash
        if !is_bind && let Value::HashSlotRef { .. } = &self.locals[idx] {
            self.locals[idx].hash_slot_write(val);
            self.locals_dirty = true;
            return Ok(());
        }
        // When binding a Proxy to a variable, update FETCH/STORE closures' captured envs
        // so they can reference the Proxy by its binding variable name (simulating capture-by-ref).
        let val = Self::update_proxy_closure_envs(val, name);
        if self.interpreter.fatal_mode
            && !name.contains("__mutsu_")
            && let Some(err) = self.interpreter.failure_to_runtime_error_if_unhandled(&val)
        {
            return Err(err);
        }
        self.locals[idx] = val.clone();
        // Circular hash reference fixup: when assigning to a hash variable,
        // if any values in the new hash reference the old hash (captured on the
        // RHS before assignment), replace them with the new hash's Arc to create
        // a true circular reference (matching Raku container semantics).
        if name.starts_with('%') && !is_bind && !is_constant {
            Self::fixup_circular_hash_refs(&mut self.locals[idx], &old_hash_arc);
        }
        // Use the potentially fixed-up value for env/shared_vars.
        let val = self.locals[idx].clone();
        // Mark variable as readonly when storing a LazyThunk
        if matches!(val, Value::LazyThunk(..)) {
            self.interpreter.mark_readonly(name);
        }
        if (is_bind || is_constant) && name.starts_with('@') {
            // For `:=` bind and `constant @x`, bypass set_shared_var's
            // List->Array normalization so the container type is preserved.
            self.interpreter
                .env_mut()
                .insert(name.to_string(), val.clone());
        } else {
            self.set_env_with_main_alias(name, val.clone());
        }
        if let Some(symbol) = Self::term_symbol_from_name(name) {
            self.interpreter
                .env_mut()
                .insert(symbol.to_string(), val.clone());
            let pkg = self.interpreter.current_package().to_string();
            if pkg != "GLOBAL" {
                self.interpreter
                    .env_mut()
                    .insert(format!("{pkg}::term:<{symbol}>"), val.clone());
            }
        }
        let mut alias_name = self.interpreter.env().get(&alias_key).and_then(|v| {
            if let Value::Str(name) = v {
                Some(name.to_string())
            } else {
                None
            }
        });
        let mut seen_aliases = std::collections::HashSet::new();
        while let Some(current_alias) = alias_name {
            if !seen_aliases.insert(current_alias.clone()) {
                break;
            }
            self.update_local_if_exists(code, &current_alias, &val);
            self.interpreter
                .env_mut()
                .insert(current_alias.clone(), val.clone());
            let next_key = format!("__mutsu_sigilless_alias::{}", current_alias);
            alias_name = self.interpreter.env().get(&next_key).and_then(|v| {
                if let Value::Str(name) = v {
                    Some(name.to_string())
                } else {
                    None
                }
            });
        }
        if let Some(attr) = name.strip_prefix('.') {
            self.interpreter
                .env_mut()
                .insert(format!("!{}", attr), val.clone());
        } else if let Some(attr) = name.strip_prefix('!') {
            self.interpreter
                .env_mut()
                .insert(format!(".{}", attr), val.clone());
        }
        if name == "_"
            && let Some(ref source_var) = self.topic_source_var
            && !source_var.starts_with('@')
            && !source_var.starts_with('%')
        {
            let sv = source_var.clone();
            self.set_env_with_main_alias(&sv, val.clone());
            self.update_local_if_exists(code, &sv, &val);
        }
        if (name.starts_with('@') || name.starts_with('%'))
            && let Some(value_type) = self.interpreter.var_type_constraint(name)
        {
            let info = crate::runtime::ContainerTypeInfo {
                value_type,
                key_type: if name.starts_with('%') {
                    self.interpreter.var_hash_key_constraint(name)
                } else {
                    None
                },
                declared_type: None,
            };
            self.interpreter
                .register_container_type_metadata(&val, info);
        }
        Ok(())
    }

    pub(super) fn exec_set_var_dynamic_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        dynamic: bool,
    ) {
        let name = Self::const_str(code, name_idx);
        self.interpreter.set_var_dynamic(name, dynamic);
        // A fresh declaration without an explicit type must not inherit stale
        // constraints from an earlier lexical with the same name.
        self.interpreter.set_var_type_constraint(name, None);
        if !name.starts_with('@') && !name.starts_with('%') && !name.starts_with('&') {
            self.interpreter.reset_atomic_var_key_decl(name);
        }
        // Pre-initialize the variable in the env with a default value so that
        // closures created during the RHS expression can capture it.
        // This enables capture-by-reference patterns like:
        //   my $proxy := Proxy.new(STORE => -> $, \v { $proxy.VAR... })
        //
        // Skip &-sigiled variables here: seeding the lexical environment with
        // `&name = Any` before the RHS runs makes `EVAL(q[sub name() { ... }])`
        // look like a routine redeclaration instead of producing a callable to
        // bind into `my &name = ...`.
        if !name.starts_with('&') && !self.interpreter.env().contains_key(name) {
            let default = if name.starts_with('@') {
                Value::real_array(Vec::new())
            } else if name.starts_with('%') {
                Value::hash(std::collections::HashMap::new())
            } else {
                Value::Package(crate::symbol::Symbol::intern("Any"))
            };
            self.interpreter.env_mut().insert(name.to_string(), default);
        }
        // Track this variable as declared within the current block scope.
        // BlockScope restoration uses this to avoid propagating block-local
        // variable values back to the outer scope.
        if let Some(set) = self.block_declared_vars.last_mut() {
            set.insert(name.to_string());
        }
    }

    pub(super) fn exec_assign_expr_local_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let idx = idx as usize;

        // If the current local is a Proxy, invoke STORE instead of overwriting
        if let Value::Proxy { storer, .. } = &self.locals[idx]
            && !matches!(storer.as_ref(), Value::Nil)
        {
            let val = self.stack.pop().unwrap_or(Value::Nil);
            let proxy_val = self.locals[idx].clone();
            self.interpreter.assign_proxy_lvalue(proxy_val, val)?;
            return Ok(());
        }

        // Fast path for simple scalar variables — skip all metadata checks
        if code.simple_locals[idx] {
            let mut val = self.stack.pop().unwrap_or(Value::Nil);
            let name = &code.locals[idx];
            if !name.starts_with('@') && !name.starts_with('%') {
                val = Self::normalize_scalar_assignment_value(val);
            }
            if matches!(val, Value::Nil)
                && !matches!(self.locals[idx], Value::Nil)
                && let Some(def) = self.interpreter.var_default(name)
            {
                val = def.clone();
            }
            if let Some(constraint) = self.interpreter.var_type_constraint_fast(name).cloned() {
                let val = if matches!(val, Value::Nil) {
                    if constraint == "Mu" {
                        val
                    } else {
                        let nominal = self
                            .interpreter
                            .nominal_type_object_name_for_constraint(&constraint);
                        Value::Package(Symbol::intern(&nominal))
                    }
                } else if !self.interpreter.type_matches_value(&constraint, &val) {
                    return Err(RuntimeError::new(
                        runtime::utils::type_check_assignment_error(name, &constraint, &val),
                    ));
                } else if !matches!(val, Value::Nil | Value::Package(_)) {
                    self.interpreter
                        .try_coerce_value_for_constraint(&constraint, val)?
                } else {
                    val
                };
                self.locals[idx] = val.clone();
                self.stack.push(val);
            } else {
                self.locals[idx] = val.clone();
                self.stack.push(val);
            }
            if self.interpreter.fatal_mode
                && !name.contains("__mutsu_")
                && let Some(err) = self
                    .interpreter
                    .failure_to_runtime_error_if_unhandled(&self.locals[idx])
            {
                return Err(err);
            }
            // Also update env (and shared_vars if active) immediately so stale
            // references don't keep old Instance values alive (preventing DESTROY
            // from firing).
            self.interpreter
                .set_shared_var(name, self.locals[idx].clone());
            // Track topic mutations for map rw writeback
            if name == "_" {
                self.interpreter.env_mut().insert(
                    "__mutsu_rw_map_topic__".to_string(),
                    self.locals[idx].clone(),
                );
            }
            self.locals_dirty = true;
            return Ok(());
        }

        let raw_val = self.stack.pop().unwrap_or(Value::Nil);
        let name = &code.locals[idx];
        self.interpreter.check_readonly_for_modify(name)?;
        let mut val = if name.starts_with('%') {
            self.coerce_hash_var_value(name, raw_val)?
        } else if name.starts_with('@') {
            let mut assigned = match raw_val {
                Value::LazyList(list) => match list.env.get(Self::LAZY_ASSIGN_PRESERVE_MARKER) {
                    Some(Value::Bool(true)) => Value::LazyList(list),
                    _ => Value::real_array(self.force_lazy_list_vm(&list)?),
                },
                other => runtime::coerce_to_array(other),
            };
            let class_name = match &self.locals[idx] {
                Value::Instance { class_name, .. } => Some(*class_name),
                Value::Package(class_name) => Some(*class_name),
                _ => None,
            };
            if let Some(class_name) = class_name {
                let class = class_name.resolve();
                if class == "Blob" || class.starts_with("blob") {
                    return Err(RuntimeError::assignment_ro(None));
                }
                if class == "Buf" || class.starts_with("buf") {
                    let items = runtime::value_to_list(&assigned)
                        .into_iter()
                        .map(|v| Value::Int(runtime::to_int(&v)))
                        .collect::<Vec<_>>();
                    assigned = self.try_compiled_method_or_interpret(
                        Value::Package(class_name),
                        "new",
                        items,
                    )?;
                }
            }
            assigned
        } else {
            Self::normalize_scalar_assignment_value(raw_val)
        };
        if matches!(val, Value::Nil)
            && let Some(def) = self.interpreter.var_default(name)
        {
            val = def.clone();
        }
        if name.starts_with('@') || name.starts_with('%') {
            val = self.coerce_typed_container_assignment(name, val, false)?;
        }
        if let Some(constraint) = self.interpreter.var_type_constraint(name)
            && !name.starts_with('%')
            && !name.starts_with('@')
        {
            if matches!(val, Value::Nil) {
                if constraint != "Mu" {
                    // Assigning Nil to a typed variable resets it to the type object
                    let nominal = self
                        .interpreter
                        .nominal_type_object_name_for_constraint(&constraint);
                    val = Value::Package(Symbol::intern(&nominal));
                }
            } else if !self.interpreter.type_matches_value(&constraint, &val) {
                return Err(RuntimeError::new(
                    runtime::utils::type_check_assignment_error(name, &constraint, &val),
                ));
            }
            if !matches!(val, Value::Nil | Value::Package(_)) {
                val = self
                    .interpreter
                    .try_coerce_value_for_constraint(&constraint, val)?;
            }
        }
        let readonly_key = format!("__mutsu_sigilless_readonly::{}", name);
        let alias_key = format!("__mutsu_sigilless_alias::{}", name);
        if matches!(
            self.interpreter.env().get(&readonly_key),
            Some(Value::Bool(true))
        ) && !matches!(self.interpreter.env().get(&alias_key), Some(Value::Str(_)))
        {
            return Err(RuntimeError::assignment_ro(None));
        }
        if !name.starts_with('@') && !name.starts_with('%') && !name.starts_with('&') {
            self.interpreter.reset_atomic_var_key(name);
        }
        if self.interpreter.fatal_mode
            && !name.contains("__mutsu_")
            && let Some(err) = self.interpreter.failure_to_runtime_error_if_unhandled(&val)
        {
            return Err(err);
        }
        self.locals[idx] = val.clone();
        self.set_env_with_main_alias(name, val.clone());
        if let Some(alias_name) = self.interpreter.env().get(&alias_key).and_then(|v| {
            if let Value::Str(name) = v {
                Some(name.to_string())
            } else {
                None
            }
        }) {
            self.update_local_if_exists(code, &alias_name, &val);
            self.interpreter.env_mut().insert(alias_name, val.clone());
        }
        if let Some(attr) = name.strip_prefix('.') {
            self.interpreter
                .env_mut()
                .insert(format!("!{}", attr), val.clone());
        } else if let Some(attr) = name.strip_prefix('!') {
            self.interpreter
                .env_mut()
                .insert(format!(".{}", attr), val.clone());
        }
        if (name.starts_with('@') || name.starts_with('%'))
            && let Some(value_type) = self.interpreter.var_type_constraint(name)
        {
            let info = crate::runtime::ContainerTypeInfo {
                value_type,
                key_type: if name.starts_with('%') {
                    self.interpreter.var_hash_key_constraint(name)
                } else {
                    None
                },
                declared_type: None,
            };
            self.interpreter
                .register_container_type_metadata(&val, info);
        }
        self.stack.push(val);
        Ok(())
    }

    pub(super) fn exec_get_pseudo_stash_op(&mut self, code: &CompiledCode, name_idx: u32) {
        let name = Self::const_str(code, name_idx);
        if name.strip_suffix("::") == Some("OUTER") {
            // OUTER:: is lexical, not package-based. Expose captured lexical vars
            // from the current interpreter environment as stash entries.
            let mut entries: HashMap<String, Value> = HashMap::new();
            for (key, val) in self.interpreter.env() {
                if self.interpreter.should_hide_from_my_global_stash(key) {
                    continue;
                }
                let display_key = Self::add_sigil_prefix(key);
                entries.insert(display_key, val.clone());
            }
            self.stack.push(Value::Hash(Arc::new(entries)));
            return;
        }
        if let Some(package) = name.strip_suffix("::")
            && package != "MY"
        {
            self.stack
                .push(self.interpreter.package_stash_value(package));
            return;
        }

        self.ensure_locals_synced(code);
        // MY:: pseudo-stash: collect all variable names from current scope.
        let mut entries: HashMap<String, Value> = HashMap::new();
        for (i, var_name) in code.locals.iter().enumerate() {
            let val = self.locals[i].clone();
            let key = Self::add_sigil_prefix(var_name);
            entries.insert(key, val);
        }
        for (key, val) in self.interpreter.env() {
            if self.interpreter.should_hide_from_my_global_stash(key) {
                continue;
            }
            let display_key = Self::add_sigil_prefix(key);
            entries.entry(display_key).or_insert_with(|| val.clone());
        }
        self.stack.push(Value::Hash(Arc::new(entries)));
    }

    /// Add a sigil prefix to a variable name for display in pseudo-stash.
    /// Names starting with @, %, & already have sigils. Others get $ prefix.
    fn add_sigil_prefix(name: &str) -> String {
        if name.starts_with('$')
            || name.starts_with('@')
            || name.starts_with('%')
            || name.starts_with('&')
        {
            name.to_string()
        } else if name.starts_with('*') || name.starts_with('?') || name.starts_with('!') {
            // Twigil variables like *CWD → $*CWD
            format!("${}", name)
        } else if name.chars().next().is_some_and(|c| c.is_uppercase()) {
            // Type names, package names — no sigil
            name.to_string()
        } else {
            format!("${}", name)
        }
    }

    /// Execute HyperSlice opcode: recursively iterate a hash.
    pub(super) fn exec_hyper_slice_op(&mut self, adverb: u8) -> Result<(), RuntimeError> {
        use crate::ast::HyperSliceAdverb;

        let target = self.stack.pop().unwrap();
        let adverb = match adverb {
            0 => HyperSliceAdverb::Kv,
            1 => HyperSliceAdverb::K,
            2 => HyperSliceAdverb::V,
            3 => HyperSliceAdverb::Tree,
            4 => HyperSliceAdverb::DeepK,
            5 => HyperSliceAdverb::DeepKv,
            _ => HyperSliceAdverb::Kv,
        };

        let hash = match target {
            Value::Hash(h) => h,
            _ => {
                return Err(RuntimeError::new(
                    "Cannot use {**} hyperslice on a non-Hash value".to_string(),
                ));
            }
        };

        let mut result = Vec::new();
        let path: Vec<String> = Vec::new();
        Self::hyperslice_recurse(&hash, &path, adverb, &mut result);
        self.stack.push(Value::array(result));
        Ok(())
    }

    fn hyperslice_recurse(
        hash: &std::collections::HashMap<String, Value>,
        path: &[String],
        adverb: crate::ast::HyperSliceAdverb,
        result: &mut Vec<Value>,
    ) {
        use crate::ast::HyperSliceAdverb;

        for (key, value) in hash.iter() {
            let mut cur_path: Vec<String> = path.to_vec();
            cur_path.push(key.clone());

            match adverb {
                HyperSliceAdverb::Kv => {
                    if let Value::Hash(inner) = value {
                        Self::hyperslice_recurse(inner, &cur_path, adverb, result);
                    } else {
                        result.push(Value::str(key.clone()));
                        result.push(value.clone());
                    }
                }
                HyperSliceAdverb::K => {
                    if let Value::Hash(inner) = value {
                        Self::hyperslice_recurse(inner, &cur_path, adverb, result);
                    } else {
                        result.push(Value::str(key.clone()));
                    }
                }
                HyperSliceAdverb::V => {
                    if let Value::Hash(inner) = value {
                        Self::hyperslice_recurse(inner, &cur_path, adverb, result);
                    } else {
                        result.push(value.clone());
                    }
                }
                HyperSliceAdverb::Tree => {
                    // Tree mode: yield key-value pairs for all entries,
                    // including sub-hashes (as their original Value::Hash)
                    result.push(Value::str(key.clone()));
                    result.push(value.clone());
                    if let Value::Hash(inner) = value {
                        Self::hyperslice_recurse(inner, &cur_path, adverb, result);
                    }
                }
                HyperSliceAdverb::DeepK => {
                    if let Value::Hash(inner) = value {
                        Self::hyperslice_recurse(inner, &cur_path, adverb, result);
                    } else {
                        let key_array: Vec<Value> =
                            cur_path.iter().map(|s| Value::str(s.clone())).collect();
                        result.push(Value::array(key_array));
                    }
                }
                HyperSliceAdverb::DeepKv => {
                    if let Value::Hash(inner) = value {
                        Self::hyperslice_recurse(inner, &cur_path, adverb, result);
                    } else {
                        let key_array: Vec<Value> =
                            cur_path.iter().map(|s| Value::str(s.clone())).collect();
                        result.push(Value::array(key_array));
                        result.push(value.clone());
                    }
                }
            }
        }
    }

    /// Execute HyperIndex opcode: drill into nested hash by key path.
    pub(super) fn exec_hyper_index_op(&mut self) -> Result<(), RuntimeError> {
        let keys = self.stack.pop().unwrap();
        let target = self.stack.pop().unwrap();

        let key_list = match keys {
            Value::Array(items, ..) => items,
            Value::Seq(items) => Arc::new(items.to_vec()),
            _ => Arc::new(vec![keys]),
        };

        let mut current = target;
        for key in key_list.iter() {
            match current {
                Value::Hash(ref h) => {
                    let k = key.to_string_value();
                    current = h.get(&k).cloned().unwrap_or(Value::Nil);
                }
                _ => {
                    current = Value::Nil;
                    break;
                }
            }
        }

        self.stack.push(current);
        Ok(())
    }

    /// Wrap an integer value to fit within a native integer type's range.
    /// For non-native constraints or non-integer values, returns the value unchanged.
    pub(super) fn wrap_native_int_by_constraint(
        constraint: &str,
        val: Value,
    ) -> Result<Value, RuntimeError> {
        use crate::runtime::native_types;
        use num_bigint::BigInt as NumBigInt;
        use num_traits::ToPrimitive;

        let (base, _) = crate::runtime::types::strip_type_smiley(constraint);
        if !native_types::is_native_int_type(base) {
            return Ok(val);
        }
        // Full-width native types don't wrap — they should throw on overflow.
        if matches!(base, "int" | "int64" | "uint" | "uint64") {
            if let Value::BigInt(ref n) = val {
                // For unsigned types, values that fit in u64 are valid
                if matches!(base, "uint" | "uint64") && n.to_u64().is_some() {
                    return Ok(val);
                }
                // For signed types, any BigInt is too large (i64 values are Value::Int)
                // For unsigned types that didn't fit in u64, also too large
                let bits = n.bits();
                return Err(RuntimeError::new(format!(
                    "Cannot unbox {} bit wide bigint into native integer",
                    bits
                )));
            }
            return Ok(val);
        }
        let big_val = match &val {
            Value::Int(n) => NumBigInt::from(*n),
            Value::BigInt(n) => (**n).clone(),
            _ => return Ok(val),
        };
        if native_types::is_in_native_range(base, &big_val) {
            return Ok(val);
        }
        let wrapped = native_types::wrap_native_int(base, &big_val);
        Ok(wrapped
            .to_i64()
            .map(Value::Int)
            .unwrap_or_else(|| Value::bigint(wrapped)))
    }
}
