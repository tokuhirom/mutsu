use super::*;
use crate::symbol::Symbol;
use std::sync::Arc;

impl VM {
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
                return Err(RuntimeError::new("X::Assignment::RO"));
            };
            let Value::Array(items, ..) = container else {
                return Err(RuntimeError::new("X::Assignment::RO"));
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
        if let Value::Sub(data) = idx {
            let len = match target {
                Some(Value::Array(items, ..)) => items.len() as i64,
                _ => 0,
            };
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

    fn coerce_hash_var_value(&mut self, name: &str, value: Value) -> Result<Value, RuntimeError> {
        if let Some(constraint) = self.interpreter.var_type_constraint_fast(name).cloned()
            && let Some(trait_name) = Self::quant_hash_trait_from_constraint(&constraint)
        {
            return self
                .interpreter
                .call_method_with_values(value, trait_name, vec![]);
        }
        if self.interpreter.check_readonly_for_modify(name).is_err()
            && matches!(value, Value::Set(_) | Value::Bag(_) | Value::Mix(_))
        {
            return Ok(value);
        }
        if let Some(constraint) = self.interpreter.var_type_constraint(name)
            && constraint.starts_with("SetHash")
        {
            return Ok(runtime::utils::coerce_value_to_quanthash(&value));
        }
        Ok(runtime::coerce_to_hash(value))
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
            Value::LazyList(list) => self.interpreter.force_lazy_list_bridge(list)?,
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

    fn coerce_typed_container_assignment(
        &mut self,
        var_name: &str,
        value: Value,
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
                    let key_value = Value::str(key.clone());
                    let target_type =
                        coercion_target(constraint).unwrap_or_else(|| constraint.clone());
                    let coerced = if self
                        .interpreter
                        .type_matches_value(&target_type, &key_value)
                    {
                        key_value.clone()
                    } else {
                        self.interpreter
                            .try_coerce_value_for_constraint(constraint, key_value.clone())?
                    };
                    if !self.interpreter.type_matches_value(&target_type, &coerced) {
                        return Err(RuntimeError::new(runtime::utils::type_check_element_error(
                            var_name, constraint, &key_value,
                        )));
                    }
                    coerced.to_string_value()
                } else {
                    key.clone()
                };
                let coerced_val = if let Some(constraint) = &value_constraint {
                    if matches!(val, Value::Nil) {
                        val.clone()
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
    ) -> Result<Vec<Value>, RuntimeError> {
        let mut coerced_items = Vec::with_capacity(items.len());
        for item in items.iter() {
            if matches!(item, Value::Nil) {
                coerced_items.push(item.clone());
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
                )?;
                coerced_items.push(Value::Array(Arc::new(sub_coerced), *sub_kind));
                continue;
            }
            let target_type = coercion_target(constraint).unwrap_or_else(|| constraint.to_string());
            let coerced = if self.interpreter.type_matches_value(&target_type, item) {
                item.clone()
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
                let str_result = self
                    .interpreter
                    .call_method_with_values(v, "Str", Vec::new())?;
                result.push_str(&str_result.to_string_value());
                continue;
            }
            result.push_str(&crate::runtime::utils::coerce_to_str(&v));
        }
        self.stack.push(Value::str(result));
        Ok(())
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
            let new_val = Self::increment_value(&val);
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
            let new_val = Self::increment_value(&val);
            self.interpreter.assign_proxy_lvalue(raw_val, new_val)?;
            self.stack.push(val);
            return Ok(());
        }
        let val = Self::normalize_incdec_source(raw_val);
        let new_val = Self::increment_value(&val);
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
        let val = Self::normalize_incdec_source(raw_val);
        let new_val = Self::decrement_value(&val);
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
        let target_is_mixhash = self
            .interpreter
            .env()
            .get(&name)
            .and_then(|v| self.interpreter.container_type_metadata(v))
            .and_then(|info| info.declared_type)
            .is_some_and(|t| t == "MixHash");
        let idx_val = self.stack.pop().unwrap_or(Value::Nil);
        let key = idx_val.to_string_value();
        let current = if let Some(container) = self.interpreter.env().get(&name) {
            match container {
                Value::Hash(h) => h.get(&key).cloned().unwrap_or(Value::Nil),
                Value::Array(arr, ..) => {
                    if let Ok(i) = key.parse::<usize>() {
                        arr.get(i).cloned().unwrap_or(Value::Nil)
                    } else {
                        Value::Nil
                    }
                }
                Value::Mix(_) if !target_is_mixhash => {
                    return Err(RuntimeError::new("X::Assignment::RO"));
                }
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
            Self::increment_value(&effective)
        } else {
            Self::decrement_value(&effective)
        };
        if let Some(container) = self.interpreter.env_mut().get_mut(&name) {
            match *container {
                Value::Hash(ref mut h) => {
                    Arc::make_mut(h).insert(key, new_val.clone());
                }
                Value::Array(ref mut arr, ..) => {
                    if let Ok(i) = idx_val.to_string_value().parse::<usize>() {
                        let a = Arc::make_mut(arr);
                        while a.len() <= i {
                            a.push(Value::Nil);
                        }
                        a[i] = new_val.clone();
                    }
                }
                Value::Mix(ref mut mix) => {
                    let m = Arc::make_mut(mix);
                    if new_val.truthy() {
                        m.insert(key, Self::mix_assignment_weight(&new_val));
                    } else {
                        m.remove(&key);
                    }
                }
                _ => {}
            }
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
        let var_name = Self::const_str(code, name_idx).to_string();
        let target_is_mixhash = self
            .interpreter
            .env()
            .get(&var_name)
            .and_then(|v| self.interpreter.container_type_metadata(v))
            .and_then(|info| info.declared_type)
            .is_some_and(|t| t == "MixHash");
        let declared_shape_key = format!("__mutsu_shaped_array_dims::{var_name}");
        let has_declared_shape = self.interpreter.env().contains_key(&declared_shape_key);
        let idx = self.stack.pop().unwrap_or(Value::Nil);
        let index_target = self.interpreter.env().get(&var_name).cloned();
        let idx = self.resolve_whatever_index_for_target(idx, index_target.as_ref());
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
        // When assigning Nil to a typed container element, use the type object
        let val = if matches!(val, Value::Nil) {
            if let Some(constraint) = self.interpreter.var_type_constraint(&var_name) {
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
        let encoded_idx = Self::encode_bound_index(&idx);
        let is_bound_index = if bind_mode {
            self.is_bound_index(&var_name, &encoded_idx)
        } else {
            false
        };
        if !bind_mode && self.is_bound_index(&var_name, &encoded_idx) {
            return Err(RuntimeError::new("X::Assignment::RO"));
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
                            return Err(RuntimeError::new("X::Assignment::RO"));
                        }
                        // Multidimensional indexing: @arr[0;0] = 'x'
                        Self::assign_array_multidim(container, keys.as_ref(), val.clone())?;
                        initialized_marks.push(encoded_idx.clone());
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
                {
                    return Err(RuntimeError::new(runtime::utils::type_check_element_error(
                        &var_name,
                        &constraint,
                        &val,
                    )));
                }
                let range_slice = if let Some(indices) = Self::slice_indices_from_index(&idx) {
                    Some((indices, self.assignment_rhs_values(&val)?))
                } else {
                    None
                };
                let mut range_initialized_marks: Vec<String> = Vec::new();
                let mut pending_source_update: Option<(String, Value)> = None;
                let mut pending_varref_update: Option<(String, Option<usize>, Value)> = None;
                if let Some(container) = self.interpreter.env_mut().get_mut(&var_name) {
                    match *container {
                        Value::Hash(ref mut hash) => {
                            let is_self_hash_ref = matches!(
                                &val,
                                Value::Hash(source_hash) if Arc::ptr_eq(hash, source_hash)
                            );
                            let h = Arc::make_mut(hash);
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
                                    return Err(RuntimeError::new("X::Assignment::RO"));
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
                                    if let Some((source_name, source_index)) =
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
                            } else {
                                return Err(RuntimeError::new("Index out of bounds"));
                            }
                            self.mark_initialized_index(&var_name, encoded_idx.clone());
                            if bind_mode {
                                self.mark_bound_index(&var_name, encoded_idx.clone());
                            }
                        }
                        Value::Set(ref mut set) => {
                            let s = Arc::make_mut(set);
                            if val.truthy() {
                                s.insert(key.clone());
                            } else {
                                s.remove(&key);
                            }
                        }
                        Value::Bag(ref mut bag) => {
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
                        Value::Mix(ref mut mix) => {
                            if !target_is_mixhash {
                                return Err(RuntimeError::new("X::Assignment::RO"));
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
        self.stack.push(val);
        Ok(())
    }

    pub(super) fn exec_index_assign_expr_nested_op(&mut self, code: &CompiledCode, name_idx: u32) {
        let var_name = Self::const_str(code, name_idx).to_string();
        let inner_idx = self.stack.pop().unwrap_or(Value::Nil);
        let outer_idx = self.stack.pop().unwrap_or(Value::Nil);
        let val = self.stack.pop().unwrap_or(Value::Nil);
        let inner_key = inner_idx.to_string_value();
        let outer_key = outer_idx.to_string_value();

        if !self.interpreter.env().contains_key(&var_name) {
            self.interpreter.env_mut().insert(
                var_name.clone(),
                Value::hash(std::collections::HashMap::new()),
            );
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
        let is_atomic_int = self.interpreter.var_type_constraint(&name).as_deref()
            == Some("atomicint")
            || self.interpreter.var_type_constraint(atomic_name).as_deref() == Some("atomicint")
            || self.interpreter.get_shared_var(&atomic_name_key).is_some();
        if is_atomic_int {
            let fetched = self.interpreter.call_function(
                "__mutsu_atomic_fetch_var",
                vec![Value::str(atomic_name.to_string())],
            )?;
            self.locals[idx] = fetched.clone();
            self.stack.push(fetched);
            return Ok(());
        }
        let val = self.locals[idx].clone();
        // Fast path: non-Nil values are always valid — skip env lookup
        if matches!(val, Value::Nil) {
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
                self.stack.push(Value::Package(Symbol::intern(&nominal)));
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

        // Fast path for simple scalar variables — skip all metadata checks
        if code.simple_locals[idx] && bind_source.is_none() {
            let mut val = raw_popped;
            let name = &code.locals[idx];
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
                self.locals[idx] = val;
            } else {
                self.locals[idx] = val;
            }
            if self.interpreter.fatal_mode
                && !name.contains("__mutsu_")
                && let Some(err) = self
                    .interpreter
                    .failure_to_runtime_error_if_unhandled(&self.locals[idx])
            {
                return Err(err);
            }
            self.locals_dirty = true;
            return Ok(());
        }

        let name = &code.locals[idx];
        let mut val = if name.starts_with('%') {
            self.coerce_hash_var_value(name, raw_popped)?
        } else if name.starts_with('@') {
            let mut assigned = match raw_popped {
                Value::LazyList(list) => match list.env.get(Self::LAZY_ASSIGN_PRESERVE_MARKER) {
                    Some(Value::Bool(true)) => Value::LazyList(list),
                    _ => Value::real_array(self.interpreter.force_lazy_list_bridge(&list)?),
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
                    return Err(RuntimeError::new("X::Assignment::RO"));
                }
                if class == "Buf" || class.starts_with("buf") {
                    let items = runtime::value_to_list(&assigned)
                        .into_iter()
                        .map(|v| Value::Int(runtime::to_int(&v)))
                        .collect::<Vec<_>>();
                    assigned = self.interpreter.call_method_with_values(
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
        if name.starts_with('@') || name.starts_with('%') {
            val = self.coerce_typed_container_assignment(name, val)?;
        }
        if let Some(constraint) = self.interpreter.var_type_constraint(name)
            && !name.starts_with('%')
            && !name.starts_with('@')
        {
            if matches!(val, Value::Nil) && self.interpreter.is_definite_constraint(&constraint) {
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
        }
        let readonly_key = format!("__mutsu_sigilless_readonly::{}", name);
        let alias_key = format!("__mutsu_sigilless_alias::{}", name);
        if matches!(
            self.interpreter.env().get(&readonly_key),
            Some(Value::Bool(true))
        ) && !matches!(self.interpreter.env().get(&alias_key), Some(Value::Str(_)))
        {
            return Err(RuntimeError::new("X::Assignment::RO"));
        }
        if !name.starts_with('@') && !name.starts_with('%') && !name.starts_with('&') {
            self.interpreter.reset_atomic_var_key(name);
        }
        if let Some(source_name) = bind_source {
            let resolved_source = self.resolve_sigilless_alias_source_name(&source_name);
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
        self.set_env_with_main_alias(name, val.clone());
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
        if !self.interpreter.env().contains_key(name) {
            let default = if name.starts_with('@') {
                Value::real_array(Vec::new())
            } else if name.starts_with('%') {
                Value::hash(std::collections::HashMap::new())
            } else {
                Value::Package(crate::symbol::Symbol::intern("Any"))
            };
            self.interpreter.env_mut().insert(name.to_string(), default);
        }
    }

    pub(super) fn exec_assign_expr_local_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let idx = idx as usize;

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
                    _ => Value::real_array(self.interpreter.force_lazy_list_bridge(&list)?),
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
                    return Err(RuntimeError::new("X::Assignment::RO"));
                }
                if class == "Buf" || class.starts_with("buf") {
                    let items = runtime::value_to_list(&assigned)
                        .into_iter()
                        .map(|v| Value::Int(runtime::to_int(&v)))
                        .collect::<Vec<_>>();
                    assigned = self.interpreter.call_method_with_values(
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
            val = self.coerce_typed_container_assignment(name, val)?;
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
            return Err(RuntimeError::new("X::Assignment::RO"));
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
}
