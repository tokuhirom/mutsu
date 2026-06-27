use super::*;
use crate::symbol::Symbol;
use std::sync::Arc;

/// A `:=` bind-source cell pre-read before the container borrow: the shared
/// `ContainerRef` cell plus the source variable name to install it into
/// afterwards (`None` when the source is already bound to that cell).
pub(super) type BindSourceCell = (Option<String>, Arc<std::sync::Mutex<Value>>);

impl Interpreter {
    /// Return the default fill value for a native type constraint.
    /// For `int`/`uint` variants returns `Value::Int(0)`,
    /// for `num` variants returns `Value::Num(0.0)`,
    /// for `str` returns `Value::str("")`,
    /// otherwise returns `Value::Package("Any")`.
    pub(super) fn native_fill_for_constraint(constraint: Option<&str>) -> Value {
        match constraint {
            Some(
                "int" | "int8" | "int16" | "int32" | "int64" | "uint" | "uint8" | "uint16"
                | "uint32" | "uint64" | "byte" | "atomicint",
            ) => Value::Int(0),
            Some("num" | "num32" | "num64") => Value::Num(0.0),
            Some("str") => Value::str(String::new()),
            // A boxed typed array (e.g. `my Int @a`) fills empty slots with the
            // element type's type object, so holes gist as `(Int)` and roundtrip
            // through `.raku`. Strip any smiley/coercion suffix first.
            Some(c) => {
                let base = c
                    .split_once('(')
                    .map_or(c, |(b, _)| b)
                    .trim_end_matches(":D")
                    .trim_end_matches(":U");
                if base.is_empty() || base == "Any" || base == "Mu" || base.contains('[') {
                    Value::Package(Symbol::intern("Any"))
                } else {
                    Value::Package(Symbol::intern(base))
                }
            }
            None => Value::Package(Symbol::intern("Any")),
        }
    }

    /// Auto-vivify `items` to at least `new_len` elements, filling new slots
    /// with `fill`. Uses fallible reservation (`try_reserve`) so an absurd
    /// index (e.g. `@a[9999999999999] = 1`) yields a catchable `X::` instead of
    /// an uncatchable `handle_alloc_error` abort that `try {}` cannot recover
    /// from. (raku aborts with a MoarVM panic on the same input.)
    ///
    /// `pub(crate)` so other fallible allocation sites (e.g. shaped-array
    /// construction in `make_shaped_array`) can reuse the same guard.
    pub(crate) fn autoviv_resize(
        items: &mut Vec<Value>,
        new_len: usize,
        fill: Value,
    ) -> Result<(), RuntimeError> {
        if new_len > items.len() {
            items.try_reserve(new_len - items.len()).map_err(|_| {
                RuntimeError::new(format!(
                    "Cannot autovivify array to {new_len} elements: memory allocation failed"
                ))
            })?;
            items.resize(new_len, fill);
        }
        Ok(())
    }

    pub(super) fn delegated_mixin_attr_key(
        &self,
        mixins: &std::collections::HashMap<String, Value>,
        method_name: &str,
    ) -> Option<String> {
        self.delegated_role_attr_key_from_mixins(mixins, method_name)
    }

    pub(super) fn assign_mixin_container_slot(
        attr_value: &mut Value,
        idx: &Value,
        val: &Value,
        range_slice: &Option<(Vec<usize>, Vec<Value>)>,
    ) -> Result<bool, RuntimeError> {
        match attr_value {
            Value::Array(items, kind) if !matches!(idx, Value::Str(_)) => {
                let mut updated = (**items).clone();
                if let Some((slice_indices, vals)) = range_slice {
                    if let Some(max_idx) = slice_indices.last().copied()
                        && max_idx >= updated.len()
                    {
                        Self::autoviv_resize(
                            &mut updated,
                            max_idx + 1,
                            Value::Package(Symbol::intern("Any")),
                        )?;
                    }
                    for (offset, i) in slice_indices.iter().enumerate() {
                        updated[*i] = vals.get(offset).cloned().unwrap_or(Value::Nil);
                    }
                } else if let Some(i) = Self::index_to_usize(idx) {
                    Self::autoviv_resize(
                        &mut updated,
                        i + 1,
                        Value::Package(Symbol::intern("Any")),
                    )?;
                    updated[i] = val.clone();
                } else {
                    return Ok(false);
                }
                *attr_value = Value::Array(Arc::new(updated), *kind);
                Ok(true)
            }
            Value::Hash(hash) if matches!(idx, Value::Str(_)) => {
                let mut updated = (**hash).clone();
                updated.insert(idx.to_string_value(), val.clone());
                *attr_value = Value::Hash(Value::hash_arc(updated));
                Ok(true)
            }
            Value::Nil if !matches!(idx, Value::Str(_)) => {
                let mut updated = Vec::new();
                if let Some((slice_indices, vals)) = range_slice {
                    if let Some(max_idx) = slice_indices.last().copied() {
                        Self::autoviv_resize(
                            &mut updated,
                            max_idx + 1,
                            Value::Package(Symbol::intern("Any")),
                        )?;
                    }
                    for (offset, i) in slice_indices.iter().enumerate() {
                        updated[*i] = vals.get(offset).cloned().unwrap_or(Value::Nil);
                    }
                } else if let Some(i) = Self::index_to_usize(idx) {
                    Self::autoviv_resize(
                        &mut updated,
                        i + 1,
                        Value::Package(Symbol::intern("Any")),
                    )?;
                    updated[i] = val.clone();
                } else {
                    return Ok(false);
                }
                *attr_value = Value::real_array(updated);
                Ok(true)
            }
            Value::Nil if matches!(idx, Value::Str(_)) => {
                let mut updated = std::collections::HashMap::new();
                updated.insert(idx.to_string_value(), val.clone());
                *attr_value = Value::Hash(Value::hash_arc(updated));
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    /// Unwrap a `__mutsu_bind_index_value` payload to `(value, first_source)`.
    /// `value` is the RHS value being bound; `first_source` is the name of the
    /// single source variable when the bind is the common `LHS := $scalar`
    /// shape. Returns `(val, None)` for a plain (non-bind) value, so callers can
    /// treat the non-bind path unchanged.
    pub(super) fn unwrap_bind_index_value(val: Value) -> (Value, Option<String>) {
        if let Value::Pair(name, payload) = &val
            && name == "__mutsu_bind_index_value"
        {
            if let Value::Array(items, ..) = payload.as_ref() {
                let value = items.first().cloned().unwrap_or(Value::Nil);
                let source = match items.get(1) {
                    Some(Value::Array(srcs, ..)) => match srcs.first() {
                        Some(Value::Str(s)) if !s.is_empty() => Some((**s).clone()),
                        _ => None,
                    },
                    _ => None,
                };
                return (value, source);
            }
            return ((**payload).clone(), None);
        }
        (val, None)
    }

    pub(super) fn varref_target(value: &Value) -> Option<(String, Option<usize>)> {
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

    pub(super) fn make_varref_value(
        name: String,
        value: Value,
        source_index: Option<usize>,
    ) -> Value {
        let mut named = std::collections::HashMap::new();
        named.insert("__mutsu_varref_name".to_string(), Value::str(name));
        named.insert("__mutsu_varref_value".to_string(), value);
        if let Some(i) = source_index {
            named.insert("__mutsu_varref_index".to_string(), Value::Int(i as i64));
        }
        Value::capture(Vec::new(), named)
    }

    pub(super) fn assign_varref_target(
        &mut self,
        source_name: &str,
        source_index: Option<usize>,
        value: Value,
    ) -> Result<(), RuntimeError> {
        // Handle indexed source encoding: "varname\x00idx\x00index_str"
        if let Some(sep_pos) = source_name.find("\x00idx\x00") {
            let var_name = &source_name[..sep_pos];
            let idx_str = &source_name[sep_pos + 5..];
            if let Ok(i) = idx_str.parse::<usize>() {
                let Some(container) = self.env_mut().get_mut(var_name) else {
                    return Err(RuntimeError::assignment_ro(None));
                };
                if let Value::Array(items, ..) = container {
                    let arr = Arc::make_mut(items);
                    Self::autoviv_resize(arr, i + 1, Value::Package(Symbol::intern("Any")))?;
                    arr[i] = value;
                    return Ok(());
                }
            }
            if let Some(Value::Hash(hash)) = self.env_mut().get_mut(source_name) {
                let h = Arc::make_mut(hash);
                h.insert(idx_str.to_string(), value);
                return Ok(());
            }
            return Err(RuntimeError::assignment_ro(None));
        }
        if let Some(i) = source_index {
            let Some(container) = self.env_mut().get_mut(source_name) else {
                return Err(RuntimeError::assignment_ro(None));
            };
            let Value::Array(items, ..) = container else {
                return Err(RuntimeError::assignment_ro(None));
            };
            let arr = Arc::make_mut(items);
            Self::autoviv_resize(arr, i + 1, Value::Package(Symbol::intern("Any")))?;
            arr[i] = value;
            return Ok(());
        }
        self.env_mut().insert(source_name.to_string(), value);
        Ok(())
    }

    pub(super) fn resolve_whatever_index_for_target(
        &mut self,
        idx: Value,
        target: Option<&Value>,
    ) -> Value {
        let len = match target {
            Some(Value::Array(items, ..)) => items.len() as i64,
            // A `:=`-bound array is held in a `ContainerRef` cell; descend it so
            // a from-end index (`@a[*-1]`) resolves the real length instead of 0
            // (which would yield a negative effective index and X::OutOfRange).
            Some(Value::ContainerRef(cell)) => match &*cell.lock().unwrap() {
                Value::Array(items, ..) => items.len() as i64,
                _ => 0,
            },
            _ => 0,
        };
        // Bare Whatever (*) in array subscript means all indices: 0, 1, ..., len-1
        if matches!(idx, Value::Whatever) {
            let indices: Vec<Value> = (0..len).map(Value::Int).collect();
            return Value::Array(
                Arc::new(crate::value::ArrayData::new(indices)),
                crate::value::ArrayKind::List,
            );
        }
        if let Value::Sub(ref data) = idx {
            let mut sub_env = data.env.clone();
            // Pass length for ALL WhateverCode parameters (e.g. *-4 .. *-2 has 2 params)
            for p in &data.params {
                sub_env.insert(p.to_string(), Value::Int(len));
            }
            let saved_env = std::mem::take(self.env_mut());
            *self.env_mut() = sub_env;
            let result = loan_env!(self, eval_block_value(&data.body)).unwrap_or(Value::Nil);
            *self.env_mut() = saved_env;
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
                        let mut sub_env = data.env.clone();
                        for p in &data.params {
                            sub_env.insert(p.to_string(), Value::Int(len));
                        }
                        let saved_env = std::mem::take(self.env_mut());
                        *self.env_mut() = sub_env;
                        let result =
                            loan_env!(self, eval_block_value(&data.body)).unwrap_or(Value::Nil);
                        *self.env_mut() = saved_env;
                        resolved.push(result);
                    } else {
                        resolved.push(item.clone());
                    }
                }
                return Value::Array(Arc::new(crate::value::ArrayData::new(resolved)), kind);
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
            "Bag" => Some("Bag"),
            "BagHash" => Some("BagHash"),
            _ => None,
        }
    }
}
