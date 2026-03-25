use super::*;
use crate::value::ArrayKind;

use super::builtins_multidim::{
    array_to_list, has_multi_indices, make_key_tuple, multidim_collect_leaves, multidim_delete,
    multidim_index,
};

impl Interpreter {
    /// Handle dynamic adverbs on multidim index: @array[$a;$b;$c]:$delete
    /// Args: [inner_expr_result, adverb_name, adverb_value]
    pub(super) fn builtin_multidim_adverb(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 3 {
            return Err(RuntimeError::new(
                "__mutsu_multidim_adverb expects value, adverb_name, and adverb_value",
            ));
        }
        let value = args[0].clone();
        let _adverb_name = args[1].to_string_value();
        let adverb_value = &args[2];

        // If the adverb is False, just return the value unchanged
        if !adverb_value.truthy() {
            return Ok(value);
        }

        // Adverb is True — currently only "delete" is supported.
        // When the inner expression is a MultiDimIndex result, we need to
        // delete the element from the array. However, the value has already
        // been evaluated, so we return it as-is for now.
        // TODO: Implement actual delete by restructuring the parser to
        // pass target array info.
        Ok(value)
    }

    /// Handle subscript adverbs (:kv, :k, :v, :p, etc.) on multidim index.
    /// Args: [target_array, adverb_name, dim0, dim1, dim2, ...]
    pub(super) fn builtin_multidim_subscript_adverb(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 3 {
            return Err(RuntimeError::new(
                "__mutsu_multidim_subscript_adverb expects target, adverb, and indices",
            ));
        }
        let target = &args[0];
        let adverb = args[1].to_string_value();
        let raw_indices = &args[2..];
        let indices = self.resolve_multidim_indices(target, raw_indices)?;

        // Check if we need multi-result mode
        if has_multi_indices(&indices) {
            return self.multidim_subscript_adverb_multi(target, &adverb, &indices);
        }

        let value = multidim_index(target, &indices);
        let key = make_key_tuple(&indices);
        let exists = !matches!(&value, Value::Nil);

        match adverb.as_str() {
            "k" => Ok(if exists { key } else { Value::Nil }),
            "kv" => {
                if exists {
                    let v = array_to_list(value);
                    Ok(Value::Array(
                        std::sync::Arc::new(vec![key, v]),
                        ArrayKind::List,
                    ))
                } else {
                    Ok(Value::Array(std::sync::Arc::new(vec![]), ArrayKind::List))
                }
            }
            "p" => {
                if exists {
                    let v = array_to_list(value);
                    Ok(Value::ValuePair(Box::new(key), Box::new(v)))
                } else {
                    Ok(Value::Nil)
                }
            }
            "v" => {
                if exists {
                    Ok(array_to_list(value))
                } else {
                    Ok(Value::Nil)
                }
            }
            "not-k" => Ok(if !exists { key } else { Value::Nil }),
            "not-kv" => {
                if !exists {
                    let v = array_to_list(value);
                    Ok(Value::Array(
                        std::sync::Arc::new(vec![key, v]),
                        ArrayKind::List,
                    ))
                } else {
                    Ok(Value::Array(std::sync::Arc::new(vec![]), ArrayKind::List))
                }
            }
            "not-p" => {
                if !exists {
                    let v = array_to_list(value);
                    Ok(Value::ValuePair(Box::new(key), Box::new(v)))
                } else {
                    Ok(Value::Nil)
                }
            }
            "not-v" => {
                if !exists {
                    Ok(array_to_list(value))
                } else {
                    Ok(Value::Nil)
                }
            }
            _ => Ok(value),
        }
    }

    /// Multi-result adverb handler for Whatever/list indices.
    fn multidim_subscript_adverb_multi(
        &mut self,
        target: &Value,
        adverb: &str,
        indices: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut leaves = Vec::new();
        multidim_collect_leaves(target, indices, &[], &mut leaves);

        let mut out = Vec::new();
        for (path, value) in leaves {
            let exists = !matches!(&value, Value::Nil);
            let key = if path.len() == 1 {
                Value::Int(path[0])
            } else {
                Value::Array(
                    std::sync::Arc::new(path.into_iter().map(Value::Int).collect()),
                    ArrayKind::List,
                )
            };
            match adverb {
                "k" => {
                    if exists {
                        out.push(key);
                    }
                }
                "kv" => {
                    if exists {
                        out.push(key);
                        out.push(array_to_list(value));
                    }
                }
                "p" => {
                    if exists {
                        out.push(Value::ValuePair(
                            Box::new(key),
                            Box::new(array_to_list(value)),
                        ));
                    }
                }
                "v" => {
                    if exists {
                        out.push(array_to_list(value));
                    }
                }
                "not-k" => {
                    if !exists {
                        out.push(key);
                    }
                }
                "not-kv" => {
                    if !exists {
                        out.push(key);
                        out.push(array_to_list(value));
                    }
                }
                "not-p" => {
                    if !exists {
                        out.push(Value::ValuePair(
                            Box::new(key),
                            Box::new(array_to_list(value)),
                        ));
                    }
                }
                "not-v" => {
                    if !exists {
                        out.push(array_to_list(value));
                    }
                }
                _ => out.push(value),
            }
        }
        Ok(Value::array(out))
    }

    /// Handle :exists with secondary adverbs on multidim index.
    /// Args: [target_array, negated_bool, adverb_name, dim0, dim1, ...]
    pub(super) fn builtin_multidim_exists_adverb(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 4 {
            return Err(RuntimeError::new(
                "__mutsu_multidim_exists_adverb expects target, negated, adverb, and indices",
            ));
        }
        let target = &args[0];
        let negated = args[1].truthy();
        let adverb = args[2].to_string_value();
        let raw_indices = &args[3..];
        let indices = self.resolve_multidim_indices(target, raw_indices)?;
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = target
            && class_name == "Stash"
            && let Some(Value::Hash(symbols)) = attributes.get("symbols")
        {
            let stash_exists = |idx: &Value| {
                let key = idx.to_string_value();
                if symbols.contains_key(&key) {
                    return true;
                }
                if !key.starts_with('$')
                    && !key.starts_with('@')
                    && !key.starts_with('%')
                    && !key.starts_with('&')
                {
                    return symbols.contains_key(&format!("${key}"));
                }
                false
            };
            let stash_indices: Vec<Value> = if indices.len() > 1 {
                indices.clone()
            } else {
                match &indices[0] {
                    Value::Array(items, ..) => items.to_vec(),
                    one => vec![one.clone()],
                }
            };
            let exists_vals: Vec<bool> = stash_indices.iter().map(stash_exists).collect();
            let exists_vals: Vec<bool> = if negated {
                exists_vals.into_iter().map(|v| !v).collect()
            } else {
                exists_vals
            };
            if stash_indices.len() > 1 {
                return Ok(Value::array(
                    exists_vals.into_iter().map(Value::Bool).collect::<Vec<_>>(),
                ));
            }
            return Ok(Value::Bool(*exists_vals.first().unwrap_or(&false)));
        }

        // Multi-result mode for Whatever/list indices
        if has_multi_indices(&indices) {
            return self.multidim_exists_adverb_multi(target, negated, &adverb, &indices);
        }

        let value = multidim_index(target, &indices);
        let raw_exists = !matches!(&value, Value::Nil);
        let exists = if negated { !raw_exists } else { raw_exists };
        let key = make_key_tuple(&indices);

        match adverb.as_str() {
            "none" => Ok(Value::Bool(exists)),
            "kv" => {
                if raw_exists {
                    Ok(Value::Array(
                        std::sync::Arc::new(vec![key, Value::Bool(exists)]),
                        ArrayKind::List,
                    ))
                } else {
                    Ok(Value::Array(std::sync::Arc::new(vec![]), ArrayKind::List))
                }
            }
            "p" => {
                if raw_exists {
                    Ok(Value::ValuePair(
                        Box::new(key),
                        Box::new(Value::Bool(exists)),
                    ))
                } else {
                    Ok(Value::Nil)
                }
            }
            "k" => {
                if raw_exists {
                    Ok(key)
                } else {
                    Ok(Value::Nil)
                }
            }
            "v" => Ok(Value::Bool(exists)),
            _ => Ok(Value::Bool(exists)),
        }
    }

    /// Multi-result :exists adverb handler for Whatever/list indices.
    fn multidim_exists_adverb_multi(
        &mut self,
        target: &Value,
        negated: bool,
        adverb: &str,
        indices: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut leaves = Vec::new();
        multidim_collect_leaves(target, indices, &[], &mut leaves);

        let mut out = Vec::new();
        for (path, value) in leaves {
            let raw_exists = !matches!(&value, Value::Nil);
            let exists = if negated { !raw_exists } else { raw_exists };
            let key = if path.len() == 1 {
                Value::Int(path[0])
            } else {
                Value::Array(
                    std::sync::Arc::new(path.into_iter().map(Value::Int).collect()),
                    ArrayKind::List,
                )
            };
            match adverb {
                "none" => out.push(Value::Bool(exists)),
                "kv" => {
                    if raw_exists {
                        out.push(key);
                        out.push(Value::Bool(exists));
                    }
                }
                "p" => {
                    if raw_exists {
                        out.push(Value::ValuePair(
                            Box::new(key),
                            Box::new(Value::Bool(exists)),
                        ));
                    }
                }
                "k" => {
                    if raw_exists {
                        out.push(key);
                    }
                }
                "v" => out.push(Value::Bool(exists)),
                _ => out.push(Value::Bool(exists)),
            }
        }
        Ok(Value::array(out))
    }

    /// Handle :delete on multidim index.
    /// Args: [var_name_string, dim0, dim1, ...]
    pub(super) fn builtin_multidim_delete(
        &mut self,
        args: &mut [Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 2 {
            return Err(RuntimeError::new(
                "__mutsu_multidim_delete expects var_name and indices",
            ));
        }
        let var_name = args[0].to_string_value();
        let raw_indices = args[1..].to_vec();
        let target_val = self.env.get(&var_name).cloned().unwrap_or(Value::Nil);
        let indices = self.resolve_multidim_indices(&target_val, &raw_indices)?;
        if let Some(target) = self.env.get_mut(&var_name) {
            Ok(multidim_delete(target, &indices))
        } else {
            Ok(Value::Nil)
        }
    }

    /// Resolve WhateverCode indices: if an index is a Sub (WhateverCode),
    /// call it with the current dimension's array length.
    fn resolve_multidim_indices(
        &mut self,
        target: &Value,
        indices: &[Value],
    ) -> Result<Vec<Value>, RuntimeError> {
        let mut resolved = Vec::with_capacity(indices.len());
        let mut current = target.clone();
        for idx in indices {
            match idx {
                Value::Sub(..) => {
                    // WhateverCode: call with array length
                    let len = match &current {
                        Value::Array(items, ..) => Value::Int(items.len() as i64),
                        _ => Value::Int(0),
                    };
                    let result = self.call_sub_value(idx.clone(), vec![len], false)?;
                    // Navigate to next dimension
                    let resolved_idx = result.clone();
                    current = multidim_index(&current, std::slice::from_ref(&resolved_idx));
                    resolved.push(result);
                }
                _ => {
                    current = multidim_index(&current, std::slice::from_ref(idx));
                    resolved.push(idx.clone());
                }
            }
        }
        Ok(resolved)
    }

    /// Handle dynamic adverb (:$delete) on multidim index.
    /// Args: [var_name_string, adverb_name, adverb_value, dim0, dim1, ...]
    pub(super) fn builtin_multidim_dynamic_adverb(
        &mut self,
        args: &mut [Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 4 {
            return Err(RuntimeError::new(
                "__mutsu_multidim_dynamic_adverb expects var_name, name, value, and indices",
            ));
        }
        let var_name = args[0].to_string_value();
        let adverb_value = args[2].truthy();
        let raw_indices = args[3..].to_vec();

        let target = self.env.get(&var_name).cloned().unwrap_or(Value::Nil);
        let indices = self.resolve_multidim_indices(&target, &raw_indices)?;

        if adverb_value {
            // Multi-result mode for Whatever/list indices
            if has_multi_indices(&indices) {
                let mut leaves = Vec::new();
                multidim_collect_leaves(&target, &indices, &[], &mut leaves);
                if let Some(t) = self.env.get_mut(&var_name) {
                    multidim_delete(t, &indices);
                }
                let values: Vec<Value> = leaves.into_iter().map(|(_, v)| v).collect();
                return Ok(Value::array(values));
            }
            if let Some(target) = self.env.get_mut(&var_name) {
                Ok(multidim_delete(target, &indices))
            } else {
                Ok(Value::Nil)
            }
        } else {
            Ok(multidim_index(&target, &indices))
        }
    }

    /// Handle :kv/:k/:v/:p with dynamic :$delete on multidim index.
    /// Args: [var_name_str, adverb_name, delete_flag, dim0, dim1, ...]
    pub(super) fn builtin_multidim_subscript_adverb_dyn(
        &mut self,
        args: &mut [Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 4 {
            return Err(RuntimeError::new(
                "__mutsu_multidim_subscript_adverb_dyn expects var_name, adverb, delete, and indices",
            ));
        }
        let var_name = args[0].to_string_value();
        let adverb = args[1].to_string_value();
        let do_delete = args[2].truthy();
        let raw_indices = args[3..].to_vec();

        let target = self.env.get(&var_name).cloned().unwrap_or(Value::Nil);
        let indices = self.resolve_multidim_indices(&target, &raw_indices)?;

        // Multi-result mode for Whatever/list indices
        if has_multi_indices(&indices) {
            // Collect leaves before potentially deleting
            let mut leaves = Vec::new();
            multidim_collect_leaves(&target, &indices, &[], &mut leaves);
            if do_delete && let Some(t) = self.env.get_mut(&var_name) {
                multidim_delete(t, &indices);
            }
            let mut out = Vec::new();
            for (path, value) in leaves {
                let exists = !matches!(&value, Value::Nil);
                let key = if path.len() == 1 {
                    Value::Int(path[0])
                } else {
                    Value::Array(
                        std::sync::Arc::new(path.into_iter().map(Value::Int).collect()),
                        ArrayKind::List,
                    )
                };
                match adverb.as_str() {
                    "k" => {
                        if exists {
                            out.push(key);
                        }
                    }
                    "kv" => {
                        if exists {
                            out.push(key);
                            out.push(array_to_list(value));
                        }
                    }
                    "p" => {
                        if exists {
                            out.push(Value::ValuePair(
                                Box::new(key),
                                Box::new(array_to_list(value)),
                            ));
                        }
                    }
                    "v" => {
                        if exists {
                            out.push(array_to_list(value));
                        }
                    }
                    _ => out.push(value),
                }
            }
            return Ok(Value::array(out));
        }

        let value = if do_delete {
            if let Some(target) = self.env.get_mut(&var_name) {
                multidim_delete(target, &indices)
            } else {
                Value::Nil
            }
        } else {
            multidim_index(&target, &indices)
        };

        let key = make_key_tuple(&indices);
        let exists = !matches!(&value, Value::Nil);

        match adverb.as_str() {
            "k" => Ok(if exists { key } else { Value::Nil }),
            "kv" => {
                if exists {
                    let v = array_to_list(value);
                    Ok(Value::Array(
                        std::sync::Arc::new(vec![key, v]),
                        ArrayKind::List,
                    ))
                } else {
                    Ok(Value::Array(std::sync::Arc::new(vec![]), ArrayKind::List))
                }
            }
            "p" => {
                if exists {
                    let v = array_to_list(value);
                    Ok(Value::ValuePair(Box::new(key), Box::new(v)))
                } else {
                    Ok(Value::Nil)
                }
            }
            "v" => {
                if exists {
                    Ok(array_to_list(value))
                } else {
                    Ok(Value::Nil)
                }
            }
            _ => Ok(value),
        }
    }

    /// Handle :exists:kv/:exists:p with dynamic :$delete on multidim index.
    /// Args: [var_name_str, negated_bool, delete_flag, adverb_name, dim0, dim1, ...]
    pub(super) fn builtin_multidim_exists_adverb_dyn(
        &mut self,
        args: &mut [Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 5 {
            return Err(RuntimeError::new(
                "__mutsu_multidim_exists_adverb_dyn requires 5+ args",
            ));
        }
        let var_name = args[0].to_string_value();
        let negated = args[1].truthy();
        let do_delete = args[2].truthy();
        let adverb = args[3].to_string_value();
        let raw_indices = args[4..].to_vec();

        // First get value (need to read before potentially deleting)
        let target_val = self.env.get(&var_name).cloned().unwrap_or(Value::Nil);
        let indices = self.resolve_multidim_indices(&target_val, &raw_indices)?;

        // Multi-result mode for Whatever/list indices
        if has_multi_indices(&indices) {
            let mut leaves = Vec::new();
            multidim_collect_leaves(&target_val, &indices, &[], &mut leaves);
            if do_delete && let Some(t) = self.env.get_mut(&var_name) {
                multidim_delete(t, &indices);
            }
            let mut out = Vec::new();
            for (path, value) in leaves {
                let raw_exists = !matches!(&value, Value::Nil);
                let exists = if negated { !raw_exists } else { raw_exists };
                let key = if path.len() == 1 {
                    Value::Int(path[0])
                } else {
                    Value::Array(
                        std::sync::Arc::new(path.into_iter().map(Value::Int).collect()),
                        ArrayKind::List,
                    )
                };
                match adverb.as_str() {
                    "none" => out.push(Value::Bool(exists)),
                    "kv" => {
                        if raw_exists {
                            out.push(key);
                            out.push(Value::Bool(exists));
                        }
                    }
                    "p" => {
                        if raw_exists {
                            out.push(Value::ValuePair(
                                Box::new(key),
                                Box::new(Value::Bool(exists)),
                            ));
                        }
                    }
                    "k" => {
                        if raw_exists {
                            out.push(key);
                        }
                    }
                    _ => out.push(Value::Bool(exists)),
                }
            }
            return Ok(Value::array(out));
        }

        let value = multidim_index(&target_val, &indices);
        // Then delete if requested
        if do_delete && let Some(target) = self.env.get_mut(&var_name) {
            multidim_delete(target, &indices);
        }

        let raw_exists = !matches!(&value, Value::Nil);
        let exists = if negated { !raw_exists } else { raw_exists };
        let key = make_key_tuple(&indices);

        match adverb.as_str() {
            "none" => Ok(Value::Bool(exists)),
            "kv" => {
                if raw_exists {
                    Ok(Value::Array(
                        std::sync::Arc::new(vec![key, Value::Bool(exists)]),
                        ArrayKind::List,
                    ))
                } else {
                    Ok(Value::Array(std::sync::Arc::new(vec![]), ArrayKind::List))
                }
            }
            "p" => {
                if raw_exists {
                    Ok(Value::ValuePair(
                        Box::new(key),
                        Box::new(Value::Bool(exists)),
                    ))
                } else {
                    Ok(Value::Nil)
                }
            }
            "k" => {
                if raw_exists {
                    Ok(key)
                } else {
                    Ok(Value::Nil)
                }
            }
            _ => Ok(Value::Bool(exists)),
        }
    }
}
