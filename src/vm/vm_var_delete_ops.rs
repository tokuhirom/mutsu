use super::*;
use std::sync::Arc;

impl VM {
    /// Resolve WhateverCode indices for array deletion.
    /// Converts `*-N` style closures to concrete integer indices.
    fn resolve_delete_index_for_array(&mut self, idx: Value, container: &Value) -> Value {
        let arr_len = match container {
            Value::Array(items, ..) => items.len(),
            _ => return idx,
        };
        match &idx {
            Value::Sub(data) => {
                let len = arr_len as i64;
                let param = data.params.first().map(|s| s.as_str()).unwrap_or("_");
                let mut sub_env = data.env.clone();
                sub_env.insert(param.to_string(), Value::Int(len));
                let saved_env = std::mem::take(self.interpreter.env_mut());
                *self.interpreter.env_mut() = sub_env;
                let resolved = self
                    .interpreter
                    .eval_block_value(&data.body)
                    .unwrap_or(Value::Nil);
                *self.interpreter.env_mut() = saved_env;
                resolved
            }
            Value::Array(items, ..) => {
                // Array of indices: resolve each element
                let resolved: Vec<Value> = items
                    .iter()
                    .map(|v| self.resolve_delete_index_for_array(v.clone(), container))
                    .collect();
                Value::array(resolved)
            }
            _ => idx,
        }
    }

    /// Trim trailing "holes" from a named array variable after deletion.
    /// A hole is either `Value::Nil` (deleted slot) or an uninitialized
    /// `Value::Package("Any")` slot (auto-vivified gap).  Explicitly
    /// assigned slots are tracked via `__mutsu_initialized_index::` metadata
    /// and are NOT trimmed.
    fn trim_trailing_array_holes(&mut self, var_name: &str) {
        let init_key = format!("__mutsu_initialized_index::{}", var_name);
        // Clone the initialized set to avoid borrow conflicts
        let initialized: std::collections::HashSet<String> = self
            .interpreter
            .env()
            .get(&init_key)
            .and_then(|v| {
                if let Value::Hash(map) = v {
                    Some(map.keys().cloned().collect())
                } else {
                    None
                }
            })
            .unwrap_or_default();
        // Get the type constraint for typed arrays (e.g. "Int" for `my Int @a`)
        let type_constraint = self
            .interpreter
            .var_type_constraint(var_name)
            .unwrap_or_default();
        let env = self.interpreter.env_mut();
        let Some(container) = env.get_mut(var_name) else {
            return;
        };
        let Value::Array(items, ..) = container else {
            return;
        };
        let arr = Arc::make_mut(items);
        while let Some(last) = arr.last() {
            let idx_str = (arr.len() - 1).to_string();
            let is_hole = match last {
                Value::Nil => true,
                Value::Package(name) if name == "Any" => !initialized.contains(&idx_str),
                // For typed arrays (e.g. `my Int @a`), the type object is also a hole
                Value::Package(name)
                    if !type_constraint.is_empty() && name == type_constraint.as_str() =>
                {
                    !initialized.contains(&idx_str)
                }
                _ => false,
            };
            if is_hole {
                arr.pop();
            } else {
                break;
            }
        }
    }

    pub(super) fn exec_delete_index_named_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let var_name = Self::const_str(code, name_idx).to_string();
        let idx = self.stack.pop().unwrap_or(Value::Nil);
        let declared_type_del = self
            .interpreter
            .env()
            .get(&var_name)
            .and_then(|v| self.interpreter.container_type_metadata(v))
            .and_then(|info| info.declared_type);
        let _target_is_mixhash = declared_type_del.as_deref().is_some_and(|t| t == "MixHash");
        let _target_is_baghash = declared_type_del.as_deref().is_some_and(|t| t == "BagHash");
        let _target_is_sethash = declared_type_del.as_deref().is_some_and(|t| t == "SetHash");
        // Note: Bag/Set immutability checks for :delete are intentionally
        // omitted here because Bag/BagHash and Set/SetHash share the same
        // Value variants and the declared_type metadata is not always
        // available (e.g., in set operator internals). The Mix check below
        // is kept because Mix operations are less commonly used internally.
        // Sync OS environment and $*HOME when deleting from %*ENV
        if var_name == "%*ENV" {
            // Remove from OS environment
            #[cfg(not(target_family = "wasm"))]
            match &idx {
                Value::Array(keys, ..) => {
                    for k in keys.iter() {
                        let key_str = k.to_string_value();
                        // SAFETY: mutsu is single-threaded
                        unsafe {
                            std::env::remove_var(&key_str);
                        }
                    }
                }
                _ => {
                    let key_str = idx.to_string_value();
                    // SAFETY: mutsu is single-threaded
                    unsafe {
                        std::env::remove_var(&key_str);
                    }
                }
            }
            let deletes_home = match &idx {
                Value::Array(keys, ..) => keys.iter().any(|k| k.to_string_value() == "HOME"),
                _ => idx.to_string_value() == "HOME",
            };
            if deletes_home {
                self.interpreter
                    .env_mut()
                    .insert("$*HOME".to_string(), Value::Nil);
                self.interpreter
                    .env_mut()
                    .insert("*HOME".to_string(), Value::Nil);
            }
        }
        // Save type metadata before delete (Arc::make_mut may change pointer)
        let saved_meta = self
            .interpreter
            .env()
            .get(&var_name)
            .and_then(|v| self.interpreter.container_type_metadata(v));
        // Resolve WhateverCode indices (e.g. *-1) for array targets
        let idx = if let Some(container) = self.interpreter.env().get(&var_name).cloned() {
            self.resolve_delete_index_for_array(idx, &container)
        } else {
            idx
        };
        // For typed arrays (e.g. `my Int @a`), deleted elements become
        // the type object (e.g. `Int`) instead of `Any`.
        let hole_type = self
            .interpreter
            .var_type_constraint(&var_name)
            .unwrap_or_else(|| "Any".to_string());
        // Save idx for unmark step (idx is consumed by delete_from_container)
        let idx_for_unmark = idx.clone();
        let result = if let Some(container) = self.interpreter.env_mut().get_mut(&var_name) {
            // Check immutability for Set/Bag/Mix (immutable variants)
            match container {
                Value::Mix(_, is_mutable) if !*is_mutable => {
                    return Err(RuntimeError::immutable("Mix", "delete"));
                }
                Value::Set(_, is_mutable) if !*is_mutable => {
                    return Err(RuntimeError::immutable("Set", "delete"));
                }
                Value::Bag(_, is_mutable) if !*is_mutable => {
                    return Err(RuntimeError::immutable("Bag", "delete"));
                }
                _ => {}
            }
            Self::delete_from_container(container, idx, &hole_type)?
        } else {
            Self::delete_from_missing_container(idx)
        };
        // Remove deleted indices from the initialized-index tracking set
        // so that trim_trailing_array_holes recognizes them as holes.
        self.unmark_initialized_indices(&var_name, &idx_for_unmark);
        // Trim trailing holes from arrays after deletion.
        // A "hole" is either Nil (deleted) or an uninitialized Package("Any") slot.
        self.trim_trailing_array_holes(&var_name);
        // If the deleted value is a hole (Nil or type object like Package("Any")),
        // substitute the container's default value if one was set via `is default(...)`.
        let result = if matches!(&result, Value::Nil | Value::Package(_)) {
            if let Some(def) = self.interpreter.var_default(&var_name) {
                def.clone()
            } else {
                result
            }
        } else {
            result
        };
        // Re-register type metadata if it was lost due to Arc::make_mut
        if let Some(info) = saved_meta
            && let Some(container) = self.interpreter.env().get(&var_name)
            && self
                .interpreter
                .container_type_metadata(container)
                .is_none()
        {
            let container = container.clone();
            self.interpreter
                .register_container_type_metadata(&container, info);
        }
        // Re-register container default if it was lost due to Arc::make_mut.
        // Use var_default (name-based, survives mutations) as the source of
        // truth, and sync it to the current container's pointer-based default.
        if let Some(def) = self.interpreter.var_default(&var_name).cloned()
            && let Some(container) = self.interpreter.env().get(&var_name).cloned()
        {
            if self.interpreter.container_default(&container).is_none() {
                self.interpreter
                    .set_container_default(&container, def.clone());
            }
            // Sync env value to locals so reads through locals see the
            // updated container with its default.
            self.locals_set_by_name(code, &var_name, container);
        }
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_delete_index_expr_op(&mut self) -> Result<(), RuntimeError> {
        let idx = self.stack.pop().unwrap_or(Value::Nil);
        let mut target = self.stack.pop().unwrap_or(Value::Nil);
        // Note: We cannot distinguish Bag from BagHash or Set from SetHash
        // in the expression form (no variable metadata), so immutability
        // checks for Bag/Set are only in the named op path.
        let result = Self::delete_from_container(&mut target, idx, "Any")?;
        self.stack.push(result);
        Ok(())
    }

    /// Delete element(s) from an array container.
    /// When idx is a single value, delete that one element.
    /// When idx is an Array, delete each element (slice delete).
    /// When idx is a Range, expand to indices and delete each.
    /// Delete element(s) from an array container.
    /// When idx is a single value, delete that one element.
    /// When idx is an Array, delete each element (slice delete).
    /// When idx is a Range, expand to indices and delete each.
    fn delete_from_array(
        container: &mut Value,
        idx: Value,
        hole_type: &str,
    ) -> Result<Value, RuntimeError> {
        match idx {
            Value::Array(indices, ..) => {
                let indices_vec: Vec<Value> = indices.to_vec();
                let mut results = Vec::with_capacity(indices_vec.len());
                for i in indices_vec {
                    let r =
                        Self::delete_array_multidim(container, std::slice::from_ref(&i), hole_type)
                            .unwrap_or(Value::Nil);
                    results.push(r);
                }
                Ok(Value::array(results))
            }
            Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. } => {
                let expanded = crate::runtime::utils::value_to_list(&idx);
                let mut results = Vec::with_capacity(expanded.len());
                for i in expanded {
                    let r =
                        Self::delete_array_multidim(container, std::slice::from_ref(&i), hole_type)
                            .unwrap_or(Value::Nil);
                    results.push(r);
                }
                Ok(Value::array(results))
            }
            _ => {
                let r =
                    Self::delete_array_multidim(container, std::slice::from_ref(&idx), hole_type)?;
                Ok(r)
            }
        }
    }

    fn delete_from_missing_container(idx: Value) -> Value {
        match idx {
            Value::Array(keys, ..) => Value::array(vec![Value::Nil; keys.len()]),
            _ => Value::Nil,
        }
    }

    fn delete_from_container(
        container: &mut Value,
        idx: Value,
        hole_type: &str,
    ) -> Result<Value, RuntimeError> {
        Ok(match container {
            Value::Hash(hash) => match idx {
                Value::Whatever => {
                    let h = Arc::make_mut(hash);
                    let removed: Vec<Value> = h.values().cloned().collect();
                    h.clear();
                    Value::array(removed)
                }
                Value::Num(f) if f.is_infinite() && f.is_sign_positive() => {
                    let h = Arc::make_mut(hash);
                    let removed: Vec<Value> = h.values().cloned().collect();
                    h.clear();
                    Value::array(removed)
                }
                Value::Array(keys, ..) => {
                    let h = Arc::make_mut(hash);
                    let removed = keys
                        .iter()
                        .map(|key| h.remove(&key.to_string_value()).unwrap_or(Value::Nil))
                        .collect();
                    Value::array(removed)
                }
                _ => Arc::make_mut(hash)
                    .remove(&idx.to_string_value())
                    .unwrap_or(Value::Nil),
            },
            Value::Package(type_name) if type_name == "Hash" || type_name == "Hash:U" => {
                match idx {
                    Value::Array(keys, ..) => Value::array(vec![Value::Nil; keys.len()]),
                    _ => Value::Nil,
                }
            }
            Value::Array(..) => Self::delete_from_array(container, idx, hole_type)?,
            Value::Set(set, _) => match idx {
                Value::Array(keys, ..) => {
                    let s = Arc::make_mut(set);
                    let removed = keys
                        .iter()
                        .map(|key| Value::Bool(s.remove(&key.to_string_value())))
                        .collect();
                    Value::array(removed)
                }
                _ => Value::Bool(Arc::make_mut(set).remove(&idx.to_string_value())),
            },
            Value::Bag(bag, _) => match idx {
                Value::Array(keys, ..) => {
                    let b = Arc::make_mut(bag);
                    let removed = keys
                        .iter()
                        .map(|key| Value::Int(b.remove(&key.to_string_value()).unwrap_or(0)))
                        .collect();
                    Value::array(removed)
                }
                _ => Value::Int(
                    Arc::make_mut(bag)
                        .remove(&idx.to_string_value())
                        .unwrap_or(0),
                ),
            },
            Value::Mix(mix, _) => match idx {
                Value::Array(keys, ..) => {
                    let m = Arc::make_mut(mix);
                    let removed = keys
                        .iter()
                        .map(|key| Value::Num(m.remove(&key.to_string_value()).unwrap_or(0.0)))
                        .collect();
                    Value::array(removed)
                }
                _ => Value::Num(
                    Arc::make_mut(mix)
                        .remove(&idx.to_string_value())
                        .unwrap_or(0.0),
                ),
            },
            _ => match idx {
                Value::Array(keys, ..) => Value::array(vec![Value::Nil; keys.len()]),
                _ => Value::Nil,
            },
        })
    }

    pub(super) fn mix_weight_as_value(weight: f64) -> Value {
        if weight.is_finite() && weight.fract() == 0.0 {
            Value::Int(weight as i64)
        } else {
            Value::Num(weight)
        }
    }
}
