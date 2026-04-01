use super::*;

impl VM {
    /// Multi-dimensional indexing: @a[$x;$y;$z]
    /// Stack: [target, dim0, dim1, ..., dimN-1] → [result]
    pub(super) fn exec_multi_dim_index_op(&mut self, ndims: u32) -> Result<(), RuntimeError> {
        let ndims = ndims as usize;
        let mut dims = Vec::with_capacity(ndims);
        for _ in 0..ndims {
            dims.push(self.stack.pop().unwrap_or(Value::Nil));
        }
        dims.reverse();
        let target = self.stack.pop().unwrap_or(Value::Nil);

        let result = self.multi_dim_index_read(&target, &dims)?;
        self.stack.push(result);
        Ok(())
    }

    /// Read a value from a nested array using multi-dimensional indices.
    /// Each dimension can be:
    /// - A scalar (Int, Str, Num, Rat, WhateverCode) — index into that level
    /// - Whatever (*) — iterate all elements at that level
    /// - An array/list — iterate specified indices at that level
    fn multi_dim_index_read(
        &mut self,
        target: &Value,
        dims: &[Value],
    ) -> Result<Value, RuntimeError> {
        if dims.is_empty() {
            return Ok(target.clone());
        }
        let dim = &dims[0];
        let rest = &dims[1..];

        match dim {
            Value::Whatever => {
                // Iterate all elements at this level
                let items = match target {
                    Value::Array(items, ..) => items,
                    _ => return Ok(Value::Nil),
                };
                let has_more_multi = rest
                    .iter()
                    .any(|v| matches!(v, Value::Whatever | Value::Array(..)));
                let mut out = Vec::with_capacity(items.len());
                for item in items.iter() {
                    let result = self.multi_dim_index_read(item, rest)?;
                    if has_more_multi {
                        // Flatten intermediate array results from deeper * or list dims
                        if let Value::Array(inner, ..) = &result {
                            out.extend(inner.iter().cloned());
                        } else {
                            out.push(result);
                        }
                    } else {
                        out.push(result);
                    }
                }
                Ok(Value::array(out))
            }
            Value::Array(indices, ..) => {
                // Multiple indices at this dimension level
                let items = match target {
                    Value::Array(items, ..) => items,
                    _ => return Ok(Value::Nil),
                };
                let has_more_multi = rest
                    .iter()
                    .any(|v| matches!(v, Value::Whatever | Value::Array(..)));
                let mut out = Vec::with_capacity(indices.len());
                for idx in indices.iter() {
                    let result = if let Some(i) = Self::index_to_usize(idx) {
                        if i < items.len() {
                            self.multi_dim_index_read(&items[i], rest)?
                        } else {
                            self.multi_dim_index_read(&Value::Nil, rest)?
                        }
                    } else {
                        Value::Nil
                    };
                    if has_more_multi {
                        if let Value::Array(inner, ..) = &result {
                            out.extend(inner.iter().cloned());
                        } else {
                            out.push(result);
                        }
                    } else {
                        out.push(result);
                    }
                }
                Ok(Value::array(out))
            }
            _ => {
                // Scalar index — resolve WhateverCode first
                let resolved = self.resolve_whatever_code_index(dim, target);
                let idx = resolved.as_ref().unwrap_or(dim);
                if let Some(i) = Self::index_to_usize(idx) {
                    let items = match target {
                        Value::Array(items, ..) => items,
                        _ => return Ok(Value::Nil),
                    };
                    if i < items.len() {
                        self.multi_dim_index_read(&items[i], rest)
                    } else {
                        // Out of bounds — return Nil for scalar index
                        Ok(Value::Nil)
                    }
                } else {
                    // Non-numeric index (e.g., string "0")
                    let i = idx.to_string_value().parse::<usize>().ok();
                    if let Some(i) = i {
                        let items = match target {
                            Value::Array(items, ..) => items,
                            _ => return Ok(Value::Nil),
                        };
                        if i < items.len() {
                            self.multi_dim_index_read(&items[i], rest)
                        } else {
                            Ok(Value::Nil)
                        }
                    } else {
                        Ok(Value::Nil)
                    }
                }
            }
        }
    }

    /// Resolve WhateverCode (e.g., *-1) or numeric coercion for a dimension index.
    fn resolve_whatever_code_index(&mut self, dim: &Value, target: &Value) -> Option<Value> {
        if let Value::Sub(data) = dim {
            let len = match target {
                Value::Array(items, ..) => items.len() as i64,
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
            return Some(result);
        }
        if let Value::Rat(n, d) = dim {
            return Some(Value::Int(*n / *d));
        }
        if let Value::Num(f) = dim {
            return Some(Value::Int(*f as i64));
        }
        if let Value::Str(s) = dim
            && let Ok(i) = s.parse::<i64>()
        {
            return Some(Value::Int(i));
        }
        None
    }

    /// Multi-dimensional index assignment with named target.
    /// Stack: [value, dim0, dim1, ..., dimN-1]
    pub(super) fn exec_multi_dim_index_assign_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        ndims: u32,
    ) -> Result<(), RuntimeError> {
        let ndims = ndims as usize;
        let mut dims = Vec::with_capacity(ndims);
        for _ in 0..ndims {
            dims.push(self.stack.pop().unwrap_or(Value::Nil));
        }
        dims.reverse();
        let value = self.stack.pop().unwrap_or(Value::Nil);

        let var_name = Self::const_str(code, name_idx).to_string();

        // Resolve WhateverCode indices
        let target_val = self
            .interpreter
            .env()
            .get(&var_name)
            .cloned()
            .unwrap_or(Value::Nil);
        let dims = self.resolve_multidim_indices_for_assign(&target_val, &dims)?;

        // Check if the index is bound (read-only)
        let encoded_idx = dims
            .iter()
            .map(|d| d.to_string_value())
            .collect::<Vec<_>>()
            .join(";");
        if self.is_bound_index(&var_name, &encoded_idx) {
            return Err(RuntimeError::assignment_ro(None));
        }

        // Check if target is a shaped array - use bounds-checked assignment
        let declared_shape_key = format!("__mutsu_shaped_array_dims::{var_name}");
        let has_declared_shape = self.interpreter.env().contains_key(&declared_shape_key);
        let is_shaped = has_declared_shape
            || self
                .interpreter
                .env()
                .get(&var_name)
                .is_some_and(crate::runtime::utils::is_shaped_array);

        // Capture container type metadata before mutation (Arc pointer may change)
        let old_type_info = self
            .interpreter
            .env()
            .get(&var_name)
            .and_then(|v| self.interpreter.container_type_metadata(v));

        // Get mutable reference to the target variable
        if let Some(container) = self.interpreter.env_mut().get_mut(&var_name) {
            if is_shaped {
                // For shaped arrays, use bounds-checked assignment
                Self::assign_array_multidim(container, &dims, value.clone())?;
            } else {
                Self::multi_dim_assign(container, &dims, value.clone())?;
            }
        }

        // Re-register container type metadata if Arc pointer changed
        if let Some(info) = old_type_info
            && let Some(updated) = self.interpreter.env().get(&var_name).cloned()
        {
            self.interpreter
                .register_container_type_metadata(&updated, info);
        }

        self.stack.push(value);
        Ok(())
    }

    /// Multi-dimensional index assignment with generic (expression) target.
    /// Stack: [target, dim0, ..., dimN-1, value]
    pub(super) fn exec_multi_dim_index_assign_generic_op(
        &mut self,
        ndims: u32,
    ) -> Result<(), RuntimeError> {
        let ndims = ndims as usize;
        let value = self.stack.pop().unwrap_or(Value::Nil);
        let mut dims = Vec::with_capacity(ndims);
        for _ in 0..ndims {
            dims.push(self.stack.pop().unwrap_or(Value::Nil));
        }
        dims.reverse();
        let mut target = self.stack.pop().unwrap_or(Value::Nil);
        let is_shaped = crate::runtime::utils::is_shaped_array(&target);
        if is_shaped {
            Self::assign_array_multidim(&mut target, &dims, value.clone())?;
        } else {
            Self::multi_dim_assign(&mut target, &dims, value.clone())?;
        }
        self.stack.push(value);
        Ok(())
    }

    /// Compute the number of leaf slots for the remaining dimensions.
    /// Each array dimension contributes its length; scalar dimensions contribute 1.
    fn remaining_slot_count(dims: &[Value]) -> usize {
        let mut count = 1usize;
        for d in dims {
            if let Value::Array(items, ..) = d {
                count *= items.len();
            }
        }
        count
    }

    /// Recursively assign a value into a nested array/hash at the given dimension indices.
    fn multi_dim_assign(
        target: &mut Value,
        dims: &[Value],
        value: Value,
    ) -> Result<(), RuntimeError> {
        if dims.is_empty() {
            *target = value;
            return Ok(());
        }

        let dim = &dims[0];
        let rest = &dims[1..];

        // For multi-index dimensions (arrays of keys/indices), need to distribute values
        if let Value::Array(indices, ..) = dim {
            if let Value::Array(values, ..) = &value {
                // Calculate how many leaf slots each index in this dimension needs
                let chunk_size = Self::remaining_slot_count(rest);
                // Distribute values to indices in chunks
                for (i, idx) in indices.iter().enumerate() {
                    let chunk_start = i * chunk_size;
                    let chunk: Vec<Value> = values
                        .iter()
                        .skip(chunk_start)
                        .take(chunk_size)
                        .cloned()
                        .collect();
                    let val = if chunk_size == 1 {
                        chunk.into_iter().next().unwrap_or(Value::Nil)
                    } else {
                        Value::real_array(chunk)
                    };
                    Self::multi_dim_assign_single(target, idx, rest, val)?;
                }
            } else {
                // Single value assigned to multiple indices
                for idx in indices.iter() {
                    Self::multi_dim_assign_single(target, idx, rest, value.clone())?;
                }
            }
            return Ok(());
        }

        // Scalar index/key
        Self::multi_dim_assign_single(target, dim, rest, value)
    }

    /// Assign a value at a single index/key, recursing into remaining dimensions.
    fn multi_dim_assign_single(
        target: &mut Value,
        dim: &Value,
        rest: &[Value],
        value: Value,
    ) -> Result<(), RuntimeError> {
        // Check if the dimension is a string key (for hash access)
        if let Value::Str(key) = dim {
            // Hash assignment
            Self::ensure_hash(target);
            if let Value::Hash(map, ..) = target {
                let map = std::sync::Arc::make_mut(map);
                let entry = map
                    .entry(key.as_str().to_string())
                    .or_insert_with(|| Value::Package(crate::symbol::Symbol::intern("Any")));
                Self::multi_dim_assign(entry, rest, value)?;
            }
            return Ok(());
        }

        // Numeric index (for array access)
        let Some(i) = Self::index_to_usize(dim) else {
            return Err(RuntimeError::new("Invalid index for multi-dim assignment"));
        };

        Self::ensure_array_size(target, i + 1);

        if let Value::Array(items, ..) = target {
            let items = std::sync::Arc::make_mut(items);
            Self::multi_dim_assign(&mut items[i], rest, value)?;
        }

        Ok(())
    }

    /// Ensure the target is a hash, converting from Nil/Any if necessary.
    fn ensure_hash(target: &mut Value) {
        match target {
            Value::Hash(..) => {}
            Value::Nil | Value::Package(..) => {
                *target = Value::Hash(std::sync::Arc::new(std::collections::HashMap::new()));
            }
            _ => {}
        }
    }

    /// Resolve WhateverCode indices for multidim assignment.
    fn resolve_multidim_indices_for_assign(
        &mut self,
        target: &Value,
        indices: &[Value],
    ) -> Result<Vec<Value>, RuntimeError> {
        let mut resolved = Vec::with_capacity(indices.len());
        let mut current = target.clone();
        for idx in indices {
            if matches!(idx, Value::Whatever) {
                // * means "all existing indices" - expand to 0..len
                let len = match &current {
                    Value::Array(items, ..) => items.len(),
                    Value::Hash(..) => {
                        // For hashes, * means all existing keys
                        if let Value::Hash(map, ..) = &current {
                            let keys: Vec<Value> =
                                map.keys().map(|k| Value::str(k.to_string())).collect();
                            let result = Value::real_array(keys);
                            resolved.push(result);
                            continue;
                        }
                        0
                    }
                    _ => 0,
                };
                let all_indices: Vec<Value> = (0..len as i64).map(Value::Int).collect();
                let result = Value::real_array(all_indices);
                // Don't advance current - Whatever applies to all elements
                resolved.push(result);
            } else if let Value::Sub(..) = idx {
                let len = match &current {
                    Value::Array(items, ..) => Value::Int(items.len() as i64),
                    _ => Value::Int(0),
                };
                let result = self
                    .interpreter
                    .call_sub_value(idx.clone(), vec![len], false)?;
                current =
                    Self::index_array_multidim(&current, std::slice::from_ref(&result), false)
                        .unwrap_or(Value::Nil);
                resolved.push(result);
            } else if let Value::Array(items, ..) = idx {
                // Resolve any Sub/WhateverCode elements within the array
                let len = match &current {
                    Value::Array(arr, ..) => Value::Int(arr.len() as i64),
                    _ => Value::Int(0),
                };
                let mut resolved_items = Vec::with_capacity(items.len());
                for item in items.iter() {
                    if let Value::Sub(..) = item {
                        let result = self.interpreter.call_sub_value(
                            item.clone(),
                            vec![len.clone()],
                            false,
                        )?;
                        resolved_items.push(result);
                    } else {
                        resolved_items.push(item.clone());
                    }
                }
                let result = Value::real_array(resolved_items);
                resolved.push(result);
            } else {
                current = Self::index_array_multidim(&current, std::slice::from_ref(idx), false)
                    .unwrap_or(Value::Nil);
                resolved.push(idx.clone());
            }
        }
        Ok(resolved)
    }

    /// Ensure the target is an array with at least `min_size` elements.
    fn ensure_array_size(target: &mut Value, min_size: usize) {
        match target {
            Value::Array(items, ..) => {
                if items.len() < min_size {
                    let items = std::sync::Arc::make_mut(items);
                    items.resize(
                        min_size,
                        Value::Package(crate::symbol::Symbol::intern("Any")),
                    );
                }
            }
            Value::Nil | Value::Package(..) => {
                let mut items = Vec::with_capacity(min_size);
                items.resize(
                    min_size,
                    Value::Package(crate::symbol::Symbol::intern("Any")),
                );
                *target = Value::real_array(items);
            }
            _ => {}
        }
    }
}
