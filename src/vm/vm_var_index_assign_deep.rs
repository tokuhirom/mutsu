use super::*;

impl Interpreter {
    /// Deep nested index assignment (3+ levels): @a[i][j][k]... = val
    /// Stack order: [value, idx_outermost, ..., idx_innermost] (innermost on top).
    /// positional_flags_idx is a constant index holding an array of booleans
    /// (innermost to outermost) indicating whether each subscript is positional.
    pub(super) fn exec_index_assign_deep_nested_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        depth: u32,
        positional_flags_idx: u32,
    ) -> Result<(), RuntimeError> {
        let var_name = Self::const_str(code, name_idx).to_string();
        let native_fill = {
            let tc = loan_env!(self, var_type_constraint(&var_name));
            Self::native_fill_for_constraint(tc.as_deref())
        };
        let depth = depth as usize;

        // Pop indices from stack: innermost first (top of stack)
        let mut indices_val: Vec<Value> = Vec::with_capacity(depth);
        for _ in 0..depth {
            indices_val.push(self.stack.pop().unwrap_or(Value::Nil));
        }
        // indices_val[0] = innermost, indices_val[depth-1] = outermost

        let raw_val_for_junction = self.stack.pop().unwrap_or(Value::Nil);

        // Junction / slice OUTERMOST subscript (`%h<a><b>{any('p','q')} = v`):
        // autothread per outer key. The rest of this op handles a single scalar
        // outermost key, so re-dispatch for each expanded key (previously the
        // junction/array was stringified into one garbage entry).
        let outermost = &indices_val[depth - 1];
        if !matches!(&raw_val_for_junction, Value::Pair(n, _) if n == "__mutsu_bind_index_value")
            && matches!(outermost, Value::Junction { .. } | Value::Array(..))
        {
            let outer_keys: Vec<Value> = match outermost {
                Value::Junction { values, .. } => values.as_ref().clone(),
                Value::Array(a, ..) => a.items.to_vec(),
                _ => unreachable!(),
            };
            let vals: Vec<Value> = match (outermost, &raw_val_for_junction) {
                (Value::Junction { .. }, Value::Junction { values: jv, .. }) => jv.as_ref().clone(),
                (Value::Junction { .. }, _) => {
                    vec![raw_val_for_junction.clone(); outer_keys.len()]
                }
                _ => self.assignment_rhs_values(&raw_val_for_junction)?,
            };
            for (i, ok) in outer_keys.into_iter().enumerate() {
                let v = vals.get(i).cloned().unwrap_or(Value::Nil);
                // Re-push for recursion. Stack (bottom→top): value, outermost,
                // ..., innermost. Replace the outermost with the scalar key.
                self.stack.push(v);
                self.stack.push(ok);
                for j in (0..depth - 1).rev() {
                    self.stack.push(indices_val[j].clone());
                }
                self.exec_index_assign_deep_nested_op(
                    code,
                    name_idx,
                    depth as u32,
                    positional_flags_idx,
                )?;
                self.stack.pop();
            }
            self.stack.push(raw_val_for_junction);
            return Ok(());
        }

        let indices: Vec<String> = indices_val.iter().map(|v| v.to_string_value()).collect();
        let val = raw_val_for_junction;

        // LHS binding (`$struct[..]<..>[..] := $scalar`): unwrap the bind
        // payload and promote the deep leaf element to a first-class
        // `ContainerRef` cell shared with the source variable (Phase 2 Stage 2,
        // the symmetric form of RHS element binding). Only a plain scalar source
        // is handled here; an indexed element source (`:= $b[j]`, encoded with
        // `\0idx\0`) is a separate slice. A cell survives COW of any enclosing
        // container, so a later write to either side reaches the other — which
        // the old `BOUND_ARRAY_REF_SENTINEL` by-name back-reference lost at depth.
        let (val, bind_source) = Self::unwrap_bind_index_value(val);
        let bind_source = bind_source.filter(|s| !s.contains("\x00idx\x00"));
        let bind_cell: Option<Arc<std::sync::Mutex<Value>>> = bind_source
            .as_ref()
            .map(|_| Arc::new(std::sync::Mutex::new(val.clone())));

        // Extract positional flags from constant
        let flags_val = code.constants[positional_flags_idx as usize].clone();
        let positional_flags: Vec<bool> = if let Value::Array(arr, _) = &flags_val {
            arr.iter().map(|v| matches!(v, Value::Bool(true))).collect()
        } else {
            vec![true; depth]
        };

        // Invalidate local cache for the variable
        if let Some(slot) = self.find_local_slot(code, &var_name)
            && matches!(self.locals[slot], Value::Array(..) | Value::Hash(..))
        {
            self.locals[slot] = Value::Nil;
        }

        // Ensure the root variable exists
        if !self.env().contains_key(&var_name) {
            let init = if var_name.starts_with('@') {
                Value::real_array(Vec::new())
            } else if var_name.starts_with('%') {
                Value::hash(std::collections::HashMap::new())
            } else if positional_flags[0] {
                Value::real_array(Vec::new())
            } else {
                Value::hash(std::collections::HashMap::new())
            };
            self.env_mut().insert(var_name.clone(), init);
        }

        // Walk down the chain of indices, autovivifying containers as needed.
        // We need a mutable reference to the current container at each level.
        // Use raw pointer traversal to avoid borrow checker issues with nested mutation.
        let root: *mut Value = self.env_mut().get_mut(&var_name).unwrap() as *mut Value;

        let mut current: *mut Value = root;

        // Walk through indices[0..depth-1], autovivifying intermediate containers
        for level in 0..depth {
            let key = &indices[level];
            let is_positional = positional_flags[level];

            // Descend through any `:=`-bound container cell at this level so the
            // traversal/assignment reaches the shared, held container.
            current = unsafe { Self::descend_container_ref(current) };

            if level < depth - 1 {
                // Intermediate level: autovivify and descend
                let next_positional = positional_flags[level + 1];
                unsafe {
                    match &mut *current {
                        Value::Array(arr_arc, _) => {
                            if let Ok(i) = key.parse::<usize>() {
                                let arr = Arc::make_mut(arr_arc);
                                Self::autoviv_resize(arr, i + 1, native_fill.clone())?;
                                // Autovivify if needed. A `ContainerRef` is a
                                // `:=`-bound cell that holds (and is descended to)
                                // a container on the next iteration; treating it
                                // as "needs vivify" would clobber the binding.
                                let needs_viv = !matches!(
                                    &arr[i],
                                    Value::Array(..) | Value::Hash(..) | Value::ContainerRef(..)
                                );
                                if needs_viv {
                                    arr[i] = if next_positional {
                                        Value::real_array(Vec::new())
                                    } else {
                                        Value::hash(std::collections::HashMap::new())
                                    };
                                }
                                current = &mut arr[i] as *mut Value;
                            }
                        }
                        Value::Hash(hash_arc) => {
                            let hash = Arc::make_mut(hash_arc);
                            if !hash.contains_key(key.as_str()) {
                                let new_val = if next_positional {
                                    Value::real_array(Vec::new())
                                } else {
                                    Value::hash(std::collections::HashMap::new())
                                };
                                hash.insert(key.clone(), new_val);
                            }
                            current = hash.get_mut(key.as_str()).unwrap() as *mut Value;
                        }
                        _ => {
                            // Autovivify the root itself if needed
                            if is_positional {
                                *current = Value::real_array(Vec::new());
                            } else {
                                *current = Value::hash(std::collections::HashMap::new());
                            }
                            // Retry this level
                            match &mut *current {
                                Value::Array(arr_arc, _) => {
                                    if let Ok(i) = key.parse::<usize>() {
                                        let arr = Arc::make_mut(arr_arc);
                                        Self::autoviv_resize(
                                            arr,
                                            i + 1,
                                            Value::Package(Symbol::intern("Any")),
                                        )?;
                                        arr[i] = if next_positional {
                                            Value::real_array(Vec::new())
                                        } else {
                                            Value::hash(std::collections::HashMap::new())
                                        };
                                        current = &mut arr[i] as *mut Value;
                                    }
                                }
                                Value::Hash(hash_arc) => {
                                    let hash = Arc::make_mut(hash_arc);
                                    let new_val = if next_positional {
                                        Value::real_array(Vec::new())
                                    } else {
                                        Value::hash(std::collections::HashMap::new())
                                    };
                                    hash.insert(key.clone(), new_val);
                                    current = hash.get_mut(key.as_str()).unwrap() as *mut Value;
                                }
                                _ => {}
                            }
                        }
                    }
                }
            } else {
                // Final level: assign the value. When binding, install the
                // shared cell *directly* (replacing any prior leaf), rather than
                // writing through it — a fresh `:=` rebinds the element to the
                // source's cell.
                let leaf_val = bind_cell
                    .as_ref()
                    .map(|c| Value::ContainerRef(c.clone()))
                    .unwrap_or_else(|| val.clone());
                unsafe {
                    match &mut *current {
                        Value::Array(arr_arc, _) => {
                            if let Ok(i) = key.parse::<usize>() {
                                let arr = Arc::make_mut(arr_arc);
                                Self::autoviv_resize(arr, i + 1, native_fill.clone())?;
                                if bind_cell.is_some() {
                                    arr[i] = leaf_val;
                                } else {
                                    Value::assign_element_slot(&mut arr[i], leaf_val);
                                }
                            }
                        }
                        Value::Hash(hash_arc) => {
                            let hash = Arc::make_mut(hash_arc);
                            if bind_cell.is_some() {
                                hash.insert(key.clone(), leaf_val);
                            } else {
                                Value::hash_insert_through(hash, key.clone(), leaf_val);
                            }
                        }
                        _ => {
                            // Autovivify at final level
                            if is_positional {
                                let mut arr = Vec::new();
                                if let Ok(i) = key.parse::<usize>() {
                                    Self::autoviv_resize(&mut arr, i + 1, native_fill.clone())?;
                                    arr[i] = leaf_val;
                                }
                                *current = Value::real_array(arr);
                            } else {
                                let mut h = std::collections::HashMap::new();
                                h.insert(key.clone(), leaf_val);
                                *current = Value::hash(h);
                            }
                        }
                    }
                }
            }
        }

        // Write the shared cell back to the source variable so both sides alias
        // the same container (the symmetric counterpart of the leaf store above).
        if let (Some(src), Some(cell)) = (bind_source, bind_cell) {
            let cell_val = Value::ContainerRef(cell);
            self.set_env_with_main_alias(&src, cell_val.clone());
            self.update_local_if_exists(code, &src, &cell_val);
        }

        // Write the updated root container back into the local slot. The descent
        // above mutated `env`'s root in place (possibly COW-detaching its outer
        // `Arc` via `make_mut`), then the slot was invalidated to `Nil` (above)
        // on the assumption reverse-sync would re-pull it. Without reverse-sync a
        // later read through the locals store would see that stale `Nil` (or an
        // outer `Arc` that no longer reaches the freshly bound cell). Refreshing
        // the local from `env` here makes both stores share the same outer `Arc`
        // spine, so deep `:=` binds and writes stay coherent without a pull.
        if let Some(slot) = self.find_local_slot(code, &var_name)
            && let Some(updated) = self.env().get(&var_name).cloned()
        {
            self.locals[slot] = updated;
        }

        self.stack.push(val);
        Ok(())
    }
}
