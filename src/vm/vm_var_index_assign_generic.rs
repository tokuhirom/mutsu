use super::vm_var_assign_ops::BindSourceCell;
use super::*;

impl Interpreter {
    /// Generic index assignment on a stack-computed target.
    /// Stack order: target (bottom), index, value (top).
    /// If the target hash has `__callframe_depth`, routes through set_caller_var.
    pub(super) fn exec_index_assign_generic_op(
        &mut self,
        code: &CompiledCode,
    ) -> Result<(), RuntimeError> {
        let raw_val = self.stack.pop().unwrap_or(Value::Nil);
        let idx = self.stack.pop().unwrap_or(Value::Nil);
        let target = self.stack.pop().unwrap_or(Value::Nil);
        let key = idx.to_string_value();

        // Detect bind marker (__mutsu_bind_index_value) and extract the actual value
        let (val, bind_source) = match raw_val {
            Value::Pair(ref name, ref payload) if name == "__mutsu_bind_index_value" => {
                match payload.as_ref() {
                    Value::Array(items, ..) if items.len() >= 2 => {
                        let value = items.first().cloned().unwrap_or(Value::Nil);
                        let source = match items.get(1) {
                            Some(Value::Array(srcs, ..)) => srcs.first().and_then(|s| match s {
                                Value::Str(name) if !name.is_empty() => Some((**name).clone()),
                                _ => None,
                            }),
                            _ => None,
                        };
                        (value, source)
                    }
                    other => (other.clone(), None),
                }
            }
            other => (other, None),
        };

        // Phase 2 Stage 2: a `:=` bind to a stack-computed element target
        // (`f()<k> := $s`, `($ref)[i] := $s`) stores a shared `ContainerRef`
        // cell at the element and writes the same cell back to the source
        // variable, so a later write to either side reaches the other. This is
        // the computed-target analogue of the named-handler cell bind (slice
        // 3/4); the old `HashEntryRef`/array element back-reference by-reference into the env
        // was stale (the alias never propagated). Pre-read before any container
        // borrow. An element source (`:= @b[j]`, encoded with `\0`) is left to a
        // later slice unless it already arrived promoted to a cell.
        let bind_cell: Option<BindSourceCell> = if let Value::ContainerRef(cell) = &val {
            Some((None, cell.clone()))
        } else if let Some(source_name) = &bind_source
            && !source_name.contains('\0')
        {
            match self.env().get(source_name) {
                Some(Value::ContainerRef(cell)) => Some((None, cell.clone())),
                _ => Some((
                    Some(source_name.clone()),
                    Arc::new(std::sync::Mutex::new(val.clone())),
                )),
            }
        } else {
            None
        };

        // Junction / slice key on a stack-computed target
        // (`%h<x>{any('p','q')} = v`, `%h<x>{@k} = (...)`). The named-handler op
        // autothreads these; the generic op (computed target) previously
        // stringified the multi-key index and wrote a single garbage entry, so a
        // later read of any real key returned Any. Only plain assignment is
        // handled here (a multi-key `:=` bind is a separate, exotic path).
        if bind_cell.is_none() && matches!(idx, Value::Junction { .. } | Value::Array(..)) {
            let resolved = match &target {
                Value::HashEntryRef { .. } => target.hash_entry_read(),
                Value::Scalar(inner) => inner.as_ref().clone(),
                other => other.clone(),
            };
            if matches!(resolved, Value::Hash(_) | Value::Array(..)) {
                let (keys, vals): (Vec<Value>, Vec<Value>) = match &idx {
                    Value::Junction { values, .. } => {
                        // A scalar RHS is assigned to every junction key; a
                        // junction RHS pairs element-wise.
                        let vals = match &val {
                            Value::Junction { values: jv, .. } => jv.as_ref().clone(),
                            _ => vec![val.clone(); values.len()],
                        };
                        (values.as_ref().clone(), vals)
                    }
                    Value::Array(keys, ..) => {
                        (keys.items.to_vec(), self.assignment_rhs_values(&val)?)
                    }
                    _ => unreachable!(),
                };
                for (i, k) in keys.iter().enumerate() {
                    let v = vals.get(i).cloned().unwrap_or(Value::Nil);
                    self.assign_into_computed_target(&resolved, k, v)?;
                }
                self.stack.push(val);
                return Ok(());
            }
        }

        match &target {
            Value::Hash(arc) => {
                // Check for callframe .my hash with depth marker
                if let Some(Value::Int(depth)) = arc.get("__callframe_depth") {
                    let depth = *depth as usize;
                    // Strip sigil from key to get bare variable name
                    let bare_name =
                        if key.starts_with('$') || key.starts_with('@') || key.starts_with('%') {
                            &key[1..]
                        } else {
                            &key
                        };
                    loan_env!(self, set_caller_var(bare_name, depth, val.clone()))?;
                    self.stack.push(val);
                    return Ok(());
                }
                // Interior mutation: write into the hash via raw pointer
                // so the change is visible to all holders of the same Arc.
                // In bind mode, store the shared cell at the key; otherwise the
                // plain value.
                let stored = match &bind_cell {
                    Some((_, cell)) => Value::ContainerRef(cell.clone()),
                    None => val.clone(),
                };
                // SAFETY: aliased in-place mutation of a shared hash so the change
                // is visible to all holders of the same Arc; see `arc_contents_mut`.
                let hd = unsafe { crate::value::arc_contents_mut(arc) };
                Value::hash_insert_through(&mut hd.map, key.clone(), stored);
                // For a fresh-cell bind, write the cell back to the source var
                // so both sides alias the same container.
                if let Some((Some(src), cell)) = &bind_cell {
                    let cell_val = Value::ContainerRef(cell.clone());
                    self.set_env_with_main_alias(src, cell_val.clone());
                    self.update_local_if_exists(code, src, &cell_val);
                }
                self.stack.push(val);
            }
            Value::Array(arc, _kind) => {
                // Interior mutation: write into the array via raw pointer
                // so the change is visible to all holders of the same Arc.
                if let Ok(i) = key.parse::<usize>() {
                    // SAFETY: aliased in-place mutation of a shared array so the
                    // change is visible to all holders of the same Arc; see
                    // `arc_contents_mut`.
                    let v = &mut unsafe { crate::value::arc_contents_mut(arc) }.items;
                    Self::autoviv_resize(v, i + 1, Value::Nil)?;
                    match &bind_cell {
                        // Bind mode installs the shared cell at the element;
                        // the same cell is written back to the source var
                        // below so both sides alias.
                        Some((_, cell)) => v[i] = Value::ContainerRef(cell.clone()),
                        // An element source (`:= @b[j]`) not promoted to a
                        // cell installs the payload directly (rare; left to a
                        // later slice).
                        None if bind_source.is_some() => v[i] = val.clone(),
                        None => {
                            // Write THROUGH an existing `:=`-bound cell so an
                            // assignment reached via a stack target (e.g.
                            // `get()<subkey>[1] = …`) updates the shared cell
                            // instead of clobbering it (nested.t 11-12).
                            Value::assign_element_slot(&mut v[i], val.clone());
                        }
                    }
                    // For a fresh-cell bind, write the cell back to the source var.
                    if let Some((Some(src), cell)) = &bind_cell {
                        let cell_val = Value::ContainerRef(cell.clone());
                        self.set_env_with_main_alias(src, cell_val.clone());
                        self.update_local_if_exists(code, src, &cell_val);
                    }
                }
                self.stack.push(val);
            }
            Value::HashEntryRef { .. } => {
                // Resolve the HashEntryRef and assign into the resolved container.
                let resolved = target.hash_entry_read();
                self.stack.push(resolved);
                self.stack.push(idx);
                self.stack.push(val);
                return self.exec_index_assign_generic_op(code);
            }
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Stash" => {
                let package = attributes
                    .as_map()
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
                    self.env_mut().insert(fq, val.clone());
                }
                self.stack.push(val);
            }
            _ => {
                self.stack.push(val);
            }
        }
        Ok(())
    }
}
