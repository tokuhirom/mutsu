use super::*;

impl Interpreter {
    pub(super) fn exec_index_assign_pseudo_stash_named_op(
        &mut self,
        code: &CompiledCode,
        stash_name_idx: u32,
        key_name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let stash_name = Self::const_str(code, stash_name_idx);
        // `PROCESS::<$name> = value` sets the process-level dynamic variable, so
        // a later `$*name` (stored in the env as `*name`) resolves to it. This is
        // how `Rakudo::Internals.REGISTER-DYNAMIC` installs defaults (e.g.
        // DBIish's `$*DBI-DEFS`).
        if stash_name == "PROCESS::" {
            let raw_key = Self::const_str(code, key_name_idx).to_string();
            // Map the sigiled stash key to the env dynamic-var key:
            //   $name → *name, @name → @*name, %name → %*name, name → *name
            let env_key = match raw_key.chars().next() {
                Some('$') => format!("*{}", &raw_key[1..]),
                Some('@') => format!("@*{}", &raw_key[1..]),
                Some('%') => format!("%*{}", &raw_key[1..]),
                _ => format!("*{raw_key}"),
            };
            let val = self.stack.pop().unwrap_or(Value::Nil);
            let val = if env_key.starts_with('@') {
                runtime::coerce_to_array(val)
            } else if env_key.starts_with('%') {
                self.coerce_hash_var_value(&env_key, val)?
            } else {
                Self::normalize_scalar_assignment_value(val)
            };
            self.env_mut().insert(env_key, val.clone());
            // An assignment is an expression: leave the assigned value on the
            // stack so `Rakudo::Internals.REGISTER-DYNAMIC`'s block (whose body is
            // `PROCESS::<$x> = ...`) returns it.
            self.stack.push(val);
            return Ok(());
        }
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

            self.check_readonly_for_modify(&resolved_name)?;
            if let Some(default) = self.var_default(&resolved_name)
                && matches!(val, Value::Nil)
            {
                val = default.clone();
            }
            if resolved_name.starts_with('@') || resolved_name.starts_with('%') {
                val = self.coerce_typed_container_assignment(&resolved_name, val, false)?;
            }
            if let Some(constraint) = loan_env!(self, var_type_constraint(&resolved_name))
                && !resolved_name.starts_with('@')
                && !resolved_name.starts_with('%')
            {
                if matches!(val, Value::Nil) {
                    if constraint != "Mu" {
                        let nominal =
                            loan_env!(self, nominal_type_object_name_for_constraint(&constraint));
                        val = Value::Package(Symbol::intern(&nominal));
                    }
                } else if !self.type_matches_value(&constraint, &val) {
                    return Err(runtime::utils::type_check_assignment_typed_error(
                        &resolved_name,
                        &constraint,
                        &val,
                    ));
                }
                if !matches!(val, Value::Nil | Value::Package(_)) {
                    val = loan_env!(self, try_coerce_value_for_constraint(&constraint, val))?;
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
        outer_positional: bool,
        inner_positional: bool,
    ) -> Result<(), RuntimeError> {
        let var_name = Self::const_str(code, name_idx).to_string();
        let native_fill = {
            let tc = loan_env!(self, var_type_constraint(&var_name));
            Self::native_fill_for_constraint(tc.as_deref())
        };
        let inner_idx = self.stack.pop().unwrap_or(Value::Nil);
        let outer_idx = self.stack.pop().unwrap_or(Value::Nil);
        let raw_val = self.stack.pop().unwrap_or(Value::Nil);
        // Detect bind marker (__mutsu_bind_index_value) and extract the actual value
        let val = match raw_val {
            Value::Pair(ref name, ref payload) if name == "__mutsu_bind_index_value" => {
                match payload.as_ref() {
                    Value::Array(items, ..) if !items.is_empty() => {
                        items.first().cloned().unwrap_or(Value::Nil)
                    }
                    other => other.clone(),
                }
            }
            other => other,
        };

        // Junction / slice outer subscript (`%h<x>{any('p','q')} = v`,
        // `%h<x>{@k} = (...)`): autothread per outer key. The rest of this op
        // handles a single scalar outer key, so re-dispatch for each expanded
        // key (previously the junction/array was stringified into one garbage
        // entry, so a later read of any real key returned Any).
        if matches!(outer_idx, Value::Junction { .. } | Value::Array(..)) {
            let outer_keys: Vec<Value> = match &outer_idx {
                Value::Junction { values, .. } => values.as_ref().clone(),
                Value::Array(a, ..) => a.items.to_vec(),
                _ => unreachable!(),
            };
            // A scalar RHS goes to every junction key; a slice/junction RHS
            // pairs element-wise.
            let vals: Vec<Value> = match (&outer_idx, &val) {
                (Value::Junction { .. }, Value::Junction { values: jv, .. }) => jv.as_ref().clone(),
                (Value::Junction { .. }, _) => vec![val.clone(); outer_keys.len()],
                _ => self.assignment_rhs_values(&val)?,
            };
            for (i, ok) in outer_keys.into_iter().enumerate() {
                let v = vals.get(i).cloned().unwrap_or(Value::Nil);
                // Re-dispatch with a single scalar outer key. Stack order
                // (bottom→top): value, outer_idx, inner_idx.
                self.stack.push(v);
                self.stack.push(ok);
                self.stack.push(inner_idx.clone());
                self.exec_index_assign_expr_nested_op(
                    code,
                    name_idx,
                    outer_positional,
                    inner_positional,
                )?;
                self.stack.pop();
            }
            self.stack.push(val);
            return Ok(());
        }

        let inner_key = inner_idx.to_string_value();
        let outer_key = outer_idx.to_string_value();

        // If the outer hash has a value type constraint, nested autovivification
        // would need to create a Hash value which won't match the constraint
        // (e.g. `my Int %h; %h<z><t> = 3` should die because %h<z> can't be a Hash).
        if var_name.starts_with('%')
            && let Some(constraint) = loan_env!(self, var_type_constraint(&var_name))
        {
            let inner_hash = Value::hash(std::collections::HashMap::new());
            if !loan_env!(self, type_matches_value(&constraint, &inner_hash)) {
                return Err(RuntimeError::new(format!(
                    "Type check failed in assignment to {var_name}; expected {constraint} but got Hash (autovivification)"
                )));
            }
        }

        if !self.env().contains_key(&var_name) {
            // Autovivify the variable as Array if the inner subscript was
            // positional (`[...]`), otherwise as Hash. For sigiled vars
            // (`@x`, `%h`) the sigil already constrains the kind, so this
            // mainly matters for scalar `$x` autoviv.
            let init = if var_name.starts_with('@') {
                Value::real_array(Vec::new())
            } else if var_name.starts_with('%') {
                Value::hash(std::collections::HashMap::new())
            } else if inner_positional {
                Value::real_array(Vec::new())
            } else {
                Value::hash(std::collections::HashMap::new())
            };
            self.env_mut().insert(var_name.clone(), init);
        }

        // Handle Array-as-outer-container (e.g. `@array[42][23] = 17`,
        // `@array[42]<key> = 17`). `inner_key` is the first/closer subscript
        // value (here "42"). `outer_key` is the outermost subscript value
        // (here "23" or "key"). We autovivify the missing entry at
        // `inner_i` based on `outer_positional`.
        // Drop the locals copy first so the Arc refcount is 1 (avoids
        // unnecessary cloning in Arc::make_mut).
        if let Some(slot) = self.find_local_slot(code, &var_name)
            && matches!(self.locals[slot], Value::Array(..))
        {
            self.locals[slot] = Value::Nil;
        }
        if let Some(Value::Array(outer_arr, _kind)) = self.env_root_descended_mut(&var_name)
            && let Ok(inner_i) = inner_key.parse::<usize>()
        {
            let arr = Arc::make_mut(outer_arr);
            Self::autoviv_resize(arr, inner_i + 1, native_fill.clone())?;
            // Autovivify the slot if it's not already a container. A
            // `:=`-bound element is a shared `ContainerRef` cell holding a
            // container — descend through it (below) instead of clobbering it.
            let needs_viv = !matches!(
                &arr[inner_i],
                Value::Array(..) | Value::Hash(..) | Value::ContainerRef(_)
            );
            if needs_viv {
                arr[inner_i] = if outer_positional {
                    Value::real_array(Vec::new())
                } else {
                    Value::hash(std::collections::HashMap::new())
                };
            }
            match &mut arr[inner_i] {
                // A bound element holds a shared cell: write through it so the
                // mutation reaches the aliased container (`@h[i] := @inner;
                // @h[i][j] = v` updates `@inner[j]`).
                Value::ContainerRef(_) => {
                    Self::assign_into_nested_container(&mut arr[inner_i], &outer_key, val.clone())?;
                }
                Value::Array(inner_arr, _) => {
                    if let Ok(j) = outer_key.parse::<usize>() {
                        // Use interior mutation when the inner array is shared
                        // (e.g., by a `ContainerRef` cell from := binding).
                        if Arc::strong_count(inner_arr) > 1 {
                            // SAFETY: aliased in-place mutation of a shared array
                            // (strong_count > 1); see `arc_contents_mut`.
                            let v = &mut unsafe { crate::value::arc_contents_mut(inner_arr) }.items;
                            Self::autoviv_resize(v, j + 1, Value::Nil)?;
                            Value::assign_element_slot(&mut v[j], val.clone());
                        } else {
                            let inner = Arc::make_mut(inner_arr);
                            Self::autoviv_resize(inner, j + 1, native_fill.clone())?;
                            Value::assign_element_slot(&mut inner[j], val.clone());
                        }
                    }
                }
                Value::Hash(inner_hash) => {
                    if Arc::strong_count(inner_hash) > 1 {
                        // SAFETY: aliased in-place mutation of a shared hash
                        // (strong_count > 1); see `arc_contents_mut`.
                        let hd = unsafe { crate::value::arc_contents_mut(inner_hash) };
                        Value::hash_insert_through(&mut hd.map, outer_key.clone(), val.clone());
                    } else {
                        let h = Arc::make_mut(inner_hash);
                        Value::hash_insert_through(h, outer_key.clone(), val.clone());
                    }
                }
                _ => {}
            }
            if let Some(updated) = self.get_env_with_main_alias(&var_name) {
                self.update_local_if_exists(code, &var_name, &updated);
            }
            self.stack.push(val);
            return Ok(());
        }

        // Hash-based nested assignment (Hash-in-Hash or Hash-containing-Array)
        // Drop the locals copy first so the Arc refcount is 1.
        // This avoids unnecessary cloning in Arc::make_mut which would
        // change the pointer and break .WHICH identity stability.
        if let Some(slot) = self.find_local_slot(code, &var_name) {
            self.locals[slot] = Value::Nil;
        }
        if let Some(Value::Hash(outer_hash)) = self.env_root_descended_mut(&var_name) {
            // At refcount 1, write in place via the raw pointer (same pattern
            // as the bound-cell Array arm below): `Arc::make_mut` relocates a
            // Weak-guarded (metadata-bearing, e.g. object-hash / `is default`)
            // hash even when it is the only strong holder, which would break
            // `.WHICH` pointer identity. When shared, keep the make_mut COW
            // detach. The cast MUST target `HashData` (not its inner map) —
            // see docs/hashdata-migration-plan.md "Latent UB found & fixed".
            let oh: &mut crate::value::HashData = if Arc::strong_count(outer_hash) == 1 {
                // SAFETY: single strong holder; in-place avoids the make_mut
                // relocation that would break `.WHICH` identity. See
                // `arc_contents_mut`.
                unsafe { crate::value::arc_contents_mut(outer_hash) }
            } else {
                Arc::make_mut(outer_hash)
            };
            // Vivify the missing entry as Array if the OUTER (second) subscript
            // is positional (e.g. `%h<key>[42] = ...`), otherwise as Hash.
            let inner_val = oh.entry(inner_key).or_insert_with(|| {
                if outer_positional {
                    Value::real_array(Vec::new())
                } else {
                    Value::hash(std::collections::HashMap::new())
                }
            });
            Self::assign_into_nested_container(inner_val, &outer_key, val.clone())?;
        }
        if let Some(updated) = self.get_env_with_main_alias(&var_name) {
            self.update_local_if_exists(code, &var_name, &updated);
        }
        self.stack.push(val);
        Ok(())
    }

    /// Assign `val` into `target[outer_key]`, descending through any chain of
    /// `:=`-bound container cells (`ContainerRef`). Used by the 2-level nested
    /// assign so that a write to a container-valued bound element
    /// (`%h<key><inner> = ...` where `%h<key>` is a cell) mutates the shared,
    /// held container in place instead of being silently dropped.
    pub(super) fn assign_into_nested_container(
        target: &mut Value,
        outer_key: &str,
        val: Value,
    ) -> Result<(), RuntimeError> {
        match target {
            Value::ContainerRef(cell) => {
                let mut guard = cell.lock().unwrap();
                Self::assign_into_nested_container(&mut guard, outer_key, val)?;
            }
            Value::Array(arr, _) => {
                if let Ok(i) = outer_key.parse::<usize>() {
                    if Arc::strong_count(arr) > 1 {
                        // SAFETY: aliased in-place mutation of a shared array
                        // (strong_count > 1); see `arc_contents_mut`.
                        let v = &mut unsafe { crate::value::arc_contents_mut(arr) }.items;
                        Self::autoviv_resize(v, i + 1, Value::Nil)?;
                        Value::assign_element_slot(&mut v[i], val);
                    } else {
                        let a = Arc::make_mut(arr);
                        Self::autoviv_resize(a, i + 1, Value::Nil)?;
                        Value::assign_element_slot(&mut a[i], val);
                    }
                }
            }
            Value::Hash(h) => {
                Value::hash_insert_through(&mut Arc::make_mut(h).map, outer_key.to_string(), val);
            }
            _ => {}
        }
        Ok(())
    }

    /// Follow `current` through any chain of `:=`-bound container cells
    /// (`ContainerRef`), returning a raw pointer to the innermost held value.
    ///
    /// SAFETY: the pointer derived from the cell's mutex data stays valid after
    /// the transient guard drops because the held `Value` is owned by the `Arc`,
    /// which the caller keeps alive; no aliasing borrow into that `Value` may be
    /// live while the returned pointer is used. (Cross-thread sharing of the same
    /// cell is the tracked, pre-existing gap documented in `aliased_mut.rs`.)
    ///
    /// Cycle-safety (Phase 4): a self-referential bind can make a cell
    /// transitively hold a `ContainerRef` back to itself. A normal cell holds a
    /// Hash/Array (the loop stops immediately); only a cell-holding-cell chain
    /// can loop. Bound the descent depth and stop on overflow so a cyclic bind
    /// terminates instead of hanging.
    pub(super) unsafe fn descend_container_ref(mut current: *mut Value) -> *mut Value {
        const MAX_DESCENT: usize = 256;
        for _ in 0..MAX_DESCENT {
            let cell = match unsafe { &mut *current } {
                Value::ContainerRef(cell) => cell.clone(),
                _ => return current,
            };
            let mut guard = cell.lock().unwrap();
            current = &mut *guard as *mut Value;
        }
        current
    }

    /// Read a root variable for index-assignment, descending through any
    /// `:=`-bound container cell so a `ContainerRef` root resolves to its held
    /// Hash/Array. Without this, a write to `$x[i]`/`$x<k>` where `$x` is itself
    /// a bound cell would fall through every handler's `Value::Hash`/`Value::Array`
    /// match and be silently dropped (Phase 3).
    ///
    /// SAFETY: the returned reference points into the cell's mutex data, kept
    /// alive by the `Arc` stored in env, and no other borrow into that data may
    /// be live while it is held (see `descend_container_ref`).
    pub(crate) fn env_root_descended_mut(&mut self, var_name: &str) -> Option<&mut Value> {
        let root = self.env_mut().get_mut(var_name)? as *mut Value;
        let descended = unsafe { Self::descend_container_ref(root) };
        Some(unsafe { &mut *descended })
    }
}
