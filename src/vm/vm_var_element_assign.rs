use super::*;

impl Interpreter {
    /// Track C: route a simple `%h{$k} = $v` through the shared cell when a
    /// thread is active, so concurrent `start` blocks accumulate into one hash
    /// instead of each mutating a private snapshot (last-writer-wins).
    /// Applies the same simplicity guards as `try_fast_hash_element_assign`
    /// (rejecting type constraints, defaults, bound indices, complex indices).
    /// Returns `Some(Ok)` when it wrote through the shared hash, else `None`.
    pub(super) fn try_shared_hash_element_assign(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Option<Result<(), RuntimeError>> {
        // Cheap early-out: only meaningful while a thread shares this env.
        if !self.shared_vars_active {
            return None;
        }
        if !self.local_bind_pairs.is_empty() {
            return None;
        }
        let var_name = Self::const_str(code, name_idx);
        if !var_name.starts_with('%') {
            return None;
        }
        let stack_len = self.stack.len();
        if stack_len < 2 {
            return None;
        }
        let idx_ref = &self.stack[stack_len - 1];
        let val_ref = &self.stack[stack_len - 2];
        // Reject complex index types (slices/ranges/junctions need the full path).
        if matches!(
            idx_ref,
            Value::Array(..)
                | Value::Junction { .. }
                | Value::GenericRange { .. }
                | Value::Range(..)
                | Value::RangeExcl(..)
                | Value::RangeExclStart(..)
                | Value::RangeExclBoth(..)
                | Value::Nil
                | Value::Seq(..)
                | Value::Slip(..)
        ) {
            return None;
        }
        // Reject bind-mode markers and Nil values (need default/type handling).
        if matches!(val_ref, Value::Pair(name, _) if name == "__mutsu_bind_index_value")
            || matches!(val_ref, Value::Nil)
        {
            return None;
        }
        // Reject when type/key constraints, defaults, readonly, or bound indices
        // exist — those need the full assignment path's healing.
        if self.var_type_constraint_fast(var_name).is_some()
            || self.var_default(var_name).is_some()
            || self.var_hash_key_constraint_fast(var_name)
            || self.readonly_vars().contains(var_name)
        {
            return None;
        }
        {
            let bound_key = format!("__mutsu_bound_index::{}", var_name);
            if self.env().contains_key(&bound_key) {
                return None;
            }
        }
        let var_name = var_name.to_string();
        let key = idx_ref.to_string_value();
        // Commit: pop idx then val and write through the shared cell.
        let idx = self.stack.pop().unwrap();
        let val = self.stack.pop().unwrap();
        match loan_env!(
            self,
            assign_hash_elem_to_shared_var(&var_name, key, val.clone())
        ) {
            Some(_) => {
                self.stack.push(val);
                Some(Ok(()))
            }
            None => {
                // Not a shared hash after all (e.g. not yet seeded): restore the
                // [val, idx] stack order and fall through to the normal path.
                self.stack.push(val);
                self.stack.push(idx);
                None
            }
        }
    }

    /// Track C: route a simple `@a[$i] = $v` through the shared cell when a
    /// thread is active, so concurrent `start` blocks accumulate into one array
    /// instead of each mutating a private snapshot (last-writer-wins). Handles
    /// only the simple case: a plain non-negative integer index, a plain value,
    /// and no type constraints / defaults / shaped dims / bound indices. Returns
    /// `Some(Ok)` when it wrote through the shared array, else `None`.
    pub(super) fn try_shared_array_element_assign(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Option<Result<(), RuntimeError>> {
        if !self.shared_vars_active {
            return None;
        }
        if !self.local_bind_pairs.is_empty() {
            return None;
        }
        let var_name = Self::const_str(code, name_idx);
        if !var_name.starts_with('@') {
            return None;
        }
        let stack_len = self.stack.len();
        if stack_len < 2 {
            return None;
        }
        // Only a plain non-negative Int index; anything else (slice, Whatever,
        // negative, lazy) needs the full index-assign path.
        let idx = match &self.stack[stack_len - 1] {
            Value::Int(n) if *n >= 0 => *n as usize,
            _ => return None,
        };
        let val_ref = &self.stack[stack_len - 2];
        if matches!(val_ref, Value::Pair(name, _) if name == "__mutsu_bind_index_value")
            || matches!(val_ref, Value::Nil)
        {
            return None;
        }
        // Reject typed / defaulted / shaped / readonly / bound arrays — those need
        // the full path's native-fill, hole, and shape handling.
        if self.var_type_constraint_fast(var_name).is_some()
            || self.var_default(var_name).is_some()
            || self.readonly_vars().contains(var_name)
        {
            return None;
        }
        {
            let shaped_key = format!("__mutsu_shaped_array_dims::{}", var_name);
            let bound_key = format!("__mutsu_bound_index::{}", var_name);
            if self.env().contains_key(&shaped_key) || self.env().contains_key(&bound_key) {
                return None;
            }
        }
        let var_name = var_name.to_string();
        // Commit: pop idx then val and write through the shared cell.
        let idx_val = self.stack.pop().unwrap();
        let val = self.stack.pop().unwrap();
        match loan_env!(
            self,
            assign_array_elem_to_shared_var(&var_name, idx, val.clone())
        ) {
            Some(_) => {
                self.stack.push(val);
                Some(Ok(()))
            }
            None => {
                // Not a shared array (e.g. not yet seeded): restore [val, idx]
                // stack order and fall through to the normal path.
                self.stack.push(val);
                self.stack.push(idx_val);
                None
            }
        }
    }

    /// Fast path for simple hash element assignment: `%h{$key} = $val`.
    ///
    /// Returns `Some(Ok(()))` if the fast path handled the assignment,
    /// `None` if the caller should fall through to the full slow path.
    /// The fast path never returns `Some(Err(...))` — any edge case that
    /// might error falls through to the slow path instead.
    ///
    /// Preconditions checked (all must hold for the fast path to fire):
    /// - Variable name starts with `%` (hash sigil)
    /// - Stack top two values are a simple index (not Array/Junction/GenericRange/Nil)
    ///   and a simple value (not a bind-mode marker)
    /// - The variable exists in the env as `Value::Hash`
    /// - No type constraints, no key constraints, no var defaults
    /// - Variable is not readonly (not bound via `:=`)
    /// - No container type metadata on the hash
    pub(super) fn try_fast_hash_element_assign(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        _is_positional: bool,
    ) -> Option<Result<(), RuntimeError>> {
        // Reject if there are any local bind pairs (`:=` bindings in scope)
        if !self.local_bind_pairs.is_empty() {
            return None;
        }
        let var_name = Self::const_str(code, name_idx);
        // Only handle %-sigiled hash variables
        if !var_name.starts_with('%') {
            return None;
        }
        // Peek at stack to check for bind-mode marker and complex indices
        // without popping (we'll pop only if we commit to the fast path)
        let stack_len = self.stack.len();
        if stack_len < 2 {
            return None;
        }
        // idx is on top of stack, val is below it
        let idx_ref = &self.stack[stack_len - 1];
        let val_ref = &self.stack[stack_len - 2];
        // Reject complex index types that need special handling
        if matches!(
            idx_ref,
            Value::Array(..)
                | Value::Junction { .. }
                | Value::GenericRange { .. }
                | Value::Range(..)
                | Value::RangeExcl(..)
                | Value::RangeExclStart(..)
                | Value::RangeExclBoth(..)
                | Value::Nil
                | Value::Seq(..)
                | Value::Slip(..)
        ) {
            return None;
        }
        // Reject bind-mode marker values
        if matches!(val_ref, Value::Pair(name, _) if name == "__mutsu_bind_index_value") {
            return None;
        }
        // Reject Nil values (need default/type-object handling)
        if matches!(val_ref, Value::Nil) {
            return None;
        }
        // Check that no type constraints, key constraints, or defaults exist
        // Use fast lookups that avoid format! allocations
        if self.var_type_constraint_fast(var_name).is_some()
            || self.var_default(var_name).is_some()
            || self.var_hash_key_constraint_fast(var_name)
            || self.readonly_vars().contains(var_name)
        {
            return None;
        }
        // Reject if any bound indices exist for this variable
        // (e.g. `%h<a> := $foo` makes element writes propagate to $foo)
        {
            let bound_key = format!("__mutsu_bound_index::{}", var_name);
            if self.env().contains_key(&bound_key) {
                return None;
            }
        }
        // Check that the variable exists in env as a plain Hash
        // and that it has no container type metadata
        let env = self.env();
        match env.get(var_name) {
            Some(Value::Hash(hash_arc)) => {
                let strong_count = Arc::strong_count(hash_arc);
                // Reject if the hash Arc has more than 2 refs (e.g. HashEntryRef binding)
                // strong_count == 1: only env holds it (no local slot)
                // strong_count == 2: env + locals hold it (common case in for loops)
                // strong_count > 2: external binding exists, fall through to slow path
                if strong_count > 2 {
                    return None;
                }
                let local_slot = if strong_count == 2 {
                    // The extra ref should be from locals — verify
                    match self.find_local_slot(code, var_name) {
                        Some(slot) => Some(slot),
                        None => return None,
                    }
                } else {
                    None
                };
                // Reject if there's container type metadata
                if hash_arc.has_type_meta() {
                    return None;
                }
                // Peek at the key to check if the existing element is a bound ref
                let peek_key = self.stack[stack_len - 1].to_string_value();
                if let Some(existing) = hash_arc.get(&peek_key) {
                    let is_bound = match existing {
                        Value::HashEntryRef { .. } | Value::Scalar(..) => true,
                        // Slice 2b: a `=`-shared (or `:=`-bound) element holds a
                        // `ContainerRef` cell; reassignment needs the slow path's
                        // replace-vs-write-through guard, not a blind insert.
                        Value::ContainerRef(_) => true,
                        Value::Pair(name, _) if name.starts_with("__mutsu_bound") => true,
                        _ => false,
                    };
                    if is_bound {
                        return None;
                    }
                }
                // All checks passed — commit to fast path
                let idx = self.stack.pop().unwrap();
                let val = self.stack.pop().unwrap();
                let key = idx.to_string_value();
                // When locals and env share the same Arc (strong_count == 2),
                // drop the local ref first so Arc::make_mut can mutate in-place
                // instead of cloning the entire HashMap (O(n) → O(1) per insert).
                if let Some(slot) = local_slot {
                    self.locals[slot] = Value::Nil;
                }
                if let Some(Value::Hash(hash)) = self.env_mut().get_mut(var_name) {
                    Value::hash_insert_through(
                        &mut Arc::make_mut(hash).map,
                        key.clone(),
                        val.clone(),
                    );
                }
                // Restore the local slot to point to the (now mutated) env Arc
                if let Some(slot) = local_slot
                    && let Some(env_val) = self.env().get(var_name).cloned()
                {
                    self.locals[slot] = env_val;
                }
                // strong_count==1 divergence repair: a re-entrant call evaluated
                // as the RHS (e.g. a `proto {*}` redispatch) can swap `self.env`
                // out from under the block's local slot via
                // `restore_env_preserving_existing`, leaving the slot pointing at
                // a stale, detached Arc while env holds the live one (strong_count
                // drops to 1). The assign above mutated only env, so a local slot
                // that still exists is — by definition of strong_count==1 — a
                // diverged copy. Mirror the live env value back to it to keep the
                // dual store coherent, so a later `state`-var persist (which reads
                // env first, then `sync_env_from_locals` flushes the slot) does not
                // clobber the value with the stale slot. No-op for a genuine
                // env-only hash (e.g. `%*ENV`) that has no local slot, and the
                // default build's blanket reconcile makes it redundant (byte-
                // identical) — it only matters on the single-store path.
                if local_slot.is_none()
                    && let Some(slot) = self.find_local_slot(code, var_name)
                    && let Some(env_val) = self.env().get(var_name).cloned()
                {
                    self.locals[slot] = env_val;
                }
                // Sync OS environment when %*ENV is modified
                #[cfg(not(target_family = "wasm"))]
                if var_name == "%*ENV" {
                    // SAFETY: std::env::set_var is unsafe because mutating the
                    // process environment races with any concurrent env access
                    // on another thread. mutsu writes %*ENV from the executing
                    // thread during normal evaluation; a spawned worker that
                    // concurrently reads env would be a latent race (tracked
                    // with the cross-thread container work, see aliased_mut.rs).
                    unsafe {
                        std::env::set_var(&key, val.to_string_value());
                    }
                    // Sync $*HOME when %*ENV<HOME> changes
                    if key == "HOME" {
                        let home_str = val.to_string_value();
                        let home_val = self.make_io_path_instance(&home_str);
                        self.env_mut()
                            .insert("$*HOME".to_string(), home_val.clone());
                        self.env_mut().insert("*HOME".to_string(), home_val);
                    }
                }
                self.stack.push(val);
                Some(Ok(()))
            }
            None => {
                // Hash doesn't exist yet — auto-vivify and insert
                let idx = self.stack.pop().unwrap();
                let val = self.stack.pop().unwrap();
                let key = idx.to_string_value();
                let mut map = std::collections::HashMap::new();
                map.insert(key.clone(), val.clone());
                self.env_mut()
                    .insert(var_name.to_string(), Value::hash(map));
                // Sync OS environment when %*ENV is modified
                #[cfg(not(target_family = "wasm"))]
                if var_name == "%*ENV" {
                    // SAFETY: std::env::set_var is unsafe because mutating the
                    // process environment races with any concurrent env access
                    // on another thread. mutsu writes %*ENV from the executing
                    // thread during normal evaluation; a spawned worker that
                    // concurrently reads env would be a latent race (tracked
                    // with the cross-thread container work, see aliased_mut.rs).
                    unsafe {
                        std::env::set_var(&key, val.to_string_value());
                    }
                    if key == "HOME" {
                        let home_str = val.to_string_value();
                        let home_val = self.make_io_path_instance(&home_str);
                        self.env_mut()
                            .insert("$*HOME".to_string(), home_val.clone());
                        self.env_mut().insert("*HOME".to_string(), home_val);
                    }
                }
                self.stack.push(val);
                Some(Ok(()))
            }
            _ => None, // Not a Hash — fall through to slow path
        }
    }
}
