use super::*;

impl Interpreter {
    /// Track C: assign a single hash element (`%h{$k} = $v`) through the shared
    /// cell so concurrent `start { %h{...} = ... }` blocks all land instead of
    /// each mutating a private snapshot (last-writer-wins). Holds the
    /// `shared_vars` lock for the whole get-make_mut-insert so two threads
    /// writing different keys can't lose each other's update — the same
    /// per-element atomicity `push_to_existing_shared_var` gives `.push`.
    ///
    /// Returns `Some(value)` if it wrote through the shared hash, `None` if the
    /// variable is not a shared hash (caller falls back to the normal path).
    pub(crate) fn assign_hash_elem_to_shared_var(
        &mut self,
        key: &str,
        elem_key: String,
        value: Value,
    ) -> Option<Value> {
        if !key.starts_with('%') || !self.shared_vars_active {
            return None;
        }
        // A genuinely-shared plain lexical `%name` is routed through the
        // `__mutsu_atomic_hash::` shared store, the same way concurrent `.push`
        // is (see `shared_array_extend`). Writing the base key directly lets a
        // sibling thread's `set_shared_var` clobber it with a stale empty
        // snapshot during env sync (lost update). The atomic store is exempt
        // from that clobber. Attribute / dynamic / twigil'd hashes (`%!`, `%.`,
        // `%*`) share a name across instances, so they keep the base-key path;
        // a thread-local `my %h` (not present in shared_vars) also falls through
        // to the normal local assignment.
        if Self::is_plain_lexical_name(key) {
            let atomic_key = format!("__mutsu_atomic_hash::{key}");
            let is_shared = {
                let sv = self.shared_vars.read().unwrap();
                sv.contains_key(&atomic_key) || sv.contains_key(key)
            };
            if is_shared {
                return Some(self.shared_hash_elem_set(key, elem_key, value));
            }
        }
        // Only a genuinely-shared base-key hash takes the write-through path; a
        // thread-local `my %h` or an attribute hash (`%!data`, not stored under
        // its own name in shared_vars) must fall through to the normal local
        // assignment — and crucially must NOT have its env copy dropped, which
        // would discard accumulated element writes.
        {
            let sv = self.shared_vars.read().unwrap();
            if !matches!(sv.get(key), Some(Value::Hash(_))) {
                return None;
            }
        }
        let is_thread_clone = self.is_thread_clone();
        if is_thread_clone {
            // Drop env's private copy so the shared cell holds the only Arc and
            // make_mut mutates in place (the thread's env is discarded anyway).
            self.env.remove(key);
        }
        let mut sv = self.shared_vars.write().unwrap();
        let Some(Value::Hash(arc)) = sv.get_mut(key) else {
            return None;
        };
        Value::hash_insert_through(&mut Arc::make_mut(arc).map, elem_key, value.clone());
        let result = Value::Hash(Arc::clone(arc));
        drop(sv);
        if is_thread_clone {
            // Mark dirty once per key (a per-key env marker avoids re-locking the
            // dirty set on every element write).
            let dirty_marker = format!("__mutsu_shared_dirty::{key}");
            if !self.env.contains_key(&dirty_marker) {
                self.mark_shared_var_dirty(key);
                self.env.insert(dirty_marker, Value::Bool(true));
            }
        } else {
            self.mark_shared_var_dirty(key);
            self.env.insert(key.to_string(), result);
        }
        Some(value)
    }

    /// Track C: assign a single array element (`@a[$i] = $v`) through the shared
    /// cell so concurrent `start { @a[...] = ... }` blocks all land instead of
    /// each mutating a private snapshot (last-writer-wins). Mirrors
    /// `assign_hash_elem_to_shared_var`: holds the `shared_vars` lock across the
    /// whole get -> Arc::make_mut -> (grow with Nil) -> set so two threads
    /// writing different indices can't lose each other's update. `idx` must be a
    /// non-negative element index (the caller rejects negatives).
    ///
    /// Returns `Some(value)` if it wrote through the shared array, `None` if the
    /// variable is not a shared array (caller falls back to the normal path).
    pub(crate) fn assign_array_elem_to_shared_var(
        &mut self,
        key: &str,
        idx: usize,
        value: Value,
    ) -> Option<Value> {
        if !key.starts_with('@') || !self.shared_vars_active {
            return None;
        }
        // A genuinely-shared plain lexical `@name` routes through the
        // `__mutsu_atomic_arr::` shared store (same as concurrent `.push`, see
        // `shared_array_extend`) so a sibling thread's stale `set_shared_var`
        // cannot clobber the merged array. Attribute / dynamic / twigil'd arrays
        // keep the base-key path; a thread-local `my @a` (not present in
        // shared_vars) falls through to the normal local assignment.
        if Self::is_plain_lexical_name(key) {
            let atomic_key = format!("__mutsu_atomic_arr::{key}");
            let is_shared = {
                let sv = self.shared_vars.read().unwrap();
                sv.contains_key(&atomic_key) || sv.contains_key(key)
            };
            if is_shared {
                return Some(self.shared_array_elem_set(key, idx, value));
            }
        }
        // See `assign_hash_elem_to_shared_var`: only a genuinely-shared base-key
        // array takes the write-through path; otherwise fall through without
        // dropping the local env copy.
        {
            let sv = self.shared_vars.read().unwrap();
            if !matches!(sv.get(key), Some(Value::Array(..))) {
                return None;
            }
        }
        let is_thread_clone = self.is_thread_clone();
        if is_thread_clone {
            // Drop env's private copy so the shared cell holds the only Arc and
            // make_mut mutates in place (the thread's env is discarded anyway).
            self.env.remove(key);
        }
        let mut sv = self.shared_vars.write().unwrap();
        let Some(Value::Array(arc, kind)) = sv.get_mut(key) else {
            return None;
        };
        let data = Arc::make_mut(arc);
        if idx >= data.items.len() {
            data.items.resize(idx + 1, Value::Nil);
        }
        data.items[idx] = value.clone();
        if *kind == ArrayKind::List {
            *kind = ArrayKind::Array;
        }
        let result = Value::Array(Arc::clone(arc), *kind);
        drop(sv);
        if is_thread_clone {
            let dirty_marker = format!("__mutsu_shared_dirty::{key}");
            if !self.env.contains_key(&dirty_marker) {
                self.mark_shared_var_dirty(key);
                self.env.insert(dirty_marker, Value::Bool(true));
            }
        } else {
            self.mark_shared_var_dirty(key);
            self.env.insert(key.to_string(), result);
        }
        Some(value)
    }

    /// Read a shared variable. If the variable is in shared_vars, return
    /// the shared version (which may have been mutated by another thread).
    #[allow(dead_code)]
    pub(crate) fn get_shared_var(&self, key: &str) -> Option<Value> {
        let sv = self.shared_vars.read().unwrap();
        sv.get(key).cloned()
    }

    /// Returns true if the given key is in the shared_vars_dirty set
    /// (i.e., was modified by an atomic/CAS operation).
    pub(crate) fn is_shared_var_dirty(&self, key: &str) -> bool {
        self.shared_vars_dirty
            .read()
            .map(|d| d.contains(key))
            .unwrap_or(false)
    }

    pub(crate) fn mark_shared_var_dirty(&self, key: &str) {
        if self
            .shared_vars_dirty
            .read()
            .ok()
            .is_some_and(|dirty| dirty.contains(key))
        {
            return;
        }
        if let Ok(mut dirty) = self.shared_vars_dirty.write() {
            dirty.insert(key.to_string());
        }
    }

    /// Write a shared variable. Updates both the local env and shared_vars.
    pub(crate) fn set_shared_var(&mut self, key: &str, value: Value) {
        // Ensure @-variables always store Array(true) (real Arrays)
        let value = if key.starts_with('@') {
            match value {
                // Preserve Shaped arrays; only normalize List to Array.
                Value::Array(items, ArrayKind::List) => Value::Array(items, ArrayKind::Array),
                other => other,
            }
        } else {
            value
        };
        self.env.insert(key.to_string(), value.clone());
        if self.shared_vars_active {
            let mut sv = self.shared_vars.write().unwrap();
            // Skip overwriting @-variables that have an active CAS atomic
            // copy — the atomic copy is the authoritative source of truth
            // and must not be clobbered by stale local snapshots.
            if key.starts_with('@') {
                let atomic_key = format!("__mutsu_atomic_arr::{key}");
                if sv.contains_key(&atomic_key) {
                    return;
                }
            } else if key.starts_with('%') {
                // Symmetric to the array case: a hash with an active atomic
                // entry (concurrent element assignment) must not be clobbered
                // by a stale local snapshot during env sync.
                let atomic_key = format!("__mutsu_atomic_hash::{key}");
                if sv.contains_key(&atomic_key) {
                    return;
                }
            }
            if sv.contains_key(key) {
                sv.insert(key.to_string(), value);
                // Mark this key as explicitly updated so sync_shared_vars_to_env
                // knows to propagate it (vs keys only initialized by clone_for_thread).
                self.mark_shared_var_dirty(key);
            }
        }
    }

    /// Clear atomic array CAS state for a variable. Called when the variable
    /// is genuinely re-declared (e.g., new loop iteration with `my @arr`).
    pub(crate) fn clear_atomic_array_state(&self, key: &str) {
        if !key.starts_with('@') {
            return;
        }
        let atomic_key = format!("__mutsu_atomic_arr::{key}");
        if let Ok(mut sv) = self.shared_vars.write() {
            sv.remove(&atomic_key);
        }
    }

    /// Hash analogue of `clear_atomic_array_state`: drop the
    /// `__mutsu_atomic_hash::` shared-store entry when a `%`-variable is
    /// (re-)declared, so a fresh `my %h` does not seed from a previous lexical's
    /// concurrent element writes (e.g. inside a loop body).
    pub(crate) fn clear_atomic_hash_state(&self, key: &str) {
        if !key.starts_with('%') {
            return;
        }
        let atomic_key = format!("__mutsu_atomic_hash::{key}");
        if let Ok(mut sv) = self.shared_vars.write() {
            sv.remove(&atomic_key);
        }
    }

    /// Sync shared variables back from shared_vars into the local env.
    /// Only syncs keys that were explicitly updated via `set_shared_var`
    /// (tracked in `shared_vars_dirty`), so that function parameters
    /// initialized by `clone_for_thread` are not overwritten with stale values.
    pub(crate) fn sync_shared_vars_to_env(&mut self) {
        // Collect dirty keys first, then drop the lock before acquiring
        // shared_vars lock.  This avoids a lock-ordering deadlock:
        // set_shared_var acquires shared_vars then shared_vars_dirty,
        // so we must not hold shared_vars_dirty while acquiring shared_vars.
        let dirty_keys: Vec<String> = {
            let dirty = self.shared_vars_dirty.read().unwrap();
            dirty.iter().cloned().collect()
        };
        if dirty_keys.is_empty() {
            return;
        }
        let updates: Vec<(String, Value)> = {
            let sv = self.shared_vars.read().unwrap();
            let mut updates = Vec::new();
            for key in &dirty_keys {
                // Atomic ops store the value under an internal shared key, while
                // dirty tracking also marks the user-visible variable name.
                let name_key = format!("__mutsu_atomic_name::{key}");
                let value_key = match sv.get(&name_key).or_else(|| self.env.get(&name_key)) {
                    Some(Value::Str(vk)) => Some(vk.as_ref().clone()),
                    _ => None,
                };
                if let Some(value_key) = value_key
                    && let Some(val) = sv.get(value_key.as_str())
                {
                    updates.push((key.clone(), val.clone()));
                    continue;
                }

                // Check for atomic array CAS storage
                let atomic_arr_key = format!("__mutsu_atomic_arr::{key}");
                if let Some(val) = sv.get(&atomic_arr_key) {
                    updates.push((key.clone(), val.clone()));
                    continue;
                }

                // Check for atomic hash CAS storage
                let atomic_hash_key = format!("__mutsu_atomic_hash::{key}");
                if let Some(val) = sv.get(&atomic_hash_key) {
                    updates.push((key.clone(), val.clone()));
                    continue;
                }

                if let Some(val) = sv.get(key) {
                    updates.push((key.clone(), val.clone()));
                }
            }
            updates
        };
        // Instance attribute CAS updates need no propagation here: cell-CAS
        // mutates the receiver's shared attribute cell in place, so every
        // alias (including the parent thread's) observes the swap directly.
        //
        // Slice 1b (cross-thread cell sharing): a cross-thread update (e.g. a
        // worker `start { cas $seen, … }` whose result is `await`ed) lands in the
        // parent env here, but the parent's matching *local slot* is not refreshed
        // unless the blanket reconcile is on. Record each synced caller-visible
        // name so the await/`.result` call site drains it straight to the caller's
        // slot (`apply_pending_rw_writeback`), dropping the reverse-sync dependency.
        //
        // The synced var's owning slot may live an *unknown number of frames up*:
        // `await` runs `run_pending_instance_destroys()` (DESTROY dispatch, empty
        // `locals`) between the shared-var sync and returning to the top-level
        // frame that owns the slot. A drop-on-miss list (`pending_rw_writeback_sources`)
        // would be consumed and discarded by those intervening DESTROY frames
        // before reaching the owner. Use the retain-on-miss list
        // (`pending_caller_var_writeback`) instead, which carries the source up the
        // frame chain until the frame whose `code` actually has the slot drains it.
        for (key, val) in updates {
            if !self.pending_caller_var_writeback.contains(&key) {
                self.pending_caller_var_writeback.push(key.clone());
            }
            self.env.insert(key, val);
        }
    }

    /// Sync shared vars for a narrow set of captured lexical names.
    /// This is used by hot paths (e.g. Lock::Async.protect) to avoid scanning
    /// large closure environments on every invocation.
    pub(crate) fn sync_shared_vars_for_names<'a, I>(&mut self, names: I)
    where
        I: IntoIterator<Item = &'a str>,
    {
        if !self.shared_vars_active {
            return;
        }
        let sv = self.shared_vars.read().unwrap();
        for name in names {
            if let Some(val) = sv.get(name) {
                if matches!(val, Value::Array(..) | Value::Hash(..)) {
                    self.env.remove(name);
                } else {
                    self.env.insert(name.to_string(), val.clone());
                }
            }
        }
    }

    pub(crate) fn clear_private_zeroarg_method_cache(&mut self) {
        self.private_zeroarg_method_cache.clear();
    }

    pub(crate) fn reset_atomic_var_key(&mut self, name: &str) {
        let name_key = format!("__mutsu_atomic_name::{name}");
        let Some(Value::Str(value_key)) = self.env.remove(&name_key) else {
            return;
        };
        let mut shared = self.shared_vars.write().unwrap();
        shared.remove(value_key.as_str());
        shared.remove(&name_key);
    }

    pub(crate) fn reset_atomic_var_key_decl(&mut self, name: &str) {
        let name_key = format!("__mutsu_atomic_name::{name}");
        self.env.remove(&name_key);
        let mut shared = self.shared_vars.write().unwrap();
        if let Some(Value::Str(value_key)) = shared.remove(&name_key) {
            shared.remove(value_key.as_str());
        }
    }

    pub(crate) fn merge_sigilless_alias_writes(&self, saved_env: &mut Env, current_env: &Env) {
        for (key, alias) in current_env.iter() {
            if !key.starts_with("__mutsu_sigilless_alias::") {
                continue;
            }
            if !key.starts_with("__mutsu_sigilless_alias::!") {
                continue;
            }
            let Value::Str(alias_name) = alias else {
                continue;
            };
            if let Some(bare) = alias_name
                .strip_prefix('$')
                .or_else(|| alias_name.strip_prefix('@'))
                .or_else(|| alias_name.strip_prefix('%'))
                .or_else(|| alias_name.strip_prefix('&'))
                && let Some(value) = current_env.get(bare).cloned()
            {
                saved_env.insert(alias_name.to_string(), value.clone());
                saved_env.insert(bare.to_string(), value);
                continue;
            }
            saved_env.insert_sym(*key, alias.clone());
            if let Some(value) = current_env.get(alias_name.as_str()).cloned() {
                saved_env.insert(alias_name.to_string(), value);
                continue;
            }
            if let Some(bare_name) = key.strip_prefix_str("__mutsu_sigilless_alias::")
                && let Some(value) = current_env.get(&bare_name).cloned()
            {
                saved_env.insert(bare_name.to_string(), value.clone());
                saved_env.insert(alias_name.to_string(), value);
            }
        }
        for (key, value) in current_env.iter() {
            if key.starts_with("__mutsu_predictive_seq_iter::")
                || key.starts_with("__mutsu_sigilless_alias::!")
            {
                saved_env.insert_sym(*key, value.clone());
            }
        }
    }
}
