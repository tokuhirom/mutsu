use super::*;

impl Interpreter {
    /// Track C: route a simple `%h{$k} = $v` through the shared cell when a
    /// thread is active, so concurrent `start` blocks accumulate into one hash
    /// instead of each mutating a private snapshot (last-writer-wins).
    /// Applies the same simplicity guards as `try_fast_hash_element_assign`
    /// (rejecting type constraints, defaults, bound indices, complex indices).
    /// Returns `Some(Ok)` when it wrote through the shared hash, else `None`.
    pub(crate) fn try_shared_hash_element_assign(
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
            idx_ref.view(),
            ValueView::Array(..)
                | ValueView::Junction { .. }
                | ValueView::GenericRange { .. }
                | ValueView::Range(..)
                | ValueView::RangeExcl(..)
                | ValueView::RangeExclStart(..)
                | ValueView::RangeExclBoth(..)
                | ValueView::Nil
                | ValueView::Seq(..)
                | ValueView::Slip(..)
        ) {
            return None;
        }
        // Reject bind-mode markers and Nil values (need default/type handling).
        if matches!(val_ref.view(), ValueView::Pair(name, _) if name == "__mutsu_bind_index_value")
            || matches!(val_ref.view(), ValueView::Nil)
        {
            return None;
        }
        // Reject when type/key constraints, defaults, readonly, or bound indices
        // exist — those need the full assignment path's healing. ADR-0042
        // slice 1: the type/key-constraint check reads the target hash's own
        // embedded metadata (`container_type_metadata`, true iff `value_type`
        // OR `key_type` OR `declared_type` is set — a key-only object hash
        // like `my %h{Int}` has an empty `value_type`, so `element_constraint_for`
        // alone would miss it) instead of the scope-blind name-keyed map.
        // Scoped tightly: `current` must NOT stay alive past this check — it
        // holds a clone of the hash's Arc, and the fast-path commit below
        // (through `assign_hash_elem_to_shared_var`) needs to see this
        // variable's TRUE reference count, not one inflated by our own
        // temporary clone (see the `try_fast_hash_element_assign` comment
        // below, where this exact shape tripped its `strong_count > 2`
        // external-binding guard).
        {
            let current = self.env().get(var_name).cloned().unwrap_or(Value::NIL);
            if self.container_type_metadata(&current).is_some()
                || self.var_default(var_name).is_some()
                || self.is_readonly(var_name)
            {
                return None;
            }
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
        // ADR-0040 slice 1: a real Hash element is a Scalar container, so an
        // aggregate stored into it itemizes (`%h{$k} = [1,2]` -> `$[1, 2]`).
        match loan_env!(
            self,
            assign_hash_elem_to_shared_var(&var_name, key, Self::itemize_value(val.clone()))
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
    pub(crate) fn try_shared_array_element_assign(
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
        let idx = match self.stack[stack_len - 1].view() {
            ValueView::Int(n) if n >= 0 => n as usize,
            _ => return None,
        };
        let val_ref = &self.stack[stack_len - 2];
        if matches!(val_ref.view(), ValueView::Pair(name, _) if name == "__mutsu_bind_index_value")
            || matches!(val_ref.view(), ValueView::Nil)
        {
            return None;
        }
        // Reject typed / defaulted / shaped / readonly / bound arrays — those need
        // the full path's native-fill, hole, and shape handling. ADR-0042
        // slice 1: the type-constraint check reads the target array's own
        // embedded metadata via `element_constraint_for` instead of the
        // scope-blind name-keyed map. Scoped tightly: `current` holds a clone
        // of the array's Arc and must not stay alive into the commit below,
        // which needs the variable's TRUE reference count (see the
        // `try_fast_hash_element_assign` comment for the bug this shape
        // caused when the clone lived too long).
        {
            let current = self.env().get(var_name).cloned().unwrap_or(Value::NIL);
            if self.element_constraint_for(var_name, &current).is_some()
                || self.var_default(var_name).is_some()
                || self.is_readonly(var_name)
            {
                return None;
            }
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
        // ADR-0040 slice 1: same itemize-at-store as the hash twin above.
        match loan_env!(
            self,
            assign_array_elem_to_shared_var(&var_name, idx, Self::itemize_value(val.clone()))
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
    /// - The variable exists in the env as a plain `Hash` value
    /// - No type constraints, no key constraints, no var defaults
    /// - Variable is not readonly (not bound via `:=`)
    /// - No container type metadata on the hash
    pub(crate) fn try_fast_hash_element_assign(
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
            idx_ref.view(),
            ValueView::Array(..)
                | ValueView::Junction { .. }
                | ValueView::GenericRange { .. }
                | ValueView::Range(..)
                | ValueView::RangeExcl(..)
                | ValueView::RangeExclStart(..)
                | ValueView::RangeExclBoth(..)
                | ValueView::Nil
                | ValueView::Seq(..)
                | ValueView::Slip(..)
                // A bare type object key coerces to "" with a string-context
                // warning (or dispatches a user .Str) — handled on the slow path.
                | ValueView::Package(..)
        ) {
            return None;
        }
        // Reject bind-mode marker values
        if matches!(val_ref.view(), ValueView::Pair(name, _) if name == "__mutsu_bind_index_value")
        {
            return None;
        }
        // Reject Nil values (need default/type-object handling)
        if matches!(val_ref.view(), ValueView::Nil) {
            return None;
        }
        // Check that no type constraints, key constraints, or defaults exist.
        // ADR-0042 slice 1: reads the target hash's own embedded metadata
        // (see the `try_shared_hash_element_assign` comment above for why
        // `container_type_metadata` rather than `element_constraint_for`) —
        // the `has_type_meta()` check further below is a second,
        // container-only belt-and-suspenders check on the SAME embedded
        // metadata, kept for its extra strong-count/local-slot bookkeeping.
        //
        // Scoped tightly in its own block: `current` clones the hash's Arc,
        // and the `strong_count` check a few lines below (the "does an
        // external binding exist" heuristic) counts EVERY live Arc clone —
        // including this temporary one, if it were still alive. An
        // unscoped `let current = ...` here made every hash-element
        // assignment whose value's rvalue-itemization is observed by
        // surrounding code (`my @z = (%a<x> = ...)`) see `strong_count == 3`
        // instead of 2, permanently falling off the fast path and losing its
        // itemization (`t/hash-key-single-itemize.t`).
        {
            let current = self.env().get(var_name).cloned().unwrap_or(Value::NIL);
            if self.container_type_metadata(&current).is_some()
                || self.var_default(var_name).is_some()
                || self.is_readonly(var_name)
            {
                return None;
            }
        }
        // Reject if any bound indices exist for this variable
        // (e.g. `%h<a> := $foo` makes element writes propagate to $foo)
        {
            let bound_key = format!("__mutsu_bound_index::{}", var_name);
            if self.env().contains_key(&bound_key) {
                return None;
            }
        }
        // Reject if this key was `:=`-bound to an immutable literal (`%h<i> := 137`):
        // the slow path must throw X::AdHoc / X::Assignment::RO, not overwrite it.
        if crate::env::elem_index_meta_possible() {
            let ro_key = self.stack[stack_len - 1].to_string_value();
            if self.is_ro_index(var_name, &ro_key) {
                return None;
            }
        }
        // Check that the variable exists in env as a plain Hash
        // and that it has no container type metadata
        let env = self.env();
        match env.get(var_name).map(Value::view) {
            Some(ValueView::Hash(hash_arc)) => {
                let strong_count = crate::gc::Gc::strong_count_of(&hash_arc);
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
                    let is_bound = match existing.view() {
                        ValueView::HashEntryRef { .. } | ValueView::Scalar(..) => true,
                        // Slice 2b: a `=`-shared (or `:=`-bound) element holds a
                        // `ContainerRef` cell; reassignment needs the slow path's
                        // replace-vs-write-through guard, not a blind insert.
                        ValueView::ContainerRef(_) => true,
                        ValueView::Pair(name, _) if name.starts_with("__mutsu_bound") => true,
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
                    self.locals[slot] = Value::NIL;
                }
                if let Some(entry) = self.env_mut().get_mut(var_name) {
                    entry.with_hash_mut(|hash| {
                        // ADR-0040 slice 1: itemize the stored value, not the
                        // rvalue pushed below (that push is a pre-existing,
                        // separate scalar-context-itemization concern).
                        Value::hash_insert_through(
                            &mut crate::gc::Gc::make_mut(hash).map,
                            key.clone(),
                            Self::itemize_value(val.clone()),
                        );
                    });
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
                // A single hash-key assignment names one scalar slot, so the
                // rvalue is itemized (`@z = (%h<x> = 1, 2)` => `@z.elems == 1`).
                // The fast path only handles a single scalar key (complex indices
                // are rejected above), so this is always a single-element result.
                self.stack.push(Self::itemize_value(val));
                Some(Ok(()))
            }
            None => {
                // Hash doesn't exist yet — auto-vivify and insert
                let idx = self.stack.pop().unwrap();
                let val = self.stack.pop().unwrap();
                let key = idx.to_string_value();
                let mut map = std::collections::HashMap::new();
                // ADR-0040 slice 1: itemize the stored value.
                map.insert(key.clone(), Self::itemize_value(val.clone()));
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
                self.stack.push(Self::itemize_value(val));
                Some(Ok(()))
            }
            _ => None, // Not a Hash — fall through to slow path
        }
    }

    pub(super) fn exec_index_assign_expr_named_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        is_positional: bool,
        target_slot: Option<u32>,
    ) -> Result<(), RuntimeError> {
        // A variable still holding a DEFERRED vivification token has no
        // container to assign into yet (`my $x := %h<g>; $x[0] = 'x'`); every
        // helper below resolves the token to `Any` and drops the write. Handle
        // it first: walk-create the path and promote the binding to a cell.
        if let Some(result) = self.try_deferred_token_index_assign(code, name_idx, &[is_positional])
        {
            return result;
        }
        // ADR-0039 slice 1: `name_idx` may name a compunit's own file-scope
        // `@`/`%` (its authoritative storage is the cell in `unit_lexicals`,
        // not `env[var_name]` — that key can hold a completely unrelated
        // same-named binding, the loading scope's own `my %items`). Every
        // helper this op delegates to below (`try_shared_hash_element_assign`,
        // `try_shared_array_element_assign`, `try_fast_hash_element_assign`,
        // `exec_index_assign_expr_named_op_inner`) is env-centric and has no
        // idea `unit_lexicals` exists, so a plain `%h{$k} = $v` on a used
        // module's own file-scope hash silently auto-vivified a brand-new,
        // disconnected env entry instead of writing through the real cell
        // (File::Temp's `%roster{$name} = $fh` inside `make-temp`, discovered
        // via the bundled-library battery gate). Mirror
        // `exec_delete_index_named_op`'s identical seed/restore for `:delete`:
        // temporarily seed env with the cell's inner container, run the op,
        // write the mutated result back through the cell, then restore env to
        // whatever it held before.
        let var_name_for_cell = Self::const_str(code, name_idx).to_string();
        let unit_cell = self.unit_lexical_container_cell(&var_name_for_cell);
        let saved_env_entry = unit_cell.as_ref().map(|cell| {
            let saved = self.env().get(&var_name_for_cell).cloned();
            let inner = cell.lock().unwrap().clone();
            self.env_mut().insert(var_name_for_cell.clone(), inner);
            saved
        });
        let result =
            self.exec_index_assign_expr_named_op_seeded(code, name_idx, is_positional, target_slot);
        if let Some(cell) = unit_cell {
            if let Some(mutated) = self.env().get(&var_name_for_cell).cloned() {
                *cell.lock().unwrap() = mutated;
            }
            // `saved_env_entry` is `Some(_)` whenever `unit_cell` is (both
            // guarded by the same `if let Some(cell) = ...` above at seed
            // time), so this `flatten()` recovers the ORIGINAL env entry —
            // `Some(Some(v))` (had one) or `Some(None)` (had none) — not a
            // "did we even seed" flag.
            match saved_env_entry.flatten() {
                Some(v) => {
                    self.env_mut().insert(var_name_for_cell.clone(), v);
                }
                None => {
                    self.env_mut().remove(&var_name_for_cell);
                }
            }
        }
        result
    }

    /// Thin wrapper around [`Self::exec_index_assign_expr_named_op_seeded_inner`]:
    /// a lazy `@`-array must reify a prefix before an element assignment
    /// (`@a[i] = v`) — the assign machinery below needs a materialized
    /// backing Array to write into. Peeking the (not-yet-popped) index off
    /// the stack lets the reify bound itself to exactly the touched element
    /// for the common simple-Int-subscript shape, instead of an unconditional
    /// capped prefix; `restore_lazy_array_slot` afterwards rebuilds a
    /// still-lazy value around the mutated prefix and the SAME live source,
    /// so the array does not lose its infinite tail (L2, bounded reify
    /// follow-up — see docs/lazy-arrays.md).
    fn exec_index_assign_expr_named_op_seeded(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        is_positional: bool,
        target_slot: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let var_name = Self::const_str(code, name_idx).to_string();
        let touched_index = self.stack.last().and_then(|idx| match idx.view() {
            ValueView::Int(n) if n >= 0 => Some(n),
            _ => None,
        });
        let lazy_source = self.reify_lazy_array_slot(&var_name, touched_index)?;
        let result = self.exec_index_assign_expr_named_op_seeded_inner(
            code,
            name_idx,
            is_positional,
            target_slot,
        );
        if let Some(ll) = lazy_source {
            self.restore_lazy_array_slot(code, &var_name, ll);
        }
        result
    }

    fn exec_index_assign_expr_named_op_seeded_inner(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        is_positional: bool,
        target_slot: Option<u32>,
    ) -> Result<(), RuntimeError> {
        // ADR-0040 slice 1: itemize the rvalue BEFORE any of the fast/slow
        // dispatch paths below run, so every element-assign destination (the
        // shared-var fast paths, the plain-hash fast path, and the full slow
        // path in vm_var_assign_index_named.rs) stores the same already-
        // itemized value — a single hook rather than patching each of the
        // dozens of `items_mut()[i] = ...` sites those paths contain.
        // Gated on a plain `Int`/`Str` index — the same "simple single
        // index" shape `elem_share_mark` below already tests — because a
        // complex index (`Array`/`Range`/`Junction`/`Whatever`/`Slip`/...)
        // may be a SLICE assignment (`@a[0,1] = (1,2),(3,4)`), whose
        // per-target element needs its OWN store-time itemization applied
        // deeper in the slow path, not a single itemization of the whole
        // RHS list here.
        {
            let stack_len = self.stack.len();
            if stack_len >= 2
                && matches!(
                    self.stack[stack_len - 1].view(),
                    ValueView::Int(_) | ValueView::Str(_)
                )
            {
                let slot = &mut self.stack[stack_len - 2];
                let old = std::mem::replace(slot, Value::NIL);
                *slot = old.itemize_for_element_store();
            }
        }
        // Slice 2b: `@aoa[i] = @row` / `%h<k> = @row` was compiled as a `:=` bind
        // (so the bind machinery installs a shared `ContainerRef` cell and
        // promotes the source) plus a `MarkElementShare` flag. Capture which
        // element to mark as a `=` value share — a simple Int/Str subscript — so
        // a later non-share reassignment REPLACES the slot instead of writing
        // through the shared cell. Complex subscripts keep pure `:=` semantics.
        let elem_share_mark: Option<(String, String)> = if self.element_share_pending {
            self.element_share_pending = false;
            let var_name = Self::const_str(code, name_idx).to_string();
            self.stack.last().and_then(|idx| match idx.view() {
                ValueView::Int(n) if n >= 0 => Some((var_name, idx.to_string_value())),
                ValueView::Str(_) => Some((var_name, idx.to_string_value())),
                _ => None,
            })
        } else {
            None
        };
        // --- Track C: shared hash/array element assignment across threads ---
        // `%h{$k} = $v` / `@a[$i] = $v` inside a `start` block must write through
        // the shared cell so concurrent threads all land (snapshot semantics
        // otherwise lose updates).
        if let Some(result) = self.try_shared_hash_element_assign(code, name_idx) {
            return result;
        }
        if let Some(result) = self.try_shared_array_element_assign(code, name_idx) {
            return result;
        }
        // --- Fast path for simple hash element assignment ---
        // Handles the common case: %h{$key} = $val with no type constraints,
        // no binding, no special containers. Skips ~16 HashMap lookups.
        if let Some(result) = self.try_fast_hash_element_assign(code, name_idx, is_positional) {
            return result;
        }
        // Save type metadata and container default by pointer BEFORE the
        // inner op runs. Auto-vivification and Arc::make_mut may
        // reconstruct the array Arc, changing the pointer used as the
        // metadata key. Reapply them on the final container so typed-array
        // hole semantics and `is default(...)` are preserved.
        let save_var_name = Self::const_str(code, name_idx).to_string();
        // Hash type metadata (including the object-hash key constraint) is now
        // embedded in `HashData` and travels with the hash across copy-on-write,
        // so the old name-based reconcile healing is no longer needed.
        let saved_type_meta_outer = self
            .env()
            .get(&save_var_name)
            .cloned()
            .and_then(|v| self.container_type_metadata(&v));
        // Guard against stale pointer-keyed defaults (Arc reuse across
        // allocations): only trust the saved default when a name-based
        // var_default is also registered.
        let saved_default_outer = if self.var_default(&save_var_name).is_some() {
            self.env()
                .get(&save_var_name)
                .and_then(|v| self.container_default(v))
        } else {
            None
        };
        // Hash-subclass element assignment temporarily projects the Instance
        // into a plain Hash for the generic hash store below. Keep the original
        // object so the projection can be committed back in place afterwards:
        // replacing the closure's captured `$obj` with that temporary Hash loses
        // both its class identity and the shared scalar container that carries
        // the write back to the caller.
        let saved_hash_subclass_instance = self.env().get(&save_var_name).cloned().and_then(|v| {
            let instance = v.deref_container();
            if let ValueView::Instance { class_name, .. } = instance.view()
                && self.is_container_subclass(&class_name.resolve())
            {
                Some(instance)
            } else {
                None
            }
        });
        let result =
            self.exec_index_assign_expr_named_op_inner(code, name_idx, is_positional, target_slot);
        if result.is_ok()
            && let Some(instance) = saved_hash_subclass_instance
            && let ValueView::Instance { attributes, .. } = instance.view()
            && let Some(ValueView::Hash(hash)) = self.env().get(&save_var_name).map(Value::view)
        {
            attributes.commit_attrs(hash.map.clone().into());
            self.env_mut().insert(save_var_name.clone(), instance);
        }
        // Restore metadata on the post-assignment container when the
        // identity-keyed map lost it OR holds a stale entry. Copy-on-write
        // changes the hash's Arc pointer (the metadata key), and freed pointers
        // get reused by later allocations carrying *different* stale metadata,
        // so a mere `.is_none()` check leaves a reused pointer's wrong entry in
        // place — re-register whenever the current entry differs from the value
        // saved before the assignment. Object-hash element reads (`%h{$int}`)
        // detect their key constraint only through this pointer-keyed metadata
        // (the read op has no variable name to fall back on), so a stale/lost
        // entry silently degrades them to string-keyed lookups returning Nil.
        if let Some(info) = saved_type_meta_outer
            && let Some(container) = self.env().get(&save_var_name).cloned()
            && self.container_type_metadata(&container).as_ref() != Some(&info)
        {
            // Hashes embed metadata in `HashData`, so the re-tagged value must
            // be written back into both env and the fast-path local slot
            // (`tag_container_metadata` returns the same Arc for non-hash
            // containers, whose Arc-pointer side table is updated in place).
            let tagged = self.tag_container_metadata(container, info);
            self.env_mut().insert(save_var_name.clone(), tagged.clone());
            self.locals_set_by_name(code, &save_var_name, tagged);
        }
        if let Some(def) = saved_default_outer
            && let Some(container) = self.env().get(&save_var_name).cloned()
            && self.container_default(&container).is_none()
        {
            let tagged = self.tag_container_default(container, def);
            self.env_mut().insert(save_var_name.clone(), tagged.clone());
            self.locals_set_by_name(code, &save_var_name, tagged);
        }
        // Object-hash original keys are embedded in `HashData` and travel with
        // the hash across copy-on-write, so no pointer migration is needed.
        // Slice 2b: now that the shared cell is installed in the element, record
        // it as a `=` value share so a later non-share reassignment replaces it.
        if result.is_ok()
            && let Some((var_name, encoded)) = elem_share_mark
        {
            self.mark_element_share(&var_name, encoded);
        }
        // Object index-assign (`$obj[i] = v` / `$obj{k} = v` dispatching
        // ASSIGN-POS/ASSIGN-KEY to an Instance or Mixin that does Positional/
        // Associative) writes the mutated object back into `env[var]` but the
        // inner op does not refresh the caller's local slot. The default build's
        // blanket env reconcile carries this; make it a precise slot write-through
        // so the `MUTSU_NO_BLANKET_RECONCILE` single-store path (and the eventual
        // `env_dirty` removal) keeps the slot coherent. Plain Array/Hash element
        // assigns already update the slot via the fast paths and never reach here
        // as an Instance/Mixin, so this only fires for object subscript targets.
        if result.is_ok()
            && matches!(
                self.env().get(&save_var_name).map(Value::view),
                Some(ValueView::Instance { .. }) | Some(ValueView::Mixin(..))
            )
            && let Some(v) = self.env().get(&save_var_name).cloned()
        {
            self.locals_set_by_name(code, &save_var_name, v);
        }
        result
    }
}
