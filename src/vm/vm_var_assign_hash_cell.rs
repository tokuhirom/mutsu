//! The element-store lane for a hash a routine reaches through a captured
//! cell: `%memo{$k} = $v` in a `my sub` (or closure) whose `%memo` is a free
//! variable of the enclosing routine (#18055).
//!
//! Capturing `%memo` boxes it into a shared `ContainerRef` cell (the lexsub
//! alias / ADR-0024 capture), and the capturing routine sees the variable
//! only as that cell under the name. The plain-hash lanes
//! ([`Interpreter::try_fast_hash_element_assign_early`] and the lane at the
//! bottom of the named store) look for a bare `Hash` in env and decline a
//! cell. The store then took the general path: the cell's hash was seeded
//! into env, the full store cascade ran on it, and the result was written back
//! through the cell, ~27k instructions per store against ~1.8k for a local
//! hash. MCP::Server::Tool::Web's tokenizer memo runs this store once per
//! search.
//!
//! The cell *is* the variable's container, and its hash node is the store's
//! target. So when that node is a plain hash held by the cell alone, the
//! store is an insert into it, made in place under the cell's lock. Every
//! shape the general path treats differently declines (`None`, nothing
//! touched):
//! - a typed, defaulted, readonly or `Map` container;
//! - a bound or mediating element;
//! - a node shared with another holder, whose copy-on-write the full path
//!   handles;
//! - cross-thread sharing.

use super::*;

impl Interpreter {
    /// `%name{$key} = $value` where `%name` is reached through a captured
    /// cell; see the module comment. `None` declines with nothing consumed.
    // Cost: O(1) expected (a fixed number of env probes and one hash insert).
    pub(crate) fn try_fast_captured_hash_element_assign(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        is_positional: bool,
        target_slot: Option<u32>,
    ) -> Option<Result<(), RuntimeError>> {
        if is_positional {
            return None;
        }
        let stack_len = self.stack.len();
        if stack_len < 2 {
            return None;
        }
        if !matches!(
            self.stack[stack_len - 1].view(),
            ValueView::Str(_) | ValueView::Int(_)
        ) {
            return None;
        }
        // The value: what a plain element store writes as is (itemized below
        // for an aggregate), never `Nil` (it decays to the element's default)
        // or a `:=` bind marker.
        let val_ref = &self.stack[stack_len - 2];
        if !val_ref.is_plain_scalar_store_payload() || Self::is_bind_index_value(val_ref) {
            return None;
        }
        // Cross-thread stores have their own name-keyed lanes and guards.
        if self.threads.shared_vars_active
            || crate::value::container_lock::multi_mutator_threads_live()
            || !self.local_bind_pairs.is_empty()
        {
            return None;
        }
        let var_name = Self::const_str(code, name_idx);
        let var_sym = code.const_sym(name_idx);
        // A plain `%` lexical: not a dynamic or twigilled name.
        let bytes = var_name.as_bytes();
        if bytes.first() != Some(&b'%') || matches!(bytes.get(1), Some(b'*' | b'!' | b'.' | b'?')) {
            return None;
        }
        // The cell the full store would seed its target from, resolved the
        // same way (`exec_index_assign_expr_named_op`): a routine-nested sub's
        // alias of its free variable or a compunit file-scope cell first, then
        // a closure capture this frame reaches by name only. The owning
        // frame's own slot-held cell is the full path's.
        let mut owner_slots: Vec<(usize, Value)> = Vec::new();
        let cell = self
            .unit_lexical_container_cell_at(var_name, target_slot)
            .or_else(|| self.capture_cell_for_element_store(code, var_sym, &mut owner_slots))?;
        if !owner_slots.is_empty() {
            return None;
        }
        if cell.is_readonly()
            || self.var_default(var_name).is_some()
            || self.is_readonly_sym(var_sym)
        {
            return None;
        }
        let key = self.stack[stack_len - 1].to_string_value();
        let committed = {
            let mut held = cell.lock().ok()?;
            if self.has_container_type_metadata(&held) {
                return None;
            }
            let ValueView::Hash(hash) = held.view() else {
                return None;
            };
            // Held by the cell alone, so the insert below mutates it in place
            // and no other holder can observe it changing. A plain hash
            // without element-level metadata (`my %h{Any}` object keys, a
            // typed or `Map` container) or a bound element is the full path's.
            if crate::gc::Gc::strong_count_of(&hash) != 1
                || hash.has_type_meta()
                || hash.declared_type.is_some()
                || hash.get(&key).is_some_and(|existing| {
                    matches!(
                        existing.view(),
                        ValueView::HashEntryRef { .. }
                            | ValueView::Scalar(..)
                            | ValueView::ContainerRef(_)
                            | ValueView::Proxy { .. }
                            | ValueView::VarRef { .. }
                    ) || matches!(existing.view(),
                        ValueView::Pair(name, _) if name.starts_with("__mutsu_bound"))
                })
            {
                return None;
            }
            // -- committed: from here the store cannot fall back --
            self.stack.pop();
            let val = self.stack.pop().unwrap_or(Value::NIL);
            let stored = Self::itemize_value_for_element_store(val.clone());
            held.with_hash_mut(|hash| {
                Value::hash_insert_through(&mut crate::gc::Gc::make_mut(hash).map, key, stored);
            });
            val
        };
        // As the plain-hash lane: a single key names one scalar slot, so the
        // expression's value is the itemized rvalue.
        self.stack.push(Self::itemize_value(committed));
        Some(Ok(()))
    }
}
