//! The env-free declaration lane (#12151): a plain `my $x = <scalar>` /
//! `my int $x = <int>` whose slot is the variable's ONLY home.
//!
//! A declaration runs four to five ops (`SetVarDynamic`, `SetVarType*`,
//! `TypeCheck`, `SetLocalDecl`), and each of them used to maintain the
//! name-keyed half of the dual store (env default, type seed, per-scope saved
//! env, `loop_local_vars`, `block_declared_vars`) and then run the generic
//! `exec_set_local_op_inner` cascade, ~10k instructions per declaration on
//! the 12x-rakudo typed-loop benchmark. For a slot the compiler proved
//! `!needs_env_sync` nothing reads that half: no closure captures the name, no
//! `GetGlobal`/`SetGlobal`, no reflective lookup. The slot is authoritative, so
//! the declaration reduces to writing it.
//!
//! [`Interpreter::env_free_decl_slot`] is the one predicate; every op that takes
//! the lane asks it with its own operands, so the ops cannot disagree about
//! which declarations are lean.
use super::*;

impl Interpreter {
    /// The slot a plain scalar `my` declaration owns outright, or `None` when
    /// the declaration must keep its name-keyed bookkeeping.
    ///
    /// Everything here is either a per-slot compile-time fact of the chunk
    /// (`needs_env_sync`, `dup_named_locals`, the `needs_cell_*` lists) or a
    /// monotonic "has this program ever done X" latch, so a program that does
    /// none of X answers it with a handful of loads.
    // Cost: O(1), c = entries of the chunk's few per-slot capture lists (each
    // empty in a program with no closure over that name).
    #[inline]
    pub(super) fn env_free_decl_slot(
        &self,
        code: &CompiledCode,
        name: &str,
        dynamic: bool,
        bind_declaration: bool,
        local_slot: Option<u32>,
    ) -> Option<usize> {
        let idx = local_slot? as usize;
        if dynamic || bind_declaration || idx >= self.locals.len() {
            return None;
        }
        // A `$` scalar with a plain name (no `@`/`%`/`&`, no attribute twigil).
        code.simple_scalar_local_desc(idx)?;
        // Nothing reads the slot by name, and no sibling slot shares the name.
        if code.needs_env_sync.get(idx).copied().unwrap_or(true)
            || code.dup_named_locals.get(idx).copied().unwrap_or(true)
        {
            return None;
        }
        if crate::opcode::reflective_name_access_possible()
            || self.threads.shared_vars_active
            || !self.threads.thread_decl_in_flight.is_empty()
            || !self.lexicals.hoist_pending_cells.is_empty()
            || !code.our_locals.is_empty()
            || !code.state_locals.is_empty()
            || self.module.lexical_fatal_mode
            || Self::atomic_name_possible(name)
        {
            return None;
        }
        // Boxing decisions the declaration would otherwise take
        // (`exec_set_local_op_body`'s `box_decl*`).
        let sym = code.locals_sym.get(idx).copied()?;
        if code.needs_cell_named_sub.contains(&sym)
            || code.needs_cell_ref_capture_slots.contains(&(idx as u32))
            || code.needs_cell_unvouched_containers.contains(&sym)
            || code.needs_cell_escaping_our_sub.contains(&sym)
            || code.self_capture_decl_locals.contains(&sym)
        {
            return None;
        }
        // Name-keyed metadata a declaration clears when it exists at all.
        if crate::env::elem_index_meta_possible()
            || crate::env::sigilless_readonly_keys_possible()
            || crate::env::scalar_bind_no_container_possible()
            || crate::env::closure_meta_keys_possible()
            || crate::env::bound_array_slice_possible()
            || !self.local_bind_pairs.is_empty()
            || !self.pending_alias_bind_names.is_empty()
        {
            return None;
        }
        Some(idx)
    }

    /// The `SetLocalDecl` store of an env-free declaration whose value is an
    /// ordinary scalar: write the slot and nothing else.
    ///
    /// Everything the generic path would do for this store is inert under the
    /// conditions below, in the order that path reaches it: the marks are only
    /// the declaration's own (no bind, rebind, `constant`, shaped or raw-param
    /// flavour); the value is a tag-probed inert payload (so no Proxy fetch,
    /// Seq reification, itemization or `Nil`-to-type-object reset); the slot
    /// holds no write-through cell; a declared constraint is the identity for
    /// the value ([`Interpreter::native_typed_store_is_identity`]); the env
    /// mirror is skipped exactly as the generic store skips it for a
    /// `!needs_env_sync` slot; and every name-keyed lane the cascade consults
    /// (`set_local_scalar_metadata_lanes_clear`, the latches in
    /// [`Self::env_free_decl_slot`]) is clear.
    ///
    /// Declines (`false`, nothing consumed) for anything else, and the caller
    /// falls through to the full path.
    // Cost: O(1).
    pub(super) fn exec_set_local_decl_fast(&mut self, code: &CompiledCode, idx: u32) -> bool {
        let idx = idx as usize;
        if !self.mark_ctx.only_declaration_marks() {
            return false;
        }
        let Some(name) = code.locals.get(idx) else {
            return false;
        };
        let Some(desc) = code.simple_scalar_local_desc(idx) else {
            return false;
        };
        if self
            .env_free_decl_slot(code, name, false, false, Some(idx as u32))
            .is_none()
            || !self.set_local_scalar_metadata_lanes_clear(code, idx, desc)
        {
            return false;
        }
        let Some(v) = self.stack.last() else {
            return false;
        };
        if !v.is_inert_scalar_store_payload() || !self.locals[idx].is_plain_scalar_store_slot() {
            return false;
        }
        if Self::slot_type_constraint_possible(code, idx)
            && !self.native_typed_store_is_identity(code, idx, v)
        {
            return false;
        }
        // -- committed: from here the store cannot fall back --
        let v = self.stack.pop().unwrap_or(Value::NIL);
        self.mark_ctx.consume_for_store();
        // The declaration steps the generic store runs unconditionally.
        self.clear_var_default(name);
        if !self.no_readonly_vars()
            && let Some(sym) = code.locals_sym.get(idx).copied()
        {
            self.unmark_readonly_sym(sym);
        }
        if !self.lexicals.constant_var_names_seen.is_empty()
            && self.lexicals.constant_var_names_seen.contains(name.as_str())
        {
            self.clear_constant_marker(name);
        }
        self.locals[idx] = v;
        true
    }
}
