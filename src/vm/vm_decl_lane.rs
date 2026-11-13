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
        // A sigilless term bound to a subscript's or a call's result is a
        // bind declaration; its slot is as much its only home as a readonly
        // term's (ADR-17376, "bound terms").
        let lean_bound = code.lean_bound_term_slot(idx);
        if dynamic || (bind_declaration && !lean_bound) || idx >= self.locals.len() {
            return None;
        }
        // A `$` scalar with a plain name (no `@`/`%`/`&`, no attribute twigil),
        // or a sigilless term whose slot is its only home (ADR-17376).
        let lean_term = code.lean_readonly_term_slot(idx) || lean_bound;
        if !lean_term {
            code.simple_scalar_local_desc(idx)?;
        }
        // Nothing reads the slot by name, and no sibling slot shares the name.
        if code.needs_env_sync.get(idx).copied().unwrap_or(true)
            || code.dup_named_locals.get(idx).copied().unwrap_or(true)
        {
            return None;
        }
        // A lean term has no env presence at all, so no thread-shared cell
        // can name it either, and its own predicate already refused every
        // by-name reader `mirrors_all_params` stands for.
        if (code.mirrors_all_params() && !lean_term)
            || (self.threads.shared_vars_active && !lean_term)
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
            || code.needs_cell_captured_containers.contains(&sym)
            || code.needs_cell_escaping_our_sub.contains(&sym)
            || code.self_capture_decl_locals.contains(&sym)
        {
            return None;
        }
        // Name-keyed metadata a declaration clears when it exists at all: THIS
        // slot's sigilless-readonly marker and alias key. They were asked as
        // two program-wide latches, so a program that had bound any sigilless
        // name (`my \z = 1` somewhere, as EC's `secp256k1` does) took the
        // full cascade for every declaration in it (~1.2K instructions each);
        // the per-name "ever an env key" bit answers the question this lane
        // actually has (#15981).
        // A lean term's readonly marker is nobody's to read: a lean readonly
        // term is readonly by its slot's flags and keeps no marker, and a lean
        // bound term's marker is settled by its own `MarkSigillessBind` on
        // every declaration (written for a value, cleared for a container).
        // So a marker of the same name left in the visible env -- typically by
        // a routine's sigilless parameter of that name (`sub publish(Str
        // \key, ...)` next to `my \key = ...`), which latches the name as an
        // env key for the whole program -- is not one the declaration has to
        // clear, and must not push every later declaration onto the general
        // store (#17331).
        if (crate::env::sigilless_readonly_keys_possible()
            && !lean_term
            && code
                .readonly_sym(idx)
                .is_none_or(crate::symbol::maybe_env_key))
            || (crate::env::closure_meta_keys_possible()
                && code.alias_sym(idx).is_none_or(crate::symbol::maybe_env_key))
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
        if code.lean_readonly_term_slot(idx) {
            return self.exec_set_local_term_decl_fast(code, idx);
        }
        if code.lean_bound_term_slot(idx) {
            return self.exec_set_local_bound_term_decl_fast(code, idx);
        }
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
            && self
                .lexicals
                .constant_var_names_seen
                .contains(name.as_str())
        {
            self.clear_constant_marker(name);
        }
        self.locals[idx] = v;
        true
    }

    /// The `SetLocalDecl` store of a lean bound sigilless term
    /// ([`CompiledCode::lean_bound_term_slot`]: `my \h = $cache{k}`,
    /// `my \m = f(...)`) whose bind source `MarkSigillessBindSource` judged a
    /// plain value: the term is bound to that value, so the store writes the
    /// slot and nothing else. The term's readonly marker is still written by
    /// the `MarkSigillessBind` that follows (ADR-17376, "bound terms").
    ///
    /// The bind source is the value itself or the `WrapVarRef` the subscript
    /// compile puts over its synthetic bind temp (`__mutsu_bind_index_ref_N`),
    /// whose wrapped element reference IS the source. Anything else -- a
    /// container to alias, a `VarRef` over a real variable, a non-inert
    /// payload -- declines (`false`, nothing consumed) to the general bind.
    // Cost: O(1).
    fn exec_set_local_bound_term_decl_fast(&mut self, code: &CompiledCode, idx: usize) -> bool {
        if !self.mark_ctx.only_bound_term_declaration_marks() {
            return false;
        }
        let Some(term_sym) = code.locals_sym.get(idx).copied() else {
            return false;
        };
        if self.sigilless_bind_source != Some((term_sym, false)) {
            return false;
        }
        let Some(name) = code.locals.get(idx) else {
            return false;
        };
        if self
            .env_free_decl_slot(code, name, false, true, Some(idx as u32))
            .is_none()
        {
            return false;
        }
        let Some(top) = self.stack.last() else {
            return false;
        };
        let source = match top.as_varref() {
            Some((name, inner, _)) if name.with_str(|s| s.starts_with("__mutsu_")) => inner,
            Some(_) => return false,
            None => top,
        };
        if !source.is_inert_scalar_store_payload() || !self.locals[idx].is_plain_scalar_store_slot()
        {
            return false;
        }
        // -- committed: from here the store cannot fall back --
        let v = source.clone();
        self.stack.pop();
        self.mark_ctx.consume_for_store();
        self.clear_var_default(name);
        if !self.no_readonly_vars() {
            self.unmark_readonly_sym(term_sym);
        }
        if !self.lexicals.constant_var_names_seen.is_empty()
            && self
                .lexicals
                .constant_var_names_seen
                .contains(name.as_str())
        {
            self.clear_constant_marker(name);
        }
        self.locals[idx] = v;
        true
    }

    /// The `SetLocalDecl` store of a lean readonly sigilless term
    /// (`my \k = EXPR`, [`CompiledCode::lean_readonly_term_slot`]): the slot
    /// is the term's only home and its readonly state is the slot's own
    /// compile-time fact (ADR-17376), so an inert value is just written.
    ///
    /// The declaration's marks are the plain ones plus `constant` (a term is
    /// declared in constant context); the value is a tag-probed inert payload
    /// (no container to bind, no Proxy, Seq or aggregate); the slot holds no
    /// write-through cell; and [`Self::env_free_decl_slot`]'s latches are
    /// clear. Declines (`false`, nothing consumed) otherwise.
    // Cost: O(1).
    fn exec_set_local_term_decl_fast(&mut self, code: &CompiledCode, idx: usize) -> bool {
        if !self.mark_ctx.only_term_declaration_marks() {
            return false;
        }
        let Some(name) = code.locals.get(idx) else {
            return false;
        };
        if self
            .env_free_decl_slot(code, name, false, false, Some(idx as u32))
            .is_none()
        {
            return false;
        }
        let Some(v) = self.stack.last() else {
            return false;
        };
        if !v.is_inert_scalar_store_payload() || !self.locals[idx].is_plain_scalar_store_slot() {
            return false;
        }
        // -- committed: from here the store cannot fall back --
        let v = self.stack.pop().unwrap_or(Value::NIL);
        self.mark_ctx.consume_for_store();
        self.clear_var_default(name);
        if !self.no_readonly_vars()
            && let Some(sym) = code.locals_sym.get(idx).copied()
        {
            self.unmark_readonly_sym(sym);
        }
        if !self.lexicals.constant_var_names_seen.is_empty()
            && self
                .lexicals
                .constant_var_names_seen
                .contains(name.as_str())
        {
            self.clear_constant_marker(name);
        }
        self.locals[idx] = v;
        true
    }
}
