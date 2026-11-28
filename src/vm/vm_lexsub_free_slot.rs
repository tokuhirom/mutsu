//! `GetLexFree`: a routine-nested sub's free-variable read through a slot of
//! its own frame (ADR-18244 stage B, #18274).
//!
//! `sub to-json(\obj) { my str @out; sub jsonify(\o) { nqp::push_s(@out, ...) } }`:
//! `jsonify` reads `@out` by name, and every read walked the whole by-name
//! chain -- the attribute, gather and package-store probes, then
//! `get_env_with_main_alias_sym`'s cascade -- before `unit_lexical_slot`
//! reached the ADR-0114 alias cell of the declaring activation. That was
//! ~3k instructions per read, the same again for each `$comma` / `$pretty`.
//!
//! The binding such a read resolves to is fixed for the call: the alias names
//! the cell of the declaring activation, which the declaration bound before
//! the sub could run (the argument of `vm_lexsub_free_cache.rs`, which this
//! replaces for the reads the compiler rewrites). So the compiler gives the
//! sub one hidden local per aliased free variable and compiles each read to
//! `GetLexFree { slot, .. }` (`CompiledCode::lexsub_free_slot_plan`). The
//! slot holds the binding or, as every call opens its frame, Nil:
//!
//! - **Nil**: take the running sub's alias binding from the env
//!   (`lexsub_alias_binding`) -- or, for a sub declared at the mainline or in
//!   a bare block, the cell ADR-0024 captured into its unit-lexical bucket,
//!   also fixed for the call -- unless a probe the by-name chain asks ahead of
//!   the alias could answer instead (the guards below, a frame linked to a
//!   code object that captured its activation, a name the sub rebinds). That
//!   binding is a shared cell, or the plain value of a read-only parameter
//!   the alias holds as is, which nobody writes. Keep it in the slot. When
//!   no alias answers, the read goes by name and the slot stays empty.
//! - **A binding**: re-test the guards -- the probes of the chain whose
//!   answer can change during a call: an rw-return context, an atomic
//!   variable of the name, a sigilless attribute alias, an escaping `our`
//!   capture, a running `for` parameter of the name, and for an `@`/`%` name
//!   the cross-thread atomic lane -- then read through it.
//!
//! A Nil or lazy thunk, and an aggregate the by-name read would reshape (an
//! itemized Array, a Hash under `@`), take the by-name path whatever the slot
//! holds; so does every read while a guard fails. A debug build checks each
//! first read of a call against the by-name read.
//!
//! A read of an enclosing scope's sigil-less constant (`LexFreeKind::Constant`,
//! emitted by `Compiler::emit_outer_term_constant_read` in any chunk) needs no
//! alias: a constant is never rebound, so the slot keeps whatever the call's
//! first by-name read answered.
//!
//! The slot is per frame by construction, so recursion, threads and a sub
//! that escaped its declaring activation need no bookkeeping: a call through
//! a code object that captured its activation fails the capture test and
//! reads by name, as before.

use super::*;
use crate::opcode::LexFreeKind;

impl Interpreter {
    /// Execute `GetLexFree { slot, name_idx, kind }`.
    // Cost: O(l) on a filled slot, l = names of the running `for` parameters
    // (the guards); on the first read of a call O(l + a + d), a = the sub's
    // aliases, d = env chain depth; otherwise the by-name read of `kind`.
    pub(super) fn exec_get_lex_free_op(
        &mut self,
        code: &CompiledCode,
        slot: u32,
        name_idx: u32,
        kind: LexFreeKind,
        ip: &mut usize,
    ) -> Result<(), RuntimeError> {
        let filled = self.locals.get(slot as usize).is_some_and(|v| !v.is_nil());
        if filled {
            // Borrowed, not cloned: only the content leaves the slot.
            let val = (self.lex_free_guards_hold_at(code, name_idx, kind))
                .then(|| self.locals[slot as usize].deref_clone())
                .filter(|val| Self::lex_free_servable(val, kind));
            if let Some(val) = val {
                self.stack.push(val);
                *ip += 1;
                return Ok(());
            }
            return self.exec_lex_free_by_name(code, name_idx, kind, ip);
        }
        if kind == LexFreeKind::Constant {
            return self.exec_lex_free_constant_first(code, slot, name_idx, ip);
        }
        if let Some(binding) = self.lex_free_binding(code, name_idx, kind) {
            let val = binding.deref_clone();
            if Self::lex_free_servable(&val, kind) {
                #[cfg(debug_assertions)]
                self.debug_assert_lex_free_agrees(code, name_idx, kind, *ip, &val)?;
                if let Some(entry) = self.locals.get_mut(slot as usize) {
                    *entry = binding;
                }
                self.stack.push(val);
                *ip += 1;
                return Ok(());
            }
        }
        self.exec_lex_free_by_name(code, name_idx, kind, ip)
    }

    /// The first read of a call of an enclosing scope's sigil-less constant:
    /// read it by name and keep the answer in the slot. A constant is never
    /// rebound, so every later read of the call would answer the same; only a
    /// value the by-name read would not pass through unchanged is left out.
    // Cost: the by-name read.
    fn exec_lex_free_constant_first(
        &mut self,
        code: &CompiledCode,
        slot: u32,
        name_idx: u32,
        ip: &mut usize,
    ) -> Result<(), RuntimeError> {
        self.exec_get_global_op(code, name_idx, ip)?;
        let name = Self::const_str(code, name_idx);
        if self.lex_free_guards_hold(name, LexFreeKind::Constant)
            && let Some(val) = self.stack.last()
            && Self::lex_free_servable(val, LexFreeKind::Constant)
        {
            let val = val.clone();
            if let Some(entry) = self.locals.get_mut(slot as usize) {
                *entry = val;
            }
        }
        Ok(())
    }

    /// The probes of the by-name chain that run ahead of the alias and whose
    /// answer can change while one call runs: the scalar chain's (see
    /// `lexsub_free_cache_guards_hold`), and for an `@`/`%` name the
    /// cross-thread atomic lane, which answers before the alias once any
    /// atomic container operation has created an entry.
    // Cost: O(l), l = names of the running `for` parameters.
    #[inline]
    fn lex_free_guards_hold(&self, name: &str, kind: LexFreeKind) -> bool {
        self.lexsub_free_cache_guards_hold(name)
            && (matches!(kind, LexFreeKind::Scalar | LexFreeKind::Constant)
                || !crate::runtime::shared_store::atomic_lane_entries_exist())
    }

    /// [`Self::lex_free_guards_hold`] for the name constant `name_idx`, testing
    /// the per-interpreter flags first and resolving the name only for the
    /// tests that need it (an atomic variable registered somewhere, a running
    /// `for` parameter).
    // Cost: O(l), l = names of the running `for` parameters; O(1) when none
    // runs and no atomic variable exists.
    #[inline]
    fn lex_free_guards_hold_at(
        &self,
        code: &CompiledCode,
        name_idx: u32,
        kind: LexFreeKind,
    ) -> bool {
        if self.rw_return_context
            || self.threads.sigilless_attrs_active
            || !self.lexicals.escaping_our_lexical_names.is_empty()
            || (matches!(kind, LexFreeKind::Array | LexFreeKind::Hash)
                && crate::runtime::shared_store::atomic_lane_entries_exist())
        {
            return false;
        }
        if !self.atomic_var_seen() && !self.topic_state.any_loop_param_active() {
            return true;
        }
        self.lex_free_guards_hold(Self::const_str(code, name_idx), kind)
    }

    /// The by-name read `kind` stands for.
    // Cost: as the by-name opcode.
    fn exec_lex_free_by_name(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        kind: LexFreeKind,
        ip: &mut usize,
    ) -> Result<(), RuntimeError> {
        match kind {
            LexFreeKind::Scalar | LexFreeKind::Constant => {
                self.exec_get_global_op(code, name_idx, ip)
            }
            LexFreeKind::Array => self.exec_get_array_var_op(code, name_idx, ip),
            LexFreeKind::Hash => self.exec_get_hash_var_op(code, name_idx, ip),
        }
    }

    /// Whether a binding's content is what the by-name read of `kind` pushes
    /// unchanged: not Nil (the chain's type-object default owns it), not a
    /// lazy thunk (the chain forces it), not a further cell, and for an
    /// aggregate the plain container the by-name opcode passes through.
    // Cost: O(1).
    #[inline]
    fn lex_free_servable(val: &Value, kind: LexFreeKind) -> bool {
        if val.is_nil() || val.is_lazy_thunk_value() || val.is_container_ref() {
            return false;
        }
        match kind {
            LexFreeKind::Scalar | LexFreeKind::Constant => true,
            LexFreeKind::Array => {
                matches!(val.view(), ValueView::Array(_, k) if !k.is_itemized())
            }
            LexFreeKind::Hash => matches!(val.view(), ValueView::Hash(_)),
        }
    }

    /// The running sub's alias binding for the name `name_idx`, taken without
    /// the by-name chain, when nothing the chain asks ahead of the alias can
    /// answer: the guards hold, the sub does not rebind the name, and the
    /// frame is not linked to a code object that captured its activation.
    /// A plain value (a read-only parameter the alias holds as is) counts only
    /// when this body writes nothing under the name.
    ///
    /// Every store the chain asks before `unit_lexical_slot`'s alias step is
    /// either behind one of those tests or cannot hold a routine's `my`
    /// lexical that the declaring routine shadows; a debug build checks the
    /// answer against the chain on every first read.
    // Cost: O(l + a + d), l = running `for` parameter names, a = the sub's
    // aliases, d = env chain depth.
    fn lex_free_binding(
        &self,
        code: &CompiledCode,
        name_idx: u32,
        kind: LexFreeKind,
    ) -> Option<Value> {
        let name = Self::const_str(code, name_idx);
        let name_sym = code.const_sym(name_idx);
        if !self.lex_free_guards_hold(name, kind)
            || code.free_var_rebinds.contains(&name_sym)
            || self.env().frame_capture_holds_name(name, Some(name_sym))
        {
            return None;
        }
        let binding = match self.lexsub_alias_binding(name, Some(name_sym)) {
            Some(cell) => cell.clone(),
            // A mainline or bare-block sub: its ADR-0024 capture.
            None => self
                .active_unit_lexical_entry()
                .and_then(|(_, cells)| cells.get(name).cloned())?,
        };
        if binding.is_container_ref()
            || !(code.free_var_writes.contains(&name_sym)
                || code.free_var_container_writes.contains(&name_sym))
        {
            Some(binding)
        } else {
            None
        }
    }

    /// Debug builds: the by-name read of a first `GetLexFree` read answers
    /// exactly what the alias taken directly does.
    #[cfg(debug_assertions)]
    fn debug_assert_lex_free_agrees(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        kind: LexFreeKind,
        ip: usize,
        direct: &Value,
    ) -> Result<(), RuntimeError> {
        let mut probe_ip = ip;
        self.exec_lex_free_by_name(code, name_idx, kind, &mut probe_ip)?;
        let by_name = self.stack.pop().unwrap_or(Value::NIL);
        debug_assert!(
            by_name.same_binding(direct),
            "GetLexFree: the alias of `{}` is not what the by-name read answers",
            Self::const_str(code, name_idx)
        );
        Ok(())
    }
}
