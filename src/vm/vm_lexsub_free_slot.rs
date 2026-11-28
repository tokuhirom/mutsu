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
//! `GetLexFree { slot, .. }` (`CompiledCode::lexsub_free_slot_plan`):
//!
//! - **Empty slot** (Nil, as every call opens its frame): resolve by name,
//!   exactly as the by-name opcode would (`exec_get_global_op` and kin), then
//!   fill the slot when the answer provably came from the alias -- the frame
//!   is not linked to a capture that binds the name, the per-read guards
//!   hold, and the alias binding's content is, bit for bit, the value the
//!   chain produced. A binding that is a plain value (the read-only parameter
//!   an alias holds as is) is kept in a fresh cell of its own.
//! - **A cell**: re-test the guards (the probes of the chain whose answer can
//!   change during a call: an rw-return context, an atomic variable of the
//!   name, a sigilless attribute alias, an escaping `our` capture, a running
//!   `for` parameter of the name, and for an `@`/`%` name the cross-thread
//!   atomic lane), then read through the cell. A Nil or lazy
//!   thunk, and an aggregate the by-name read would reshape (an itemized
//!   Array, a Hash under `@`), take the by-name path.
//! - **`Never`** (the chain's answer was not the alias, or the name is one
//!   the sub rebinds): every read of the call takes the by-name path.
//!
//! The slot is per frame by construction, so recursion, threads and a sub
//! that escaped its declaring activation need no bookkeeping: a call through
//! a code object that captured its activation fails the capture test and
//! reads by name, as before.

use super::*;
use crate::opcode::LexFreeKind;

/// What an unfillable `GetLexFree` slot holds for the rest of its call. Any
/// non-cell, non-Nil value would do; the slot is a hidden local no other
/// opcode names.
const LEX_FREE_NEVER: Value = Value::FALSE;

impl Interpreter {
    /// Execute `GetLexFree { slot, name_idx, kind }`.
    // Cost: O(l) on a filled slot, l = names of the running `for` parameters
    // (the guards); otherwise the by-name read of `kind`, plus on the first
    // read of a call O(a + d), a = the sub's aliases, d = env chain depth.
    pub(super) fn exec_get_lex_free_op(
        &mut self,
        code: &CompiledCode,
        slot: u32,
        name_idx: u32,
        kind: LexFreeKind,
        ip: &mut usize,
    ) -> Result<(), RuntimeError> {
        let held = self
            .locals
            .get(slot as usize)
            .cloned()
            .unwrap_or(Value::NIL);
        if held.is_container_ref() {
            let name = Self::const_str(code, name_idx);
            if self.lex_free_guards_hold(name, kind) {
                let val = held.deref_clone();
                if Self::lex_free_servable(&val, kind) {
                    self.stack.push(val);
                    *ip += 1;
                    return Ok(());
                }
            }
            return self.exec_lex_free_by_name(code, name_idx, kind, ip);
        }
        if !held.is_nil() {
            return self.exec_lex_free_by_name(code, name_idx, kind, ip);
        }
        // The first read of the call: take the alias straight from the env
        // when nothing ahead of it in the by-name chain can answer.
        if let Some(cell) = self.lex_free_direct(code, name_idx, kind) {
            let val = cell.deref_clone();
            if Self::lex_free_servable(&val, kind) {
                #[cfg(debug_assertions)]
                self.debug_assert_lex_free_agrees(code, name_idx, kind, *ip, &val)?;
                if let Some(entry) = self.locals.get_mut(slot as usize) {
                    *entry = cell;
                }
                self.stack.push(val);
                *ip += 1;
                return Ok(());
            }
        }
        self.exec_lex_free_by_name(code, name_idx, kind, ip)?;
        if let Some(val) = self.stack.last().cloned() {
            self.lex_free_fill(code, slot, name_idx, kind, &val);
        }
        Ok(())
    }

    /// The running sub's alias binding for the name `name_idx`, taken without
    /// the by-name chain, as the cell to keep in the slot: the guards hold,
    /// the frame is not linked to a capture, and the alias is a cell. A
    /// plain-value alias (a read-only parameter) goes through the verified
    /// fill instead.
    ///
    /// Every store the chain asks before `unit_lexical_slot`'s alias step is
    /// either behind a guard re-tested here (an rw-return context, atomics,
    /// sigilless attributes, escaping `our` captures, loop parameters) or
    /// cannot hold a routine's `my` lexical that the declaring routine
    /// shadows; a debug build checks the answer against the chain on every
    /// first read.
    // Cost: O(l + a + d), l = running `for` parameter names, a = the sub's
    // aliases, d = env chain depth.
    fn lex_free_direct(
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
        let cell = self.lexsub_alias_binding(name, Some(name_sym))?;
        cell.is_container_ref().then(|| cell.clone())
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

    /// The probes of the by-name chain that run ahead of the alias and whose
    /// answer can change while one call runs: the scalar chain's (see
    /// `lexsub_free_cache_guards_hold`), and for an `@`/`%` name the
    /// cross-thread atomic lane, which answers before the alias once any
    /// atomic container operation has created an entry.
    // Cost: O(l), l = names of the running `for` parameters.
    #[inline]
    fn lex_free_guards_hold(&self, name: &str, kind: LexFreeKind) -> bool {
        self.lexsub_free_cache_guards_hold(name)
            && (kind == LexFreeKind::Scalar
                || !crate::runtime::shared_store::atomic_lane_entries_exist())
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
            LexFreeKind::Scalar => self.exec_get_global_op(code, name_idx, ip),
            LexFreeKind::Array => self.exec_get_array_var_op(code, name_idx, ip),
            LexFreeKind::Hash => self.exec_get_hash_var_op(code, name_idx, ip),
        }
    }

    /// Whether a cell's content is what the by-name read of `kind` pushes
    /// unchanged: not Nil (the chain's type-object default owns it), not a
    /// lazy thunk (the chain forces it), and for an aggregate the plain
    /// container the by-name opcode passes through.
    // Cost: O(1).
    #[inline]
    fn lex_free_servable(val: &Value, kind: LexFreeKind) -> bool {
        if val.is_nil() || val.is_lazy_thunk_value() || val.is_container_ref() {
            return false;
        }
        match kind {
            LexFreeKind::Scalar => true,
            LexFreeKind::Array => {
                matches!(val.view(), ValueView::Array(_, k) if !k.is_itemized())
            }
            LexFreeKind::Hash => matches!(val.view(), ValueView::Hash(_)),
        }
    }

    /// Store in `slot` the binding the by-name read just answered `val` from,
    /// when the module docs' conditions show it was the running sub's alias;
    /// otherwise mark the slot [`LEX_FREE_NEVER`]. A guard that fails now may
    /// hold on the next read, so it leaves the slot empty.
    // Cost: O(a + d), a = the sub's aliases, d = env chain depth.
    fn lex_free_fill(
        &mut self,
        code: &CompiledCode,
        slot: u32,
        name_idx: u32,
        kind: LexFreeKind,
        val: &Value,
    ) {
        let name = Self::const_str(code, name_idx);
        if !self.lex_free_guards_hold(name, kind) {
            return;
        }
        let name_sym = code.const_sym(name_idx);
        let bound = self.lex_free_binding(code, name, name_sym, val);
        if let Some(entry) = self.locals.get_mut(slot as usize) {
            *entry = bound.unwrap_or(LEX_FREE_NEVER);
        }
    }

    /// The running routine-nested sub's alias cell for `name`, when it is
    /// the binding the by-name read answered `val` from.
    // Cost: O(a + d), as `lexsub_alias_binding`.
    fn lex_free_binding(
        &self,
        code: &CompiledCode,
        name: &str,
        name_sym: Symbol,
        val: &Value,
    ) -> Option<Value> {
        // A rebinding (`$x := ...`) inside the sub seats a new binding the
        // slot would not follow.
        if code.free_var_rebinds.contains(&name_sym) {
            return None;
        }
        // A frame linked to a code object that captured its activation
        // resolves the name there, not through the alias.
        if self.env().frame_capture_holds_name(name, Some(name_sym)) {
            return None;
        }
        let cell = self.lexsub_alias_binding(name, Some(name_sym))?.clone();
        if !cell.deref_clone().same_binding(val) {
            return None;
        }
        if cell.is_container_ref() {
            return Some(cell);
        }
        // A plain value is the binding of a read-only parameter, which no one
        // writes; a name this body writes would need the cell it lacks.
        if code.free_var_writes.contains(&name_sym)
            || code.free_var_container_writes.contains(&name_sym)
        {
            return None;
        }
        Some(Value::container_ref(crate::gc::Gc::new(
            crate::value::ContainerCell::new(cell),
        )))
    }
}
