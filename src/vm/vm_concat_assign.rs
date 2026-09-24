//! `OpCode::ConcatAssignLocal`: `$local ~= <rhs>` as one instruction.
//!
//! The point of the fusion is **ownership**, not dispatch count (#8695). The
//! sequence it replaces —
//! `GetLocalMetaAssign{slot, EmptyStr}; <rhs>; Concat; SetLocal(slot)` — reads
//! the accumulated string onto the stack before the RHS runs, so by the time
//! `Concat` executes the buffer is held twice (slot + stack) and concatenation
//! can only build a third one. That is O(len) per append, i.e. O(n²) to build
//! an n-character string, which is what made `$s ~= 'x'` in a loop 35x slower
//! than rakudo at 160k characters and diverging.
//!
//! Reading the slot *after* the RHS, and moving the value out of it rather
//! than cloning, leaves the append holding the only reference — so it can grow
//! the existing allocation (`Value::str_appended_nfc`) and the whole
//! accumulation becomes linear.

use super::*;
use crate::token_kind::MetaAssignIdentity;

impl Interpreter {
    /// Execute `$local ~= <rhs>` with the RHS already on the stack.
    // Cost: amortized O(m) on the in-place path, m = chars of the RHS (including a
    // slot mirrored to env, e.g. one declared inside `given`/`when`, and a private
    // attribute slot whose `self` cell is released alongside it); O(n + m) on the
    // fallback (a Proxy, a constrained attribute, a string still held elsewhere,
    // or a non-Str side), n = chars already accumulated. A variable captured by a
    // closure is appended in place by `AtomicCompoundVar` instead.
    pub(super) fn exec_concat_assign_local_op(
        &mut self,
        code: &CompiledCode,
        slot: u32,
    ) -> Result<(), RuntimeError> {
        self.exec_concat_local_op(code, slot, true)
    }

    /// Execute `$local = $local ~ <rhs>` with the RHS already on the stack
    /// (#9141). Identical to [`Self::exec_concat_assign_local_op`] except that
    /// the general path does not seed an undefined LHS with `''`: a literal
    /// `~` warns on it, as the unfused sequence did.
    // Cost: as exec_concat_assign_local_op.
    pub(super) fn exec_concat_reassign_local_op(
        &mut self,
        code: &CompiledCode,
        slot: u32,
    ) -> Result<(), RuntimeError> {
        self.exec_concat_local_op(code, slot, false)
    }

    fn exec_concat_local_op(
        &mut self,
        code: &CompiledCode,
        slot: u32,
        seed: bool,
    ) -> Result<(), RuntimeError> {
        let rhs = self.stack.pop().unwrap_or(Value::NIL);
        if self.try_concat_assign_attr_in_place(code, slot as usize, &rhs)
            || self.try_concat_assign_local_in_place(code, slot as usize, &rhs)?
        {
            self.publish_state_local(code, slot);
            return Ok(());
        }
        // Everything else runs the exact sequence this opcode replaces, in the
        // same order and through the same handlers, so every shape the fast
        // path declines (a container or Proxy in the slot, an undefined LHS
        // needing the identity seed, a `.Stringy` operand, a junction) behaves
        // as it always did.
        self.exec_get_local_op(code, slot)?;
        if seed {
            self.exec_meta_assign_identity_op(MetaAssignIdentity::EmptyStr)?;
        }
        self.stack.push(rhs);
        self.exec_concat_op()?;
        self.exec_set_local_op(code, slot)?;
        self.publish_state_local(code, slot);
        Ok(())
    }

    /// `$!attr ~= <Str>` on an attribute's local slot, appended in place, or
    /// `false` with nothing touched so the caller runs the general path.
    ///
    /// The string is held by the slot AND by `self`'s attribute cell (the
    /// slot mirrors it), so the general path's append always copied it: O(n)
    /// per `$!buf ~= ...`, quadratic over a method's accumulation (#9209).
    /// Here the slot's reference is moved out and the cell's is dropped under
    /// the attribute map's write lock, so the append owns the buffer.
    ///
    /// Taken only when the store the general path would run is the identity
    /// for a `Str` result: both sides plain `Str`, no user `infix:<~>` /
    /// `infix:<~=>`, no declared, attribute or `where` constraint on the
    /// attribute (a check that could fail must see the old value intact), and
    /// no env mirror of the slot (another holder of the string).
    fn try_concat_assign_attr_in_place(
        &mut self,
        code: &CompiledCode,
        idx: usize,
        rhs: &Value,
    ) -> bool {
        let Some((bare, is_private, sigil)) = code.local_attr_key(idx) else {
            return false;
        };
        let ValueView::Str(suffix) = rhs.view() else {
            return false;
        };
        if sigil != '$'
            || !self
                .locals
                .get(idx)
                .is_some_and(|v| matches!(v.view(), ValueView::Str(_)))
            || code.needs_env_sync.get(idx).copied().unwrap_or(true)
            || crate::opcode::reflective_name_access_possible()
            || self.user_infix_override("infix:<~>")
            || self.user_declared_infix_ops.contains_key("infix:<~=>")
        {
            return false;
        }
        let name = &code.locals[idx];
        let name_sym = code.locals_sym.get(idx).copied();
        if loan_env!(self, var_type_constraint_value_for(name, name_sym)).is_some()
            || self
                .scalar_attr_type_constraint(name)
                .is_some_and(|ty| !matches!(ty.as_str(), "Any" | "Mu" | "Str" | "Stringy" | "Cool"))
            || self.self_attr_where_constraint(name).is_some()
        {
            return false;
        }
        let plan = crate::value::StrAppendPlan::for_suffix(suffix.as_str());
        let lhs = std::mem::replace(&mut self.locals[idx], Value::NIL);
        match self.append_attr_cell_str_in_place(bare, is_private, sigil, lhs, &plan) {
            Ok(new_val) => {
                self.locals[idx] = new_val;
                true
            }
            Err(lhs) => {
                self.locals[idx] = lhs;
                false
            }
        }
    }

    /// Append `rhs` to the string in `slot` by growing its buffer, or answer
    /// `false` without touching anything so the caller runs the general path.
    ///
    /// Every condition here is a correctness gate, not a heuristic:
    ///
    /// - **Both sides plain `Str`.** An allomorph or mixin views as `Mixin`, a
    ///   user object needs `.Stringy` dispatch, an undefined LHS needs the
    ///   METAOP_ASSIGN identity seed — none of which this path performs.
    /// - **Any `Str` suffix, normalized in bounded time.** The result must
    ///   still be NFC, but re-running NFC over the whole concatenation is
    ///   O(len) per append and reintroduces the quadratic cost (#8725), so
    ///   `StrAppendPlan::for_suffix` reads the suffix alone and reports
    ///   whether the join can compose at all. It cannot for an ASCII suffix,
    ///   nor for any suffix starting at a normalization boundary (a snowman,
    ///   a CJK ideograph, a composed `é`); when it can — `"e" ~= "\x[301]"`
    ///   must yield a single `é` — only a bounded window around the join is
    ///   renormalized.
    /// - **An env mirror is released too, and written back by the real
    ///   store.** A slot that syncs to env (one captured by a closure, or
    ///   declared inside `given`/`when`) holds its string twice, so the append
    ///   would copy; the mirror must hold the very same allocation as the slot
    ///   (anything else is a divergence this path does not reason about), is
    ///   cleared alongside the slot for the append, and the result then goes
    ///   through `exec_set_local_op`, which updates both halves (#9141).
    /// - **The ordinary scalar store's own metadata gates**, asked through the
    ///   one predicate that owns that list
    ///   (`set_local_scalar_fast_metadata_clear`), because writing the slot
    ///   directly skips exactly the steps those gates protect.
    fn try_concat_assign_local_in_place(
        &mut self,
        code: &CompiledCode,
        idx: usize,
        rhs: &Value,
    ) -> Result<bool, RuntimeError> {
        let ValueView::Str(suffix) = rhs.view() else {
            return Ok(false);
        };
        let Some(current) = self.locals.get(idx) else {
            return Ok(false);
        };
        if !matches!(current.view(), ValueView::Str(_)) || !current.is_plain_scalar_store_slot() {
            return Ok(false);
        }
        if crate::opcode::reflective_name_access_possible() {
            return Ok(false);
        }
        let mirrored = code.needs_env_sync.get(idx).copied().unwrap_or(true);
        if !self.set_local_scalar_fast_metadata_clear(code, idx) {
            return Ok(false);
        }
        // The one probe that is not a latch: a shared cell or Proxy parked in
        // env under this name, which an ordinary store would write *through*.
        let name = &code.locals[idx];
        let name_sym = code.locals_sym.get(idx).copied();
        let env_entry = self.env().get_for(name, name_sym);
        if env_entry.is_some_and(|v| v.is_container_ref() || v.is_proxy_value()) {
            return Ok(false);
        }
        let mirror_sym = if mirrored {
            match (name_sym, env_entry) {
                (Some(sym), Some(v)) if v.same_binding(current) => Some(sym),
                _ => return Ok(false),
            }
        } else {
            None
        };
        // -- committed --
        // Moving the value out is what makes the buffer unique; cloning it
        // here would defeat the whole opcode. `Value::NIL` is never observable
        // in the slot (or its mirror): nothing runs between the take and the
        // store.
        let plan = crate::value::StrAppendPlan::for_suffix(suffix.as_str());
        let lhs = std::mem::replace(&mut self.locals[idx], Value::NIL);
        let Some(sym) = mirror_sym else {
            self.locals[idx] = lhs.str_appended_nfc(&plan);
            return Ok(true);
        };
        if let Some(entry) = self.env_mut().get_mut_sym(sym) {
            *entry = Value::NIL;
        }
        self.stack.push(lhs.str_appended_nfc(&plan));
        self.exec_set_local_op(code, idx as u32)?;
        Ok(true)
    }
}
