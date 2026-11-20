//! `OpCode::StrEqLocalConst`: `$local eq 'literal'` fused (ADR-18357 D1).
//!
//! A tokenizer predicate is a chain of `$ch eq ' '`. Unfused, each one is
//! three ops: `GetLocal` clones the slot's `Str` onto the stack, `LoadConst`
//! clones the literal, and `StrEq` pops both, compares them and drops them.
//! The comparison itself is a length test and a `memcmp`; the clones, the
//! drops and the two extra dispatches cost more.
//!
//! The fused op borrows both operands where they live. It takes the narrow
//! arm only when reading the slot is exactly what `GetLocal`'s own fast path
//! would push, the slot holds a plain `Str`, and no user candidate of the
//! operator is in scope. In every other case it runs the three ops it
//! replaces, through their own handlers (ADR-18357 D3).

use super::vm_operator_values::StrCmp;
use super::*;

impl Interpreter {
    // Cost: O(p), p = common prefix; O(1) when the lengths differ.
    pub(super) fn exec_str_eq_local_const_op(
        &mut self,
        code: &CompiledCode,
        slot: u32,
        const_idx: u32,
        negate: bool,
    ) -> Result<(), RuntimeError> {
        let kind = if negate { StrCmp::Ne } else { StrCmp::Eq };
        if let Some(equal) = self.str_eq_local_const_narrow(code, slot as usize, const_idx, kind) {
            self.stack.push(Value::truth(equal != negate));
            return Ok(());
        }
        self.exec_get_local_op(code, slot)?;
        self.stack.push(code.constants[const_idx as usize].clone());
        self.exec_str_cmp_op(kind)
    }

    /// Whether the slot's plain `Str` equals the constant, or `None` when the
    /// narrow arm does not apply (see the module doc).
    // Cost: O(p), p = common prefix; O(1) when it declines.
    #[inline]
    fn str_eq_local_const_narrow(
        &self,
        code: &CompiledCode,
        slot: usize,
        const_idx: u32,
        kind: StrCmp,
    ) -> Option<bool> {
        if !crate::opcode_native_int_step::narrow_ops_enabled()
            || self.rw_return_context
            || !crate::vm::vm_jit::local_read_unspoiled()
            || !code.local_read_plain(slot)
        {
            return None;
        }
        let local = self.locals.get(slot)?.as_str()?;
        let literal = code.constants.get(const_idx as usize)?.as_str()?;
        if self.user_infix_override(kind.infix_name()) {
            return None;
        }
        Some(crate::builtins::str_prim::str_eq(local, literal))
    }
}
