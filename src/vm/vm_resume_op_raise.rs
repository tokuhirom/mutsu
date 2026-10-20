//! ADR-0072 at an op's own raise site (#11691): the `X::TypeCheck::Assignment`
//! a store op raises itself (`my Int $x = "a"`) runs the resume-capable
//! `CATCH` handlers inline, as `die` does, so `.resume` continues with the
//! next statement instead of leaving the protected block. Rakudo throws that
//! one resumably; an `X::Assignment::RO` it raises is not resumable, and keeps
//! unwinding here.
//!
//! A resumed store leaves its container as it was, as in Rakudo: a scalar
//! declaration keeps its type object (`my Int $x = "a"` resumes with `$x`
//! `Int`), an assignment keeps the old value. Only ops whose operand stack at
//! the raise point is known take part: `SetLocal`/`SetLocalDecl` have popped
//! the value and push nothing, and a scalar declaration's `TypeCheck` still
//! holds the rejected value, which becomes the type object its store will
//! write. Element stores, `@`/`%` stores and binds keep unwinding to the
//! region (#11691 follow-up).

use crate::opcode::{CompiledCode, OpCode};
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

impl Interpreter {
    /// Whether an exception raised by `op` itself may resume at the next op.
    // Cost: O(1).
    pub(crate) fn op_raise_resumes_in_place(op: &OpCode) -> bool {
        matches!(
            op,
            OpCode::SetLocal(_) | OpCode::SetLocalDecl { .. } | OpCode::TypeCheck(..)
        )
    }

    /// Offer `err`, raised by the op at `*ip`, to the resume-capable `CATCH`
    /// handlers at its raise site ([`Interpreter::try_catch_inline`]). A
    /// handler that resumes continues at the next op with the container
    /// unchanged; anything else unwinds as before.
    // Cost: O(h) for the handlers offered the exception, plus their own cost.
    pub(crate) fn resume_op_raise(
        &mut self,
        code: &CompiledCode,
        ip: &mut usize,
        err: RuntimeError,
    ) -> Result<(), RuntimeError> {
        if !Self::is_inline_catchable(&err)
            || err.catch_inline_verdict().is_some()
            || !err.exception.as_deref().is_some_and(|exception| {
                matches!(exception.view(), ValueView::Instance { class_name, .. }
                    if class_name == "X::TypeCheck::Assignment")
            })
        {
            return Err(err);
        }
        let declared_type = match &code.ops[*ip] {
            OpCode::TypeCheck(tc_idx, var_name_idx, ..) => {
                // An `@`/`%` declaration's check runs per element; resuming it
                // would leave a half-built aggregate.
                let scalar = var_name_idx
                    .is_none_or(|idx| !Self::const_str(code, idx).starts_with(['@', '%']));
                if !scalar {
                    return Err(err);
                }
                let constraint = Self::const_str(code, *tc_idx);
                Some(Value::package(Symbol::intern(
                    &self.nominal_type_object_name_for_constraint(constraint),
                )))
            }
            OpCode::SetLocal(slot) | OpCode::SetLocalDecl { slot, .. } => {
                // An `@`/`%` list assignment can fail part way through.
                if code
                    .locals
                    .get(*slot as usize)
                    .is_some_and(|name| name.starts_with(['@', '%']))
                {
                    return Err(err);
                }
                None
            }
            _ => None,
        };
        self.try_catch_inline(err)?;
        if let Some(type_object) = declared_type
            && let Some(top) = self.stack.last_mut()
        {
            *top = type_object;
        }
        *ip += 1;
        Ok(())
    }
}
