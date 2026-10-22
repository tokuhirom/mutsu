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
//! write. A single-element store resumes by storing the type object in the
//! slot, and an `@` declaration's check resumes per element (#14153). `%`
//! stores, slice stores and binds keep unwinding to the region.

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
            OpCode::SetLocal(_)
                | OpCode::SetLocalDecl { .. }
                | OpCode::TypeCheck(..)
                | OpCode::IndexAssignExprNamed { .. }
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
        // A refused single-element store resumes by storing the constraint's
        // type object in the slot, which is what Rakudo leaves there; the
        // store's own result is that type object.
        if let OpCode::IndexAssignExprNamed {
            name_idx,
            is_positional,
            target_slot,
            element_share,
            ..
        } = &code.ops[*ip]
        {
            let Some((index, constraint)) = err.element_store_resume().cloned() else {
                return Err(err);
            };
            self.try_catch_inline(err)?;
            // A refused assignment keeps the slot's old value; only a slot
            // that did not exist yet is created, holding the type object.
            let target = target_slot
                .and_then(|slot| self.locals.get(slot as usize).cloned())
                .or_else(|| self.env().get_sym(code.const_sym(*name_idx)).cloned());
            if let Some(existing) = target
                .and_then(|t| Self::existing_element_container(&t, &index, *is_positional))
            {
                self.stack.push(existing.into_deref());
                *ip += 1;
                return Ok(());
            }
            let type_object = Value::package(Symbol::intern(
                &self.nominal_type_object_name_for_constraint(&constraint),
            ));
            // The dispatch arm's operand order after its swap: value, index.
            self.stack.push(type_object);
            self.stack.push(index);
            self.exec_index_assign_expr_named_op(
                code,
                *name_idx,
                *is_positional,
                *target_slot,
                *element_share,
            )?;
            *ip += 1;
            return Ok(());
        }
        let declared_type = match &code.ops[*ip] {
            OpCode::TypeCheck(tc_idx, var_name_idx, ..) => {
                let aggregate = var_name_idx
                    .map(|idx| Self::const_str(code, idx))
                    .filter(|name| name.starts_with(['@', '%']));
                if let Some(name) = aggregate {
                    // An `@` declaration resumes per element: the failing one
                    // becomes the type object and the check runs again for the
                    // next. A `%` store keeps unwinding.
                    if !name.starts_with('@') {
                        return Err(err);
                    }
                    return self.resume_array_element_check(code, *tc_idx, err);
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

    /// Resume an `@` declaration's element check (#14153): the first element
    /// the constraint refuses is replaced by the constraint's type object, as
    /// Rakudo leaves it, and the op runs again so the next refused element
    /// raises (and is offered to the handlers) in turn.
    // Cost: O(n) for the n elements of the list being declared.
    fn resume_array_element_check(
        &mut self,
        code: &CompiledCode,
        tc_idx: u32,
        err: RuntimeError,
    ) -> Result<(), RuntimeError> {
        let constraint = Self::const_str(code, tc_idx);
        let Some(top) = self.stack.last().cloned() else {
            return Err(err);
        };
        if !matches!(top.view(), ValueView::Array(..)) {
            return Err(err);
        }
        let mut items = crate::runtime::value_to_list(&top);
        let Some(bad) = items.iter().position(|item| {
            !self.array_elements_match_constraint(constraint, &Value::real_array(vec![item.clone()]))
        }) else {
            return Err(err);
        };
        self.try_catch_inline(err)?;
        items[bad] = Value::package(Symbol::intern(
            &self.nominal_type_object_name_for_constraint(constraint),
        ));
        if let Some(slot) = self.stack.last_mut() {
            *slot = Value::real_array(items);
        }
        Ok(())
    }
}
