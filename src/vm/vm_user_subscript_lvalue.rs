//! A subscript assignment or binding through a user `postcircumfix:<[ ]>` /
//! `<{ }>` candidate (#16294).
//!
//! `@a[1.5] = v` with Array::Rounded's `multi sub postcircumfix:<[ ]>(R:D \SELF,
//! Any:D \index) { old-same SELF, index.round }` stores into the element the
//! candidate addresses: rakudo assigns to the container the delegating CORE
//! call returns. The store here is native, so the candidate is run first, in a
//! capture mode, only to learn the index its delegation to the CORE routine
//! used; the native store then goes to that index.

use super::*;

impl Interpreter {
    /// Replace the index on top of the stack (`[value, index]`) with the index
    /// a matching user `postcircumfix` candidate hands the CORE subscript
    /// routine, when the target is an object such a candidate is written for.
    /// Leaves the stack alone when no user candidate accepts `(target, index)`
    /// or none delegates to the CORE routine.
    // Cost: O(1) when the target is not an object; otherwise one candidate
    // resolution and one call of it.
    pub(super) fn remap_index_through_user_postcircumfix(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        is_positional: bool,
        target_slot: Option<u32>,
    ) -> Result<(), RuntimeError> {
        if self.stack.len() < 2 {
            return Ok(());
        }
        let Some(target) = target_slot
            .and_then(|slot| self.locals.get(slot as usize).cloned())
            .or_else(|| self.env().get_sym(code.const_sym(name_idx)).cloned())
            .map(|value| value.deref_container())
        else {
            return Ok(());
        };
        if !matches!(
            target.view(),
            ValueView::Instance { .. } | ValueView::Mixin(..) | ValueView::Array(..)
        ) {
            return Ok(());
        }
        let index = self.stack[self.stack.len() - 1].clone();
        let op_name = crate::lexical_builtin_op::postcircumfix_of_subscript(is_positional);
        let args = vec![target, index];
        let outer = self.dispatch.subscript_lvalue_capture.replace(None);
        let result = self.call_user_postcircumfix(op_name, args);
        let captured = std::mem::replace(&mut self.dispatch.subscript_lvalue_capture, outer);
        result?;
        if let Some(Some(index)) = captured {
            let top = self.stack.len() - 1;
            self.stack[top] = index;
        }
        Ok(())
    }

    /// Call the user `postcircumfix` candidate that accepts `args`, the way
    /// the read side finds it: the candidates a block's `use` imported, a
    /// family a `sub EXPORT` hook handed out, or a registered one whose
    /// invocant is typed with a user class (the CORE candidates take an
    /// untyped invocant, so only such a candidate outranks them).
    // Cost: one candidate resolution and one call of it.
    fn call_user_postcircumfix(
        &mut self,
        op_name: &str,
        args: Vec<Value>,
    ) -> Result<(), RuntimeError> {
        if let Some(result) = self.try_lexical_operator_candidates(op_name, args.clone()) {
            result?;
            return Ok(());
        }
        if self
            .try_exported_captured_family(op_name, args.clone())?
            .is_some()
        {
            return Ok(());
        }
        if let Some(def) = self.resolve_function_with_types(op_name, &args)?
            && def.param_defs.first().is_some_and(|p| {
                p.type_constraint.as_deref().is_some_and(|t| {
                    let t = t.trim_end_matches([':', 'D', 'U', '?']);
                    !t.is_empty() && !Self::is_builtin_type(t)
                })
            })
        {
            self.call_routine_def(&def, args)?;
        }
        Ok(())
    }
}
