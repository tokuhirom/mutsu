//! A call of a lexically imported plain routine (ADR-8751) binds its
//! positional arguments straight off the stack, as the registry alias's cached
//! call does (`exec_call_func_op_inner`), instead of draining them into an
//! argument vector first (#13906).

use super::*;
use crate::ast::FunctionDef;

impl Interpreter {
    /// Call `def`, bound to the `CallOnCodeVar` site's name `name_idx`, with
    /// the `arity` arguments on top of the stack through the positional-light
    /// entry. `Ok(false)`, with the stack untouched, when the call needs the
    /// general entry: named, Junction or `|` arguments, a deprecated or
    /// test-assertion routine, a wrapped one, or a signature the light binder
    /// declines.
    // Cost: O(a) plus the call, a = arguments.
    pub(super) fn try_lex_import_light_call(
        &mut self,
        code: &CompiledCode,
        def: &FunctionDef,
        name_idx: u32,
        arity: usize,
        arg_sources_idx: Option<u32>,
        compiled_fns: &CompiledFns,
    ) -> Result<bool, RuntimeError> {
        let Some(cf) = def.compiled_fn() else {
            return Ok(false);
        };
        if def.deprecated_message.is_some() || def.is_test_assertion || self.stack.len() < arity {
            return Ok(false);
        }
        // The light binder reads no rw-source slots, so the site is asked only
        // whether it spreads a `|EXPR` (no decode, no slot-map refill); the map
        // is left empty as `decode_arg_sources_shared` leaves it for a site
        // without slotted sources.
        if Self::stack_args_have_slip(code, arg_sources_idx) {
            return Ok(false);
        }
        self.pending_call_arg_source_slots.clear();
        let name = Self::const_str(code, name_idx);
        let start = self.stack.len() - arity;
        // The parser's synthetic callsite-line Pair, last when present, is
        // not an argument (`call_compiled_function_positional_light_at`).
        let marker = self.stack[start..]
            .last()
            .is_some_and(Self::is_callsite_line_marker);
        let end = self.stack.len() - marker as usize;
        let callsite_line = marker
            .then(|| Self::callsite_line_of_view(&self.stack[end].view()))
            .flatten();
        let args = &self.stack[start..end];
        let light = !args.iter().any(Self::arg_needs_general_call)
            && Self::is_positional_light_call_eligible(cf, name, args.len(), args, Some(code))
            && !Self::call_shares_container_into_scalar_param(cf, args)
            && self.wrap_sub_id_for_name(name).is_none();
        if !light {
            return Ok(false);
        }
        self.set_pending_callsite_line(callsite_line);
        self.stack.truncate(end);
        let cf = std::sync::Arc::clone(cf);
        let fns = cf.compiled_fns.clone();
        let result = self.call_compiled_function_positional_light_at(
            &cf,
            start,
            fns.as_deref().unwrap_or(compiled_fns),
            name,
            code.const_sym(name_idx),
            Some(code),
        )?;
        let result = self.maybe_fetch_rw_proxy(result, !cf.returns_container())?;
        self.stack.push(result);
        Ok(true)
    }

    /// Whether `arg` (a Pair, named or positional, or a Junction, possibly
    /// behind a variable wrapper or a Scalar) sends the call to the general
    /// entry. One `view()` of the unwrapped argument answers it; the Junction
    /// case equals `extract_junction_from_arg(..).is_some()` without cloning
    /// the eigenstates.
    // Cost: O(1).
    #[inline]
    fn arg_needs_general_call(arg: &Value) -> bool {
        match arg.unwrap_varref().view() {
            ValueView::Pair(..) | ValueView::ValuePair(..) | ValueView::Junction { .. } => true,
            ValueView::Scalar(inner) => matches!(inner.view(), ValueView::Junction { .. }),
            _ => false,
        }
    }
}
