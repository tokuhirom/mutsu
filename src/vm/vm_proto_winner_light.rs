//! The winner of a `{*}`-only proto, bound through the positional light bind.
//!
//! A call through a trivial proto (`my proto sub insert(|) {*}`) resolves its
//! winner with `vm_resolve_trivial_proto_candidate`, which keeps the proto's
//! own candidate set (an inner proto hides the outer candidates). Running the
//! winner went through `compile_and_call_function_def`, which has no caller
//! `CompiledCode`: the light bind declines every sigilless parameter whose
//! argument is a caller variable (its promotion indexes the caller's locals),
//! so such a winner always took the general binder. Array::Sorted::Util's
//! `insert(@a, $pos, \value)` is one, called twice per
//! Concurrent::PriorityQueue push.
//!
//! The call site has its code, so the winner binds here as a multi winner on
//! the ordinary multi arm does: the dispatch frame `nextsame`/`callsame` read,
//! the samewith context, and the light bind. Only the binding changes; the
//! candidate choice is the proto's.

use super::*;

impl Interpreter {
    /// Run `def`, the resolved winner of a trivial proto, through the positional
    /// light bind, or `None` (nothing done) when this call or the winner needs
    /// the general binder. The caller has already declined a call with a
    /// named argument.
    // Cost: O(a + p), a = arguments, p = the winner's parameters (the light
    // bind's admissions), plus the call.
    pub(super) fn try_trivial_proto_winner_light(
        &mut self,
        code: &CompiledCode,
        def: &crate::ast::FunctionDef,
        name_sym: Symbol,
        args: &[Value],
        compiled_fns: &CompiledFns,
    ) -> Option<Result<Value, RuntimeError>> {
        let name = name_sym.as_str();
        // The general entry runs a dispatchee and the deprecation report; a
        // winner with either keeps it.
        if def.dispatchee.is_some() || def.deprecated_message.is_some() {
            return None;
        }
        let cf = def.compiled_fn()?.clone();
        // The light bind is written for a callee of the running compunit, as
        // `compile_and_call_function_def` requires: a module's file-scope `my`
        // is lexical to its own unit.
        if self.unit_of_source_sym(cf.source_file_sym()) != self.current_unit
            || !self.is_positional_light_call_eligible(
                &cf,
                name,
                Self::positional_light_argc(args),
                args,
                Some(code),
            )
            || loan_env!(self, routine_is_test_assertion_by_name(name, args))
            || self.wrap_sub_id_for_name(name).is_some()
            || self.light_call_blocked_by_mainline_capture(name)
        {
            return None;
        }
        let pushed_dispatch = loan_env!(
            self,
            push_multi_dispatch_frame_with_winner_sym(name, name_sym, args, Some(def))
        );
        self.push_samewith_context(name, None, None);
        // The winner's own nested-sub table, as `compile_and_call_function_def`
        // uses: a module routine called from the importing script resolves its
        // `my sub` keys there, not in the caller's table.
        let fns = cf.compiled_fns.as_deref().unwrap_or(compiled_fns);
        let result = self.call_compiled_function_positional_light(
            &cf,
            args,
            fns,
            name,
            name_sym,
            Some(code),
        );
        self.pop_samewith_context();
        if pushed_dispatch {
            self.pop_multi_dispatch();
        }
        Some(result.and_then(|value| {
            loan_env!(self, maybe_fetch_rw_proxy(value, !cf.returns_container()))
        }))
    }
}
