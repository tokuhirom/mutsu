//! The builtin method as the final candidate of a deferral chain that starts
//! in a method `augment`ed onto a core type (#10198).
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// When a method `augment`ed onto a core type (`augment class Str { multi
    /// method FatRat(Str:D:) { nextsame } }`, `augment class Array { method
    /// sort(|c) { callsame } }`) defers with `nextsame`/`callsame`/`nextwith`/
    /// `callwith` and the user candidates are exhausted, the builtin method of
    /// the receiver's type is the next — and last — candidate, exactly as the
    /// setting's own method is in Rakudo's MRO. mutsu implements those natively,
    /// so they are not `MethodDef`s and never appear in the user MRO walk; the
    /// deferral used to answer `Nil`.
    ///
    /// Only a native receiver qualifies (not a user `Instance`, whose MRO is
    /// all `MethodDef`s, nor a `Mixin`, which
    /// `native_mixin_base_next_candidate` bridges), and only when user code
    /// really declared the method on the receiver's type or an ancestor — the
    /// same registry question the native fast paths ask before they decline
    /// in favor of the user method. `native_base_bypass` then makes that
    /// question answer "no" for this one receiver and method while the builtin
    /// runs, so the deferral cannot re-enter the augmentation.
    pub(super) fn native_core_type_next_candidate(
        &mut self,
        override_args: Option<&[Value]>,
    ) -> Option<Result<Value, RuntimeError>> {
        let ctx = self.samewith_context_stack.last().cloned()?;
        let invocant = self
            .method_dispatch_stack
            .last()
            .map(|f| f.invocant.clone())
            .or_else(|| ctx.invocant.clone())
            .or_else(|| self.env.get("self").cloned())?;
        if invocant.is_lazy_match_value()
            || matches!(
                invocant.view(),
                ValueView::Instance { .. } | ValueView::Mixin(..) | ValueView::Package(_)
            )
        {
            return None;
        }
        let method_sym = Symbol::intern(&ctx.name);
        if !self.native_lever_a_user_override_sym(&invocant, method_sym) {
            return None;
        }
        let args: Vec<Value> = match override_args {
            Some(a) => a.to_vec(),
            None => self
                .method_dispatch_stack
                .last()
                .map(|f| f.args.clone())
                .or_else(|| ctx.args.clone())
                .unwrap_or_default(),
        };
        // Hide the augmentation from the override gate for exactly this
        // receiver and method while the builtin runs, so neither the pure
        // native probe nor the interpreter's builtin dispatch re-enters it.
        let type_name = crate::runtime::utils::value_type_name(&invocant);
        let saved = self.native_base_bypass.replace((
            type_name.as_ptr() as usize,
            method_sym,
            invocant.nanbox_bits(),
        ));
        let result = match self.try_native_method(&invocant, method_sym, &args) {
            Some(result) => result,
            // A builtin that needs the interpreter (`sort` with a comparator).
            None => self.call_method_with_values(invocant, &ctx.name, args),
        };
        self.native_base_bypass = saved;
        Some(result)
    }
}
