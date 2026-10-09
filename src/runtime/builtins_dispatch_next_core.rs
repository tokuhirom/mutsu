//! The builtin method as the final candidate of a deferral chain that starts
//! in a method `augment`ed onto a core type (#10198).
//!
//! When such a method (`augment class Str { multi method FatRat(Str:D:) {
//! nextsame } }`, `augment class Array { method sort(|c) { callsame } }`)
//! defers and the user candidates are exhausted, the builtin of the receiver's
//! type is the next and last candidate, as the setting's own method is in
//! Rakudo's MRO. The frame builder appends it as `DeferralEntry::Native`
//! (ADR-11276 slice 4); the builtin runs with the augmentation hidden from the
//! override gate so the deferral cannot re-enter it.
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Whether `invocant` is a native core-type value on which user code
    /// declared (`augment`ed) a method `name`: the registry question the
    /// native fast paths ask before they decline in favor of the user method.
    // Cost: O(1) memoized registry probe.
    pub(super) fn core_type_receiver_has_user_override(
        &mut self,
        invocant: &Value,
        name: &str,
    ) -> bool {
        if invocant.is_lazy_match_value()
            || matches!(
                invocant.view(),
                ValueView::Instance { .. } | ValueView::Mixin(..) | ValueView::Package(_)
            )
        {
            return false;
        }
        self.native_lever_a_user_override_sym(invocant, Symbol::intern(name))
    }

    /// Run the builtin `name` of a core-type receiver with the augmentation
    /// hidden from the override gate for exactly this receiver and method, so
    /// neither the pure native probe nor the interpreter's builtin dispatch
    /// re-enters it.
    pub(super) fn run_core_type_builtin(
        &mut self,
        invocant: Value,
        name: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let method_sym = Symbol::intern(name);
        let type_name = crate::runtime::utils::value_type_name(&invocant);
        let saved = self.dispatch.native_base_bypass.replace((
            type_name.as_ptr() as usize,
            method_sym,
            invocant.nanbox_bits(),
        ));
        let result = match self.try_native_method(&invocant, method_sym, &args) {
            Some(result) => result,
            // A builtin that needs the interpreter (`sort` with a comparator).
            None => self.call_method_with_values(invocant, name, args),
        };
        self.dispatch.native_base_bypass = saved;
        result
    }

    /// When a grammar's own `method ws` (or `alpha`, `ident`, ...) defers with
    /// `callsame`/`nextsame` and the user candidates are exhausted, the
    /// built-in rule is the next candidate, exactly as `Match`'s method is in
    /// Rakudo's MRO: it runs at the invocant cursor's position and answers the
    /// advanced (or failed) cursor. `None` when the receiver is no grammar
    /// cursor or the method is no built-in rule.
    // Cost: O(k) for the rule's k consumed chars plus O(a) to copy the
    // cursor's a attributes (see `grammar_builtin_rule_on_cursor`).
    pub(super) fn native_grammar_builtin_rule_base(
        &mut self,
    ) -> Option<Result<Value, RuntimeError>> {
        let name = self.dispatch.samewith_context_stack.last()?.name.clone();
        let invocant = self.dispatch.method_dispatch_stack.last()?.invocant.clone();
        self.grammar_builtin_rule_on_cursor(&invocant, &name)
            .map(Ok)
    }
}
