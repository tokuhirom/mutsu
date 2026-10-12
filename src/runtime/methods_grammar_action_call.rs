//! Calling the action method a reduced grammar node dispatches to.
//!
//! The post-parse action walk makes one method call per reduced node that
//! the actions class answers — on a YAML document, one per matched space or
//! quote character as well as one per scalar, key and map. Every such call
//! used to go through `call_method_with_values`, the general by-value method
//! funnel, which runs dozens of name-keyed interceptors (`emit`, `ACCEPTS`,
//! `sum`, the IO::Handle and Pair-subclass accessors, ...) before it reaches
//! the user method. An action method is a user method on a user class whose
//! name is a rule name, so the funnel always ended in the same place: the
//! compiled-method resolution of `try_dispatch_compiled_method_direct`. On
//! `benchmarks/bench-yaml-parse-big.raku` that walk to the method cost
//! ~25,000 instructions per action call, more than most action bodies do.
//!
//! [`Interpreter::call_grammar_action_method`] resolves the method directly
//! and falls back to the funnel only when no compiled user method answers
//! (a native or role-mixin receiver, an on-demand compile failure), so the
//! answer a call gets does not change — only the probes in front of it go.

use super::*;

impl Interpreter {
    /// Call the action method for one reduced node: the proto variant's
    /// `rule:sym<...>` method when the node carries one and the actions class
    /// answers it, else the plain `rule_name` method. Returns whether an
    /// action method was called at all, and its result; a node the actions
    /// class has no method for answers `(false, Ok(match_obj))`.
    ///
    /// A `X::Method::NotFound` for the variant method itself (an actions
    /// class answering the name through a mechanism the existence probe
    /// cannot see) falls back to the plain rule method, as before.
    // Cost: O(1) per call beyond the action method's own body: two
    // `actions_answer_method` probes (memoized per class and name) and one
    // generation-cached method resolution.
    pub(super) fn call_grammar_action_method(
        &mut self,
        actions: &Value,
        rule_name: &str,
        sym_method_name: Option<&str>,
        match_obj: &Value,
    ) -> (bool, Result<Value, RuntimeError>) {
        let actions_class_name = match actions.view() {
            ValueView::Instance { class_name, .. } => Some(class_name.as_str()),
            // A stateless `:actions(Actions)` grammar action is often passed
            // as the bare type object, not an instance.
            ValueView::Package(name) => Some(name.as_str()),
            _ => None,
        };
        if let Some(sym_name) = sym_method_name
            && actions_class_name.is_none_or(|cn| self.actions_answer_method(cn, sym_name))
        {
            let result = self.call_action_method(actions, sym_name, match_obj);
            return match result {
                Err(e) if e.is_method_not_found_for(sym_name) => {
                    // Fall back to the plain rule name.
                    if actions_class_name.is_none_or(|cn| self.actions_answer_method(cn, rule_name))
                    {
                        (true, self.call_action_method(actions, rule_name, match_obj))
                    } else {
                        (true, Ok(match_obj.clone()))
                    }
                }
                other => (true, other),
            };
        }
        if actions_class_name.is_none_or(|cn| self.actions_answer_method(cn, rule_name)) {
            return (true, self.call_action_method(actions, rule_name, match_obj));
        }
        (false, Ok(match_obj.clone()))
    }

    /// `actions.<method>(match_obj)`: the compiled user method when one
    /// resolves, else the general method funnel.
    // Cost: O(1) when the method resolves from the generation-keyed resolve
    // cache; the funnel fallback is the general call's own cost.
    fn call_action_method(
        &mut self,
        actions: &Value,
        method: &str,
        match_obj: &Value,
    ) -> Result<Value, RuntimeError> {
        if let Some(result) =
            self.try_dispatch_compiled_method_direct(actions, method, std::slice::from_ref(match_obj))
        {
            return result;
        }
        self.call_method_with_values(actions.clone(), method, vec![match_obj.clone()])
    }
}
