//! Dispatch a grammar action at the subrule's reduction (ADR-7576).
//!
//! Rakudo calls an action method the moment its rule reduces, during
//! matching, so a `{ ... }` block that follows `<a>` already sees `a`'s action
//! effects and `$<a>.made`. The post-parse walk
//! ([`Interpreter::invoke_grammar_actions`]) runs them after the whole match
//! instead. This module is the reduction-time half: [`file_named_candidate`]
//! calls [`Interpreter::dispatch_reduced_subrule`] right after it builds the
//! capture node of a real-match subrule, and the node's `reduced` flag keeps
//! the walk from running the same action a second time.
//!
//! [`file_named_candidate`]: Interpreter::file_named_candidate

use super::super::*;
use crate::value::regex_caps::{CapNode, MatchTarget};
use std::sync::Arc;

impl Interpreter {
    /// Run the action of the subrule `cap` that just reduced, when an
    /// action-driven parse is live and the matcher is in its real pass.
    ///
    /// A failure is parked in the pending-action-error slot (read by
    /// `take_pending_regex_error` too), the way an embedded `{ ... }` block's
    /// `die` is parked: the match-entry points re-raise it after the engine
    /// unwinds, and a parse that still matched raises it as well.
    // Cost: O(1) when no action-driven parse is live; otherwise one action
    // call plus an O(e) env snapshot, e = the names in scope, only when the
    // rule has an action method.
    pub(in crate::runtime) fn dispatch_reduced_subrule(
        &mut self,
        cap: &Arc<CapNode>,
        rule_name: &str,
    ) {
        if !super::regex_helpers::reduce_dispatch_armed() || cap.reduced.get() {
            return;
        }
        // An earlier action died: the parse is over as far as the program can
        // tell (rakudo unwinds out of `.parse` at the `die`), so no later
        // action runs for the rest of the match.
        if crate::runtime::regex_parse::PENDING_ACTION_ERROR.with(|slot| slot.borrow().is_some()) {
            return;
        }
        let Some(mut actions) = self.regex_state.current_grammar_actions.take() else {
            return;
        };
        // A node with no action method of its own stays for the walk, which
        // still has to descend into any child that was not reduced here.
        if !self.reduction_has_action(&actions, rule_name, cap.sym) {
            self.regex_state.current_grammar_actions = Some(actions);
            return;
        }
        let target = super::regex_helpers::current_match_target()
            .unwrap_or_else(|| MatchTarget::new(""));
        let match_obj = Value::lazy_match(Arc::clone(cap), target);
        let outcome = self.run_reduction_action(match_obj, &mut actions, rule_name);
        self.regex_state.current_grammar_actions = Some(actions);
        cap.reduced.set();
        // A `return` out of the action itself is the action's own result. One
        // that targets an enclosing routine (a `CATCH { default { return … } }`
        // of the caller handling this action's `die`) leaves `.parse` like the
        // exception does, so it is kept too.
        if let Err(error) = outcome
            && (error.return_value.is_none() || error.is_targeted_return())
        {
            crate::runtime::regex_parse::PENDING_ACTION_ERROR.with(|slot| {
                let mut slot = slot.borrow_mut();
                if slot.is_none() {
                    *slot = Some(error);
                }
            });
        }
    }

    /// Whether the actions object answers a method for the rule (or for its
    /// `:sym<>` variant).
    fn reduction_has_action(
        &mut self,
        actions: &Value,
        rule_name: &str,
        sym: Option<Symbol>,
    ) -> bool {
        let Some(class_name) = Self::actions_class_name_of(actions) else {
            return true;
        };
        if let Some(sym) = sym {
            let variant = self.variant_action_method_name(Some(&class_name), rule_name, &sym.resolve());
            if self.actions_answer_method(&class_name, &variant) {
                return true;
            }
        }
        self.actions_answer_method(&class_name, rule_name)
    }

    /// The action call of [`Self::dispatch_reduced_subrule`], with the
    /// engine's own bindings (`$/`, `$_`, the capture variables) put back and
    /// every caller lexical the action rebound reported to the outer VM, as
    /// an embedded `{ ... }` block does (`eval_regex_code_block_body`).
    fn run_reduction_action(
        &mut self,
        match_obj: Value,
        actions: &mut Value,
        rule_name: &str,
    ) -> Result<(), RuntimeError> {
        let snapshot: HashMap<Symbol, Value> =
            self.env.iter().map(|(k, v)| (*k, v.clone())).collect();
        let saved_match = self.env.get("/").cloned();
        let saved_topic = self.env.get("_").cloned();
        let saved_made = self.env.get("made").cloned();
        let saved_in_block = self.regex_state.in_regex_code_block;
        self.regex_state.in_regex_code_block = true;
        let result = self.invoke_grammar_actions(match_obj, actions, rule_name);
        self.regex_state.in_regex_code_block = saved_in_block;
        for (key, saved) in [("/", saved_match), ("_", saved_topic), ("made", saved_made)] {
            match saved {
                Some(value) => {
                    self.env.insert(key.to_string(), value);
                }
                None => {
                    self.env.remove(key);
                }
            }
        }
        for (k, v) in &self.env {
            if snapshot.get(k).is_none_or(|old| !old.same_binding(v)) {
                let name = k.resolve();
                if matches!(name.as_str(), "/" | "_" | "made") {
                    continue;
                }
                if let Some(set) = self.carrier_writes.as_mut() {
                    set.insert(name.clone());
                }
                self.pending_local_updates.push((name, v.clone()));
            }
        }
        result.map(|_| ())
    }
}
