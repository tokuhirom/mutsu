//! A call-site route for `.parse` / `.subparse` on a grammar (#12917).
//!
//! `$grammar.parse($text)` (and `.subparse`) on a grammar type object or
//! instance whose class declares neither method is answered by the native
//! `Grammar` row (`ctors_mop::grammar`). Before this route, the call reached
//! that row only at the end of the general `CallMethod` path: every receiver
//! probe of `exec_call_method_op_impl`, then
//! `try_compiled_method_or_interpret_sym`, `call_method_with_values` and
//! `dispatch_method_by_name_1`, about 30k instructions per call. YAMLish
//! makes one such call per plain scalar it concretizes
//! (`$schema.new.parse($!value)`).
//!
//! # The guard
//!
//! The route answers only when all of these hold, and otherwise leaves the
//! call to the full path untouched:
//!
//! - the name is `parse` or `subparse`, unquoted, with no `.?`/`.^` modifier,
//!   and no accessor-ref marker is pending;
//! - the call site spreads no `|` argument, and every argument is a plain
//!   value (`Value::is_plain_argument`: no `Junction`, `Failure`, lazy `Seq`,
//!   `Proxy` or named `Pair`, each of which the full path treats first);
//! - the receiver is a grammar type object or a grammar instance
//!   (`grammar_receiver_class`, the same test `dispatch_method_by_name_1`'s
//!   arm makes), not a mixin;
//! - the grammar declares no method of that name in its MRO or a composed
//!   role (`grammar_has_user_method`, the arm's own test), no type declares a
//!   `^find_method` that could claim the call, and no method is wrapped.
//!
//! Then the call is what the arm does: the native row with the receiver and
//! the arguments, with the call site's argument sources published as the full
//! path publishes them, and the caller's slots reconciled afterwards as after
//! any method call that can run user code.

use super::*;

impl Interpreter {
    /// The route above. `Some` when it answered the call (the result is on the
    /// stack, or the error is returned), `None` when the full path must run.
    // Cost: O(a) for the guard, a = arguments, plus the parse.
    pub(super) fn try_grammar_parse_site(
        &mut self,
        code: &CompiledCode,
        method: &str,
        arity: u32,
        modifier_idx: Option<u32>,
        quoted: bool,
        arg_sources_idx: Option<u32>,
    ) -> Option<Result<(), RuntimeError>> {
        if modifier_idx.is_some()
            || quoted
            || !matches!(method, "parse" | "subparse")
            || self.accessor_ref_pending
        {
            return None;
        }
        let arity = arity as usize;
        let len = self.stack.len();
        if len < arity + 1 {
            return None;
        }
        let recv_at = len - arity - 1;
        if !self.stack[recv_at + 1..]
            .iter()
            .all(Value::is_plain_argument)
        {
            return None;
        }
        let target = self.stack[recv_at].clone();
        if !(target.is_package_value() || target.is_instance_value()) {
            return None;
        }
        let grammar = match self.grammar_receiver_class(&target) {
            Ok(Some(grammar)) => grammar,
            _ => return None,
        };
        if self.grammar_has_user_method(&grammar, method)
            || self.has_any_wrap_chains()
            || self.user_find_method_lookup(&target, method).is_some()
        {
            return None;
        }
        let decoded = self.decode_arg_sources_shared(code, arg_sources_idx);
        if decoded.as_ref().is_some_and(|(_, spreads)| *spreads) {
            return None;
        }
        self.set_pending_call_arg_sources_shared(decoded.map(|(names, _)| names));
        let mut full = Vec::with_capacity(arity + 1);
        full.push(target);
        full.extend(Self::hllize_call_args(
            self.stack.drain(recv_at + 1..).collect(),
        ));
        self.stack.truncate(recv_at);
        self.dispatch.method_dispatch_pure = false;
        let result =
            crate::builtins::method_table::invoke_owner_raw(self, &["Grammar"], method, &full)
                .unwrap_or_else(|| {
                    Err(RuntimeError::new(format!(
                        "No such method '{method}' for invocant of type '{grammar}'"
                    )))
                });
        match result {
            Ok(value) => {
                self.stack.push(value);
                self.drain_and_reconcile_after_cached_call(code);
                Some(Ok(()))
            }
            Err(error) => Some(Err(error)),
        }
    }
}
