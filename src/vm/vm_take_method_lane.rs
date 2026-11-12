//! `$obj.take` on an instance of a user class: `Mu.take`, answered without the
//! method cascade.
//!
//! `Mu.take` is `method take { take self }`. On an instance of a user class that
//! does not declare `take` itself (directly, through a role, or by a `handles`
//! delegation), the generic path walks the whole instance cascade
//! (`call_method_with_values`: the by-name groups, the native probes, the MRO
//! walk, ~35k instructions) to reach the `Mu` row, which runs
//! [`Interpreter::take_statement`]. EEL's tree walk
//! (`gather { take $node; for $node.children { .take for nodes($_) } }`) takes
//! every descendant once per ancestor this way (#17530).
//!
//! The lane runs `take_statement` directly when the call is an unquoted,
//! argument-less `.take` with no modifier, nothing in the program wraps a
//! method or declares a `^find_method`, and the receiver is such an instance.
//! An instance carrying a built-in payload (`class MyInt is Int`), whose
//! `.take` the cascade answers on its payload, and every other receiver fall
//! through to the generic path unchanged.

use super::*;

impl Interpreter {
    /// Answer `.take` on a user-class instance, popping the receiver. `None`
    /// leaves the stack untouched and the call to the generic path.
    // Cost: O(1) for the guard (a name compare, a set probe, a memoized
    // `has_user_method` probe and the payload probe); a hit costs
    // `take_statement`.
    pub(super) fn try_take_method_lane(
        &mut self,
        method: &str,
        arity: u32,
        modifier_idx: Option<u32>,
        quoted: bool,
        arg_sources_idx: Option<u32>,
    ) -> Option<Result<Value, RuntimeError>> {
        if method != "take"
            || arity != 0
            || modifier_idx.is_some()
            || quoted
            || arg_sources_idx.is_some()
            || self.accessor_ref_pending
            || self.has_any_wrap_chains()
            || crate::runtime::find_method_intercept::any_user_find_method()
        {
            return None;
        }
        let target = self.stack.last()?;
        let ValueView::Instance { class_name, .. } = target.view() else {
            return None;
        };
        let class = class_name.resolve();
        if !self.types.user_declared_classes.contains(class.as_str())
            || crate::builtins::native_base::redirect(target, method).is_some()
        {
            return None;
        }
        if self.has_user_method(&class, "take") {
            return None;
        }
        let target = self.stack.pop()?;
        self.pending_call_arg_source_slots.clear();
        self.set_pending_call_arg_sources(None);
        crate::vm::vm_stats::record_dispatch_entry_outcome("callmethod", "take-lane");
        Some(self.take_statement(target.clone()).map(|()| target))
    }
}
