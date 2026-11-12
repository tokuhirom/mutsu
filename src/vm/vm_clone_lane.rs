//! `$obj.clone(:attr(v), ...)` on an instance of a user class: `Mu.clone`,
//! answered without the method cascade.
//!
//! On an instance of a user class that does not declare `clone`, the compiled
//! dispatch found no user method and handed the call to the interpreter's whole
//! instance cascade (`call_method_with_values` -> `dispatch_instance_and_fallback`,
//! ~65k instructions), whose `clone` arm is
//! [`Interpreter::native_instance_clone_value`]. EEL's fork copies its frames,
//! instructions and goals with `.clone(...)` (#17530). The lane calls that arm
//! directly, at the point the compiled dispatch has bound the arguments, for an
//! instance of a class [`Interpreter::plain_method_lane_class_eligible`]
//! admits (user-declared, not a CStruct, no `IO::Handle`/`IO::Path` base) whose
//! inherited `clone` is `Mu`'s (not a built-in parent's, as a `DateTime`
//! subclass's is), with no `clone` of its own and no built-in payload, called
//! with named twiddles only. Every other call falls through unchanged.

use super::*;

impl Interpreter {
    /// `Mu.clone` on a user-class instance; `None` leaves the call to the
    /// cascade.
    // Cost: O(1) for the guard (memoized class probes) plus the clone's own
    // O(a + t), a = attributes, t = twiddles.
    pub(super) fn try_clone_instance_lane(
        &mut self,
        target: &Value,
        method_sym: crate::symbol::Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if method_sym.as_str() != "clone"
            || self.has_any_wrap_chains()
            || crate::runtime::find_method_intercept::any_user_find_method()
            || !args.iter().all(|a| matches!(a.view(), ValueView::Pair(..)))
        {
            return None;
        }
        let ValueView::Instance { class_name, .. } = target.view() else {
            return None;
        };
        if crate::builtins::native_base::redirect(target, "clone").is_some()
            || !self.plain_method_lane_class_eligible(class_name)
            || !self.builtin_method_is_mus(class_name, method_sym)
            || self.has_user_method_sym(class_name.as_str(), method_sym)
        {
            return None;
        }
        self.native_instance_clone_value(target, args)
    }
}
