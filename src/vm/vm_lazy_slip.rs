//! A method called on a lazy-tail Slip (#13117, `builtins::lazy_slip`) reads
//! the Slip's elements as the lazy concatenation of its items, the same lazy
//! list the list constructor builds when the Slip lands in a list literal.

use super::*;

impl Interpreter {
    /// The receiver a method call on `target` reads: the lazy concatenation of
    /// a lazy-tail Slip's items, or `target` unchanged. A method about the
    /// Slip itself rather than its elements (its type, identity, `.Slip`,
    /// `.raku`) keeps the Slip; `.gist` is the lazy list's `(...)`.
    // Cost: O(k) to build the concatenation, k = items in the Slip; O(1) for
    // any other receiver.
    pub(crate) fn lazy_slip_receiver(target: Value, method: &str) -> Value {
        if matches!(
            method,
            "WHAT" | "WHICH" | "WHO" | "HOW" | "VAR" | "Slip" | "raku" | "Str" | "item"
                | "defined" | "isa" | "does" | "can"
        ) || method.starts_with('^')
            || !crate::builtins::lazy_slip::has_lazy_tail(&target)
        {
            return target;
        }
        let ValueView::Slip(items) = target.view() else {
            return target;
        };
        let mut elems = Vec::with_capacity(items.len());
        let mut lazy_slots = Vec::new();
        Self::extend_with_slip_items(&mut elems, &mut lazy_slots, &items);
        Self::lazy_literal_with_slipped_tail(elems, &lazy_slots, false)
    }

    /// `.Slip` on a genuinely lazy receiver: the lazy-tail Slip, answered
    /// before any dispatch path that would reify the receiver to copy its
    /// elements (`lazy_slip::lazy_slip_tail`).
    // Cost: O(1).
    pub(super) fn lazy_slip_coercion(target: &Value, method: &str, argc: usize) -> Option<Value> {
        if method != "Slip" || argc != 0 {
            return None;
        }
        crate::builtins::lazy_slip::lazy_slip_tail(target)
    }
}
