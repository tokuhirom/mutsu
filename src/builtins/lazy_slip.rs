//! The lazy Slip (#13117): `.Slip` and `slip(...)` of a genuinely lazy list.
//!
//! A Slip is a `List` that reifies on demand in Rakudo, so slipping an infinite
//! list must not pull it. mutsu's eager `Value::Slip` holds its items; a lazy
//! source rides it as ONE item, the lazy list itself, which is the shape
//! `|EXPR` already gives a lazy operand: the list constructor turns such an item
//! into a lazy tail (`lazy_literal_with_slipped_tail`), and a method called on
//! the Slip reads that concatenation (`Interpreter::lazy_slip_receiver`).

use crate::value::{Value, ValueView};

/// The lazy-tail Slip of a genuinely lazy `target`: a Slip holding the lazy
/// list as its one item, the shape `|EXPR` gives a lazy operand and the list
/// constructor turns into a lazy tail. An unbounded Range becomes its
/// `.succ`-stepping list first. `None` for anything that can be reified now.
// Cost: O(1).
pub(crate) fn lazy_slip_tail(target: &Value) -> Option<Value> {
    match target.view() {
        ValueView::LazyList(ll) if ll.renders_lazy_placeholder() => {
            Some(Value::slip(vec![target.clone()]))
        }
        _ => crate::runtime::unbounded_range::lazy_list(target)
            .map(|ll| Value::slip(vec![Value::lazy_list(crate::gc::Gc::new(ll))])),
    }
}

/// Whether `value` is a lazy-tail Slip: one of its items is a genuinely lazy
/// list that rides it whole.
// Cost: O(k), k = items in the Slip.
pub(crate) fn has_lazy_tail(value: &Value) -> bool {
    value.is_lazy_tail_slip()
}
