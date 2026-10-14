//! A `Match` answers `Any`'s `self.list.METHOD` methods on its positional
//! captures (ADR-11276 §9.98, #13161).
//!
//! Rakudo's `Match` is a `Capture`, and `Any` declares `min`, `sort`,
//! `Array`, `grep`, ... as `self.list.METHOD` (and `Hash` as reading
//! `self.hash`), where `Capture.list` is the positional captures and
//! `Capture.hash` the named ones. So both native dispatch entries
//! (`try_native_method` and `call_method_with_values`) swap such a receiver
//! for that list before anything answers the call; a grammar that declares
//! the method itself keeps its own.

use super::Interpreter;
use crate::value::Value;

impl Interpreter {
    /// The list (or, for `Hash`, the map) standing in for `target` when it is
    /// a `Match` and `method` is one of `Any`'s `self.list.METHOD` methods its
    /// grammar does not declare; `None` otherwise.
    // Cost: O(1) for a non-Match receiver or another name; for a Match the
    // memoized `has_user_method` probe plus O(p) to copy p positional captures.
    pub(crate) fn match_list_receiver(&mut self, target: &Value, method: &str) -> Option<Value> {
        let receiver = target.match_list_receiver(method)?;
        if self.has_user_method(target.match_dispatch_class(), method) {
            return None;
        }
        Some(receiver)
    }
}
