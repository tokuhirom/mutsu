//! Deciding whether a `.?method` call missed on its own receiver.

use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

impl Interpreter {
    /// Whether `error` is the optional call's own miss on `receiver`, which
    /// `.?` answers with `Nil`. A miss inside a method that was found
    /// propagates instead (#19113).
    ///
    /// The value-level check ([`RuntimeError::is_method_not_found_for_receiver`])
    /// knows the core type hierarchy only. A user class that inherits from a
    /// core type (`class B is Array`) dispatches a miss under the parent's
    /// name, and only the registry knows the receiver is one.
    // Cost: O(m), m = the receiver class's MRO length.
    pub(crate) fn optional_call_missed_receiver(
        &mut self,
        error: &RuntimeError,
        name: &str,
        receiver: &Value,
    ) -> bool {
        if error.is_method_not_found_for_receiver(name, receiver) {
            return true;
        }
        error.is_method_not_found_for(name)
            && error
                .method_not_found_typename()
                .is_some_and(|typename| self.type_matches_value(&typename, receiver))
    }
}
