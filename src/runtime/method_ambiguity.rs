//! Raising the ambiguity a multi-method resolution recorded.
use super::*;

impl Interpreter {
    /// `Err(X::Multi::Ambiguous)` when the resolution that just picked a
    /// candidate of `method` on `class_name` found two equally narrow ones
    /// (`self.dispatch.dispatch_ambiguous`, set by `pick_method_winner`), and
    /// clears the flag. Every call path that runs the candidate a resolution
    /// returned checks it, or the first-listed candidate runs silently
    /// (#18868).
    // Cost: O(1) when not ambiguous; otherwise the candidate signatures'
    // formatting, O(c * p), c = candidates, p = parameters each.
    pub(crate) fn take_dispatch_ambiguity(
        &mut self,
        class_name: &str,
        method: &str,
    ) -> Result<(), RuntimeError> {
        if !self.dispatch.dispatch_ambiguous {
            return Ok(());
        }
        self.dispatch.dispatch_ambiguous = false;
        let sigs = self.format_method_candidate_signatures(class_name, method, None);
        Err(super::methods_signature_errors::make_multi_ambiguous_error(
            method, class_name, &sigs,
        ))
    }
}
