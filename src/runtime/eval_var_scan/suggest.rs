//! The names an undeclared-variable error in `EVAL` may suggest (#18747).

use super::UndeclaredVars;
use crate::value::ValueView;
use std::collections::HashSet;

impl UndeclaredVars<'_> {
    /// The names a suggestion may offer: the EVAL'd code's own declarations
    /// and the variables of the scope the code is compiled in -- the caller's,
    /// or the `context =>` frame's -- which it sees too (#18747). Only
    /// variables are offered, never a type or routine stored under a bare
    /// name, a twigil variable or an internal one.
    // Cost: O(d + e), d = the code's declarations, e = the visible names
    // (only on the error path).
    pub(super) fn all_declared(&self) -> HashSet<String> {
        let mut names: HashSet<String> = self.scopes.iter().flatten().cloned().collect();
        if self.forward.is_some() {
            return names;
        }
        // The static scopes around the `EVAL` call, unless `context =>`
        // moved the code to another frame's scope.
        if self.interp.pending_eval_context_routine.is_none()
            && let Some(code) = self.caller_code()
        {
            names.extend(code.eval_scope_names.iter().filter(|n| is_offerable(n)).cloned());
        }
        for (key, value) in self.interp.env().iter() {
            let key = key.resolve();
            let is_variable = !matches!(
                value.deref_container().view(),
                ValueView::Package(_) | ValueView::Routine { .. } | ValueView::Sub(_)
            );
            if is_variable && is_offerable(&key) {
                names.insert(key.to_string());
            }
        }
        names
    }
}

/// Whether a variable name may be offered as a suggestion: a user-spelled
/// name, not a twigil variable (`$?FILE`, `$*OUT`), a package-qualified one
/// or an internal one (`__mutsu_*`, a leading `_`).
// Cost: O(|name|).
fn is_offerable(name: &str) -> bool {
    let bare = name.trim_start_matches(['$', '@', '%', '&']);
    bare.starts_with(|c: char| c.is_alphabetic()) && !bare.contains("::")
}
