//! The names an `EVAL` string's calling scope declares (see [`CallerNames`]).

use std::cell::RefCell;
use std::collections::HashMap;

use super::DeclaredKind;

thread_local! {
    /// The names an `EVAL` string's calling scope declares: the unit being
    /// converted is only the string, so its own declarations cannot say what
    /// `aa` of `enum E <aa bb>; EVAL "aa"` is. Empty outside such a conversion.
    static CALLER_NAMES: RefCell<HashMap<String, DeclaredKind>> =
        RefCell::new(HashMap::new());
}

/// RAII guard installing an `EVAL` string's caller names (see `CALLER_NAMES`).
pub(in crate::rakuast) struct CallerNames(HashMap<String, DeclaredKind>);

impl CallerNames {
    // Cost: O(n), n = number of caller names.
    pub(in crate::rakuast) fn install(types: Vec<String>, terms: Vec<String>) -> Self {
        let mut names = HashMap::new();
        names.extend(types.into_iter().map(|n| (n, DeclaredKind::Type)));
        names.extend(terms.into_iter().map(|n| (n, DeclaredKind::Term)));
        Self(CALLER_NAMES.with(|c| std::mem::replace(&mut *c.borrow_mut(), names)))
    }
}

impl Drop for CallerNames {
    fn drop(&mut self) {
        CALLER_NAMES.with(|c| *c.borrow_mut() = std::mem::take(&mut self.0));
    }
}

/// What the calling scope of the `EVAL` string being converted declares `name` as.
// Cost: O(k), k = length of `name`.
pub(super) fn kind(name: &str) -> Option<DeclaredKind> {
    CALLER_NAMES.with(|c| c.borrow().get(name).copied())
}
