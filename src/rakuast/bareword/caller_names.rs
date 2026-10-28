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
    /// A setting term the caller's registry also lists (`Less`,
    /// `PromiseStatus::Kept`) keeps its setting classification (`Term::Enum`).
    // Cost: O(n), n = number of caller names.
    pub(in crate::rakuast) fn install(types: Vec<String>, terms: Vec<String>) -> Self {
        let mut names = HashMap::new();
        names.extend(types.into_iter().map(|n| (n, DeclaredKind::Type)));
        names.extend(
            terms
                .into_iter()
                .filter(|n| crate::rakuast::core_term_names::kind(n).is_none())
                .map(|n| (n, DeclaredKind::Term)),
        );
        Self(CALLER_NAMES.with(|c| std::mem::replace(&mut *c.borrow_mut(), names)))
    }
}

impl Drop for CallerNames {
    fn drop(&mut self) {
        CALLER_NAMES.with(|c| *c.borrow_mut() = std::mem::take(&mut self.0));
    }
}

thread_local! {
    /// The type names the calling scope of a `Str.AST` declares. A bare class
    /// name renders as a `Term::Name` (they are `CALLER_NAMES` terms), but the
    /// `is NAME` trait of a declaration in the source names a type when NAME is
    /// one of these.
    static CALLER_TRAIT_TYPES: RefCell<std::collections::HashSet<String>> =
        RefCell::new(std::collections::HashSet::new());
}

/// RAII guard installing the type names of the calling scope for `is NAME`
/// traits (see `CALLER_TRAIT_TYPES`).
pub(in crate::rakuast) struct CallerTraitTypes(std::collections::HashSet<String>);

impl CallerTraitTypes {
    // Cost: O(n), n = number of caller type names.
    pub(in crate::rakuast) fn install(types: Vec<String>) -> Self {
        let names: std::collections::HashSet<String> = types.into_iter().collect();
        Self(CALLER_TRAIT_TYPES.with(|c| std::mem::replace(&mut *c.borrow_mut(), names)))
    }
}

impl Drop for CallerTraitTypes {
    fn drop(&mut self) {
        CALLER_TRAIT_TYPES.with(|c| *c.borrow_mut() = std::mem::take(&mut self.0));
    }
}

/// Whether the calling scope declares the type `name`, for an `is NAME` trait.
// Cost: O(k), k = length of `name`.
pub(super) fn declares_trait_type(name: &str) -> bool {
    CALLER_TRAIT_TYPES.with(|c| c.borrow().contains(name))
}

/// What the calling scope of the `EVAL` string being converted declares `name` as.
// Cost: O(k), k = length of `name`.
pub(super) fn kind(name: &str) -> Option<DeclaredKind> {
    CALLER_NAMES.with(|c| c.borrow().get(name).copied())
}
