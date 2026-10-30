//! The names `::T` type captures introduced in signatures parsed so far.
//!
//! rakudo's `T` in `sub f(::T $x --> T)` is a generic type object (a
//! `GenericHOW` whose `^archetypes.generic` is true) until the call binds it.
//! mutsu has no such type object: it keeps the bare name, so the name is
//! remembered here for `^archetypes` to answer `generic` (static::Analyzer
//! reads it; #15610). A leaf module: the parser writes it, the runtime reads it.

use std::cell::RefCell;
use std::collections::HashSet;

thread_local! {
    static CAPTURES: RefCell<HashSet<String>> = RefCell::new(HashSet::new());
}

/// Remember `name` (`T`) as a type capture.
// Cost: O(1) expected plus a copy of the name on first sight.
pub(crate) fn note(name: &str) {
    CAPTURES.with(|captures| {
        let mut captures = captures.borrow_mut();
        if !captures.contains(name) {
            captures.insert(name.to_string());
        }
    });
}

/// Whether `name` was introduced as a type capture.
// Cost: O(1) expected.
pub(crate) fn is_capture(name: &str) -> bool {
    CAPTURES.with(|captures| captures.borrow().contains(name))
}

#[cfg(test)]
mod tests {
    #[test]
    fn remembers_captures() {
        assert!(!super::is_capture("TCaptureTestOnly"));
        super::note("TCaptureTestOnly");
        assert!(super::is_capture("TCaptureTestOnly"));
    }
}
