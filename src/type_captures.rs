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

/// Whether a parameterized type name (`Array[T]`, `Hash[Int,Array[T]]`) has a
/// type capture among its arguments, at any depth.
// Cost: O(n), n = length of the name.
pub(crate) fn mentions_capture(name: &str) -> bool {
    let Some((_, rest)) = name.split_once('[') else {
        return false;
    };
    let inner = rest.strip_suffix(']').unwrap_or(rest);
    let mut depth = 0usize;
    let mut start = 0;
    let mut args = Vec::new();
    for (at, c) in inner.char_indices() {
        match c {
            '[' => depth += 1,
            ']' => depth = depth.saturating_sub(1),
            ',' if depth == 0 => {
                args.push(&inner[start..at]);
                start = at + 1;
            }
            _ => {}
        }
    }
    args.push(&inner[start..]);
    args.iter()
        .any(|arg| is_capture(arg.trim()) || mentions_capture(arg.trim()))
}

#[cfg(test)]
mod tests {
    #[test]
    fn remembers_captures() {
        assert!(!super::is_capture("TCaptureTestOnly"));
        super::note("TCaptureTestOnly");
        assert!(super::is_capture("TCaptureTestOnly"));
    }

    #[test]
    fn finds_nested_captures() {
        super::note("TNestedOnly");
        assert!(super::mentions_capture("Array[TNestedOnly]"));
        assert!(super::mentions_capture("Hash[Int,Array[TNestedOnly]]"));
        assert!(!super::mentions_capture("Array[Int]"));
        assert!(!super::mentions_capture("TNestedOnly"));
    }
}
