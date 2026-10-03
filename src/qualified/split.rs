//! A qualified name taken apart at its `::` separators, decided once per
//! symbol: the read-side counterparts of [`super::qualified`].
//!
//! The hand-written forms -- `name.rsplit_once("::")`,
//! `name.rsplit("::").next()`, `name.split("::")` -- re-scan the text on every
//! call, and the call sites run per dispatch or per variable read. A symbol's
//! text never changes, so each answer is memoized against the symbol.

use crate::symbol::Symbol;

/// `name` split at its last `::` into the package part and the last segment,
/// or `None` when it has no qualifier: `Foo::Bar::baz` -> (`Foo::Bar`, `baz`).
/// The split is purely textual, exactly what `rsplit_once("::")` returns; a
/// leading sigil stays on the package part (`$P::x` -> (`$P`, `x`)), see
/// [`super::split_qualified_var`] for the sigil-aware split.
// Cost: O(1) amortized (one memo probe; the first call per symbol scans it).
pub(crate) fn split_qualified(name: Symbol) -> Option<(Symbol, Symbol)> {
    thread_local! {
        static SPLITS: std::cell::RefCell<rustc_hash::FxHashMap<Symbol, Option<(Symbol, Symbol)>>> =
            std::cell::RefCell::new(rustc_hash::FxHashMap::default());
    }
    if !super::is_qualified(name) {
        return None;
    }
    if let Some(split) = SPLITS.with(|c| c.borrow().get(&name).copied()) {
        return split;
    }
    let split = name
        .as_str()
        .rsplit_once("::")
        .map(|(head, tail)| (Symbol::intern(head), Symbol::intern(tail)));
    SPLITS.with(|c| {
        c.borrow_mut().insert(name, split);
    });
    split
}

/// The last `::` segment of `name`, or `name` itself when it is unqualified:
/// `name.rsplit("::").next()`, decided once per symbol. Unlike
/// [`super::unqualified_part`] a leading sigil is not carried over.
// Cost: O(1) amortized (one memo probe; the first call per symbol scans it).
pub(crate) fn last_segment(name: Symbol) -> Symbol {
    split_qualified(name).map_or(name, |(_, tail)| tail)
}

/// Whether `name` is declared inside `pkg`: `pkg` is one of the packages
/// enclosing `name` (`A::B::c` is inside `A::B` and `A`, not inside `A::B::c`
/// itself nor `A::Bx`). The textual form this replaces is
/// `name.strip_prefix(pkg).is_some_and(|r| r.starts_with("::"))`.
// Cost: O(d), d = number of `::` segments of `name` (each step a memo hit).
pub(crate) fn is_inside_package(name: Symbol, pkg: Symbol) -> bool {
    super::package_parent(name)
        .is_some_and(|parent| super::package_ancestors(parent).any(|a| a == pkg))
}

/// The `::` segments of `name`, outermost first: `A::B::c` -> [`A`, `B`, `c`].
/// Empty segments are kept, as `str::split` keeps them. Built once per symbol.
// Cost: O(1) amortized (one memo probe; the first call per symbol scans it).
pub(crate) fn segments(name: Symbol) -> &'static [Symbol] {
    thread_local! {
        static SEGMENTS: std::cell::RefCell<rustc_hash::FxHashMap<Symbol, &'static [Symbol]>> =
            std::cell::RefCell::new(rustc_hash::FxHashMap::default());
    }
    if let Some(segs) = SEGMENTS.with(|c| c.borrow().get(&name).copied()) {
        return segs;
    }
    // Leaked like the interner's own strings: one slice per distinct
    // qualified name, kept for the life of the process.
    let segs: &'static [Symbol] = Box::leak(
        name.as_str()
            .split("::")
            .map(Symbol::intern)
            .collect::<Vec<_>>()
            .into_boxed_slice(),
    );
    SEGMENTS.with(|c| {
        c.borrow_mut().insert(name, segs);
    });
    segs
}

/// Whether `name`'s trailing `::` segments spell `tail`: `name` is `tail`
/// itself, or ends in `::tail` (`A::B::C` ends with `C` and `B::C`, not with
/// `::C`'s lookalike `XC`).
// Cost: O(|tail|).
pub(crate) fn ends_with_segments(name: &str, tail: &str) -> bool {
    name.strip_suffix(tail)
        .is_some_and(|prefix| prefix.is_empty() || prefix.ends_with("::"))
}

/// The `::` segments of text that is NOT a symbol and is looked at once --
/// an exception message's leading `X::Foo` --  where interning (and so
/// [`segments`]) would leak a table entry per distinct message.
// Cost: O(n), n = length of `text`.
pub(crate) fn text_segments(text: &str) -> std::str::Split<'_, &'static str> {
    text.split("::")
}

/// Whether a parameter's type-constraint text is a type capture (`::T`,
/// `::?CLASS`, `::(expr)`) rather than a nominal type name. A capture binds a
/// type; it never names a package, so it must not be resolved as one.
// Cost: O(1).
#[inline]
pub(crate) fn is_type_capture(constraint: &str) -> bool {
    constraint.as_bytes().starts_with(b"::")
}

/// The name a type capture binds (`::T` -> `T`), or `None` when `constraint`
/// is not a capture (see [`is_type_capture`]).
// Cost: O(1).
#[inline]
pub(crate) fn type_capture_name(constraint: &str) -> Option<&str> {
    // `::` is two ASCII bytes, so index 2 is a char boundary.
    is_type_capture(constraint).then(|| &constraint[2..])
}

/// `name` without the trailing `::` of a stash spelling (`Foo::Bar::` ->
/// `Foo::Bar`, `::` -> ``), or `None` when it does not end in one.
// Cost: O(1) amortized (the split is memoized per symbol).
pub(crate) fn stash_stem(name: Symbol) -> Option<Symbol> {
    split_qualified(name)
        .filter(|(_, tail)| tail.as_str().is_empty())
        .map(|(head, _)| head)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn s(text: &str) -> Symbol {
        Symbol::intern(text)
    }

    #[test]
    fn split_agrees_with_rsplit_once() {
        for text in ["Foo::Bar::baz", "a::b", "$P::x", "::x", "x::", "plain", ""] {
            let want = text.rsplit_once("::");
            let got = split_qualified(s(text)).map(|(h, t)| (h.as_str(), t.as_str()));
            assert_eq!(got, want, "{text}");
        }
    }

    #[test]
    fn last_segment_agrees_with_rsplit_next() {
        for text in ["Foo::Bar::baz", "a::b", "plain", "x::", ""] {
            let want = text.rsplit("::").next().unwrap_or(text);
            assert_eq!(last_segment(s(text)).as_str(), want, "{text}");
        }
    }

    #[test]
    fn inside_package_matches_the_prefix_form() {
        assert!(is_inside_package(s("A::B::c"), s("A::B")));
        assert!(is_inside_package(s("A::B::c"), s("A")));
        assert!(!is_inside_package(s("A::B::c"), s("A::B::c")));
        assert!(!is_inside_package(s("Ax::c"), s("A")));
        assert!(!is_inside_package(s("c"), s("")));
    }

    #[test]
    fn capture_and_stash_helpers_agree_with_the_text_forms() {
        for text in ["::T", "::?CLASS", "T", "", ":T"] {
            assert_eq!(type_capture_name(text), text.strip_prefix("::"), "{text}");
        }
        for text in ["Foo::Bar::", "::", "Foo", "Foo::x"] {
            let want = text.strip_suffix("::");
            assert_eq!(stash_stem(s(text)).map(|x| x.as_str()), want, "{text}");
        }
    }

    #[test]
    fn ends_with_segments_respects_the_separator() {
        assert!(ends_with_segments("A::B::C", "C"));
        assert!(ends_with_segments("A::B::C", "B::C"));
        assert!(ends_with_segments("C", "C"));
        assert!(!ends_with_segments("A::XC", "C"));
        assert!(!ends_with_segments("C", "A::C"));
    }

    #[test]
    fn segments_agree_with_split() {
        for text in ["A::B::c", "plain", "::x", "x::"] {
            let want: Vec<&str> = text.split("::").collect();
            let got: Vec<&str> = segments(s(text)).iter().map(|x| x.as_str()).collect();
            assert_eq!(got, want, "{text}");
        }
    }
}
