//! A qualified name taken apart at its `::` separators, decided once per
//! symbol: the read-side counterparts of [`super::qualified`].
//!
//! The hand-written forms -- `name.rsplit_once("::")`,
//! `name.rsplit("::").next()`, `name.split("::")` -- re-scan the text on every
//! call, and the call sites run per dispatch or per variable read. A symbol's
//! text never changes, so each answer is memoized against the symbol.

use crate::symbol::Symbol;

/// Whether the text before `at` ends the way a categorical name's bracket group
/// is introduced: `term:`, `infix:sym` -- a colon pair, optionally a named
/// adverb -- and not a `::` package separator (`Foo::<...>`, `Foo::Bar<...>`).
fn opens_category_group(text: &str, at: usize) -> bool {
    let head = text[..at].trim_end_matches(|c: char| c.is_alphanumeric() || c == '_' || c == '-');
    head.ends_with(':') && !head.ends_with("::")
}

/// The byte offset just past the bracket group that opens at `open`, or `None`
/// when `open` does not open a categorical's group (or it is never closed).
fn category_group_end(text: &str, open: usize) -> Option<usize> {
    let close = match text[open..].chars().next()? {
        '<' => '>',
        '\u{ab}' => '\u{bb}', // « »
        _ => return None,
    };
    if !opens_category_group(text, open) {
        return None;
    }
    let inner = open + text[open..].chars().next()?.len_utf8();
    let rel = text[inner..].find(close)?;
    Some(inner + rel + close.len_utf8())
}

/// The byte offsets of the `::` package separators of `text`, left to right.
///
/// A `::` inside a categorical's bracket group is part of the NAME, not a
/// qualifier: `term:<Foo::Bar>` is the term `Foo::Bar` (one name), whereas
/// `Pkg::term:<Foo::Bar>` is that term inside package `Pkg`. Every layer that
/// takes a name apart at `::` must agree on this, or one layer qualifies a
/// name another layer treats as plain.
// Cost: O(|text|).
pub(crate) fn separators(text: &str) -> impl Iterator<Item = usize> + '_ {
    let bytes = text.as_bytes();
    let mut pos = 0;
    std::iter::from_fn(move || {
        while pos < bytes.len() {
            match bytes[pos] {
                b':' if bytes.get(pos + 1) == Some(&b':') => {
                    pos += 2;
                    return Some(pos - 2);
                }
                b'<' | 0xC2 => {
                    // `<` is ASCII; `«` is the two bytes C2 AB, so a C2 byte is
                    // the start of that character only when AB follows.
                    let opens = bytes[pos] == b'<' || bytes.get(pos + 1) == Some(&0xAB);
                    if opens && let Some(end) = category_group_end(text, pos) {
                        pos = end;
                        continue;
                    }
                    pos += 1;
                }
                _ => pos += 1,
            }
        }
        None
    })
}

/// The byte offset of the last `::` package separator of `text`, or `None` when
/// the name has no qualifier (see [`separators`]).
///
/// A name without a bracket in front of its last `::` -- nearly every name --
/// is answered by the plain reverse search, with no forward scan.
// Cost: O(|text|).
pub(crate) fn last_separator(text: &str) -> Option<usize> {
    let at = text.rfind("::")?;
    if !text[..at].contains(['<', '\u{ab}']) {
        return Some(at);
    }
    separators(text).last()
}

/// Whether `text` carries a `::` package separator (see [`separators`]).
// Cost: O(|text|).
pub(crate) fn has_separator(text: &str) -> bool {
    let Some(at) = text.find("::") else {
        return false;
    };
    !text[..at].contains(['<', '\u{ab}']) || separators(text).next().is_some()
}

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
    let text = name.as_str();
    let split = last_separator(text)
        .map(|at| (Symbol::intern(&text[..at]), Symbol::intern(&text[at + 2..])));
    SPLITS.with(|c| {
        c.borrow_mut().insert(name, split);
    });
    split
}

/// `name` split at its FIRST `::` into the leading segment and the rest
/// (`A::B::c` -> (`A`, `B::c`)), or `None` when it is unqualified: the
/// memoized `split_once("::")`.
// Cost: O(1) amortized (the segment split is memoized per symbol).
pub(crate) fn split_first(name: Symbol) -> Option<(&'static str, &'static str)> {
    if !super::is_qualified(name) {
        return None;
    }
    let text = name.as_str();
    let head = segments(name)[0].as_str();
    Some((head, &text[head.len() + 2..]))
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
    let text = name.as_str();
    let mut segs = Vec::new();
    let mut start = 0;
    for at in separators(text) {
        segs.push(Symbol::intern(&text[start..at]));
        start = at + 2;
    }
    segs.push(Symbol::intern(&text[start..]));
    let segs: &'static [Symbol] = Box::leak(segs.into_boxed_slice());
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
    fn split_first_agrees_with_split_once() {
        for text in ["A::B::c", "a::b", "plain", "::x", "x::"] {
            assert_eq!(split_first(s(text)), text.split_once("::"), "{text}");
        }
    }

    #[test]
    fn a_double_colon_inside_a_category_group_is_not_a_separator() {
        // The term `Foo::Bar` is one name; so is the same name in `«»` form.
        for text in [
            "term:<Foo::Bar>",
            "term:\u{ab}Foo::Bar\u{bb}",
            "term:<Foo::Bar>/0",
            "infix:sym<a::b>",
            "&term:<Foo::Bar>",
        ] {
            assert!(!has_separator(text), "{text}");
            assert_eq!(last_separator(text), None, "{text}");
            assert_eq!(split_qualified(s(text)), None, "{text}");
            assert!(!crate::qualified::is_qualified(s(text)), "{text}");
            assert_eq!(segments(s(text)).len(), 1, "{text}");
        }
    }

    #[test]
    fn a_package_in_front_of_a_category_group_still_qualifies_it() {
        let text = "Pkg::term:<Foo::Bar>";
        assert!(has_separator(text));
        assert_eq!(last_separator(text), Some(3));
        let (head, tail) = split_qualified(s(text)).expect("qualified");
        assert_eq!((head.as_str(), tail.as_str()), ("Pkg", "term:<Foo::Bar>"));
        let segs: Vec<&str> = segments(s("A::B::term:<X::Y>"))
            .iter()
            .map(|x| x.as_str())
            .collect();
        assert_eq!(segs, ["A", "B", "term:<X::Y>"]);
    }

    #[test]
    fn a_type_argument_list_is_not_a_category_group() {
        // `Foo::Bar<...>` and `Foo::<...>` follow a package separator, not a
        // categorical colon pair, so their `::` keep qualifying.
        for text in ["Foo::Bar<X::Y>", "Foo::<X::Y>"] {
            assert!(has_separator(text), "{text}");
            assert_eq!(last_separator(text), text.rfind("::"), "{text}");
        }
        // An unclosed group is just text.
        assert!(has_separator("term:<Foo::Bar"));
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
