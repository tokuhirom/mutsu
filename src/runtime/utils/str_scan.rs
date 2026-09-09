//! Byte scans for the fixed ASCII markers the name-resolution hot paths test
//! for (`::`, `::&`, `__ANON`).
//!
//! `str::contains(&str)` builds a two-way searcher per call; on the short
//! variable/package names these paths test that setup dominates the scan,
//! and a free-variable read in a routine body asked it several times per
//! read (the package-lexical, unit-lexical and `our`-mirror resolvers each
//! probe `name` for `::` and the running package for `::&`). Callgrind on the
//! vendored `Test.rakumod`'s assertion loop put `is_contained_in` at ~14k
//! instructions per assertion, ~6k of it from `package_scope_lexical` alone
//! (vendor-real-test-module-flip, #7554). A linear byte scan for a
//! two-to-six byte needle is a handful of instructions per haystack byte and
//! needs no setup.

/// `hay` contains the ASCII `needle` (non-empty).
#[inline]
pub(crate) fn has_ascii(hay: impl AsRef<str>, needle: &[u8]) -> bool {
    let h = hay.as_ref().as_bytes();
    if h.len() < needle.len() {
        return false;
    }
    h.windows(needle.len()).any(|w| w == needle)
}

/// `name` is package-qualified: it contains `::`.
#[inline]
pub(crate) fn has_double_colon(name: impl AsRef<str>) -> bool {
    has_ascii(name, b"::")
}

/// `pkg` is a mangled routine-body scope name (`Pkg::&sub/arity`).
#[inline]
pub(crate) fn has_routine_scope_marker(pkg: impl AsRef<str>) -> bool {
    has_ascii(pkg, b"::&")
}

/// `name` is a compiler-synthesised anonymous container slot
/// (`%__ANON_HASH__` / `@__ANON_ARRAY__`).
#[inline]
pub(crate) fn has_anon_marker(name: impl AsRef<str>) -> bool {
    has_ascii(name, b"__ANON")
}

/// `name` is parameterized: it contains a `[`.
///
/// The single-byte twin of [`has_double_colon`]. `str::contains(char)` builds a
/// `CharSearcher` and drives the generic `Searcher` protocol; on the short type
/// names the type-matching paths ask this about, that setup is the whole cost.
/// It was 4.5% of a `Buf.push` loop (#7696), where `type_matches` and the
/// parametric/coercion name parsers each ask it once per type check.
#[inline]
pub(crate) fn has_bracket(name: &str) -> bool {
    name.as_bytes().contains(&b'[')
}

/// `name.split_once('[')`, without the `CharSearcher` setup.
///
/// `[` is ASCII, so its byte offset is always a char boundary and the split is
/// byte-for-byte what `split_once` returns.
#[inline]
pub(crate) fn split_once_bracket(name: &str) -> Option<(&str, &str)> {
    let at = name.as_bytes().iter().position(|&b| b == b'[')?;
    Some((&name[..at], &name[at + 1..]))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scans_agree_with_str_contains() {
        for s in [
            "",
            ":",
            "::",
            "a::b",
            "P::&f/2",
            "%__ANON_HASH__",
            "@__ANON_ARRAY__",
            "__ANO",
            "x",
            "Test",
            "num_of_tests_run",
            "IO::Handle+{M::R}",
        ] {
            assert_eq!(has_double_colon(s), s.contains("::"), "{s:?}");
            assert_eq!(has_routine_scope_marker(s), s.contains("::&"), "{s:?}");
            assert_eq!(has_anon_marker(s), s.contains("__ANON"), "{s:?}");
        }
    }

    #[test]
    fn bracket_scans_agree_with_str() {
        for s in [
            "",
            "[",
            "Buf",
            "Buf[uint8]",
            "Blob[uint8]",
            "CArray[Pointer[void]]",
            "R[Int]",
            "]",
            "Numeric",
            "\u{3042}[Int]",
        ] {
            assert_eq!(has_bracket(s), s.contains('['), "{s:?}");
            assert_eq!(split_once_bracket(s), s.split_once('['), "{s:?}");
        }
    }
}
