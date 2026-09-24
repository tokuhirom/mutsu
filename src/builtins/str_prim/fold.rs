//! Case- and mark-insensitive comparison: the one implementation behind
//! `nqp::indexic`/`indexim`/`indexicim`/`eqatic` and every `Str` method's
//! `:i`/`:ignorecase`/`:m`/`:ignoremark` adverb (`index`, `indices`,
//! `contains`, `starts-with`, `ends-with`, `substr-eq`).
//!
//! Each **grapheme** is folded on its own -- `:i` is the Unicode full case
//! fold (`fc`, so `ß` folds to `ss`), `:m` drops the combining marks -- and a
//! hit must start and end on the boundary between two folded graphemes. That
//! is how MoarVM answers `"straße".index("SS", :i)` with 4 while
//! `"aİ".index("i", :i)` is no hit (`İ` folds to `i` + a combining dot, so a
//! bare `i` would end inside it).

use std::borrow::Cow;

use crate::builtins::grapheme_index::{GraphemeIndex, Units};

/// How two strings are compared.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Fold {
    Exact,
    /// `:i` -- Unicode full case fold.
    Case,
    /// `:m` -- combining marks dropped.
    Mark,
    /// `:i :m`.
    CaseMark,
}

impl Fold {
    pub(crate) fn new(ignore_case: bool, ignore_mark: bool) -> Fold {
        match (ignore_case, ignore_mark) {
            (false, false) => Fold::Exact,
            (true, false) => Fold::Case,
            (false, true) => Fold::Mark,
            (true, true) => Fold::CaseMark,
        }
    }

    #[inline]
    pub(crate) fn is_exact(self) -> bool {
        self == Fold::Exact
    }

    /// One grapheme, folded.
    fn unit(self, g: &str) -> Cow<'_, str> {
        if g.len() == 1 {
            // A lone ASCII byte: no marks to strip, and its case fold is its
            // ASCII lowercase.
            let b = g.as_bytes()[0];
            return match self {
                Fold::Case | Fold::CaseMark if b.is_ascii_uppercase() => {
                    Cow::Owned((b.to_ascii_lowercase() as char).to_string())
                }
                _ => Cow::Borrowed(g),
            };
        }
        let stripped: Cow<'_, str> = match self {
            Fold::Mark | Fold::CaseMark => {
                use unicode_normalization::UnicodeNormalization;
                let s: String = g
                    .nfd()
                    .filter(|c| !unicode_normalization::char::is_combining_mark(*c))
                    .collect();
                // A grapheme made only of marks keeps them rather than
                // folding to nothing.
                if s.is_empty() {
                    Cow::Borrowed(g)
                } else {
                    Cow::Owned(s)
                }
            }
            _ => Cow::Borrowed(g),
        };
        match self {
            Fold::Case | Fold::CaseMark => {
                Cow::Owned(crate::builtins::unicode::grapheme_foldcase(&stripped))
            }
            _ => stripped,
        }
    }
}

/// `s` with every grapheme folded.
///
/// Cost: O(n), n = chars of `s`.
pub(crate) fn fold_str(s: &str, fold: Fold) -> Cow<'_, str> {
    if fold.is_exact() {
        return Cow::Borrowed(s);
    }
    let mut out = String::with_capacity(s.len());
    for (_, g) in Units::from(s, 0) {
        out.push_str(&fold.unit(g));
    }
    Cow::Owned(out)
}

/// The folded haystack from grapheme `from` on, with the folded byte offset
/// at which each original grapheme starts (plus the end), so a hit in the
/// folded text can be mapped back and checked against grapheme boundaries.
struct Folded {
    text: String,
    starts: Vec<usize>,
}

fn fold_from(text: &str, idx: &GraphemeIndex, from: usize, fold: Fold) -> Folded {
    let mut out = String::new();
    let mut starts = Vec::new();
    for (_, g) in Units::from(text, idx.byte_at(text, from)) {
        starts.push(out.len());
        out.push_str(&fold.unit(g));
    }
    starts.push(out.len());
    Folded { text: out, starts }
}

/// The first grapheme at or after `from` where `needle` occurs under `fold`
/// (see [`super::index`], which answers the exact and empty-needle cases).
///
/// Cost: O(n - from) to fold the haystack, plus the search.
pub(crate) fn find(
    text: &str,
    idx: &GraphemeIndex,
    from: usize,
    needle: &str,
    fold: Fold,
) -> Option<usize> {
    if idx.is_flat() && needle.is_ascii() {
        // One byte per grapheme on both sides: folding is ASCII lowercasing
        // (or nothing, for `:m`), and every byte offset is a boundary.
        let hay = &text.as_bytes()[from.min(text.len())..];
        let needle = needle.as_bytes();
        let eq = |a: &u8, b: &u8| match fold {
            Fold::Mark => a == b,
            _ => a.eq_ignore_ascii_case(b),
        };
        if needle.len() > hay.len() {
            return None;
        }
        return (0..=hay.len() - needle.len())
            .find(|&i| {
                hay[i..i + needle.len()]
                    .iter()
                    .zip(needle)
                    .all(|(a, b)| eq(a, b))
            })
            .map(|i| from + i);
    }
    let needle = fold_str(needle, fold);
    let hay = fold_from(text, idx, from, fold);
    let mut at = 0;
    while let Some(off) = hay.text[at..].find(needle.as_ref()) {
        let b = at + off;
        if let Ok(g) = hay.starts.binary_search(&b)
            && hay.starts.binary_search(&(b + needle.len())).is_ok()
        {
            return Some(from + g);
        }
        at = b + hay.text[b..].chars().next().map_or(1, char::len_utf8);
    }
    None
}

/// Whether `needle` occurs at exactly grapheme `pos` under `fold`.
///
/// Cost: O(m) amortized, m = chars of `needle`: only as many haystack
/// graphemes are folded as it takes to cover the folded needle.
pub(crate) fn eq_at(text: &str, idx: &GraphemeIndex, pos: usize, needle: &str, fold: Fold) -> bool {
    if pos > idx.len() {
        return false;
    }
    if fold.is_exact() {
        let count = if needle.is_ascii() && !needle.contains("\r\n") {
            needle.len()
        } else {
            Units::from(needle, 0).count()
        };
        return super::slice(text, idx, pos, count) == needle;
    }
    let needle = fold_str(needle, fold);
    let mut matched = 0;
    for (_, g) in Units::from(text, idx.byte_at(text, pos)) {
        if matched == needle.len() {
            break;
        }
        let unit = fold.unit(g);
        let rest = &needle[matched..];
        if rest.len() < unit.len() || !rest.starts_with(unit.as_ref()) {
            return false;
        }
        matched += unit.len();
    }
    matched == needle.len()
}
