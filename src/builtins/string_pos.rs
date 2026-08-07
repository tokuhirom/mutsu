//! Grapheme-based string positions.
//!
//! A Raku string is a sequence of **graphemes**, so every position and length a
//! string method reports or accepts is a grapheme index. `.chars` already
//! counted graphemes (`str.graphemes(true).count()`), but `substr`, `index`,
//! `rindex` and `indices` counted *codepoints*, so the two scales disagreed on
//! any string holding a multi-codepoint grapheme.
//!
//! `\r\n` is exactly such a grapheme and is everywhere in wire protocols, which
//! made the mismatch load-bearing rather than exotic:
//!
//! ```text
//! "AAA\r\n--bnd".index("--bnd")   raku: 4   mutsu was: 5
//! "\r\nHello".substr(1)           raku: "Hello"   mutsu was: "\nHello"
//! ```
//!
//! Cro's `multipart/form-data` parser walks a body with exactly that pair of
//! calls, so it lost a character per boundary and rejected every multipart body.
//!
//! Both helpers take a fast path for ASCII text with no `\r\n`, where one byte
//! is one grapheme and no segmentation pass is needed.

use unicode_segmentation::UnicodeSegmentation;

/// True when `s` has one grapheme per byte, so byte offsets *are* grapheme
/// offsets. ASCII guarantees one byte per codepoint; the only ASCII sequence
/// that merges two codepoints into one grapheme is `\r\n`.
#[inline]
fn is_flat_ascii(s: &str) -> bool {
    s.is_ascii() && !s.contains("\r\n")
}

/// Split `s` into its graphemes, the units a positional string method indexes.
pub(crate) fn grapheme_units(s: &str) -> Vec<&str> {
    if is_flat_ascii(s) {
        return (0..s.len()).map(|i| &s[i..i + 1]).collect();
    }
    s.graphemes(true).collect()
}

/// Convert a **byte** offset (what `str::find` returns) into the grapheme
/// offset Raku reports. `byte_pos` must lie on a grapheme boundary, which it
/// does for the result of a substring search.
pub(crate) fn grapheme_offset(s: &str, byte_pos: usize) -> usize {
    if is_flat_ascii(s) {
        return byte_pos;
    }
    s[..byte_pos].graphemes(true).count()
}

/// The number of graphemes in `s` — the length `index`/`substr` positions are
/// measured against, and what `.chars` reports.
pub(crate) fn grapheme_len(s: &str) -> usize {
    if is_flat_ascii(s) {
        return s.len();
    }
    s.graphemes(true).count()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn crlf_is_one_grapheme_everywhere() {
        let s = "AAA\r\n--bnd";
        assert_eq!(grapheme_len(s), 9);
        assert_eq!(grapheme_offset(s, s.find("--bnd").unwrap()), 4);
        assert_eq!(grapheme_units(s)[3], "\r\n");
    }

    #[test]
    fn flat_ascii_takes_the_fast_path() {
        let s = "hello world";
        assert_eq!(grapheme_len(s), 11);
        assert_eq!(grapheme_offset(s, 6), 6);
        assert_eq!(grapheme_units(s).len(), 11);
    }

    #[test]
    fn combining_marks_count_as_one() {
        // "e" + COMBINING ACUTE ACCENT is one grapheme, two codepoints.
        let s = "a\u{65}\u{301}b";
        assert_eq!(grapheme_len(s), 3);
        assert_eq!(grapheme_offset(s, s.find('b').unwrap()), 2);
    }
}
