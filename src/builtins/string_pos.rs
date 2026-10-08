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

fn is_utf8_c8_payload(g: &str) -> bool {
    g == "x"
}

fn is_hex_digit(g: &str) -> bool {
    g.len() == 1 && g.as_bytes()[0].is_ascii_hexdigit()
}

/// True when `s` has one grapheme per byte, so byte offsets *are* grapheme
/// offsets. ASCII guarantees one byte per codepoint; the only ASCII sequence
/// that merges two codepoints into one grapheme is `\r\n`.
#[inline]
fn is_flat_ascii(s: &str) -> bool {
    s.is_ascii() && !s.contains("\r\n")
}

/// Split `s` into its graphemes, the units a positional string method indexes.
///
/// Cost: O(n), n = bytes of `s`, and it allocates a `Vec` of n entries even on
/// the flat-ASCII fast path. Nothing is cached, so a caller that indexes one
/// grapheme pays for all of them.
pub(crate) fn grapheme_units(s: &str) -> Vec<&str> {
    if is_flat_ascii(s) {
        return (0..s.len()).map(|i| &s[i..i + 1]).collect();
    }
    let raw: Vec<(usize, &str)> = s.grapheme_indices(true).collect();
    let mut units = Vec::with_capacity(raw.len());
    let mut i = 0;
    while i < raw.len() {
        let (start, grapheme) = raw[i];
        if grapheme == crate::runtime::utf8_c8::SYNTHETIC_MARKER_STR
            && i + 3 < raw.len()
            && is_utf8_c8_payload(raw[i + 1].1)
            && is_hex_digit(raw[i + 2].1)
            && is_hex_digit(raw[i + 3].1)
        {
            let end = raw[i + 3].0 + raw[i + 3].1.len();
            units.push(&s[start..end]);
            i += 4;
        } else {
            units.push(grapheme);
            i += 1;
        }
    }
    units
}

/// The number of graphemes in `s` — the length `index`/`substr` positions are
/// measured against, and what `.chars` reports.
///
/// Cost: O(n), n = bytes of `s`, recomputed on every call (flat ASCII: two
/// byte scans; otherwise a full segmentation pass into a `Vec`).
pub(crate) fn grapheme_len(s: &str) -> usize {
    if is_flat_ascii(s) {
        return s.len();
    }
    crate::builtins::grapheme_index::Units::from(s, 0).count()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn crlf_is_one_grapheme_everywhere() {
        let s = "AAA\r\n--bnd";
        assert_eq!(grapheme_len(s), 9);
        assert_eq!(grapheme_units(s)[3], "\r\n");
    }

    #[test]
    fn flat_ascii_takes_the_fast_path() {
        let s = "hello world";
        assert_eq!(grapheme_len(s), 11);
        assert_eq!(grapheme_units(s).len(), 11);
    }

    #[test]
    fn combining_marks_count_as_one() {
        // "e" + COMBINING ACUTE ACCENT is one grapheme, two codepoints.
        let s = "a\u{65}\u{301}b";
        assert_eq!(grapheme_len(s), 3);
    }
}
