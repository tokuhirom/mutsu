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

/// Convert a **byte** offset (what `str::find` returns) into the grapheme
/// offset Raku reports. `byte_pos` must lie on a grapheme boundary, which it
/// does for the result of a substring search.
pub(crate) fn grapheme_offset(s: &str, byte_pos: usize) -> usize {
    if is_flat_ascii(s) {
        return byte_pos;
    }
    grapheme_units(&s[..byte_pos]).len()
}

/// The byte offset at which `s`'s **final** grapheme starts, or `s.len()` when
/// `s` is empty.
///
/// An incremental decoder cannot know that the last grapheme it decoded is
/// finished: the very next byte could be a combining mark that extends it. This
/// is the split point such a decoder holds back — see
/// `feed_utf8_incremental` in `src/runtime/native_proc_async.rs`.
pub(crate) fn last_grapheme_start(s: &str) -> usize {
    if s.is_empty() {
        return 0;
    }
    if is_flat_ascii(s) {
        return s.len() - 1;
    }
    grapheme_units(s)
        .last()
        .map(|g| s.len() - g.len())
        .unwrap_or(0)
}

/// Whether nothing that can follow `s` could merge with its final grapheme, so
/// an incremental decoder may release that grapheme instead of holding it back.
///
/// UAX #29 GB4 breaks after LF and after any `Control`, whatever comes next, so
/// those two are safe to hand out immediately. `CR` is the one exception — GB3
/// joins `CR × LF` — and every other grapheme can still be extended by a
/// following `Extend`/`ZWJ`/`SpacingMark`/jamo/`Regional_Indicator`, while a
/// trailing `Prepend` attaches to whatever follows it. All of those stay held.
///
/// This is what keeps line-oriented output streaming: a chunk ending in a
/// newline is delivered whole, so a `.lines` consumer sees the line as the child
/// writes it rather than waiting for the next read (see
/// `feed_utf8_incremental` in `src/runtime/native_proc_async.rs`).
pub(crate) fn final_grapheme_is_unextendable(s: &str) -> bool {
    match s.chars().next_back() {
        // The overwhelmingly common case, answered without a property lookup.
        Some('\n') => true,
        // Every ASCII control except CR is gc=Cc, hence GCB=Control.
        Some(c) if c.is_ascii() => c.is_ascii_control() && c != '\r',
        Some(c) => crate::builtins::uniprop::unicode_grapheme_cluster_break(c) == "Control",
        None => false,
    }
}

/// The number of graphemes in `s` — the length `index`/`substr` positions are
/// measured against, and what `.chars` reports.
pub(crate) fn grapheme_len(s: &str) -> usize {
    if is_flat_ascii(s) {
        return s.len();
    }
    grapheme_units(s).len()
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
    fn last_grapheme_start_holds_back_one_cluster() {
        assert_eq!(last_grapheme_start(""), 0);
        assert_eq!(last_grapheme_start("abc"), 2);
        // CRLF is one grapheme, so a decoder holds both bytes back together.
        assert_eq!(last_grapheme_start("a\r\n"), 1);
        // "e" + COMBINING ACUTE ACCENT: the base codepoint is held back with
        // its mark, which is the whole point of the holdback.
        let s = "ab\u{65}\u{301}";
        assert_eq!(&s[last_grapheme_start(s)..], "\u{65}\u{301}");
    }

    #[test]
    fn only_lf_and_controls_are_unextendable() {
        // A newline can never be extended, so it is released with its line.
        assert!(final_grapheme_is_unextendable("Started\n"));
        assert!(final_grapheme_is_unextendable("a\r\n"));
        assert!(final_grapheme_is_unextendable("x\t"));
        // CR is held: the next read may start with the LF that joins it.
        assert!(!final_grapheme_is_unextendable("a\r"));
        // An ordinary character may be extended by a combining mark.
        assert!(!final_grapheme_is_unextendable("abc"));
        assert!(!final_grapheme_is_unextendable("e\u{301}"));
        assert!(!final_grapheme_is_unextendable(""));
    }

    #[test]
    fn combining_marks_count_as_one() {
        // "e" + COMBINING ACUTE ACCENT is one grapheme, two codepoints.
        let s = "a\u{65}\u{301}b";
        assert_eq!(grapheme_len(s), 3);
        assert_eq!(grapheme_offset(s, s.find('b').unwrap()), 2);
    }
}
