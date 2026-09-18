//! Deciding what an in-place `Str` append has to renormalize (#8725).
//!
//! `Value::str_appended_nfc` may only grow the accumulated buffer if the
//! result is still NFC, because every path that produces a `Str` normalizes
//! it and the rest of the interpreter (`.chars`, `eq`, hashing) relies on
//! that. Re-running NFC over the whole concatenation restores the invariant
//! but costs O(len) per append, which is exactly the quadratic behaviour the
//! fused `$s ~= ...` opcode exists to remove (#8695).
//!
//! The way out is that NFC is *local*: normalization only ever rearranges or
//! composes characters inside one "normalization segment", and a segment can
//! never start before a character that has a **boundary before** it — one
//! with canonical combining class 0 that is not itself allowed to compose
//! backwards (`NFC_QC = Yes`). Concatenating two NFC strings across such a
//! boundary yields an NFC string, with no work at all.
//!
//! So the plan for a given suffix is one of two shapes:
//!
//! - [`StrAppendPlan::Direct`] — the suffix begins at a boundary, so the join
//!   cannot compose. Normalizing the *suffix alone* (O(len(suffix))) is
//!   enough, and the accumulated buffer is not read at all. This covers every
//!   ASCII suffix (the case #8695 already handled) plus the overwhelmingly
//!   common non-ASCII ones: a snowman, a CJK ideograph, an emoji, an
//!   already-composed accented letter.
//! - [`StrAppendPlan::Join`] — the suffix begins with a combining mark or a
//!   Hangul V/T jamo, so it composes or reorders with what came before
//!   (`"e"` ~ `"\x[301]"` is one `é`; `"\x[1100]"` ~ `"\x[1161]"` is one
//!   syllable). Only a bounded window around the join then needs redoing:
//!   [`nfc_join_window`] backs up over the accumulated string's trailing
//!   combining run to the last boundary and reports where to splice.

use std::borrow::Cow;
use unicode_normalization::char::canonical_combining_class;
use unicode_normalization::{IsNormalized, UnicodeNormalization, is_nfc_quick};

/// How far back [`nfc_join_window`] will look for a normalization boundary
/// before giving up and letting the caller renormalize everything.
///
/// Stream-safe text (UAX #15) never puts more than 30 non-starters in a row,
/// so a real-world combining run is found well inside this. The cap exists so
/// that a *pathological* accumulation — `$s ~= "\x[301]"` in a loop builds one
/// grapheme with n marks and no interior boundary at all — degrades to the
/// full-normalization cost it would have had anyway, instead of scanning the
/// whole buffer on every append and then doing that work as well.
const JOIN_WINDOW_LIMIT: usize = 64;

/// True when a normalization segment can start at `c`, i.e. `c` can neither
/// compose with nor be reordered past whatever precedes it.
///
/// Both halves are needed. `NFC_QC = Yes` alone is not enough: a combining
/// mark that never composes still has a non-zero combining class and so may be
/// canonically reordered with the marks around it. Combining class 0 alone is
/// not enough either: a Hangul V or T jamo is a starter that nonetheless
/// composes with the L or LV before it, and `NFC_QC` reports it as `Maybe`.
pub(crate) fn has_nfc_boundary_before(c: char) -> bool {
    if c.is_ascii() {
        return true;
    }
    canonical_combining_class(c) == 0 && is_nfc_quick(std::iter::once(c)) == IsNormalized::Yes
}

/// What an append of `suffix` onto an already-NFC buffer has to do.
pub(crate) enum StrAppendPlan<'a> {
    /// Push this text onto the buffer unchanged; the join cannot compose.
    Direct(Cow<'a, str>),
    /// Renormalize a window around the join, then splice. Carries the
    /// **raw** suffix on purpose: the window is normalized as one piece, so
    /// pre-normalizing the suffix would only be a second pass over it.
    Join(&'a str),
}

impl<'a> StrAppendPlan<'a> {
    /// Classify `suffix`. Reads only the suffix, so it is O(len(suffix)) and
    /// can be computed before the caller commits to the in-place path.
    pub(crate) fn for_suffix(suffix: &'a str) -> Self {
        // An all-ASCII suffix is NFC and starts at a boundary by inspection,
        // and this check is the one that runs on the hot ASCII accumulation,
        // so keep it ahead of any table lookup.
        if suffix.is_ascii() {
            return StrAppendPlan::Direct(Cow::Borrowed(suffix));
        }
        match suffix.chars().next() {
            // Unreachable -- an empty suffix is ASCII -- but appending nothing
            // cannot disturb the buffer, so this is the right answer anyway.
            None => StrAppendPlan::Direct(Cow::Borrowed(suffix)),
            Some(first) if has_nfc_boundary_before(first) => {
                if is_nfc_quick(suffix.chars()) == IsNormalized::Yes {
                    StrAppendPlan::Direct(Cow::Borrowed(suffix))
                } else {
                    StrAppendPlan::Direct(Cow::Owned(suffix.nfc().collect()))
                }
            }
            Some(_) => StrAppendPlan::Join(suffix),
        }
    }

    /// Bytes to reserve for this append when the buffer has to be copied.
    /// Only a hint: normalizing the join window can make it a few bytes
    /// longer than its inputs (a singleton such as U+2ADC expands under NFC),
    /// and `push_str` grows the buffer if so.
    pub(crate) fn suffix_len_hint(&self) -> usize {
        match self {
            StrAppendPlan::Direct(text) => text.len(),
            StrAppendPlan::Join(suffix) => suffix.len(),
        }
    }
}

/// Where to cut `buf` so that `buf[..cut]` can be kept verbatim and
/// `buf[cut..]` renormalized together with the suffix.
///
/// Returns `None` when no boundary turns up within [`JOIN_WINDOW_LIMIT`]
/// characters, which tells the caller the splice is not worth it and the
/// whole concatenation should just be renormalized.
///
/// The cut is the start of the **last** character that has a boundary before
/// it, never `buf.len()`: that final character is what the suffix's leading
/// mark would compose with, so it has to be inside the window.
pub(crate) fn nfc_join_window(buf: &str) -> Option<usize> {
    let mut scanned = 0usize;
    for (idx, c) in buf.char_indices().rev() {
        if has_nfc_boundary_before(c) {
            return Some(idx);
        }
        scanned += 1;
        if scanned >= JOIN_WINDOW_LIMIT {
            return None;
        }
    }
    // Either the buffer is empty or it is one short combining run with no
    // interior boundary; in both cases the whole of it is the window.
    Some(0)
}

/// Apply `plan` to `buf`, which must already be NFC, leaving it NFC.
pub(crate) fn append_nfc(buf: &mut String, plan: &StrAppendPlan<'_>) {
    match plan {
        StrAppendPlan::Direct(text) => buf.push_str(text),
        StrAppendPlan::Join(suffix) => match nfc_join_window(buf) {
            Some(cut) => {
                let window: String = buf[cut..].chars().chain(suffix.chars()).nfc().collect();
                buf.truncate(cut);
                buf.push_str(&window);
            }
            None => {
                let whole: String = buf.chars().chain(suffix.chars()).nfc().collect();
                buf.clear();
                buf.push_str(&whole);
            }
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn appended(lhs: &str, suffix: &str) -> String {
        let mut buf = lhs.to_string();
        append_nfc(&mut buf, &StrAppendPlan::for_suffix(suffix));
        buf
    }

    /// The property the whole module exists to preserve.
    fn assert_matches_full_nfc(lhs: &str, suffix: &str) {
        let expected: String = lhs.chars().chain(suffix.chars()).nfc().collect();
        assert_eq!(
            appended(lhs, suffix),
            expected,
            "lhs={lhs:?} suffix={suffix:?}"
        );
    }

    #[test]
    fn ascii_and_plain_starters_take_the_direct_plan() {
        assert!(matches!(
            StrAppendPlan::for_suffix("abc"),
            StrAppendPlan::Direct(_)
        ));
        assert!(matches!(
            StrAppendPlan::for_suffix("\u{2603}"),
            StrAppendPlan::Direct(_)
        ));
        assert!(matches!(
            StrAppendPlan::for_suffix("\u{e9}"),
            StrAppendPlan::Direct(_)
        ));
    }

    #[test]
    fn a_leading_combining_mark_takes_the_join_plan() {
        assert!(matches!(
            StrAppendPlan::for_suffix("\u{301}"),
            StrAppendPlan::Join(_)
        ));
    }

    #[test]
    fn hangul_v_and_t_jamo_compose_backwards_and_are_not_boundaries() {
        // Starters with combining class 0, so the combining-class test alone
        // would wrongly call them boundaries.
        assert_eq!(canonical_combining_class('\u{1161}'), 0);
        assert!(!has_nfc_boundary_before('\u{1161}'));
        assert!(!has_nfc_boundary_before('\u{11A8}'));
        assert!(has_nfc_boundary_before('\u{1100}'));
    }

    #[test]
    fn a_singleton_is_not_a_boundary_so_it_is_renormalized() {
        // U+212B ANGSTROM SIGN normalizes to U+00C5; NFC_QC = No.
        assert!(!has_nfc_boundary_before('\u{212B}'));
        assert_matches_full_nfc("x", "\u{212B}");
    }

    #[test]
    fn composition_across_the_join() {
        assert_eq!(appended("e", "\u{301}"), "\u{e9}");
        assert_matches_full_nfc("e", "\u{301}");
        assert_matches_full_nfc("\u{1100}", "\u{1161}");
        assert_eq!(appended("\u{1100}", "\u{1161}"), "\u{AC00}");
        assert_matches_full_nfc("\u{AC00}", "\u{11A8}");
    }

    #[test]
    fn canonical_reordering_across_the_join() {
        // A below-mark (ccc 220) arriving after an above-mark (ccc 230) has to
        // be reordered in front of it -- and once it is, it composes with the
        // starter, which is only visible if the window reached back that far.
        // The buffer is always NFC in the real caller, hence `\u{e5}` and not
        // the decomposed `a\u{30A}`.
        assert_matches_full_nfc("\u{e5}", "\u{323}");
        assert_eq!(appended("\u{e5}", "\u{323}"), "\u{1EA1}\u{30A}");
    }

    #[test]
    fn a_long_combining_run_falls_back_to_full_normalization() {
        let mut lhs = String::from("a");
        for _ in 0..JOIN_WINDOW_LIMIT + 10 {
            lhs.push('\u{334}'); // ccc 1, never composes, so no interior boundary
        }
        assert!(nfc_join_window(&lhs).is_none());
        assert_matches_full_nfc(&lhs, "\u{301}");
        // The acute is not blocked by the ccc-1 overlays, so the full
        // renormalization must still compose it onto the starter.
        assert!(appended(&lhs, "\u{301}").starts_with('\u{e1}'));
    }

    #[test]
    fn appending_to_an_empty_buffer() {
        assert_matches_full_nfc("", "\u{301}");
        assert_matches_full_nfc("", "\u{2603}");
    }

    #[test]
    fn a_suffix_that_is_not_itself_nfc_is_normalized() {
        assert_eq!(appended("x", "e\u{301}"), "x\u{e9}");
        assert_matches_full_nfc("x", "e\u{301}");
    }

    #[test]
    fn an_ascii_buffer_taken_non_ascii() {
        assert_matches_full_nfc("ascii", "\u{2603}");
    }

    /// The window rule fails *silently* when it is wrong — a mis-sized window
    /// changes `.chars` and string comparison rather than raising anything —
    /// so pin it against the full-normalization answer over every split of a
    /// corpus built from the characters that actually interact: starters,
    /// marks of two different combining classes, Hangul jamo, a singleton,
    /// and an already-composed letter.
    #[test]
    fn every_split_of_a_composing_corpus_matches_full_nfc() {
        const PIECES: &[&str] = &[
            "a", "e", "x", "\u{301}",  // combining acute, ccc 230, composes
            "\u{323}",  // combining dot below, ccc 220, reorders before 301
            "\u{30A}",  // combining ring above, ccc 230
            "\u{334}",  // combining tilde overlay, ccc 1, never composes
            "\u{e9}",   // already-composed e-acute
            "\u{1100}", // Hangul L
            "\u{1161}", // Hangul V
            "\u{11A8}", // Hangul T
            "\u{AC00}", // Hangul LV syllable
            "\u{212B}", // ANGSTROM SIGN, NFC_QC = No
            "\u{2603}", // SNOWMAN, a plain starter
        ];
        // A cheap deterministic generator: no rand dependency, but enough
        // sequences that every adjacency in PIECES shows up on both sides of
        // the join.
        let mut state = 0x2545_F491_4F6C_DD1Du64;
        let mut next = move || {
            state ^= state << 13;
            state ^= state >> 7;
            state ^= state << 17;
            state as usize
        };
        for _ in 0..4000 {
            let mut whole = String::new();
            for _ in 0..(next() % 6) + 2 {
                whole.push_str(PIECES[next() % PIECES.len()]);
            }
            // The accumulated buffer is always NFC in the real caller, so
            // normalize the left side before splitting, exactly as the append
            // path would have found it.
            let lhs_len = next() % (whole.chars().count() + 1);
            let lhs: String = whole.chars().take(lhs_len).nfc().collect();
            let suffix: String = whole.chars().skip(lhs_len).collect();
            assert_matches_full_nfc(&lhs, &suffix);
        }
    }
}
