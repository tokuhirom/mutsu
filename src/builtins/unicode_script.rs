//! Unicode Script lookup.
//!
//! The same problem [`super::unicode_gc`] solved, one property over: this was
//! 161 compiled `regex::Regex` matches against a one-character string, tried
//! in alphabetical order until one hit, then a `String` allocation for an
//! answer that is a fixed `&'static str`.
//!
//! It is reached per character, and twice per character from
//! `unicode_word_break`'s letter arm -- once directly and once through
//! `unicode_line_break`. **The cost is far worse for non-ASCII than the probe
//! count suggests.** For an ASCII letter most of the 161 classes reject on the
//! first byte, but a CJK or kana codepoint (`E3 81 82`) shares its lead bytes
//! with many script classes, so the UTF-8 automaton has to descend before
//! rejecting. Measured on a release build before this table,
//! `.uniprop('Word_Break')` cost 3.52us for an ASCII letter and **29.39us for
//! a hiragana one** -- against rakudo's 2.84us.
//!
//! The structure is [`super::unicode_gc`]'s, and for the same reasons: a
//! 128-byte direct-index table for ASCII, a two-stage trie over 64-codepoint
//! blocks for the rest of the BMP, and a binary search over 780 runs above it.
//! ~18 KB of `.rodata` in total, with no heap, no lock and no lazy
//! initialisation.

use super::unicode_script_data as data;

/// A Unicode Script, as an index into [`data::SCRIPT_NAMES`].
///
/// A newtype rather than a 162-variant enum: nothing branches on a particular
/// script the way `is_cclass` branches on a category, so the name is the whole
/// interface.
#[derive(Clone, Copy, PartialEq, Eq, Debug, PartialOrd, Ord, Hash)]
pub(crate) struct Script(u8);

impl Script {
    /// `Unknown` -- the fallthrough for a codepoint in no script, which is
    /// also what the ordered-regex probe returned when nothing matched.
    pub(crate) const UNKNOWN: Self = Self(data::UNKNOWN_CODE);

    /// The script's name, as `.uniprop('Script')` reports it.
    pub(crate) fn as_str(self) -> &'static str {
        data::SCRIPT_NAMES
            .get(self.0 as usize)
            .copied()
            .unwrap_or("Unknown")
    }
}

/// The Script of `ch`.
pub(crate) fn script(ch: char) -> Script {
    let cp = ch as u32;
    let code = if cp < 0x80 {
        data::ASCII_CATS[cp as usize]
    } else if cp < 0x10000 {
        let leaf = data::BMP_INDEX[(cp >> data::BMP_SHIFT) as usize] as usize;
        let offset = (cp as usize) & ((1 << data::BMP_SHIFT) - 1);
        data::BMP_LEAVES[(leaf << data::BMP_SHIFT) | offset]
    } else {
        // `ASTRAL_STARTS[0]` is exactly `0x10000`, which is `<= cp` here, so
        // the partition point is at least 1 and the subtraction cannot wrap.
        let i = data::ASTRAL_STARTS.partition_point(|&start| start <= cp) - 1;
        data::ASTRAL_CATS[i]
    };
    Script(code)
}

/// The Script name of `ch`.
pub(crate) fn script_name(ch: char) -> &'static str {
    script(ch).as_str()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn spot_checks_across_the_tiers() {
        // ASCII tier. Latin letters are Latin; digits and punctuation are
        // Common, not Latin.
        assert_eq!(script_name('A'), "Latin");
        assert_eq!(script_name('z'), "Latin");
        assert_eq!(script_name('7'), "Common");
        assert_eq!(script_name(' '), "Common");
        assert_eq!(script_name('+'), "Common");
        // BMP tier -- the case the table exists for.
        assert_eq!(script_name('\u{3042}'), "Hiragana"); // HIRAGANA LETTER A
        assert_eq!(script_name('\u{30A2}'), "Katakana"); // KATAKANA LETTER A
        assert_eq!(script_name('\u{4E00}'), "Han"); // CJK IDEOGRAPH ONE
        assert_eq!(script_name('\u{AC00}'), "Hangul"); // HANGUL SYLLABLE GA
        assert_eq!(script_name('\u{0410}'), "Cyrillic");
        assert_eq!(script_name('\u{05D0}'), "Hebrew");
        assert_eq!(script_name('\u{0627}'), "Arabic");
        assert_eq!(script_name('\u{0E01}'), "Thai");
        assert_eq!(script_name('\u{0391}'), "Greek");
        assert_eq!(script_name('\u{3041}'), "Hiragana");
        // Unassigned codepoints are in no script.
        assert_eq!(script_name('\u{0378}'), "Unknown");
        // astral tier
        assert_eq!(script_name('\u{20000}'), "Han"); // CJK ext B
        assert_eq!(script_name('\u{1F600}'), "Common"); // GRINNING FACE
        assert_eq!(script_name('\u{10FFFF}'), "Unknown");
    }

    #[test]
    fn unknown_is_the_fallthrough() {
        assert_eq!(Script::UNKNOWN.as_str(), "Unknown");
        assert_eq!(script('\u{0378}'), Script::UNKNOWN);
        assert_eq!(
            data::SCRIPT_NAMES[data::UNKNOWN_CODE as usize],
            "Unknown",
            "the fallthrough code must name the fallthrough script"
        );
    }
}
