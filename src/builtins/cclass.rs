//! MoarVM's character classes (`nqp::const::CCLASS_*`) -- the one
//! membership table for every layer that asks "is this a digit / word /
//! whitespace character" (ADR-0118 §2.5).
//!
//! Rakudo's regex backslash classes and POSIX-ish named rules are this same
//! table: `\d` is `CCLASS_NUMERIC`, `\w` is `CCLASS_WORD`, `\s` is
//! `CCLASS_WHITESPACE`, `\n` is `CCLASS_NEWLINE`, and `<alpha>` / `<alnum>` are
//! `CCLASS_ALPHABETIC` / `CCLASS_ALPHANUMERIC` plus `_`. So the regex engine
//! and `nqp::iscclass` / `findcclass` call into here rather than restating the
//! rules with Rust's `char` predicates (which disagree: `char::is_alphanumeric`
//! admits `²` and `Ⅰ`, `is_ascii_digit` rejects `٣`).
//!
//! **The numbers are MoarVM's, measured against rakudo, not invented.** nqp
//! code branches on them directly. The membership rules were read off rakudo by
//! probing `nqp::iscclass` per class (see `t/nqp/nqp-cclass-uniprop.t`) and the
//! regex mapping by probing each class over 0..0x3000 and three astral blocks
//! (`t/regex/regex-cclass-parity.t`).

use crate::builtins::unicode_gc::{GeneralCategory, general_category};

pub(crate) const UPPERCASE: i64 = 1;
pub(crate) const LOWERCASE: i64 = 2;
pub(crate) const ALPHABETIC: i64 = 4;
pub(crate) const NUMERIC: i64 = 8;
pub(crate) const HEXADECIMAL: i64 = 16;
pub(crate) const WHITESPACE: i64 = 32;
pub(crate) const PRINTING: i64 = 64;
pub(crate) const BLANK: i64 = 256;
pub(crate) const CONTROL: i64 = 512;
pub(crate) const PUNCTUATION: i64 = 1024;
pub(crate) const ALPHANUMERIC: i64 = 2048;
pub(crate) const NEWLINE: i64 = 4096;
pub(crate) const WORD: i64 = 8192;
pub(crate) const ANY: i64 = 65535;

/// Is `ch` a member of the MoarVM character class `cclass` (a `CCLASS_*` bit)?
///
/// Every rule is derived from the General Category, which is what MoarVM's own
/// classes are defined over -- note that `CCLASS_ALPHABETIC` is `L*`, NOT the
/// Unicode `Alphabetic` property (rakudo answers 0 for U+2160 ROMAN NUMERAL
/// ONE, which is `Nl` and `Alphabetic=Yes`), and `CCLASS_UPPERCASE` is `Lu`
/// rather than `Uppercase`, for the same reason.
// Cost: O(1).
#[inline]
pub(crate) fn is_cclass(cclass: i64, ch: char) -> bool {
    if cclass == ANY {
        return true;
    }
    cclass & cclass_bits(ch) != 0
}

/// `\w`: `CCLASS_WORD` (letters, decimal digits, `_`).
// Cost: O(1).
#[inline]
pub(crate) fn is_word(ch: char) -> bool {
    if ch.is_ascii() {
        return ch.is_ascii_alphanumeric() || ch == '_';
    }
    is_cclass(WORD, ch)
}

/// `\d`: `CCLASS_NUMERIC` (General_Category `Nd`, in every script).
// Cost: O(1).
#[inline]
pub(crate) fn is_digit(ch: char) -> bool {
    if ch.is_ascii() {
        return ch.is_ascii_digit();
    }
    is_cclass(NUMERIC, ch)
}

/// `\s`: `CCLASS_WHITESPACE` (`Z*` plus U+0009..U+000D and U+0085).
// Cost: O(1).
#[inline]
pub(crate) fn is_space(ch: char) -> bool {
    if ch.is_ascii() {
        return matches!(ch, ' ' | '\t'..='\r');
    }
    is_cclass(WHITESPACE, ch)
}

/// `\n` (one codepoint of it): `CCLASS_NEWLINE` (U+000A..U+000D, U+0085,
/// U+2028, U+2029). The two-codepoint `\r\n` is the regex engine's to handle.
// Cost: O(1).
#[inline]
pub(crate) fn is_newline(ch: char) -> bool {
    matches!(ch, '\n'..='\r' | '\u{85}' | '\u{2028}' | '\u{2029}')
}

/// Every `CCLASS_*` bit `ch` belongs to, as one integer.
///
/// This is called once per character scanned by `nqp::findcclass` /
/// `findnotcclass` -- the inner loop of a hand-rolled NQP scanner such as
/// `JSON::Fast`'s `parse-string` -- so it answers all thirteen classes from a
/// single table load rather than re-deriving each one from string
/// comparisons against the category name.
// Cost: O(1).
pub(crate) fn cclass_bits(ch: char) -> i64 {
    let mut bits = CCLASS_BY_GC[general_category(ch) as usize];
    let cp = ch as u32;
    // The members that are not a function of the General_Category: specific
    // codepoints, and the ASCII-only hexadecimal class.
    if ch.is_ascii_hexdigit() {
        bits |= HEXADECIMAL;
    }
    if matches!(cp, 0x0A | 0x0B | 0x0C | 0x0D | 0x85) {
        bits |= WHITESPACE | NEWLINE;
    }
    if cp == 0x09 {
        bits |= WHITESPACE | BLANK;
    }
    if cp == 0x5F {
        bits |= WORD;
    }
    bits
}

/// The `CCLASS_*` bits implied by a General_Category, indexed by its
/// discriminant. `t/nqp/nqp-cclass-uniprop.t` pins the result against rakudo.
const CCLASS_BY_GC: [i64; GeneralCategory::ALL.len()] = {
    let mut table = [0i64; GeneralCategory::ALL.len()];
    let mut i = 0;
    while i < table.len() {
        table[i] = cclass_bits_for_gc(GeneralCategory::ALL[i]);
        i += 1;
    }
    table
};

const fn cclass_bits_for_gc(gc: GeneralCategory) -> i64 {
    let alphabetic = gc.in_mask(GeneralCategory::LETTER);
    let numeric = matches!(gc, GeneralCategory::Nd);
    let separator = gc.in_mask(GeneralCategory::SEPARATOR);
    let control = matches!(gc, GeneralCategory::Cc);
    let mut bits = 0i64;
    if matches!(gc, GeneralCategory::Lu) {
        bits |= UPPERCASE;
    }
    if matches!(gc, GeneralCategory::Ll) {
        bits |= LOWERCASE;
    }
    if alphabetic {
        bits |= ALPHABETIC;
    }
    if numeric {
        bits |= NUMERIC;
    }
    if separator {
        bits |= WHITESPACE;
    }
    if !control {
        bits |= PRINTING;
    }
    if matches!(gc, GeneralCategory::Zs) {
        bits |= BLANK;
    }
    if control {
        bits |= CONTROL;
    }
    if gc.in_mask(GeneralCategory::PUNCTUATION) {
        bits |= PUNCTUATION;
    }
    if alphabetic || numeric {
        bits |= ALPHANUMERIC | WORD;
    }
    if matches!(gc, GeneralCategory::Zl | GeneralCategory::Zp) {
        bits |= NEWLINE;
    }
    bits
}
