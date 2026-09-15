//! [`FirstSet`], the set of characters a match may begin with — the data half
//! of the ADR-0099 Stage 1 scan prefilter, split out from the analysis that
//! composes it ([`super::regex_prefilter_analysis`]) and the scan that consumes
//! it ([`super::regex_prefilter`]).
//!
//! The per-atom constructors at the bottom of this file are the leaves that
//! analysis composes: one atom in, the characters it can match at a start
//! position out. They are here rather than there because each is a statement
//! about what a `FirstSet` holds, and because ADR-0099 §4 constraint 1 puts
//! one rule on all of them — the class constructor *calls* the engine's own
//! evaluator over the ASCII range instead of restating its table, so `\w`,
//! `<:Lu>` and every negated or `:i` combination of them cannot drift from
//! what the engine matches.

use super::super::*;
use super::regex_eval_class::class_matches_ignorecase;

/// A superset of the characters that can appear at the first position of a
/// match.
///
/// ASCII is exact and lives in a 128-bit bitmap — that is the whole point of
/// the structure, since rejecting a position must cost about one instruction
/// rather than the ~983 an engine entry costs (ADR-0099 §2.4). Non-ASCII is
/// deliberately coarse: either *every* non-ASCII character is admitted (the
/// conservative default, `non_ascii: None`) or an explicit short list is. That
/// asymmetry is the right trade because enumerating the reverse case-fold
/// closure, or the members of a Unicode property, over the whole of Unicode
/// would cost far more than the scan it is meant to save — and admitting a
/// character only ever costs a wasted engine entry, never a wrong answer.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct FirstSet {
    /// One bit per ASCII character; `ascii[i >> 6] & (1 << (i & 63))`.
    ascii: [u64; 2],
    /// `None` — every non-ASCII character may start a match; `Some(list)` —
    /// only these (sorted, deduplicated, and in practice at most a handful).
    non_ascii: Option<Vec<char>>,
}

impl FirstSet {
    /// Nothing at all: no ASCII character, no non-ASCII character.
    pub(crate) fn empty() -> Self {
        FirstSet {
            ascii: [0; 2],
            non_ascii: Some(Vec::new()),
        }
    }

    /// No ASCII character, but every non-ASCII one — the starting point for
    /// any set derived from a predicate this module only evaluates over ASCII.
    pub(super) fn ascii_none_rest_all() -> Self {
        FirstSet {
            ascii: [0; 2],
            non_ascii: None,
        }
    }

    /// Every character.
    pub(super) fn universal() -> Self {
        FirstSet {
            ascii: [u64::MAX; 2],
            non_ascii: None,
        }
    }

    pub(super) fn single(c: char) -> Self {
        let mut set = FirstSet::empty();
        set.insert(c);
        set
    }

    pub(super) fn insert(&mut self, c: char) {
        let cp = c as u32;
        if cp < 128 {
            self.ascii[(cp >> 6) as usize] |= 1u64 << (cp & 63);
        } else if let Some(list) = self.non_ascii.as_mut()
            && let Err(at) = list.binary_search(&c)
        {
            list.insert(at, c);
        }
    }

    pub(super) fn union(&mut self, other: &FirstSet) {
        self.ascii[0] |= other.ascii[0];
        self.ascii[1] |= other.ascii[1];
        match (self.non_ascii.as_mut(), other.non_ascii.as_ref()) {
            (None, _) => {}
            (Some(_), None) => self.non_ascii = None,
            (Some(mine), Some(theirs)) => {
                for &c in theirs {
                    if let Err(at) = mine.binary_search(&c) {
                        mine.insert(at, c);
                    }
                }
            }
        }
    }

    /// Whether `c` may begin a match. The ASCII path — the one that decides
    /// the scan — is a shift and a mask.
    #[inline]
    pub(crate) fn contains(&self, c: char) -> bool {
        let cp = c as u32;
        if cp < 128 {
            self.ascii[(cp >> 6) as usize] & (1u64 << (cp & 63)) != 0
        } else {
            match self.non_ascii.as_ref() {
                None => true,
                Some(list) => list.binary_search(&c).is_ok(),
            }
        }
    }

    /// A set that admits everything constrains nothing, so applying it would
    /// be pure overhead.
    pub(crate) fn is_universal(&self) -> bool {
        self.ascii == [u64::MAX; 2] && self.non_ascii.is_none()
    }

    pub(super) fn is_empty(&self) -> bool {
        self.ascii == [0; 2] && self.non_ascii.as_ref().is_some_and(|l| l.is_empty())
    }
}

/// The characters a literal atom can match at a start position.
///
/// Without `:i` that is the character itself. With it, the engine's test is
/// `ch.to_lowercase().to_string() == c.to_lowercase().to_string()`
/// (`regex_match_atom_simple.rs`), which this enumerates exactly over ASCII —
/// the fold closure, not a folded needle: a folded needle is unsound because
/// multi-character folds (`ß`/`SS`, `ﬁ`/`fi`) make a case-folded literal
/// *variable-length* (ADR-0099 §4 constraint 2). Every non-ASCII character is
/// admitted, since the reverse closure over Unicode reaches ASCII targets from
/// far away (`K` U+212A lowercases to `k`, `ſ` U+017F to `s`).
pub(super) fn literal_first_set(ch: char, ignore_case: bool) -> FirstSet {
    if !ignore_case {
        return FirstSet::single(ch);
    }
    let want = ch.to_lowercase().to_string();
    let mut set = FirstSet::ascii_none_rest_all();
    for cp in 0u8..128 {
        let c = cp as char;
        if c.to_lowercase().to_string() == want {
            set.insert(c);
        }
    }
    set
}

/// The characters a character-class atom can match at a start position,
/// derived by asking the engine's own evaluator about each ASCII character
/// rather than by restating its table (see the module doc comment).
pub(super) fn class_first_set(class: &CharClass, ignore_case: bool) -> FirstSet {
    let mut set = if class_is_ascii_only(class, ignore_case) {
        FirstSet::empty()
    } else {
        FirstSet::ascii_none_rest_all()
    };
    for cp in 0u8..128 {
        let c = cp as char;
        if class_matches_ignorecase(class, c, ignore_case) {
            set.insert(c);
        }
    }
    // `\r\n` is one grapheme, and the class arm accepts a class containing
    // `\n` at the `\r` that starts it.
    if set.contains('\n') {
        set.insert('\r');
    }
    // A `Grapheme` item matches a whole multi-codepoint cluster, which the
    // per-character evaluator above necessarily answers `false` for — the
    // comparison happens at the atom, where the subject text is. Its leading
    // codepoint is the one a scan position would be tested at.
    for item in &class.items {
        if let ClassItem::Grapheme(g) = item
            && let Some(lead) = g.chars().next()
        {
            set.insert(lead);
        }
    }
    set
}

/// Whether `class` provably matches no non-ASCII character, which is what
/// lets its first-set be exact rather than "ASCII plus everything else".
///
/// Deliberately a short whitelist of the items whose non-ASCII behaviour is
/// obvious from the item itself: an explicit character or range below U+0080,
/// and `\d` (which the evaluator defines as `is_ascii_digit`). A negated class
/// matches almost every non-ASCII character by construction; under `:i` a
/// non-ASCII character can fold onto an ASCII member; and a `Grapheme` entry is
/// compared in NFC against a normalized subject cluster, so its leading
/// codepoint as stored is not quite a promise about the subject's. None of the
/// three qualifies.
fn class_is_ascii_only(class: &CharClass, ignore_case: bool) -> bool {
    if class.negated || ignore_case {
        return false;
    }
    class.items.iter().all(|item| match item {
        ClassItem::Char(c) => c.is_ascii(),
        ClassItem::Range(a, b) => a.is_ascii() && b.is_ascii(),
        ClassItem::Digit => true,
        _ => false,
    })
}

/// `\n` as the engine's `Newline` atom defines it.
pub(super) fn newline_first_set() -> FirstSet {
    let mut set = FirstSet::empty();
    for c in ['\n', '\r', '\u{85}', '\u{2028}'] {
        set.insert(c);
    }
    set
}

pub(super) fn whitespace_first_set() -> FirstSet {
    let mut set = FirstSet::ascii_none_rest_all();
    for cp in 0u8..128 {
        let c = cp as char;
        if c.is_whitespace() {
            set.insert(c);
        }
    }
    set
}
