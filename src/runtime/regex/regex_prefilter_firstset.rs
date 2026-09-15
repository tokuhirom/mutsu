//! [`FirstSet`], the set of characters a match may begin with — the data half
//! of the ADR-0099 Stage 1 scan prefilter, split out from the analysis that
//! derives it ([`super::regex_prefilter_analysis`]) and the scan that consumes
//! it ([`super::regex_prefilter`]).

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
