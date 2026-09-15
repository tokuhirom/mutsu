//! [`ScanPositions`], the iterator a narrowed scan walks — the consuming half
//! of the ADR-0099 Stage 1 prefilter, split out from the [`Prefilter`] the
//! facts are derived into ([`super::regex_prefilter`]).
//!
//! One variant per narrowing mechanism, plus the plain `Range` an unfiltered
//! scan falls back to. Each variant is *lazy*: the common case is "try the
//! first few positions and return on the first match", so a scan must not pay
//! for positions the caller never asks for — see
//! [`super::regex_prefilter::regex_scan_positions`] for why that matters.

use super::regex_prefilter::Prefilter;
use std::sync::Arc;

/// Iterator returned by [`super::regex_prefilter::regex_scan_positions`]. See
/// that function's doc comment for why this is an iterator rather than a `Vec`.
pub(crate) enum ScanPositions<'c> {
    Range(std::ops::RangeInclusive<usize>),
    /// Positions where the pattern's required literal prefix occurs.
    Literal {
        chars: &'c [char],
        prefilter: Arc<Prefilter>,
        pos: usize,
        /// Inclusive last viable start.
        last: usize,
    },
    /// Positions whose character is in the pattern's first-character set.
    FirstChar {
        chars: &'c [char],
        prefilter: Arc<Prefilter>,
        pos: usize,
        /// Inclusive last viable start.
        last: usize,
    },
    /// Positions consistent with an occurrence of the pattern's required
    /// *inner* literal — each occurrence at `p` admits the window
    /// `[p - max_before, p - min_before]` — additionally filtered by the
    /// first-character set when there is one, since both are necessary
    /// conditions and applying both is free.
    Inner {
        chars: &'c [char],
        prefilter: Arc<Prefilter>,
        /// Next position that may be yielded. Monotonic, which is what keeps
        /// overlapping windows from yielding a position twice.
        pos: usize,
        /// Inclusive last viable start.
        last: usize,
        /// Where the next needle search begins.
        search: usize,
        /// Inclusive end of the window currently being walked.
        window_end: Option<usize>,
        /// The needle has no further occurrence, so no window can open again.
        exhausted: bool,
    },
}

/// The first index at or after `from` where `needle` occurs in `haystack`.
///
/// A plain forward scan, like the `Literal` arm's: the needles here are a
/// handful of characters and the comparison is over `char`, not bytes, so the
/// setup a sublinear searcher needs would cost more than it saves at this size.
fn find_from(haystack: &[char], needle: &[char], from: usize) -> Option<usize> {
    let last = haystack.len().checked_sub(needle.len())?;
    (from..=last).find(|&i| haystack[i..i + needle.len()] == *needle)
}

impl ScanPositions<'_> {
    /// No candidate at all — the subject is shorter than any match could be.
    pub(super) fn empty() -> Self {
        #[expect(clippy::reversed_empty_ranges, reason = "an empty RangeInclusive")]
        ScanPositions::Range(1..=0)
    }
}

impl Iterator for ScanPositions<'_> {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        match self {
            ScanPositions::Range(r) => r.next(),
            ScanPositions::Literal {
                chars,
                prefilter,
                pos,
                last,
            } => {
                let needle = prefilter.prefix.as_deref().unwrap_or_default();
                while *pos <= *last {
                    let candidate = *pos;
                    *pos += 1;
                    if chars[candidate..candidate + needle.len()] == *needle {
                        crate::vm::vm_stats::record_regex_prefilter_position_hit();
                        return Some(candidate);
                    }
                }
                None
            }
            ScanPositions::FirstChar {
                chars,
                prefilter,
                pos,
                last,
            } => {
                let set = prefilter.first.as_ref()?;
                while *pos <= *last {
                    let candidate = *pos;
                    *pos += 1;
                    if set.contains(chars[candidate]) {
                        crate::vm::vm_stats::record_regex_prefilter_position_hit();
                        return Some(candidate);
                    }
                }
                None
            }
            ScanPositions::Inner {
                chars,
                prefilter,
                pos,
                last,
                search,
                window_end,
                exhausted,
            } => {
                let inner = prefilter.inner.as_ref()?;
                let set = prefilter.first.as_ref();
                loop {
                    if let Some(end) = *window_end {
                        while *pos <= end {
                            if *pos > *last {
                                return None;
                            }
                            let candidate = *pos;
                            *pos += 1;
                            if set.is_none_or(|s| s.contains(chars[candidate])) {
                                crate::vm::vm_stats::record_regex_prefilter_position_hit();
                                return Some(candidate);
                            }
                        }
                        *window_end = None;
                        if *pos > *last {
                            return None;
                        }
                    }
                    if *exhausted {
                        return None;
                    }
                    let Some(at) = find_from(chars, &inner.literal, *search) else {
                        *exhausted = true;
                        return None;
                    };
                    *search = at + 1;
                    // An occurrence closer to the subject start than the
                    // pattern's minimum lead-in cannot be the one a match
                    // contains; the next occurrence still can.
                    let Some(end) = at.checked_sub(inner.min_before) else {
                        continue;
                    };
                    if end < *pos {
                        // Entirely behind the positions already yielded.
                        continue;
                    }
                    if let Some(max_before) = inner.max_before {
                        *pos = (*pos).max(at.saturating_sub(max_before));
                    }
                    *window_end = Some(end);
                }
            }
        }
    }
}
