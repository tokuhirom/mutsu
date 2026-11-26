//! The payload of a `Str` value: a flat buffer, or a lazily flattened list of
//! strands (ADR-0120).
//!
//! A `Str` used to be a bare `Arc<String>`, so every producer paid for its
//! whole result: `$s x $c` wrote n*c bytes, and `$a ~ $b` copied `$a` whenever
//! anything else still held it. MoarVM instead builds a *strand* list -- a few
//! references to flat strings, each with a repeat count -- and only writes the
//! characters out when something reads them. [`StrBody`] is that
//! representation: [`StrBody::Lazy`] holds the strands and flattens them
//! **once**, into a cache, on the first read.
//!
//! [`StrBody::Slice`] is a byte range of another body: a piece cut out of a
//! string (`.split`) shares the invocant's buffer instead of copying it, as
//! MoarVM's substring strands do (#19201).
//!
//! `StrBody` derefs to `str`, so every reader keeps reading a `&str`
//! exactly as before; only the producers below build strands and slices.
//!
//! Invariants:
//! - A strand's base is always a [`StrBody::Flat`] body, so a strand list is
//!   one level deep: flattening or dropping never recurses, and no chain of
//!   intermediate results stays alive.
//! - A strand list has at most [`MAX_STRANDS`] entries (MoarVM's bound);
//!   a concatenation that would exceed it is built flat instead.
//! - A slice's base is always a [`StrBody::Flat`] body ([`StrBody::slice_of`]
//!   re-bases a slice of a slice and copies a piece of a strand list), so
//!   reading a slice is O(1) and never recurses.
//!   Its range lies on `char` boundaries of the base.
//! - The concatenation of the strands is NFC. The producers only strand a
//!   join that cannot compose (see `str_prim::build`), so this holds by
//!   construction, like every other `Str`.

use std::fmt;
use std::ops::Deref;
use std::sync::{Arc, OnceLock};

/// The most strands one string holds (MoarVM's `MVM_STRING_MAX_STRANDS`).
pub(crate) const MAX_STRANDS: usize = 64;

/// Results shorter than this (in bytes) are built flat: a copy that small is
/// cheaper than the flatten the first read of a strand list pays.
pub(crate) const STRAND_MIN_BYTES: usize = 1024;

/// A `Str` payload. See the module docs.
pub enum StrBody {
    /// One buffer, the common case.
    Flat(String),
    /// Strands, flattened on first read.
    Lazy(Box<LazyStr>),
    /// A byte range of another body, sharing its buffer.
    Slice(StrSlice),
}

/// The substring form of a [`StrBody`]: `base[start..end]`.
pub struct StrSlice {
    /// Always a [`StrBody::Flat`] body.
    base: Arc<StrBody>,
    start: usize,
    end: usize,
}

/// Bases at most this long (in bytes) are always shared by a slice. Above it a
/// piece is shared only when it is a sizeable part of the base (see
/// [`StrBody::slice_of`]), so that holding one small piece does not keep a huge
/// string alive.
pub(crate) const SLICE_SHARE_MAX_BASE: usize = 64 * 1024;

/// The strand form of a [`StrBody`].
pub struct LazyStr {
    strands: Vec<Strand>,
    /// Total length in bytes of the flattened string.
    len: usize,
    /// The flattened string, filled by the first read.
    flat: OnceLock<String>,
}

/// `base` repeated `reps` times. `base` is a `Flat` or `Slice` body, never a
/// strand list.
#[derive(Clone)]
struct Strand {
    base: Arc<StrBody>,
    reps: usize,
}

impl StrBody {
    /// The string as a `&str` (the same as the `Deref`).
    ///
    /// Cost: O(1), except the first read of a strand list (O(n)).
    #[inline]
    pub fn as_str(&self) -> &str {
        self
    }

    /// `base[start..end]` (byte offsets on `char` boundaries) as a body: a
    /// [`StrBody::Slice`] sharing `base`'s buffer when that is worth it,
    /// otherwise a flat copy.
    ///
    /// A slice keeps its whole base alive, so it is used only when the base is
    /// at most [`SLICE_SHARE_MAX_BASE`] bytes or the piece is at least an
    /// eighth of it; an empty piece is always a (non-allocating) flat body.
    /// A slice of a slice re-bases onto the underlying body; a piece of a
    /// strand list is copied.
    ///
    /// Cost: O(1) when shared; O(m) when copied, m = bytes of the piece.
    pub(crate) fn slice_of(base: &Arc<StrBody>, start: usize, end: usize) -> StrBody {
        debug_assert!(start <= end && end <= base.byte_len());
        let len = end - start;
        if len == 0 {
            return StrBody::Flat(String::new());
        }
        let (root, offset) = match &**base {
            StrBody::Slice(sl) => (&sl.base, sl.start),
            _ => (base, 0),
        };
        let root_len = root.byte_len();
        // Only a flat root is shared: a slice of a strand list could chain back
        // into strands and lose the one-hop read.
        if root.flat_buf().is_none() || (root_len > SLICE_SHARE_MAX_BASE && len < root_len / 8) {
            return StrBody::Flat(base.as_str()[start..end].to_owned());
        }
        StrBody::Slice(StrSlice {
            base: Arc::clone(root),
            start: offset + start,
            end: offset + end,
        })
    }

    /// The flat buffer if this body is flat, without flattening a strand list.
    #[inline]
    fn flat_buf(&self) -> Option<&String> {
        match self {
            StrBody::Flat(s) => Some(s),
            StrBody::Lazy(_) | StrBody::Slice(_) => None,
        }
    }

    /// Length in bytes, without flattening.
    ///
    /// Cost: O(1).
    #[inline]
    pub(crate) fn byte_len(&self) -> usize {
        match self {
            StrBody::Flat(s) => s.len(),
            StrBody::Lazy(l) => l.len,
            StrBody::Slice(sl) => sl.end - sl.start,
        }
    }

    /// Whether this body is a strand list (flattened or not).
    pub(crate) fn is_lazy(&self) -> bool {
        matches!(self, StrBody::Lazy(_))
    }

    /// Whether this body is a strand list that has not been flattened yet.
    #[cfg(test)]
    pub(crate) fn is_unflattened_strands(&self) -> bool {
        matches!(self, StrBody::Lazy(l) if l.flat.get().is_none())
    }

    /// The first character, without flattening a strand list.
    ///
    /// Cost: O(1).
    pub(crate) fn first_char(&self) -> Option<char> {
        match self {
            StrBody::Flat(s) => s.chars().next(),
            StrBody::Slice(_) => self.as_str().chars().next(),
            StrBody::Lazy(l) => l
                .strands
                .iter()
                .find(|s| s.reps > 0 && !s.base.is_empty())
                .and_then(|s| s.base.chars().next()),
        }
    }

    /// Number of strands a concatenation needs to reference this body.
    fn strand_count(&self) -> usize {
        match self {
            StrBody::Flat(_) | StrBody::Slice(_) => 1,
            StrBody::Lazy(l) => l.strands.len(),
        }
    }

    /// `base` repeated `reps` times, as one strand. `base` must be NFC and
    /// repeat as NFC (no copy composes with the one before it).
    ///
    /// Cost: O(1) when `base` is flat; O(n) when it is a strand list (it is
    /// flattened into a new base first), n = bytes of `base`.
    pub(crate) fn repeated(base: &Arc<StrBody>, reps: usize) -> StrBody {
        let base = match base.flat_buf() {
            Some(_) => Arc::clone(base),
            None => Arc::new(StrBody::Flat(base.to_string())),
        };
        let len = base.byte_len() * reps;
        StrBody::Lazy(Box::new(LazyStr {
            strands: vec![Strand { base, reps }],
            len,
            flat: OnceLock::new(),
        }))
    }

    /// The concatenation of `parts` as a strand list, or `None` when it would
    /// need more than [`MAX_STRANDS`] strands. The parts must be NFC and every
    /// join between them must be unable to compose.
    ///
    /// Cost: O(k), k = strands referenced (at most [`MAX_STRANDS`]).
    pub(crate) fn concat_strands(parts: &[&Arc<StrBody>]) -> Option<StrBody> {
        let count: usize = parts
            .iter()
            .filter(|p| p.byte_len() > 0)
            .map(|p| p.strand_count())
            .sum();
        if count > MAX_STRANDS {
            return None;
        }
        let mut strands = Vec::with_capacity(count);
        let mut len = 0usize;
        for part in parts {
            if part.byte_len() == 0 {
                continue;
            }
            len += part.byte_len();
            match &***part {
                // A slice reads in O(1) without recursing, so it is as good a
                // strand base as a flat body.
                StrBody::Flat(_) | StrBody::Slice(_) => strands.push(Strand {
                    base: Arc::clone(part),
                    reps: 1,
                }),
                StrBody::Lazy(l) => strands.extend(l.strands.iter().cloned()),
            }
        }
        Some(StrBody::Lazy(Box::new(LazyStr {
            strands,
            len,
            flat: OnceLock::new(),
        })))
    }

    /// Move the string out of a body its holder owns exclusively, leaving it
    /// empty and flat: a flat body gives up its buffer, a strand list its
    /// cached flattening (or a fresh one), a slice a copy of its range. The caller grows the buffer in
    /// place and stores it back as `StrBody::Flat`.
    ///
    /// Cost: O(1) for a flat body; O(n) for a strand list that has not been
    /// read yet, n = bytes of the string.
    pub(crate) fn take_flat(&mut self) -> String {
        let flat = match self {
            StrBody::Flat(s) => std::mem::take(s),
            StrBody::Lazy(l) => l.flat.take().unwrap_or_else(|| l.flatten()),
            StrBody::Slice(sl) => sl.base.as_str()[sl.start..sl.end].to_owned(),
        };
        *self = StrBody::Flat(String::new());
        flat
    }
}

impl LazyStr {
    /// Write the strands out into one buffer.
    ///
    /// Cost: O(n), n = bytes of the result.
    fn flatten(&self) -> String {
        let mut out: Vec<u8> = Vec::with_capacity(self.len);
        for strand in &self.strands {
            // A base is flat or a slice by construction, so this deref never
            // flattens.
            let base: &str = &strand.base;
            if strand.reps == 0 || base.is_empty() {
                continue;
            }
            let start = out.len();
            let total = base.len() * strand.reps;
            out.extend_from_slice(base.as_bytes());
            // Doubling: one memcpy per doubling instead of `reps` pushes.
            while out.len() - start < total {
                let have = out.len() - start;
                let take = (total - have).min(have);
                out.extend_from_within(start..start + take);
            }
        }
        // SAFETY: `out` is a sequence of whole copies of UTF-8 strings: each
        // doubling step copies `take` bytes from the start of the segment,
        // where `take` is either the whole segment so far (a whole number of
        // copies of `base`) or `total - have`, which is also a whole number
        // of copies because both `total` and `have` are. Skipping the
        // validation scan matters for a multi-GiB repetition.
        unsafe { String::from_utf8_unchecked(out) }
    }
}

impl Deref for StrBody {
    type Target = str;

    /// The string. A strand list is flattened on the first call and the
    /// result cached, so later calls are O(1). A slice is O(1).
    #[inline]
    fn deref(&self) -> &str {
        match self {
            StrBody::Flat(s) => s,
            StrBody::Lazy(l) => l.flat.get_or_init(|| l.flatten()),
            // The base is flat, so this is one hop.
            StrBody::Slice(sl) => &sl.base.as_str()[sl.start..sl.end],
        }
    }
}

impl From<String> for StrBody {
    fn from(s: String) -> Self {
        StrBody::Flat(s)
    }
}

impl fmt::Debug for StrBody {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&**self, f)
    }
}

impl fmt::Display for StrBody {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&**self, f)
    }
}

impl PartialEq for StrBody {
    fn eq(&self, other: &Self) -> bool {
        **self == **other
    }
}

impl Eq for StrBody {}

impl PartialEq<str> for StrBody {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl PartialEq<&str> for StrBody {
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

impl PartialEq<String> for StrBody {
    fn eq(&self, other: &String) -> bool {
        **self == *other
    }
}

impl PartialOrd for StrBody {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for StrBody {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (**self).cmp(&**other)
    }
}

impl std::hash::Hash for StrBody {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (**self).hash(state)
    }
}

impl std::borrow::Borrow<str> for StrBody {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl AsRef<str> for StrBody {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn flat(s: &str) -> Arc<StrBody> {
        Arc::new(StrBody::Flat(s.to_string()))
    }

    #[test]
    fn repeated_is_lazy_until_read() {
        let body = StrBody::repeated(&flat("ab"), 3);
        assert!(body.is_unflattened_strands());
        assert_eq!(body.byte_len(), 6);
        assert_eq!(body.as_str(), "ababab");
        assert!(!body.is_unflattened_strands());
    }

    #[test]
    fn repeat_of_multibyte_flattens_whole_chars() {
        let body = StrBody::repeated(&flat("あい"), 5);
        assert_eq!(body.as_str(), "あい".repeat(5));
    }

    #[test]
    fn zero_reps_is_empty() {
        let body = StrBody::repeated(&flat("abc"), 0);
        assert_eq!(body.byte_len(), 0);
        assert_eq!(body.as_str(), "");
    }

    #[test]
    fn concat_splices_strands_one_level_deep() {
        let a = Arc::new(StrBody::repeated(&flat("x"), 4));
        let b = flat("yz");
        let ab = Arc::new(StrBody::concat_strands(&[&a, &b]).unwrap());
        let abab = StrBody::concat_strands(&[&ab, &ab]).unwrap();
        match &abab {
            StrBody::Lazy(l) => {
                assert_eq!(l.strands.len(), 4);
                assert!(l.strands.iter().all(|s| s.base.flat_buf().is_some()));
            }
            StrBody::Flat(_) | StrBody::Slice(_) => panic!("expected strands"),
        }
        assert_eq!(abab.as_str(), "xxxxyzxxxxyz");
    }

    #[test]
    fn concat_refuses_past_the_strand_limit() {
        let one = flat("a");
        let parts: Vec<&Arc<StrBody>> = std::iter::repeat_n(&one, MAX_STRANDS + 1).collect();
        assert!(StrBody::concat_strands(&parts).is_none());
        assert!(StrBody::concat_strands(&parts[..MAX_STRANDS]).is_some());
    }

    #[test]
    fn take_flat_takes_the_cache() {
        let mut body = StrBody::repeated(&flat("ab"), 2);
        assert_eq!(body.as_str(), "abab");
        let mut s = body.take_flat();
        s.push('!');
        assert_eq!(s, "abab!");
        assert!(matches!(&body, StrBody::Flat(b) if b.is_empty()));
    }

    #[test]
    fn slice_shares_the_base_buffer() {
        let base = flat("alpha beta gamma");
        let body = StrBody::slice_of(&base, 6, 10);
        assert!(matches!(body, StrBody::Slice(_)));
        assert_eq!(body.as_str(), "beta");
        assert_eq!(body.byte_len(), 4);
        assert_eq!(body.first_char(), Some('b'));
        assert_eq!(Arc::strong_count(&base), 2);
    }

    #[test]
    fn slice_of_a_slice_rebases() {
        let base = flat("0123456789");
        let outer = Arc::new(StrBody::slice_of(&base, 2, 8));
        let inner = StrBody::slice_of(&outer, 1, 3);
        assert_eq!(inner.as_str(), "34");
        match &inner {
            StrBody::Slice(sl) => assert!(Arc::ptr_eq(&sl.base, &base)),
            _ => panic!("expected a slice"),
        }
    }

    #[test]
    fn empty_and_small_pieces_of_a_huge_base_are_copied() {
        let base = flat("x");
        assert!(matches!(StrBody::slice_of(&base, 0, 0), StrBody::Flat(ref s) if s.is_empty()));
        let big = flat(&"y".repeat(SLICE_SHARE_MAX_BASE + 1));
        assert!(matches!(StrBody::slice_of(&big, 0, 3), StrBody::Flat(_)));
        let half = StrBody::slice_of(&big, 0, SLICE_SHARE_MAX_BASE / 2);
        assert!(matches!(half, StrBody::Slice(_)));
    }

    #[test]
    fn piece_of_a_strand_list_is_copied() {
        let lazy = Arc::new(StrBody::repeated(&flat("ab"), 3));
        let piece = StrBody::slice_of(&lazy, 1, 4);
        assert!(matches!(piece, StrBody::Flat(_)));
        assert_eq!(piece.as_str(), "bab");
    }

    #[test]
    fn slice_takes_flat_and_concats_as_a_strand() {
        let base = flat("héllo world");
        let mut body = StrBody::slice_of(&base, 0, 6);
        assert_eq!(body.as_str(), "héllo");
        let a = Arc::new(StrBody::slice_of(&base, 7, 12));
        let cat = StrBody::concat_strands(&[&a, &a]).unwrap();
        assert_eq!(cat.as_str(), "worldworld");
        let mut s = body.take_flat();
        s.push('!');
        assert_eq!(s, "héllo!");
        assert_eq!(base.as_str(), "héllo world");
    }

    #[test]
    fn take_flat_flattens_an_unread_list() {
        let mut body = StrBody::repeated(&flat("é"), 3);
        let mut s = body.take_flat();
        s.push('.');
        assert_eq!(s, "ééé.");
    }
}
