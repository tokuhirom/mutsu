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
//! `StrBody` derefs to `String`, so every reader keeps reading a `&str`
//! exactly as before; only the producers below build strands.
//!
//! Invariants:
//! - A strand's base is always a [`StrBody::Flat`] body, so a strand list is
//!   one level deep: flattening or dropping never recurses, and no chain of
//!   intermediate results stays alive.
//! - A strand list has at most [`MAX_STRANDS`] entries (MoarVM's bound);
//!   a concatenation that would exceed it is built flat instead.
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
}

/// The strand form of a [`StrBody`].
pub struct LazyStr {
    strands: Vec<Strand>,
    /// Total length in bytes of the flattened string.
    len: usize,
    /// The flattened string, filled by the first read.
    flat: OnceLock<String>,
}

/// `base` repeated `reps` times. `base` is always a `Flat` body.
#[derive(Clone)]
struct Strand {
    base: Arc<StrBody>,
    reps: usize,
}

impl StrBody {
    /// The flat buffer if this body is flat, without flattening a strand list.
    #[inline]
    fn flat_buf(&self) -> Option<&String> {
        match self {
            StrBody::Flat(s) => Some(s),
            StrBody::Lazy(_) => None,
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
            StrBody::Flat(_) => 1,
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
            None => Arc::new(StrBody::Flat(String::clone(base))),
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
                StrBody::Flat(_) => strands.push(Strand {
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
    /// cached flattening (or a fresh one). The caller grows the buffer in
    /// place and stores it back as `StrBody::Flat`.
    ///
    /// Cost: O(1) for a flat body; O(n) for a strand list that has not been
    /// read yet, n = bytes of the string.
    pub(crate) fn take_flat(&mut self) -> String {
        let flat = match self {
            StrBody::Flat(s) => std::mem::take(s),
            StrBody::Lazy(l) => l.flat.take().unwrap_or_else(|| l.flatten()),
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
            // A base is flat by construction, so this deref never flattens.
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
    type Target = String;

    /// The string. A strand list is flattened on the first call and the
    /// result cached, so later calls are O(1).
    #[inline]
    fn deref(&self) -> &String {
        match self {
            StrBody::Flat(s) => s,
            StrBody::Lazy(l) => l.flat.get_or_init(|| l.flatten()),
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
            StrBody::Flat(_) => panic!("expected strands"),
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
    fn take_flat_flattens_an_unread_list() {
        let mut body = StrBody::repeated(&flat("é"), 3);
        let mut s = body.take_flat();
        s.push('.');
        assert_eq!(s, "ééé.");
    }
}
