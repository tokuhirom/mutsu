//! [`HashKey`] — a hash key that is cheap to create *and* cheap to clone.
//!
//! Step 1 of [#7549](https://github.com/tokuhirom/mutsu/issues/7549). This
//! module introduces the type and nothing else: `HashData::map` is still
//! `HashMap<String, Value>`, so landing this changes no behavior. Switching the
//! two maps over is the separate step that the issue's measurement gates.
//!
//! # The problem it exists to solve
//!
//! `HashData::map` is a `HashMap<String, Value>` under `derive(Clone)`, so every
//! value-copy of a hash deep-clones each `String` key: one heap allocation per
//! key. Measured on the issue, a `my %h = %g` over a 200-key hash costs 200.9
//! allocations per copy — 200 keys plus one table — and the key clones are ~72%
//! of that shape's wall clock.
//!
//! The *other* half is key creation, which the issue's original framing missed.
//! Every hot store site already mints a fresh `String` per insert
//! (`Value::hash_assign_at` does `key.to_string()`, `EntryTerminal::insert` and
//! the `vm_var_assign_index_named` paths do `key.clone()`) and moves it into
//! `Value::hash_insert_through`. `bench-hash` pays that 10,000 times and clones
//! a hash exactly *once* in the whole run, so a key type that is only
//! cheap-to-clone would buy it nothing.
//!
//! # Why this shape
//!
//! Ruby, Python and Perl 5 all converge on one invariant: a hash key is an
//! immutable, shared object, so copying a hash never copies key bytes. Perl 5
//! stores keys as refcounted `HEK`s in the interpreter-global `PL_strtab`;
//! Ruby passes `String` keys through `rb_fstring` into a GC-reclaimable
//! deduplicating table, and embeds strings up to 23 bytes in the `RString`
//! itself; Python's `str` is immutable and refcounted with its hash cached in
//! the object, so a `dict` copy only increfs. `String` breaks the invariant at
//! the first clause — it is uniquely owned, so `HashMap::clone` *must* copy.
//!
//! [`HashKey`] takes the two properties that need no global state and skips the
//! one that does:
//!
//! - **short keys inline** (`INLINE_CAP` bytes, covering most Raku keys) —
//!   zero allocation to create *and* zero to clone;
//! - **longer keys shared** behind an `Arc<str>` — one allocation to create,
//!   O(1) refcount bump to clone;
//! - **no interning.** Deduplication is what Perl's `PL_strtab` and Ruby's
//!   fstring table add, and their refcount/GC reclamation is also the answer to
//!   the issue's "interning arbitrary runtime keys would grow the table without
//!   bound" objection — unbounded growth is a property of *immortal* interning,
//!   not of interning. It is still out of scope here: it buys a global table
//!   and a lock in a threaded VM for a win no measurement has asked for.
//!
//! # Size
//!
//! `INLINE_CAP` is chosen so `HashKey` is exactly as wide as the `String` it
//! replaces (24 bytes). A wider key would grow every hash table in the
//! interpreter, which could easily cost more than the allocations it saves, so
//! the size is pinned by a test rather than left to chance.

use std::borrow::Borrow;
use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

/// Bytes a key holds without allocating.
///
/// 15 keeps `HashKey` at `size_of::<String>()` (see the module docs): the
/// `Arc<str>` variant is a 16-byte fat pointer at offset 8, so the inline
/// variant gets the same 16 bytes, one of which is the length.
pub const INLINE_CAP: usize = 15;

#[derive(Clone)]
enum Repr {
    /// `bytes[..len]` is valid UTF-8. Upheld by construction: the only way to
    /// build this variant is [`HashKey::new`], which copies from a `&str`.
    Inline {
        len: u8,
        bytes: [u8; INLINE_CAP],
    },
    Shared(Arc<str>),
}

/// A Raku hash key: immutable, cheap to create, cheap to clone.
///
/// Behaves like a `str` everywhere it matters — [`Deref`](std::ops::Deref),
/// `Borrow<str>`, `AsRef<str>`, and [`Hash`]/[`Eq`]/[`Ord`] all agree with
/// `str`, so a `HashMap<HashKey, _>` is looked up with a plain `&str` exactly
/// as a `HashMap<String, _>` is.
#[derive(Clone)]
pub struct HashKey(Repr);

impl HashKey {
    /// Build a key from a string slice. Allocates only when `s` is longer than
    /// `INLINE_CAP`.
    #[inline]
    pub fn new(s: &str) -> Self {
        let len = s.len();
        if len <= INLINE_CAP {
            let mut bytes = [0u8; INLINE_CAP];
            bytes[..len].copy_from_slice(s.as_bytes());
            HashKey(Repr::Inline {
                len: len as u8,
                bytes,
            })
        } else {
            HashKey(Repr::Shared(Arc::from(s)))
        }
    }

    /// The key as a string slice.
    #[inline]
    pub fn as_str(&self) -> &str {
        match &self.0 {
            Repr::Inline { len, bytes } => {
                let raw = &bytes[..*len as usize];
                debug_assert!(
                    std::str::from_utf8(raw).is_ok(),
                    "HashKey inline bytes are not UTF-8"
                );
                // SAFETY: `Repr::Inline` is only ever built by `HashKey::new`,
                // which copies whole bytes out of a `&str` and records that
                // slice's length — so `bytes[..len]` is exactly those bytes and
                // is valid UTF-8. Nothing mutates a `HashKey` after
                // construction (there is no `&mut` accessor), so the invariant
                // cannot be broken later. Checked in debug builds above.
                unsafe { std::str::from_utf8_unchecked(raw) }
            }
            Repr::Shared(s) => s,
        }
    }

    /// Number of bytes in the key.
    #[inline]
    pub fn len(&self) -> usize {
        match &self.0 {
            Repr::Inline { len, .. } => *len as usize,
            Repr::Shared(s) => s.len(),
        }
    }

    /// Whether the key is the empty string.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Whether this key is stored inline (i.e. cost no allocation).
    ///
    /// Test-facing: callers should not branch on the representation, but the
    /// whole point of the type is which side of the boundary a key lands on, so
    /// the tests pin it.
    #[inline]
    pub fn is_inline(&self) -> bool {
        matches!(self.0, Repr::Inline { .. })
    }
}

impl std::ops::Deref for HashKey {
    type Target = str;
    #[inline]
    fn deref(&self) -> &str {
        self.as_str()
    }
}

impl Borrow<str> for HashKey {
    #[inline]
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl AsRef<str> for HashKey {
    #[inline]
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

// `Borrow<str>` obliges `Hash`/`Eq`/`Ord` to agree with `str`'s — a
// `HashMap<HashKey, _>::get(&str)` silently misses otherwise. Forwarding is the
// only implementation that keeps that promise, so none of these are derived.
impl Hash for HashKey {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_str().hash(state);
    }
}

impl PartialEq for HashKey {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl Eq for HashKey {}

impl PartialOrd for HashKey {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for HashKey {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl PartialEq<str> for HashKey {
    #[inline]
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl PartialEq<&str> for HashKey {
    #[inline]
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

impl PartialEq<String> for HashKey {
    #[inline]
    fn eq(&self, other: &String) -> bool {
        self.as_str() == other.as_str()
    }
}

impl PartialEq<HashKey> for str {
    #[inline]
    fn eq(&self, other: &HashKey) -> bool {
        self == other.as_str()
    }
}

impl PartialEq<HashKey> for &str {
    #[inline]
    fn eq(&self, other: &HashKey) -> bool {
        *self == other.as_str()
    }
}

impl PartialEq<HashKey> for String {
    #[inline]
    fn eq(&self, other: &HashKey) -> bool {
        self.as_str() == other.as_str()
    }
}

impl From<&str> for HashKey {
    #[inline]
    fn from(s: &str) -> Self {
        HashKey::new(s)
    }
}

impl From<&String> for HashKey {
    #[inline]
    fn from(s: &String) -> Self {
        HashKey::new(s)
    }
}

impl From<String> for HashKey {
    /// Reuses `s`'s existing allocation when the key is too long to inline, so
    /// converting an owned `String` never allocates twice.
    #[inline]
    fn from(s: String) -> Self {
        if s.len() <= INLINE_CAP {
            HashKey::new(&s)
        } else {
            HashKey(Repr::Shared(Arc::from(s)))
        }
    }
}

impl From<Box<str>> for HashKey {
    #[inline]
    fn from(s: Box<str>) -> Self {
        if s.len() <= INLINE_CAP {
            HashKey::new(&s)
        } else {
            HashKey(Repr::Shared(Arc::from(s)))
        }
    }
}

impl From<Arc<str>> for HashKey {
    /// Keeps the existing `Arc` rather than re-inlining a short key: the
    /// allocation has already been paid for, and cloning it is O(1) either way.
    #[inline]
    fn from(s: Arc<str>) -> Self {
        HashKey(Repr::Shared(s))
    }
}

impl From<HashKey> for String {
    #[inline]
    fn from(k: HashKey) -> String {
        k.as_str().to_string()
    }
}

impl Default for HashKey {
    #[inline]
    fn default() -> Self {
        HashKey::new("")
    }
}

/// Quoted, like `str`'s — so a `HashData` debug dump reads the same as it did
/// when the keys were `String`s.
impl fmt::Debug for HashKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.as_str(), f)
    }
}

impl fmt::Display for HashKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

#[cfg(test)]
mod tests;
