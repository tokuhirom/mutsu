//! Per-site memo for a built-in method call answered from the method table
//! (ADR-11276 §2.5; `@a.elems`, `$r.numerator`).
//!
//! `OpCode::CallMethodMut` on a plain receiver whose method has a table row
//! is answered by that row, but deciding so means asking the registry whether
//! user code `augment`ed the receiver's type with a method of that name. The
//! answer holds until a declaration changes, so the chunk remembers it for one
//! registry write generation: one word per string constant (a site is a call
//! whose method name is that constant), holding the generation and an opaque
//! payload the reader packs (the receiver shape and the row).
//!
//! What the memo may assume, and what it re-checks on every hit, is decided by
//! the reader (`Interpreter::try_method_site_lane`); this type only stores
//! `(generation, payload)`. The generation is the interpreter's registry write
//! generation, which starts in a range of its own per interpreter
//! (`registry_gen.rs`), so a word a thread wrote into a shared chunk is never
//! read as current by another interpreter whose registry snapshot differs.

use std::sync::{Mutex, OnceLock};

/// One site's memo: the generation it was filled under, and the payload.
type Word = Mutex<Option<(u64, u32)>>;

/// One memo per string constant of a chunk.
#[derive(Debug, Default)]
pub(crate) struct MethodSiteCaches(OnceLock<Box<[Word]>>);

impl Clone for MethodSiteCaches {
    /// A cloned chunk starts cold: the memo is an optimization, not state.
    fn clone(&self) -> Self {
        Self::default()
    }
}

impl MethodSiteCaches {
    /// The memo words; `sites` is the chunk's constant count when the table is
    /// first built. A constant appended later has no word and never hits.
    // Cost: O(1) once built; the first call allocates O(sites).
    fn words(&self, sites: usize) -> &[Word] {
        self.0
            .get_or_init(|| (0..sites).map(|_| Mutex::new(None)).collect())
    }

    /// The payload constant `idx` was filled with under `generation`, if any.
    // Cost: O(1), one uncontended lock.
    #[inline]
    pub(crate) fn cached(&self, sites: usize, idx: usize, generation: u64) -> Option<u32> {
        match *self.words(sites).get(idx)?.lock().ok()? {
            Some((g, payload)) if g == generation => Some(payload),
            _ => None,
        }
    }

    /// Remember `payload` for constant `idx` under `generation` (read before
    /// the decision it records was made).
    // Cost: O(1), one uncontended lock.
    pub(crate) fn remember(&self, sites: usize, idx: usize, generation: u64, payload: u32) {
        if let Some(word) = self.words(sites).get(idx)
            && let Ok(mut w) = word.lock()
        {
            *w = Some((generation, payload));
        }
    }
}
