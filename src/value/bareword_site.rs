//! Per-site memo for a bareword term that names its own type object
//! (ADR-0121 D3; `P` in `P.new(...)`, `Int` in `$x ~~ Int`).
//!
//! `OpCode::GetBareWord` resolves its name at run time through the whole
//! term-resolution chain on every execution: ~2,100 instructions for a plain
//! class name, more than a tenth of what `P.new(x => 1, y => 2)` costs. rakudo
//! resolves the name at compile time. A name that resolved to the type object
//! of its own spelling keeps resolving to it until a declaration changes, so
//! the chunk remembers the answer for one registry write generation, one word
//! per string constant (a site is a `GetBareWord` of that constant).
//!
//! What the memo may assume, and what it re-checks on every hit, is decided
//! by the reader (`Interpreter::exec_get_bare_word_op`); this type only stores
//! `(generation, type object)`.

use crate::symbol::Symbol;
use std::sync::{Mutex, OnceLock};

/// One memo per string constant of a chunk.
#[derive(Debug, Default)]
pub(crate) struct BarewordSiteCaches(OnceLock<Box<[Mutex<Option<(u64, Symbol)>>]>>);

impl Clone for BarewordSiteCaches {
    /// A cloned chunk starts cold: the memo is an optimization, not state.
    fn clone(&self) -> Self {
        Self::default()
    }
}

impl BarewordSiteCaches {
    /// The memo words; `sites` is the chunk's constant count when the table is
    /// first built. A constant appended later has no word and never hits.
    // Cost: O(1) once built; the first call allocates O(sites).
    fn words(&self, sites: usize) -> &[Mutex<Option<(u64, Symbol)>>] {
        self.0
            .get_or_init(|| (0..sites).map(|_| Mutex::new(None)).collect())
    }

    /// The type object constant `idx` resolved to under `generation`, if the
    /// site remembered one then.
    // Cost: O(1), one uncontended lock.
    #[inline]
    pub(crate) fn cached(&self, sites: usize, idx: usize, generation: u64) -> Option<Symbol> {
        match *self.words(sites).get(idx)?.lock().ok()? {
            Some((g, sym)) if g == generation => Some(sym),
            _ => None,
        }
    }

    /// Remember that constant `idx` resolved to type object `sym` under
    /// `generation` (read before the resolution ran, so a resolution that
    /// itself declared something is not remembered past it).
    // Cost: O(1), one uncontended lock.
    pub(crate) fn remember(&self, sites: usize, idx: usize, generation: u64, sym: Symbol) {
        if let Some(word) = self.words(sites).get(idx)
            && let Ok(mut w) = word.lock()
        {
            *w = Some((generation, sym));
        }
    }
}
