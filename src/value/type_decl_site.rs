//! Per-site memo for a typed declaration whose constraint is a plain,
//! unshadowed builtin type name (`my Str $c`, `my Int $i`).
//!
//! `OpCode::SetVarType*` runs on every execution of a typed `my`, and before it
//! can register the constraint it resolves the spelling: a type-capture probe,
//! a package-alias probe, a constant-alias probe and three registry probes
//! asking whether a user `subset`/`enum`/`role` shadows the builtin name. For a
//! core name nothing shadows, every one of them answers "no" — and keeps
//! answering "no" until a declaration changes the registry. So the chunk
//! remembers, per string constant, the registry write generation under which
//! the constant was found to be such a name (#11467).
//!
//! What the memo may assume, and what the reader re-checks on every hit, is
//! decided by `Interpreter::type_decl_constraint_is_plain_builtin`; this type
//! only stores the generation. The generation is per interpreter and every
//! interpreter's counter starts in a range of its own
//! (`Interpreter::fresh_registry_write_gen`), so a memo one thread wrote into a
//! shared chunk never reads as current to another.

use std::sync::OnceLock;
use std::sync::atomic::{AtomicU64, Ordering};

/// One memo word per string constant of a chunk: `generation + 1`, or `0` for
/// "never found plain".
#[derive(Debug, Default)]
pub(crate) struct TypeDeclSiteCaches(OnceLock<Box<[AtomicU64]>>);

impl Clone for TypeDeclSiteCaches {
    /// A cloned chunk starts cold: the memo is an optimization, not state.
    fn clone(&self) -> Self {
        Self::default()
    }
}

impl TypeDeclSiteCaches {
    /// The memo words; `sites` is the chunk's constant count when the table is
    /// first built. A constant appended later has no word and never hits.
    // Cost: O(1) once built; the first call allocates O(sites).
    fn words(&self, sites: usize) -> &[AtomicU64] {
        self.0
            .get_or_init(|| (0..sites).map(|_| AtomicU64::new(0)).collect())
    }

    /// Whether constant `idx` was found to be a plain builtin name under
    /// `generation`.
    // Cost: O(1), one relaxed load.
    #[inline]
    pub(crate) fn cached(&self, sites: usize, idx: usize, generation: u64) -> bool {
        self.words(sites)
            .get(idx)
            .is_some_and(|w| w.load(Ordering::Relaxed) == generation.wrapping_add(1))
    }

    /// Remember that constant `idx` is a plain builtin name under `generation`
    /// (read before the check ran, so a check that itself wrote the registry
    /// is not remembered past it).
    // Cost: O(1), one relaxed store.
    pub(crate) fn remember(&self, sites: usize, idx: usize, generation: u64) {
        if let Some(w) = self.words(sites).get(idx) {
            w.store(generation.wrapping_add(1), Ordering::Relaxed);
        }
    }
}
