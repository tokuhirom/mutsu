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
///
/// `user` is the same shape for a constraint naming a registered user class or
/// role (`my Tag $t`, `Interpreter::type_decl_constraint_is_plain_user_type`),
/// whose resolution also depends on the running package: each word packs
/// `generation + 1` (high 32 bits) with the package symbol's id (low 32), so
/// one atomic load answers both and a racing writer cannot tear the pair.
#[derive(Debug, Default)]
pub(crate) struct TypeDeclSiteCaches(OnceLock<Box<[AtomicU64]>>, OnceLock<Box<[AtomicU64]>>);

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

    /// The user-type memo words, built like [`Self::words`].
    // Cost: O(1) once built; the first call allocates O(sites).
    fn user_words(&self, sites: usize) -> &[AtomicU64] {
        self.1
            .get_or_init(|| (0..sites).map(|_| AtomicU64::new(0)).collect())
    }

    /// The user-type memo key for `generation` and the running package's
    /// symbol id, or `None` once the generation no longer fits the packed
    /// word (the memo then simply never hits).
    // Cost: O(1).
    #[inline]
    pub(crate) fn user_key(generation: u64, package_id: u32) -> Option<u64> {
        let stamp = generation
            .checked_add(1)
            .filter(|g| *g <= u64::from(u32::MAX))?;
        Some((stamp << 32) | u64::from(package_id))
    }

    /// Whether constant `idx` was found to be a plain user type name under
    /// `key` ([`Self::user_key`]).
    // Cost: O(1), one relaxed load.
    #[inline]
    pub(crate) fn user_cached(&self, sites: usize, idx: usize, key: u64) -> bool {
        self.user_words(sites)
            .get(idx)
            .is_some_and(|w| w.load(Ordering::Relaxed) == key)
    }

    /// Remember that constant `idx` is a plain user type name under `key`.
    // Cost: O(1), one relaxed store.
    pub(crate) fn remember_user(&self, sites: usize, idx: usize, key: u64) {
        if let Some(w) = self.user_words(sites).get(idx) {
            w.store(key, Ordering::Relaxed);
        }
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
