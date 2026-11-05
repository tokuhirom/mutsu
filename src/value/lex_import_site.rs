//! Per-site memo for a call of a lexically imported routine
//! (ADR-8751 stage 1; `OpCode::LexImportCall`, #14371).
//!
//! A call site inside a block that `use`s a module asks the block's import
//! sets which routine the name is bound to: two hash lookups and an
//! `Arc<FunctionDef>` clone, every call. The answer changes only when a
//! lexical import set is (re)written, so the chunk remembers it for one
//! *import-set generation*: one word per string constant (a site is a call
//! whose name is that constant), holding the generation, the import chain the
//! answer was read through, and the bound definition.
//!
//! The generation is the interpreter's own counter
//! (`ModuleState::lexical_import_gen`), bumped at the one site that writes
//! `lexical_import_sets`, and it starts in a range of its own per interpreter
//! (`Interpreter::fresh_registry_write_gen`), so a word a thread wrote into a
//! shared chunk is never read as current by another interpreter whose import
//! sets differ.
//!
//! This type only stores `(generation, chain, definition)`; what may be
//! memoized is decided by the reader (`exec_lex_import_call_site`).

use crate::ast::FunctionDef;
use std::sync::atomic::{AtomicU32, AtomicU64, Ordering, fence};
use std::sync::{Arc, Mutex, OnceLock};

/// What one site remembered.
#[derive(Debug)]
struct Entry {
    generation: u64,
    chain_idx: u32,
    def: Arc<FunctionDef>,
}

type Word = Mutex<Option<Entry>>;

/// What an operator site remembered: no import set of the chain bound a
/// dispatcher for the operator under this generation.
///
/// A sequence-locked pair `(generation, chain)` so the per-execution read is
/// lock-free (#7817). `seq` is 0 while empty, odd while a writer is storing
/// and even otherwise; a reader accepts the pair only if `seq` is the same
/// non-zero even value before and after reading it. A writer that finds
/// another writer active drops its update: the word is a memo, not state.
#[derive(Debug, Default)]
struct AbsentWord {
    seq: AtomicU32,
    generation: AtomicU64,
    chain_idx: AtomicU32,
}

impl AbsentWord {
    #[inline]
    fn matches(&self, generation: u64, chain_idx: u32) -> bool {
        let before = self.seq.load(Ordering::Acquire);
        if before == 0 || before & 1 == 1 {
            return false;
        }
        let g = self.generation.load(Ordering::Relaxed);
        let c = self.chain_idx.load(Ordering::Relaxed);
        fence(Ordering::Acquire);
        self.seq.load(Ordering::Relaxed) == before && g == generation && c == chain_idx
    }

    fn store(&self, generation: u64, chain_idx: u32) {
        let seq = self.seq.load(Ordering::Relaxed);
        if seq & 1 == 1
            || self
                .seq
                .compare_exchange(
                    seq,
                    seq.wrapping_add(1),
                    Ordering::Acquire,
                    Ordering::Relaxed,
                )
                .is_err()
        {
            return;
        }
        fence(Ordering::Release);
        self.generation.store(generation, Ordering::Relaxed);
        self.chain_idx.store(chain_idx, Ordering::Relaxed);
        // Skip 0 on wrap-around: 0 means "empty".
        let next = seq.wrapping_add(2);
        self.seq
            .store(if next == 0 { 2 } else { next }, Ordering::Release);
    }
}

/// One memo per string constant of a chunk.
#[derive(Debug, Default)]
pub(crate) struct LexImportSiteCaches {
    words: OnceLock<Box<[Word]>>,
    absent: OnceLock<Box<[AbsentWord]>>,
}

impl Clone for LexImportSiteCaches {
    /// A cloned chunk starts cold: the memo is an optimization, not state.
    fn clone(&self) -> Self {
        Self::default()
    }
}

impl LexImportSiteCaches {
    /// The memo words; `sites` is the chunk's constant count when the table is
    /// first built. A constant appended later has no word and never hits.
    // Cost: O(1) once built; the first call allocates O(sites).
    fn words(&self, sites: usize) -> &[Word] {
        self.words
            .get_or_init(|| (0..sites).map(|_| Mutex::new(None)).collect())
    }

    fn absent_words(&self, sites: usize) -> &[AbsentWord] {
        self.absent
            .get_or_init(|| (0..sites).map(|_| AbsentWord::default()).collect())
    }

    /// Whether operator constant `idx` was found bound by no import set of
    /// `chain_idx` under `generation` (`OpCode::LexImportBuiltinOp`, #7817).
    // Cost: O(1), three atomic loads.
    #[inline]
    pub(crate) fn operator_absent(
        &self,
        sites: usize,
        idx: usize,
        generation: u64,
        chain_idx: u32,
    ) -> bool {
        self.absent_words(sites)
            .get(idx)
            .is_some_and(|word| word.matches(generation, chain_idx))
    }

    /// Remember that no import set of `chain_idx` binds operator constant
    /// `idx` under `generation` (read before the lookup it records was made).
    // Cost: O(1), a few atomic stores.
    pub(crate) fn remember_operator_absent(
        &self,
        sites: usize,
        idx: usize,
        generation: u64,
        chain_idx: u32,
    ) {
        if let Some(word) = self.absent_words(sites).get(idx) {
            word.store(generation, chain_idx);
        }
    }

    /// The definition constant `idx` was filled with for `chain_idx` under
    /// `generation`, if any.
    // Cost: O(1), one uncontended lock and an `Arc` clone.
    #[inline]
    pub(crate) fn cached(
        &self,
        sites: usize,
        idx: usize,
        generation: u64,
        chain_idx: u32,
    ) -> Option<Arc<FunctionDef>> {
        match &*self.words(sites).get(idx)?.lock().ok()? {
            Some(e) if e.generation == generation && e.chain_idx == chain_idx => {
                Some(Arc::clone(&e.def))
            }
            _ => None,
        }
    }

    /// Remember `def` for constant `idx` read through `chain_idx` under
    /// `generation` (read before the lookup it records was made).
    // Cost: O(1), one uncontended lock.
    pub(crate) fn remember(
        &self,
        sites: usize,
        idx: usize,
        generation: u64,
        chain_idx: u32,
        def: Arc<FunctionDef>,
    ) {
        if let Some(word) = self.words(sites).get(idx)
            && let Ok(mut w) = word.lock()
        {
            *w = Some(Entry {
                generation,
                chain_idx,
                def,
            });
        }
    }
}
