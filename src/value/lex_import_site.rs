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
use std::sync::{Arc, Mutex, OnceLock};

/// What one site remembered.
#[derive(Debug)]
struct Entry {
    generation: u64,
    chain_idx: u32,
    def: Arc<FunctionDef>,
}

type Word = Mutex<Option<Entry>>;

/// One memo per string constant of a chunk.
#[derive(Debug, Default)]
pub(crate) struct LexImportSiteCaches(OnceLock<Box<[Word]>>);

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
        self.0
            .get_or_init(|| (0..sites).map(|_| Mutex::new(None)).collect())
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
