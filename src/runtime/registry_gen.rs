//! Registry write generations: one process-wide sequence of values, each of
//! which names one registry content.
//!
//! Several memos live in compiled chunks that every interpreter of a process
//! shares (a thread's interpreter runs the same `CompiledCode` as its parent)
//! and are keyed on the registry write generation alone: TRIR's
//! `ClassOperandSite`, `GetBareWord`'s type-object memo, the method-table
//! call-site lane. The same holds for the lexical-import generation and the
//! `LexImportCall` site memo.
//!
//! Every value a generation takes comes from [`next_generation`], so no two
//! interpreters ever count to the same value from different contents: a new
//! interpreter starts at a fresh value and every registry write moves its
//! interpreter to another fresh one. A generation therefore names exactly one
//! registry content, and an interpreter that shares that content may share the
//! number too: a thread's fork starts with its parent's registry snapshot, so
//! it starts at its parent's generation (`TypeState::fork_for_thread`), and the
//! memos its parent filled in shared chunks are current for it (#17337). Its
//! first registry write, or its parent's, moves that side to a value of its own.
//!
//! Before this the counters were per interpreter (each in a range of its own,
//! bumped by one), so a fork's generation never matched its parent's or a
//! sibling's: three ParaSeq workers running the same chunk overwrote each
//! other's site memos on every call, and each call paid the full resolution.

use std::sync::atomic::{AtomicU64, Ordering};

/// The next unused generation value of this process.
static NEXT_GENERATION: AtomicU64 = AtomicU64::new(1);

/// A generation value no interpreter of this process has had yet.
// Cost: O(1), one atomic increment.
#[inline]
pub(crate) fn next_generation() -> u64 {
    NEXT_GENERATION.fetch_add(1, Ordering::Relaxed)
}

impl crate::runtime::Interpreter {
    /// A registry write-generation counter for a new interpreter that shares
    /// no registry content with any other: a value never used before.
    // Cost: O(1), one atomic increment.
    pub(crate) fn fresh_registry_write_gen() -> AtomicU64 {
        AtomicU64::new(next_generation())
    }
}
