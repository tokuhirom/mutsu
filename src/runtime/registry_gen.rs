//! The starting value of an interpreter's registry write generation.
//!
//! Several memos live in compiled chunks that every interpreter of a process
//! shares (a thread's interpreter runs the same `CompiledCode` as its parent)
//! and are keyed on the registry write generation alone: TRIR's
//! `ClassOperandSite`, `GetBareWord`'s type-object memo. A thread's registry
//! is a snapshot that diverges from its parent's, so a generation number is
//! only meaningful to the interpreter that counted it. Starting every
//! interpreter's counter in a range of its own keeps one interpreter's memo
//! from ever reading as current to another.

use std::sync::atomic::{AtomicU64, Ordering};

/// Each interpreter's counter starts at a distinct multiple of this.
/// 2^40 registry writes by one interpreter are out of reach.
const EPOCH_STRIDE: u64 = 1 << 40;

static NEXT_EPOCH: AtomicU64 = AtomicU64::new(0);

impl crate::runtime::Interpreter {
    /// A registry write-generation counter for a new interpreter, starting in
    /// a range no other interpreter of this process uses.
    // Cost: O(1), one atomic increment.
    pub(crate) fn fresh_registry_write_gen() -> AtomicU64 {
        AtomicU64::new(
            NEXT_EPOCH
                .fetch_add(1, Ordering::Relaxed)
                .wrapping_mul(EPOCH_STRIDE),
        )
    }
}
