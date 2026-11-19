//! Whether any lexical import set in the process may bind a built-in
//! operator's user candidates (ADR-8751 stage 2, #18211).
//!
//! Inside a unit or block with a `use`, the compiler prefixes every built-in
//! operator op with `OpCode::LexImportBuiltinOp`, because whether the `use`
//! imports `infix:<eq>` candidates is only known once it has run (a
//! `sub EXPORT` hook computes its answer at run time, and `require` loads at
//! run time). Almost no program ever binds one, yet each prefix used to cost a
//! dispatch, a world-table read and, under the JIT, a helper call.
//!
//! This flag answers "no set can bind an operator" with one atomic load. It
//! over-approximates on purpose: it is raised before the first set that binds
//! an operator is stored and never lowered, so a reader that finds it clear
//! knows that no interpreter in the process has bound one yet. A reader that
//! finds it raised asks the exact per-world question
//! (`LexicalImportTables::operator_sets`) as before. Never lowering it is what
//! makes it sound with several interpreters and threads sharing one process:
//! no ordering of their writes can clear it while one of them still binds an
//! operator.

use std::sync::atomic::{AtomicU32, Ordering};

/// Non-zero once some import set in the process has bound a built-in
/// operator's dispatcher. The JIT loads it inline as a 32-bit word
/// (`vm_jit_compile`, `OpCode::LexImportBuiltinOp`).
pub(crate) static OPERATOR_IMPORT_POSSIBLE: AtomicU32 = AtomicU32::new(0);

/// Whether some import set may bind a built-in operator's candidates. `false`
/// means no `LexImportBuiltinOp` prefix can find a dispatcher.
// Cost: O(1), one atomic load.
#[inline]
pub(crate) fn operator_import_possible() -> bool {
    OPERATOR_IMPORT_POSSIBLE.load(Ordering::Acquire) != 0
}

/// Record that an import set is about to bind a built-in operator's
/// dispatcher. Called before the set is stored, so a reader that can see the
/// set also sees the flag.
// Cost: O(1), one atomic store.
#[inline]
pub(crate) fn note_operator_import() {
    OPERATOR_IMPORT_POSSIBLE.store(1, Ordering::SeqCst);
}

#[cfg(test)]
mod tests {
    #[test]
    fn the_flag_is_never_lowered_by_a_reader() {
        super::note_operator_import();
        assert!(super::operator_import_possible());
        assert!(super::operator_import_possible());
    }
}
