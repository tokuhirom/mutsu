//! The non-executing parse mode (#11212).
//!
//! A compiling parse runs code while it parses: a `use` loads its module in
//! the program's interpreter when the parser reaches it (ADR-17558,
//! `crate::compile_time_loader`), and slang activation (ADR-0026,
//! `runtime::slang_activation`) runs a slang module's mainline. A caller that
//! promises not to execute the program — `--dump-ast`, `--dump-bytecode`, and
//! the analysis API behind the language server (ADR-0065 D4) — parses under a
//! [`NoExecuteGuard`], and neither runs while it is held.
//!
//! The parse is then degraded, never wrong in a way that runs code: a module's
//! exports come from the static export scan alone (names it computes while it
//! loads are unknown), and a slang-activating `use` leaves the unit in the
//! ordinary grammar.

use std::cell::Cell;

thread_local! {
    static NO_EXECUTE: Cell<bool> = const { Cell::new(false) };
}

/// Whether the current parse must not run any module code.
pub(crate) fn no_execute() -> bool {
    NO_EXECUTE.with(Cell::get)
}

/// Holds the non-executing parse mode on this thread until dropped. Nests: the
/// previous mode is restored on drop, so an inner guard never re-enables
/// execution for an outer non-executing caller.
pub(crate) struct NoExecuteGuard {
    previous: bool,
}

impl NoExecuteGuard {
    pub(crate) fn enter() -> Self {
        let previous = NO_EXECUTE.with(|f| f.replace(true));
        Self { previous }
    }
}

impl Drop for NoExecuteGuard {
    fn drop(&mut self) {
        NO_EXECUTE.with(|f| f.set(self.previous));
    }
}
