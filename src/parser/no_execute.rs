//! The non-executing parse mode (#11212).
//!
//! Two parse-time probes run a `use`d module's mainline to learn how the rest
//! of the unit parses: the dynamic `EXPORT::*` probe (#9500,
//! `runtime::parse_time_exports`) and slang activation (ADR-0026,
//! `runtime::slang_activation`). A caller that promises not to execute the
//! program — `--dump-ast`, `--dump-bytecode`, and the analysis API behind the
//! language server (ADR-0065 D4) — parses under a [`NoExecuteGuard`], and both
//! probes are skipped while it is held.
//!
//! The parse is then degraded, never wrong in a way that runs code: a module's
//! computed export names fall back to what the static export scan found, and a
//! slang-activating `use` leaves the unit in the ordinary grammar.

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
