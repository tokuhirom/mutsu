//! ADR-8673 D1: a TRIR-to-TRIR call pushes no routine-stack entry until
//! something can observe it.
//!
//! The entry an inner call would push is recorded here instead, together with
//! everything the eager push reads at call time (the call-site line and file).
//! Before an op that can observe the routine stack runs, the TRIR loop calls
//! [`Interpreter::trir_observe`], which pushes every recorded entry not yet on
//! the stack, bottom up, and switches `current_unit` the way the eager push
//! would have. A call returns by popping its entry only if it was pushed.
//!
//! Which ops observe is decided by where they are dispatched
//! (`exec.rs`): the main loop lists the ops proved not to look at the stack,
//! and every other op reaches `exec_trir_observing_op` through one arm that
//! calls `trir_observe` first. An op added later is therefore an observer
//! until it is deliberately listed as pure.
//!
//! `MUTSU_TRIR_LAZY_FRAMES=0` restores the eager push, for bisection. The
//! profiler, whose sampler walks the routine stack at any poll, also gets the
//! eager push.

use super::TrChunk;
use crate::runtime::Interpreter;
use crate::symbol::Symbol;

/// The routine-stack entry of a TRIR call that has not been pushed yet.
#[derive(Debug)]
pub(crate) struct LazyRoutine {
    package: Symbol,
    name: Symbol,
    line: Option<u32>,
    file: Option<Symbol>,
    def_file: Option<Symbol>,
    hidden: bool,
    /// The `current_unit` the push replaced, once pushed: put back on pop.
    saved_unit: Option<Symbol>,
}

/// The entries of the live TRIR calls (oldest first), and how many of them,
/// counted from the bottom, are on the routine stack.
#[derive(Debug, Default)]
pub(crate) struct LazyFrames {
    entries: Vec<LazyRoutine>,
    live: usize,
}

impl LazyFrames {
    /// Whether some entry is not on the routine stack yet.
    // Cost: O(1).
    #[inline(always)]
    pub(crate) fn pending(&self) -> bool {
        self.live != self.entries.len()
    }
}

/// Whether inner TRIR calls push their entries lazily.
// Cost: O(1) after the first call.
pub(crate) fn lazy_frames_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| {
        std::env::var("MUTSU_TRIR_LAZY_FRAMES").map_or(true, |v| v != "0")
            && !crate::vm::vm_poll::profiler_armed()
    })
}

impl Interpreter {
    /// Record the entry a call to `chunk` would push now, without pushing it.
    // Cost: O(d), d = the frames `innermost_executing_file` walks when no
    // recorded entry names the call site's file (O(1) inside a TRIR caller).
    pub(super) fn trir_defer_routine(&mut self, chunk: &TrChunk) {
        let file = self.trir_call_site_file();
        let entry = LazyRoutine {
            package: self.current_package_sym(),
            name: chunk.name,
            line: self.current_source_line(),
            file,
            def_file: chunk.def_file.get().copied(),
            hidden: chunk.is_hidden_from_backtrace,
            saved_unit: None,
        };
        self.trir.lazy.entries.push(entry);
        super::stats::record_lazy_deferred();
    }

    /// The file the eager push would record as the call site: the file of
    /// the innermost running routine. While a recorded entry is not pushed,
    /// that routine is the innermost recorded one, whose declaration file the
    /// stack walk would have found on its frame.
    // Cost: O(1) when a recorded entry answers; otherwise the stack walk of
    // `executing_source_file_sym`.
    fn trir_call_site_file(&self) -> Option<Symbol> {
        let lazy = &self.trir.lazy;
        if lazy.pending()
            && let Some(top) = lazy.entries.last()
            && let Some(file) = top.def_file
        {
            return Some(file);
        }
        if lazy.pending() {
            // The innermost routine declares no file: the walk stops at its
            // frame and answers the dynamically scoped `?FILE`.
            return self.current_source_file_sym();
        }
        self.executing_source_file_sym()
    }

    /// Drop the innermost recorded entry, popping it off the routine stack
    /// if it was pushed.
    // Cost: O(1).
    pub(super) fn trir_finish_deferred_routine(&mut self) {
        let Some(entry) = self.trir.lazy.entries.pop() else {
            return;
        };
        if self.trir.lazy.live > self.trir.lazy.entries.len() {
            self.trir.lazy.live -= 1;
            if let Some(unit) = entry.saved_unit {
                self.current_unit = unit;
            }
            self.pop_routine();
        }
    }

    /// An op that can observe the routine stack is about to run: push every
    /// recorded entry that is not on it yet.
    // Cost: O(1) when nothing is pending; otherwise O(p) pushes, p = pending
    // entries (each pushed once per call at most).
    #[inline(always)]
    pub(crate) fn trir_observe(&mut self) {
        if self.trir.lazy.pending() {
            self.trir_materialize_frames();
        }
    }

    // Cost: O(p), p = pending entries.
    #[cold]
    #[inline(never)]
    fn trir_materialize_frames(&mut self) {
        while self.trir.lazy.live < self.trir.lazy.entries.len() {
            let i = self.trir.lazy.live;
            let e = &self.trir.lazy.entries[i];
            let (package, name, line, file, def_file, hidden) =
                (e.package, e.name, e.line, e.file, e.def_file, e.hidden);
            self.push_routine_with_visibility(
                package, name, line, file, def_file, hidden, 0, false,
            );
            let saved = def_file.map(|file| {
                let unit = self.unit_of_source_sym(Some(file));
                std::mem::replace(&mut self.current_unit, unit)
            });
            self.trir.lazy.entries[i].saved_unit = saved;
            self.trir.lazy.live += 1;
            super::stats::record_lazy_pushed();
        }
    }
}
