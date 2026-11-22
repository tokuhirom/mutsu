//! The run-time half of the replay-safety proof (ADR-18012 §2.3): while a
//! `BEGIN` body that is being recorded runs, no other compiled code may run.
//!
//! The static scan ([`super::scan`]) admits only operations whose built-in
//! form computes a value. What it cannot see is a built-in whose receiver
//! turns out to be a user object (`.comb` on an instance of a class with its
//! own `comb`), an `augment`ed built-in type, a user `infix:<~>` candidate, a
//! regex code block, a `where` clause: each of those runs compiled code other
//! than the body's own.
//!
//! Every dispatch loop that runs a code object's ops, and the JIT's entry into
//! one, takes its polls from `vm_poll::DispatchPolls::enter(code)`, the only
//! constructor there is; while a body is recorded that reports the code to
//! [`note_code_entry`]. TRIR routines report to [`note_routine_entry`]. So any
//! other code a body reaches marks it, and a marked body is not recorded.
//!
//! The report costs one bit test of a state word the loop entry loads anyway;
//! the bit is set only while a body is recorded, which is only during a
//! module's first load.

use std::cell::Cell;
use std::sync::Mutex;

/// How many bodies are recorded right now, process-wide. Guarded so that the
/// count and the `vm_poll` bit change together.
static WATCHING: Mutex<usize> = Mutex::new(0);

thread_local! {
    /// The body being recorded on this thread: its code object, 0 when none.
    static WATCHED: Cell<usize> = const { Cell::new(0) };
    /// Whether that body has reached other compiled code.
    static TAINTED: Cell<bool> = const { Cell::new(false) };
}

/// Code object `code` starts running (reported only while some body is
/// recorded).
// Cost: O(1).
pub(crate) fn note_code_entry(code: &crate::opcode::CompiledCode) {
    let key = std::ptr::from_ref(code) as usize;
    WATCHED.with(|w| {
        let watched = w.get();
        if watched != 0 && watched != key {
            TAINTED.with(|t| t.set(true));
        }
    });
}

/// A TRIR routine starts running: always other code than a body.
// Cost: O(1) (one bit test while nothing is recorded).
#[inline]
pub(crate) fn note_routine_entry() {
    if crate::vm::vm_poll::code_watch_on() && WATCHED.with(Cell::get) != 0 {
        TAINTED.with(|t| t.set(true));
    }
}

// Cost: O(1).
fn adjust_watching(delta: isize) {
    let mut count = WATCHING.lock().unwrap_or_else(std::sync::PoisonError::into_inner);
    let was = *count;
    *count = was.saturating_add_signed(delta);
    if (was == 0) != (*count == 0) {
        crate::vm::vm_poll::set_code_watch(*count != 0);
    }
}

/// Watches one body of `code` while it runs.
pub(crate) struct Watch {
    outer: usize,
    outer_tainted: bool,
}

impl Watch {
    /// Start watching a body of `code`.
    // Cost: O(1).
    pub(crate) fn start(code: &crate::opcode::CompiledCode) -> Watch {
        adjust_watching(1);
        let outer = WATCHED.with(|w| w.replace(std::ptr::from_ref(code) as usize));
        let outer_tainted = TAINTED.with(|t| t.replace(false));
        Watch {
            outer,
            outer_tainted,
        }
    }

    /// Stop watching: whether the body ran no other compiled code.
    // Cost: O(1).
    pub(crate) fn finish(self) -> bool {
        let clean = !TAINTED.with(Cell::get);
        drop(self);
        clean
    }
}

impl Drop for Watch {
    fn drop(&mut self) {
        WATCHED.with(|w| w.set(self.outer));
        // An inner body's run is part of the outer one's.
        TAINTED.with(|t| t.set(self.outer_tainted || t.get()));
        adjust_watching(-1);
    }
}
