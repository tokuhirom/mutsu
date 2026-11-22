//! The run-time half of the replay-safety proof (ADR-18012 §2.3): while a
//! `BEGIN` body that is being recorded runs, no other compiled code may run.
//!
//! The static scan ([`super::scan`]) admits only operations whose built-in
//! form computes a value. What it cannot see is a built-in whose receiver
//! turns out to be a user object (`.comb` on an instance of a class with its
//! own `comb`), an `augment`ed built-in type, a user `infix:<~>` candidate, a
//! regex code block, a `where` clause: each of those runs compiled code other
//! than the body's own. Every such run starts in [`note_code_entry`] (the
//! bytecode run loop's entries) or [`note_routine_entry`] (TRIR), so marking
//! the body there is enough.
//!
//! The check costs one relaxed atomic load while no body is recorded, which is
//! always outside a module's first load.

use std::cell::Cell;
use std::sync::atomic::{AtomicUsize, Ordering};

/// How many threads are recording a body right now.
static WATCHING: AtomicUsize = AtomicUsize::new(0);

thread_local! {
    /// The body being recorded on this thread: its code object, 0 when none.
    static WATCHED: Cell<usize> = const { Cell::new(0) };
    /// Whether that body has reached other compiled code.
    static TAINTED: Cell<bool> = const { Cell::new(false) };
}

/// Code object `code` starts running.
// Cost: O(1) (one relaxed load while nothing is watched).
#[inline]
pub(crate) fn note_code_entry(code: &crate::opcode::CompiledCode) {
    if WATCHING.load(Ordering::Relaxed) != 0 {
        let key = std::ptr::from_ref(code) as usize;
        WATCHED.with(|w| {
            let watched = w.get();
            if watched != 0 && watched != key {
                TAINTED.with(|t| t.set(true));
            }
        });
    }
}

/// A TRIR routine starts running: always other code than a body.
// Cost: O(1).
#[inline]
pub(crate) fn note_routine_entry() {
    if WATCHING.load(Ordering::Relaxed) != 0 && WATCHED.with(Cell::get) != 0 {
        TAINTED.with(|t| t.set(true));
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
        WATCHING.fetch_add(1, Ordering::Relaxed);
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
        WATCHING.fetch_sub(1, Ordering::Relaxed);
    }
}
