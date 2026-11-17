//! Suspending the parse of a compilation unit while another unit is compiled
//! and run in the middle of it (ADR-17558).
//!
//! A `use` loads its module when the parser reaches it. The module's own
//! compile is an ordinary parse on the same thread, and the parser keeps its
//! per-unit state in thread-locals (the lexical scopes, the language revision,
//! the slang modes, the memo tables, the warnings collected so far, the source
//! the line numbers are read against, ...). A nested unit parse resets that
//! state at its start and leaves its own behind, so the enclosing parse would
//! continue with the module's state.
//!
//! [`suspend`] moves every piece of per-unit parser state out of its
//! thread-local, leaving the state a fresh thread starts with, and the returned
//! [`SuspendedUnit`] moves it back when dropped. Each module that owns such a
//! thread-local lists it in its own `suspend_unit_state` function, so the
//! thread-locals stay private to their modules. Process-wide tables a nested
//! parse is meant to extend (the module scan caches, the inline-export table,
//! the unit revisions) are not suspended.

use std::cell::{Cell, RefCell};
use std::thread::LocalKey;

/// The suspended state of the enclosing unit's parse; restored on drop.
#[must_use = "dropping the guard restores the suspended parse at once"]
pub(crate) struct SuspendedUnit {
    restores: Vec<Box<dyn FnOnce()>>,
}

impl SuspendedUnit {
    /// Move `key`'s value out, leaving `fresh`, and restore it on drop.
    // Cost: O(1).
    pub(in crate::parser) fn take<T: 'static>(
        &mut self,
        key: &'static LocalKey<RefCell<T>>,
        fresh: T,
    ) {
        let saved = key.with(|cell| std::mem::replace(&mut *cell.borrow_mut(), fresh));
        self.restores.push(Box::new(move || {
            key.with(|cell| *cell.borrow_mut() = saved);
        }));
    }

    /// [`Self::take`] for a `Cell`.
    // Cost: O(1).
    pub(in crate::parser) fn take_cell<T: 'static>(
        &mut self,
        key: &'static LocalKey<Cell<T>>,
        fresh: T,
    ) {
        let saved = key.with(|cell| cell.replace(fresh));
        self.restores.push(Box::new(move || {
            key.with(|cell| cell.set(saved));
        }));
    }
}

impl Drop for SuspendedUnit {
    fn drop(&mut self) {
        // Restored in reverse, so a thread-local listed twice ends at its
        // oldest value.
        while let Some(restore) = self.restores.pop() {
            restore();
        }
    }
}

/// Suspend the parse in progress on this thread until the returned guard is
/// dropped; the code run meanwhile starts from a fresh thread's parser state.
// Cost: O(t), t = per-unit thread-locals (a fixed set, about fifty).
pub(crate) fn suspend() -> SuspendedUnit {
    let mut unit = SuspendedUnit {
        restores: Vec::with_capacity(64),
    };
    super::suspend_unit_state(&mut unit);
    super::stmt::suspend_unit_state(&mut unit);
    super::primary::suspend_unit_state(&mut unit);
    super::expr::suspend_unit_state(&mut unit);
    super::comment_log::suspend_unit_state(&mut unit);
    super::core_type_fold::suspend_unit_state(&mut unit);
    super::decl_doc::suspend_unit_state(&mut unit);
    super::infix_stopper::suspend_unit_state(&mut unit);
    super::soft_sorry::suspend_unit_state(&mut unit);
    super::stmt_ending_brace::suspend_unit_state(&mut unit);
    super::term_origins::suspend_unit_state(&mut unit);
    unit
}
