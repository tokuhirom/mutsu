//! One mixin type is composed by one thread at a time (#18266).
//!
//! The type registry is one world shared by every thread (ADR-17216), and
//! composing a mixin type `C+{R}` writes it in several steps: the class
//! declaration first, then the role's methods, submethods and HOW. A second
//! thread that asked for the same type in between found the class already
//! registered, took it as composed, and dispatched on a class that did not
//! have the role's methods yet ("No such method 'mark' for invocant of type
//! 'Conc::Maker[Any]+{Marker}'", about 1 run in 60 of
//! `t/concurrency/thread-lock/role-pun-shared-across-threads.t`).
//!
//! `ensure_mixin_class_with_args` holds this lock from its "is it already
//! registered?" check to the type's publication, so a thread either composes
//! the whole type or waits for the thread that does. Rakudo serializes the
//! same step (the metamodel's mixin cache is guarded by a lock).
//!
//! The lock is re-entrant per thread: composing a stacked type composes the
//! type below it first, through the same function.

use std::cell::Cell;
use std::sync::{Mutex, MutexGuard};

static MIXIN_COMPOSE: Mutex<()> = Mutex::new(());

thread_local! {
    /// How deep this thread is inside mixin composition.
    static DEPTH: Cell<u32> = const { Cell::new(0) };
}

/// Held while this thread composes (or checks for) a mixin type.
pub(crate) struct MixinComposeGuard {
    /// The process-wide lock, held by the outermost guard of a thread only.
    _outer: Option<MutexGuard<'static, ()>>,
}

impl MixinComposeGuard {
    /// Take the composition lock, unless this thread already holds it. A
    /// single-threaded program takes an uncontended mutex per composition,
    /// which runs once per mixin type.
    // Cost: O(1), plus the wait for another thread's composition.
    pub(crate) fn acquire() -> Self {
        let depth = DEPTH.with(Cell::get);
        let outer = (depth == 0).then(|| {
            // A panic while composing leaves no half-written lock state worth
            // refusing later compositions over: recover the guard.
            MIXIN_COMPOSE
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
        });
        DEPTH.with(|d| d.set(depth + 1));
        Self { _outer: outer }
    }
}

impl Drop for MixinComposeGuard {
    fn drop(&mut self) {
        DEPTH.with(|d| d.set(d.get().saturating_sub(1)));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_lock_is_reentrant_on_one_thread() {
        let outer = MixinComposeGuard::acquire();
        let inner = MixinComposeGuard::acquire();
        assert_eq!(DEPTH.with(Cell::get), 2);
        drop(inner);
        drop(outer);
        assert_eq!(DEPTH.with(Cell::get), 0);
        // Released: another acquisition does not block.
        drop(MixinComposeGuard::acquire());
    }
}
