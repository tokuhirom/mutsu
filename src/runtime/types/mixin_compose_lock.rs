//! One mixin type is composed by one thread at a time (#18266).
//!
//! The type registry is one world shared by every thread (ADR-17216), and
//! composing a mixin type `C+{R}` writes it in several steps: the class
//! declaration first, then the role's methods, submethods and HOW. A second
//! thread that asked for the same type in between found the class already
//! registered, took it as composed, and dispatched on a class that did not
//! have the role's methods yet (`No such method 'mark' for invocant of type
//! 'Conc::Maker[Any]+{Marker}'`, about 1 run in 60 of
//! `t/concurrency/thread-lock/role-pun-shared-across-threads.t`).
//!
//! `ensure_mixin_class_with_args` holds this lock from its "is it already
//! registered?" check to the type's publication, so a thread either composes
//! the whole type or waits for the thread that does. Rakudo serializes the
//! same step (the metamodel's mixin cache is guarded by a lock).
//!
//! It is the same lock a role's pun class is composed under
//! (`pun_compose.rs`, #18858): composing a mixin can pun a role and punning
//! one can compose a mixin, so two locks could be taken in opposite orders by
//! two threads. That lock is re-entrant per thread (composing a stacked type
//! composes the type below it first, through the same function), and a
//! thread waiting on it counts as quiescent for the cycle collector's
//! stop-the-world.

use crate::runtime::pun_compose::PunComposeGuard;
use crate::value::RuntimeError;

/// Held while this thread composes (or checks for) a mixin type.
pub(crate) struct MixinComposeGuard {
    _lock: PunComposeGuard,
}

impl MixinComposeGuard {
    /// Take the type-composition lock (re-entrant on one thread). A
    /// single-threaded program takes it uncontended, once per mixin type.
    // Cost: O(1), plus the wait for another thread's composition.
    pub(crate) fn acquire() -> Result<Self, RuntimeError> {
        Ok(Self {
            _lock: PunComposeGuard::acquire()?,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_lock_is_reentrant_on_one_thread() {
        let outer = MixinComposeGuard::acquire().unwrap();
        let inner = MixinComposeGuard::acquire().unwrap();
        drop(inner);
        drop(outer);
        // Released: another thread can take it.
        std::thread::spawn(|| drop(MixinComposeGuard::acquire().unwrap()))
            .join()
            .unwrap();
    }
}
