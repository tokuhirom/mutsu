//! The world declaration lock (ADR-17216 slice 2b).
//!
//! The type registry is shared by every thread, and registering a class or a
//! role is many registry writes plus checks between them: "has this
//! declaration site already registered its class?" (a `class :: { ... }`
//! evaluated on every call of a routine), the method table filled method by
//! method, a failed declaration's rollback of the classes it added. Two
//! threads running the same declaration at once would see each other's half
//! (Red's `grep` declares `my $*OUT = class :: { method put ... }` on every
//! call; two concurrent `grep`s reported "already has a method 'put'").
//!
//! So a type declaration's registration runs under one process-wide lock.
//! It is reentrant on the holding thread, because registering a class
//! registers its nested declarations and can run trait code that declares
//! more. Rakudo serialises composition the same way (the metamodel's
//! compose lock).
//!
//! A declaration that blocks on another thread which itself declares a type
//! (a trait that `await`s a `start { class ... }`) would deadlock; no
//! declaration path waits on another thread today.

use std::sync::{Condvar, Mutex, PoisonError};
use std::thread::ThreadId;

/// The holding thread and its nesting depth.
static HOLDER: Mutex<(Option<ThreadId>, usize)> = Mutex::new((None, 0));
static RELEASED: Condvar = Condvar::new();

/// Held while a type declaration registers; see the module docs.
pub(crate) struct DeclLockGuard {
    _private: (),
}

/// Take the declaration lock, waiting for another thread's declaration to
/// finish; a nested take on the holding thread only deepens it.
// Cost: O(1) uncontended (one mutex acquisition); otherwise the wait.
pub(crate) fn enter() -> DeclLockGuard {
    let me = std::thread::current().id();
    let mut holder = HOLDER.lock().unwrap_or_else(PoisonError::into_inner);
    loop {
        match holder.0 {
            None => {
                *holder = (Some(me), 1);
                break;
            }
            Some(thread) if thread == me => {
                holder.1 += 1;
                break;
            }
            Some(_) => {
                holder = RELEASED
                    .wait(holder)
                    .unwrap_or_else(PoisonError::into_inner);
            }
        }
    }
    DeclLockGuard { _private: () }
}

impl Drop for DeclLockGuard {
    fn drop(&mut self) {
        let mut holder = HOLDER.lock().unwrap_or_else(PoisonError::into_inner);
        holder.1 = holder.1.saturating_sub(1);
        if holder.1 == 0 {
            holder.0 = None;
            RELEASED.notify_one();
        }
    }
}
