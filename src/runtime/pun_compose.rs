//! One thread at a time composes a role's pun class (#18858).
//!
//! A pun class is composed on first use and found by name afterwards
//! (`ensure_parametric_role_pun_class_for`, `ensure_plain_role_pun_class`).
//! The declaration registry is shared by every thread (ADR-17216), and
//! composing a class takes several registry writes: the class entry first,
//! then its methods, accessors and composed roles. A second thread that
//! asked for the same pun in between found the class by name, took it as
//! composed and dispatched on it with half its methods missing. Monad's
//! `t/14_concurrent_types.t` lost worker threads to
//! `No such method 'some' for invocant of type 'Monad::Maybe'` that way. Two
//! threads could also both miss and compose the same pun at once.
//!
//! So composing a pun takes one process-wide lock, and the name being
//! composed is recorded until the class is complete. A thread that finds the
//! class while another thread is still composing it waits for the lock; once
//! it holds the lock, nobody else is composing, and the registry answers.
//!
//! The lock is a [`LockRuntime`], the runtime of Raku's own `Lock`: it is
//! reentrant (composing a pun can compose another), and a thread waiting on
//! it counts as quiescent for the cycle collector's stop-the-world.

use std::collections::HashMap;
use std::sync::{Mutex, OnceLock};
use std::thread::ThreadId;

use crate::runtime::native_methods::state_lock::LockRuntime;
use crate::value::RuntimeError;

/// The lock every pun composition holds.
fn compose_lock() -> &'static LockRuntime {
    static LOCK: OnceLock<LockRuntime> = OnceLock::new();
    LOCK.get_or_init(LockRuntime::default)
}

/// The pun names being composed now, and the thread composing each.
fn in_progress() -> &'static Mutex<HashMap<String, ThreadId>> {
    static NAMES: OnceLock<Mutex<HashMap<String, ThreadId>>> = OnceLock::new();
    NAMES.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Whether a thread other than this one is composing the pun `name` now: its
/// registry entry, if any, is not complete yet.
// Cost: O(n) expected, n = name.len() (one hash under an uncontended mutex).
pub(crate) fn composed_elsewhere(name: &str) -> bool {
    let names = in_progress().lock().unwrap_or_else(|e| e.into_inner());
    if names.is_empty() {
        return false;
    }
    names
        .get(name)
        .is_some_and(|owner| *owner != std::thread::current().id())
}

/// The composition lock, held until dropped.
pub(crate) struct PunComposeGuard {
    me: ThreadId,
    /// The name this guard marked as being composed, cleared on drop.
    marked: Option<String>,
}

impl PunComposeGuard {
    /// Take the composition lock, waiting while another thread composes.
    // Cost: O(1) uncontended; a contended wait lasts as long as the other
    // thread's composition.
    pub(crate) fn acquire() -> Result<Self, RuntimeError> {
        let me = std::thread::current().id();
        crate::runtime::native_methods::acquire_lock(compose_lock(), me)?;
        Ok(Self { me, marked: None })
    }

    /// Record that this thread is composing the pun `name` from now until
    /// the guard is dropped.
    // Cost: O(n) expected, n = name.len().
    pub(crate) fn mark(&mut self, name: &str) {
        let mut names = in_progress().lock().unwrap_or_else(|e| e.into_inner());
        // A reentrant composition of the same name (a pun whose role body
        // asks for the pun again) keeps the outer mark.
        if names.insert(name.to_string(), self.me).is_none() {
            self.marked = Some(name.to_string());
        }
    }
}

impl Drop for PunComposeGuard {
    // Cost: O(n) expected, n = the marked name's length.
    fn drop(&mut self) {
        if let Some(name) = self.marked.take() {
            in_progress()
                .lock()
                .unwrap_or_else(|e| e.into_inner())
                .remove(&name);
        }
        let _ = crate::runtime::native_methods::release_lock(compose_lock(), self.me);
    }
}
