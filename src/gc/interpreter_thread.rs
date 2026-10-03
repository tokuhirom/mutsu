//! Automatic stop-the-world registration for threads that run an interpreter
//! (#11714). See [`register_interpreter_thread`].

use super::stw::{
    mark_thread_registered, preregister_worker_quiescent, thread_is_registered, worker_started,
};

/// A thread that registered itself by building an interpreter (see
/// [`register_interpreter_thread`]). Unregisters when the thread exits.
struct InterpreterThreadRegistration;

impl InterpreterThreadRegistration {
    /// The same birth protocol as a `spawn_user_thread` worker: raise the
    /// count while counted quiescent, then leave quiescence through the
    /// checked exit, which parks first if a collector already stopped the
    /// world without this thread in its target.
    fn enter() -> Self {
        super::gc_ptr::enter_mutator_worker();
        preregister_worker_quiescent();
        worker_started();
        InterpreterThreadRegistration
    }
}

impl Drop for InterpreterThreadRegistration {
    fn drop(&mut self) {
        // Runs as a TLS destructor. Drop the thread's `Gc`-bearing
        // thread-locals while still registered, as `spawn_user_thread`'s
        // `WorkerGuard` does. A thread-local that built an interpreter (e.g.
        // `regex_parse`'s `VALIDATE_INTERP`) was initialized after this slot,
        // so its destructor has already run: destructors run in reverse order.
        crate::value::drop_thread_local_gc_state();
        mark_thread_registered(false);
        super::gc_ptr::exit_mutator_worker();
    }
}

thread_local! {
    static INTERPRETER_THREAD_REGISTRATION: InterpreterThreadRegistration =
        InterpreterThreadRegistration::enter();
}

/// Register the calling thread as a mutator for the rest of its life, unless
/// it already is one (the CLI main thread, a `spawn_user_thread` worker).
/// Called by `Interpreter::new`, so every thread that runs an interpreter
/// counts toward a collector's quiescence target.
///
/// Before this (#11714), an embedder or `cargo test` thread running its own
/// interpreter was invisible to the accounting. A collector on another such
/// thread saw no other mutator, skipped the stop-the-world, and drained the
/// process-global candidate buffer, which holds this thread's nodes too. Its
/// trial deletion then decremented strong counts that this thread was still
/// dropping (`Gc::drop strong-count underflow`), and its reclaim window's
/// global `collecting()` flag silently skipped this thread's own decrements.
///
/// Only with the collector on: with `MUTSU_GC=off` nothing is ever scanned,
/// so there is nothing to coordinate with, and a registered thread that is
/// idle outside a safepoint would only make other collectors time out.
///
/// The registration lasts until thread exit rather than until the
/// interpreter's drop, because an interpreter's `Value`s are dropped after any
/// `Drop` impl on it could run. A long-lived thread that keeps no interpreter
/// but stays registered costs liveness only: a collector times out on it and
/// defers its scan (see the module docs), it never scans under it.
pub(crate) fn register_interpreter_thread() {
    if thread_is_registered() || !super::gc_ptr::gc_enabled() {
        return;
    }
    // `try_with`: an interpreter built by another TLS destructor during thread
    // teardown finds this slot gone; that thread is about to exit anyway.
    let _ = INTERPRETER_THREAD_REGISTRATION.try_with(|_| ());
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gc::stw::other_mutators_active;

    /// #11714: a thread that builds an interpreter joins the quiescence
    /// target (with the collector on) and leaves it when the thread exits.
    #[test]
    fn an_interpreter_thread_registers_until_it_exits() {
        let _s = crate::gc::test_support::serial_lock();
        let gc_on = super::super::gc_ptr::gc_enabled();
        let before = super::super::gc_ptr::mutator_worker_count();
        let (registered_tx, registered_rx) = std::sync::mpsc::channel();
        let (release_tx, release_rx) = std::sync::mpsc::channel::<()>();
        let thread = std::thread::spawn(move || {
            assert!(!thread_is_registered());
            register_interpreter_thread();
            // A second interpreter on the same thread must not count it twice.
            register_interpreter_thread();
            registered_tx.send(thread_is_registered()).unwrap();
            release_rx.recv().unwrap();
        });
        assert_eq!(registered_rx.recv().unwrap(), gc_on);
        if gc_on {
            assert_eq!(super::super::gc_ptr::mutator_worker_count(), before + 1);
            // This (unregistered) thread now sees another mutator, so its
            // collector would stop the world instead of scanning under it.
            assert!(other_mutators_active());
        } else {
            assert_eq!(super::super::gc_ptr::mutator_worker_count(), before);
        }
        release_tx.send(()).unwrap();
        thread.join().unwrap();
        assert_eq!(super::super::gc_ptr::mutator_worker_count(), before);
    }
}
