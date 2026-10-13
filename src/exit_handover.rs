//! The worker-to-main `exit` handover of ADR-11617.
//!
//! `exit` ends the *process* in Raku, wherever it is called, and the process
//! still runs its END phasers on the way out. Those phasers belong to the main
//! program's interpreter, so a worker thread that calls `exit` does not end the
//! process itself: it records the status here and waits for the main
//! interpreter to *adopt* it — unwind its mainline, run END, and exit with the
//! worker's status. A main thread that never comes back to a poll (a long native
//! computation) does not adopt within [`GRACE`]; the worker then *abandons* the
//! handover and exits the process directly, without END, as before.
//!
//! This is a leaf module (std only): the value layer's blocking waits read
//! [`interrupts_current_thread`], so it cannot live under `crate::runtime`.
//!
//! States, each move made under [`WAKE`]'s mutex:
//!
//! ```text
//! IDLE --request--> REQUESTED --adopt--> EXITING
//!   |                   `--grace runs out--> ABANDONED
//!   |--adopt (main called `exit` itself)--> EXITING
//!   `--adopt (main ran off its end)--> FINISHED
//! ```
//!
//! A worker `exit` in `EXITING` parks: the process is already on its way out
//! and the first `exit` owns the status. In `FINISHED` (the main program is
//! past END, joining its outstanding threads) and `ABANDONED` it ends the
//! process directly.

use std::cell::Cell;
use std::sync::atomic::{AtomicI64, AtomicU8, Ordering};
use std::sync::{Condvar, Mutex};

const IDLE: u8 = 0;
const REQUESTED: u8 = 1;
const EXITING: u8 = 2;
const FINISHED: u8 = 3;
const ABANDONED: u8 = 4;

/// How long a requesting worker waits for the main interpreter to adopt its
/// `exit` before it ends the process itself. A main thread that is running Raku
/// code, sleeping or awaiting answers within one poll (milliseconds); this only
/// bounds the case where it is stuck in native code that never polls.
#[cfg(not(target_arch = "wasm32"))] // the browser build has no worker threads
pub(crate) const GRACE: std::time::Duration = std::time::Duration::from_secs(1);

/// Read without the lock on the hot path ([`requested`]); written only under
/// [`WAKE`]'s mutex.
static STATE: AtomicU8 = AtomicU8::new(IDLE);
static CODE: AtomicI64 = AtomicI64::new(0);
static WAKE: (Mutex<()>, Condvar) = (Mutex::new(()), Condvar::new());

thread_local! {
    /// Set on the thread that runs the main program (`main.rs`), the only one
    /// whose waits a handover interrupts.
    static ADOPTER: Cell<bool> = const { Cell::new(false) };
}

/// Mark the calling thread as the one whose interpreter adopts worker exits.
pub(crate) fn mark_adopter_thread() {
    ADOPTER.with(|a| a.set(true));
}

/// Whether a worker's `exit` is waiting to be adopted.
// Cost: O(1), one relaxed atomic load.
#[inline(always)]
pub(crate) fn requested() -> bool {
    STATE.load(Ordering::Relaxed) == REQUESTED
}

/// Whether a blocking wait on this thread should give up now: a handover is
/// pending and this is the thread that has to adopt it. Waits on every other
/// thread are unaffected.
// Cost: O(1).
#[inline]
pub(crate) fn interrupts_current_thread() -> bool {
    requested() && ADOPTER.with(Cell::get)
}

/// Whether the calling thread is the one that adopts worker exits: its
/// blocking waits must be able to notice [`requested`].
// Cost: O(1).
#[inline]
pub(crate) fn is_adopter_thread() -> bool {
    ADOPTER.with(Cell::get)
}

/// How often a wait that has no wake-up from [`request`] re-checks it on the
/// adopter thread (a `Thread` join, a GC-off condvar wait).
pub(crate) const POLL: std::time::Duration = std::time::Duration::from_millis(10);

/// What [`request`] found.
#[cfg(not(target_arch = "wasm32"))] // the browser build has no worker threads
#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Request {
    /// This call made the request; wait for it with [`await_adoption`].
    Made,
    /// The process is already exiting (another worker's `exit` is pending or
    /// was adopted, or the main program called `exit`); the first `exit`
    /// latches the status, and the process is about to end.
    AlreadyExiting,
    /// The main program is past its END phasers (or a handover was
    /// abandoned): nothing will adopt this `exit`, so end the process directly.
    Closed,
}

/// Ask the main interpreter to end the process with status `code`.
// Cost: O(1).
#[cfg(not(target_arch = "wasm32"))] // the browser build has no worker threads
pub(crate) fn request(code: i64) -> Request {
    let _guard = lock();
    match STATE.load(Ordering::Relaxed) {
        IDLE => {
            CODE.store(code, Ordering::Relaxed);
            STATE.store(REQUESTED, Ordering::Release);
            WAKE.1.notify_all();
            Request::Made
        }
        REQUESTED | EXITING => Request::AlreadyExiting,
        _ => Request::Closed,
    }
}

/// Close the handover on behalf of the main interpreter, returning the pending
/// worker status, if any, for it to adopt. Called once the main program is
/// about to run its END phasers. `main_exiting` says the main program called
/// `exit` itself; with a pending request it is exiting either way.
// Cost: O(1).
pub(crate) fn adopt(main_exiting: bool) -> Option<i64> {
    let _guard = lock();
    let prev = STATE.load(Ordering::Relaxed);
    match prev {
        // The worker gave up waiting and is ending the process right now, or
        // an earlier call already closed the handover.
        ABANDONED | EXITING | FINISHED => return None,
        _ => {}
    }
    let next = if main_exiting || prev == REQUESTED {
        EXITING
    } else {
        FINISHED
    };
    STATE.store(next, Ordering::Release);
    WAKE.1.notify_all();
    (prev == REQUESTED).then(|| CODE.load(Ordering::Relaxed))
}

/// Wait (at most `grace`) for the main interpreter to adopt this worker's
/// request. `true`: adopted, so the main thread ends the process and the caller
/// only has to stay out of its way. `false`: the handover is abandoned, and the
/// caller ends the process itself.
///
/// The caller runs this counted GC-quiescent: it touches no `Value`.
// Cost: O(1) plus the wait.
#[cfg(not(target_arch = "wasm32"))] // the browser build has no worker threads
pub(crate) fn await_adoption(grace: std::time::Duration) -> bool {
    let guard = lock();
    let (_guard, _) = WAKE
        .1
        .wait_timeout_while(guard, grace, |_| STATE.load(Ordering::Relaxed) == REQUESTED)
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    if STATE.load(Ordering::Relaxed) == EXITING {
        return true;
    }
    STATE.store(ABANDONED, Ordering::Release);
    false
}

/// Sleep for `duration` on the adopter thread, waking early when a worker's
/// `exit` is pending. Returns whether it was interrupted. Other threads sleep
/// the whole duration.
// Cost: O(1) plus the sleep.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn sleep_interruptible(duration: std::time::Duration) -> bool {
    if !ADOPTER.with(Cell::get) {
        std::thread::sleep(duration);
        return false;
    }
    let guard = lock();
    let _ = WAKE
        .1
        .wait_timeout_while(guard, duration, |_| !requested())
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    requested()
}

/// Park the calling thread for the rest of the process's life. A worker whose
/// `exit` was adopted, or lost to an earlier one, waits here for the main
/// thread to end the process.
// Cost: never returns.
#[cfg(not(target_arch = "wasm32"))] // the browser build has no worker threads
pub(crate) fn park_forever() -> ! {
    loop {
        std::thread::park();
    }
}

fn lock() -> std::sync::MutexGuard<'static, ()> {
    WAKE.0
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
}

#[cfg(test)]
mod tests {
    // The state machine is process-global, so its transitions are pinned in one
    // test that restores IDLE at the end.
    use super::*;
    use std::time::Duration;

    #[test]
    fn a_request_is_adopted_once_and_closes_the_handover() {
        STATE.store(IDLE, Ordering::SeqCst);
        assert_eq!(request(3), Request::Made);
        assert!(requested());
        assert_eq!(request(4), Request::AlreadyExiting);
        let waiter = std::thread::spawn(|| await_adoption(Duration::from_secs(10)));
        assert_eq!(adopt(false), Some(3));
        assert!(waiter.join().unwrap());
        assert!(!requested());
        // The process is on its way out: a later worker `exit` waits for it.
        assert_eq!(request(5), Request::AlreadyExiting);
        assert_eq!(adopt(false), None);
        // A main program that ran off its end closes the handover for good.
        STATE.store(IDLE, Ordering::SeqCst);
        assert_eq!(adopt(false), None);
        assert_eq!(request(5), Request::Closed);
        // An unanswered request is abandoned after the grace period, and an
        // adopt that comes later takes nothing over.
        STATE.store(IDLE, Ordering::SeqCst);
        assert_eq!(request(6), Request::Made);
        assert!(!await_adoption(Duration::from_millis(1)));
        assert_eq!(adopt(true), None);
        assert_eq!(request(7), Request::Closed);
        STATE.store(IDLE, Ordering::SeqCst);
    }
}
