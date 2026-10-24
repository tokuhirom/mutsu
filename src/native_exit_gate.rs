//! The native-call exit gate (#14864).
//!
//! `std::process::exit` runs libc's `exit()`, whose `atexit` handlers and
//! shared-library destructors tear down C libraries' global state: OpenSSL's
//! `OPENSSL_cleanup` frees the default library context, for one. A worker
//! thread that is still *inside* a `NativeCall` into that library when the main
//! thread ends the program then reads freed or NULL state (`EVP_MD_fetch` →
//! `CRYPTO_THREAD_read_lock` on a NULL-derived object, SIGSEGV). rakudo has the
//! same exit, but a Raku program cannot see this teardown, so mutsu must not
//! let it overlap native code (ADR-12814, guarantee 2: everything the program
//! cannot see stays memory-safe).
//!
//! Every FFI call holds a `NativeCallGuard`; [`exit_process`] raises
//! `EXITING`, waits `QUIESCE_TIMEOUT` for the in-flight calls to return and
//! only then lets `exit()` run its handlers. A thread that tries to start a
//! native call after that point parks forever (the process is on its way out),
//! so no call can begin during teardown either. If a call is still running at
//! the deadline (a thread blocked in `read`/`accept`), the process leaves with
//! `_exit` after flushing C stdio, skipping the handlers that would race it.
//!
//! This is a leaf module (std, plus `libc` for the fallback).

// The browser build has no FFI: nothing there constructs a guard.
#![cfg_attr(target_arch = "wasm32", allow(dead_code))]

use std::cell::Cell;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::time::{Duration, Instant};

/// Native calls currently in flight, process-wide.
static IN_FLIGHT: AtomicUsize = AtomicUsize::new(0);
/// Set once, by [`exit_process`]; never cleared.
static EXITING: AtomicBool = AtomicBool::new(false);

/// How long [`exit_process`] waits for in-flight native calls. A call into a
/// library (a handshake step, a digest) returns in microseconds to
/// milliseconds; this only bounds a thread blocked in native code.
const QUIESCE_TIMEOUT: Duration = Duration::from_millis(250);

thread_local! {
    /// Native calls this thread has in flight (a callback may call native code
    /// again, so it nests). A thread already inside one is never parked: its
    /// outer call is what [`exit_process`] is waiting for.
    static DEPTH: Cell<usize> = const { Cell::new(0) };
}

/// Proof that the holding thread may be executing native code.
pub(crate) struct NativeCallGuard(());

impl NativeCallGuard {
    /// Register a native call about to start. Parks the thread forever when the
    /// process is already exiting.
    // Cost: O(1), two atomic read-modify-writes.
    pub(crate) fn enter() -> Self {
        let nested = DEPTH.with(Cell::get) > 0;
        if !nested && EXITING.load(Ordering::SeqCst) {
            park_forever();
        }
        IN_FLIGHT.fetch_add(1, Ordering::SeqCst);
        // Pairs with the store in `exit_process`: either it saw our increment
        // and waits for us, or we see its flag here and back out.
        if !nested && EXITING.load(Ordering::SeqCst) {
            IN_FLIGHT.fetch_sub(1, Ordering::SeqCst);
            park_forever();
        }
        DEPTH.with(|d| d.set(d.get() + 1));
        Self(())
    }
}

impl Drop for NativeCallGuard {
    fn drop(&mut self) {
        DEPTH.with(|d| d.set(d.get() - 1));
        IN_FLIGHT.fetch_sub(1, Ordering::SeqCst);
    }
}

fn park_forever() -> ! {
    loop {
        std::thread::park();
    }
}

/// Native calls in flight on threads other than the caller.
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
fn other_in_flight() -> usize {
    let own = DEPTH.with(Cell::get);
    IN_FLIGHT.load(Ordering::SeqCst).saturating_sub(own)
}

/// Wait for every other thread's native calls to return and close the gate
/// behind them. `true` when no native code is running anywhere else.
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
fn quiesce() -> bool {
    EXITING.store(true, Ordering::SeqCst);
    let deadline = Instant::now() + QUIESCE_TIMEOUT;
    while other_in_flight() > 0 {
        if Instant::now() >= deadline {
            return false;
        }
        std::thread::sleep(Duration::from_micros(200));
    }
    true
}

/// End the process with `code`. Same as `std::process::exit`, but libc's exit
/// handlers never overlap a thread still running native code.
// Cost: O(w), w = time in-flight native calls take to return, at most 250 ms.
pub fn exit_process(code: i32) -> ! {
    #[cfg(not(target_arch = "wasm32"))]
    if !quiesce() {
        leave_without_handlers(code);
    }
    std::process::exit(code)
}

/// `_exit` after flushing what `exit()` would have: C stdio buffers, which a
/// `NativeCall`ed `printf` fills. Rust's own stdout is flushed by the caller.
#[cfg(all(unix, feature = "native"))]
fn leave_without_handlers(code: i32) -> ! {
    // SAFETY: fflush(NULL) flushes every open C stream; _exit ends the process
    // without running handlers. Neither touches mutsu state.
    unsafe {
        libc::fflush(std::ptr::null_mut());
        libc::_exit(code)
    }
}

#[cfg(not(all(unix, feature = "native")))]
fn leave_without_handlers(code: i32) -> ! {
    std::process::exit(code)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nested_calls_count_and_release() {
        let a = NativeCallGuard::enter();
        let b = NativeCallGuard::enter();
        assert_eq!(DEPTH.with(Cell::get), 2);
        drop(b);
        drop(a);
        assert_eq!(DEPTH.with(Cell::get), 0);
        assert_eq!(other_in_flight(), 0);
    }
}
