//! Cooperative stop-the-world for the cycle scan (design doc §6.1, ADR-0001).
//!
//! Trial deletion mutates and restores GC strong counts, so it is unsound
//! against concurrent mutation. Before this module, the collector simply
//! declined the cycle scan while any worker thread was live — sound, but a
//! long-running threaded program (a server loop) never joins, so its cycles
//! were never reclaimed.
//!
//! The protocol (§6.1 "stop-the-world だが cooperative"):
//!
//! - The collecting thread sets [`STOP_REQUESTED`] and waits until every
//!   *other* mutator thread is **quiescent** — parked at a GC safepoint or
//!   blocked inside a *safe region* (a blocking wait that provably does not
//!   touch the `Gc` graph: promise/channel/lock condvar waits, thread joins,
//!   `sleep`). Bacon-Rajan trial deletion needs no remote root enumeration —
//!   quiescence alone makes the scan sound.
//! - Mutators check the flag at every safepoint ([`park_at_safepoint`]) and
//!   park until released.
//! - Blocking waits use [`stw_aware_wait`] / [`block_quiescent`], which count
//!   the thread quiescent for the duration and — crucially — refuse to *leave*
//!   quiescence while a stop is in progress, so a wake-up mid-scan cannot
//!   start mutating refcounts under the collector (see
//!   [`quiescent_exit_checked`] for the leave protocol).
//! - If quiescence is not reached within a bounded wait (an unwrapped blocking
//!   site, e.g. a raw socket read), the collector gives up, releases the
//!   request, re-queues its suspects, and backs off before retrying
//!   ([`stw_cooldown_active`]) — degrading to the previous defer-until-join
//!   behavior. Timeout is a *liveness* fallback only; it never trades away
//!   soundness.
//!
//! Thread accounting: user worker threads register via
//! `enter_mutator_worker`/`exit_mutator_worker` (`spawn_user_thread`). With
//! exactly one additional mutator (the main thread), the number of *other*
//! mutators the collector must see quiescent is simply the current worker
//! count — whether the collector runs on main (all workers must be quiescent)
//! or on a worker (workers-1 + main). A worker that is mid-exit (dropping its
//! interpreter's `Value`s) is neither quiescent nor unregistered, so the
//! collector keeps waiting until those drops are done.

use std::sync::atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering};
use std::sync::{Condvar, Mutex, MutexGuard, OnceLock};
use std::time::{Duration, Instant};

/// A collect has requested the world to stop. Mutators park at their next
/// safepoint; quiescent waits refuse to complete while this is set.
static STOP_REQUESTED: AtomicBool = AtomicBool::new(false);

/// Number of mutator threads currently quiescent (parked or in a safe region).
static QUIESCENT: AtomicUsize = AtomicUsize::new(0);

/// Monotonic-ish clock (ms since first use) after which STW attempts may run
/// again; set on a timeout so an unwrapped blocking site does not make every
/// candidate-trigger safepoint burn a full STW wait.
static STW_RETRY_AT_MS: AtomicU64 = AtomicU64::new(0);

fn now_ms() -> u64 {
    static EPOCH: OnceLock<Instant> = OnceLock::new();
    EPOCH.get_or_init(Instant::now).elapsed().as_millis() as u64
}

/// Rendezvous for both directions: parked mutators wait on it for release, and
/// the collector waits on it for the quiescent count to reach its target.
fn rendezvous() -> &'static (Mutex<()>, Condvar) {
    static R: OnceLock<(Mutex<()>, Condvar)> = OnceLock::new();
    R.get_or_init(|| (Mutex::new(()), Condvar::new()))
}

/// Whether a stop-the-world is currently requested. One relaxed load — the
/// fast-path cost on safepoints and wait loops.
#[inline]
pub(crate) fn stw_requested() -> bool {
    STOP_REQUESTED.load(Ordering::Acquire)
}

/// Whether STW attempts are in a post-timeout backoff window.
pub(crate) fn stw_cooldown_active() -> bool {
    now_ms() < STW_RETRY_AT_MS.load(Ordering::Acquire)
}

/// Test-only: clear cross-test STW residue (the retry cooldown a previous
/// test's deliberate timeout armed). Called by `gc::test_support::serial_lock`.
#[cfg(test)]
pub(crate) fn test_reset() {
    STW_RETRY_AT_MS.store(0, Ordering::Release);
}

fn quiescent_enter() {
    QUIESCENT.fetch_add(1, Ordering::AcqRel);
    // Wake a collector waiting for the count to reach its target.
    rendezvous().1.notify_all();
}

/// Leave quiescence. If a (possibly new) stop-the-world is in progress at the
/// moment of leaving, the collector may already have counted this thread —
/// re-enter quiescence and park until release, then try to leave again. This
/// closes the race where collector A releases, this thread starts leaving, and
/// collector B achieves a fresh stop with the stale count.
fn quiescent_exit_checked() {
    loop {
        QUIESCENT.fetch_sub(1, Ordering::AcqRel);
        if !stw_requested() {
            return;
        }
        quiescent_enter();
        wait_until_released();
    }
}

/// Block (counted quiescent by the caller) until no stop is requested.
fn wait_until_released() {
    let (lock, cvar) = rendezvous();
    let mut guard = lock.lock().unwrap();
    while stw_requested() {
        let (g, _) = cvar.wait_timeout(guard, Duration::from_millis(10)).unwrap();
        guard = g;
    }
}

/// Park the calling mutator until the stop-the-world (if any) is released.
/// Called from `gc_safepoint` — a single load when no stop is requested.
#[inline]
pub(crate) fn park_at_safepoint() {
    if !stw_requested() {
        return;
    }
    park_slow();
}

#[cold]
fn park_slow() {
    quiescent_enter();
    wait_until_released();
    quiescent_exit_checked();
}

/// Run a blocking operation `f` that provably does not touch the `Gc` graph
/// (a thread join, a `sleep`, an OS wait), counting this thread quiescent for
/// its duration. After `f` returns, the thread leaves quiescence via the
/// checked protocol, so it cannot resume mutation mid-scan.
pub(crate) fn block_quiescent<R>(f: impl FnOnce() -> R) -> R {
    if !super::gc_ptr::gc_enabled() {
        return f();
    }
    quiescent_enter();
    let r = f();
    if stw_requested() {
        wait_until_released();
    }
    quiescent_exit_checked();
    r
}

/// A condvar wait that counts the thread quiescent and is stop-the-world
/// aware: it returns only when `ready(&guard)` holds AND no stop is in
/// progress at the moment of leaving. `ready` must be a pure read (no `Value`
/// clones, no container mutation) — it runs while the thread is counted
/// quiescent.
///
/// The mutex is released for the duration of each inner wait (std condvar
/// semantics), so a collector tracing a node whose `Trace` impl takes this
/// same mutex only contends with the brief wake-and-check windows, never
/// deadlocks. Uses a bounded wait per iteration so a stop release (signalled
/// on a *different* condvar) is observed promptly.
///
/// Note the leave protocol runs while the caller's mutex is HELD (the guard is
/// returned locked). That is safe because leaving only happens when no stop is
/// requested, and the re-park loop inside `quiescent_exit_checked` waits on
/// the global rendezvous — a collector achieving a fresh stop in that window
/// scans without needing this mutex to make progress on *this* thread (it may
/// briefly block tracing this one node, bounded by the 10ms re-check).
pub(crate) fn stw_aware_wait<'a, T>(
    cvar: &Condvar,
    mut guard: MutexGuard<'a, T>,
    mut ready: impl FnMut(&T) -> bool,
) -> MutexGuard<'a, T> {
    if !super::gc_ptr::gc_enabled() {
        // GC off: a plain untimed wait loop — zero polling overhead, identical
        // to the pre-STW behavior of the wrapped call sites.
        while !ready(&guard) {
            guard = cvar.wait(guard).unwrap();
        }
        return guard;
    }
    quiescent_enter();
    loop {
        if ready(&guard) && !stw_requested() {
            break;
        }
        let (g, _) = cvar.wait_timeout(guard, Duration::from_millis(10)).unwrap();
        guard = g;
    }
    quiescent_exit_checked();
    guard
}

/// RAII release for an achieved stop-the-world.
pub(crate) struct StwGuard {
    _private: (),
}

impl Drop for StwGuard {
    fn drop(&mut self) {
        STOP_REQUESTED.store(false, Ordering::Release);
        rendezvous().1.notify_all();
    }
}

/// Attempt to stop the world: request a stop and wait (bounded) until every
/// other mutator thread is quiescent. On success returns a guard that releases
/// the world when dropped — the caller runs the cycle scan while holding it.
/// On timeout the request is withdrawn, a retry cooldown is armed, and `None`
/// returned; the caller falls back to deferring its suspects.
///
/// Only one stop can be in flight; a concurrent second caller fails fast (its
/// suspects are re-queued and picked up later).
pub(crate) fn try_stop_the_world(timeout: Duration) -> Option<StwGuard> {
    if STOP_REQUESTED.swap(true, Ordering::AcqRel) {
        return None;
    }
    let deadline = Instant::now() + timeout;
    let (lock, cvar) = rendezvous();
    let mut guard = lock.lock().unwrap();
    loop {
        // Re-read the target every iteration: a worker finishing mid-wait
        // unregisters itself (after its `Value` drops are done), lowering the
        // number of threads that must park.
        let needed = super::gc_ptr::mutator_worker_count();
        if QUIESCENT.load(Ordering::Acquire) >= needed {
            drop(guard);
            return Some(StwGuard { _private: () });
        }
        let now = Instant::now();
        if now >= deadline {
            drop(guard);
            STOP_REQUESTED.store(false, Ordering::Release);
            cvar.notify_all();
            // Back off: an unwrapped blocking site is holding quiescence up;
            // retrying at every candidate trigger would stall the mutators
            // that DO park. Suspects stay queued and dead sweeps continue.
            STW_RETRY_AT_MS.store(now_ms() + 100, Ordering::Release);
            return None;
        }
        let (g, _) = cvar.wait_timeout(guard, deadline - now).unwrap();
        guard = g;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // The STW statics are process-global, so tests that manipulate the worker
    // count / stop flag must not interleave. cargo runs #[test]s in one
    // process; serialize them.
    fn serial() -> std::sync::MutexGuard<'static, ()> {
        crate::gc::test_support::serial_lock()
    }

    #[test]
    fn stw_reaches_quiescence_with_a_parking_worker() {
        let _s = serial();
        // A fake worker that loops hitting the safepoint park; the "collector"
        // must achieve STW while the worker is alive, and the worker must
        // resume afterwards.
        super::super::gc_ptr::enter_mutator_worker();
        let stop = std::sync::Arc::new(AtomicBool::new(false));
        let stop2 = stop.clone();
        let handle = std::thread::spawn(move || {
            while !stop2.load(Ordering::Relaxed) {
                park_at_safepoint();
                std::thread::yield_now();
            }
        });

        let guard = try_stop_the_world(Duration::from_secs(5));
        assert!(guard.is_some(), "worker parks at safepoints, STW must land");
        // World is stopped: the quiescent count includes the worker.
        assert!(QUIESCENT.load(Ordering::Acquire) >= 1);
        drop(guard);

        stop.store(true, Ordering::Relaxed);
        handle.join().unwrap();
        super::super::gc_ptr::exit_mutator_worker();
    }

    #[test]
    fn stw_times_out_against_a_non_cooperating_worker() {
        let _s = serial();
        // A worker that never parks: the collector must give up cleanly and
        // withdraw the request.
        super::super::gc_ptr::enter_mutator_worker();
        let guard = try_stop_the_world(Duration::from_millis(50));
        assert!(guard.is_none(), "non-parking worker must force a timeout");
        assert!(!stw_requested(), "timeout must withdraw the stop request");
        assert!(stw_cooldown_active(), "timeout must arm the retry cooldown");
        super::super::gc_ptr::exit_mutator_worker();
        // Clear the cooldown for whichever test runs next.
        STW_RETRY_AT_MS.store(0, Ordering::Release);
    }

    #[test]
    fn block_quiescent_counts_and_releases() {
        let _s = serial();
        // With GC off (`MUTSU_GC` unset in a plain `cargo test` run) the
        // wrapper short-circuits and never touches the counter; the counting
        // behavior is only observable under `MUTSU_GC=on` (the CI gc-stress
        // job runs this same test with it set).
        let gc_on = super::super::gc_ptr::gc_enabled();
        let before = QUIESCENT.load(Ordering::Acquire);
        let r = block_quiescent(|| {
            if gc_on {
                assert_eq!(QUIESCENT.load(Ordering::Acquire), before + 1);
            }
            42
        });
        assert_eq!(r, 42);
        assert_eq!(QUIESCENT.load(Ordering::Acquire), before);
    }

    #[test]
    fn stw_aware_wait_leaves_only_when_ready() {
        let _s = serial();
        let pair = std::sync::Arc::new((Mutex::new(false), Condvar::new()));
        let pair2 = pair.clone();
        let waiter = std::thread::spawn(move || {
            let (m, cv) = &*pair2;
            let guard = m.lock().unwrap();
            let guard = stw_aware_wait(cv, guard, |ready| *ready);
            assert!(*guard);
        });
        std::thread::sleep(Duration::from_millis(30));
        {
            let (m, cv) = &*pair;
            *m.lock().unwrap() = true;
            cv.notify_all();
        }
        waiter.join().unwrap();
    }
}
