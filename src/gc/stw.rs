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
//! Thread accounting: every mutator thread REGISTERS — workers via
//! `spawn_user_thread` (counter raised parent-side to close the spawn window,
//! per-thread flag set in the worker) and the CLI main thread via
//! `mutsu::gc_register_main_thread`. Only registered threads count toward the
//! quiescence target (`needed = registered_total - self`) and increment the
//! quiescent counter, so the equation stays self-consistent under arbitrary
//! extra threads (e.g. `cargo test` harness threads running in-process
//! interpreters — those obey a stop but are invisible to the accounting). A
//! worker that is mid-exit (dropping its interpreter's `Value`s) is neither
//! quiescent nor unregistered, so the collector keeps waiting until those
//! drops are done.

use crate::runtime::thread_compat::Instant;
use std::sync::atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering};
use std::sync::{Condvar, Mutex, MutexGuard, OnceLock};
use std::time::Duration;

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

thread_local! {
    /// Whether the current thread is a REGISTERED mutator (the CLI main thread
    /// and every `spawn_user_thread` worker). Only registered threads count
    /// toward the quiescence target and increment [`QUIESCENT`] — an
    /// unregistered thread (e.g. a `cargo test` harness thread running an
    /// in-process interpreter) obeys a stop (it blocks at park points and on
    /// leaving safe regions) but is invisible to the accounting, so it can
    /// neither satisfy nor starve another collector's rendezvous. This keeps
    /// the equation self-consistent under arbitrary thread populations:
    /// `needed = registered_total - (self if registered)`, and every counted
    /// quiescent thread is also counted in the target.
    static REGISTERED_MUTATOR: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// Mark the calling thread as a registered mutator (see [`REGISTERED_MUTATOR`]).
/// Called by `spawn_user_thread` on each worker and by `main` on startup; the
/// `enter_mutator_worker`/`exit_mutator_worker` counter is maintained
/// separately (parent-side, to close the spawn window).
pub(crate) fn mark_thread_registered(on: bool) {
    REGISTERED_MUTATOR.with(|r| r.set(on));
}

fn thread_is_registered() -> bool {
    REGISTERED_MUTATOR.with(|r| r.get())
}

/// Whether any *other* registered mutator thread exists (the collector uses
/// this to decide if a stop-the-world is needed at all).
pub(crate) fn other_mutators_active() -> bool {
    let total = super::gc_ptr::mutator_worker_count();
    let self_counted = usize::from(thread_is_registered());
    total > self_counted
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
    // SeqCst (not AcqRel): this store participates in the Dekker-style handshake
    // with `try_stop_the_world` (which stores STOP then loads QUIESCENT). AcqRel
    // does not order a store-then-load pair against another thread's store-then-
    // load on the *other* location (StoreLoad reordering is permitted), so with
    // AcqRel the collector could read a stale "quiescent" count for a worker that
    // has already read a stale "no stop" and resumed mutating — corrupting trial
    // deletion mid-scan. Making both sides' store+load SeqCst forces a single
    // total order in which at least one side observes the other's write.
    QUIESCENT.fetch_add(1, Ordering::SeqCst);
    // Wake a collector waiting for the count to reach its target.
    rendezvous().1.notify_all();
}

/// Wake a collector blocked in [`try_stop_the_world`] so it re-evaluates its
/// quiescence equation. Called from `gc_ptr::exit_mutator_worker`: a worker
/// EXITING (unregistering) satisfies the equation by lowering `needed`, not by
/// raising [`QUIESCENT`], so without this wake the collector slept its full
/// remaining timeout and only noticed at the deadline — during short-lived
/// worker churn (a `Promise.start` per loop iteration) some worker is almost
/// always mid-exit at stop time, so nearly every collect paid the whole STW
/// timeout as pause (S17-lowlevel/semaphore.t: ~50ms × hundreds of collects =
/// CI timeout).
pub(crate) fn notify_worker_exit() {
    if super::gc_ptr::gc_enabled() {
        rendezvous().1.notify_all();
    }
}

/// Leave quiescence. If a (possibly new) stop-the-world is in progress at the
/// moment of leaving, the collector may already have counted this thread —
/// re-enter quiescence and park until release, then try to leave again. This
/// closes the race where collector A releases, this thread starts leaving, and
/// collector B achieves a fresh stop with the stale count.
fn quiescent_exit_checked() {
    loop {
        // Both the count decrement and the stop check are SeqCst: this is the
        // mutator side of the Dekker handshake with `try_stop_the_world`. Ordering
        // them SeqCst guarantees that if the collector achieved a stop by counting
        // this thread quiescent, this thread observes the stop here and re-parks
        // (below) instead of returning to mutate the `Gc` graph under the scan.
        QUIESCENT.fetch_sub(1, Ordering::SeqCst);
        if !STOP_REQUESTED.load(Ordering::SeqCst) {
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

/// Count a not-yet-started worker thread quiescent on the parent's behalf,
/// right before `spawn_user_thread` creates it. This closes the *birth window*
/// (the parent's `enter_mutator_worker` → the worker's first safepoint or safe
/// region): the unborn worker is already in the quiescence target, but until
/// it starts executing it cannot park, so during a spawn burst (`Promise.start`
/// per loop iteration) some thread is almost always inside clone3/thread setup
/// and `try_stop_the_world` burns its full timeout at nearly every candidate
/// trigger — S17-lowlevel/semaphore.t (4000 rapid `Promise.start`s) degraded
/// from seconds to a CI timeout under `MUTSU_GC=on`. Counting the unborn
/// worker quiescent is sound: it provably touches no `Gc` state until
/// [`worker_started`] leaves quiescence via the checked protocol.
pub(crate) fn preregister_worker_quiescent() {
    if super::gc_ptr::gc_enabled() {
        quiescent_enter();
    }
}

/// The spawned worker's first act (before any user code): become a registered
/// mutator and leave the parent-granted quiescent state. The checked exit
/// parks first if a stop-the-world is in progress, so the worker cannot start
/// mutating refcounts mid-scan. Pairs with [`preregister_worker_quiescent`].
pub(crate) fn worker_started() {
    mark_thread_registered(true);
    if super::gc_ptr::gc_enabled() {
        quiescent_exit_checked();
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
    if thread_is_registered() {
        quiescent_enter();
        wait_until_released();
        quiescent_exit_checked();
    } else {
        // Unregistered threads still obey the stop (they may be running an
        // in-process interpreter, e.g. under `cargo test`), they just do not
        // count toward the rendezvous.
        wait_until_released();
    }
}

/// Run a blocking operation `f` that provably does not touch the `Gc` graph
/// (a thread join, a `sleep`, an OS wait), counting this thread quiescent for
/// its duration. After `f` returns, the thread leaves quiescence via the
/// checked protocol, so it cannot resume mutation mid-scan.
pub(crate) fn block_quiescent<R>(f: impl FnOnce() -> R) -> R {
    if !super::gc_ptr::gc_enabled() {
        return f();
    }
    if !thread_is_registered() {
        // Not part of the accounting; still refuse to resume mid-scan.
        let r = f();
        if stw_requested() {
            wait_until_released();
        }
        return r;
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
/// [`stw_aware_wait`] against a mutex this takes itself, so the wasm build can
/// release it between rounds.
///
/// Natively this is `stw_aware_wait` with the lock taken for you, and the
/// result is always `Some`. On wasm nobody else can ever set the condition
/// while this thread waits — there is no other thread — so instead of parking
/// on the condvar it runs the cooperative scheduler ([`crate::runtime::wasm_sched::pump`])
/// between checks, dropping the guard each round so the task that resolves the
/// wait can take the same lock. `None` means the pump ran dry: the waiter is
/// blocked on something that can never happen, and the caller should raise a
/// deadlock error rather than spin forever.
pub(crate) fn wait_until<'a, T>(
    mutex: &'a Mutex<T>,
    cvar: &Condvar,
    ready: impl FnMut(&T) -> bool,
) -> Option<MutexGuard<'a, T>> {
    #[cfg(not(target_arch = "wasm32"))]
    {
        let guard = mutex.lock().unwrap();
        Some(stw_aware_wait(cvar, guard, ready))
    }
    #[cfg(target_arch = "wasm32")]
    {
        let _ = cvar;
        let mut ready = ready;
        loop {
            let guard = mutex.lock().unwrap();
            if ready(&guard) {
                return Some(guard);
            }
            drop(guard);
            if !crate::runtime::wasm_sched::pump() {
                return None;
            }
        }
    }
}

/// What a `None` from [`wait_until`] means, phrased for the user who hit it.
pub(crate) const DEADLOCK_MESSAGE: &str = "deadlock: nothing left to run while waiting. The WASM build runs on a single \
     thread, so a `start` block that blocks on a value sent only after it began \
     can never be woken";

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
    if !thread_is_registered() {
        // Uncounted, but still refuses to leave the wait mid-scan.
        loop {
            if ready(&guard) && !stw_requested() {
                return guard;
            }
            let (g, _) = cvar.wait_timeout(guard, Duration::from_millis(10)).unwrap();
            guard = g;
        }
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
    // wasm32 has exactly one thread, so the world is already stopped: whoever
    // called the collector IS the only mutator, and every "worker" is a task on
    // `runtime::wasm_sched`'s queue that by definition is not running. Take the
    // stop unconditionally — the rendezvous below would otherwise wait on a
    // condvar (which wasm32 std cannot do) for a quiescence count that queued,
    // never-started tasks keep artificially high.
    #[cfg(target_arch = "wasm32")]
    {
        let _ = timeout;
        return Some(StwGuard { _private: () });
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        // SeqCst: the collector side of the Dekker handshake. This store must be
        // totally ordered against every mutator's `quiescent_exit_checked`
        // (QUIESCENT store then STOP load) so a worker leaving quiescence cannot slip
        // past the stop into `Gc` mutation while this collector counts it quiescent.
        if STOP_REQUESTED.swap(true, Ordering::SeqCst) {
            return None;
        }
        let deadline = Instant::now() + timeout;
        let (lock, cvar) = rendezvous();
        let mut guard = lock.lock().unwrap();
        let self_counted = usize::from(thread_is_registered());
        loop {
            // Re-read the target every iteration: a worker finishing mid-wait
            // unregisters itself (after its `Value` drops are done), lowering the
            // number of threads that must park. The collector's own thread, if
            // registered, is running this very function — exclude it.
            let needed = super::gc_ptr::mutator_worker_count().saturating_sub(self_counted);
            // SeqCst load (see the `swap` above): pairs with the mutators' SeqCst
            // QUIESCENT stores to complete the handshake's total order.
            if QUIESCENT.load(Ordering::SeqCst) >= needed {
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
            mark_thread_registered(true);
            while !stop2.load(Ordering::Relaxed) {
                park_at_safepoint();
                std::thread::yield_now();
            }
            mark_thread_registered(false);
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
        // Counting applies to REGISTERED mutator threads only.
        mark_thread_registered(true);
        let before = QUIESCENT.load(Ordering::Acquire);
        let r = block_quiescent(|| {
            if gc_on {
                assert_eq!(QUIESCENT.load(Ordering::Acquire), before + 1);
            }
            42
        });
        assert_eq!(r, 42);
        assert_eq!(QUIESCENT.load(Ordering::Acquire), before);
        mark_thread_registered(false);
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
