//! What a pool worker owes the rest of the program when it yields
//! (ADR-0105 D2/D4).
//!
//! A worker "yields" when it reaches a blocking point (the park hook in
//! `enter_blocking`), when its task ends, or — for a worker that does neither
//! for a while — on the pool tick. Two kinds of work wait for that moment:
//!
//! - **Deferred notifications** (D2): an `await` wake-up granted by a promise
//!   this worker kept. Delivering it at the yield rather than at the `keep`
//!   means the woken thread never overtakes the keeper's own straight-line
//!   code (Rakudo's F3 ordering, ADR-0105 Appendix A experiment 3). The tick
//!   delivers a notification only once it has waited [`WAKE_GRACE`]: that
//!   fallback exists for a keeper that never yields (a CPU-bound loop, an
//!   unhooked blocking call), and delivering at the first 10ms tick let an
//!   interpreted keeper's straight-line code lose the race it exists to win.
//! - **Deferred starts** (D4): tasks this worker submitted while no worker was
//!   idle. They stay queued until the submitter parks (which grows the pool),
//!   ends its task (it dequeues them itself), or the tick grows the pool for a
//!   submitter that is still running.
//!
//! The tick runs on a GC-registered helper thread, armed only while something
//! is deferred; it exits after a second with nothing to do.

use super::native::{IN_TASK, grow_as_needed};
use std::cell::RefCell;
use std::sync::{Arc, Condvar, Mutex, OnceLock, PoisonError};
use std::time::{Duration, Instant};

/// A notification deferred to the worker's next yield.
pub(crate) type Deferred = Box<dyn FnOnce() + Send + 'static>;
/// A worker's deferred notifications, each with the moment it was deferred.
type DeferredList = Arc<Mutex<Vec<(Instant, Deferred)>>>;

/// How long a worker may keep running before the tick delivers what it
/// deferred and starts what it submitted — the cadence of Rakudo's
/// supervisor (ADR-0105 D4).
const TICK: Duration = Duration::from_millis(10);

/// How long a deferred notification may wait for its worker to yield before
/// the tick delivers it anyway. Only a keeper that does not yield at all pays
/// it; one that parks or ends its task delivers at once.
const WAKE_GRACE: Duration = Duration::from_millis(100);

/// An armed-but-idle tick thread exits after this long.
const TICK_IDLE_EXIT: Duration = Duration::from_secs(1);

thread_local! {
    /// This worker's deferred notifications (set for the worker's lifetime).
    static DEFERRED: RefCell<Option<DeferredList>> = const { RefCell::new(None) };
}

/// Every live worker's deferred list, for the tick.
fn all_lists() -> &'static Mutex<Vec<DeferredList>> {
    static LISTS: OnceLock<Mutex<Vec<DeferredList>>> = OnceLock::new();
    LISTS.get_or_init(|| Mutex::new(Vec::new()))
}

/// Give the calling worker a deferred list. Called once per worker.
pub(super) fn register_worker() {
    let list: DeferredList = Arc::new(Mutex::new(Vec::new()));
    all_lists()
        .lock()
        .unwrap_or_else(PoisonError::into_inner)
        .push(list.clone());
    DEFERRED.with(|d| *d.borrow_mut() = Some(list));
}

/// Drop the calling worker's deferred list, delivering anything left on it.
pub(super) fn unregister_worker() {
    flush_own();
    let Some(list) = DEFERRED.with(|d| d.borrow_mut().take()) else {
        return;
    };
    all_lists()
        .lock()
        .unwrap_or_else(PoisonError::into_inner)
        .retain(|l| !Arc::ptr_eq(l, &list));
}

/// Defer `f` to the calling worker's next yield. Hands `f` back when the
/// caller is not a pool worker inside a task (or the tick cannot run), in
/// which case the caller runs it now.
pub(crate) fn defer_until_yield(f: Deferred) -> Result<(), Deferred> {
    if !IN_TASK.with(|c| c.get()) {
        return Err(f);
    }
    let Some(list) = DEFERRED.with(|d| d.borrow().clone()) else {
        return Err(f);
    };
    if !arm_tick() {
        return Err(f);
    }
    list.lock()
        .unwrap_or_else(PoisonError::into_inner)
        .push((Instant::now(), f));
    Ok(())
}

/// Deliver the calling worker's deferred notifications.
// Cost: O(d), d = notifications this worker deferred since its last yield.
pub(super) fn flush_own() {
    let Some(list) = DEFERRED.with(|d| d.borrow().clone()) else {
        return;
    };
    run_all(&list);
}

fn run_all(list: &DeferredList) {
    let pending = {
        let mut guard = list.lock().unwrap_or_else(PoisonError::into_inner);
        if guard.is_empty() {
            return;
        }
        std::mem::take(&mut *guard)
    };
    for (_, f) in pending {
        f();
    }
}

/// Deliver the notifications on `list` deferred before `cutoff`. Returns
/// whether any younger ones remain.
fn run_older_than(list: &DeferredList, cutoff: Instant) -> bool {
    let due: Vec<Deferred> = {
        let mut guard = list.lock().unwrap_or_else(PoisonError::into_inner);
        if guard.is_empty() {
            return false;
        }
        let (due, young): (Vec<_>, Vec<_>) = std::mem::take(&mut *guard)
            .into_iter()
            .partition(|(at, _)| *at <= cutoff);
        *guard = young;
        due.into_iter().map(|(_, f)| f).collect()
    };
    let remaining = !list
        .lock()
        .unwrap_or_else(PoisonError::into_inner)
        .is_empty();
    for f in due {
        f();
    }
    remaining
}

/// The calling thread reached a blocking point: release what it owes
/// (ADR-0105 D2's deferred wake-ups, D3's rendezvous). Runs before the
/// thread takes the lock it is about to block on.
// Cost: O(d + b), d = deferred notifications, b = rendezvous owed.
pub(super) fn on_park() {
    crate::value::promise_wake::release_borrowed_wakes();
    if IN_TASK.with(|c| c.get()) {
        flush_own();
    }
}

struct TickState {
    armed: bool,
    running: bool,
}

fn tick() -> &'static (Mutex<TickState>, Condvar) {
    static TICK_STATE: OnceLock<(Mutex<TickState>, Condvar)> = OnceLock::new();
    TICK_STATE.get_or_init(|| {
        (
            Mutex::new(TickState {
                armed: false,
                running: false,
            }),
            Condvar::new(),
        )
    })
}

/// Make sure a tick comes within [`TICK`]. `false` when no tick thread can be
/// started; the caller then must not defer anything.
// Cost: O(1).
pub(super) fn arm_tick() -> bool {
    let (lock, cvar) = tick();
    let mut st = lock.lock().unwrap_or_else(PoisonError::into_inner);
    if !st.armed {
        st.armed = true;
        cvar.notify_one();
    }
    if st.running {
        return true;
    }
    st.running = true;
    drop(st);
    match crate::runtime::builtins_system::try_spawn_gc_helper_thread("pool-tick", tick_loop) {
        Ok(_) => true,
        Err(_) => {
            let mut st = lock.lock().unwrap_or_else(PoisonError::into_inner);
            st.running = false;
            st.armed = false;
            drop(st);
            // Another worker may have deferred against the tick this spawn
            // was meant to start: deliver everything now rather than strand it.
            let lists: Vec<DeferredList> = all_lists()
                .lock()
                .unwrap_or_else(PoisonError::into_inner)
                .clone();
            for list in &lists {
                run_all(list);
            }
            false
        }
    }
}

fn tick_loop() {
    let (lock, cvar) = tick();
    loop {
        // Wait (quiescent: touches no `Gc` state) until something is
        // deferred, or exit once idle for a while.
        let keep_going = crate::gc::block_quiescent(|| {
            let mut st = lock.lock().unwrap_or_else(PoisonError::into_inner);
            loop {
                if st.armed {
                    return true;
                }
                let (g, timeout) = cvar
                    .wait_timeout(st, TICK_IDLE_EXIT)
                    .unwrap_or_else(PoisonError::into_inner);
                st = g;
                if timeout.timed_out() && !st.armed {
                    st.running = false;
                    return false;
                }
            }
        });
        if !keep_going {
            return;
        }
        crate::gc::block_quiescent(|| std::thread::sleep(TICK));
        // Disarm before delivering: anything deferred from here on re-arms
        // the next tick.
        lock.lock().unwrap_or_else(PoisonError::into_inner).armed = false;
        let lists: Vec<DeferredList> = all_lists()
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .clone();
        let cutoff = Instant::now().checked_sub(WAKE_GRACE);
        let mut waiting = false;
        for list in &lists {
            waiting |= match cutoff {
                Some(cutoff) => run_older_than(list, cutoff),
                None => !list
                    .lock()
                    .unwrap_or_else(PoisonError::into_inner)
                    .is_empty(),
            };
        }
        if waiting {
            // Notifications still inside their grace period: tick again.
            lock.lock().unwrap_or_else(PoisonError::into_inner).armed = true;
        }
        grow_as_needed();
    }
}
