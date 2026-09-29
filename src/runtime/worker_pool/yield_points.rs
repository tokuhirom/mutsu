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
//!   An awaiter that arrives only *after* the keep defers onto the keeper's
//!   list the same way, through the [`KeeperMark`] the resolution recorded:
//!   without it, an `await` reached late returned at once and raced the
//!   keeper's straight-line code (#10016).
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
type DeferredList = Arc<Mutex<DeferredState>>;

#[derive(Default)]
struct DeferredState {
    /// How many times the worker has yielded. A [`KeeperMark`] taken at
    /// epoch `e` is still pending exactly while this equals `e`.
    epoch: u64,
    items: Vec<(Instant, Deferred)>,
    /// Test-only: the tick leaves this list alone, so a unit test observes
    /// the yield ordering without racing the wall-clock fallback.
    #[cfg(test)]
    tick_exempt: bool,
}

/// Where a promise was resolved: a pool worker inside a task, and how many
/// times that worker had yielded at the moment (ADR-0105 D2, #10016). An
/// `await` reaching the resolved promise before that worker yields again
/// defers its return onto the worker's list, exactly as a parked awaiter's
/// wake-up was deferred, so it cannot overtake the keeper's straight-line
/// code either.
#[derive(Clone)]
pub(crate) struct KeeperMark {
    list: DeferredList,
    epoch: u64,
}

/// The calling thread's [`KeeperMark`], or `None` when it is not a pool
/// worker running a task (a resolution there wakes everyone immediately).
// Cost: O(1).
pub(crate) fn keeper_mark() -> Option<KeeperMark> {
    if !IN_TASK.with(|c| c.get()) {
        return None;
    }
    let list = DEFERRED.with(|d| d.borrow().clone())?;
    let epoch = list.lock().unwrap_or_else(PoisonError::into_inner).epoch;
    Some(KeeperMark { list, epoch })
}

impl KeeperMark {
    /// Run `f` at the marked worker's next yield. Hands `f` back when that
    /// worker has yielded since the mark (the caller runs it now), when the
    /// caller *is* the marked worker (its own code is already ordered after
    /// its keep), or when no tick can back the deferral.
    // Cost: O(1).
    pub(crate) fn defer(&self, f: Deferred) -> Result<(), Deferred> {
        let own = DEFERRED.with(|d| {
            d.borrow()
                .as_ref()
                .is_some_and(|l| Arc::ptr_eq(l, &self.list))
        });
        if own {
            return Err(f);
        }
        // Armed before the list is locked: a failed spawn delivers every
        // list, this one included.
        if !arm_tick() {
            return Err(f);
        }
        let mut st = self.list.lock().unwrap_or_else(PoisonError::into_inner);
        if st.epoch != self.epoch {
            return Err(f);
        }
        st.items.push((Instant::now(), f));
        Ok(())
    }
}

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
    let list: DeferredList = Arc::new(Mutex::new(DeferredState::default()));
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
        .items
        .push((Instant::now(), f));
    Ok(())
}

/// The calling worker yields: deliver its deferred notifications and end the
/// epoch every outstanding [`KeeperMark`] on it was taken in.
// Cost: O(d), d = notifications this worker deferred since its last yield.
pub(super) fn flush_own() {
    let Some(list) = DEFERRED.with(|d| d.borrow().clone()) else {
        return;
    };
    let pending = {
        let mut guard = list.lock().unwrap_or_else(PoisonError::into_inner);
        guard.epoch += 1;
        std::mem::take(&mut guard.items)
    };
    for (_, f) in pending {
        f();
    }
}

/// Deliver everything on `list` without ending its worker's epoch: the worker
/// has not yielded, the tick just cannot stand behind the deferral any more.
fn run_all(list: &DeferredList) {
    let pending = std::mem::take(&mut list.lock().unwrap_or_else(PoisonError::into_inner).items);
    for (_, f) in pending {
        f();
    }
}

/// Deliver the notifications on `list` deferred before `cutoff`. Returns
/// whether any younger ones remain.
fn run_older_than(list: &DeferredList, cutoff: Instant) -> bool {
    let due: Vec<Deferred> = {
        let mut guard = list.lock().unwrap_or_else(PoisonError::into_inner);
        #[cfg(test)]
        if guard.tick_exempt {
            return false;
        }
        if guard.items.is_empty() {
            return false;
        }
        let (due, young): (Vec<_>, Vec<_>) = std::mem::take(&mut guard.items)
            .into_iter()
            .partition(|(at, _)| *at <= cutoff);
        guard.items = young;
        due.into_iter().map(|(_, f)| f).collect()
    };
    let remaining = !list
        .lock()
        .unwrap_or_else(PoisonError::into_inner)
        .items
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
                    .items
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::{SharedPromise, Value};
    use std::sync::mpsc;

    /// Make this test thread a pool worker inside a task whose deferred list
    /// the wall-clock tick never touches, so only a yield can release it.
    fn become_keeper() -> DeferredList {
        register_worker();
        IN_TASK.with(|c| c.set(true));
        let list = DEFERRED.with(|d| d.borrow().clone()).unwrap();
        list.lock().unwrap().tick_exempt = true;
        list
    }

    fn stop_being_keeper() {
        IN_TASK.with(|c| c.set(false));
        unregister_worker();
    }

    /// #10016: an `await` reaching a promise after a pool worker kept it
    /// returns only at that worker's next yield. Deterministic: the awaiter
    /// is observed deferred on the keeper's list, and nothing but the yield
    /// below can release it.
    #[test]
    fn late_awaiter_waits_for_the_keepers_yield() {
        let list = become_keeper();
        let promise = SharedPromise::new();
        promise.keep(Value::int(7), String::new(), String::new());
        let (tx, rx) = mpsc::channel();
        let awaited = promise.clone();
        let awaiter = std::thread::spawn(move || {
            let (value, _, _) = awaited.wait();
            tx.send(value).unwrap();
        });
        while list.lock().unwrap().items.is_empty() {
            assert!(rx.try_recv().is_err(), "returned before the keeper yielded");
            std::thread::yield_now();
        }
        assert!(rx.try_recv().is_err(), "returned before the keeper yielded");
        flush_own();
        assert_eq!(rx.recv().unwrap(), Value::int(7));
        awaiter.join().unwrap();
        stop_being_keeper();
    }

    /// Once the keeper has yielded, a late `await` returns without deferring.
    #[test]
    fn awaiter_after_the_yield_returns_at_once() {
        let list = become_keeper();
        let promise = SharedPromise::new();
        promise.keep(Value::int(1), String::new(), String::new());
        flush_own();
        let awaited = promise.clone();
        std::thread::spawn(move || awaited.wait()).join().unwrap();
        assert!(list.lock().unwrap().items.is_empty());
        stop_being_keeper();
    }

    /// The keeper awaiting its own promise is already ordered after its keep.
    #[test]
    fn keeper_awaiting_its_own_promise_does_not_defer() {
        let list = become_keeper();
        let promise = SharedPromise::new();
        promise.keep(Value::int(1), String::new(), String::new());
        let _ = promise.wait();
        assert!(list.lock().unwrap().items.is_empty());
        stop_being_keeper();
    }
}
