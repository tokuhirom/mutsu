//! Process-wide elastic worker pool for short-lived user tasks (ADR-0020,
//! growth policy per ADR-0123).
//!
//! `submit` runs a task on a warm pooled worker when one is idle. Otherwise the
//! pool decides between growing and queueing:
//!
//! - **Grow freely** while fewer than `8 x cores` workers are *active* (not
//!   blocked) and a worker stack fits the address-space budget
//!   (`stack_budget`).
//! - **Queue** past that point: a worker that is still running will come back
//!   for the task. This is what bounds a burst of 64 CPU-bound `start` blocks
//!   to the budget instead of 64 x 256 MiB of stacks (#9377).
//! - **Grow regardless** when no worker is running at all -- every live worker
//!   is blocked (`await`, a channel receive, `sleep`, a join). mutsu's `await`
//!   holds its thread, so a queued task may be exactly what those workers wait
//!   for; queueing it would deadlock. The new worker steps down to a smaller
//!   stack, past the budget if it must.
//! - **Reject** the task only when the OS refuses even that thread: its
//!   rejecter turns the failure into a catchable Raku error (a broken
//!   `start` promise), never a panic.
//!
//! "Blocked" is observed at the GC's blocking choke points: `gc::block_quiescent`
//! and `gc::wait_until` call `enter_blocking`, which is also where the "no
//! worker is running" case is detected -- the moment the last running worker
//! blocks, the queue gets a new worker.
//!
//! A soft floor of `min(cores, 8)` workers stays alive, and workers beyond the
//! floor exit after an idle grace period.
//!
//! On wasm32 there is no pool: `submit` delegates to `spawn_user_thread`,
//! whose cooperative scheduler is already a pool of one.

use crate::runtime::builtins_system::SpawnError;

/// Reports a task the pool could not run to whoever is waiting for it.
pub(crate) type Rejecter = Box<dyn FnOnce(&SpawnError) + Send + 'static>;

#[cfg(not(target_arch = "wasm32"))]
mod yield_points;

#[cfg(not(target_arch = "wasm32"))]
mod native {
    use super::Rejecter;
    use crate::runtime::builtins_system::{SpawnError, StackPolicy};
    use std::cell::Cell;
    use std::collections::VecDeque;
    use std::sync::{Condvar, Mutex, MutexGuard, OnceLock, PoisonError};
    use std::time::{Duration, Instant};

    pub(super) struct Task {
        pub(super) run: Box<dyn FnOnce() + Send + 'static>,
        pub(super) reject: Option<Rejecter>,
    }

    pub(super) struct PoolState {
        pub(super) queue: VecDeque<Task>,
        /// Workers parked in `wait_for_task` (not running or blocked in a task).
        pub(super) idle: usize,
        /// Total live workers (idle + starting + running + blocked).
        pub(super) live: usize,
        /// Workers spawned but not yet at their first `wait_for_task`: each is
        /// already spoken for by one queued task.
        pub(super) starting: usize,
        /// Workers inside a task that are blocked (see `enter_blocking`).
        pub(super) blocked: usize,
    }

    impl PoolState {
        /// Workers that will come back for queued work without anyone else
        /// making progress: idle, starting or running.
        fn active(&self) -> usize {
            self.live - self.blocked
        }

        /// Workers inside a task and not blocked.
        fn running(&self) -> usize {
            self.live - self.idle - self.starting - self.blocked
        }

        /// Queued tasks that no idle or starting worker is about to take.
        fn unclaimed(&self) -> usize {
            self.queue.len().saturating_sub(self.idle + self.starting)
        }

        /// ADR-0123's growth rule: how to grow the pool, or `None` to leave
        /// unclaimed tasks queued for a running worker.
        pub(super) fn growth(&self, soft_cap: usize) -> Option<StackPolicy> {
            if self.unclaimed() == 0 {
                return None;
            }
            if self.running() == 0 {
                // Everyone is blocked: nobody is coming back for the queue.
                return Some(StackPolicy::Required);
            }
            (self.active() < soft_cap).then_some(StackPolicy::Budgeted)
        }
    }

    /// Lock the pool state. No user code ever runs under this lock, so a
    /// poisoned lock can only mean a panic in the bookkeeping itself, whose
    /// counters are still the best information there is: keep going.
    pub(super) fn lock_pool() -> MutexGuard<'static, PoolState> {
        pool().0.lock().unwrap_or_else(PoisonError::into_inner)
    }

    /// Idle grace period for workers above the keep-alive floor.
    const IDLE_GRACE: Duration = Duration::from_secs(1);

    pub(super) fn pool() -> &'static (Mutex<PoolState>, Condvar) {
        static POOL: OnceLock<(Mutex<PoolState>, Condvar)> = OnceLock::new();
        POOL.get_or_init(|| {
            (
                Mutex::new(PoolState {
                    queue: VecDeque::new(),
                    idle: 0,
                    live: 0,
                    starting: 0,
                    blocked: 0,
                }),
                Condvar::new(),
            )
        })
    }

    fn cores() -> usize {
        static CORES: OnceLock<usize> = OnceLock::new();
        *CORES.get_or_init(|| {
            std::thread::available_parallelism()
                .map(|n| n.get())
                .unwrap_or(4)
        })
    }

    /// Soft floor of kept-alive workers. 256 MiB *reserved* stack each makes
    /// this an address-space budget (ADR-0020 §3.2): `min(cores, 8)`.
    fn keep_alive_floor() -> usize {
        cores().min(8)
    }

    /// Active workers past which the pool queues instead of growing, as long
    /// as some worker is running. Rakudo's `ThreadPoolScheduler` default
    /// `max_threads` (8 x cores): past it, rakudo queues too.
    // Cost: O(1), cores = the process CPU count.
    pub(super) fn soft_cap() -> usize {
        cores() * 8
    }

    /// Escape hatch: `MUTSU_POOL=off` restores thread-per-task for A/B
    /// comparison and flake triage.
    pub(super) fn pool_enabled() -> bool {
        static ENABLED: OnceLock<bool> = OnceLock::new();
        *ENABLED.get_or_init(|| {
            !matches!(
                std::env::var("MUTSU_POOL").as_deref(),
                Ok("off") | Ok("0") | Ok("no")
            )
        })
    }

    thread_local! {
        /// Whether this thread is a pool worker currently running a task.
        pub(super) static IN_TASK: Cell<bool> = const { Cell::new(false) };
        /// Nesting depth of `enter_blocking` on this thread; only the
        /// outermost level is counted.
        static BLOCK_DEPTH: Cell<u32> = const { Cell::new(0) };
    }

    /// Grow the pool per [`PoolState::growth`] until it says to stop, and
    /// report any task that could not get a worker. Called with no lock held,
    /// on a thread that is free to run a rejecter (it holds no lock the
    /// rejected task's waiter could need).
    pub(in crate::runtime::worker_pool) fn grow_as_needed() {
        let cvar = &pool().1;
        loop {
            let policy = {
                let mut st = lock_pool();
                let Some(policy) = st.growth(soft_cap()) else {
                    return;
                };
                st.live += 1;
                st.starting += 1;
                policy
            };
            let err = match crate::runtime::builtins_system::try_spawn_user_thread(
                "pool",
                policy,
                worker_loop,
            ) {
                Ok(_) => {
                    crate::vm::vm_stats::record_pool_spawn();
                    continue;
                }
                Err(e) => e,
            };
            let rejected = {
                let mut st = lock_pool();
                st.live -= 1;
                st.starting -= 1;
                match (policy, st.growth(soft_cap())) {
                    // The budget ran out; a running worker will get to the
                    // queue. (If the pool meanwhile lost its last running
                    // worker, `growth` now says `Required`: go round again.)
                    (StackPolicy::Budgeted, Some(StackPolicy::Required)) => None,
                    (StackPolicy::Budgeted, _) => return,
                    // Nothing can run the newest task: give it back to its
                    // waiter as an error.
                    (StackPolicy::Required, _) => st.queue.pop_back(),
                }
            };
            cvar.notify_all();
            match rejected {
                Some(task) => {
                    reject(task, &err);
                    return;
                }
                None => continue,
            }
        }
    }

    pub(super) fn reject(task: Task, err: &SpawnError) {
        match task.reject {
            Some(rejecter) => rejecter(err),
            None => eprintln!("mutsu: dropped a background task: {}", err.message()),
        }
    }

    fn worker_loop() {
        {
            let mut st = lock_pool();
            st.starting -= 1;
        }
        super::yield_points::register_worker();
        while let Some(task) = wait_for_task() {
            IN_TASK.with(|c| c.set(true));
            // A panicking task must not take the worker's `live` accounting
            // with it: catch, forget, move on — same process-level outcome as
            // a panicking dedicated thread (the panic is already turned into a
            // broken Promise by `guard_worker_panic` where that matters).
            let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(task.run));
            // Task end is a yield (ADR-0105 D2): deliver the wake-ups this
            // task deferred. Deferred starts need nothing: this worker is
            // about to dequeue them itself.
            super::yield_points::flush_own();
            IN_TASK.with(|c| c.set(false));
            // Task boundary (ADR-0020 §3.4): task N's pending DESTROY queue
            // and failure registry must not leak into task N+1 while the
            // thread stays GC-registered. (This also releases any ADR-0105 D3
            // rendezvous the task still owes.)
            crate::value::drop_thread_local_gc_state();
        }
        super::yield_points::unregister_worker();
    }

    /// Park until a task is available. Returns `None` when the worker should
    /// exit (idle past the grace period while above the keep-alive floor).
    fn wait_for_task() -> Option<Task> {
        // The park provably touches no `Gc` state (a queue pop moves a `Box`),
        // so the whole wait counts quiescent: an idle pool never starves a
        // stop-the-world (ADR-0020 §3.3).
        crate::gc::block_quiescent(|| {
            let cvar = &pool().1;
            let mut st = lock_pool();
            if let Some(task) = st.queue.pop_front() {
                return Some(task);
            }
            st.idle += 1;
            let deadline = Instant::now() + IDLE_GRACE;
            loop {
                if let Some(task) = st.queue.pop_front() {
                    st.idle -= 1;
                    return Some(task);
                }
                if st.live <= keep_alive_floor() {
                    // At/below the floor: park until woken, no deadline.
                    st = cvar.wait(st).unwrap_or_else(PoisonError::into_inner);
                    continue;
                }
                let now = Instant::now();
                if now >= deadline {
                    st.idle -= 1;
                    st.live -= 1;
                    return None;
                }
                let (g, _) = cvar
                    .wait_timeout(st, deadline - now)
                    .unwrap_or_else(PoisonError::into_inner);
                st = g;
            }
        })
    }

    /// See [`super::enter_blocking`].
    pub(crate) struct BlockingGuard {
        counted: bool,
    }

    impl Drop for BlockingGuard {
        fn drop(&mut self) {
            if !self.counted {
                return;
            }
            BLOCK_DEPTH.with(|d| d.set(d.get() - 1));
            if BLOCK_DEPTH.with(|d| d.get()) == 0 {
                lock_pool().blocked -= 1;
            }
        }
    }

    pub(super) fn enter_blocking() -> BlockingGuard {
        // A blocking point is a yield (ADR-0105 D2/D3), on any thread.
        super::yield_points::on_park();
        if !IN_TASK.with(|c| c.get()) {
            return BlockingGuard { counted: false };
        }
        let depth = BLOCK_DEPTH.with(|d| {
            let n = d.get() + 1;
            d.set(n);
            n
        });
        if depth == 1 {
            lock_pool().blocked += 1;
            grow_as_needed();
        }
        BlockingGuard { counted: true }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        fn state(
            queue: usize,
            idle: usize,
            live: usize,
            starting: usize,
            blocked: usize,
        ) -> PoolState {
            let mut q = VecDeque::new();
            for _ in 0..queue {
                q.push_back(Task {
                    run: Box::new(|| {}),
                    reject: None,
                });
            }
            PoolState {
                queue: q,
                idle,
                live,
                starting,
                blocked,
            }
        }

        #[test]
        fn no_unclaimed_work_means_no_growth() {
            // One queued task, one idle worker about to take it.
            assert_eq!(state(1, 1, 1, 0, 0).growth(8), None);
            // One queued task, one worker starting for it.
            assert_eq!(state(1, 0, 1, 1, 0).growth(8), None);
        }

        #[test]
        fn grows_within_the_soft_cap() {
            assert_eq!(state(1, 0, 2, 0, 0).growth(8), Some(StackPolicy::Budgeted));
        }

        #[test]
        fn queues_past_the_soft_cap_while_someone_runs() {
            assert_eq!(state(5, 0, 8, 0, 0).growth(8), None);
            // Blocked workers do not count toward the cap.
            assert_eq!(state(5, 0, 8, 0, 3).growth(8), Some(StackPolicy::Budgeted));
        }

        #[test]
        fn all_blocked_forces_growth() {
            assert_eq!(state(1, 0, 8, 0, 8).growth(8), Some(StackPolicy::Required));
            // Nobody at all (first submit).
            assert_eq!(state(1, 0, 0, 0, 0).growth(8), Some(StackPolicy::Required));
            // A starting worker is spoken for by another task.
            assert_eq!(state(2, 0, 3, 1, 2).growth(8), Some(StackPolicy::Required));
        }
    }
}

/// Return the maximum number of active workers exposed by `ThreadPoolScheduler`.
// Cost: O(1), cores = the process CPU count.
pub(crate) fn max_threads() -> usize {
    #[cfg(not(target_arch = "wasm32"))]
    {
        native::soft_cap()
    }
    #[cfg(target_arch = "wasm32")]
    {
        std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(1)
            * 8
    }
}

/// Mark the calling thread as blocked for as long as the returned guard lives,
/// if it is a pool worker running a task; otherwise a no-op. Entering the
/// outermost block may start a new worker for queued tasks -- this is the
/// ADR-0123 hook that keeps blocking `await` deadlock-free without growing
/// the pool on every CPU-bound burst. Call it before taking any lock the
/// blocking operation needs: growth may run a rejected task's rejecter.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn enter_blocking() -> native::BlockingGuard {
    native::enter_blocking()
}

/// wasm32: there is no pool to account to.
#[cfg(target_arch = "wasm32")]
pub(crate) struct BlockingGuard;

#[cfg(target_arch = "wasm32")]
pub(crate) fn enter_blocking() -> BlockingGuard {
    BlockingGuard
}

/// Run `task` on a pooled worker thread. The task may run arbitrary user VM
/// code (workers reserve the deep-recursion stack) and may block indefinitely
/// (`await`, channel receive) — the pool grows instead of deadlocking. There
/// is no join handle: completion is observed through whatever the task itself
/// resolves (a promise, a channel, a counter).
///
/// If no worker can be started for it at all, the task is dropped with a
/// diagnostic on stderr; a task whose waiter must hear about that uses
/// [`submit_or_reject`].
pub(crate) fn submit(task: impl FnOnce() + Send + 'static) {
    submit_task(Box::new(task), None);
}

/// [`submit`], calling `reject` (on the submitting thread, or on the worker
/// whose blocking made the pool try to grow) with the reason when no worker
/// can ever run `task` -- so the waiter gets a catchable error instead of a
/// hang or a panic (ADR-0123).
pub(crate) fn submit_or_reject(
    task: impl FnOnce() + Send + 'static,
    reject: impl FnOnce(&SpawnError) + Send + 'static,
) {
    submit_task(Box::new(task), Some(Box::new(reject)));
}

#[cfg(not(target_arch = "wasm32"))]
fn submit_task(run: Box<dyn FnOnce() + Send + 'static>, reject: Option<Rejecter>) {
    use crate::runtime::builtins_system::{StackPolicy, try_spawn_user_thread};
    if !native::pool_enabled() {
        // Thread-per-task: no queue to wait in, so a refused thread is
        // reported straight away.
        if let Err(e) = try_spawn_user_thread("pool", StackPolicy::Required, run) {
            match reject {
                Some(reject) => reject(&e),
                None => eprintln!("mutsu: dropped a background task: {}", e.message()),
            }
        }
        return;
    }
    crate::vm::vm_stats::record_pool_task();
    // ADR-0105 D4: a task submitted by a running pool worker while no worker
    // is idle must not overtake its submitter. It waits for the submitter to
    // yield — park (which grows the pool), task end (the submitter dequeues it
    // itself) — or for the pool tick, instead of getting a fresh worker now.
    let from_worker = native::IN_TASK.with(|c| c.get());
    let defer = {
        let mut st = native::lock_pool();
        let defer = from_worker && st.idle == 0;
        st.queue.push_back(native::Task { run, reject });
        defer
    };
    native::pool().1.notify_one();
    if defer && yield_points::arm_tick() {
        return;
    }
    native::grow_as_needed();
}

/// Defer `f` to the calling pool worker's next yield — its next blocking
/// point, its task end, or the pool tick (ADR-0105 D2). Hands `f` back when
/// the caller is not a pool worker running a task; the caller runs it now.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn defer_until_yield(
    f: Box<dyn FnOnce() + Send + 'static>,
) -> Result<(), Box<dyn FnOnce() + Send + 'static>> {
    yield_points::defer_until_yield(f)
}

/// wasm32: the cooperative pump is already sequential; nothing to defer to.
#[cfg(target_arch = "wasm32")]
pub(crate) fn defer_until_yield(
    f: Box<dyn FnOnce() + Send + 'static>,
) -> Result<(), Box<dyn FnOnce() + Send + 'static>> {
    Err(f)
}

/// wasm32: the cooperative scheduler is already a pool of one — queue the task.
#[cfg(target_arch = "wasm32")]
fn submit_task(run: Box<dyn FnOnce() + Send + 'static>, _reject: Option<Rejecter>) {
    crate::runtime::builtins_system::spawn_user_thread("pool", run);
}

/// Handle on a pooled task whose completion (and result) the spawner waits
/// for. Natively this is a channel the task sends its result on — a worker
/// that panics drops the sender during unwind, so `join` reports the panic as
/// `Err` exactly like a dedicated thread's `join` would. On wasm32 it wraps
/// the cooperative scheduler's `JoinHandle`, whose `join` *runs* the queued
/// task — a channel wait would spin forever there, which is why the cfg fork
/// lives here and not at the call sites.
pub(crate) struct TaskHandle<T> {
    #[cfg(not(target_arch = "wasm32"))]
    rx: std::sync::mpsc::Receiver<T>,
    #[cfg(target_arch = "wasm32")]
    inner: crate::runtime::thread_compat::JoinHandle<T>,
}

impl<T> TaskHandle<T> {
    /// Wait for the task to finish and take its result. Callers wrap this in
    /// `gc::block_quiescent` like any thread join — the wait itself touches no
    /// `Gc` state.
    pub(crate) fn join(self) -> std::thread::Result<T> {
        #[cfg(not(target_arch = "wasm32"))]
        {
            self.rx
                .recv()
                .map_err(|e| Box::new(e) as Box<dyn std::any::Any + Send>)
        }
        #[cfg(target_arch = "wasm32")]
        {
            self.inner.join()
        }
    }
}

/// Run `task` on a pooled worker and return a handle its spawner can `join`.
/// For the joined fan-out sites (hyper/race batches, throttle workers). The
/// tasks run concurrently up to the pool's growth rule (ADR-0123); a spawner
/// that blocks joining them frees its own slot, and if every worker ends up
/// blocked the pool grows regardless, so a join never waits on a task that
/// nobody will run. A task the pool has to reject drops its sender, so its
/// `join` reports `Err` like a panicked thread's would.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn submit_joinable<T: Send + 'static>(
    task: impl FnOnce() -> T + Send + 'static,
) -> TaskHandle<T> {
    let (tx, rx) = std::sync::mpsc::channel();
    submit_or_reject(
        move || {
            let _ = tx.send(task());
        },
        |_| {},
    );
    TaskHandle { rx }
}

#[cfg(target_arch = "wasm32")]
pub(crate) fn submit_joinable<T: Send + 'static>(
    task: impl FnOnce() -> T + Send + 'static,
) -> TaskHandle<T> {
    TaskHandle {
        inner: crate::runtime::builtins_system::spawn_user_thread("pool", task),
    }
}
