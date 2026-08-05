//! Process-wide elastic worker pool for short-lived user tasks (ADR-0020).
//!
//! `submit` runs a task on a warm pooled worker when one is idle, and grows the
//! pool otherwise. Growth is the ADR-0020 §3.2 starvation check that keeps
//! blocking `await` deadlock-free: a busy worker may be *blocked* on a wait
//! whose resolution needs another worker, so a queued task never waits for a
//! busy worker to come back — if no worker is idle, a new one is spawned.
//! Workers keep the 256 MiB user-code stack reservation
//! (`USER_THREAD_STACK_SIZE`); a soft floor of `min(cores, 8)` workers stays
//! alive, and workers beyond the floor exit after an idle grace period.
//!
//! On wasm32 there is no pool: `submit` delegates to `spawn_user_thread`,
//! whose cooperative scheduler is already a pool of one.

#[cfg(not(target_arch = "wasm32"))]
mod native {
    use std::collections::VecDeque;
    use std::sync::{Condvar, Mutex, OnceLock};
    use std::time::{Duration, Instant};

    pub(super) type Task = Box<dyn FnOnce() + Send + 'static>;

    pub(super) struct PoolState {
        pub(super) queue: VecDeque<Task>,
        /// Workers parked in `wait_for_task` (not running or blocked in a task).
        pub(super) idle: usize,
        /// Total live workers (idle + running a task, possibly blocked).
        pub(super) live: usize,
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
                }),
                Condvar::new(),
            )
        })
    }

    /// Soft floor of kept-alive workers. 256 MiB *reserved* stack each makes
    /// this an address-space budget (ADR-0020 §3.2): `min(cores, 8)`.
    fn keep_alive_floor() -> usize {
        static FLOOR: OnceLock<usize> = OnceLock::new();
        *FLOOR.get_or_init(|| {
            std::thread::available_parallelism()
                .map(|n| n.get())
                .unwrap_or(4)
                .min(8)
        })
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

    pub(super) fn spawn_pool_worker() {
        // `spawn_user_thread` carries the whole worker-lifetime GC protocol:
        // `enter_mutator_worker`/`preregister_worker_quiescent` on the parent,
        // `worker_started` + `WorkerGuard` (drain -> unregister -> exit) in the
        // worker. The pool adds only the per-task boundary inside `worker_loop`.
        crate::runtime::builtins_system::spawn_user_thread(worker_loop);
    }

    fn worker_loop() {
        while let Some(task) = wait_for_task() {
            // A panicking task must not take the worker's `live` accounting
            // with it: catch, forget, move on — same process-level outcome as
            // a panicking dedicated thread (the panic is already turned into a
            // broken Promise by `guard_worker_panic` where that matters).
            let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(task));
            // Task boundary (ADR-0020 §3.4): task N's pending DESTROY queue
            // and failure registry must not leak into task N+1 while the
            // thread stays GC-registered.
            crate::value::drop_thread_local_gc_state();
        }
    }

    /// Park until a task is available. Returns `None` when the worker should
    /// exit (idle past the grace period while above the keep-alive floor).
    fn wait_for_task() -> Option<Task> {
        // The park provably touches no `Gc` state (a queue pop moves a `Box`),
        // so the whole wait counts quiescent: an idle pool never starves a
        // stop-the-world (ADR-0020 §3.3).
        crate::gc::block_quiescent(|| {
            let (lock, cvar) = pool();
            let mut st = lock.lock().unwrap();
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
                    st = cvar.wait(st).unwrap();
                    continue;
                }
                let now = Instant::now();
                if now >= deadline {
                    st.idle -= 1;
                    st.live -= 1;
                    return None;
                }
                let (g, _) = cvar.wait_timeout(st, deadline - now).unwrap();
                st = g;
            }
        })
    }
}

/// Run `task` on a pooled worker thread. The task may run arbitrary user VM
/// code (workers reserve the deep-recursion stack) and may block indefinitely
/// (`await`, channel receive) — the pool grows instead of deadlocking. There
/// is no join handle: completion is observed through whatever the task itself
/// resolves (a promise, a channel, a counter).
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn submit(task: impl FnOnce() + Send + 'static) {
    if !native::pool_enabled() {
        crate::runtime::builtins_system::spawn_user_thread(task);
        return;
    }
    crate::vm::vm_stats::record_pool_task();
    let (lock, cvar) = native::pool();
    let mut st = lock.lock().unwrap();
    st.queue.push_back(Box::new(task));
    // Starvation check (ADR-0020 §3.2): grow whenever there are more queued
    // tasks than parked workers. Busy workers may be blocked on an `await`
    // this very task resolves, so waiting for one is not an option.
    let grow = st.queue.len() > st.idle;
    if grow {
        st.live += 1;
        crate::vm::vm_stats::record_pool_spawn();
    }
    cvar.notify_one();
    drop(st);
    if grow {
        native::spawn_pool_worker();
    }
}

/// wasm32: the cooperative scheduler is already a pool of one — queue the task.
#[cfg(target_arch = "wasm32")]
pub(crate) fn submit(task: impl FnOnce() + Send + 'static) {
    crate::runtime::builtins_system::spawn_user_thread(task);
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
/// For the joined fan-out sites (hyper/race batches, throttle workers) that
/// need every task running *concurrently*: the submit-side starvation check
/// spawns a fresh worker whenever none is idle, so N submitted tasks get N
/// workers just like thread-per-task did.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn submit_joinable<T: Send + 'static>(
    task: impl FnOnce() -> T + Send + 'static,
) -> TaskHandle<T> {
    let (tx, rx) = std::sync::mpsc::channel();
    submit(move || {
        let _ = tx.send(task());
    });
    TaskHandle { rx }
}

#[cfg(target_arch = "wasm32")]
pub(crate) fn submit_joinable<T: Send + 'static>(
    task: impl FnOnce() -> T + Send + 'static,
) -> TaskHandle<T> {
    TaskHandle {
        inner: crate::runtime::builtins_system::spawn_user_thread(task),
    }
}
