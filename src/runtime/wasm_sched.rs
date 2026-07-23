//! Cooperative single-threaded scheduler for the `wasm32` build.
//!
//! Browsers give a wasm module one thread and no way to create another:
//! `std::thread::spawn` traps (`RuntimeError: unreachable`), which is why
//! `start { ... }`, `await`, `Thread.start`, `Promise.in` and `Supply.interval`
//! all died on the playground while `Channel`, `Supply`, `react` and `Lock` —
//! none of which need a second thread — already worked.
//!
//! Real threads are also out of reach for the deployed site: shared-memory wasm
//! needs `SharedArrayBuffer`, which needs the COOP/COEP headers that GitHub
//! Pages cannot serve. So instead of *parallel* execution this module provides
//! *concurrent* execution: every would-be thread becomes a task on a run queue,
//! and every point where the interpreter would block on another thread pumps
//! that queue instead.
//!
//! ```text
//! start { ... }        -> enqueue a task, return a Planned promise
//! await $p             -> pump() until $p is resolved
//! $chan.receive        -> pump() until the queue has a value
//! sleep 1              -> pump(), then advance the virtual clock
//! ```
//!
//! [`pump`] is the whole event loop: run a ready task if there is one, and
//! otherwise jump the virtual clock to the earliest timer deadline and fire it.
//! When neither is possible nothing can ever make the waiter ready — that is a
//! real deadlock, and `pump` returns `false` so the caller can raise an error
//! instead of hanging the browser tab.
//!
//! The one thing this cannot emulate is a task that blocks halfway through on
//! something only its *waiter* would do later: tasks run to completion, so
//! `my $p = start { $c.receive }; $c.send(1); await $p` works (the task is not
//! started until the `await`), but a task that waits on a value sent after it
//! has already begun deadlocks. Native mutsu runs both.

use std::collections::VecDeque;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex, OnceLock};

type Task = Box<dyn FnOnce() + Send>;

/// Ready-to-run tasks, oldest first. Never held across running a task: a task
/// may enqueue more tasks, and may itself pump the queue from a nested
/// `await`.
fn queue() -> &'static Mutex<VecDeque<(u64, Task)>> {
    static QUEUE: OnceLock<Mutex<VecDeque<(u64, Task)>>> = OnceLock::new();
    QUEUE.get_or_init(|| Mutex::new(VecDeque::new()))
}

fn next_id() -> u64 {
    static NEXT: AtomicU64 = AtomicU64::new(1);
    NEXT.fetch_add(1, Ordering::Relaxed)
}

/// Seconds the virtual clock has been advanced past real time, as `f64` bits.
///
/// Nothing in a browser can block the single thread for a second without
/// freezing the page, so `sleep`/timer waits do not spend real time — they jump
/// this offset forward instead. Both the wall clock (`now`, `time`,
/// `DateTime.now`) and the monotonic clock the timer heap runs on include it,
/// so `my $t = now; sleep 2; say now - $t` still says `2`.
static CLOCK_OFFSET_SECS: AtomicU64 = AtomicU64::new(0);

pub(crate) fn clock_offset_secs() -> f64 {
    f64::from_bits(CLOCK_OFFSET_SECS.load(Ordering::Relaxed))
}

/// Move the virtual clock forward by `secs` (never backwards).
pub(crate) fn advance_clock(secs: f64) {
    if !secs.is_finite() || secs <= 0.0 {
        return;
    }
    let mut cur = CLOCK_OFFSET_SECS.load(Ordering::Relaxed);
    loop {
        let next = (f64::from_bits(cur) + secs).to_bits();
        match CLOCK_OFFSET_SECS.compare_exchange_weak(
            cur,
            next,
            Ordering::Relaxed,
            Ordering::Relaxed,
        ) {
            Ok(_) => return,
            Err(observed) => cur = observed,
        }
    }
}

/// Advance the virtual clock so that [`crate::runtime::thread_compat::mono_now`]
/// reaches `deadline` (no-op when it is already there).
pub(crate) fn advance_clock_to(deadline: f64) {
    advance_clock(deadline - crate::runtime::thread_compat::mono_now());
}

/// Queue `f` to run later on the single thread, and hand back the slot its
/// result lands in. The task does not start here: deferring it is what lets
/// `my $p = start { $c.receive }; $c.send(1); await $p` work, since by the time
/// the task runs the main line has already sent.
pub(crate) fn spawn<F, T>(f: F) -> PendingTask<T>
where
    F: FnOnce() -> T + Send + 'static,
    T: Send + 'static,
{
    let id = next_id();
    let slot: Arc<Mutex<Option<std::thread::Result<T>>>> = Arc::new(Mutex::new(None));
    let write_to = Arc::clone(&slot);
    let task: Task = Box::new(move || {
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(f));
        *write_to.lock().unwrap() = Some(result);
    });
    queue().lock().unwrap().push_back((id, task));
    PendingTask { id, slot }
}

/// The wasm stand-in for a `JoinHandle`'s thread: an id in the run queue plus
/// the slot the task's return value lands in.
pub(crate) struct PendingTask<T> {
    id: u64,
    slot: Arc<Mutex<Option<std::thread::Result<T>>>>,
}

impl<T> PendingTask<T> {
    fn is_finished(&self) -> bool {
        self.slot.lock().unwrap().is_some()
    }

    /// Run this task to completion (if it has not run already) and take its
    /// result. Fails only when the task cannot run at all — it is already on
    /// the stack (a task joining itself).
    pub(crate) fn join(self) -> std::thread::Result<T> {
        if !self.is_finished() {
            run_task(self.id);
        }
        self.slot.lock().unwrap().take().unwrap_or_else(|| {
            Err(Box::new(
                "join on a task that is already running (single-threaded WASM build)".to_string(),
            ) as Box<dyn std::any::Any + Send>)
        })
    }
}

/// Pull one specific queued task out and run it. Returns false when it is not
/// in the queue (already run, or currently running further up the stack).
fn run_task(id: u64) -> bool {
    let task = {
        let mut q = queue().lock().unwrap();
        q.iter().position(|(qid, _)| *qid == id).map(|at| {
            let (_, task) = q.remove(at).expect("position() just found it");
            task
        })
    };
    match task {
        Some(task) => {
            task();
            true
        }
        None => false,
    }
}

/// Run the oldest ready task. Returns false when the queue is empty.
fn run_one() -> bool {
    let task = queue().lock().unwrap().pop_front();
    match task {
        Some((_, task)) => {
            task();
            true
        }
        None => false,
    }
}

pub(crate) fn has_ready_tasks() -> bool {
    !queue().lock().unwrap().is_empty()
}

/// Make one step of progress: run a ready task, or fire the earliest timer
/// (jumping the virtual clock to its deadline).
///
/// Returns `false` when there is nothing left to run — the caller is blocked on
/// something that can never happen, so it should raise an error rather than
/// spin.
pub(crate) fn pump() -> bool {
    if run_one() {
        return true;
    }
    crate::runtime::native_methods::interval_timer::wasm_fire_next_timer()
}

/// Run everything that is ready, so `start { say "hi" }` still prints when the
/// program never awaits it. Bounded: a self-rescheduling timer (an untapped
/// `Supply.interval`) would otherwise spin forever.
pub(crate) fn drain_ready() {
    const MAX_STEPS: usize = 100_000;
    for _ in 0..MAX_STEPS {
        if !run_one() {
            return;
        }
    }
}
