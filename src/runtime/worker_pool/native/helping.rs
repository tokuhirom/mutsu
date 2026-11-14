//! Helping while blocked (ADR-0123 §7): when the OS refuses the thread a
//! `Required` growth asks for, a pool worker blocked in a [`crate::gc::wait_until`]
//! runs the starved task nested on its own stack, instead of the task being
//! rejected.
//!
//! mutsu's `await` holds its thread, so N parked `start` blocks need N
//! threads. Past the process's thread limit (`ulimit -u`, which the ecosystem
//! sandbox sets to 400) the pool used to reject the newest task: its promise
//! broke, and a program that waits for something that task does first (Log::Dispatch's
//! `$ready.keep; await $starter`, #17392) hung. Rakudo runs the same program
//! on a handful of threads, because its `await` gives the worker back.
//!
//! The wasm build already runs every wait this way (`wait_until` pumps the
//! cooperative scheduler), so the task closures are known to nest. Natively it
//! is only the last resort, because a nested task delays its helper: the
//! helper resumes only once the nested task finishes, so a nested task that
//! waits for something only its helper's own continuation produces cannot
//! complete. Without helping that task would not have run at all, so the
//! trade is a possible stall in place of a certain failure, and only after the
//! OS has refused a thread.
//!
//! Only pool workers help. A non-pool thread (the main thread awaiting the
//! workers) never runs a task nested: it is typically the one thread that
//! will release what the tasks wait for.

use super::*;
use std::sync::atomic::{AtomicBool, Ordering};

/// Set when a `Required` growth was refused while a helper was waiting, and
/// cleared once no unclaimed task is left for a helper. Read lock-free on
/// every helper poll, so a pool that is not starved pays one load.
static STARVED: AtomicBool = AtomicBool::new(false);

/// Record that the queue holds a task no new thread can be started for.
// Cost: O(1).
pub(super) fn mark_starved() {
    STARVED.store(true, Ordering::Release);
}

/// Whether a starved task may be waiting for a helper, and the calling thread
/// has the stack to run one.
// Cost: O(1), one atomic load and a stack-pointer probe.
pub(crate) fn help_wanted() -> bool {
    STARVED.load(Ordering::Acquire) && has_room_to_help()
}

/// Whether the calling thread has stack enough to run a task nested: at least
/// half of what a freshly started worker gets (`fresh_stack_usable_bytes`).
/// Every nested task deepens its helper's stack, and under an address-space
/// limit there may be no segment to grow onto, so a helper that is already
/// deep leaves the task to a shallower one rather than starting it where it
/// would die with "Too deep recursion". An unguarded thread cannot tell, so
/// it does not help.
// Cost: O(1).
fn has_room_to_help() -> bool {
    crate::native_stack::headroom_bytes()
        .is_some_and(|room| room >= crate::native_stack::fresh_stack_usable_bytes() / 2)
}

/// A pool worker's registration as a helper; see [`register_helper`].
pub(crate) struct HelperGuard {
    counted: bool,
}

impl HelperGuard {
    /// Whether this thread may run a starved task while it waits.
    pub(crate) fn can_help(&self) -> bool {
        self.counted
    }
}

impl Drop for HelperGuard {
    fn drop(&mut self) {
        if self.counted {
            lock_pool().helpers -= 1;
        }
    }
}

/// Count the calling thread as a helper for as long as the guard lives, if it
/// is a pool worker running a task; otherwise a guard that cannot help.
// Cost: O(1) (the pool lock).
pub(crate) fn register_helper() -> HelperGuard {
    if !pool_enabled() || !IN_TASK.with(Cell::get) {
        return HelperGuard { counted: false };
    }
    lock_pool().helpers += 1;
    HelperGuard { counted: true }
}

/// Run one starved task nested on the calling helper's stack, if one is still
/// queued and no worker is running to take it. Called by a helper with no
/// lock held and out of the GC's quiescent state. Returns whether a task ran.
///
/// While the task runs, the helper counts as running, not blocked, and its
/// blocking depth starts from zero, so the nested task's own waits are
/// accounted (and may help in turn) like any task's. The task boundary's
/// `drop_thread_local_gc_state` is NOT run: it would drop the state of the
/// task the helper is still inside.
// Cost: O(1) bookkeeping, plus the nested task.
pub(crate) fn help_one() -> bool {
    let task = {
        let mut st = lock_pool();
        if st.unclaimed() == 0 || st.running() > 0 {
            // Nothing left for a helper, or a running worker will come back
            // for the queue: the next refused growth re-arms the flag.
            STARVED.store(false, Ordering::Release);
            return false;
        }
        let task = st.queue.pop_front();
        if st.unclaimed() == 0 {
            STARVED.store(false, Ordering::Release);
        }
        if task.is_some() {
            st.blocked -= 1;
        }
        task
    };
    let Some(task) = task else {
        return false;
    };
    let depth = BLOCK_DEPTH.with(|d| d.replace(0));
    // As in `worker_loop`: a panicking task must not take the accounting
    // with it.
    let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(task.run));
    super::super::yield_points::flush_own();
    BLOCK_DEPTH.with(|d| d.set(depth));
    lock_pool().blocked += 1;
    true
}
