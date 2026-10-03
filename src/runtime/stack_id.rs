//! `$*STACK-ID`: an Int naming the call stack the code runs on (#11269).
//!
//! Rakudo (since 2022.06) answers `0` in the mainline and a fresh number for
//! every other stack: each `start` block, `Promise.then` callback or thread
//! gets its own, even when a pool worker runs several of them one after the
//! other. The numbering itself is an implementation detail; code uses the
//! value only as a per-stack identifier (Log::Dispatch's `:thread-id`).
//!
//! The initial thread is stack `0`. A pooled task runs inside
//! `run_on_fresh_stack`, which gives it a new id for its duration; any other
//! thread draws a new id on its first read and keeps it.
use std::cell::Cell;
use std::sync::atomic::{AtomicU64, Ordering};

static NEXT_STACK_ID: AtomicU64 = AtomicU64::new(1);

thread_local! {
    static STACK_ID: Cell<Option<u64>> = const { Cell::new(None) };
}

// Cost: O(1).
fn fresh_stack_id() -> u64 {
    NEXT_STACK_ID.fetch_add(1, Ordering::Relaxed)
}

/// The current stack's id: `0` on the initial thread, else the id of the
/// running pooled task or of this thread.
// Cost: O(1).
pub(crate) fn current_stack_id() -> u64 {
    STACK_ID.with(|cell| match cell.get() {
        Some(id) => id,
        None => {
            let id = if crate::runtime::methods_collection_ops::is_initial_thread() {
                0
            } else {
                fresh_stack_id()
            };
            cell.set(Some(id));
            id
        }
    })
}

/// Run `task` as a stack of its own: `$*STACK-ID` reads a fresh id inside it,
/// and the caller's id is back afterwards (a wasm32 task runs on the caller's
/// thread).
// Cost: O(1) plus the task.
pub(crate) fn run_on_fresh_stack<T>(task: impl FnOnce() -> T) -> T {
    struct Restore(Option<u64>);
    impl Drop for Restore {
        fn drop(&mut self) {
            STACK_ID.with(|cell| cell.set(self.0));
        }
    }
    let _restore = Restore(STACK_ID.with(|cell| cell.replace(Some(fresh_stack_id()))));
    task()
}
