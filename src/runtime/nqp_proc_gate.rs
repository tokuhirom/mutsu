//! The permit gate of one output stream of an `nqp::spawnprocasync` child
//! (`nqp_ops_proc`, ADR-11502 §2.4.3): `nqp::permit` sets how many chunks
//! the stream's reader may read, and the child's waiter settles on it before
//! it pushes `done`.

use std::sync::{Condvar, Mutex, PoisonError};

/// The reader side of one output stream.
#[derive(Default)]
struct StreamState {
    /// How many more chunks the stream may read: -1 for any number.
    permits: i64,
    /// The reader waits for a permit.
    parked: bool,
    /// The reader reached the end of the stream.
    finished: bool,
}

/// A stream's [`StreamState`] and the condition its changes signal.
#[derive(Default)]
pub(crate) struct StreamGate {
    state: Mutex<StreamState>,
    changed: Condvar,
}

impl StreamGate {
    /// Wait until a chunk may be read, and take one permit.
    // Cost: O(1), plus the wait.
    pub(crate) fn take(&self) {
        let mut state = self.state.lock().unwrap_or_else(PoisonError::into_inner);
        while state.permits == 0 {
            state.parked = true;
            self.changed.notify_all();
            state = crate::gc::block_quiescent(|| {
                self.changed
                    .wait(state)
                    .unwrap_or_else(PoisonError::into_inner)
            });
        }
        state.parked = false;
        if state.permits > 0 {
            state.permits -= 1;
        }
    }

    // Cost: O(1).
    pub(crate) fn set_permits(&self, n: i64) {
        self.state
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .permits = n;
        self.changed.notify_all();
    }

    // Cost: O(1).
    pub(crate) fn finish(&self) {
        self.state
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .finished = true;
        self.changed.notify_all();
    }

    /// Whether the reader reached the end of the stream.
    // Cost: O(1).
    pub(crate) fn is_finished(&self) -> bool {
        self.state
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .finished
    }

    /// Wait until the reader has nothing more to push for now: it reached
    /// the end of the stream, or it waits for a permit.
    // Cost: O(1), plus the wait.
    pub(crate) fn settle(&self) {
        let mut state = self.state.lock().unwrap_or_else(PoisonError::into_inner);
        while !state.finished && !state.parked {
            state = crate::gc::block_quiescent(|| {
                self.changed
                    .wait(state)
                    .unwrap_or_else(PoisonError::into_inner)
            });
        }
    }
}
