//! TAP / `Test` module state, extracted out of the `Interpreter` god-struct.
//!
//! What is left of it. mutsu used to answer `plan`/`ok`/`is`/`subtest` with its
//! own native TAP provider, and this type held that provider's bookkeeping: the
//! per-plan counter, the subtest nesting stack, the pending-`todo` ranges and
//! the bail-out flag. `use Test` loads rakudo's own `Test.rakumod` now and the
//! native provider is gone (#7566), so the module keeps all of that in its own
//! Raku-level state and none of it is mutsu's to track.
//!
//! Two things still are:
//!
//! * [`TapState::active`] — "a test file is running", the gate the bare-word
//!   resolver and the thread-clone path consult;
//! * the shared cross-thread counter in [`TestState`] — a test assertion run on
//!   a spawned thread (`start` block, `Promise` callback) has to number itself
//!   against the same counter as the main thread.

use std::sync::Arc;
use std::sync::atomic::AtomicUsize;

/// Per-plan TAP counter and plan tracking. One instance per `plan` scope.
#[derive(Debug, Default)]
pub(crate) struct TestState {
    pub(crate) ran: usize,
    /// Shared atomic counter for test numbering across threads.
    /// When set, `ran` is derived from this counter instead of the local field.
    pub(crate) shared_ran: Option<Arc<AtomicUsize>>,
}

impl TestState {
    pub(crate) fn new() -> Self {
        Self {
            ran: 0,
            shared_ran: None,
        }
    }

    /// Get an Arc to the shared counter, creating one if needed.
    pub(crate) fn ensure_shared_ran(&mut self) -> Arc<AtomicUsize> {
        if let Some(ref counter) = self.shared_ran {
            counter.clone()
        } else {
            let counter = Arc::new(AtomicUsize::new(self.ran));
            self.shared_ran = Some(counter.clone());
            counter
        }
    }
}

/// All TAP/`Test` module runtime state, owned as a single `Interpreter` field.
#[derive(Debug, Default)]
pub(crate) struct TapState {
    /// Per-plan TAP counter. `None` until a `Test` module is loaded.
    state: Option<TestState>,
}

impl TapState {
    /// True once any test state exists (i.e. a `Test` module has been loaded).
    pub(crate) fn active(&self) -> bool {
        self.state.is_some()
    }

    /// Mutable access to the current `TestState`, creating an empty one on first use.
    pub(crate) fn ensure_state(&mut self) -> &mut TestState {
        self.state.get_or_insert_with(TestState::new)
    }

    /// Build the `TapState` for a freshly spawned thread: the child shares the
    /// parent's TAP counter (so test numbering stays consistent across threads,
    /// e.g. `pass`/`flunk` inside `Promise.start`).
    pub(crate) fn clone_for_thread(&mut self) -> TapState {
        let state = self.state.as_mut().map(|parent| {
            let shared = parent.ensure_shared_ran();
            TestState {
                ran: parent.ran,
                shared_ran: Some(shared),
            }
        });
        TapState { state }
    }
}
