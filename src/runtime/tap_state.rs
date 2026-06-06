//! TAP / `Test` module state, extracted out of the `Interpreter` god-struct.
//!
//! All the runtime bookkeeping for the `Test` module — the per-plan TAP counter
//! (`TestState`), the subtest nesting stack, and the bail-out flag — used to live
//! as four loose fields on `Interpreter`, poked directly from ~50 call sites.
//! `TapState` groups them behind a small API so the test-function bodies talk to
//! one cohesive object instead of reaching into the interpreter's namespace.
//!
//! This is the carrier for the eventual ownership move out of `Interpreter`
//! (lever B): once every access goes through `TapState`'s methods, changing how
//! the state is owned/shared is a localized change to this type and the `tap`
//! field, not a sweep over every call site. See PLAN.md "Test 関数の TAP 状態所有".

use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

/// Per-plan TAP counter and plan tracking. One instance per `plan`/subtest scope.
#[derive(Debug, Default)]
pub(crate) struct TestState {
    pub(crate) planned: Option<usize>,
    pub(crate) ran: usize,
    /// Shared atomic counter for test numbering across threads.
    /// When set, `ran` is derived from this counter instead of the local field.
    pub(crate) shared_ran: Option<Arc<AtomicUsize>>,
    pub(crate) failed: usize,
    pub(crate) force_todo: Vec<TodoRange>,
}

#[derive(Debug, Clone)]
pub(crate) struct TodoRange {
    pub(crate) start: usize,
    pub(crate) end: usize,
    pub(crate) reason: String,
}

impl TestState {
    pub(crate) fn new() -> Self {
        Self {
            planned: None,
            ran: 0,
            shared_ran: None,
            failed: 0,
            force_todo: Vec::new(),
        }
    }

    /// Increment and return the next test number.
    /// Uses the shared atomic counter if available, otherwise the local field.
    pub(crate) fn next_ran(&mut self) -> usize {
        if let Some(ref counter) = self.shared_ran {
            let n = counter.fetch_add(1, Ordering::SeqCst) + 1;
            self.ran = n;
            n
        } else {
            self.ran += 1;
            self.ran
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
    /// Per-plan TAP counter/plan. `None` until the first test call (`plan`/`ok`/…).
    state: Option<TestState>,
    /// Nesting depth of active subtests (0 = top level).
    subtest_depth: usize,
    /// Stack tracking whether each nested subtest callable is a Sub (true) or
    /// Block (false). Used by `plan skip-all` to reject Block callables.
    subtest_callable_is_sub: Vec<bool>,
    /// Set by `bail-out`; suppresses the trailing plan/summary footer.
    bailed_out: bool,
}

impl TapState {
    /// True once any test state exists (i.e. a `plan`/test call has happened).
    pub(crate) fn active(&self) -> bool {
        self.state.is_some()
    }

    /// Shared read access to the current `TestState`, if any.
    pub(crate) fn state(&self) -> Option<&TestState> {
        self.state.as_ref()
    }

    /// Mutable access to the current `TestState`, if any.
    pub(crate) fn state_mut(&mut self) -> Option<&mut TestState> {
        self.state.as_mut()
    }

    /// Mutable access to the current `TestState`, creating an empty one on first use.
    pub(crate) fn ensure_state(&mut self) -> &mut TestState {
        self.state.get_or_insert_with(TestState::new)
    }

    /// Remove and return the current `TestState` (used when entering a subtest).
    pub(crate) fn take_state(&mut self) -> Option<TestState> {
        self.state.take()
    }

    /// Replace the current `TestState` (used to restore a parent after a subtest).
    pub(crate) fn set_state(&mut self, state: Option<TestState>) {
        self.state = state;
    }

    /// Current subtest nesting depth (0 = top level).
    pub(crate) fn subtest_depth(&self) -> usize {
        self.subtest_depth
    }

    /// Whether the innermost active subtest callable is a Sub (`true`) or Block
    /// (`false`). `None` when not inside a subtest.
    pub(crate) fn subtest_callable_is_sub_last(&self) -> Option<bool> {
        self.subtest_callable_is_sub.last().copied()
    }

    /// Override the kind (Sub/Block) recorded for the innermost subtest callable.
    pub(crate) fn set_subtest_callable_is_sub_last(&mut self, is_sub: bool) {
        if let Some(last) = self.subtest_callable_is_sub.last_mut() {
            *last = is_sub;
        }
    }

    /// Enter a subtest: stash the parent `TestState`, install a fresh one, and
    /// push onto the subtest stack (defaulting the callable kind to Sub).
    /// Returns the parent state to be restored by [`set_state`] on exit.
    pub(crate) fn begin_subtest(&mut self) -> Option<TestState> {
        let parent = self.state.take();
        self.state = Some(TestState::new());
        self.subtest_depth += 1;
        // Default to true (Sub); callers override via set_subtest_callable_is_sub_last.
        self.subtest_callable_is_sub.push(true);
        parent
    }

    /// Pop the subtest stack on exit (mirrors [`begin_subtest`]). The caller is
    /// responsible for restoring the parent `TestState` via [`set_state`].
    pub(crate) fn end_subtest(&mut self) {
        self.subtest_depth = self.subtest_depth.saturating_sub(1);
        self.subtest_callable_is_sub.pop();
    }

    /// Whether `bail-out` has been called.
    pub(crate) fn bailed_out(&self) -> bool {
        self.bailed_out
    }

    /// Mark the test run as bailed out.
    pub(crate) fn set_bailed_out(&mut self) {
        self.bailed_out = true;
    }

    /// Build the `TapState` for a freshly spawned thread: the child shares the
    /// parent's TAP counter (so test numbering stays consistent across threads,
    /// e.g. `pass`/`flunk` inside `Promise.start`) but starts with its own plan,
    /// failure count, and empty subtest stack.
    pub(crate) fn clone_for_thread(&mut self) -> TapState {
        let state = self.state.as_mut().map(|parent| {
            let shared = parent.ensure_shared_ran();
            TestState {
                planned: None,
                ran: parent.ran,
                shared_ran: Some(shared),
                failed: 0,
                force_todo: parent.force_todo.clone(),
            }
        });
        TapState {
            state,
            subtest_depth: 0,
            subtest_callable_is_sub: Vec::new(),
            bailed_out: false,
        }
    }
}
