//! RAII guards that restore interpreter call-dispatch state on drop --
//! including when a Rust panic unwinds through the guarded call, not just on
//! normal return.
//!
//! Several call-dispatch functions (`call_compiled_closure`,
//! `call_compiled_closure_with_topic`, `call_compiled_function_named_inner`)
//! temporarily mutate a piece of `Interpreter` state for the duration of a
//! call and restore it with a plain `self.field = saved;` statement near the
//! function's end. A Rust panic caught at an outer `catch_unwind` boundary
//! (`run_inner_guarded`/`run_range_guarded`) skips straight past such a
//! statement -- only `Drop` runs on unwind -- leaking the callee's value into
//! the code that resumes at the boundary. See
//! `todo/deep/panic-unwind-leaks-side-channel-call-state.md`.
//!
//! `current_package` gets its own guard in `runtime::accessors_stack`
//! (`CurrentPackageGuard`) because its backing storage is already
//! interior-mutable (`Arc<RwLock<String>>` / `Arc<AtomicU32>`), so that guard
//! needs no unsafe code at all. `state_scope_id` and `when_matched` have no
//! such interior-mutable backing, and the call sites that guard them are too
//! large to route every subsequent `self.*` call through a safe
//! `DerefMut`-based guard (which would mean rewriting hundreds of unrelated
//! lines to use the guard binding instead of `self`), so those two hold a raw
//! `*mut Interpreter` captured at construction time instead.
use super::*;

/// # Safety invariant (shared by every guard in this file)
/// The `Interpreter` behind the guard's raw pointer must outlive the guard,
/// must not move in memory while the guard is alive, and must not be
/// re-entered through a DIFFERENT `&mut Interpreter` alias while the guard is
/// alive. This holds at every call site that constructs one of these guards:
/// each is an ordinary `&mut self` dispatch method that owns a single
/// exclusive borrow of the `Interpreter` for its entire body, the
/// `Interpreter` itself is heap/stack-resident and owned outside the call
/// (never moved mid-call), and the guard never escapes the function that
/// constructs it.
/// RAII guard restoring [`Interpreter::state_scope_id`] on drop. Used by
/// `call_compiled_closure_with_topic` and `call_compiled_function_named_inner`,
/// which switch it to the callee's `state` scope for the call's duration.
pub(crate) struct StateScopeGuard {
    interp: *mut Interpreter,
    saved: Option<u64>,
}

impl StateScopeGuard {
    /// # Safety
    /// See the module-level safety invariant.
    pub(crate) unsafe fn new(interp: &mut Interpreter, new_value: Option<u64>) -> Self {
        let saved = interp.state_scope_id;
        interp.state_scope_id = new_value;
        StateScopeGuard {
            interp: interp as *mut Interpreter,
            saved,
        }
    }
}

impl Drop for StateScopeGuard {
    fn drop(&mut self) {
        // SAFETY: see the module-level invariant.
        unsafe {
            (*self.interp).state_scope_id = self.saved;
        }
    }
}

/// RAII guard restoring the `when_matched` flag (via
/// [`Interpreter::when_matched`] / `set_when_matched`, the field itself being
/// private to `crate::runtime`) on drop. Used by
/// `call_compiled_function_named_inner`, which resets it to `false` for a
/// routine body so a bare `when` inside it does not leak its match state into
/// an enclosing `given`/`with`.
pub(crate) struct WhenMatchedGuard {
    interp: *mut Interpreter,
    saved: bool,
}

impl WhenMatchedGuard {
    /// # Safety
    /// See the module-level safety invariant.
    pub(crate) unsafe fn new(interp: &mut Interpreter, new_value: bool) -> Self {
        let saved = interp.when_matched();
        interp.set_when_matched(new_value);
        WhenMatchedGuard {
            interp: interp as *mut Interpreter,
            saved,
        }
    }
}

impl Drop for WhenMatchedGuard {
    fn drop(&mut self) {
        // SAFETY: see the module-level invariant.
        unsafe {
            (*self.interp).set_when_matched(self.saved);
        }
    }
}

/// RAII guard restoring lexical pragma state (`use fatal`, `use strict`,
/// `use newline`, `use MONKEY-TYPING` -- see [`Interpreter::save_pragma_state`])
/// on drop. Unlike [`StateScopeGuard`]/[`WhenMatchedGuard`], this one needs no
/// raw pointer: it is only used by `call_compiled_closure`, a short function
/// where every subsequent `self.*` access can be rewritten to go through the
/// guard's `Deref`/`DerefMut` instead, so a plain safe `&'a mut Interpreter`
/// borrow works.
pub(crate) struct PragmaGuard<'a> {
    interp: &'a mut Interpreter,
    saved: (bool, bool, crate::runtime::NewlineMode, bool),
}

impl<'a> PragmaGuard<'a> {
    pub(crate) fn new(interp: &'a mut Interpreter) -> Self {
        let saved = interp.save_pragma_state();
        PragmaGuard { interp, saved }
    }
}

impl<'a> std::ops::Deref for PragmaGuard<'a> {
    type Target = Interpreter;
    fn deref(&self) -> &Interpreter {
        self.interp
    }
}

impl<'a> std::ops::DerefMut for PragmaGuard<'a> {
    fn deref_mut(&mut self) -> &mut Interpreter {
        self.interp
    }
}

impl<'a> Drop for PragmaGuard<'a> {
    fn drop(&mut self) {
        self.interp.restore_pragma_state(self.saved);
    }
}
