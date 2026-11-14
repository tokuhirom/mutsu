//! The wait of a pool worker that may help (ADR-0123 §7): [`super::stw::stw_aware_wait`]
//! that also gives up its wait when a starved task wants a helper, so
//! `wait_until` can run that task nested and then wait again.

use super::stw::{quiescent_enter, quiescent_exit_checked, stw_requested, thread_is_registered};
use std::sync::{Condvar, MutexGuard, OnceLock};
use std::time::Duration;

/// The worker pool's side of helping, installed by the pool when it is
/// created: the GC sits below the runtime and does not name it (#10779).
pub(crate) struct HelpHooks {
    /// Count the calling thread as a helper; `false` when it may not help
    /// (it is not a pool worker running a task).
    pub(crate) register: fn() -> bool,
    /// Undo a `register` that returned `true`.
    pub(crate) unregister: fn(),
    /// Whether a starved task wants a helper the calling thread could be.
    pub(crate) wanted: fn() -> bool,
    /// Run one starved task nested, if one is still there.
    pub(crate) help: fn(),
}

static HOOKS: OnceLock<HelpHooks> = OnceLock::new();

/// Install the pool's helping hooks. The first installation wins.
// Cost: O(1).
pub(crate) fn install_help_hooks(hooks: HelpHooks) {
    let _ = HOOKS.set(hooks);
}

/// The calling thread's registration as a helper for one wait; released on
/// drop.
pub(super) struct Helper(Option<&'static HelpHooks>);

impl Helper {
    /// Register the calling thread, when a pool exists and it may help.
    // Cost: O(1) (the pool lock).
    pub(super) fn register() -> Self {
        Self(HOOKS.get().filter(|hooks| (hooks.register)()))
    }

    /// The hooks to help through, when this thread may help.
    pub(super) fn hooks(&self) -> Option<&'static HelpHooks> {
        self.0
    }
}

impl Drop for Helper {
    fn drop(&mut self) {
        if let Some(hooks) = self.0 {
            (hooks.unregister)();
        }
    }
}

/// How often a helper re-checks for a starved task while its condvar stays
/// silent. A starvation is never notified on the waiter's own condvar, so it
/// is polled, at the period the STW-aware wait already polls at.
const HELP_POLL: Duration = Duration::from_millis(10);

/// Wait on `cvar` until `ready` holds and no stop-the-world is pending, and
/// return the guard -- or return `None`, with `guard` released and this thread
/// out of the quiescent state, as soon as a starved task wants a helper. The
/// caller runs it (`hooks.help`) and waits again.
// Cost: O(1) per wake-up, plus `ready`.
pub(super) fn wait_or_help<'a, T>(
    cvar: &Condvar,
    mut guard: MutexGuard<'a, T>,
    ready: &mut impl FnMut(&T) -> bool,
    hooks: &HelpHooks,
) -> Option<MutexGuard<'a, T>> {
    let wants_help = hooks.wanted;
    if !super::gc_ptr::gc_enabled() || !thread_is_registered() {
        // Outside the quiescence accounting: a plain polled wait, which still
        // refuses to leave mid-scan.
        loop {
            if ready(&guard) && !stw_requested() {
                return Some(guard);
            }
            if wants_help() && !stw_requested() {
                return None;
            }
            guard = cvar.wait_timeout(guard, HELP_POLL).unwrap().0;
        }
    }
    quiescent_enter();
    loop {
        if ready(&guard) && !stw_requested() {
            break;
        }
        if wants_help() && !stw_requested() {
            drop(guard);
            quiescent_exit_checked();
            return None;
        }
        guard = cvar.wait_timeout(guard, HELP_POLL).unwrap().0;
    }
    quiescent_exit_checked();
    Some(guard)
}
