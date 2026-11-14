//! The wait of a pool worker that may help (ADR-0123 §7): [`super::stw::stw_aware_wait`]
//! that also gives up its wait when a starved task wants a helper, so
//! `wait_until` can run that task nested and then wait again.

use super::stw::{quiescent_enter, quiescent_exit_checked, stw_requested, thread_is_registered};
use std::sync::{Condvar, MutexGuard};
use std::time::Duration;

/// How often a helper re-checks for a starved task while its condvar stays
/// silent. A starvation is never notified on the waiter's own condvar, so it
/// is polled, at the period the STW-aware wait already polls at.
const HELP_POLL: Duration = Duration::from_millis(10);

/// Wait on `cvar` until `ready` holds and no stop-the-world is pending, and
/// return the guard -- or return `None`, with `guard` released and this thread
/// out of the quiescent state, as soon as a starved task wants a helper. The
/// caller runs it (`worker_pool::help_one`) and waits again.
// Cost: O(1) per wake-up, plus `ready`.
pub(super) fn wait_or_help<'a, T>(
    cvar: &Condvar,
    mut guard: MutexGuard<'a, T>,
    ready: &mut impl FnMut(&T) -> bool,
) -> Option<MutexGuard<'a, T>> {
    let wants_help = crate::runtime::worker_pool::help_wanted;
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
