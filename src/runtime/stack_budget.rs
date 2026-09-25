//! Address-space budget for the native stacks of user-code threads (ADR-0123).
//!
//! Every thread that runs user VM code reserves a large native stack so deep
//! Raku recursion has room (ADR-0100). The reservation costs no physical
//! memory, but it does count against the process's virtual address space, and
//! under `ulimit -v` (`RLIMIT_AS`) a burst of such threads exhausts the limit:
//! the ecosystem sandbox's 6 GB limit fits barely twenty 256 MiB stacks, so
//! 64 `start` blocks made `pthread_create` fail with `EAGAIN` (#9377).
//!
//! Waiting for `pthread_create` to fail is the wrong signal, because the
//! address space that is left at that point is also what the heap needs: the
//! next `malloc` fails, and Rust aborts. So the stacks get a budget of their
//! own -- half of `RLIMIT_AS`, when there is a limit at all -- that is checked
//! *before* a thread is created:
//!
//! - the worker pool grows only while a stack fits the budget, and otherwise
//!   leaves the task queued for a worker that is still making progress;
//! - a thread that must exist (every pool worker is blocked, or the user asked
//!   for a `Thread` explicitly) steps down to a smaller stack tier, and past
//!   the budget if it has to -- the budget is the line for *optional* growth,
//!   not a hard cap.
//!
//! A smaller stack is still guarded (ADR-0100 scales its reserve with the
//! stack), so the cost of stepping down is that deep recursion raises its
//! catchable "Too deep recursion" earlier, never a crash.

use std::sync::OnceLock;
use std::sync::atomic::{AtomicUsize, Ordering};

const MIB: usize = 1024 * 1024;

/// Stack sizes a user-code thread may get, largest (the normal one) first.
/// The smallest must stay guardable by `vm_stack_guard`.
pub(crate) const STACK_TIERS: [usize; 3] = [256 * MIB, 64 * MIB, 32 * MIB];

/// Bytes of stack currently reserved by live user-code threads (not counting
/// the main thread, whose stack exists before the budget is consulted).
static RESERVED: AtomicUsize = AtomicUsize::new(0);

/// The stack budget in bytes, or `None` when the address space is unlimited.
///
/// `MUTSU_STACK_BUDGET_MB` overrides the derived value (for tests, and for
/// running under a limit the process cannot see); `0` is honoured as "no
/// optional growth at all".
fn budget() -> Option<usize> {
    static BUDGET: OnceLock<Option<usize>> = OnceLock::new();
    *BUDGET.get_or_init(|| {
        if let Ok(mb) = std::env::var("MUTSU_STACK_BUDGET_MB")
            && let Ok(mb) = mb.trim().parse::<usize>()
        {
            return Some(mb.saturating_mul(MIB));
        }
        address_space_limit().map(|limit| limit / 2)
    })
}

/// The soft `RLIMIT_AS`, or `None` when it is unlimited or cannot be read.
#[cfg(all(unix, feature = "native"))]
fn address_space_limit() -> Option<usize> {
    let mut lim = libc::rlimit {
        rlim_cur: 0,
        rlim_max: 0,
    };
    // SAFETY: `getrlimit` only writes the struct we hand it.
    let rc = unsafe { libc::getrlimit(libc::RLIMIT_AS, &mut lim) };
    if rc != 0 || lim.rlim_cur == libc::RLIM_INFINITY {
        return None;
    }
    usize::try_from(lim.rlim_cur).ok()
}

#[cfg(not(all(unix, feature = "native")))]
fn address_space_limit() -> Option<usize> {
    None
}

/// A stack reservation, released when dropped (the thread that owns the stack
/// holds it until it exits).
pub(crate) struct StackReservation {
    size: usize,
}

impl StackReservation {
    pub(crate) fn size(&self) -> usize {
        self.size
    }
}

impl Drop for StackReservation {
    fn drop(&mut self) {
        RESERVED.fetch_sub(self.size, Ordering::AcqRel);
    }
}

/// Reserve `size` bytes if they fit the budget.
pub(crate) fn try_reserve(size: usize) -> Option<StackReservation> {
    let Some(limit) = budget() else {
        RESERVED.fetch_add(size, Ordering::AcqRel);
        return Some(StackReservation { size });
    };
    RESERVED
        .fetch_update(Ordering::AcqRel, Ordering::Acquire, |cur| {
            cur.checked_add(size).filter(|next| *next <= limit)
        })
        .ok()
        .map(|_| StackReservation { size })
}

/// Reserve `size` bytes whether or not they fit the budget -- for a thread
/// that has to exist regardless.
pub(crate) fn reserve_over_budget(size: usize) -> StackReservation {
    RESERVED.fetch_add(size, Ordering::AcqRel);
    StackReservation { size }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tiers_descend_and_stay_guardable() {
        assert!(STACK_TIERS.windows(2).all(|w| w[0] > w[1]));
        assert!(*STACK_TIERS.last().unwrap() >= 32 * MIB);
    }
}
