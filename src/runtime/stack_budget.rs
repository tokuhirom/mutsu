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

/// The stack a user-code thread starts on (ADR-17392).
///
/// With no budget (an unlimited address space, the normal case) it is the
/// whole logical stack, `native_stack::LOGICAL_STACK_BYTES`, as before stacks
/// grew. Under a budget it is the smallest guardable stack
/// (`native_stack::MIN_GUARDABLE_STACK_BYTES`: 2 MiB in a release build,
/// 16 MiB in a debug one), and the thread maps further segments only as deep
/// as it actually recurses, each against the budget. A worker parked in
/// `await` then costs that small stack, whichever worker it is, where every
/// optional-growth worker used to reserve the full 256 MiB and the rest
/// stepped down through ever smaller fixed tiers (ADR-0123 D2, §6).
///
/// A build without stack segments (no `native` feature, or wasm32) keeps the
/// whole logical stack: a small one could not grow.
// Cost: O(1).
pub(crate) fn initial_stack_bytes() -> usize {
    let growable = cfg!(all(feature = "native", not(target_arch = "wasm32")));
    if growable && budget().is_some() {
        crate::native_stack::MIN_GUARDABLE_STACK_BYTES
    } else {
        crate::native_stack::LOGICAL_STACK_BYTES
    }
}

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

/// Address space one glibc malloc arena reserves: `HEAP_MAX_SIZE`, which is
/// `2 * DEFAULT_MMAP_THRESHOLD_MAX` = 64 MiB on a 64-bit target.
#[cfg(all(target_os = "linux", target_env = "gnu", feature = "native"))]
const MALLOC_ARENA_BYTES: usize = 64 * MIB;

/// Cap glibc's malloc arenas when the address space is limited.
///
/// glibc gives a thread that meets contention its own arena, up to `8 x cores`
/// of them, and every arena reserves 64 MiB of address space up front. Under
/// `RLIMIT_AS` those reservations come out of the half D1 leaves the heap, and
/// they scale with the thread count exactly as the stacks do: 48 `start`
/// workers parked on one `await` under the ecosystem sandbox's 6 GB limit
/// spent ~2 GB on arenas alone, and the next thread's `pthread_create` or the
/// next `malloc` failed (ADR-0123 §5). The cap keeps the arenas to about a
/// sixteenth of the limit, never fewer than two; threads beyond it share
/// arenas, which costs some allocator contention and nothing else.
///
/// Does nothing without a limit, or when the user set `MALLOC_ARENA_MAX`
/// themselves. Call it once, before any worker thread exists.
// Cost: O(1).
pub(crate) fn cap_malloc_arenas() {
    #[cfg(all(target_os = "linux", target_env = "gnu", feature = "native"))]
    {
        if std::env::var_os("MALLOC_ARENA_MAX").is_some() {
            return;
        }
        let Some(limit) = address_space_limit() else {
            return;
        };
        let arenas = (limit / 16 / MALLOC_ARENA_BYTES).max(2);
        let arenas = libc::c_int::try_from(arenas).unwrap_or(libc::c_int::MAX);
        // SAFETY: `mallopt` only adjusts allocator tuning; M_ARENA_MAX takes
        // effect for arenas created after the call.
        unsafe {
            libc::mallopt(libc::M_ARENA_MAX, arenas);
        }
    }
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
        .try_update(Ordering::AcqRel, Ordering::Acquire, |cur| {
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
    fn the_initial_stack_is_guardable() {
        assert!(initial_stack_bytes() >= crate::native_stack::MIN_GUARDABLE_STACK_BYTES);
    }
}
