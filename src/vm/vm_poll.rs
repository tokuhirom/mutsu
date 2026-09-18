//! The VM poll network shared by safepoint consumers (ADR-0106 Slice 1).
//!
//! GC is the first consumer of the network. The profiler gate is present now
//! so the sampler can be added without changing the dispatch-loop contract;
//! Slice 2 will replace the placeholder profiler consumer with the sampler.

use crate::gc::SafepointKind;

/// The site carried by a poll is the bytecode instruction pointer that caused
/// it. A `u32` matches bytecode jump operands and keeps the JIT helper ABI
/// compact. Non-dispatch safepoints use zero until they have an instruction
/// pointer of their own.
pub(crate) type PollSite = u32;

struct PollTriggers {
    armed: bool,
    gc: bool,
    profiler: bool,
}

fn triggers() -> &'static PollTriggers {
    static TRIGGERS: std::sync::OnceLock<PollTriggers> = std::sync::OnceLock::new();
    TRIGGERS.get_or_init(|| {
        let gc = crate::gc::gc_safepoints_armed();
        let profiler = profile_enabled();
        PollTriggers {
            armed: gc || profiler,
            gc,
            profiler,
        }
    })
}

/// Whether at least one poll consumer is armed.
///
/// This is the single cached gate used by the bytecode dispatch loops. With
/// both consumers disabled, the loop does not enter [`poll`] at all.
#[inline]
pub(crate) fn armed() -> bool {
    let t = triggers();
    t.armed || test_profiler_enabled()
}

/// Run the consumers for one VM poll.
#[inline]
pub(crate) fn poll(kind: SafepointKind, site: PollSite) {
    let t = triggers();
    if t.gc {
        // `armed` was checked by the caller for the hot dispatch-loop sites;
        // this entry point is also used directly at call/re-entry boundaries.
        crate::gc::gc_safepoint_armed(kind);
    }
    if t.profiler || test_profiler_enabled() {
        profiler_poll(kind, site);
    }
}

/// Parse the Slice 5-compatible environment gate early, while keeping this
/// slice independent of the profiler's eventual CLI and report machinery.
fn profile_enabled() -> bool {
    match std::env::var("MUTSU_PROFILE").ok().as_deref() {
        Some("1") => true,
        None | Some("0") => false,
        Some(other) => {
            eprintln!(
                "[mutsu profiler] warning: unrecognized MUTSU_PROFILE={other:?}, treating as 0"
            );
            false
        }
    }
}

/// Placeholder for the sampler consumer. Slice 2 will use `kind` and `site`
/// to record a sample; keeping the call here makes the poll network's second
/// consumer and the JIT site ABI testable before that implementation lands.
#[inline]
fn profiler_poll(_kind: SafepointKind, _site: PollSite) {
    #[cfg(test)]
    if test_profiler_enabled() {
        TEST_POLLS
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .push((_kind, _site));
    }
}

#[cfg(test)]
static TEST_PROFILER_ARMED: std::sync::atomic::AtomicBool =
    std::sync::atomic::AtomicBool::new(false);

#[cfg(test)]
static TEST_POLLS: std::sync::Mutex<Vec<(SafepointKind, PollSite)>> =
    std::sync::Mutex::new(Vec::new());

#[cfg(test)]
fn test_profiler_enabled() -> bool {
    TEST_PROFILER_ARMED.load(std::sync::atomic::Ordering::Relaxed)
}

#[cfg(not(test))]
#[inline]
fn test_profiler_enabled() -> bool {
    false
}

#[cfg(all(test, feature = "jit"))]
mod tests {
    use super::*;
    use std::sync::MutexGuard;

    fn test_lock() -> MutexGuard<'static, ()> {
        static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
        LOCK.get_or_init(|| std::sync::Mutex::new(()))
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }

    #[test]
    fn jit_safepoint_forwards_the_backedge_site_to_the_second_consumer() {
        let _lock = test_lock();
        TEST_POLLS
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .clear();
        TEST_PROFILER_ARMED.store(true, std::sync::atomic::Ordering::Relaxed);

        // The helper has the same ABI used by Cranelift. Calling it here pins
        // the value that a generated native backedge supplies to vm_poll.
        unsafe {
            super::super::vm_jit_helpers::safepoint(std::ptr::null_mut(), 73);
        }

        TEST_PROFILER_ARMED.store(false, std::sync::atomic::Ordering::Relaxed);
        assert_eq!(
            *TEST_POLLS
                .lock()
                .unwrap_or_else(|poisoned| poisoned.into_inner()),
            vec![(SafepointKind::Backedge, 73)]
        );
    }
}
