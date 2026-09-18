//! The VM poll network shared by safepoint consumers (ADR-0106 Slice 1).
//!
//! GC is the first consumer of the network. The profiler gate is present now
//! so the sampler can be added without changing the dispatch-loop contract;
//! Slice 2 will replace the placeholder profiler consumer with the sampler.

use crate::gc::SafepointKind;

/// The site carried by a poll is the bytecode instruction pointer that caused
/// it. A `u32` matches bytecode jump operands and keeps the JIT helper ABI
/// compact. Non-dispatch safepoints use [`NO_SITE`] until they have an
/// instruction pointer of their own.
pub(crate) type PollSite = u32;

/// The site of a poll raised somewhere other than a dispatch backedge (a call
/// or return boundary, a nested run). `u32::MAX` rather than 0, so "no ip" is
/// distinguishable from "the first instruction of the chunk".
pub(crate) const NO_SITE: PollSite = u32::MAX;

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

/// Whether the profiler consumer is armed.
///
/// Read at JIT compile time to pick the backedge shim (ADR-0106 §5 step 3).
/// Arming is a process-lifetime decision made from the environment on the
/// first poll, which happens long before the hotness threshold compiles a
/// chunk, so the answer cannot change under a compiled body.
#[inline]
pub(crate) fn profiler_armed() -> bool {
    triggers().profiler || test_profiler_enabled()
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

/// Polls seen while the profiler consumer was armed, and the polls among them
/// that arrived from JIT-compiled native code. Slice 1 scaffolding: it is what
/// makes "the ip-passing shim was emitted, and fired, from a native backedge"
/// observable end to end before Slice 2's sampler exists. Slice 2 replaces
/// these with the sample ring buffers.
static POLLS: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
static NATIVE_POLLS: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
static LAST_NATIVE_SITE: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(NO_SITE);

/// Note a poll raised by a JIT-compiled backedge. Only the ip-passing shim
/// calls this, and that shim is only emitted when the profiler is armed, so a
/// disarmed run never reaches it. With the JIT compiled out there is no native
/// code to raise one, and the counter stays at the zero `dump` reports.
#[cfg(feature = "jit")]
#[inline]
pub(crate) fn note_native_poll(site: PollSite) {
    NATIVE_POLLS.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    LAST_NATIVE_SITE.store(site, std::sync::atomic::Ordering::Relaxed);
}

/// One stderr line at exit when the profiler gate is armed, in the shape
/// `vm_stats::dump` established. No-op otherwise.
pub(crate) fn dump() {
    if !profiler_armed() {
        return;
    }
    let polls = POLLS.load(std::sync::atomic::Ordering::Relaxed);
    let native = NATIVE_POLLS.load(std::sync::atomic::Ordering::Relaxed);
    let last = LAST_NATIVE_SITE.load(std::sync::atomic::Ordering::Relaxed);
    let last = if last == NO_SITE {
        "none".to_string()
    } else {
        last.to_string()
    };
    eprintln!(
        "profiler-poll: polls={polls} native-polls={native} last-native-site={last} (ADR-0106 slice 1; no sampler yet)"
    );
}

/// Placeholder for the sampler consumer. Slice 2 will use `kind` and `site`
/// to record a sample; keeping the call here makes the poll network's second
/// consumer and the JIT site ABI testable before that implementation lands.
#[inline]
fn profiler_poll(_kind: SafepointKind, _site: PollSite) {
    POLLS.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
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

    fn polls_while_armed(f: impl FnOnce()) -> Vec<(SafepointKind, PollSite)> {
        let _lock = test_lock();
        TEST_POLLS
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .clear();
        TEST_PROFILER_ARMED.store(true, std::sync::atomic::Ordering::Relaxed);
        f();
        TEST_PROFILER_ARMED.store(false, std::sync::atomic::Ordering::Relaxed);
        TEST_POLLS
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .clone()
    }

    #[test]
    fn jit_safepoint_forwards_the_backedge_site_to_the_second_consumer() {
        // The helper has the same ABI Cranelift calls it with. Calling it here
        // pins the value a generated native backedge supplies to `vm_poll`.
        let seen = polls_while_armed(|| unsafe {
            super::super::vm_jit_helpers::safepoint_at(std::ptr::null_mut(), 73);
        });
        assert_eq!(seen, vec![(SafepointKind::Backedge, 73)]);
    }

    #[test]
    fn the_disarmed_shim_still_polls_but_carries_no_site() {
        // The specialized (profiler-off) form keeps its single argument, so it
        // has no ip to report — the poll still reaches every other consumer.
        let seen = polls_while_armed(|| unsafe {
            super::super::vm_jit_helpers::safepoint(std::ptr::null_mut());
        });
        assert_eq!(seen, vec![(SafepointKind::Backedge, NO_SITE)]);
    }

    #[test]
    fn the_emitted_shim_carries_an_ip_only_when_the_profiler_is_armed() {
        let _lock = test_lock();
        TEST_PROFILER_ARMED.store(false, std::sync::atomic::Ordering::Relaxed);
        let disarmed = super::super::vm_jit_tier_b::PollShim::current();
        TEST_PROFILER_ARMED.store(true, std::sync::atomic::Ordering::Relaxed);
        let armed = super::super::vm_jit_tier_b::PollShim::current();
        TEST_PROFILER_ARMED.store(false, std::sync::atomic::Ordering::Relaxed);

        assert!(
            !disarmed.with_site,
            "a run with no profiler must keep the argument-free backedge call"
        );
        assert!(armed.with_site, "a profiled run must carry the bytecode ip");
        assert_ne!(disarmed.addr, armed.addr);
    }
}
