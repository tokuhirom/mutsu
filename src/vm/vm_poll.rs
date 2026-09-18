//! The VM poll network shared by safepoint consumers (ADR-0106 Slices 1-2).
//!
//! GC is the first consumer of the network and the profiler is the second.
//! Everything the profiler does sits **inside** the armed branch, never before
//! it: a disarmed run must not pay so much as an atomic load per opcode, which
//! is what ADR-0106 §8 gate 1 measures.

use crate::gc::SafepointKind;
use crate::opcode::CompiledCode;
use crate::runtime::Interpreter;

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
        if profiler {
            // Everything the sampler allocates -- the tick thread, the
            // per-thread buffers -- comes into existence here and nowhere
            // else, so a disarmed run carries none of it (gate 1b).
            crate::profile::arm();
        }
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

/// Whether the profiler consumer is armed.  JIT code generation uses this
/// process-lifetime decision to select the location-carrying helper ABI, so a
/// disarmed native backedge keeps the original one-argument helper shape.
#[inline]
pub(crate) fn profiler_armed() -> bool {
    let t = triggers();
    t.profiler || test_profiler_enabled()
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

/// Poll with the bytecode chunk and the running interpreter available, for the
/// exact line counts and for the sampler's stack walk.  Non-dispatch
/// boundaries use [`poll`] because they do not own an instruction pointer.
///
/// The profiler runs *before* the GC consumer: the line was entered at this ip
/// whatever the collector then does, and time spent inside a collect is not
/// time this line was running (`profile::exclude_non_raku` discounts it).
#[inline]
pub(crate) fn poll_code(
    kind: SafepointKind,
    site: PollSite,
    code: &CompiledCode,
    interp: &Interpreter,
) {
    let t = triggers();
    if t.profiler || test_profiler_enabled() {
        record_line(code, site, interp);
    }
    if t.gc {
        crate::gc::gc_safepoint_armed(kind);
    }
    if t.profiler || test_profiler_enabled() {
        profiler_poll(kind, site);
    }
}

/// Record a line entry without making it a GC safepoint: the exact count, and
/// a sample if this thread's tick is due.
///
/// The JIT emits a call to this at every line transition inside a compiled
/// body, and only while the profiler is armed. It is what keeps native code
/// sampling at line granularity instead of once per native body entry — the
/// time half of the parity #8713 established for the counts, and ADR-0106 §8
/// gate 4. The helper keeps the gate for test and future callers.
#[inline]
pub(crate) fn record_line(code: &CompiledCode, site: PollSite, interp: &Interpreter) {
    if profiler_armed() {
        // Resolved once and handed to both consumers: the exact counter needs
        // it to spot a line transition and the sampler needs it to know where
        // this poll stands.
        let here = code
            .location_at(site as usize)
            .map(|(file, line)| crate::profile::LineLocation { file, line });
        crate::profile::record_line_at(code, here);
        crate::profile::sample_if_due(interp, here);
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

/// The poll-network half of the profiler consumer that needs no interpreter:
/// the test hook that pins the JIT's site ABI. The sampler proper runs beside
/// it in [`poll_code`], where the interpreter's Raku stack is in hand.
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
        let code = crate::opcode::CompiledCode::new();
        let mut interp = crate::runtime::Interpreter::new();
        unsafe {
            super::super::vm_jit_helpers::profile_safepoint(&mut interp, &code, 73);
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
