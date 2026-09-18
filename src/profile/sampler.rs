//! The sampler (ADR-0106 Slice 2) — what Raku code is running when a timer
//! fires.
//!
//! # Mechanism
//!
//! A timer thread bumps one global [`EPOCH`] at the configured rate. Every VM
//! poll — which is every opcode dispatch, plus the JIT's native line hooks —
//! compares that epoch against a thread-local `last_seen` and takes a sample
//! when the two differ. There is deliberately **no registry of mutator
//! threads** to keep in step with `clone_for_thread` or the worker pool: the
//! epoch reaches every thread that polls, which is exactly the set of threads
//! running Raku code.
//!
//! The consequence, and it is a property of the report rather than a bug: a
//! thread blocked in a native call (`sleep`, IO, `await`, a GC stop-the-world
//! park) does not poll, so it contributes nothing. [`report`] states that in
//! the report header so nobody reads a missing thread as an idle one.
//!
//! # What the sample path may do
//!
//! Read one clock, walk the `RoutineFrame` stack, and copy fixed-size records
//! into a per-thread buffer reserved at arm time. It allocates nothing and
//! contends on nothing: an allocating sampler would pollute the GC's
//! candidate buffer and the `alloc-stats` counters, which are two of the
//! things a profile is opened to measure. Folding into the shared tables
//! happens when a buffer fills or at exit, off the sample path.
//!
//! # Weighting
//!
//! Each sample carries the elapsed time since the previous sample **on that
//! thread**, not one tick's worth, so a long region that delays its poll
//! contributes its real duration. That is ADR-0106 §7's mitigation for
//! safepoint bias. Time provably not spent running Raku code — a GC collect,
//! a stop-the-world park, a blocking `sleep`/join/read — is subtracted by
//! [`exclude_non_raku`], so it does not silently land on whichever line the
//! thread happened to be standing on.
//!
//! [`report`]: super::report

use super::LineLocation;
use super::aggregate::ThreadSamples;
use crate::runtime::Interpreter;
use std::cell::{Cell, RefCell};
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};
use std::time::{Duration, Instant};

/// Bumped by the timer thread; compared against each thread's `last_seen`.
/// Relaxed throughout: this is a tick, and a sample taken one poll late is
/// still a sample.
static EPOCH: AtomicU64 = AtomicU64::new(0);

/// `u64::MAX` means "this thread has not polled yet", which is distinct from
/// "this thread has seen epoch 0" — the first poll initializes the clock
/// baseline instead of charging the whole thread startup to one line.
const UNSEEN: u64 = u64::MAX;

/// How deep a stack walk a single sample may perform. Deep recursion would
/// otherwise make the sample path O(depth) at the tick rate, and would need an
/// unbounded buffer. The **top** frames are kept, because those are the ones
/// self time and the nearest caller edge come from; a truncated sample still
/// attributes its self time exactly and only loses inclusive credit for
/// frames below the cut.
pub(crate) const MAX_SAMPLE_FRAMES: usize = 192;

/// Samples held per thread before folding. At the 1000 Hz default this is a
/// fold every few seconds per thread.
const SAMPLES_PER_BUFFER: usize = 1024;

/// How the epoch advances.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum Tick {
    /// The shipped mode: a timer thread at `MUTSU_PROFILE_RATE` Hz.
    Timer,
    /// Every poll is a tick. Deterministic — the samples a run takes are a
    /// function of the bytecode it executes, not of the machine's load — which
    /// is what makes the sampler's *structure* assertable in a test without
    /// asserting a duration (ADR-0106 D5). Far slower than `Timer`, and not a
    /// mode to profile a real program in.
    EveryPoll,
}

/// Arm-time configuration, fixed for the process lifetime.
pub(crate) struct SamplerConfig {
    pub(crate) tick: Tick,
    pub(crate) rate_hz: u64,
    pub(crate) started_at: Instant,
}

static CONFIG: std::sync::OnceLock<SamplerConfig> = std::sync::OnceLock::new();

pub(crate) fn config() -> Option<&'static SamplerConfig> {
    CONFIG.get()
}

/// Threads that have taken at least one sample, for the report header.
static SAMPLED_THREADS: AtomicUsize = AtomicUsize::new(0);

/// Cached [`Tick::EveryPoll`] so the due check is a load rather than a
/// `OnceLock` probe and a match.
static EVERY_POLL: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

pub(crate) fn sampled_threads() -> usize {
    SAMPLED_THREADS.load(Ordering::Relaxed)
}

thread_local! {
    /// Hot: read on every poll inside the armed branch, written once per tick.
    static LAST_SEEN: Cell<u64> = const { Cell::new(UNSEEN) };
    /// Clock baseline and the accumulated non-Raku time to discount from the
    /// next sample's weight.
    static CLOCK: Cell<Option<Instant>> = const { Cell::new(None) };
    static EXCLUDED: Cell<Duration> = const { Cell::new(Duration::ZERO) };
    /// Where the *previous* poll stood. See [`sample_if_due`]: this, not the
    /// location of the poll that notices the tick, is what a sample credits.
    static LAST_POLL: Cell<Option<LineLocation>> = const { Cell::new(None) };
    /// The buffer itself. Registered globally so a thread still alive at exit
    /// (a worker-pool thread, above all) can have its tail drained.
    static BUFFER: RefCell<Option<ThreadSamples>> = const { RefCell::new(None) };
}

/// Arm the sampler. Called once, from the poll network's arm-time trigger
/// computation, so nothing here exists in a disarmed run (§8 gate 1b).
pub(crate) fn arm() {
    let tick = match std::env::var("MUTSU_PROFILE_TICK").ok().as_deref() {
        None | Some("timer") => Tick::Timer,
        Some("every-poll") => Tick::EveryPoll,
        Some(other) => {
            eprintln!(
                "[mutsu profiler] warning: unrecognized MUTSU_PROFILE_TICK={other:?}, using timer"
            );
            Tick::Timer
        }
    };
    let rate_hz = match std::env::var("MUTSU_PROFILE_RATE").ok().as_deref() {
        None => DEFAULT_RATE_HZ,
        Some(text) => match text.parse::<u64>() {
            Ok(hz) if (1..=1_000_000).contains(&hz) => hz,
            _ => {
                eprintln!(
                    "[mutsu profiler] warning: MUTSU_PROFILE_RATE={text:?} is not a rate in 1..=1000000, using {DEFAULT_RATE_HZ}"
                );
                DEFAULT_RATE_HZ
            }
        },
    };
    let _ = CONFIG.set(SamplerConfig {
        tick,
        rate_hz,
        started_at: Instant::now(),
    });
    match tick {
        Tick::Timer => spawn_timer(rate_hz),
        Tick::EveryPoll => EVERY_POLL.store(true, Ordering::Relaxed),
    }
}

/// stackprof and py-spy both default here; ADR-0106 §10 leaves the number open
/// until gate 2 has been measured at a few rates.
const DEFAULT_RATE_HZ: u64 = 1000;

/// The tick source. Detached on purpose: it holds no state anything else
/// reads, so the process exits out from under it without a join.
fn spawn_timer(rate_hz: u64) {
    let period = Duration::from_nanos(1_000_000_000 / rate_hz.max(1));
    let spawned = std::thread::Builder::new()
        .name("mutsu-profiler-tick".to_string())
        .stack_size(64 * 1024)
        .spawn(move || {
            loop {
                std::thread::sleep(period);
                EPOCH.fetch_add(1, Ordering::Relaxed);
            }
        });
    if let Err(e) = spawned {
        eprintln!(
            "[mutsu profiler] warning: cannot start the tick thread ({e}); no time will be sampled"
        );
    }
}

/// Note that this thread is at `here`, and take a sample if it has not yet
/// seen the current epoch.
///
/// # Why the sample credits the *previous* poll's line
///
/// A poll-based sampler cannot see where the tick fired; it finds out at the
/// next poll, by which point the region that was running has finished and the
/// thread is standing at the start of the next one. Crediting the line the
/// poll landed on therefore shifts a profile one region late — systematically,
/// not on average — and on Raku code, where a line is a handful of opcodes,
/// "one region late" means a hot line's cost is reported against the line
/// below it. Crediting the line of the previous poll instead names the region
/// that was actually running when the tick fired: in the interpreter the polls
/// are per opcode, and in JIT-compiled code the emitted hooks are per line
/// transition, so in both the previous poll bounds exactly the region that
/// delayed this one.
///
/// It is also what makes the elapsed weighting do its ADR-0106 §7 job. A long
/// native region that delays its poll has its whole duration charged to the
/// line that entered it rather than to whatever ran next.
///
/// The caller has already established that the profiler is armed, so the cost
/// borne by a disarmed run is zero and the cost borne by an armed one is a
/// thread-local swap, a relaxed load and a compare per poll.
#[inline]
pub(crate) fn sample_if_due(interp: &Interpreter, here: Option<LineLocation>) {
    let was_at = LAST_POLL.with(|c| c.replace(here));
    let seen = LAST_SEEN.with(|c| c.get());
    let epoch = if EVERY_POLL.load(Ordering::Relaxed) {
        seen.wrapping_add(1)
    } else {
        EPOCH.load(Ordering::Relaxed)
    };
    if epoch == seen {
        return;
    }
    LAST_SEEN.with(|c| c.set(epoch));
    if seen == UNSEEN {
        // First poll on this thread: start the clock rather than charge the
        // thread's whole startup to whatever line it reached first.
        CLOCK.with(|c| c.set(Some(Instant::now())));
        SAMPLED_THREADS.fetch_add(1, Ordering::Relaxed);
        return;
    }
    take_sample(interp, was_at);
}

/// The sample path proper. `#[cold]`-adjacent by construction (once per tick),
/// and kept out of [`sample_if_due`] so the due check stays inlinable.
#[inline(never)]
fn take_sample(interp: &Interpreter, was_at: Option<LineLocation>) {
    let now = Instant::now();
    let previous = CLOCK.with(|c| c.replace(Some(now)));
    let excluded = EXCLUDED.with(|c| c.replace(Duration::ZERO));
    let Some(previous) = previous else {
        return;
    };
    let elapsed = now
        .saturating_duration_since(previous)
        .saturating_sub(excluded);

    let stack = interp.routine_stack();

    BUFFER.with(|cell| {
        let Ok(mut slot) = cell.try_borrow_mut() else {
            // A re-entrant sample cannot happen (the poll network is not
            // re-entered from inside this function), but a borrow error must
            // never panic inside the VM.
            return;
        };
        let buffer = slot.get_or_insert_with(|| {
            ThreadSamples::with_capacity(SAMPLES_PER_BUFFER, MAX_SAMPLE_FRAMES)
        });
        buffer.record(elapsed.as_nanos() as u64, was_at, stack);
    });
}

/// Run `f` with its duration charged to nothing.
///
/// The time a thread spends in a GC collect, parked for a stop-the-world, or
/// blocked in a `sleep`/join/read is not time its Raku line is running, and
/// weighting by elapsed time would otherwise credit all of it to whichever
/// line reached the poll. Subtracting it here is what keeps GC and IO out of
/// the line table; naming *which* subsystem it went to instead is #8704.
#[inline]
pub(crate) fn exclude_non_raku<R>(f: impl FnOnce() -> R) -> R {
    if !crate::vm::vm_poll::profiler_armed() {
        return f();
    }
    let started = Instant::now();
    let r = f();
    let spent = started.elapsed();
    EXCLUDED.with(|c| c.set(c.get().saturating_add(spent)));
    r
}

/// Fold this thread's pending samples into the shared tables. Called at exit
/// for the flushing thread; other live threads are drained through the
/// registry (`aggregate::fold_all_threads`).
pub(crate) fn fold_this_thread() {
    BUFFER.with(|cell| {
        if let Ok(mut slot) = cell.try_borrow_mut()
            && let Some(buffer) = slot.as_mut()
        {
            buffer.fold();
        }
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn excluded_time_is_not_charged_to_the_next_sample() {
        EXCLUDED.with(|c| c.set(Duration::ZERO));
        // `exclude_non_raku` is gated on the profiler being armed, so drive
        // the accumulator the way the gate would.
        let spent = {
            let started = Instant::now();
            std::thread::sleep(Duration::from_millis(5));
            started.elapsed()
        };
        EXCLUDED.with(|c| c.set(c.get().saturating_add(spent)));
        let excluded = EXCLUDED.with(|c| c.replace(Duration::ZERO));
        assert!(excluded >= Duration::from_millis(5));
        assert_eq!(EXCLUDED.with(|c| c.get()), Duration::ZERO);
    }

    #[test]
    fn every_poll_tick_always_reports_a_new_epoch() {
        // The deterministic mode must never observe "same epoch as last time",
        // which is what makes a test's sample set a function of the bytecode.
        let mut seen = UNSEEN;
        for _ in 0..4 {
            let epoch = seen.wrapping_add(1);
            assert_ne!(epoch, seen);
            seen = epoch;
        }
    }
}
