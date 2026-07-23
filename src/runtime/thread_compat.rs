//! One façade over "run this concurrently", "wait", and "what time is it" —
//! OS threads and real clocks natively, the cooperative queue and virtual clock
//! of [`crate::runtime::wasm_sched`] on `wasm32`.
//!
//! Every spawn site in the interpreter goes through
//! [`crate::runtime::builtins_system::spawn_user_thread`] /
//! `spawn_gc_helper_thread`, which return this module's [`JoinHandle`] rather
//! than `std::thread::JoinHandle`. That is what keeps the wasm build free of
//! `cfg` noise at ~20 call sites: the concurrency primitives are written once
//! against real threads, and only this module knows the browser has none.

use std::time::Duration;

/// Handle on work that runs concurrently: an OS thread natively, a queued task
/// on wasm.
pub(crate) struct JoinHandle<T> {
    #[cfg(not(target_arch = "wasm32"))]
    inner: std::thread::JoinHandle<T>,
    #[cfg(target_arch = "wasm32")]
    inner: super::wasm_sched::PendingTask<T>,
}

impl<T> JoinHandle<T> {
    /// Wait for the work to finish and take its result. On wasm this *runs* the
    /// task (it was only queued by `spawn`), so a `join` is the point at which
    /// a `start` block actually executes if nothing pumped the queue earlier.
    pub(crate) fn join(self) -> std::thread::Result<T> {
        self.inner.join()
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn spawn_thread<F, T>(stack_size: Option<usize>, f: F) -> JoinHandle<T>
where
    F: FnOnce() -> T + Send + 'static,
    T: Send + 'static,
{
    let mut builder = std::thread::Builder::new();
    if let Some(size) = stack_size {
        builder = builder.stack_size(size);
    }
    JoinHandle {
        inner: builder.spawn(f).expect("failed to spawn worker thread"),
    }
}

/// Queue `f` on the single browser thread. The stack size is meaningless here
/// (there is one stack) and is ignored.
#[cfg(target_arch = "wasm32")]
pub(crate) fn spawn_thread<F, T>(_stack_size: Option<usize>, f: F) -> JoinHandle<T>
where
    F: FnOnce() -> T + Send + 'static,
    T: Send + 'static,
{
    JoinHandle {
        inner: super::wasm_sched::spawn(f),
    }
}

/// Block the current thread for `duration`.
///
/// On wasm there is nothing to block — freezing the only thread would freeze
/// the page — so a sleep instead runs whatever work is pending and jumps the
/// virtual clock forward by the requested time. Code that sleeps to let a
/// `start` block get somewhere therefore still behaves as intended, and elapsed
/// time still reads back correctly.
pub(crate) fn sleep(duration: Duration) {
    #[cfg(not(target_arch = "wasm32"))]
    {
        std::thread::sleep(duration);
    }
    #[cfg(target_arch = "wasm32")]
    {
        let deadline = mono_now() + duration.as_secs_f64();
        while mono_now() < deadline && super::wasm_sched::has_ready_tasks() {
            super::wasm_sched::pump();
        }
        super::wasm_sched::advance_clock_to(deadline);
    }
}

/// Seconds on a monotonic clock, from an arbitrary process-local epoch. Used
/// for deadlines (the timer heap, elapsed-time checks) where `SystemTime` would
/// be wrong because it can jump.
pub(crate) fn mono_now() -> f64 {
    #[cfg(not(target_arch = "wasm32"))]
    {
        use std::sync::OnceLock;
        use std::time::Instant;
        static EPOCH: OnceLock<Instant> = OnceLock::new();
        EPOCH.get_or_init(Instant::now).elapsed().as_secs_f64()
    }
    #[cfg(target_arch = "wasm32")]
    {
        // `Instant::now()` panics on wasm32-unknown-unknown, so the monotonic
        // clock is the host's `Date.now()` plus whatever the virtual clock has
        // been advanced by. `Date.now()` can step backwards across an NTP
        // adjustment where a true monotonic clock would not; over the lifetime
        // of one page that is not worth a second clock.
        js_epoch_secs() + super::wasm_sched::clock_offset_secs()
    }
}

/// A drop-in `std::time::Instant` that also works in a browser.
///
/// `std::time::Instant::now()` panics on wasm32-unknown-unknown ("time not
/// implemented on this platform"), which took down every deadline in the
/// interpreter — the `react` drive loop's 30s bound, the supply throttle/stable
/// windows, the GC's stop-the-world timeout. This carries [`mono_now`] seconds
/// instead and supports the same handful of operations those sites use.
#[derive(Copy, Clone, PartialEq, PartialOrd, Debug)]
pub(crate) struct Instant(f64);

impl Instant {
    pub(crate) fn now() -> Self {
        Instant(mono_now())
    }

    pub(crate) fn elapsed(&self) -> Duration {
        Duration::from_secs_f64((mono_now() - self.0).max(0.0))
    }

    /// Saturating, like `Instant::saturating_duration_since`: a deadline that
    /// has already passed yields `ZERO`, never a negative (panicking) value.
    pub(crate) fn duration_since(&self, earlier: Instant) -> Duration {
        Duration::from_secs_f64((self.0 - earlier.0).max(0.0))
    }
}

impl std::ops::Add<Duration> for Instant {
    type Output = Instant;
    fn add(self, rhs: Duration) -> Instant {
        Instant(self.0 + rhs.as_secs_f64())
    }
}

impl std::ops::Sub<Instant> for Instant {
    type Output = Duration;
    fn sub(self, rhs: Instant) -> Duration {
        self.duration_since(rhs)
    }
}

/// Seconds since the UNIX epoch, from the JS `Date.now()` the host already has.
/// Without this every wasm build reported time 0 — `DateTime.now.year` was 1970
/// and `time` was `0`.
#[cfg(all(target_arch = "wasm32", feature = "wasm"))]
pub(crate) fn js_epoch_secs() -> f64 {
    use wasm_bindgen::prelude::*;
    #[wasm_bindgen]
    unsafe extern "C" {
        #[wasm_bindgen(js_namespace = Date)]
        fn now() -> f64;
    }
    now() / 1000.0
}

/// wasm32 without the `wasm` feature has no JS glue to ask, so time stands
/// still — as it did everywhere before.
#[cfg(all(target_arch = "wasm32", not(feature = "wasm")))]
pub(crate) fn js_epoch_secs() -> f64 {
    0.0
}
