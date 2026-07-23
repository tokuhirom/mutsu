//! Shared deadline-heap timer: one process-wide thread drives every timed
//! event — `Supply.interval` emitters, `Promise.in`/`Promise.at` keepers,
//! scheduler `cue(:in/:at)` delays — off a deadline min-heap, instead of one
//! sleeping OS thread per pending timer (which multiplied into thousands of
//! `clock_nanosleep` wake-ups per second on interval-heavy workloads —
//! S17-supply/syntax.t creates 2000 short-lived 1ms intervals — and amplified
//! badly under CPU contention). This mirrors how Rakudo's scheduler drives
//! timed events from a single timer queue.
//!
//! An entry's action returns `Some(period)` to reschedule itself (interval
//! semantics) or `None` to die (one-shot / receiver gone). Actions run with
//! the heap lock RELEASED, so an action may itself register new timers.
//! Actions must stay cheap (a channel send, a promise keep, a thread spawn) —
//! never run user VM code on the driver thread.
//!
//! Deadlines are seconds on [`thread_compat::mono_now`] rather than `Instant`
//! values: `Instant::now()` panics on wasm32, where the same heap is driven by
//! the cooperative scheduler's pump instead of by a thread.
use crate::runtime::native_methods::SupplyEvent;
use crate::runtime::thread_compat;
use crate::value::Value;
use std::collections::BinaryHeap;
use std::sync::{Condvar, Mutex, OnceLock};
use std::time::Duration;

/// Returns `Some(period)` to be rescheduled `period` after this deadline
/// (fixed-rate, with catch-up skipping), or `None` to be dropped.
type TimerAction = Box<dyn FnMut() -> Option<Duration> + Send>;

struct TimerEntry {
    /// Deadline in `thread_compat::mono_now()` seconds.
    next: f64,
    action: TimerAction,
}

// BinaryHeap is a max-heap; order entries by Reverse(next) so `peek` is the
// earliest deadline. Ties broken arbitrarily (registration order irrelevant).
impl PartialEq for TimerEntry {
    fn eq(&self, other: &Self) -> bool {
        self.next == other.next
    }
}
impl Eq for TimerEntry {}
impl PartialOrd for TimerEntry {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for TimerEntry {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // Reversed so the max-heap's `peek` is the EARLIEST deadline.
        other.next.total_cmp(&self.next)
    }
}

type TimerState = (Mutex<BinaryHeap<TimerEntry>>, Condvar);

fn timer_state() -> &'static TimerState {
    static STATE: OnceLock<&'static TimerState> = OnceLock::new();
    STATE.get_or_init(|| {
        let state: &'static TimerState =
            Box::leak(Box::new((Mutex::new(BinaryHeap::new()), Condvar::new())));
        // On wasm there is no driver thread to spawn: the heap is driven by
        // `wasm_fire_next_timer` from the cooperative scheduler's pump, which
        // jumps the virtual clock to the earliest deadline rather than
        // sleeping until it.
        #[cfg(target_arch = "wasm32")]
        return state;
        // One long-lived driver thread for the whole process. Actions may
        // clone/drop `Gc` values (a kept promise handle), so the driver is a
        // registered GC mutator; it parks quiescent while waiting.
        #[cfg(not(target_arch = "wasm32"))]
        crate::runtime::builtins_system::spawn_gc_helper_thread(move || {
            let (heap, cvar) = state;
            let mut guard = heap.lock().unwrap();
            loop {
                let now = thread_compat::mono_now();
                // Collect every due entry first, then run the actions with
                // the heap lock released: an action registering a new timer
                // (or dropping a value whose finalizer does) must not
                // re-enter the heap mutex.
                let mut due = Vec::new();
                while guard.peek().is_some_and(|e| e.next <= now) {
                    due.push(guard.pop().unwrap());
                }
                if due.is_empty() {
                    let wait = match guard.peek() {
                        // Nothing scheduled: sleep until a registration pokes
                        // us. Bounded waits keep the thread responsive to a
                        // GC stop-the-world (block_quiescent would pin the
                        // heap lock; short chunks avoid holding anything
                        // during the actual wait).
                        None => Duration::from_millis(500),
                        Some(entry) => Duration::from_secs_f64((entry.next - now).max(0.0)),
                    };
                    let (g, _) =
                        crate::gc::block_quiescent(|| cvar.wait_timeout(guard, wait).unwrap());
                    guard = g;
                    continue;
                }
                drop(guard);
                let reschedule = run_due_actions(due);
                guard = heap.lock().unwrap();
                for entry in reschedule {
                    guard.push(entry);
                }
            }
        });
        state
    })
}

/// Run each due entry's action (with the heap lock RELEASED — see the module
/// docs) and return the ones that asked to be rescheduled.
fn run_due_actions(due: Vec<TimerEntry>) -> Vec<TimerEntry> {
    let mut reschedule = Vec::new();
    for mut entry in due {
        if let Some(period) = (entry.action)() {
            // Fixed-rate schedule, but never a catch-up burst: if we fell
            // behind (loaded host), skip ahead.
            entry.next += period.as_secs_f64();
            let now = thread_compat::mono_now();
            if entry.next < now {
                entry.next = now + period.as_secs_f64();
            }
            reschedule.push(entry);
        }
    }
    reschedule
}

/// Fire the earliest pending timer, jumping the virtual clock forward to its
/// deadline. Returns false when no timer is pending — the cooperative
/// scheduler's signal that a waiter can never be woken.
///
/// This is the wasm stand-in for the driver thread: same heap, same actions,
/// but driven from [`crate::runtime::wasm_sched::pump`] at the points where a
/// native build would be blocked waiting.
#[cfg(target_arch = "wasm32")]
pub(crate) fn wasm_fire_next_timer() -> bool {
    let (heap, _) = timer_state();
    let deadline = match heap.lock().unwrap().peek() {
        Some(entry) => entry.next,
        None => return false,
    };
    crate::runtime::wasm_sched::advance_clock_to(deadline);
    let now = thread_compat::mono_now();
    let mut due = Vec::new();
    {
        let mut guard = heap.lock().unwrap();
        while guard.peek().is_some_and(|e| e.next <= now) {
            due.push(guard.pop().unwrap());
        }
    }
    let reschedule = run_due_actions(due);
    let mut guard = heap.lock().unwrap();
    for entry in reschedule {
        guard.push(entry);
    }
    true
}

fn register_entry(delay: Duration, action: TimerAction) {
    let (heap, cvar) = timer_state();
    let mut guard = heap.lock().unwrap();
    guard.push(TimerEntry {
        next: thread_compat::mono_now() + delay.as_secs_f64(),
        action,
    });
    cvar.notify_all();
}

/// Clamp a user-supplied seconds value to a `Duration` that is safe to add to
/// an `Instant`: non-finite and negative inputs become zero, and huge values
/// are capped (~95 years) so `Instant + Duration` can never overflow.
pub(crate) fn clamp_delay_secs(secs: f64) -> Duration {
    const MAX_DELAY_SECS: f64 = 3.0e9;
    if secs.is_finite() && secs > 0.0 {
        Duration::from_secs_f64(secs.min(MAX_DELAY_SECS))
    } else {
        Duration::ZERO
    }
}

/// Register a new interval emitter: first emission after `initial_delay`
/// (0 = as soon as the driver runs), then every `period`. Emits monotonically
/// increasing ticks (0, 1, 2, ...) as `SupplyEvent::Emit(Int)` into `tx`
/// until the receiver is dropped.
pub(crate) fn register_interval(
    period: Duration,
    initial_delay: Duration,
    tx: super::supply_channel::SupplySender,
) {
    let mut tick: i64 = 0;
    register_entry(
        initial_delay,
        Box::new(move || {
            if tx.send(SupplyEvent::Emit(Value::int(tick))).is_ok() {
                tick = tick.saturating_add(1);
                Some(period)
            } else {
                // send failed → receiver gone → entry dropped.
                None
            }
        }),
    );
}

/// Register a one-shot action to run once `delay` from now. The action runs
/// on the shared driver thread, so it must stay cheap (keep a promise, spawn
/// a worker) — never run user VM code in it.
pub(crate) fn register_once(delay: Duration, action: Box<dyn FnOnce() + Send>) {
    let mut action = Some(action);
    register_entry(
        delay,
        Box::new(move || {
            if let Some(f) = action.take() {
                f();
            }
            None
        }),
    );
}
