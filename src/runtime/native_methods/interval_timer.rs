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
use crate::runtime::native_methods::SupplyEvent;
use crate::value::Value;
use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::sync::mpsc::Sender;
use std::sync::{Condvar, Mutex, OnceLock};
use std::time::{Duration, Instant};

/// Returns `Some(period)` to be rescheduled `period` after this deadline
/// (fixed-rate, with catch-up skipping), or `None` to be dropped.
type TimerAction = Box<dyn FnMut() -> Option<Duration> + Send>;

struct TimerEntry {
    next: Instant,
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
        Reverse(self.next).cmp(&Reverse(other.next))
    }
}

type TimerState = (Mutex<BinaryHeap<TimerEntry>>, Condvar);

fn timer_state() -> &'static TimerState {
    static STATE: OnceLock<&'static TimerState> = OnceLock::new();
    STATE.get_or_init(|| {
        let state: &'static TimerState =
            Box::leak(Box::new((Mutex::new(BinaryHeap::new()), Condvar::new())));
        // One long-lived driver thread for the whole process. Actions may
        // clone/drop `Gc` values (a kept promise handle), so the driver is a
        // registered GC mutator; it parks quiescent while waiting.
        crate::runtime::builtins_system::spawn_gc_helper_thread(move || {
            let (heap, cvar) = state;
            let mut guard = heap.lock().unwrap();
            loop {
                let now = Instant::now();
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
                        Some(entry) => entry.next - now,
                    };
                    let (g, _) =
                        crate::gc::block_quiescent(|| cvar.wait_timeout(guard, wait).unwrap());
                    guard = g;
                    continue;
                }
                drop(guard);
                let mut reschedule = Vec::new();
                for mut entry in due {
                    if let Some(period) = (entry.action)() {
                        // Fixed-rate schedule, but never a catch-up burst:
                        // if we fell behind (loaded host), skip ahead.
                        entry.next += period;
                        let now = Instant::now();
                        if entry.next < now {
                            entry.next = now + period;
                        }
                        reschedule.push(entry);
                    }
                }
                guard = heap.lock().unwrap();
                for entry in reschedule {
                    guard.push(entry);
                }
            }
        });
        state
    })
}

fn register_entry(delay: Duration, action: TimerAction) {
    let (heap, cvar) = timer_state();
    let mut guard = heap.lock().unwrap();
    guard.push(TimerEntry {
        next: Instant::now() + delay,
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
    tx: Sender<SupplyEvent>,
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
