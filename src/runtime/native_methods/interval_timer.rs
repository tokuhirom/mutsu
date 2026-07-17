//! Shared interval timer: one process-wide thread drives every
//! `Supply.interval` emitter off a deadline min-heap, instead of one
//! 1ms-sleep-loop OS thread per interval instance (which multiplied into
//! thousands of `clock_nanosleep` wake-ups per second on interval-heavy
//! workloads — S17-supply/syntax.t creates 2000 short-lived 1ms intervals —
//! and amplified badly under CPU contention). This mirrors how Rakudo's
//! scheduler drives intervals from a single timer queue.
//!
//! An entry dies when its channel receiver is dropped (`send` fails), same
//! lifecycle as the old per-instance thread.
use crate::runtime::native_methods::SupplyEvent;
use crate::value::Value;
use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::sync::mpsc::Sender;
use std::sync::{Condvar, Mutex, OnceLock};
use std::time::{Duration, Instant};

struct IntervalEntry {
    next: Instant,
    period: Duration,
    tick: i64,
    tx: Sender<SupplyEvent>,
}

// BinaryHeap is a max-heap; order entries by Reverse(next) so `peek` is the
// earliest deadline. Ties broken arbitrarily (registration order irrelevant).
impl PartialEq for IntervalEntry {
    fn eq(&self, other: &Self) -> bool {
        self.next == other.next
    }
}
impl Eq for IntervalEntry {}
impl PartialOrd for IntervalEntry {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for IntervalEntry {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        Reverse(self.next).cmp(&Reverse(other.next))
    }
}

type TimerState = (Mutex<BinaryHeap<IntervalEntry>>, Condvar);

fn timer_state() -> &'static TimerState {
    static STATE: OnceLock<&'static TimerState> = OnceLock::new();
    STATE.get_or_init(|| {
        let state: &'static TimerState =
            Box::leak(Box::new((Mutex::new(BinaryHeap::new()), Condvar::new())));
        // One long-lived driver thread for the whole process. It only touches
        // scalar `Value`s (`Value::int` ticks — no Gc payload), and parks
        // quiescent while waiting, so it is safe as a GC helper.
        crate::runtime::builtins_system::spawn_gc_helper_thread(move || {
            let (heap, cvar) = state;
            let mut guard = heap.lock().unwrap();
            loop {
                let now = Instant::now();
                match guard.peek() {
                    None => {
                        // Nothing scheduled: sleep until a registration pokes
                        // us. Bounded waits keep the thread responsive to a
                        // GC stop-the-world (block_quiescent would pin the
                        // heap lock; short chunks avoid holding anything
                        // during the actual wait).
                        let (g, _) = crate::gc::block_quiescent(|| {
                            cvar.wait_timeout(guard, Duration::from_millis(500))
                                .unwrap()
                        });
                        guard = g;
                    }
                    Some(entry) if entry.next > now => {
                        let wait = entry.next - now;
                        let (g, _) =
                            crate::gc::block_quiescent(|| cvar.wait_timeout(guard, wait).unwrap());
                        guard = g;
                    }
                    Some(_) => {
                        let mut entry = guard.pop().unwrap();
                        if entry
                            .tx
                            .send(SupplyEvent::Emit(Value::int(entry.tick)))
                            .is_ok()
                        {
                            entry.tick = entry.tick.saturating_add(1);
                            // Fixed-rate schedule, but never a catch-up burst:
                            // if we fell behind (loaded host), skip ahead.
                            entry.next += entry.period;
                            let now = Instant::now();
                            if entry.next < now {
                                entry.next = now + entry.period;
                            }
                            guard.push(entry);
                        }
                        // send failed → receiver gone → entry dropped.
                    }
                }
            }
        });
        state
    })
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
    let (heap, cvar) = timer_state();
    let mut guard = heap.lock().unwrap();
    guard.push(IntervalEntry {
        next: Instant::now() + initial_delay,
        period,
        tick: 0,
        tx,
    });
    cvar.notify_all();
}
