use crate::runtime::*;
use std::sync::OnceLock;
use std::sync::atomic::{AtomicU64, Ordering};

// --- FakeScheduler support (for Test::Tap) ---

/// A scheduled item in a FakeScheduler.
#[derive(Debug, Clone)]
struct FakeScheduled {
    deadline: f64,
    /// Regular callback (from user code) or None for counter-mode entries.
    callback: Option<Value>,
    /// Counter index for counter-mode scheduled items.
    counter_value: i64,
}

/// Per-instance state for FakeScheduler.
#[derive(Debug, Clone)]
struct FakeSchedulerState {
    time: f64,
    upcoming: Vec<FakeScheduled>,
}

type FakeSchedulerMap = std::sync::Mutex<HashMap<u64, FakeSchedulerState>>;

fn fake_scheduler_map() -> &'static FakeSchedulerMap {
    static MAP: OnceLock<FakeSchedulerMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

static FAKE_SCHEDULER_ID_COUNTER: AtomicU64 = AtomicU64::new(1);

pub(in crate::runtime) fn next_fake_scheduler_id() -> u64 {
    FAKE_SCHEDULER_ID_COUNTER.fetch_add(1, Ordering::Relaxed)
}

pub(in crate::runtime) fn fake_scheduler_cue(
    scheduler_id: u64,
    callback: Value,
    every: Option<f64>,
    delay: f64,
) {
    if let Ok(mut map) = fake_scheduler_map().lock() {
        let state = map
            .entry(scheduler_id)
            .or_insert_with(|| FakeSchedulerState {
                time: 0.0,
                upcoming: Vec::new(),
            });
        if let Some(every) = every {
            // Schedule 100 future callbacks (matching FakeScheduler behavior)
            let mut deadline = state.time + delay;
            for _ in 0..100 {
                state.upcoming.push(FakeScheduled {
                    deadline,
                    callback: Some(callback.clone()),
                    counter_value: -1,
                });
                deadline += every;
            }
        } else {
            // No :every -- execute immediately (but we store it for progress-by)
            state.upcoming.push(FakeScheduled {
                deadline: state.time + delay,
                callback: Some(callback),
                counter_value: -1,
            });
        }
    }
}

/// Register a counter-mode cue: each scheduled item produces an incrementing
/// integer value instead of calling a callback.
pub(in crate::runtime) fn fake_scheduler_cue_counter(scheduler_id: u64, every: f64, delay: f64) {
    if let Ok(mut map) = fake_scheduler_map().lock() {
        let state = map
            .entry(scheduler_id)
            .or_insert_with(|| FakeSchedulerState {
                time: 0.0,
                upcoming: Vec::new(),
            });
        let mut deadline = state.time + delay;
        for i in 0..100i64 {
            state.upcoming.push(FakeScheduled {
                deadline,
                callback: None,
                counter_value: i,
            });
            deadline += every;
        }
    }
}

/// Advance time, run callbacks whose deadline <= new time, and return counter
/// values from counter-mode entries.  Regular callbacks are returned for the
/// caller to execute.
pub(in crate::runtime) fn fake_scheduler_progress_by(
    scheduler_id: u64,
    duration: f64,
) -> (Vec<Value>, Vec<i64>) {
    if let Ok(mut map) = fake_scheduler_map().lock() {
        if let Some(state) = map.get_mut(&scheduler_id) {
            state.time += duration;
            let new_time = state.time;
            let mut to_run = Vec::new();
            let mut remaining = Vec::new();
            for item in state.upcoming.drain(..) {
                if item.deadline <= new_time {
                    to_run.push(item);
                } else {
                    remaining.push(item);
                }
            }
            to_run.sort_by(|a, b| {
                a.deadline
                    .partial_cmp(&b.deadline)
                    .unwrap_or(std::cmp::Ordering::Equal)
            });
            state.upcoming = remaining;
            let mut callbacks = Vec::new();
            let mut counter_values = Vec::new();
            for item in to_run {
                if let Some(cb) = item.callback {
                    callbacks.push(cb);
                } else {
                    counter_values.push(item.counter_value);
                }
            }
            (callbacks, counter_values)
        } else {
            (Vec::new(), Vec::new())
        }
    } else {
        (Vec::new(), Vec::new())
    }
}

pub(in crate::runtime) fn fake_scheduler_init(scheduler_id: u64, time: f64) {
    if let Ok(mut map) = fake_scheduler_map().lock() {
        map.insert(
            scheduler_id,
            FakeSchedulerState {
                time,
                upcoming: Vec::new(),
            },
        );
    }
}
