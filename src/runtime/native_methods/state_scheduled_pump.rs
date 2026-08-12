//! Process-global registries backing `Supply.schedule-on()`'s deferred tap
//! delivery (ADR-0028 Slice 1).
//!
//! Two independent registries, one per scheduler-kind fork:
//!
//! - The **pump registry** (`ThreadPoolScheduler` fork, ADR §2) maps a
//!   `pump_id` to the sender half of a `supply_event_channel()`. The tap-time
//!   emit/done/quit shims look up the sender and forward events into it; a
//!   pooled drain worker (`run_supply_act_loop`) owns the receiver half and
//!   invokes the real callbacks.
//! - The **cue-thunk registry** (any-other-Scheduler fork, ADR §3) stashes a
//!   `(kind, real_cb, payload)` triple under a fresh id so a zero-arg
//!   synthesized thunk handed to the scheduler's own `.cue` can look it back
//!   up and run it once the scheduler decides to.

use crate::runtime::*;
use std::sync::OnceLock;

use super::supply_channel::{SupplySender, supply_event_channel};

type ScheduledPumpMap = std::sync::Mutex<HashMap<u64, SupplySender>>;

fn scheduled_pump_map() -> &'static ScheduledPumpMap {
    static MAP: OnceLock<ScheduledPumpMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

fn next_scheduled_pump_id() -> u64 {
    use std::sync::atomic::{AtomicU64, Ordering};
    static COUNTER: AtomicU64 = AtomicU64::new(1);
    COUNTER.fetch_add(1, Ordering::Relaxed)
}

/// Create a fresh pump channel and register its sender half. Returns the
/// `pump_id` (to be baked into the emit/done/quit shims and recorded on the
/// Tap handle) and the receiver half (to be moved into the drain worker).
pub(in crate::runtime) fn register_scheduled_pump() -> (u64, super::supply_channel::SupplyReceiver)
{
    let (tx, rx) = supply_event_channel();
    let id = next_scheduled_pump_id();
    if let Ok(mut map) = scheduled_pump_map().lock() {
        map.insert(id, tx);
    }
    (id, rx)
}

/// Forward an event into a pump's channel. A missing `pump_id` (drain already
/// closed) is silently ignored — the same behavior as sending into a dropped
/// `mpsc::Sender`.
pub(in crate::runtime) fn scheduled_pump_send(pump_id: u64, event: super::state::SupplyEvent) {
    if let Ok(map) = scheduled_pump_map().lock()
        && let Some(tx) = map.get(&pump_id)
    {
        let _ = tx.send(event);
    }
}

/// Drop a pump's sender, disconnecting the channel so the drain worker's
/// blocking `recv()` observes end-of-stream and exits. Called from
/// `Tap.close`/`Tap.cancel`.
pub(in crate::runtime) fn drop_scheduled_pump(pump_id: u64) {
    if let Ok(mut map) = scheduled_pump_map().lock() {
        map.remove(&pump_id);
    }
}

/// Which callback a cue-thunk registry entry (ADR §3) stands for.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(in crate::runtime) enum ScheduledCueKind {
    Emit,
    Done,
    Quit,
}

type CueThunkMap = std::sync::Mutex<HashMap<u64, (ScheduledCueKind, Value, Vec<Value>)>>;

fn cue_thunk_map() -> &'static CueThunkMap {
    static MAP: OnceLock<CueThunkMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

fn next_cue_thunk_id() -> u64 {
    use std::sync::atomic::{AtomicU64, Ordering};
    static COUNTER: AtomicU64 = AtomicU64::new(1);
    COUNTER.fetch_add(1, Ordering::Relaxed)
}

/// Stash a real callback + its call payload under a fresh id, to be run once
/// the target scheduler gets around to invoking the zero-arg thunk that
/// carries this id.
pub(in crate::runtime) fn register_cue_thunk(
    kind: ScheduledCueKind,
    real_cb: Value,
    payload: Vec<Value>,
) -> u64 {
    let id = next_cue_thunk_id();
    if let Ok(mut map) = cue_thunk_map().lock() {
        map.insert(id, (kind, real_cb, payload));
    }
    id
}

/// Consume a cue-thunk entry (run-once: the scheduler invokes its thunk
/// exactly once).
pub(in crate::runtime) fn take_cue_thunk(id: u64) -> Option<(ScheduledCueKind, Value, Vec<Value>)> {
    cue_thunk_map()
        .lock()
        .ok()
        .and_then(|mut map| map.remove(&id))
}
