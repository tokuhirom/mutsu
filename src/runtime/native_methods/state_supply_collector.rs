//! Process-global registry backing `Interpreter::supply_collect_values`
//! (ADR-0031 Decision B / Slice 2): a `collector_id -> ReactWaker` map so the
//! synthesized `__SupplyCollector` emit/done/quit shim (invoked from
//! whichever thread the tapped supply's own delivery machinery runs on) can
//! push events into the waker the calling thread is draining, without a
//! synthesized callable needing to carry a Rust closure directly.
//!
//! This is the same "shim instance carries an id, a native method dispatch
//! looks the id up in a process-global map" idiom `state_scheduled_pump.rs`
//! established for `Supply.schedule-on()` (ADR-0028 Slice 1/2).

use crate::runtime::HashMap;
use crate::value::SharedChannel;
use crate::value::waker::ReactWaker;
use std::sync::OnceLock;

/// Where a collector's emit/done/quit events go.
#[derive(Clone)]
pub(in crate::runtime) enum CollectorSink {
    /// `supply_collect_values`: an eager drain reading a waker.
    Waker(ReactWaker),
    /// `Supply.list` / `.Seq`: a lazy list pulling from a channel; emit sends,
    /// done closes and quit fails it, so the consumer sees each value as soon
    /// as it is emitted and the quit only after the values before it.
    Channel(SharedChannel),
}

type SupplyCollectorMap = std::sync::Mutex<HashMap<u64, CollectorSink>>;

fn supply_collector_map() -> &'static SupplyCollectorMap {
    static MAP: OnceLock<SupplyCollectorMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

fn next_supply_collector_id() -> u64 {
    use std::sync::atomic::{AtomicU64, Ordering};
    static COUNTER: AtomicU64 = AtomicU64::new(1);
    COUNTER.fetch_add(1, Ordering::Relaxed)
}

/// Register a fresh collector sink for `waker`. Returns the `collector_id` to
/// bake into the emit/done/quit shims built for this drain.
// Cost: O(1) amortized.
pub(in crate::runtime) fn register_supply_collector(waker: ReactWaker) -> u64 {
    register_collector_sink(CollectorSink::Waker(waker))
}

/// Register a collector that feeds `channel` (the lazy `Supply.list`).
// Cost: O(1) amortized.
pub(in crate::runtime) fn register_supply_channel_collector(channel: SharedChannel) -> u64 {
    register_collector_sink(CollectorSink::Channel(channel))
}

// Cost: O(1) amortized.
fn register_collector_sink(sink: CollectorSink) -> u64 {
    let id = next_supply_collector_id();
    if let Ok(mut map) = supply_collector_map().lock() {
        map.insert(id, sink);
    }
    id
}

/// Look up a collector's sink (the shim's native-method dispatch calls this
/// on every emit/done/quit invocation).
// Cost: O(1).
pub(in crate::runtime) fn supply_collector_sink(collector_id: u64) -> Option<CollectorSink> {
    supply_collector_map()
        .lock()
        .ok()
        .and_then(|map| map.get(&collector_id).cloned())
}

/// Drop a collector once its drain is done, so a source that (incorrectly,
/// or after the drain gave up at its deadline) keeps emitting later just
/// finds nothing registered instead of leaking the waker forever.
// Cost: O(1).
pub(in crate::runtime) fn unregister_supply_collector(collector_id: u64) {
    if let Ok(mut map) = supply_collector_map().lock() {
        map.remove(&collector_id);
    }
}
