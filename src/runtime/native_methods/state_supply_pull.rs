//! The receivers behind a lazy `.list` / `.Seq` of a supply backed directly by
//! a native event channel (`Supply.interval`, an `IO::Socket::Async` read
//! stream, ...): `pull_id -> SupplyReceiver`, so a `LazyList` in
//! `CatPullMode::SupplyReceive` mode, which can only carry a `Value`, finds
//! the receiver it pulls from by an integer id (#17428).
//!
//! An entry is dropped once its stream ends (done, quit or the sender gone).
//! A list abandoned before that keeps its subscriber, as Rakudo's tap of the
//! supply stays open after a `last`.

use super::SupplyEvent;
use super::supply_channel::SupplyReceiver;
use crate::runtime::HashMap;
use crate::value::Value;
use std::sync::{Arc, Mutex, OnceLock};

type PullMap = Mutex<HashMap<u64, Arc<Mutex<SupplyReceiver>>>>;

fn pull_map() -> &'static PullMap {
    static MAP: OnceLock<PullMap> = OnceLock::new();
    MAP.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Register `rx` for lazy pulling; the id goes into the list's `cat` slot.
// Cost: O(1) amortized.
pub(in crate::runtime) fn register_supply_pull(rx: SupplyReceiver) -> u64 {
    use std::sync::atomic::{AtomicU64, Ordering};
    static NEXT: AtomicU64 = AtomicU64::new(1);
    let id = NEXT.fetch_add(1, Ordering::Relaxed);
    pull_map()
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .insert(id, Arc::new(Mutex::new(rx)));
    id
}

/// The next element of the stream registered as `id`, blocking until the
/// supply emits: `Some(Ok(value))` for an emit, `Some(Err(reason))` for a
/// quit, `None` once the stream is done (or `id` is unknown).
// Cost: O(1) plus the wait for the next event.
pub(crate) fn supply_pull_next(id: u64) -> Option<Result<Value, Value>> {
    let rx = pull_map()
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .get(&id)
        .cloned()?;
    let event = rx.lock().unwrap_or_else(|e| e.into_inner()).recv();
    let finish = || {
        pull_map()
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .remove(&id);
    };
    match event {
        Ok(SupplyEvent::Emit(value)) => Some(Ok(value)),
        Ok(SupplyEvent::Quit(reason)) => {
            finish();
            Some(Err(reason))
        }
        Ok(SupplyEvent::Done) | Err(_) => {
            finish();
            None
        }
    }
}
