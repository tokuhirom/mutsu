use super::native_methods::supply_channel::{SupplyReceiver, supply_event_channel};
use super::native_methods::{
    SupplyEvent, next_supply_id, supply_channel_map_pub, take_supply_channel,
};
use crate::symbol::Symbol;
use crate::value::{AttrMap, Value, ValueView};
use std::collections::HashMap;
use std::sync::mpsc;

/// Whether `attrs` describes a Supply fed by an event channel (a `signal()`
/// supply, a timer, a process output stream, ...) rather than by a Supplier or
/// by materialized values.
// Cost: O(1).
pub(super) fn is_channel_source(attrs: &AttrMap) -> bool {
    !attrs.contains_key("supplier_id")
        && matches!(
            attrs.get("supply_id").map(Value::view),
            Some(ValueView::Int(sid)) if super::native_methods::has_supply_channel(sid as u64)
        )
}

/// `Supply.merge` over sources of which at least one is channel-backed.
///
/// The merged supply is itself channel-backed. A pump on the worker pool
/// forwards each source's events as they arrive, so the merge stays live: it
/// is `done` when every source is, and `quit`s with the first source that
/// does. Materialized sources are emitted first, as the other merge paths do.
// Cost: O(1) to build; the pump spends O(1) per event, k = number of sources.
pub(super) fn create_channel_merge_supply(sources: &[AttrMap]) -> Value {
    enum Source {
        Channel(SupplyReceiver),
        Static(Vec<Value>),
    }
    let mut parts: Vec<Source> = Vec::with_capacity(sources.len());
    for attrs in sources {
        if is_channel_source(attrs)
            && let Some(ValueView::Int(sid)) = attrs.get("supply_id").map(Value::view)
            && let Some(rx) = take_supply_channel(sid as u64)
        {
            parts.push(Source::Channel(rx));
            continue;
        }
        let items = match attrs.get("values").map(Value::view) {
            Some(ValueView::Array(items, ..)) => items.to_vec(),
            _ => Vec::new(),
        };
        parts.push(Source::Static(items));
    }

    let merged_id = next_supply_id();
    let (tx, rx) = supply_event_channel();
    // The pump holds `Value`s (Gc nodes) and idles on a waker, parking at a GC
    // safepoint each round so a stop-the-world can proceed meanwhile.
    super::worker_pool::submit(move || {
        let mut done = vec![false; parts.len()];
        for (i, part) in parts.iter().enumerate() {
            if let Source::Static(vals) = part {
                for v in vals {
                    if tx.send(SupplyEvent::Emit(v.clone())).is_err() {
                        return;
                    }
                }
                done[i] = true;
            }
        }
        let waker = crate::value::waker::ReactWaker::new();
        for part in &parts {
            if let Source::Channel(rx) = part {
                rx.register_waker(&waker);
            }
        }
        loop {
            crate::gc::gc_park_point();
            let mut progressed = false;
            for (i, part) in parts.iter().enumerate() {
                let Source::Channel(rx) = part else { continue };
                while !done[i] {
                    match rx.try_recv() {
                        Ok(SupplyEvent::Emit(v)) => {
                            progressed = true;
                            if tx.send(SupplyEvent::Emit(v)).is_err() {
                                return;
                            }
                        }
                        Ok(SupplyEvent::Quit(reason)) => {
                            let _ = tx.send(SupplyEvent::Quit(reason));
                            return;
                        }
                        Ok(SupplyEvent::Done) | Err(mpsc::TryRecvError::Disconnected) => {
                            progressed = true;
                            done[i] = true;
                        }
                        Err(mpsc::TryRecvError::Empty) => break,
                    }
                }
            }
            if done.iter().all(|d| *d) {
                let _ = tx.send(SupplyEvent::Done);
                return;
            }
            if !progressed {
                waker.wait_activity(std::time::Duration::from_millis(100));
            }
        }
    });

    if let Ok(mut map) = supply_channel_map_pub().lock() {
        map.insert(merged_id, rx);
    }
    let mut attrs = HashMap::new();
    attrs.insert("values".to_string(), Value::array(Vec::new()));
    attrs.insert("taps".to_string(), Value::array(Vec::new()));
    attrs.insert("supply_id".to_string(), Value::int(merged_id as i64));
    attrs.insert("live".to_string(), Value::FALSE);
    Value::make_instance(Symbol::intern("Supply"), attrs)
}
