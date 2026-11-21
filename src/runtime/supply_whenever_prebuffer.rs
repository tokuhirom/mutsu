//! A `whenever` in a `supply` block subscribes when it runs (#18524).
//!
//! mutsu collects a supply block's `whenever`s as subscription markers while
//! the block body runs and opens their sources only after the body has
//! returned (the marker loop of `native_supply_mut_unhooked`). A live
//! `Supplier` source that emits in between -- the body itself emitting into a
//! supplier it `whenever`ed, or a tap callback the body triggers -- had no
//! subscriber yet, so those values were lost. rakudo taps the source the
//! moment the `whenever` runs and delivers what arrives while the body is
//! still running once the body is done (a supply block runs one handler at a
//! time): Red's `ResultSeq.Supply` forwards insert events made during its
//! snapshot through exactly such a supplier.
//!
//! So when the `whenever` runs, a push sink starts collecting the source's
//! emissions, keyed by the whenever's id; the marker loop swaps it for the
//! real tap under the emit gate (no emission falls between them or reaches
//! both) and hands the collected values to the body after the block's own
//! tap is wired.

use crate::runtime::native_methods::{supplier_sink_register, supplier_sink_unregister};
use crate::value::Value;
use crate::value::waker::{ReactWaker, SinkEvent};
use std::collections::HashMap;
use std::sync::{Mutex, OnceLock};

struct Prebuffer {
    supplier_id: u64,
    sink_id: u64,
    waker: ReactWaker,
}

fn prebuffers() -> &'static Mutex<HashMap<u64, Prebuffer>> {
    static MAP: OnceLock<Mutex<HashMap<u64, Prebuffer>>> = OnceLock::new();
    MAP.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Start collecting what `supplier_id` emits for the `whenever` `whenever_id`
/// until [`take`] hands the values over.
// Cost: O(1) plus one sink registration.
pub(crate) fn start(whenever_id: u64, supplier_id: u64) {
    let waker = ReactWaker::new();
    let sink_id = supplier_sink_register(supplier_id, 0, &waker, false);
    if let Ok(mut map) = prebuffers().lock() {
        map.insert(
            whenever_id,
            Prebuffer {
                supplier_id,
                sink_id,
                waker,
            },
        );
    }
}

/// Stop collecting for `whenever_id` and return the values its source emitted
/// since [`start`], in order. The caller holds the emit gate while it also
/// registers the real tap, so nothing is emitted in between. Empty when no
/// collection was started for it.
// Cost: O(v), v = values collected.
pub(crate) fn take(whenever_id: u64) -> Vec<Value> {
    let Some(pre) = prebuffers().lock().ok().and_then(|mut map| map.remove(&whenever_id)) else {
        return Vec::new();
    };
    supplier_sink_unregister(pre.supplier_id, pre.sink_id);
    pre.waker
        .drain()
        .into_iter()
        .filter_map(|(_, event, _)| match event {
            SinkEvent::Emit(v) => Some(v),
            _ => None,
        })
        .collect()
}
