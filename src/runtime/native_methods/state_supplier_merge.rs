//! Done accounting for `Supply.merge` over live (Supplier-backed) sources.
//!
//! The *values* of a merge are forwarded by an ordinary forward tap on each
//! source (`register_supplier_forward_tap`, see `state_supplier`), which needs
//! no state of its own — every emission goes straight to the merge output
//! supplier. Only `done` needs bookkeeping: the merged Supply is done when
//! *every* source is done, not when the first one is, so each source has to be
//! counted off against the total. That is what this module tracks, in the same
//! shape as the zip state next door.

use std::collections::HashMap;
use std::sync::OnceLock;
use std::sync::atomic::{AtomicU64, Ordering};

struct MergeState {
    done_count: usize,
    source_count: usize,
    output_supplier_id: u64,
}

type MergeStateMap = std::sync::Mutex<HashMap<u64, MergeState>>;

fn merge_state_map() -> &'static MergeStateMap {
    static MAP: OnceLock<MergeStateMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

/// Which merge states each source supplier participates in. Kept beside the
/// tap subscriptions rather than inside them because a merge source is an
/// ordinary forward tap — it needs no per-emit state, only this done tally.
type MergeSourceMap = std::sync::Mutex<HashMap<u64, Vec<u64>>>;

fn merge_source_map() -> &'static MergeSourceMap {
    static MAP: OnceLock<MergeSourceMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

fn next_merge_state_id() -> u64 {
    static COUNTER: AtomicU64 = AtomicU64::new(1);
    COUNTER.fetch_add(1, Ordering::Relaxed)
}

/// Start tracking a merge whose output is `output_supplier_id` and which has
/// `source_count` live sources to hear `done` from.
pub(in crate::runtime) fn register_merge_state(
    source_count: usize,
    output_supplier_id: u64,
) -> u64 {
    let id = next_merge_state_id();
    if let Ok(mut map) = merge_state_map().lock() {
        map.insert(
            id,
            MergeState {
                done_count: 0,
                source_count,
                output_supplier_id,
            },
        );
    }
    id
}

/// Record that `source_supplier_id` is one of the merge's live sources.
pub(in crate::runtime) fn register_merge_source(source_supplier_id: u64, merge_state_id: u64) {
    if let Ok(mut map) = merge_source_map().lock() {
        map.entry(source_supplier_id)
            .or_default()
            .push(merge_state_id);
    }
}

/// The merge states `supplier_id` feeds, for the `done` propagation to walk.
pub(in crate::runtime) fn get_supplier_merge_state_ids(supplier_id: u64) -> Vec<u64> {
    if let Ok(map) = merge_source_map().lock() {
        return map.get(&supplier_id).cloned().unwrap_or_default();
    }
    Vec::new()
}

/// Count one source off. Returns the merge's output supplier once *every*
/// source has finished, and `None` while any source is still live.
pub(in crate::runtime) fn merge_source_done(merge_state_id: u64) -> Option<u64> {
    if let Ok(mut map) = merge_state_map().lock()
        && let Some(state) = map.get_mut(&merge_state_id)
    {
        state.done_count += 1;
        if state.done_count >= state.source_count {
            return Some(state.output_supplier_id);
        }
    }
    None
}
