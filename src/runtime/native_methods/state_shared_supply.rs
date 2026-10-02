//! Start-up state of `.share`d on-demand supplies.
//!
//! `supply { ... }.share` runs its block once, on the first tap, with the
//! shared supplier as the block's emitter; every later tap joins that one
//! run. Whether the block already ran used to live in the shared Supply's
//! own attributes (`shared_started`), which only the receiver of a `.tap`
//! call wrote back: a tap through a copy of the Supply (a derived
//! `grep`/`map`/`head`, a `whenever` source, a react subscription) never saw
//! the flag, and those consumers bypassed the start-up entirely (#10740).
//! Keying the flag on the shared supplier id makes every consumer agree, and
//! claiming it under one lock makes the start-up run exactly once even when
//! two threads tap at the same time.

use crate::runtime::Interpreter;
use crate::runtime::RuntimeError;
use crate::value::AttrMap;
use std::collections::HashSet;
use std::sync::{Mutex, OnceLock};

fn started_set() -> &'static Mutex<HashSet<u64>> {
    static SET: OnceLock<Mutex<HashSet<u64>>> = OnceLock::new();
    SET.get_or_init(|| Mutex::new(HashSet::new()))
}

/// Claim the start-up of the shared supply whose supplier is `supplier_id`:
/// `true` for exactly one caller (the one that must run the block), `false`
/// for every caller after it.
// Cost: O(1).
pub(in crate::runtime) fn shared_supply_claim_start(supplier_id: u64) -> bool {
    started_set()
        .lock()
        .is_ok_and(|mut set| set.insert(supplier_id))
}

/// Whether the shared supply whose supplier is `supplier_id` already started.
// Cost: O(1).
fn shared_supply_started(supplier_id: u64) -> bool {
    started_set()
        .lock()
        .is_ok_and(|set| set.contains(&supplier_id))
}

impl Interpreter {
    /// Start a `.share`d on-demand supply that a consumer is about to read
    /// through its shared supplier without tapping it (a `whenever` source,
    /// a react subscription): run its block once, on the shared supplier,
    /// exactly as its first `.tap` would. The consumer registers on the
    /// supplier first, so the block's synchronous `emit`s reach it. Anything
    /// that is not a shared supply, or one that already started, is left
    /// alone.
    // Cost: O(1) when there is nothing to start; otherwise one run of the
    // shared block's body.
    pub(crate) fn start_shared_supply_if_pending(
        &mut self,
        attrs: &AttrMap,
    ) -> Result<(), RuntimeError> {
        if !attrs.contains_key("shared_on_demand") {
            return Ok(());
        }
        let Some(sid) = super::supplier_id_from_attrs(attrs) else {
            return Ok(());
        };
        if shared_supply_started(sid) {
            return Ok(());
        }
        // The canonical tap claims the start-up itself, so a concurrent
        // first tap still runs the block only once.
        self.native_supply_mut(
            attrs.clone(),
            "tap",
            Vec::new(),
            &mut super::AttrPublisher::detached(),
        )?;
        Ok(())
    }
}
