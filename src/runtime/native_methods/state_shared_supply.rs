//! Start-up state of `.share`d on-demand supplies.
//!
//! `supply { ... }.share` runs its block once, when `.share` is called, with
//! the shared supplier as the block's emitter; every tap joins that one run
//! (#10839 -- it used to start on the first tap). The one `"tap"` that runs
//! the block is the one that claims the start-up here, keyed on the shared
//! supplier id rather than on the Supply's own attributes: a copy of the
//! Supply (a derived `grep`/`map`/`head`, a `whenever` source, a react
//! subscription) never saw an attribute flag (#10740), and claiming under one
//! lock keeps the start-up to exactly one run.

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

impl Interpreter {
    /// Start a `.share`d on-demand supply: run its block once, on the shared
    /// supplier, with no consumer attached. Called by `Supply.share` itself,
    /// as raku's `share` taps its source on the spot; every later consumer
    /// (a direct tap, a derived `grep`/`map`/`head`, a `whenever`, a react)
    /// then joins the running block through the shared supplier. A block
    /// that dies while starting quits the shared supplier rather than
    /// throwing from `.share` (raku's share taps with a `quit` handler).
    // Cost: one run of the shared block's body.
    pub(crate) fn start_shared_supply(&mut self, attrs: &AttrMap) -> Result<(), RuntimeError> {
        // The canonical tap claims the start-up itself (see
        // `shared_supply_claim_start`), so the block runs exactly once.
        self.native_supply_mut(
            attrs.clone(),
            "tap",
            Vec::new(),
            &mut super::AttrPublisher::detached(),
        )?;
        Ok(())
    }
}
