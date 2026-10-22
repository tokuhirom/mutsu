//! The subscriptions a `supply { ... }` block opened from inside its own
//! `whenever` bodies, keyed by the block's emitter supplier id.
//!
//! A `whenever` written directly in the block body is tapped when the block
//! is tapped, and that tap's `upstream_taps` list records it. A `whenever`
//! written inside another `whenever`'s body runs much later, from whatever
//! thread delivered the value, and its subscription used to be recorded
//! nowhere: closing the block's tap could not reach it, so its own `CLOSE`
//! phasers never ran (#12864). Rakudo keeps such taps in the block's
//! `SupplyBlockState.active` set and closes every one when the block is
//! closed or completes; this registry is that set. An entry leaves the set
//! when its own source completes, so a long-lived block (a server's listener
//! block opening one subscription per connection) does not accumulate the
//! taps of finished subscriptions.

use crate::runtime::{Interpreter, RuntimeError};
use crate::symbol::Symbol;
use crate::value::{Value, ValueView};
use std::collections::HashMap;
use std::sync::{Mutex, OnceLock};

/// Emitter supplier id → the live child taps, as `(whenever id, Tap)` pairs
/// in registration order.
type ChildTapMap = Mutex<HashMap<u64, Vec<(u64, Value)>>>;

fn child_tap_map() -> &'static ChildTapMap {
    static MAP: OnceLock<ChildTapMap> = OnceLock::new();
    MAP.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Record `tap`, opened by the nested `whenever` `whenever_id`, as a child of
/// the supply block whose emitter is `emitter_supplier_id`.
// Cost: O(1) amortized.
pub(in crate::runtime) fn register_supply_child_tap(
    emitter_supplier_id: u64,
    whenever_id: u64,
    tap: Value,
) {
    if let Ok(mut map) = child_tap_map().lock() {
        map.entry(emitter_supplier_id)
            .or_default()
            .push((whenever_id, tap));
    }
}

/// Drop the child entry of `whenever_id` once its source has completed: there
/// is nothing left to close.
// Cost: O(c), c = live child taps of this block.
fn forget_supply_child_tap(emitter_supplier_id: u64, whenever_id: u64) {
    if let Ok(mut map) = child_tap_map().lock()
        && let Some(children) = map.get_mut(&emitter_supplier_id)
    {
        children.retain(|(id, _)| *id != whenever_id);
        if children.is_empty() {
            map.remove(&emitter_supplier_id);
        }
    }
}

/// Take every live child tap of the block, in the order they were opened.
/// (Rakudo's active set is a hash, so it promises no order among them.)
// Cost: O(c), c = live child taps of this block.
fn take_supply_child_taps(emitter_supplier_id: u64) -> Vec<Value> {
    let children = child_tap_map()
        .lock()
        .ok()
        .and_then(|mut map| map.remove(&emitter_supplier_id))
        .unwrap_or_default();
    children.into_iter().map(|(_, tap)| tap).collect()
}

/// Enumerate the registry's `Value`s for the collector: a child Tap handle is
/// reachable only from here while its block is live.
pub(in crate::runtime) fn visit_supply_child_tap_roots(visitor: &mut dyn crate::gc::RootVisitor) {
    if let Ok(map) = child_tap_map().lock() {
        for children in map.values() {
            for (_, tap) in children {
                visitor.visit_value(tap);
            }
        }
    }
}

impl Interpreter {
    /// Done marker for a `whenever` registered from inside another
    /// `whenever`'s body: its source completing removes its tap from the
    /// enclosing block's child set.
    // Cost: O(1).
    pub(in crate::runtime) fn make_supply_child_tap_done_marker(
        emitter_supplier_id: u64,
        whenever_id: u64,
    ) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert(
            "emitter_supplier_id".to_string(),
            Value::int(emitter_supplier_id as i64),
        );
        attrs.insert("whenever_id".to_string(), Value::int(whenever_id as i64));
        Value::make_instance(Symbol::intern("__SupplyChildTapDone"), attrs)
    }

    /// Handle `done_cb` if it is a [`Self::make_supply_child_tap_done_marker`]
    /// marker; `false` for any other done callback.
    // Cost: O(c), c = live child taps of the enclosing block.
    pub(in crate::runtime) fn forget_supply_child_tap_on_done(done_cb: &Value) -> bool {
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = done_cb.view()
        else {
            return false;
        };
        if class_name != "__SupplyChildTapDone" {
            return false;
        }
        let attrs = attributes.as_map();
        if let (Some(sid), Some(wid)) = (
            attrs.get("emitter_supplier_id").and_then(Value::as_int),
            attrs.get("whenever_id").and_then(Value::as_int),
        ) {
            forget_supply_child_tap(sid as u64, wid as u64);
        }
        true
    }

    /// Close every subscription the supply block whose emitter is
    /// `emitter_supplier_id` opened from inside its `whenever` bodies, so
    /// their own `CLOSE` phasers run before the block's. Called where the
    /// block ends: its tap closed, or the block completing.
    // Cost: O(c) tap closes, c = live child taps of the block (each close
    // cascades through its own block).
    pub(in crate::runtime) fn close_supply_child_taps(
        &mut self,
        emitter_supplier_id: u64,
    ) -> Result<(), RuntimeError> {
        for tap in take_supply_child_taps(emitter_supplier_id) {
            if let ValueView::Instance {
                class_name,
                attributes,
                ..
            } = tap.view()
                && (class_name == "Tap" || class_name == "IO::Socket::Async::ListenSocket")
            {
                let attrs = attributes.as_map().clone();
                self.tap_close(&attrs)?;
            }
        }
        Ok(())
    }

    /// End the supply block whose emitter is `emitter_supplier_id`: its nested
    /// `whenever` taps close first, then its CLOSE phasers (and its sources'
    /// on-close callbacks) run. Taking the callbacks makes this run-once
    /// against an explicit `Tap.close` of the same block.
    // Cost: O(c + k), c = live child taps, k = close callbacks (each runs its
    // block).
    pub(crate) fn run_supply_close_callbacks(
        &mut self,
        emitter_supplier_id: u64,
    ) -> Result<(), RuntimeError> {
        self.close_supply_child_taps(emitter_supplier_id)?;
        for cb in super::take_supplier_close_callbacks(emitter_supplier_id) {
            self.call_sub_value(cb, vec![])?;
        }
        Ok(())
    }
}
