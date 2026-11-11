//! `Supply.tap(..., :tap(&cb))`: `cb` receives the `Tap` before the supply
//! starts producing (#16888, EEL 0.3's "immediate cancellation" checks).
//!
//! Rakudo creates the `Tap` first, hands it to the `tap` callback and only
//! then runs the supply; a supply block whose tap was closed in that callback
//! never runs its body. mutsu builds its `Tap` handle after registering the
//! subscription, so the callback gets a provisional handle: closing it before
//! the supply starts is recorded under the handle's id, and the real handle's
//! attributes are moved into it afterwards, so the object the callback saw is
//! the one `.tap` returns and later closes the subscription.
//!
//! A supply that is not a supply block (`(1, 2).Supply`, a `Supplier`'s)
//! emits as rakudo's does whatever the callback did; its subscription is
//! closed once registered.
//!
//! A supply over a user `Tappable` (`Supply.new($tappable)`) hands the
//! callback on instead: the tappable's own `tap(&emit, &done, &quit, &tap)`
//! decides when to call it and with which `Tap` (EEL's `MatcherSupply`). The
//! callback waits on a per-thread stack while the tap runs; the adapter's
//! `&tap` (`supply_tappable`) takes it with `__mutsu_supply_tapped`.

use super::*;
use crate::symbol::Symbol;
use crate::value::AttrMap;
use std::collections::HashSet;
use std::sync::Mutex;
use std::sync::atomic::{AtomicU64, Ordering};

/// The attribute naming a provisional handle.
const PENDING_TAP_ID: &str = "pending_tap_id";

static NEXT_PENDING_TAP: AtomicU64 = AtomicU64::new(1);
/// Provisional handles closed before their supply started.
static CLOSED_BEFORE_START: Mutex<Option<HashSet<u64>>> = Mutex::new(None);

thread_local! {
    /// The `:tap` callbacks of the `Tappable` supplies being tapped on this
    /// thread, innermost last; a taken one is `Nil`.
    static TAPPABLE_HOOKS: std::cell::RefCell<Vec<Value>> = const { std::cell::RefCell::new(Vec::new()) };
    /// The `Tap` each `Tappable` tapped on this thread answered, innermost
    /// last (`Nil` until it answers).
    static TAPPABLE_TAPS: std::cell::RefCell<Vec<Value>> = const { std::cell::RefCell::new(Vec::new()) };
}

/// Pops the callback pushed for a `Tappable` tap, however the tap ends.
struct TappableHookGuard;

impl Drop for TappableHookGuard {
    fn drop(&mut self) {
        TAPPABLE_HOOKS.with(|hooks| hooks.borrow_mut().pop());
        TAPPABLE_TAPS.with(|taps| taps.borrow_mut().pop());
    }
}

/// Record that the provisional handle in `attributes`, if it is one, was
/// closed. `true` when it was.
// Cost: O(1).
pub(crate) fn note_pending_tap_close(attributes: &AttrMap) -> bool {
    let Some(ValueView::Int(id)) = attributes.get(PENDING_TAP_ID).map(Value::view) else {
        return false;
    };
    CLOSED_BEFORE_START
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .get_or_insert_with(HashSet::new)
        .insert(id as u64);
    true
}

/// Whether the provisional handle `id` was closed, forgetting it.
// Cost: O(1).
fn take_pending_tap_close(id: u64) -> bool {
    CLOSED_BEFORE_START
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .as_mut()
        .is_some_and(|set| set.remove(&id))
}

impl Interpreter {
    /// Whether this `.tap` / `.act` takes the route here: it is given a `:tap`
    /// callback, or taps a supply over a user `Tappable`.
    // Cost: O(n), n = arguments.
    pub(super) fn has_tap_hook(attrs: &AttrMap, method: &str, args: &[Value]) -> bool {
        matches!(method, "tap" | "act")
            && (attrs.contains_key(TAPPABLE_SUPPLY)
                || Self::named_value(args, "tap")
                    .is_some_and(|h| Self::supply_has_active_callback(&h)))
    }

    /// `.tap` / `.act` routed here by [`Self::has_tap_hook`]: a `:tap`
    /// callback runs on a provisional handle, then the supply is tapped
    /// without it.
    // Cost: O(a) plus the callback and the tap, a = the real handle's attributes.
    pub(super) fn supply_tap_with_hook(
        &mut self,
        attrs: AttrMap,
        method: &str,
        args: &[Value],
        publish: &mut crate::runtime::native_methods::AttrPublisher<'_>,
    ) -> Result<(Value, AttrMap), RuntimeError> {
        let hook = Self::named_value(args, "tap").unwrap_or(Value::NIL);
        let rest: Vec<Value> = args
            .iter()
            .filter(|a| !matches!(a.view(), ValueView::Pair(key, _) if key == "tap"))
            .cloned()
            .collect();
        self.supply_tap_hooked(attrs, method, rest, hook, publish)
    }

    /// Tap a supply over a user `Tappable`: its `&tap` hands the caller's
    /// `:tap` callback the Tap the tappable answers, and closing the Tap
    /// returned here closes that one too (EEL's `MatcherSupply` stops on it).
    // Cost: O(1) plus the tap.
    fn tappable_supply_tap(
        &mut self,
        attrs: AttrMap,
        method: &str,
        rest: Vec<Value>,
        hook: Value,
        publish: &mut crate::runtime::native_methods::AttrPublisher<'_>,
    ) -> Result<(Value, AttrMap), RuntimeError> {
        TAPPABLE_HOOKS.with(|hooks| hooks.borrow_mut().push(hook));
        TAPPABLE_TAPS.with(|taps| taps.borrow_mut().push(Value::NIL));
        let _guard = TappableHookGuard;
        let (tap, attrs) = self.native_supply_mut_unhooked(attrs, method, rest, publish)?;
        let answered = TAPPABLE_TAPS.with(|taps| taps.borrow().last().cloned());
        if let Some(answered) = answered
            && matches!(answered.view(), ValueView::Instance { .. })
            && let ValueView::Instance { attributes, .. } = tap.view()
        {
            let mut upstream = match attributes.to_map().get("upstream_taps").map(Value::view) {
                Some(ValueView::Array(items, ..)) => items.iter().cloned().collect::<Vec<_>>(),
                _ => Vec::new(),
            };
            upstream.push(answered);
            attributes.insert("upstream_taps", Value::array(upstream));
        }
        Ok((tap, attrs))
    }

    // Cost: as `supply_tap_with_hook`.
    fn supply_tap_hooked(
        &mut self,
        attrs: AttrMap,
        method: &str,
        rest: Vec<Value>,
        hook: Value,
        publish: &mut crate::runtime::native_methods::AttrPublisher<'_>,
    ) -> Result<(Value, AttrMap), RuntimeError> {
        if attrs.contains_key(TAPPABLE_SUPPLY) {
            return self.tappable_supply_tap(attrs, method, rest, hook, publish);
        }
        let id = NEXT_PENDING_TAP.fetch_add(1, Ordering::Relaxed);
        let mut handle_attrs = std::collections::HashMap::new();
        handle_attrs.insert(PENDING_TAP_ID.to_string(), Value::int(id as i64));
        let handle = Value::make_instance(Symbol::intern("Tap"), handle_attrs);
        let hooked = self.call_supply_callback(hook, vec![handle.clone()]);
        let closed = take_pending_tap_close(id);
        hooked?;
        let supply_block = Self::runs_a_supply_block(&attrs);
        if closed && supply_block {
            return Ok((handle, attrs));
        }
        let (tap, attrs) = self.native_supply_mut_unhooked(attrs, method, rest, publish)?;
        if let (ValueView::Instance { attributes: real, .. }, ValueView::Instance { attributes, .. }) =
            (tap.view(), handle.view())
        {
            for (key, value) in real.to_map() {
                attributes.insert(key, value);
            }
            // The handle is a real one now: a later close is not a close
            // before start.
            attributes.insert(PENDING_TAP_ID, Value::NIL);
        }
        if closed {
            self.tap_close(&tap_attr_map(&handle))?;
        }
        Ok((handle, attrs))
    }
}

/// The attribute marking a supply over a user `Tappable`.
pub(crate) const TAPPABLE_SUPPLY: &str = "tappable_supply";

impl Interpreter {
    /// `__mutsu_supply_tapped($tap)`: the `&tap` a `Tappable` is given calls
    /// the `:tap` callback of the tap in progress, once.
    // Cost: O(1) plus the callback.
    pub(crate) fn builtin_supply_tapped(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let hook = TAPPABLE_HOOKS.with(|hooks| {
            hooks
                .borrow_mut()
                .last_mut()
                .map(|slot| std::mem::replace(slot, Value::NIL))
        });
        if let Some(hook) = hook
            && Self::supply_has_active_callback(&hook)
        {
            let tap = args.first().cloned().unwrap_or(Value::NIL);
            self.call_supply_callback(hook, vec![tap])?;
        }
        Ok(Value::NIL)
    }

    /// `__mutsu_supply_tappable_tap($tap)`: the Tap a `Tappable`'s `tap`
    /// answered, for [`Self::tappable_supply_tap`] to close with its own.
    // Cost: O(1).
    pub(crate) fn builtin_supply_tappable_tap(args: &[Value]) -> Value {
        let tap = args.first().cloned().unwrap_or(Value::NIL);
        TAPPABLE_TAPS.with(|taps| {
            if let Some(slot) = taps.borrow_mut().last_mut() {
                *slot = tap;
            }
        });
        Value::NIL
    }

    /// Whether tapping the supply with `attrs` runs a `supply { }` block: it
    /// is one, or an on-demand stage (`.map`, `.grep`, ...) derived from one.
    // Cost: O(d), d = derived stages (bounded).
    fn runs_a_supply_block(attrs: &AttrMap) -> bool {
        let mut attrs = attrs.clone();
        for _ in 0..64 {
            if attrs
                .get("on_demand_callback")
                .is_some_and(Self::is_supply_block_producer)
            {
                return true;
            }
            let Some(ValueView::Instance { attributes, .. }) =
                attrs.get("derive_source").map(Value::view)
            else {
                return false;
            };
            attrs = attributes.to_map();
        }
        false
    }
}

/// A Tap instance's attributes as the map `tap_close` reads.
// Cost: O(a), a = attributes.
fn tap_attr_map(handle: &Value) -> AttrMap {
    match handle.view() {
        ValueView::Instance { attributes, .. } => attributes.to_map(),
        _ => AttrMap::default(),
    }
}
