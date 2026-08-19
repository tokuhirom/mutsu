use super::native_methods::*;
use super::*;
use crate::symbol::Symbol;
use crate::value::AttrMap;
use crate::value::ValueView;

/// How a `whenever` QUIT phaser handled (or did not handle) an exception.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum QuitOutcome {
    /// Nothing matched / it rethrew — the quit propagates downstream.
    Unhandled,
    /// Matched, but did not call `done`; the caller completes the supply.
    Handled,
    /// Called `done`, which already completed the supply via the emitter.
    HandledViaDone,
}

impl Interpreter {
    pub(super) fn supply_has_active_callback(callback: &Value) -> bool {
        !callback.is_nil()
    }

    /// Invoke a done callback. If the callback is a WheneverDoneGroup marker,
    /// decrement the group counter and only call the real done callback when
    /// all whenevers are done. Otherwise, call the callback directly.
    pub(super) fn invoke_done_callback(&mut self, done_cb: Value) -> Result<(), RuntimeError> {
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = done_cb.view()
            && class_name == "__WheneverDoneGroup"
            && let Some(ValueView::Int(group_id)) =
                attributes.as_map().get("group_id").map(|v| v.view())
        {
            if let Some(real_done_cb) = whenever_done_group_decrement(group_id as u64) {
                // The group's stored callback may itself be a marker (a chained
                // on-demand whenever passes the outer group's marker as the
                // inner tap's done), so dispatch recursively.
                self.invoke_done_callback(real_done_cb)?;
            }
            return Ok(());
        }
        // A done chain bundles several done callbacks (e.g. a whenever's LAST
        // phaser plus the enclosing supply's done-group marker) into the single
        // `done => ...` slot of a chained inner tap. Fire each in order.
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = done_cb.view()
            && class_name == "__SupplyDoneChain"
        {
            if let Some(ValueView::Array(cbs, ..)) =
                attributes.as_map().get("callbacks").map(|v| v.view())
            {
                for cb in cbs.iter().cloned().collect::<Vec<_>>() {
                    self.invoke_done_callback(cb)?;
                }
            }
            return Ok(());
        }
        // A close marker fires the supply's CLOSE-phaser callbacks (registered
        // on the emitter) when the supply terminates normally. Taking them
        // gives run-once across normal termination and an explicit tap close.
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = done_cb.view()
            && class_name == "__SupplyCloseMarker"
            && let Some(ValueView::Int(cid)) = attributes
                .as_map()
                .get("close_supplier_id")
                .map(|v| v.view())
        {
            for cb in take_supplier_close_callbacks(cid as u64) {
                self.call_sub_value(cb, vec![], true)?;
            }
            return Ok(());
        }
        // When an on-demand supply with `whenever`s completes via `done` (an
        // explicit `done` in the block or a `done` inside a whenever body), the
        // whole supply finishes: each whenever source's on-close callbacks run
        // and the downstream `done` handler fires.
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = done_cb.view()
            && class_name == "__SupplyOnDemandComplete"
        {
            let attrs = attributes.as_map();
            if let Some(ValueView::Array(on_close, ..)) = attrs.get("on_close").map(|v| v.view()) {
                for cb in on_close.iter().cloned().collect::<Vec<_>>() {
                    self.call_sub_value(cb, vec![], true)?;
                }
            }
            // `done` inside a `whenever` body ends the enclosing supply the
            // same way an explicit `Supplier.done` does: tear down the
            // `whenever`s' upstream subscriptions so the source stops
            // reaching this (now-complete) block's body for later emits.
            if let Some(upstream_taps) = attrs.get("upstream_taps") {
                self.close_upstream_taps(upstream_taps)?;
            }
            if let Some(down) = attrs.get("done_cb")
                && Self::supply_has_active_callback(down)
            {
                let _ = self.call_sub_value(down.clone(), vec![], true);
            }
            return Ok(());
        }
        self.call_sub_value(done_cb, Vec::new(), true)?;
        Ok(())
    }

    /// Like `invoke_done_callback`, but a die escaping the callback (the
    /// established shape: a `whenever`'s `LAST` phaser body throwing, e.g.
    /// `Cro::HTTP::RawBodyParser::ContentLength`'s "connection closed too
    /// soon" check) routes to `supplier_id`'s own quit callbacks instead of
    /// propagating out of the `.done()`/emit-completion call that triggered
    /// it. Mirrors the established whenever-body-emit die-to-quit
    /// conversion in the `"emit"` arm of `native_supplier_methods.rs`
    /// (`SupplierEmitAction::Call`) byte-for-byte, including its silent-drop
    /// behavior when nothing registered a `quit =>` handler — real Raku
    /// terminates the supply via quit either way, but mutsu's existing
    /// contract (already exercised by that sibling site) is that an
    /// unobserved quit is simply not delivered anywhere, not resurfaced as
    /// an error from the unrelated call that happened to trigger done.
    ///
    /// Returns `true` when the callback died and was converted to a quit —
    /// the caller's `take_supplier_done_callbacks(supplier_id)` loop must
    /// stop delivering the rest of that batch then (e.g. the enclosing
    /// whenever-done-group marker that would otherwise still fire the
    /// downstream `done =>` handler right after): a supply terminates via
    /// either `done` or `quit`, never both.
    pub(super) fn invoke_done_callback_or_quit(
        &mut self,
        done_cb: Value,
        supplier_id: u64,
    ) -> Result<bool, RuntimeError> {
        if let Err(err) = self.invoke_done_callback(done_cb) {
            // A `return` inside the callback targets its lexically enclosing
            // routine: propagate unchanged, not a supply failure.
            if err.is_return() || err.return_value.is_some() {
                return Err(err);
            }
            // `done`/`last` inside a whenever body ends the enclosing supply;
            // propagate the control signal unchanged so the supply machinery
            // consumes it.
            if err.is_react_done() || err.is_last() || err.is_supply_body_done() {
                return Err(err);
            }
            // `next` skips the rest of this body run — not a supply failure.
            if err.is_next() {
                return Ok(false);
            }
            let reason = err
                .exception
                .as_deref()
                .cloned()
                .unwrap_or_else(|| Value::str(err.message.clone()));
            // ADR-0031: the downstream tap's quit => handler for a
            // whenever-subscribed source lives on the enclosing supply
            // block's emitter (Decision A), not on this source's own
            // `supplier_id` — reach it via the serialize-group link, same
            // as the `Supplier."quit"` unhandled-QUIT-phaser arm.
            for qcb in take_supplier_quit_callbacks_via_group(supplier_id) {
                self.call_supply_quit_handler(qcb, reason.clone())?;
            }
            return Ok(true);
        }
        Ok(false)
    }

    /// Marker registered on the emitter's done so that a supply terminating via
    /// `done` fires every whenever source's on-close callbacks plus the
    /// downstream done handler.
    pub(super) fn make_on_demand_complete_marker(
        done_cb: Option<Value>,
        on_close: Vec<Value>,
        upstream_taps: Vec<Value>,
    ) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("on_close".to_string(), Value::array(on_close));
        if let Some(cb) = done_cb {
            attrs.insert("done_cb".to_string(), cb);
        }
        if !upstream_taps.is_empty() {
            attrs.insert("upstream_taps".to_string(), Value::array(upstream_taps));
        }
        Value::make_instance(Symbol::intern("__SupplyOnDemandComplete"), attrs)
    }

    /// Env key through which a whenever body learns the done group of its
    /// enclosing supply block, so a nested `whenever` registered at dispatch
    /// time (inside the body) can join the group and keep the supply open.
    pub(super) const WHENEVER_DONE_GROUP_ENV_KEY: &'static str = "__mutsu_whenever_done_group";

    /// Env key naming the emitter of the `supply` block a callback was written
    /// in, so [`Interpreter::call_supply_tap`] can make that emitter the
    /// innermost dynamically active one while the callback runs.
    ///
    /// The captured env alone cannot answer this. A callback captures the whole
    /// live env, so when an inner `supply` block's body runs *inside* an outer
    /// supply's `whenever` body, the inner block's callbacks capture both
    /// blocks' `__mutsu_supply_emitter_N` bindings — and picking one by scanning
    /// for the prefix is a `HashMap`-order lottery. Recorded here at callback
    /// creation time, where `active_supply_emitters.last()` is unambiguously the
    /// enclosing block.
    pub(crate) const WHENEVER_EMITTER_ENV_KEY: &'static str = "__mutsu_whenever_emitter";

    /// Return a copy of `sub` whose captured env additionally binds `key` to
    /// `val` (CoW — the original sub and its env are untouched). Non-Sub values
    /// pass through unchanged.
    pub(super) fn sub_with_env_key(sub: &Value, key: &str, val: Value) -> Value {
        if let Some(data) = sub.as_sub() {
            let mut new_data = data.clone();
            new_data.env.insert(key.to_string(), val);
            Value::from_sub_data(new_data)
        } else {
            sub.clone()
        }
    }

    /// Bundle several done callbacks into one value for the single `done =>`
    /// slot of a chained inner tap. `invoke_done_callback` fires each in order.
    pub(super) fn make_supply_done_chain(callbacks: Vec<Value>) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("callbacks".to_string(), Value::array(callbacks));
        Value::make_instance(Symbol::intern("__SupplyDoneChain"), attrs)
    }

    /// Wrap the real outer tap callback with a Supply's `do_callbacks` chain,
    /// so a `.do($cb)` derived from an on-demand source still runs `$cb` for
    /// values delivered *asynchronously* through a nested `whenever` — not
    /// just ones the body `emit`s synchronously (those already go through the
    /// `do_cbs` loop directly). `call_supply_tap` unwraps this marker before
    /// invoking the callback.
    pub(super) fn make_supply_do_wrapped_tap(do_callbacks: Vec<Value>, real_tap: Value) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("do_callbacks".to_string(), Value::array(do_callbacks));
        attrs.insert("real_tap".to_string(), real_tap);
        Value::make_instance(Symbol::intern("__SupplyDoWrappedTap"), attrs)
    }

    /// Register `tap_cb` as `emitter_supplier_id`'s outer subscriber, wrapping
    /// it with `attrs`'s `do_callbacks` first when present (see
    /// `make_supply_do_wrapped_tap`).
    pub(super) fn register_outer_tap_with_do_callbacks(
        attrs: &AttrMap,
        emitter_supplier_id: u64,
        tap_cb: &Value,
        delay_seconds: f64,
    ) {
        let registered = match attrs.get("do_callbacks").map(Value::view) {
            Some(ValueView::Array(cbs, ..)) if !cbs.is_empty() => {
                Self::make_supply_do_wrapped_tap(cbs.to_vec(), tap_cb.clone())
            }
            _ => tap_cb.clone(),
        };
        register_supplier_tap(emitter_supplier_id, registered, delay_seconds);
    }

    /// Marker registered as a done callback so the emitter's CLOSE-phaser
    /// callbacks fire when the supply terminates normally.
    pub(super) fn make_supply_close_marker(close_supplier_id: u64) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert(
            "close_supplier_id".to_string(),
            Value::int(close_supplier_id as i64),
        );
        Value::make_instance(Symbol::intern("__SupplyCloseMarker"), attrs)
    }

    /// Create a WheneverDoneGroup marker Value for registering as a done
    /// callback on inner suppliers.
    pub(super) fn make_whenever_done_group_marker(group_id: u64) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("group_id".to_string(), Value::int(group_id as i64));
        Value::make_instance(Symbol::intern("__WheneverDoneGroup"), attrs)
    }

    pub(crate) fn runtime_error_from_supply_reason(reason: Value) -> RuntimeError {
        let message = reason.to_string_value();
        let mut err = RuntimeError::new(message);
        err.exception = Some(Box::new(reason));
        err
    }

    pub(super) fn supply_is_terminated(attributes: &AttrMap) -> bool {
        supplier_id_from_attrs(attributes)
            .map(|supplier_id| {
                let (_, done, quit_reason) = supplier_snapshot(supplier_id);
                done || quit_reason.is_some()
            })
            .unwrap_or_else(|| {
                attributes.get("done").map(Value::truthy).unwrap_or(false)
                    || attributes.contains_key("quit_reason")
            })
    }

    /// Run a single `whenever` QUIT phaser body with `reason` bound as `$_`,
    /// reporting how it handled the exception:
    /// - `Unhandled`: nothing matched / it rethrew — the quit propagates.
    /// - `Handled`: a `when`/`default` matched (or `succeed`) but it did not
    ///   call `done`, so the caller still completes the supply with done.
    /// - `HandledViaDone`: it called `done`, which rewrites to
    ///   `$emitter.done()` + return — the emitter completion already fired the
    ///   downstream done, so the caller must NOT fire it again.
    pub(super) fn run_whenever_quit_phaser(
        &mut self,
        quit_cb: Value,
        reason: Value,
    ) -> QuitOutcome {
        let saved_when = self.when_matched();
        self.set_when_matched(false);
        // Per-thread, not per-process: the phaser body runs synchronously here,
        // so only a `done` raised on this thread can be its doing.
        let done_before = thread_supplier_done_count();
        let result = self.call_sub_value(quit_cb, vec![reason], true);
        let matched = self.when_matched();
        self.set_when_matched(saved_when);
        // If the phaser called `done` (it ran `$emitter.done()`), the supply
        // was already completed via the emitter — don't let the caller fire the
        // downstream done a second time. The `done` rewrite returns from the
        // phaser sub normally, so this is the reliable signal.
        if thread_supplier_done_count() > done_before {
            return QuitOutcome::HandledViaDone;
        }
        match result {
            Ok(_) if matched => QuitOutcome::Handled,
            Ok(_) => QuitOutcome::Unhandled,
            Err(err) if err.is_react_done() || err.is_succeed() => QuitOutcome::Handled,
            Err(_) => QuitOutcome::Unhandled,
        }
    }
}
