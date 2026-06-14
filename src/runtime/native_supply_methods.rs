use super::native_methods::*;
use super::*;
use crate::symbol::Symbol;

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
        !matches!(callback, Value::Nil)
    }

    /// Invoke a done callback. If the callback is a WheneverDoneGroup marker,
    /// decrement the group counter and only call the real done callback when
    /// all whenevers are done. Otherwise, call the callback directly.
    pub(super) fn invoke_done_callback(&mut self, done_cb: Value) -> Result<(), RuntimeError> {
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &done_cb
            && *class_name == "__WheneverDoneGroup"
            && let Some(Value::Int(group_id)) = attributes.as_map().get("group_id")
        {
            if let Some(real_done_cb) = whenever_done_group_decrement(*group_id as u64) {
                let _ = self.call_sub_value(real_done_cb, vec![], true);
            }
            return Ok(());
        }
        // A close marker fires the supply's CLOSE-phaser callbacks (registered
        // on the emitter) when the supply terminates normally. Taking them
        // gives run-once across normal termination and an explicit tap close.
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &done_cb
            && *class_name == "__SupplyCloseMarker"
            && let Some(Value::Int(cid)) = attributes.as_map().get("close_supplier_id")
        {
            for cb in take_supplier_close_callbacks(*cid as u64) {
                self.call_sub_value(cb, vec![], true)?;
            }
            return Ok(());
        }
        // When an on-demand supply with `whenever`s completes via `done` (an
        // explicit `done` in the block or a `done` inside a whenever body), the
        // whole supply finishes: each whenever source's on-close callbacks run
        // and the downstream `done` handler fires.
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &done_cb
            && *class_name == "__SupplyOnDemandComplete"
        {
            let attrs = attributes.as_map();
            if let Some(Value::Array(on_close, ..)) = attrs.get("on_close") {
                for cb in on_close.iter().cloned().collect::<Vec<_>>() {
                    self.call_sub_value(cb, vec![], true)?;
                }
            }
            if let Some(down) = attrs.get("done_cb")
                && Self::supply_has_active_callback(down)
            {
                let _ = self.call_sub_value(down.clone(), vec![], true);
            }
            return Ok(());
        }
        let _ = self.call_sub_value(done_cb, Vec::new(), true);
        Ok(())
    }

    /// Marker registered on the emitter's done so that a supply terminating via
    /// `done` fires every whenever source's on-close callbacks plus the
    /// downstream done handler.
    pub(super) fn make_on_demand_complete_marker(
        done_cb: Option<Value>,
        on_close: Vec<Value>,
    ) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("on_close".to_string(), Value::array(on_close));
        if let Some(cb) = done_cb {
            attrs.insert("done_cb".to_string(), cb);
        }
        Value::make_instance(Symbol::intern("__SupplyOnDemandComplete"), attrs)
    }

    /// Marker registered as a done callback so the emitter's CLOSE-phaser
    /// callbacks fire when the supply terminates normally.
    pub(super) fn make_supply_close_marker(close_supplier_id: u64) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert(
            "close_supplier_id".to_string(),
            Value::Int(close_supplier_id as i64),
        );
        Value::make_instance(Symbol::intern("__SupplyCloseMarker"), attrs)
    }

    /// Create a WheneverDoneGroup marker Value for registering as a done
    /// callback on inner suppliers.
    pub(super) fn make_whenever_done_group_marker(group_id: u64) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("group_id".to_string(), Value::Int(group_id as i64));
        Value::make_instance(Symbol::intern("__WheneverDoneGroup"), attrs)
    }

    pub(super) fn runtime_error_from_supply_reason(reason: Value) -> RuntimeError {
        let message = reason.to_string_value();
        let mut err = RuntimeError::new(message);
        err.exception = Some(Box::new(reason));
        err
    }

    pub(super) fn supply_is_terminated(attributes: &HashMap<String, Value>) -> bool {
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

    pub(super) fn call_supply_quit_handler(
        &mut self,
        quit_cb: Value,
        reason: Value,
    ) -> Result<(), RuntimeError> {
        let saved_when = self.when_matched();
        self.set_when_matched(false);
        let handled = match self.call_sub_value(quit_cb, vec![reason.clone()], true) {
            Ok(_) => true,
            Err(err) if err.is_succeed => true,
            Err(err) => {
                self.set_when_matched(saved_when);
                return Err(err);
            }
        };
        self.set_when_matched(saved_when);
        if handled {
            Ok(())
        } else {
            Err(Self::runtime_error_from_supply_reason(reason))
        }
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
        let done_before = supplier_done_count();
        let result = self.call_sub_value(quit_cb, vec![reason], true);
        let matched = self.when_matched();
        self.set_when_matched(saved_when);
        // If the phaser called `done` (it ran `$emitter.done()`), the supply
        // was already completed via the emitter — don't let the caller fire the
        // downstream done a second time. The `done` rewrite returns from the
        // phaser sub normally, so this is the reliable signal.
        if supplier_done_count() > done_before {
            return QuitOutcome::HandledViaDone;
        }
        match result {
            Ok(_) if matched => QuitOutcome::Handled,
            Ok(_) => QuitOutcome::Unhandled,
            Err(err) if err.is_react_done || err.is_succeed => QuitOutcome::Handled,
            Err(_) => QuitOutcome::Unhandled,
        }
    }
}
