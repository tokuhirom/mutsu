//! Split out of native_supply_methods.rs. See that file for the shared
//! helpers and the `QuitOutcome` enum.
use super::native_methods::*;
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Phase A of the on-demand supply runtime, shared by `tap`/`act`, the react
    /// event loop, `await`/`.Promise`, and `supply_get_values`. Builds an emitter
    /// `Supplier` (with a `supplier_id` when `emitter_supplier_id` is `Some`), runs
    /// the on-demand body inside a fresh `supply_emit_buffer` frame, and returns
    /// the callback result, the emitted items, and whether the body itself ran
    /// `done` (tracked via the global supplier-done counter).
    pub(crate) fn run_on_demand_body(
        &mut self,
        on_demand_cb: Value,
        emitter_supplier_id: Option<u64>,
    ) -> (Result<Value, RuntimeError>, Vec<Value>, bool) {
        let emitter = Value::make_instance(Symbol::intern("Supplier"), {
            let mut a = HashMap::new();
            a.insert("emitted".to_string(), Value::array(Vec::new()));
            a.insert("done".to_string(), Value::Bool(false));
            if let Some(sid) = emitter_supplier_id {
                a.insert("supplier_id".to_string(), Value::Int(sid as i64));
            }
            a
        });
        self.supply_emit_buffer.push(Vec::new());
        let done_before = supplier_done_count();
        let result = self.call_sub_value(on_demand_cb, vec![emitter], false);
        let body_ran_done = supplier_done_count() > done_before;
        let emitted = self.supply_emit_buffer.pop().unwrap_or_default();
        (result, emitted, body_ran_done)
    }

    /// Extract source values from a Supply's attributes.
    pub(super) fn supply_get_values(
        &mut self,
        attributes: &HashMap<String, Value>,
    ) -> Result<Vec<Value>, RuntimeError> {
        if let Some(on_demand_cb) = attributes.get("on_demand_callback") {
            let (_, emitted, _) = self.run_on_demand_body(on_demand_cb.clone(), None);
            Ok(emitted)
        } else {
            Ok(match attributes.get("values") {
                Some(Value::Array(items, ..)) => items.to_vec(),
                _ => Vec::new(),
            })
        }
    }

    /// Implement Supply.Promise for on-demand supplies (supply { ... } blocks).
    /// Runs the supply body through a custom event loop so that async
    /// `whenever` subscriptions (e.g. Supply.interval) are properly handled.
    /// Keeps the promise with the last emitted value when done.
    pub(super) fn supply_promise_on_demand(
        &mut self,
        attributes: &HashMap<String, Value>,
        promise: &crate::value::SharedPromise,
    ) -> Result<(), RuntimeError> {
        use crate::runtime::native_methods::take_supply_channel;
        use std::time::Duration;

        let on_demand_cb = match attributes.get("on_demand_callback") {
            Some(cb) => cb.clone(),
            None => {
                promise.keep(Value::Nil, String::new(), String::new());
                return Ok(());
            }
        };

        // Create an emitter supplier with a supplier_id so emits are tracked
        let emitter_supplier_id = next_supplier_id();
        // Register the promise on the emitter supplier. When supplier_done()
        // fires, it will keep all pending promises before supplier_reset()
        // clears the state.
        supplier_register_promise(emitter_supplier_id, promise.clone());

        // Enter react-like context to collect whenever registrations
        let (cb_result, emitted, _) =
            self.run_on_demand_body(on_demand_cb, Some(emitter_supplier_id));

        if let Err(err) = cb_result
            && !err.is_react_done()
        {
            promise.break_with(
                err.exception
                    .as_deref()
                    .cloned()
                    .unwrap_or_else(|| Value::str(err.message.clone())),
                String::new(),
                String::new(),
            );
            return Ok(());
        }

        // Check if promise was already resolved (synchronous supply that called done)
        if promise.is_resolved() {
            return Ok(());
        }

        // Separate subscription registrations from plain emitted values
        let mut subscriptions = Vec::new();
        let mut plain_values = Vec::new();
        for item in emitted {
            if let Value::Array(ref arr, ..) = item
                && arr.len() == 4
                && matches!(&arr[0], Value::Instance { class_name, .. } if class_name == "Supply")
            {
                subscriptions.push(item);
            } else {
                plain_values.push(item);
            }
        }

        if subscriptions.is_empty() {
            // No async subscriptions, just use plain emitted values
            let result = plain_values.last().cloned().unwrap_or(Value::Nil);
            promise.keep(result, String::new(), String::new());
            return Ok(());
        }

        // Build channel receivers for async subscriptions. Static (finite,
        // channel-less) sources such as `Supply.from-list(...)` have no live
        // channel; replay them synchronously here, running the body then the
        // LAST phaser (or the QUIT phaser if forcing/iterating the source
        // dies) and capturing emitted values. This makes
        // `await (supply { whenever Supply.from-list(...) { ... } })` resolve
        // with the last emitted value even when the whenever never iterates.
        let mut react_subs: Vec<crate::runtime::subtest::ReactSubscription> = Vec::new();
        let mut static_last_value: Option<Value> = None;
        for sub_val in &subscriptions {
            if let Value::Array(items, ..) = sub_val
                && items.len() >= 2
            {
                let source = items[0].clone();
                let callback = items[1].clone();
                let last_cbs = items
                    .get(2)
                    .and_then(Self::value_array_items)
                    .unwrap_or_default();
                let quit_cbs = items
                    .get(3)
                    .and_then(Self::value_array_items)
                    .unwrap_or_default();
                if let Value::Instance {
                    attributes: inner_attrs,
                    ..
                } = &source
                {
                    // Try to get channel via supply_id (or parent_supply_id for lines)
                    let inner_map = inner_attrs.as_map();
                    let supply_id = inner_map
                        .get("parent_supply_id")
                        .or_else(|| inner_map.get("supply_id"))
                        .and_then(|v| {
                            if let Value::Int(id) = v {
                                Some(*id as u64)
                            } else {
                                None
                            }
                        });
                    if let Some(sid) = supply_id
                        && let Some(rx) = take_supply_channel(sid)
                    {
                        react_subs.push(crate::runtime::subtest::ReactSubscription {
                            receiver: Some(rx),
                            ..crate::runtime::subtest::ReactSubscription::new(callback)
                        });
                        continue;
                    }
                    // No live channel: a static/finite source. Replay it now.
                    let mut lv = static_last_value.take().unwrap_or(Value::Nil);
                    self.replay_static_whenever_promise(
                        &source, &callback, &last_cbs, &quit_cbs, &mut lv,
                    )?;
                    static_last_value = Some(lv);
                    if promise.is_resolved() {
                        return Ok(());
                    }
                }
            }
        }

        if react_subs.is_empty() {
            // No live channels: resolve with the last value emitted by the
            // static sources (or any plain synchronously-emitted value).
            let result = static_last_value
                .or_else(|| plain_values.last().cloned())
                .unwrap_or(Value::Nil);
            promise.keep(result, String::new(), String::new());
            return Ok(());
        }

        // Drive the channel-backed subscriptions through the shared react loop
        // under the Promise policy: it polls until the supply block's `done`
        // keeps this promise (via the Supplier.done handler / supplier registry)
        // or the deadline elapses, keeping the promise with the last emitted
        // value. Seed that value from anything emitted synchronously before the
        // subscriptions.
        let seed = static_last_value
            .or_else(|| plain_values.last().cloned())
            .unwrap_or(Value::Nil);
        self.drive_react_subscriptions(
            react_subs,
            crate::runtime::subtest::SupplyDrivePolicy::Promise {
                promise: promise.clone(),
                deadline: std::time::Instant::now() + Duration::from_secs(30),
                last_value: seed,
            },
        )
    }

    /// Bridge to the relocated VM-side drive loop for callers that only hold
    /// `&mut Interpreter` (the `await $supply` / `$supply.Promise` path). The
    /// drive loop now lives on `impl VM` (see `vm/vm_react_loop.rs`) so that
    /// `whenever`-body dispatch can run compiled bytecode; the VM owns the
    /// `Interpreter` by value, so we hand it over via the established
    /// `mem::take` / `VM::new` / `into_interpreter` dance, run the loop, and take
    /// the interpreter back. State (`supply_emit_buffer`, the supplier
    /// registries are process-global) is preserved across the round trip.
    pub(crate) fn drive_react_subscriptions(
        &mut self,
        react_subs: Vec<crate::runtime::subtest::ReactSubscription>,
        policy: crate::runtime::subtest::SupplyDrivePolicy,
    ) -> Result<(), RuntimeError> {
        // CP-3 collapse: run the react drive loop with fresh execution registers
        // in place instead of the `mem::take(self)` + `VM::new` sub-VM.
        self.with_nested_registers(|vm| vm.drive_react_subscriptions_nested(react_subs, policy))
    }

    /// On the `await`/`.Promise` path, replay a finite/static `whenever` source
    /// (e.g. `Supply.from-list(...)`) synchronously: run the body callback for
    /// each value, then the LAST phaser callbacks. A lazy source element (e.g.
    /// `gather { ... }`) is forced here; if forcing or the body dies, the QUIT
    /// phaser callbacks run instead (with the exception bound to `$_`). Any
    /// value emitted by the body or the phasers is captured into `last_value`,
    /// which becomes the awaited supply's result.
    pub(super) fn replay_static_whenever_promise(
        &mut self,
        source: &Value,
        callback: &Value,
        last_cbs: &[Value],
        quit_cbs: &[Value],
        last_value: &mut Value,
    ) -> Result<(), RuntimeError> {
        let values = match source {
            Value::Instance { attributes, .. } => match attributes.as_map().get("values") {
                Some(Value::Array(items, ..)) => items.to_vec(),
                _ => Vec::new(),
            },
            _ => Vec::new(),
        };

        // Capture whatever the given callback `emit`s into `last_value`.
        fn run_capture(
            this: &mut Interpreter,
            cb: Value,
            args: Vec<Value>,
            last_value: &mut Value,
        ) -> Result<(), RuntimeError> {
            this.supply_emit_buffer.push(Vec::new());
            let res = this.call_sub_value(cb, args, true);
            let emitted = this.supply_emit_buffer.pop().unwrap_or_default();
            if let Some(last) = emitted.last() {
                *last_value = last.clone();
            }
            res.map(|_| ())
        }

        let err_to_value = |err: &RuntimeError| -> Value {
            err.exception
                .as_deref()
                .cloned()
                .unwrap_or_else(|| Value::str(err.message.clone()))
        };

        // Run the body for each source value; force lazy elements so a dying
        // gather surfaces as a quit.
        let mut quit_reason: Option<Value> = None;
        'replay: for v in values {
            let items: Vec<Value> = match &v {
                Value::LazyList(ll) => match self.force_lazy_list(ll) {
                    Ok(items) => items,
                    Err(err) => {
                        quit_reason = Some(err_to_value(&err));
                        break 'replay;
                    }
                },
                _ => vec![v],
            };
            for item in items {
                if let Err(err) = run_capture(self, callback.clone(), vec![item], last_value) {
                    if err.is_react_done() || err.is_last() {
                        break 'replay;
                    }
                    if err.is_next() || err.is_redo() {
                        continue;
                    }
                    // A `die` quits the supply: route to the QUIT phaser.
                    quit_reason = Some(err_to_value(&err));
                    break 'replay;
                }
            }
        }

        if let Some(reason) = quit_reason {
            for q in quit_cbs {
                let _ = run_capture(self, q.clone(), vec![reason.clone()], last_value);
            }
        } else {
            for l in last_cbs {
                let _ = run_capture(self, l.clone(), Vec::new(), last_value);
            }
        }
        Ok(())
    }
}
