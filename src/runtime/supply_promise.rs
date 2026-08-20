//! Split out of native_supply_methods.rs. See that file for the shared
//! helpers and the `QuitOutcome` enum.
use super::native_methods::*;
use super::*;
use crate::symbol::Symbol;
use crate::value::AttrMap;

impl Interpreter {
    /// Call a `whenever`/tap callback with the callback's own supply emitter
    /// made dynamically visible, so a bare `emit` inside a *sub* the callback
    /// calls reaches the right supply.
    ///
    /// A `whenever` callback carries its block's emitter explicitly, stamped on
    /// at creation time under [`Self::WHENEVER_EMITTER_ENV_KEY`]. Any other
    /// callback falls back to the `__mutsu_supply_emitter_<id>` lexical the
    /// parser binds as the on-demand body's parameter: a callback written inside
    /// a `supply` block captures it, an unrelated tap callback does not and
    /// pushes nothing. The fallback is only a guess when a callback captured
    /// more than one — the stamp is the authoritative answer.
    pub(crate) fn call_supply_tap(
        &mut self,
        tap: Value,
        args: Vec<Value>,
        propagate_return: bool,
    ) -> Result<Value, RuntimeError> {
        // A `__SupplyDoWrappedTap` marker (see `make_supply_do_wrapped_tap`)
        // bundles a `.do($cb)` source's `do_callbacks` with the real outer
        // subscriber. Run the do callbacks first — same as the synchronous
        // `plain_values` path already does — then unwrap to the real tap for
        // everything below (emitter-stamp detection needs the real Sub, not
        // this Instance marker).
        let tap = if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = tap.view()
            && class_name == "__SupplyDoWrappedTap"
        {
            let attrs = attributes.as_map();
            if let Some(ValueView::Array(cbs, ..)) = attrs.get("do_callbacks").map(Value::view) {
                for cb in cbs.iter().cloned().collect::<Vec<_>>() {
                    self.call_sub_value(cb, args.clone(), true)?;
                }
            }
            attrs.get("real_tap").cloned().unwrap_or(Value::NIL)
        } else {
            tap
        };
        // `(emitter, is_stamped)`: only a stamped emitter is authoritative.
        let (emitter, stamped) = Self::whenever_tap_emitter(&tap);
        if let Some(ref e) = emitter {
            self.active_supply_emitters.push(e.clone());
        }
        // This function's own `match` below handles a `done`/`is_react_done()`
        // signal raised anywhere in the tap body's dynamic extent (directly or
        // via a nested sub call) — see `runtime::react_done_handler_depth`.
        let _react_done_handler =
            crate::runtime::react_done_handler_depth::ReactDoneHandlerGuard::new();
        let res = self.call_sub_value(tap, args, propagate_return);
        if emitter.is_some() {
            self.active_supply_emitters.pop();
        }
        // A bare `done` written inside a *sub* the body called is not rewritten
        // to `$emitter.done()` by the parser, so it unwinds to here as a raw
        // react-done signal. Rakudo's `done` unwinds to the enclosing `supply`
        // block and completes *that* supply, so consume it here instead of
        // letting it escape into whoever emitted the value — an outer supply's
        // `whenever` body, which it would wrongly terminate. Only a stamped
        // `whenever` callback (one written in a `supply` block) claims it; a
        // react block's `done` still travels to the react loop.
        match (res, stamped, emitter) {
            // The desugar's own terminator (`ast::Stmt::SupplyBodyDone`) —
            // its preceding `$emitter.done()` statement already ran, so just
            // absorb the signal; nothing further to call.
            (Err(err), ..) if err.is_supply_body_done() => Ok(Value::NIL),
            (Err(err), true, Some(e)) if err.is_react_done() => {
                self.call_method_with_values(e, "done", vec![])?;
                Ok(Value::NIL)
            }
            // ADR-0031 Decision A: the dual of the `done` absorption above. A
            // `die` (or any other non-control error) raised in a *stamped*
            // `whenever` body belongs to the enclosing `supply` block, not to
            // whichever upstream source happened to dispatch this callback —
            // convert it to `$emitter.quit($reason)` via the canonical
            // `Supplier."quit"` so the block's own quit-handling protocol
            // (`QuitOutcome`, downstream `quit =>`) runs exactly once, on the
            // right object. The control-signal exclusion list is copied
            // verbatim from the emit-dispatch fallback this replaces
            // (`native_supplier_methods.rs`) so `next`/`last`/`return`/`done`
            // keep unwinding as control flow instead of tearing the supply
            // down. When the stamped emitter carries no `supplier_id` (the
            // replay path's `run_on_demand_body(cb, None)` shape) there is
            // nothing to quit — fall through and return the error as-is.
            (Err(err), true, Some(e))
                if !(err.is_return()
                    || err.return_value.is_some()
                    || err.is_react_done()
                    || err.is_last()
                    || err.is_supply_body_done()
                    || err.is_next()
                    || err.is_redo())
                    && Self::emitter_supplier_id_of(&e).is_some() =>
            {
                let reason = err
                    .exception
                    .as_deref()
                    .cloned()
                    .unwrap_or_else(|| Value::str(err.message));
                self.call_method_with_values(e, "quit", vec![reason])?;
                Ok(Value::NIL)
            }
            (res, ..) => res,
        }
    }

    /// Extract the supply-block emitter a `whenever`/tap/phaser callback
    /// carries: `(emitter, is_stamped)`. A stamped emitter (installed under
    /// [`Self::WHENEVER_EMITTER_ENV_KEY`] at `whenever` registration) is
    /// authoritative; the `__mutsu_supply_emitter_<id>` lexical the on-demand
    /// body's parameter binds is only a fallback guess.
    pub(crate) fn whenever_tap_emitter(tap: &Value) -> (Option<Value>, bool) {
        tap.as_sub()
            .map(|data| match data.env.get(Self::WHENEVER_EMITTER_ENV_KEY) {
                Some(own) => (Some(own.clone()), true),
                None => (
                    data.env
                        .keys()
                        .find(|k| k.with_str(|s| s.starts_with("__mutsu_supply_emitter_")))
                        .and_then(|k| data.env.get_sym(*k).cloned()),
                    false,
                ),
            })
            .unwrap_or((None, false))
    }

    /// `Some(supplier_id)` when `emitter` is a `Supplier` instance carrying a
    /// `supplier_id` attribute (i.e. it stands for a live `supply` block's
    /// emitter, as opposed to the id-less emitter `run_on_demand_body` mints
    /// for the replay path, which has nothing registered against it to quit).
    pub(crate) fn emitter_supplier_id_of(emitter: &Value) -> Option<u64> {
        match emitter.view() {
            ValueView::Instance { attributes, .. } => supplier_id_from_attrs(&attributes.as_map()),
            _ => None,
        }
    }

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
            a.insert("done".to_string(), Value::FALSE);
            if let Some(sid) = emitter_supplier_id {
                a.insert("supplier_id".to_string(), Value::int(sid as i64));
            }
            a
        });
        self.supply_emit_buffer.push(Vec::new());
        // "Did the body complete *this* supply?" must be asked of this emitter,
        // not of the process. With an id, count `done`s on the emitter itself;
        // without one (`done` cannot reach a supplier), fall back to this
        // thread's total — the body runs synchronously here, so a `done` on
        // another thread's pipeline is never this body's.
        let done_before = match emitter_supplier_id {
            Some(sid) => supplier_done_call_count(sid),
            None => thread_supplier_done_count(),
        };
        // A bare `emit` reached from a *sub* called by the body is not rewritten
        // to `$emitter.emit(...)`, so it needs the emitter dynamically.
        self.active_supply_emitters.push(emitter.clone());
        let mut result = self.call_sub_value(on_demand_cb, vec![emitter], false);
        self.active_supply_emitters.pop();
        // The body's own bare `done` (`ast::Stmt::SupplyBodyDone`) always ends
        // just this synchronous call — absorb it here rather than relying on
        // callers to special-case it (they already treat a stray
        // `is_react_done()` this way, but that signal means something
        // different: an *actual* react-level `done`, not this one).
        if let Err(ref e) = result
            && e.is_supply_body_done()
        {
            result = Ok(Value::NIL);
        }
        let done_after = match emitter_supplier_id {
            Some(sid) => supplier_done_call_count(sid),
            None => thread_supplier_done_count(),
        };
        let body_ran_done = done_after > done_before;
        let emitted = self.supply_emit_buffer.pop().unwrap_or_default();
        (result, emitted, body_ran_done)
    }

    /// Rewrite every `whenever <Promise>` subscription marker the body just
    /// registered into the supplier-backed `whenever <Supply>` form.
    ///
    /// `run_whenever_with_value` records a subscription as a 4-element marker
    /// `[source, body, [LAST…], [QUIT…]]`, and every consumer downstream of
    /// here recognises one only by its source being a `Supply` — a `Promise`
    /// source fell through as an ordinary emitted value and was handed to the
    /// tap as the raw marker array. Raku's `whenever $promise` is exactly a
    /// one-shot supply ("emit the result once, then done"), so minting a
    /// supplier for it here lets the existing tap / serialize-group / done-group
    /// machinery drive it unchanged.
    ///
    /// The promise is NOT armed here: a supplier keeps no backlog
    /// (`register_supplier_tap` does not replay), so an already-resolved
    /// promise would emit into a tapless supplier and lose the value. Each arm
    /// is parked on `pending_promise_whenever_arms` and fired by
    /// [`Self::arm_pending_promise_whenevers`] once the consumer has registered
    /// its taps.
    pub(crate) fn normalize_promise_whenever_markers(&mut self, emitted: Vec<Value>) -> Vec<Value> {
        if !emitted.iter().any(Self::is_promise_whenever_marker) {
            return emitted;
        }
        emitted
            .into_iter()
            .map(|item| {
                if !Self::is_promise_whenever_marker(&item) {
                    return item;
                }
                let ValueView::Array(arr, ..) = item.view() else {
                    return item;
                };
                let ValueView::Promise(promise) = arr[0].view() else {
                    return item;
                };
                let supplier_id = next_supplier_id();
                let supplier = Value::make_instance(Symbol::intern("Supplier"), {
                    let mut a = HashMap::new();
                    a.insert("emitted".to_string(), Value::array(Vec::new()));
                    a.insert("done".to_string(), Value::FALSE);
                    a.insert("supplier_id".to_string(), Value::int(supplier_id as i64));
                    a
                });
                let supply = Value::make_instance(Symbol::intern("Supply"), {
                    let mut a = HashMap::new();
                    a.insert("values".to_string(), Value::array(Vec::new()));
                    a.insert("taps".to_string(), Value::array(Vec::new()));
                    a.insert("live".to_string(), Value::TRUE);
                    a.insert("supplier_id".to_string(), Value::int(supplier_id as i64));
                    a.insert("supplier_done".to_string(), Value::FALSE);
                    a
                });
                self.pending_promise_whenever_arms
                    .push((promise.clone(), supplier));
                Value::array(vec![supply, arr[1].clone(), arr[2].clone(), arr[3].clone()])
            })
            .collect()
    }

    /// True for a `whenever` subscription marker whose source is a `Promise`.
    pub(crate) fn is_promise_whenever_marker(item: &Value) -> bool {
        matches!(item.view(), ValueView::Array(arr, ..)
            if arr.len() == 4 && matches!(arr[0].view(), ValueView::Promise(_)))
    }

    /// Arm every promise parked by [`Self::normalize_promise_whenever_markers`]:
    /// when the promise resolves, push its result into the supplier that now
    /// stands in for it and immediately signal `done` (a promise fires once).
    /// A broken promise `quit`s the stand-in, so the `whenever`'s QUIT phaser
    /// and the tap's `quit` handler see it.
    ///
    /// Callers must run this only after registering the taps for the rewritten
    /// markers. The waiter runs on whichever thread resolves the promise (or
    /// synchronously here when it already has), so it drives a thread clone of
    /// this interpreter — the same pair `promise_chain_method` uses for `.then`.
    pub(crate) fn arm_pending_promise_whenevers(&mut self) {
        for (promise, supplier) in std::mem::take(&mut self.pending_promise_whenever_arms) {
            let mut thread_interp = self.clone_for_thread();
            promise.on_resolve(Box::new(move |status, result, _output, _stderr| {
                let method = if status == "Kept" { "emit" } else { "quit" };
                let _ =
                    thread_interp.call_method_with_values(supplier.clone(), method, vec![result]);
                if status == "Kept" {
                    let _ = thread_interp.call_method_with_values(supplier, "done", vec![]);
                }
            }));
        }
    }

    /// Extract source values from a Supply's attributes.
    ///
    /// ADR-0031 Decision B (Slice 2): for an on-demand `supply { ... }`
    /// block, this is a thin wrapper around [`Self::supply_collect_values`]
    /// — rebuild the `Supply` value from its attributes and tap-and-drain
    /// it, instead of the synchronous replay this function used to do
    /// inline (walking `run_on_demand_body`'s raw emitted markers by hand,
    /// recursing into `replay_cold_whenever_capture` for a cold nested
    /// `whenever` source and silently dropping a live one). Tapping the
    /// *outer* supply directly routes every nested `whenever` through the
    /// same `"tap"|"act"` dispatch that already drives all four
    /// whenever-source branches correctly (including a live one, since
    /// Slice 1), so the manual marker-walk this used to need is gone
    /// entirely: by the time a value reaches the collector shim, any nested
    /// whenever it came through has already been fully driven.
    ///
    /// A plain, non-on-demand Supply (a static `values` array, or a live
    /// Supplier-/channel-backed one — neither of which can ever contain a
    /// *nested* `whenever` marker, since only an on-demand body's own
    /// `run_on_demand_body` call can register one) keeps the old direct
    /// attribute read instead of also going through `.tap()`. Two reasons,
    /// not just one: (1) Defect B never applied to this shape — there is no
    /// marker to lose — so tap-and-drain would only add the drain's cost for
    /// no correctness gain, and could even *block* on a genuinely infinite
    /// live source a caller never meant to materialize (e.g.
    /// `Supply.interval(...).head(3)` reading `source_values` only for its
    /// static branch); (2) a value delivered through `.tap()`'s
    /// callback-parameter binding is itemized (Raku's own "binding a List
    /// value to a `$`-sigil parameter containerizes it" rule — confirmed
    /// against `raku` directly, not a mutsu quirk), which would silently
    /// break a combinator like `.flat` that specifically needs the source's
    /// *un-itemized* stored shape (`roast/S17-supply/flat.t` "On demand
    /// publish with flat" pinned this during Slice 2 development).
    pub(super) fn supply_get_values(
        &mut self,
        attributes: &AttrMap,
    ) -> Result<Vec<Value>, RuntimeError> {
        if attributes.contains_key("on_demand_callback") {
            let attrs_map: HashMap<String, Value> = attributes.into();
            let supply = Value::make_instance(Symbol::intern("Supply"), attrs_map);
            return self.supply_collect_values(&supply, true);
        }
        Ok(match attributes.get("values").map(Value::view) {
            Some(ValueView::Array(items, ..)) => items.to_vec(),
            _ => Vec::new(),
        })
    }

    /// ADR-0031 Decision B (Slice 2): tap `supply` and drain the resulting
    /// event stream into a plain `Vec<Value>`, instead of the old
    /// synchronous replay (`replay_cold_whenever_capture` /
    /// `replay_static_whenever_promise`, both retired). `.tap()` already
    /// drives every whenever-source flavour correctly (ADR-0028's four
    /// branches, and — since Slice 1 — quit ownership too), so tapping the
    /// caller's own supply and collecting through the same `"tap"|"act"`
    /// chokepoint observes a value emitted *after* the synchronous portion
    /// of the tap call returns (a live inner subscription), which the old
    /// pull-based replay silently dropped
    /// (`todo/deep/cold-supply-whenever-source-replayed-not-tapped.md`,
    /// probe5 case E).
    ///
    /// The `__SupplyCollector` shim (`native_methods::supply_collector`) is
    /// an empty-env synthesized callable whose body is one `MethodCall` on a
    /// literal internal instance — the same idiom ADR-0028 §2's
    /// `__ScheduledTapPump` established — so it is trivially safe for the
    /// supply's own emitting thread(s) to invoke cross-thread; invoking it
    /// just pushes the event into the [`crate::value::waker::ReactWaker`]
    /// this function drains (the same ADR-0008 waker primitive
    /// `supply_list_values`'s direct-supplier fast path already uses).
    ///
    /// `wait_until_done`: `true` blocks (bounded by a 30s deadline, the same
    /// budget `supply_promise_on_demand` uses) until the tapped supply
    /// signals done/quit or the deadline elapses — hitting the deadline is a
    /// mutsu defect to observe, not a silent hang, so whatever was collected
    /// so far is returned rather than blocking forever. `false` only drains
    /// whatever the synchronous portion of the `.tap()` call already
    /// delivered. Either way, a source that completed synchronously (the
    /// common case: a finite/static source, or a cold whenever chain that
    /// fully replays inside the `.tap()` call itself) is already queued
    /// before the first drain, so this returns immediately without ever
    /// touching the waker's blocking wait.
    pub(crate) fn supply_collect_values(
        &mut self,
        supply: &Value,
        wait_until_done: bool,
    ) -> Result<Vec<Value>, RuntimeError> {
        use crate::value::waker::{ReactWaker, SinkEvent};
        use std::time::Duration;

        let waker = ReactWaker::new();
        let collector_id = crate::runtime::native_methods::register_supply_collector(waker.clone());
        let emit_shim = Self::build_supply_collector_shim(collector_id, "emit");
        let done_shim = Self::build_supply_collector_shim(collector_id, "done");
        let quit_shim = Self::build_supply_collector_shim(collector_id, "quit");

        let tap_handle = match self.call_method_with_values(
            supply.clone(),
            "tap",
            vec![
                emit_shim,
                Value::pair("done".to_string(), done_shim),
                Value::pair("quit".to_string(), quit_shim),
            ],
        ) {
            Ok(tap) => tap,
            Err(err) => {
                crate::runtime::native_methods::unregister_supply_collector(collector_id);
                return Err(err);
            }
        };

        let mut out = Vec::new();
        let mut quit_reason: Option<Value> = None;
        let deadline = crate::runtime::thread_compat::Instant::now() + Duration::from_secs(30);
        'drain: loop {
            for (_, event) in waker.drain() {
                match event {
                    SinkEvent::Emit(v) => out.push(v),
                    SinkEvent::Done => break 'drain,
                    SinkEvent::Quit(reason) => {
                        quit_reason = Some(reason);
                        break 'drain;
                    }
                }
            }
            if !wait_until_done {
                break;
            }
            let now = crate::runtime::thread_compat::Instant::now();
            if now >= deadline {
                break;
            }
            waker.wait_activity((deadline - now).min(Duration::from_millis(100)));
        }
        crate::runtime::native_methods::unregister_supply_collector(collector_id);
        let _ = self.call_method_with_values(tap_handle, "close", vec![]);

        if let Some(reason) = quit_reason {
            return Err(Self::runtime_error_from_supply_reason(reason));
        }
        Ok(out)
    }

    /// Implement Supply.Promise for on-demand supplies (supply { ... } blocks).
    /// Runs the supply body through a custom event loop so that async
    /// `whenever` subscriptions (e.g. Supply.interval) are properly handled.
    /// Keeps the promise with the last emitted value when done.
    pub(super) fn supply_promise_on_demand(
        &mut self,
        attributes: &AttrMap,
        promise: &crate::value::SharedPromise,
    ) -> Result<(), RuntimeError> {
        use crate::runtime::native_methods::take_supply_channel;
        use std::time::Duration;

        let on_demand_cb = match attributes.get("on_demand_callback") {
            Some(cb) => cb.clone(),
            None => {
                promise.keep(Value::NIL, String::new(), String::new());
                return Ok(());
            }
        };

        // Create an emitter supplier with a supplier_id so emits are tracked
        let emitter_supplier_id = next_supplier_id();
        // Register the promise on the emitter supplier. When supplier_done()
        // fires, it will keep all pending promises before supplier_reset()
        // clears the state.
        supplier_register_promise(emitter_supplier_id, promise.clone());

        // Enter react-like context to collect whenever registrations. Keep
        // `on_demand_cb` around (cheap `Value` clone) — the trailing
        // background drive below needs it to build a correctly-scoped
        // thread-clone via `clone_for_thread_for_block`.
        let (cb_result, emitted, _) =
            self.run_on_demand_body(on_demand_cb.clone(), Some(emitter_supplier_id));

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
            let is_supply_sub = if let ValueView::Array(arr, ..) = item.view() {
                arr.len() == 4
                    && matches!(arr[0].view(), ValueView::Instance { class_name, .. } if class_name == "Supply")
            } else {
                false
            };
            // A `whenever <Promise>` source is a subscription too; the loop
            // below drives it through a one-shot channel, exactly as the react
            // loop does for a promise source.
            if is_supply_sub || Self::is_promise_whenever_marker(&item) {
                subscriptions.push(item);
            } else {
                plain_values.push(item);
            }
        }

        if subscriptions.is_empty() {
            // No async subscriptions, just use plain emitted values
            let result = plain_values.last().cloned().unwrap_or(Value::NIL);
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
        // `register_nested_on_demand_source` below may register entries in
        // `self.supply_stream_consumers` (so a nested stage's `emit` streams
        // live into its consuming `whenever`'s callback instead of being
        // buffered). Those entries must move to `thread_interp` before the
        // background drive spawns — see the `stream_consumers_base` split
        // below — because the drive loop that actually observes the async
        // events runs on a *cloned* interpreter with its own (freshly empty)
        // `supply_stream_consumers`, not on `self`.
        let stream_consumers_base = self.supply_stream_consumers.len();
        for sub_val in &subscriptions {
            if let ValueView::Array(items, ..) = sub_val.view()
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
                // A promise source fires once: feed its result through a
                // one-shot channel followed by Done, the same shape the react
                // loop builds for `react { whenever $promise { … } }`.
                if let ValueView::Promise(shared) = source.view() {
                    let (tx, rx) =
                        crate::runtime::native_methods::supply_channel::supply_event_channel();
                    let shared_clone = shared.clone();
                    crate::runtime::builtins_system::spawn_gc_helper_thread(
                        "promise-wait",
                        move || {
                            let (result, _, _) = shared_clone.wait();
                            let _ =
                                tx.send(crate::runtime::native_methods::SupplyEvent::Emit(result));
                            let _ = tx.send(crate::runtime::native_methods::SupplyEvent::Done);
                        },
                    );
                    react_subs.push(crate::runtime::subtest::ReactSubscription {
                        receiver: Some(rx),
                        promise: Some(shared.clone()),
                        last_callbacks: last_cbs,
                        quit_callbacks: quit_cbs,
                        ..crate::runtime::subtest::ReactSubscription::new(callback)
                    });
                    continue;
                }
                if let ValueView::Instance {
                    attributes: inner_attrs,
                    ..
                } = source.view()
                {
                    // Try to get channel via supply_id (or parent_supply_id for lines)
                    let inner_map = inner_attrs.as_map();
                    let supply_id = inner_map
                        .get("parent_supply_id")
                        .or_else(|| inner_map.get("supply_id"))
                        .and_then(|v| {
                            if let ValueView::Int(id) = v.view() {
                                Some(id as u64)
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
                    // A live `Supplier`-backed source has no channel of its own:
                    // it pushes through the supplier registry's sinks, exactly as
                    // `react { whenever $supplier.Supply { … } }` does. Replaying
                    // it as a static source instead (what this used to do) read
                    // whatever it had emitted *so far* and then ran the LAST
                    // phaser, so `Promise(supply { whenever $live { … } })`
                    // resolved before the producer had emitted anything —
                    // a Cro response body parsed on the socket thread arrived
                    // after the promise was already kept with an empty Buf.
                    if let Some(ValueView::Int(supplier_id)) =
                        inner_map.get("supplier_id").map(Value::view)
                        && matches!(
                            inner_map.get("live").map(Value::view),
                            Some(ValueView::Bool(true))
                        )
                    {
                        react_subs.push(crate::runtime::subtest::ReactSubscription {
                            supplier_id: Some(supplier_id as u64),
                            last_callbacks: last_cbs,
                            quit_callbacks: quit_cbs,
                            ..crate::runtime::subtest::ReactSubscription::new(callback)
                        });
                        continue;
                    }
                    // A nested `supply { ... }` source (an on-demand supply
                    // that is not itself directly backed by a channel or a
                    // live `Supplier`): wire it as a live streaming stage the
                    // same way the react loop's `build_react_subscriptions`
                    // does via `register_nested_on_demand_source`, instead of
                    // falling through to the static/finite replay below.
                    // `replay_static_whenever_promise` materializes the
                    // source through `supply_get_values`, which intentionally
                    // *drops* a still-live nested subscription rather than
                    // replaying it — so a `whenever <derived-supply>` inside
                    // this `Promise(supply {...})` body would silently lose
                    // every value the derived supply emits asynchronously
                    // after this point (see
                    // todo/deep/last-phaser-loses-outer-var-mutations-when-whenever-source-is-a-nested-supply.md).
                    if let Some(early_done) =
                        self.register_nested_on_demand_source(sub_val, &mut react_subs, 0)?
                    {
                        if early_done {
                            // No background drive will run: nothing will ever
                            // consume the stream consumer(s) this call (or
                            // its own recursion) may have left registered.
                            self.supply_stream_consumers.truncate(stream_consumers_base);
                            return Ok(());
                        }
                        continue;
                    }
                    // No live channel: a static/finite source. ADR-0031
                    // Decision B (Slice 2): materialize it via
                    // `supply_get_values` (fixing `await` on a supply whose
                    // `whenever` source is a cold on-demand supply, which
                    // used to return `Nil` — probe4) instead of the retired
                    // `replay_static_whenever_promise`'s own pull. This
                    // branch's `source` never itself carries
                    // `on_demand_callback` (that shape is claimed above by
                    // `register_nested_on_demand_source`), so
                    // `supply_get_values` takes its plain-values fast path
                    // here — same cost as before, no new tap-and-drain call.
                    let mut lv = static_last_value.take().unwrap_or(Value::NIL);
                    let (values, initial_quit) = match self.supply_get_values(&inner_map) {
                        Ok(items) => (items, None),
                        Err(err) => (
                            Vec::new(),
                            Some(
                                err.exception
                                    .as_deref()
                                    .cloned()
                                    .unwrap_or_else(|| Value::str(err.message.clone())),
                            ),
                        ),
                    };
                    self.drive_whenever_promise_over_values(
                        values,
                        initial_quit,
                        &callback,
                        &last_cbs,
                        &quit_cbs,
                        &mut lv,
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
            self.supply_stream_consumers.truncate(stream_consumers_base);
            let result = static_last_value
                .or_else(|| plain_values.last().cloned())
                .unwrap_or(Value::NIL);
            promise.keep(result, String::new(), String::new());
            return Ok(());
        }

        // Drive the channel-backed subscriptions through the shared react loop
        // under the Promise policy: it polls until the supply block's `done`
        // keeps this promise (via the Supplier.done handler / supplier registry)
        // or the deadline elapses, keeping the promise with the last emitted
        // value. Seed that value from anything emitted synchronously before the
        // subscriptions.
        //
        // This drive runs on a background thread, not the calling thread: the
        // caller must get back a Planned promise immediately (raku semantics —
        // `Promise(supply {...})` never blocks), and the calling thread is
        // sometimes the very thread whose completion the supply is waiting on
        // (e.g. a Cro response body's `Promise(supply { whenever
        // self.body-byte-stream {...} })` resolved from inside `.body-text`,
        // which the producer thread blocks on before it can send `done` —
        // driving inline here deadlocks that cycle). Everything above this
        // point (running the body to collect subscriptions, the synchronous
        // resolution branches) stays on the calling thread; only the
        // long-lived poll moves off it.
        //
        // `clone_for_thread_for_block(&on_demand_cb)` (not a bare
        // `clone_for_thread`) keeps the callback's own captured scalars off
        // the cross-thread bare-name lane the same way `start {}` does (see
        // ADR-0023 if any captured name is also an active for-loop parameter
        // at the coercion site). The helper thread is GC-registered
        // (`spawn_user_thread`) per the Gc-thread registration rule — never a
        // raw `std::thread::spawn`. This must be `spawn_user_thread`, not
        // `spawn_gc_helper_thread`: `drive_react_subscriptions` below runs the
        // whenever body as real VM bytecode (method dispatch, grammar/regex
        // recursion, ...), i.e. genuine user code, not GC-helper plumbing —
        // `spawn_gc_helper_thread`'s default ~2 MiB stack overflows on deep
        // recursion there (observed as a SIGSEGV inside grammar-driven regex
        // matching, e.g. `Cro::HTTP::Cookie.from-set-cookie`).
        let seed = static_last_value
            .or_else(|| plain_values.last().cloned())
            .unwrap_or(Value::NIL);
        let policy = crate::runtime::subtest::SupplyDrivePolicy::Promise {
            promise: promise.clone(),
            deadline: crate::runtime::thread_compat::Instant::now() + Duration::from_secs(30),
            last_value: seed,
            emitter_supplier_id: Some(emitter_supplier_id),
        };
        let mut thread_interp = self.clone_for_thread_for_block(&on_demand_cb);
        // Hand any stream consumer(s) `register_nested_on_demand_source`
        // registered above (on `self`, the calling thread) over to the
        // interpreter that will actually observe the async events —
        // `clone_for_thread_for_block` starts `thread_interp` with an empty
        // `supply_stream_consumers`, so a nested stage's live-forwarding
        // wiring would otherwise vanish and its `emit`s would never reach
        // the consuming `whenever`'s callback.
        thread_interp.supply_stream_consumers.extend(
            self.supply_stream_consumers
                .split_off(stream_consumers_base),
        );
        crate::runtime::builtins_system::spawn_user_thread("react", move || {
            // The drive loop keeps/breaks `promise` directly as it runs (see
            // `drive_react_subscriptions_inner`'s `SupplyDrivePolicy::Promise`
            // handling); its `Result` here only carries Rust-level plumbing
            // errors that have nowhere else to go now that the caller has
            // already returned, so there is nothing to propagate.
            let _ = thread_interp.drive_react_subscriptions(react_subs, policy);
        });
        Ok(())
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

    /// Drive a `whenever` body over an already-materialized list of source
    /// values, capturing every value the body and its phasers emit, in
    /// order. This is the tail of the old `replay_cold_whenever_capture`
    /// (ADR-0031 Decision B / Slice 2 retired the pull-based replay itself —
    /// see [`Self::supply_collect_values`] — but the "run this whenever's
    /// body over each already-known value" logic is still needed by the one
    /// caller left that cannot go through `.tap()` directly: a nested cold
    /// whenever source inside the `"tap"|"act"` dispatch's own on-demand
    /// branch, which has already established the value list is static
    /// before reaching here). Lazy source elements are forced; a `done`/
    /// `last` from the body stops the drive; `next`/`redo` skip to the next
    /// value; a `die` (or the caller's own `initial_quit`, e.g. an error
    /// materializing `values` in the first place) runs the whenever's QUIT
    /// phasers if any are registered, otherwise its reason is returned as
    /// the second tuple element for the caller to deliver (quit callback or
    /// hard error). LAST phasers run on normal completion.
    pub(crate) fn drive_whenever_body_over_values(
        &mut self,
        values: Vec<Value>,
        initial_quit: Option<Value>,
        callback: &Value,
        last_cbs: &[Value],
        quit_cbs: &[Value],
    ) -> (Vec<Value>, Option<Value>) {
        // This construct handles `next`/`last`/`redo`, so a loop-control
        // statement raised anywhere in its dynamic extent has somewhere to go
        // (`runtime/loop_handler_depth.rs`). Without the guard the raise site
        // would convert the signal into a thrown `X::ControlFlow` and silently
        // break this loop.
        let _loop_handler = crate::runtime::loop_handler_depth::LoopHandlerGuard::new();
        let mut quit_reason = initial_quit;

        fn run_capture(
            this: &mut Interpreter,
            cb: Value,
            args: Vec<Value>,
            captured: &mut Vec<Value>,
        ) -> Result<(), RuntimeError> {
            this.supply_emit_buffer.push(Vec::new());
            // The caller below handles `is_react_done()`/`is_last()` from this
            // body's dynamic extent (directly or via a nested sub call) — see
            // `runtime::react_done_handler_depth`.
            let _react_done_handler =
                crate::runtime::react_done_handler_depth::ReactDoneHandlerGuard::new();
            let res = this.call_sub_value(cb, args, true);
            drop(_react_done_handler);
            let mut emitted = this.supply_emit_buffer.pop().unwrap_or_default();
            captured.append(&mut emitted);
            res.map(|_| ())
        }

        let err_to_value = |err: &RuntimeError| -> Value {
            err.exception
                .as_deref()
                .cloned()
                .unwrap_or_else(|| Value::str(err.message.clone()))
        };

        let mut captured: Vec<Value> = Vec::new();
        'replay: for v in values {
            let lazy = if let ValueView::LazyList(ll) = v.view() {
                Some(ll.clone())
            } else {
                None
            };
            let items: Vec<Value> = match lazy {
                Some(ll) => match self.force_lazy_list(&ll) {
                    Ok(items) => items,
                    Err(err) => {
                        quit_reason = Some(err_to_value(&err));
                        break 'replay;
                    }
                },
                None => vec![v],
            };
            for item in items {
                if let Err(err) = run_capture(self, callback.clone(), vec![item], &mut captured) {
                    if err.is_react_done() || err.is_last() || err.is_supply_body_done() {
                        break 'replay;
                    }
                    if err.is_next() || err.is_redo() {
                        continue;
                    }
                    quit_reason = Some(err_to_value(&err));
                    break 'replay;
                }
            }
        }

        if let Some(reason) = quit_reason {
            if quit_cbs.is_empty() {
                return (captured, Some(reason));
            }
            for q in quit_cbs {
                let _ = run_capture(self, q.clone(), vec![reason.clone()], &mut captured);
            }
        } else {
            for l in last_cbs {
                let _ = run_capture(self, l.clone(), Vec::new(), &mut captured);
            }
        }
        (captured, None)
    }

    /// Drive a `whenever` body over an already-materialized list of source
    /// values, on the `await`/`.Promise` path: run the body callback for each
    /// value, then the LAST phaser callbacks. This is the tail of the old
    /// `replay_static_whenever_promise` (ADR-0031 Decision B / Slice 2
    /// retired the pull-based replay itself — the caller now materializes
    /// `values` via [`Self::supply_get_values`], which tap-and-drains an
    /// on-demand source instead of reading a static snapshot, fixing `await
    /// (supply { whenever <cold on-demand supply source> { ... } })`
    /// returning `Nil` — probe4). A lazy source element (e.g. `gather { ...
    /// }`) is forced here; if forcing or the body dies, the QUIT phaser
    /// callbacks run instead (with the exception bound to `$_`). Any value
    /// emitted by the body or the phasers is captured into `last_value`,
    /// which becomes the awaited supply's result.
    pub(super) fn drive_whenever_promise_over_values(
        &mut self,
        values: Vec<Value>,
        initial_quit: Option<Value>,
        callback: &Value,
        last_cbs: &[Value],
        quit_cbs: &[Value],
        last_value: &mut Value,
    ) -> Result<(), RuntimeError> {
        // This construct handles `next`/`last`/`redo`, so a loop-control
        // statement raised anywhere in its dynamic extent has somewhere to go
        // (`runtime/loop_handler_depth.rs`). Without the guard the raise site
        // would convert the signal into a thrown `X::ControlFlow` and silently
        // break this loop.
        let _loop_handler = crate::runtime::loop_handler_depth::LoopHandlerGuard::new();

        // Capture whatever the given callback `emit`s into `last_value`.
        fn run_capture(
            this: &mut Interpreter,
            cb: Value,
            args: Vec<Value>,
            last_value: &mut Value,
        ) -> Result<(), RuntimeError> {
            this.supply_emit_buffer.push(Vec::new());
            // The caller below handles `is_react_done()`/`is_last()` from this
            // body's dynamic extent — see `runtime::react_done_handler_depth`.
            let _react_done_handler =
                crate::runtime::react_done_handler_depth::ReactDoneHandlerGuard::new();
            let res = this.call_sub_value(cb, args, true);
            drop(_react_done_handler);
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
        let mut quit_reason: Option<Value> = initial_quit;
        'replay: for v in values {
            let lazy = if let ValueView::LazyList(ll) = v.view() {
                Some(ll.clone())
            } else {
                None
            };
            let items: Vec<Value> = match lazy {
                Some(ll) => match self.force_lazy_list(&ll) {
                    Ok(items) => items,
                    Err(err) => {
                        quit_reason = Some(err_to_value(&err));
                        break 'replay;
                    }
                },
                None => vec![v],
            };
            for item in items {
                if let Err(err) = run_capture(self, callback.clone(), vec![item], last_value) {
                    if err.is_react_done() || err.is_last() || err.is_supply_body_done() {
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
