//! Split out of native_supply_methods.rs. See that file for the shared
//! helpers and the `QuitOutcome` enum.
use super::native_methods::*;
use super::*;
use crate::symbol::Symbol;
use crate::value::AttrMap;

impl Interpreter {
    pub(super) fn native_supplier(
        &mut self,
        attributes: &AttrMap,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // This construct handles `next`/`last`/`redo`, so a loop-control
        // statement raised anywhere in its dynamic extent has somewhere to go
        // (`runtime/loop_handler_depth.rs`). Without the guard the raise site
        // would convert the signal into a thrown `X::ControlFlow` and silently
        // break this loop.
        let _loop_handler = crate::runtime::loop_handler_depth::LoopHandlerGuard::new();
        match method {
            "Supply" => {
                // Return a Supply backed by this Supplier
                let supplier_id = supplier_id_from_attrs(attributes).unwrap_or_else(next_supply_id);
                let (values, done, quit_reason) = supplier_snapshot(supplier_id);
                let mut supply_attrs = HashMap::new();
                supply_attrs.insert("values".to_string(), Value::array(values));
                supply_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                supply_attrs.insert(
                    "live".to_string(),
                    Value::truth(!done && quit_reason.is_none()),
                );
                supply_attrs.insert("supplier_id".to_string(), Value::int(supplier_id as i64));
                supply_attrs.insert("supplier_done".to_string(), Value::truth(done));
                if attributes.contains_key("preserving") {
                    // Supplier::Preserving: its supplies replay the buffered
                    // backlog to the next tap (see supplier_take_preserved_backlog).
                    supply_attrs.insert("preserving".to_string(), Value::TRUE);
                }
                if let Some(reason) = quit_reason {
                    supply_attrs.insert("quit_reason".to_string(), reason);
                }
                Ok(Value::make_instance(Symbol::intern("Supply"), supply_attrs))
            }
            "__mutsu_interval_tick" => {
                // One tick of a scheduler-driven `Supply.interval`. The block
                // the scheduler holds calls this; the value emitted is the
                // number of ticks so far, so the stream is 0, 1, 2, ... exactly
                // as a timer-driven interval produces.
                if let Some(supplier_id) = supplier_id_from_attrs(attributes) {
                    let (emitted, done, _) = supplier_snapshot(supplier_id);
                    if done {
                        return Ok(Value::NIL);
                    }
                    let value = Value::int(emitted.len() as i64);
                    supplier_emit(supplier_id, value.clone());
                    let actions = supplier_emit_callbacks(supplier_id, &value);
                    let _ = self.drive_supplier_emit_actions(supplier_id, actions);
                }
                Ok(Value::NIL)
            }
            "__mutsu_register_close_phaser" => {
                // A CLOSE phaser in a supply block registers its body here, on
                // the emitter's supplier_id, to run when the tap closes or the
                // supply terminates (see take_supplier_close_callbacks).
                if let Some(supplier_id) = supplier_id_from_attrs(attributes)
                    && let Some(cb) = args.into_iter().next()
                {
                    register_supplier_close_callback(supplier_id, cb);
                }
                Ok(Value::NIL)
            }
            "emit" => {
                // Push to supply_emit_buffer (works for on-demand callbacks)
                let value = Self::supplier_emit_value(&args)?;
                if Self::supply_is_terminated(attributes)
                    || self.on_demand_quit_pending(supplier_id_from_attrs(attributes))
                {
                    return Ok(Value::NIL);
                }
                // Streaming on-demand react path (see native_supplier_mut emit).
                if let Some(sid) = supplier_id_from_attrs(attributes)
                    && let Some(res) = self.try_stream_emit(sid, &value)
                {
                    return res.map(|_| Value::NIL);
                }
                self.supply_emit_collect(supplier_id_from_attrs(attributes), &value)?;
                if let Some(supplier_id) = supplier_id_from_attrs(attributes) {
                    supplier_emit(supplier_id, value.clone());
                    // Dispatch tap callbacks (head_limit, unique, produce, etc.)
                    let actions = supplier_emit_callbacks(supplier_id, &value);
                    // A live tap consumed this emission — it is not part of the
                    // Supplier::Preserving backlog a future tap replays.
                    if !actions.is_empty() && attributes.contains_key("preserving") {
                        supplier_mark_preserved_consumed(supplier_id);
                    }
                    for action in actions {
                        match action {
                            SupplierEmitAction::Call(tap, emitted, delay_seconds) => {
                                Self::sleep_for_supply_delay(delay_seconds);
                                if let Err(err) = self.call_supply_tap(tap, vec![emitted], true) {
                                    // A `return` inside the tap callback targets the
                                    // callback's lexically enclosing routine: propagate
                                    // the signal unchanged so that routine's call frame
                                    // consumes it — it is not a supply failure.
                                    if err.is_return() || err.return_value.is_some() {
                                        return Err(err);
                                    }
                                    // `done`/`last` inside a whenever body ends
                                    // the enclosing supply. Propagate the control
                                    // signal unchanged so the supply machinery
                                    // consumes it; falling through to the
                                    // quit/failure path below strips the control
                                    // flag and re-raises it as a thrown
                                    // `X::ControlFlow`, which then surfaced on a
                                    // channel reader thread as "done without
                                    // supply or react" and killed the process.
                                    if err.is_react_done()
                                        || err.is_last()
                                        || err.is_supply_body_done()
                                    {
                                        return Err(err);
                                    }
                                    // `next` inside a whenever body skips the rest
                                    // of the body for THIS value (Rakudo maps it to
                                    // a control exception the supply machinery
                                    // absorbs) — it is not a supply failure.
                                    if err.is_next() {
                                        continue;
                                    }
                                    return Err(err);
                                }
                            }
                            other => {
                                // Every other action kind either re-emits into a
                                // derived supplier or needs one of the shared
                                // handlers; `drive_supplier_emit_actions` runs it
                                // and, crucially, keeps driving the chain that
                                // re-emit starts.
                                self.drive_supplier_emit_actions(supplier_id, vec![other])?;
                            }
                        }
                    }
                }
                Ok(Value::NIL)
            }
            "done" => {
                // Bumped so the on-demand tap handler can tell (by comparing
                // the count before/after running the block body) that the body
                // itself called `done` — the supplier's own done state is reset
                // below, so it can't be read back later. Keyed by supplier id so
                // a concurrent pipeline's `done` cannot be mistaken for this
                // emitter's.
                bump_supplier_done_count(supplier_id_from_attrs(attributes));
                let preserving = attributes.contains_key("preserving");
                if let Some(supplier_id) = supplier_id_from_attrs(attributes) {
                    // With a tap already listening the done is delivered now, so
                    // it is not left in the preserved replay list for a later
                    // tap; with nothing listening it stays there for exactly one
                    // (mirrors the mutable-lane arm below).
                    if preserving && supplier_tap_count(supplier_id) > 0 {
                        supplier_mark_terminal_delivered(supplier_id);
                    }
                    supplier_done(supplier_id);
                    self.propagate_supplier_done(supplier_id)?;
                    close_all_supplier_taps(supplier_id);
                    // A `Supplier::Preserving` keeps its whole terminal state
                    // (backlog + done flag) past `.done` so a tap registered
                    // afterwards still replays it (see the mutable-lane arm's
                    // matching comment below for the full rationale).
                    if !preserving {
                        supplier_reset(supplier_id);
                    }
                }
                Ok(Value::NIL)
            }
            "quit" => {
                // Raku wraps a non-exception quit reason (a plain Str) in
                // X::AdHoc, so a `QUIT { default { .message } }` handler works
                // either way. Object instances (user exception classes like
                // `my class OMGBears is Exception`) pass through untouched —
                // `as_exception_value`'s name-based check cannot see `is
                // Exception` ancestry, so gate on the value shape instead.
                let reason = args
                    .first()
                    .cloned()
                    .unwrap_or_else(|| Value::str_from("Died"));
                let reason = if matches!(reason.view(), ValueView::Instance { .. }) {
                    reason
                } else {
                    Self::as_exception_value(reason)
                };
                if let Some(supplier_id) = supplier_id_from_attrs(attributes)
                    && self.defer_on_demand_quit(supplier_id, &reason)
                {
                    return Ok(Value::NIL);
                }
                if let Some(supplier_id) = supplier_id_from_attrs(attributes) {
                    supplier_quit(supplier_id, reason.clone());
                    self.propagate_supplier_quit(supplier_id, reason.clone())?;
                    Self::reset_supplier_after_quit(
                        supplier_id,
                        attributes.contains_key("preserving"),
                    );
                }
                Ok(Value::NIL)
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Supplier",
                method
            ))),
        }
    }

    // --- Supplier mutable ---

    /// The value `Supplier.emit` publishes. A bareword `a => 1` argument is a
    /// *named* argument (a `Pair` here, a `ValuePair` when positional), so it
    /// leaves `emit` without a positional and fails like rakudo's
    /// `method emit(Supplier:D: \value)` signature would.
    // Cost: O(n), n = number of arguments.
    fn supplier_emit_value(args: &[Value]) -> Result<Value, RuntimeError> {
        match args.iter().find(|a| !a.is_string_pair_value()) {
            Some(v) => Ok(v.clone()),
            None if args.is_empty() => Ok(Value::NIL),
            None => Err(RuntimeError::new(
                "Too few positionals passed; expected 2 arguments but got 1",
            )),
        }
    }

    pub(super) fn native_supplier_mut(
        &mut self,
        mut attrs: AttrMap,
        method: &str,
        args: Vec<Value>,
        publish: &mut crate::runtime::native_methods::AttrPublisher<'_>,
    ) -> Result<(Value, AttrMap), RuntimeError> {
        // This construct handles `next`/`last`/`redo`, so a loop-control
        // statement raised anywhere in its dynamic extent has somewhere to go
        // (`runtime/loop_handler_depth.rs`). Without the guard the raise site
        // would convert the signal into a thrown `X::ControlFlow` and silently
        // break this loop.
        let _loop_handler = crate::runtime::loop_handler_depth::LoopHandlerGuard::new();
        match method {
            "emit" => {
                let value = Self::supplier_emit_value(&args)?;
                if Self::supply_is_terminated(&attrs)
                    || self.on_demand_quit_pending(supplier_id_from_attrs(&attrs))
                {
                    return Ok((Value::NIL, attrs));
                }
                // Streaming on-demand react path: deliver synchronously to the
                // consumer instead of buffering, so an infinite synchronous body
                // can be terminated on emit-to-dead-consumer.
                if let Some(sid) = supplier_id_from_attrs(&attrs)
                    && let Some(res) = self.try_stream_emit(sid, &value)
                {
                    return res.map(|_| (Value::NIL, attrs));
                }
                // Push to supply_emit_buffer if active (or stream it to the
                // tap of the on-demand body that frame belongs to).
                self.supply_emit_collect(supplier_id_from_attrs(&attrs), &value)?;
                if let Some(buf) = self.async_state.supply_emit_timed_buffer.last_mut() {
                    buf.push((value.clone(), crate::thread_compat::Instant::now()));
                }
                if let Some(supplier_id) = supplier_id_from_attrs(&attrs) {
                    supplier_emit(supplier_id, value.clone());
                }
                let pushed = attrs.get_mut("emitted").and_then(|emitted| {
                    emitted.with_array_mut(|items, _kind| {
                        crate::gc::Gc::make_mut(items).push(value.clone());
                    })
                });
                if pushed.is_none() {
                    attrs.insert(
                        "emitted".to_string(),
                        Value::array(vec![args.first().cloned().unwrap_or(Value::NIL)]),
                    );
                }
                if let Some(ValueView::Int(supplier_id)) = attrs.get("supplier_id").map(Value::view)
                {
                    let sid = supplier_id as u64;
                    // If this trigger feeds a `whenever` in a supply block, hold
                    // the block's serialize lock across the whole callback
                    // dispatch. A sibling `whenever` emitted on another thread
                    // then waits — even while this handler is blocked inside
                    // `await` — enforcing "only in one whenever block at a time"
                    // (roast S17-supply/syntax.t test 53). Non-block suppliers are
                    // absent from the side map, so this is a no-op for them.
                    let _serialize_guard =
                        crate::runtime::native_methods::supplier_serialize_group(sid)
                            .map(crate::runtime::native_methods::acquire_supply_serialize);
                    let actions = supplier_emit_callbacks(sid, &value);
                    // A live tap consumed this emission — it is not part of the
                    // Supplier::Preserving backlog a future tap replays.
                    if !actions.is_empty() && attrs.contains_key("preserving") {
                        supplier_mark_preserved_consumed(sid);
                    }
                    for action in actions {
                        match action {
                            SupplierEmitAction::Call(tap, emitted, delay_seconds) => {
                                Self::sleep_for_supply_delay(delay_seconds);
                                if let Err(err) = self.call_supply_tap(tap, vec![emitted], true) {
                                    // A `return` inside the tap callback targets the
                                    // callback's lexically enclosing routine: propagate
                                    // the signal unchanged so that routine's call frame
                                    // consumes it — it is not a supply failure.
                                    if err.is_return() || err.return_value.is_some() {
                                        return Err(err);
                                    }
                                    // `done`/`last` inside a whenever body ends
                                    // the enclosing supply. Propagate the control
                                    // signal unchanged so the supply machinery
                                    // consumes it; falling through to the
                                    // quit/failure path below strips the control
                                    // flag and re-raises it as a thrown
                                    // `X::ControlFlow`, which then surfaced on a
                                    // channel reader thread as "done without
                                    // supply or react" and killed the process.
                                    if err.is_react_done()
                                        || err.is_last()
                                        || err.is_supply_body_done()
                                    {
                                        return Err(err);
                                    }
                                    // `next` inside a whenever body skips the rest
                                    // of the body for THIS value (Rakudo maps it to
                                    // a control exception the supply machinery
                                    // absorbs) — it is not a supply failure.
                                    if err.is_next() {
                                        continue;
                                    }
                                    return Err(err);
                                }
                            }
                            other => {
                                // See the matching arm in the immutable lane
                                // above: one shared driver for every re-emitting
                                // action kind, so a combinator chain is driven to
                                // its end rather than one hop.
                                self.drive_supplier_emit_actions(sid, vec![other])?;
                            }
                        }
                    }
                }
                Ok((Value::NIL, attrs))
            }
            "done" => {
                bump_supplier_done_count(supplier_id_from_attrs(&attrs));
                let preserving = attrs.contains_key("preserving");
                attrs.insert("done".to_string(), Value::TRUE);
                // Everything from here on wakes the taps, and a concurrent
                // `.emit` gates on exactly this flag (`supply_is_terminated`);
                // publishing at return would let that emit slip past a `done`
                // the taps have already been told about.
                publish.publish(&attrs);
                if let Some(supplier_id) = supplier_id_from_attrs(&attrs) {
                    // With a tap already listening the done is delivered now, so
                    // it is not left in the preserved replay list for a later
                    // tap; with nothing listening it stays there for exactly one.
                    if preserving && supplier_tap_count(supplier_id) > 0 {
                        supplier_mark_terminal_delivered(supplier_id);
                    }
                    supplier_done(supplier_id);
                }
                if let Some(ValueView::Int(supplier_id)) = attrs.get("supplier_id").map(Value::view)
                {
                    let sid = supplier_id as u64;
                    self.propagate_supplier_done(sid)?;
                    close_all_supplier_taps(sid);
                    // A `Supplier::Preserving` keeps its whole terminal state:
                    // the un-replayed backlog AND the done flag outlive `.done`,
                    // so a tap made afterwards still replays the buffered values
                    // and then sees `done` (Raku: `$p.emit(1); $p.done;
                    // $p.Supply.tap` delivers 1 then done). Resetting here made
                    // such a supply silent, which is how a Cro response body
                    // parsed before the consumer tapped it vanished.
                    if !preserving {
                        supplier_reset(sid);
                    }
                }
                if !preserving {
                    attrs.insert("done".to_string(), Value::FALSE);
                    attrs.remove("emitted");
                }
                Ok((Value::NIL, attrs))
            }
            "quit" => {
                // Raku wraps a non-exception quit reason (a plain Str) in
                // X::AdHoc, so a `QUIT { default { .message } }` handler works
                // either way. Object instances (user exception classes like
                // `my class OMGBears is Exception`) pass through untouched —
                // `as_exception_value`'s name-based check cannot see `is
                // Exception` ancestry, so gate on the value shape instead.
                let reason = args
                    .first()
                    .cloned()
                    .unwrap_or_else(|| Value::str_from("Died"));
                let reason = if matches!(reason.view(), ValueView::Instance { .. }) {
                    reason
                } else {
                    Self::as_exception_value(reason)
                };
                // The on-demand body that owns this emitter is still running:
                // deliver the quit after the values it emitted so far (#11237).
                if let Some(supplier_id) = supplier_id_from_attrs(&attrs)
                    && self.defer_on_demand_quit(supplier_id, &reason)
                {
                    return Ok((Value::NIL, attrs));
                }
                attrs.insert("done".to_string(), Value::TRUE);
                attrs.insert("quit_reason".to_string(), reason.clone());
                // Same as `done` above: the quit propagation below wakes the taps
                // and runs their QUIT phasers, so the terminal state must already
                // be on the shared cell when they look.
                publish.publish(&attrs);
                if let Some(supplier_id) = supplier_id_from_attrs(&attrs) {
                    supplier_quit(supplier_id, reason.clone());
                }
                if let Some(ValueView::Int(supplier_id)) = attrs.get("supplier_id").map(Value::view)
                {
                    let sid = supplier_id as u64;
                    self.propagate_supplier_quit(sid, reason.clone())?;
                    Self::reset_supplier_after_quit(sid, attrs.contains_key("preserving"));
                }
                attrs.insert("done".to_string(), Value::FALSE);
                attrs.remove("emitted");
                if !attrs.contains_key("preserving") {
                    attrs.remove("quit_reason");
                }
                Ok((Value::NIL, attrs))
            }
            _ => Err(RuntimeError::new(format!(
                "No native mutable method '{}' on Supplier",
                method
            ))),
        }
    }

    // --- Supply mutable ---

    /// Settle a supplier's state once its `quit` has reached every current
    /// consumer. A plain `Supplier` keeps no terminal state, exactly as after
    /// `done`: raku does not replay a quit to a tap made afterwards (#10866).
    /// A `Supplier::Preserving` keeps the quit (and its backlog) for the next
    /// tap, as it keeps `done`.
    // Cost: O(1).
    fn reset_supplier_after_quit(supplier_id: u64, preserving: bool) {
        if preserving {
            supplier_reset_keep_quit(supplier_id);
        } else {
            supplier_reset(supplier_id);
        }
    }
}
