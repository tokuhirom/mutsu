use crate::runtime::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

use super::state::SupplyEvent;
use crate::value::AttrMap;

/// One `run_supply_act_loop` dispatch: the value to pass to the plain
/// callback (unused when `end_cb` is set), an optional `(callback, args)` to
/// call instead (the tap's `done =>`/`quit =>` handler), and whether that
/// handler is a done-group marker/`__SupplyDoneChain` that needs
/// `invoke_done_callback` rather than a plain call.
type ActLoopDispatchUnit = (Value, Option<(Value, Vec<Value>)>, bool);

impl Interpreter {
    pub(in crate::runtime) fn native_encoding_builtin(
        attributes: &AttrMap,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        Ok(match method {
            "name" => attributes
                .get("name")
                .cloned()
                .unwrap_or(Value::str(String::new())),
            "alternative-names" => attributes
                .get("alternative-names")
                .cloned()
                .unwrap_or_else(|| Value::array(Vec::new())),
            "encoder" => {
                let enc_name = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let mut attrs = HashMap::new();
                attrs.insert("encoding".to_string(), Value::str(enc_name));
                // Extract :replacement named arg from args
                for arg in args {
                    if let ValueView::Pair(key, value) = arg.view()
                        && key == "replacement"
                    {
                        attrs.insert("replacement".to_string(), value.clone());
                    }
                }
                Value::make_instance(Symbol::intern("Encoding::Encoder::Builtin"), attrs)
            }
            // Cost: O(1).
            "decoder" => {
                let enc_name = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let translate_nl = args.iter().any(|arg| {
                    matches!(arg.view(), ValueView::Pair(key, value)
                        if key == "translate-nl" && value.truthy())
                });
                return crate::runtime::stream_decoder_object::new_decoder(&enc_name, translate_nl);
            }
            "gist" | "Str" => {
                let name = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Value::str(format!("Encoding::Builtin<{}>", name))
            }
            "WHAT" => Value::package(Symbol::intern("Encoding::Builtin")),
            _ => Value::NIL,
        })
    }

    pub(in crate::runtime) fn native_encoding_encoder(
        attributes: &AttrMap,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        match method {
            "encode-chars" => {
                let input = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let enc_name = attributes
                    .get("encoding")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let replacement = attributes.get("replacement");
                let enc_lower = enc_name.to_lowercase();
                let is_ascii = matches!(enc_lower.as_str(), "ascii" | "us-ascii");
                let is_utf8_c8 = enc_lower == "utf8-c8";

                let mut bytes: Vec<Value> = Vec::new();
                if is_utf8_c8 {
                    for b in crate::runtime::utf8_c8::encode_utf8_c8(&input) {
                        bytes.push(Value::int(b as i64));
                    }
                } else if is_ascii {
                    for ch in input.chars() {
                        if ch as u32 > 127 {
                            if let Some(repl) = replacement {
                                let repl_str = if matches!(repl.view(), ValueView::Bool(true)) {
                                    // :replacement (Bool True) -> default replacement char '?'
                                    "?".to_string()
                                } else {
                                    repl.to_string_value()
                                };
                                for b in repl_str.bytes() {
                                    bytes.push(Value::int(b as i64));
                                }
                            } else {
                                return Err(RuntimeError::new(format!(
                                    "Cannot encode character '{}' (U+{:04X}) in ASCII",
                                    ch, ch as u32
                                )));
                            }
                        } else {
                            bytes.push(Value::int(ch as u32 as i64));
                        }
                    }
                } else {
                    // UTF-8 encoding
                    for b in input.as_bytes() {
                        bytes.push(Value::int(*b as i64));
                    }
                }

                Ok(crate::value::value_buf::make_buf(
                    Symbol::intern("Blob[uint8]"),
                    bytes,
                ))
            }
            "WHAT" => Ok(Value::package(Symbol::intern("Encoding::Encoder::Builtin"))),
            _ => Ok(Value::NIL),
        }
    }

    /// Background event loop for Supply.act on live supplies (e.g., signal).
    /// Receives events from the channel and calls the callback.
    /// If the callback calls `exit`, terminates the entire process.
    ///
    /// `done_cb` / `quit_cb` are the tap's `done =>` / `quit =>` handlers: a
    /// channel-backed source signals the end of the stream with a
    /// `SupplyEvent::Done`/`Quit`, and dropping out of the loop without running
    /// them left an `IO::Socket::Async` reader waiting forever for the `done`
    /// that a closed peer had already sent (roast S32-io/IO-Socket-Async.t
    /// "Echo server").
    ///
    /// `close_flag` is the Tap-teardown handle (`register_act_loop_close` id
    /// plus the channel's shared close flag): `Tap.close` sets the flag and
    /// this loop's bounded wait re-checks it, so the worker exits instead of
    /// blocking on the channel forever. `None` (the scheduled-pump drain,
    /// whose sender is dropped on close) keeps the plain blocking receive. A
    /// flag-driven exit is a *close*, not a `done` — it must not run the
    /// done chain (raku does not fire LAST phasers on `.close`; CLOSE phasers
    /// are fired separately by `native_tap`).
    ///
    /// `is_lines`/`line_chomp` mirror the `.lines`-derived Supply's own
    /// attributes: when set, a received chunk is appended to a carry-over
    /// buffer and split into complete lines (`take_complete_lines_from_buffer`,
    /// the same splitter the react/whenever drive loop uses) instead of being
    /// forwarded to `cb` verbatim — a TCP read boundary can land mid-line, so
    /// a per-chunk split with no carry-over would emit truncated lines. Any
    /// partial trailing line is flushed once as its own line when the source
    /// signals `Done`/`Quit` (see
    /// `todo/tickets/supply-lines-drops-channel-backed-supplies.md`).
    ///
    /// `head_limit` mirrors a `.head(N)`-derived Supply's own attribute (see
    /// the "head" arm in `native_supply_dispatch.rs`): once `N` plain-value
    /// units have been dispatched, the loop stops accepting more even though
    /// the channel itself never signals `Done` (an infinite source like
    /// `Supply.interval` has no natural end) — it fires `done_cb` itself and
    /// breaks, same as a real upstream `Done`
    /// (`todo/tickets/head-on-a-channel-backed-supply-drops-every-value.md`).
    #[allow(clippy::too_many_arguments)]
    pub(in crate::runtime) fn run_supply_act_loop(
        interp: &mut Interpreter,
        rx: &super::supply_channel::SupplyReceiver,
        cb: &Value,
        delay_seconds: f64,
        done_cb: Option<Value>,
        quit_cb: Option<Value>,
        close_flag: Option<(u64, std::sync::Arc<std::sync::atomic::AtomicBool>)>,
        is_lines: bool,
        line_chomp: bool,
        mut head_limit: Option<usize>,
        producer_supplier_id: Option<u64>,
    ) {
        use super::state::take_complete_lines_from_buffer;
        use std::io::Write;
        use std::sync::atomic::Ordering;
        let mut line_buffer = String::new();
        'outer: loop {
            let received = match &close_flag {
                None => rx.recv().map_err(|_| ()),
                // wasm: the pool runs on the cooperative scheduler, where a
                // timeout poll loop would spin the only thread, so there is
                // nothing to loop over -- one close check, then a plain
                // blocking receive.
                Some((_, flag)) => {
                    #[cfg(target_arch = "wasm32")]
                    {
                        if flag.load(Ordering::Acquire) {
                            Err(()) // closed: exit like a disconnect
                        } else {
                            rx.recv().map_err(|_| ())
                        }
                    }
                    #[cfg(not(target_arch = "wasm32"))]
                    loop {
                        if flag.load(Ordering::Acquire) {
                            break Err(()); // closed: exit like a disconnect
                        }
                        // The 250 ms cap is a safety net, not a latency bound: a
                        // close racing the wait is honoured at most 250 ms late.
                        match rx.recv_timeout(std::time::Duration::from_millis(250)) {
                            Ok(ev) => break Ok(ev),
                            Err(std::sync::mpsc::RecvTimeoutError::Timeout) => continue,
                            Err(std::sync::mpsc::RecvTimeoutError::Disconnected) => {
                                break Err(());
                            }
                        }
                    }
                }
            };
            // Re-check after a successful receive: once `close` returns no new
            // body dispatch may start (the pin test t/supply-tap-close-interval.t
            // relies on this being a hard guarantee).
            if let Some((_, flag)) = &close_flag
                && flag.load(Ordering::Acquire)
            {
                break;
            }
            // `is_done_marker` selects `invoke_done_callback` over a plain call:
            // the tap's `done =>` slot may hold a done-group marker or a
            // `__SupplyDoneChain` (a `whenever`'s LAST phasers bundled with the
            // enclosing supply's group marker), which only that dispatcher
            // understands. It falls through to a plain call for an ordinary
            // callable, so the other callers are unaffected.
            //
            // A single received event can expand into several dispatch units
            // when `is_lines`: a chunk may complete more than one line (or
            // none, if the buffer still holds a partial line), and a
            // Done/Quit flushes any trailing partial line as one more unit
            // before its done/quit callback. `outer_should_break` tracks
            // whether the whole batch ends the loop (Done/Quit always do,
            // whether or not their callback is actually present — matching
            // the plain-value path's break-on-no-callback below).
            let mut units: Vec<ActLoopDispatchUnit> = Vec::new();
            let mut outer_should_break = false;
            match received {
                Ok(SupplyEvent::Emit(value)) => {
                    if is_lines {
                        line_buffer.push_str(&value.to_string_value());
                        for line in
                            take_complete_lines_from_buffer(&mut line_buffer, line_chomp, false)
                        {
                            units.push((Value::str(line), None, false));
                        }
                    } else {
                        units.push((value, None, false));
                    }
                    if let Some(remaining) = head_limit {
                        if units.len() >= remaining {
                            units.truncate(remaining);
                            head_limit = Some(0);
                            outer_should_break = true;
                            if let Some(ref cb) = done_cb {
                                units.push((Value::NIL, Some((cb.clone(), Vec::new())), true));
                            }
                        } else {
                            head_limit = Some(remaining - units.len());
                        }
                    }
                }
                Ok(SupplyEvent::Done) => {
                    outer_should_break = true;
                    if is_lines && !line_buffer.is_empty() {
                        units.push((Value::str(std::mem::take(&mut line_buffer)), None, false));
                    }
                    if let Some(ref cb) = done_cb {
                        units.push((Value::NIL, Some((cb.clone(), Vec::new())), true));
                    }
                }
                Ok(SupplyEvent::Quit(reason)) => {
                    outer_should_break = true;
                    if is_lines && !line_buffer.is_empty() {
                        units.push((Value::str(std::mem::take(&mut line_buffer)), None, false));
                    }
                    if let Some(ref cb) = quit_cb {
                        units.push((Value::NIL, Some((cb.clone(), vec![reason])), false));
                    }
                }
                Err(()) => break,
            };
            if units.is_empty() && !outer_should_break {
                // Buffer absorbed a chunk with no complete line yet: wait for
                // the next chunk (or Done, which flushes it).
                continue;
            }
            for (value, end_cb, is_done_marker) in units {
                Self::sleep_for_supply_delay(delay_seconds);
                // Handled below by the `is_react_done()`/`is_last()`/
                // `is_supply_body_done()` arm on this reader thread — see
                // `runtime::react_done_handler_depth`.
                let _react_done_handler =
                    crate::runtime::react_done_handler_depth::ReactDoneHandlerGuard::new();
                // `guard_worker_panic` (issue #8185): this loop runs detached on a
                // pooled worker, whose `worker_loop` catches and *discards* an
                // escaping panic — a Rust panic raised outside the VM's own
                // `run_inner_guarded` frames (native method plumbing, dispatch
                // helpers) therefore killed the tap silently, with no
                // diagnostic and no quit. Converting it here to the same
                // catchable `X::AdHoc` ("Internal error: ...") the VM boundary
                // produces puts it on the ordinary failure path below, so it
                // reaches a `quit =>` handler or the loud unhandled report.
                let result = crate::vm::guard_worker_panic(|| match end_cb {
                    Some((end, _)) if is_done_marker => {
                        interp.invoke_done_callback(end).map(|_| ())
                    }
                    Some((end, args)) => interp.call_sub_value(end, args, true).map(|_| ()),
                    None => interp
                        .call_sub_value(cb.clone(), vec![value], true)
                        .map(|_| ()),
                });
                drop(_react_done_handler);
                // Flush stdout (check both the per-interpreter buffer and the
                // shared thread output buffer used by thread clones).
                if !interp.output_sink().output.is_empty() {
                    print!("{}", interp.output_sink().output);
                    let _ = std::io::stdout().flush();
                    interp.output_sink_mut().output.clear();
                }
                if let Some(ref shared) = interp.output_sink().shared_thread_output {
                    let drained = std::mem::take(&mut *shared.lock().unwrap());
                    if !drained.is_empty() {
                        print!("{}", drained);
                        let _ = std::io::stdout().flush();
                    }
                }
                // Flush stderr
                if !interp.output_sink().stderr_output.is_empty() {
                    eprint!("{}", interp.output_sink().stderr_output);
                    let _ = std::io::stderr().flush();
                    interp.output_sink_mut().stderr_output.clear();
                }
                if let Some(ref shared) = interp.output_sink().shared_thread_stderr {
                    let drained = std::mem::take(&mut *shared.lock().unwrap());
                    if !drained.is_empty() {
                        eprint!("{}", drained);
                        let _ = std::io::stderr().flush();
                    }
                }
                // If the callback called exit, terminate the process
                if interp.control.halted {
                    std::process::exit(interp.control.exit_code as i32);
                }
                // If the callback threw an unhandled exception, terminate — but a
                // `done`/`last` is a control signal the supply machinery owns, not a
                // failure. It reaches here whenever the body (or the tap's `done =>`
                // chain, which carries the enclosing supply's LAST phasers and its
                // done-group marker) completes the supply from this reader thread.
                // Treating it as an unhandled exception killed the whole process
                // mid-file. Every other supply drive loop absorbs it the same way —
                // see the `is_react_done() || is_last()` arms in
                // `native_supply_mut_methods` and `vm_react_subscriptions`.
                // `is_supply_body_done()` is the same story for a `whenever` body
                // written directly inside a `supply { }` block (its `done` desugars
                // to this signal, see `ast::Stmt::SupplyBodyDone`) that fires from
                // *this* reader thread — e.g. a `whenever Supply.interval(...) {
                // done if ... }` nested in a `supply { }`.
                if let Err(err) = result {
                    if err.is_react_done() || err.is_last() || err.is_supply_body_done() {
                        break 'outer;
                    }
                    // When `cb` is *producer* code — a `supply { }` block's
                    // `whenever` body, driven here because its source is a live
                    // channel — its failure is the enclosing supply's quit, not
                    // an unhandled crash of the process: deliver it to the
                    // emitter's registered `quit =>` handlers and tear the loop
                    // down (issue #8185). Reached via the serialize-group link
                    // for the same reason `invoke_supply_done_callback_for_supplier`
                    // is (ADR-0031 Decision A): the downstream handler lives on
                    // the enclosing supply block's emitter, not on this source.
                    let mut err = err;
                    if let Some(sid) = producer_supplier_id {
                        match Self::deliver_act_loop_producer_quit(interp, sid, &err) {
                            Ok(true) => break 'outer,
                            Ok(false) => {}
                            // The quit handler itself failed: report *that*,
                            // loudly, rather than losing both errors.
                            Err(handler_err) => err = handler_err,
                        }
                    }
                    eprintln!(
                        "Unhandled exception in code scheduled on thread\n{}",
                        err.message
                    );
                    let _ = std::io::stderr().flush();
                    std::process::exit(1);
                }
            }
            if outer_should_break {
                break;
            }
        }
        // Every loop exit lands here (the `process::exit` paths above tear the
        // whole process down anyway): drop the registry entry so act loops
        // that end on their own don't leak it.
        if let Some((id, _)) = close_flag {
            super::state_lock::unregister_act_loop_close(id);
        }
    }
}
