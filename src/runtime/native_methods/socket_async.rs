use crate::runtime::*;
use crate::symbol::Symbol;

use std::sync::atomic::{AtomicBool, Ordering};

use super::state::*;
use super::state_supplier::*;
use crate::value::AttrMap;

impl Interpreter {
    fn async_socket_status_result(status: &str, result: Value, cause: Value) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("status".to_string(), Value::str_from(status));
        attrs.insert("result".to_string(), result);
        attrs.insert("cause".to_string(), cause);
        Value::make_instance(Symbol::intern("IO::Socket::Async::StatusResult"), attrs)
    }

    pub(super) fn async_socket_kept(result: Value) -> Value {
        Self::async_socket_status_result("Kept", result, Value::NIL)
    }

    pub(super) fn async_socket_broken(cause: Value) -> Value {
        Self::async_socket_status_result("Broken", Value::NIL, cause)
    }

    pub(super) fn async_supplier_emit_value(
        &mut self,
        supplier_id: u64,
        value: Value,
    ) -> Result<(), RuntimeError> {
        // Same shared driver the Supplier `emit` lanes use, so a combinator
        // chain hanging off an async socket's Supply is driven to its end.
        supplier_emit(supplier_id, value.clone());
        let actions = supplier_emit_callbacks(supplier_id, &value);
        self.drive_supplier_emit_actions(supplier_id, actions)
    }

    pub(super) fn async_supplier_done_value(&mut self, supplier_id: u64) {
        supplier_done(supplier_id);
        for (dsid, emitted) in flush_supplier_line_taps(supplier_id) {
            let _ = self.handle_supply_forward(dsid, emitted);
        }
        for (dsid, emitted) in flush_supplier_words_taps(supplier_id) {
            let _ = self.handle_supply_forward(dsid, emitted);
        }
        for done_cb in take_supplier_done_callbacks(supplier_id) {
            let _ = self.invoke_done_callback(done_cb);
        }
    }

    pub(in crate::runtime) fn native_socket_async_listener(
        &mut self,
        attributes: &AttrMap,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "tap" | "act" => {
                let callback = args.first().cloned().unwrap_or(Value::NIL);
                let quit_cb = Self::named_value(&args, "quit");
                let host = attributes
                    .get("host")
                    .map(Value::to_string_value)
                    .unwrap_or_else(|| "0.0.0.0".to_string());
                let requested_port = attributes
                    .get("port")
                    .and_then(|v| match v.view() {
                        ValueView::Int(i) => Some(i as u16),
                        ValueView::Num(n) if n.is_finite() => Some(n as u16),
                        _ => None,
                    })
                    .unwrap_or(0);
                let enc = attributes
                    .get("enc")
                    .map(Value::to_string_value)
                    .unwrap_or_else(|| "utf-8".to_string());
                // Port 0 means "let the OS pick a free ephemeral port" (Raku
                // semantics). Pass it straight to `bind()`: `local_addr()` below
                // reports the port the kernel actually assigned, which is what
                // `.socket-port` hands back. Substituting a port from a
                // PROCESS-LOCAL counter here — as this used to — made every mutsu
                // process start at the same number and consult only its own
                // listener map for occupancy, so two concurrent processes both
                // picked it and one failed to bind (t/io-socket-recv-limit.t and
                // t/io-socket-async-bin.t collided under `prove -j4`, which is what
                // the "flaky S17/io" label was actually made of).
                let port = requested_port;

                let host_ok = host == "0.0.0.0"
                    || host == "::"
                    || host == "127.0.0.1"
                    || host == "::1"
                    || host == "localhost"
                    || host.parse::<std::net::IpAddr>().is_ok();
                if !host_ok {
                    if let Some(q) = quit_cb.clone() {
                        let mut attrs = HashMap::new();
                        attrs.insert(
                            "message".to_string(),
                            Value::str(format!("Failed to resolve '{}'", host)),
                        );
                        let ex = Value::make_instance(Symbol::intern("Exception"), attrs);
                        let _ = self.call_sub_value(q, vec![ex]);
                    }
                    return Ok(failed_listen_socket(format!("Failed to resolve '{}'", host)));
                }

                // Bind a real TCP listener (`runtime::tcp_core`, shared with
                // `nqp::asynclisten`).
                let tcp_listener = match crate::runtime::tcp_core::bind_tcp_listener(&host, port) {
                    Ok(l) => l,
                    Err(err) => {
                        let message = match err {
                            crate::runtime::tcp_core::TcpBindError::Resolve => {
                                format!("Failed to resolve '{}'", host)
                            }
                            crate::runtime::tcp_core::TcpBindError::Io(e) => {
                                format!("Failed to bind: {}", e)
                            }
                        };
                        if let Some(q) = quit_cb {
                            let mut attrs = HashMap::new();
                            attrs.insert("message".to_string(), Value::str(message.clone()));
                            let ex = Value::make_instance(Symbol::intern("Exception"), attrs);
                            let _ = self.call_sub_value(q, vec![ex]);
                        }
                        return Ok(failed_listen_socket(message));
                    }
                };
                // Get the actual bound port (in case port was 0)
                let actual_port = tcp_listener.local_addr().map(|a| a.port()).unwrap_or(port);

                let listener_id = next_async_listener_id();
                register_async_listener(listener_id, AsyncSocketListenerState { closed: false });

                // Set up a closed flag for the accept thread
                let closed_flag = Arc::new(AtomicBool::new(false));
                register_listener_closed_flag(listener_id, closed_flag.clone());
                // Raised by the accept thread once it has dropped the OS
                // listener, so `Tap.close` can wait for the port to be free
                // again instead of returning while it is still accepting.
                let stopped_flag = Arc::new(AtomicBool::new(false));
                register_listener_stopped_flag(listener_id, stopped_flag.clone());

                // Create a supply channel so the react event loop can
                // receive accepted connections
                let supply_id = next_supply_id();
                let (tx, rx) = super::supply_channel::supply_event_channel();
                if let Ok(mut map) = supply_channel_map().lock() {
                    map.insert(supply_id, rx);
                }

                let accept_host = host.clone();
                let accept_enc = enc;
                // Accept from the socket reactor (#12968): an idle listener
                // wakes no thread. The handlers build `Gc` values (the
                // connection Instance) on the reactor, a registered GC helper.
                let stopped_on_refusal = stopped_flag.clone();
                let on_conn: crate::runtime::tcp_core::OnConn = {
                    let tx = tx.clone();
                    Box::new(move |stream, peer_addr| {
                        let conn_id = next_async_socket_id();
                        // Store the TcpStream globally
                        if let Ok(cloned) = stream.try_clone() {
                            register_tcp_stream(conn_id, cloned);
                        }

                        let mut conn_attrs = HashMap::new();
                        conn_attrs.insert("conn-id".to_string(), Value::int(conn_id as i64));
                        conn_attrs.insert("tcp-real".to_string(), Value::TRUE);
                        conn_attrs.insert("socket-host".to_string(), Value::str(accept_host.clone()));
                        conn_attrs.insert("socket-port".to_string(), Value::int(actual_port as i64));
                        conn_attrs.insert(
                            "peer-host".to_string(),
                            Value::str(peer_addr.ip().to_string()),
                        );
                        conn_attrs.insert("peer-port".to_string(), Value::int(peer_addr.port() as i64));
                        conn_attrs.insert("enc".to_string(), Value::str(accept_enc.clone()));
                        let conn_val =
                            Value::make_instance(Symbol::intern("IO::Socket::Async"), conn_attrs);
                        tx.send(SupplyEvent::Emit(conn_val)).is_ok()
                    })
                };
                // The OS listener is already dropped, freeing the port, when
                // this runs, so a `Tap.close` that waits on the stopped flag can
                // rely on the listener being gone once it returns. The backlog
                // is drained after a close, or the accept failed: the Supply
                // ends. A dropped receiver needs no `Done`.
                let on_end: crate::runtime::tcp_core::OnEnd = Box::new(move |end| {
                    if !matches!(end, crate::runtime::tcp_core::AcceptEnd::Stopped) {
                        let _ = tx.send(SupplyEvent::Done);
                    }
                    stopped_flag.store(true, Ordering::SeqCst);
                });
                let spawned = crate::runtime::tcp_core::serve_listener(
                    "sock-async",
                    tcp_listener,
                    closed_flag,
                    on_conn,
                    on_end,
                );
                // A refused accept thread (#9401) ends the listener (the OS
                // listener was dropped with the closure) and is reported the
                // way a failed bind is: through the tap's `quit`, or as a
                // catchable X::AdHoc when there is none.
                if let Err(err) = spawned {
                    close_async_listener(listener_id);
                    stopped_on_refusal.store(true, Ordering::SeqCst);
                    discard_supply_channel(supply_id);
                    let Some(q) = quit_cb else {
                        return Err(err);
                    };
                    let mut attrs = HashMap::new();
                    attrs.insert("message".to_string(), Value::str(err.message.to_string()));
                    let ex = Value::make_instance(Symbol::intern("X::AdHoc"), attrs);
                    let _ = self.call_sub_value(q, vec![ex]);
                    return Ok(failed_listen_socket(err.message.to_string()));
                }

                // Build a Supply instance that the react event loop can subscribe to
                let mut supply_attrs = HashMap::new();
                supply_attrs.insert("values".to_string(), Value::array(Vec::new()));
                supply_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                supply_attrs.insert("live".to_string(), Value::TRUE);
                supply_attrs.insert("supply_id".to_string(), Value::int(supply_id as i64));
                // Carry the listener handle on the Supply so a downstream
                // consumer (the supply-block tap path) can point its Tap at
                // the OS listener — closing that outer tap must stop
                // listening, exactly like closing the Tap returned here.
                supply_attrs.insert("listener-id".to_string(), Value::int(listener_id as i64));
                let supply_val = Value::make_instance(Symbol::intern("Supply"), supply_attrs);

                // If we are inside a react block, register the subscription so the
                // react event loop drains the accept channel. Otherwise this is a
                // BARE `.tap()`: react/whenever has an event loop that drains the
                // channel, a bare tap does not, so drive the callback ourselves on
                // a worker thread (like `start {}` — a cloned interpreter runs the
                // callback per accepted connection; the callback writes to the real
                // TcpStream, so a client on the main thread sees the data over the
                // OS socket).
                if !self.async_state.supply_emit_buffer.is_empty() {
                    // Same 5-element shape as every other whenever subscription
                    // marker ([source, body, [LAST…], [QUIT…], id]) so the supply-
                    // block tap path recognises it; a 2-element array fell
                    // through as a plain emitted value there. The listener
                    // whenever's own LAST/QUIT phasers are not routed here.
                    // TODO: thread them through run_whenever_with_value.
                    let sub = Value::array(vec![
                        supply_val,
                        callback,
                        Value::array(Vec::new()),
                        Value::array(Vec::new()),
                        Value::NIL,
                    ]);
                    if let Some(last) = self.async_state.supply_emit_buffer.last_mut() {
                        last.push(sub);
                    }
                } else if !matches!(callback.view(), ValueView::Nil) {
                    // Per-listener accept driver: the same event-driven act
                    // loop as any channel-backed tap (#12892), so an idle
                    // listener holds no worker.
                    if let Some(rx) = take_supply_channel(supply_id) {
                        super::ActLoop::new(self.clone_for_thread(), callback).start(rx, None);
                    }
                }

                let socket_port = SharedPromise::new();
                socket_port.keep(Value::int(actual_port as i64), String::new(), String::new());
                let socket_host = SharedPromise::new();
                socket_host.keep(Value::str(host), String::new(), String::new());

                let mut tap_attrs = HashMap::new();
                tap_attrs.insert("listener-id".to_string(), Value::int(listener_id as i64));
                tap_attrs.insert("socket-port".to_string(), Value::promise(socket_port));
                tap_attrs.insert("socket-host".to_string(), Value::promise(socket_host));
                Ok(Value::make_instance(
                    Symbol::intern("IO::Socket::Async::ListenSocket"),
                    tap_attrs,
                ))
            }
            _ => Err(RuntimeError::new(format!(
                "No method '{}' on IO::Socket::Async::Listener",
                method
            ))),
        }
    }

    /// The id of a UDP socket's bound state.
    // Cost: O(1).
    fn udp_socket_id(attributes: &AttrMap) -> Option<u64> {
        attributes
            .get("udp-socket-id")
            .and_then(|v| match v.view() {
                ValueView::Int(i) => Some(i as u64),
                _ => None,
            })
    }

    /// `close` on a UDP socket: done every supply it feeds.
    // Cost: O(s), s = the socket's supplies.
    pub(super) fn udp_socket_close(&mut self, attributes: &AttrMap) {
        if let Some(id) = Self::udp_socket_id(attributes)
            && let Some(state) = get_udp_bound_socket(id)
        {
            update_udp_bound_socket(id, |s| s.closed = true);
            for sid in &state.supply_ids {
                self.async_supplier_done_value(*sid);
            }
        }
    }

    /// `Supply` on a UDP socket: a live supply of the datagrams it receives.
    // Cost: O(1).
    pub(super) fn udp_socket_supply(&mut self, attributes: &AttrMap) -> Result<Value, RuntimeError> {
        let id = Self::udp_socket_id(attributes)
            .ok_or_else(|| RuntimeError::new("Missing UDP socket id for Supply"))?;
        // Stamped as `supplier_id` below, so it must come from the
        // SUPPLIER counter: every downstream registration
        // (register_supplier_tap / supplier_done / emit) keys the
        // supplier registry with it, and `next_supply_id()` is a
        // separate counter that also starts at 1 -- drawing from it
        // cross-delivered a same-numbered `Supplier.new`'s emissions
        // to this socket's tap.
        let supply_id = next_supplier_id();
        update_udp_bound_socket(id, |s| s.supply_ids.push(supply_id));

        let mut attrs = HashMap::new();
        attrs.insert("values".to_string(), Value::array(Vec::new()));
        attrs.insert("taps".to_string(), Value::array(Vec::new()));
        attrs.insert("live".to_string(), Value::TRUE);
        attrs.insert("supplier_id".to_string(), Value::int(supply_id as i64));
        Ok(Value::make_instance(Symbol::intern("Supply"), attrs))
    }

    /// `IO::Socket::Async.print-to($host, $port, $str)` /
    /// `.write-to($host, $port, $buf)` on a UDP socket: a `Promise` kept once
    /// the datagram is sent.
    // Cost: O(n), n = bytes sent.
    pub(crate) fn async_socket_send_to(
        &mut self,
        attributes: &AttrMap,
        args: &[Value],
        text: bool,
    ) -> Result<Value, RuntimeError> {
        if !Self::async_socket_is_udp(attributes) {
            let method = if text { "print-to" } else { "write-to" };
            return Err(crate::runtime::did_you_mean::method_not_found(
                method,
                "IO::Socket::Async",
            ));
        }
        let host = args
            .first()
            .map(Value::to_string_value)
            .unwrap_or_else(|| "127.0.0.1".to_string());
        let port = args
            .get(1)
            .map(|v| match v.view() {
                ValueView::Int(i) => i as u16,
                ValueView::Num(f) => f as u16,
                _ => v.to_string_value().parse::<u16>().unwrap_or(0),
            })
            .unwrap_or(0);
        let bytes = if text {
            args.get(2)
                .map(|v| self.render_str_value(v))
                .unwrap_or_default()
                .into_bytes()
        } else {
            args.get(2).and_then(Self::extract_bytes).unwrap_or_else(|| {
                args.get(2)
                    .map(Value::to_string_value)
                    .unwrap_or_default()
                    .into_bytes()
            })
        };

        self.udp_deliver_data(&host, port, &bytes)?;

        let promise = SharedPromise::new();
        promise.keep(
            Self::async_socket_kept(Value::TRUE),
            String::new(),
            String::new(),
        );
        Ok(Value::promise(promise))
    }

    /// Deliver UDP data to a bound socket listening on host:port.
    fn udp_deliver_data(
        &mut self,
        host: &str,
        port: u16,
        bytes: &[u8],
    ) -> Result<(), RuntimeError> {
        if let Some((_id, state)) = lookup_udp_bound_socket(host, port) {
            let text = String::from_utf8_lossy(bytes).to_string();
            for sid in &state.supply_ids {
                let _ = self.async_supplier_emit_value(*sid, Value::str(text.clone()));
            }
        }
        Ok(())
    }
}

/// The `IO::Socket::Async::ListenSocket` a listen that could not start answers:
/// its `socket-port` and `socket-host` are broken with why, so awaiting them
/// throws instead of waiting forever.
// Cost: O(m), m = chars of the message.
fn failed_listen_socket(message: String) -> Value {
    let mut attrs = HashMap::new();
    for key in ["socket-port", "socket-host"] {
        let promise = SharedPromise::new();
        let mut ex = HashMap::new();
        ex.insert("message".to_string(), Value::str(message.clone()));
        promise.break_with(
            Value::make_instance(Symbol::intern("X::AdHoc"), ex),
            String::new(),
            String::new(),
        );
        attrs.insert(key.to_string(), Value::promise(promise));
    }
    Value::make_instance(Symbol::intern("IO::Socket::Async::ListenSocket"), attrs)
}
