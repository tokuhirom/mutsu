use crate::runtime::*;
use crate::symbol::Symbol;

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc;

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
        supplier_emit(supplier_id, value.clone());
        let actions = supplier_emit_callbacks(supplier_id, &value);
        for action in actions {
            match action {
                SupplierEmitAction::Call(tap, emitted, delay_seconds) => {
                    Self::sleep_for_supply_delay(delay_seconds);
                    let _ = self.call_sub_value(tap, vec![emitted], true);
                }
                SupplierEmitAction::UniqueCheck {
                    callback,
                    value: val,
                    delay_seconds,
                    ..
                } => {
                    Self::sleep_for_supply_delay(delay_seconds);
                    let _ = self.call_sub_value(callback, vec![val], true);
                }
                SupplierEmitAction::ClassifyCheck {
                    value: val,
                    tap_index,
                } => {
                    let _ = self.handle_classify_emit(supplier_id, tap_index, val);
                }
                SupplierEmitAction::HeadLimitReached { supplier_id: sid2 } => {
                    let deferred_promises = supplier_done_deferred(sid2);
                    for done_cb in take_supplier_done_callbacks(sid2) {
                        let _ = self.invoke_done_callback(done_cb);
                    }
                    for (promise, result) in deferred_promises {
                        promise.keep(result, String::new(), String::new());
                    }
                }
                SupplierEmitAction::ProduceCall {
                    callback,
                    callable,
                    value: val,
                    accumulator,
                    delay_seconds,
                    tap_index,
                } => {
                    let new_acc = if let Some(acc) = accumulator {
                        self.call_sub_value(callable, vec![acc, val], false)
                            .unwrap_or(Value::NIL)
                    } else {
                        val
                    };
                    supplier_produce_update_acc(supplier_id, tap_index, new_acc.clone());
                    Self::sleep_for_supply_delay(delay_seconds);
                    let _ = self.call_sub_value(callback, vec![new_acc], true);
                }
                SupplierEmitAction::StartCall {
                    callable,
                    value: val,
                    output_supplier_id,
                } => {
                    self.run_start_call_in_thread(callable, val, output_supplier_id);
                }
                SupplierEmitAction::BatchEmit {
                    downstream_supplier_id,
                    batch,
                } => {
                    let batch_value = Value::array(batch);
                    supplier_emit(downstream_supplier_id, batch_value.clone());
                    let ds_actions = supplier_emit_callbacks(downstream_supplier_id, &batch_value);
                    for ds_action in ds_actions {
                        if let SupplierEmitAction::Call(tap, emitted, delay_seconds) = ds_action {
                            Self::sleep_for_supply_delay(delay_seconds);
                            let _ = self.call_sub_value(tap, vec![emitted], true);
                        }
                    }
                }
                SupplierEmitAction::FlatEmit {
                    downstream_supplier_id,
                    items,
                } => {
                    for item in items {
                        supplier_emit(downstream_supplier_id, item.clone());
                        let ds_actions = supplier_emit_callbacks(downstream_supplier_id, &item);
                        for ds_action in ds_actions {
                            if let SupplierEmitAction::Call(tap, emitted, delay_seconds) = ds_action
                            {
                                Self::sleep_for_supply_delay(delay_seconds);
                                let _ = self.call_sub_value(tap, vec![emitted], true);
                            }
                        }
                    }
                }
                SupplierEmitAction::ZipBuffer {
                    zip_state_id: zid,
                    source_index: si,
                    value: val,
                } => {
                    let result = zip_buffer_value(zid, si, val);
                    if let ZipAction::Emit(tuple_val) = result {
                        let (output_sid, wf) = zip_state_info(zid);
                        let emit_val = if let Some(wfn) = wf {
                            if let ValueView::Array(items, ..) = tuple_val.view() {
                                self.call_sub_value(wfn, items.to_vec(), false)
                                    .unwrap_or(tuple_val)
                            } else {
                                tuple_val
                            }
                        } else {
                            tuple_val
                        };
                        supplier_emit(output_sid, emit_val.clone());
                        let ds_actions = supplier_emit_callbacks(output_sid, &emit_val);
                        for da in ds_actions {
                            if let SupplierEmitAction::Call(tap, emitted, delay_seconds) = da {
                                Self::sleep_for_supply_delay(delay_seconds);
                                let _ = self.call_sub_value(tap, vec![emitted], true);
                            }
                        }
                    }
                }
                SupplierEmitAction::ZipLatestBuffer {
                    zip_latest_state_id: zid,
                    source_index: si,
                    value: val,
                } => {
                    let result = zip_latest_buffer_value(zid, si, val);
                    if let ZipAction::Emit(tuple_val) = result {
                        let (output_sid, wf) = zip_latest_state_info(zid);
                        let emit_val = if let Some(wfn) = wf {
                            if let ValueView::Array(items, ..) = tuple_val.view() {
                                self.call_sub_value(wfn, items.to_vec(), false)
                                    .unwrap_or(tuple_val)
                            } else {
                                tuple_val
                            }
                        } else {
                            tuple_val
                        };
                        supplier_emit(output_sid, emit_val.clone());
                        let ds_actions = supplier_emit_callbacks(output_sid, &emit_val);
                        for da in ds_actions {
                            if let SupplierEmitAction::Call(tap, emitted, delay_seconds) = da {
                                Self::sleep_for_supply_delay(delay_seconds);
                                let _ = self.call_sub_value(tap, vec![emitted], true);
                            }
                        }
                    }
                }
                SupplierEmitAction::Migrate {
                    value: val,
                    master_supplier_id,
                    downstream_supplier_id,
                    tap_index,
                } => {
                    self.handle_supply_migrate(
                        val,
                        master_supplier_id,
                        downstream_supplier_id,
                        tap_index,
                    )?;
                }
                SupplierEmitAction::ForwardEmit {
                    downstream_supplier_id,
                    value: val,
                } => {
                    self.handle_supply_forward(downstream_supplier_id, val)?;
                }
                SupplierEmitAction::TransformCall {
                    downstream_supplier_id,
                    callable,
                    mode,
                    value: val,
                } => {
                    self.handle_supply_transform_emit(downstream_supplier_id, callable, mode, val)?;
                }
            }
        }
        Ok(())
    }

    pub(super) fn async_supplier_done_value(&mut self, supplier_id: u64) {
        supplier_done(supplier_id);
        for (tap, emitted) in flush_supplier_line_taps(supplier_id) {
            let _ = self.call_sub_value(tap, vec![emitted], true);
        }
        for (tap, emitted) in flush_supplier_words_taps(supplier_id) {
            let _ = self.call_sub_value(tap, vec![emitted], true);
        }
        for done_cb in take_supplier_done_callbacks(supplier_id) {
            let _ = self.invoke_done_callback(done_cb);
        }
    }

    pub(super) fn async_supplier_quit_value(&mut self, supplier_id: u64, reason: Value) {
        supplier_quit(supplier_id, reason.clone());
        for (tap, emitted) in flush_supplier_line_taps(supplier_id) {
            let _ = self.call_sub_value(tap, vec![emitted], true);
        }
        for (tap, emitted) in flush_supplier_words_taps(supplier_id) {
            let _ = self.call_sub_value(tap, vec![emitted], true);
        }
        for quit_cb in take_supplier_quit_callbacks(supplier_id) {
            let _ = self.call_sub_value(quit_cb, vec![reason.clone()], true);
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
                let port = if requested_port == 0 {
                    allocate_async_listen_port()
                } else {
                    requested_port
                };

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
                        let _ = self.call_sub_value(q, vec![ex], true);
                    }
                    return Ok(Value::make_instance(Symbol::intern("Tap"), HashMap::new()));
                }

                // Bind a real TCP listener (retry briefly if port is still held
                // by a recently-closed listener thread)
                let bind_addr = format!("{}:{}", host, port);
                let tcp_listener = {
                    let mut last_err = None;
                    let mut bound = None;
                    for attempt in 0..10 {
                        match std::net::TcpListener::bind(&bind_addr) {
                            Ok(l) => {
                                bound = Some(l);
                                break;
                            }
                            Err(e) if e.kind() == std::io::ErrorKind::AddrInUse && attempt < 9 => {
                                std::thread::sleep(std::time::Duration::from_millis(20));
                                last_err = Some(e);
                            }
                            Err(e) => {
                                last_err = Some(e);
                                break;
                            }
                        }
                    }
                    match bound {
                        Some(l) => l,
                        None => {
                            let e = last_err.unwrap();
                            if let Some(q) = quit_cb {
                                let mut attrs = HashMap::new();
                                attrs.insert(
                                    "message".to_string(),
                                    Value::str(format!("Failed to bind: {}", e)),
                                );
                                let ex = Value::make_instance(Symbol::intern("Exception"), attrs);
                                let _ = self.call_sub_value(q, vec![ex], true);
                            }
                            return Ok(Value::make_instance(Symbol::intern("Tap"), HashMap::new()));
                        }
                    }
                };
                // Get the actual bound port (in case port was 0)
                let actual_port = tcp_listener.local_addr().map(|a| a.port()).unwrap_or(port);

                let listener_id = next_async_listener_id();
                register_async_listener(
                    listener_id,
                    AsyncSocketListenerState {
                        host: host.clone(),
                        port: actual_port,
                        callback: callback.clone(),
                        closed: false,
                        encoding: enc.clone(),
                    },
                );

                // Set up a closed flag for the accept thread
                let closed_flag = Arc::new(AtomicBool::new(false));
                register_listener_closed_flag(listener_id, closed_flag.clone());

                // Create a supply channel so the react event loop can
                // receive accepted connections
                let supply_id = next_supply_id();
                let (tx, rx) = mpsc::channel::<SupplyEvent>();
                if let Ok(mut map) = supply_channel_map().lock() {
                    map.insert(supply_id, rx);
                }

                // Set a short accept timeout so the thread can check
                // the closed flag periodically
                let _ = tcp_listener
                    .set_nonblocking(false)
                    .and_then(|_| tcp_listener.set_nonblocking(false));

                let accept_host = host.clone();
                let accept_enc = enc.clone();
                // Start a background thread to accept connections
                std::thread::spawn(move || {
                    // Use a short timeout on accept so we can check the closed flag
                    // TcpListener doesn't have set_timeout, so we use non-blocking + sleep
                    let _ = tcp_listener.set_nonblocking(true);
                    loop {
                        if closed_flag.load(Ordering::SeqCst) {
                            let _ = tx.send(SupplyEvent::Done);
                            break;
                        }
                        match tcp_listener.accept() {
                            Ok((stream, peer_addr)) => {
                                let conn_id = next_async_socket_id();
                                // Store the TcpStream globally
                                if let Ok(cloned) = stream.try_clone() {
                                    register_tcp_stream(conn_id, cloned);
                                }

                                let mut conn_attrs = HashMap::new();
                                conn_attrs
                                    .insert("conn-id".to_string(), Value::int(conn_id as i64));
                                conn_attrs.insert("tcp-real".to_string(), Value::TRUE);
                                conn_attrs.insert(
                                    "socket-host".to_string(),
                                    Value::str(accept_host.clone()),
                                );
                                conn_attrs.insert(
                                    "socket-port".to_string(),
                                    Value::int(actual_port as i64),
                                );
                                conn_attrs.insert(
                                    "peer-host".to_string(),
                                    Value::str(peer_addr.ip().to_string()),
                                );
                                conn_attrs.insert(
                                    "peer-port".to_string(),
                                    Value::int(peer_addr.port() as i64),
                                );
                                conn_attrs
                                    .insert("enc".to_string(), Value::str(accept_enc.clone()));
                                let conn_val = Value::make_instance(
                                    Symbol::intern("IO::Socket::Async"),
                                    conn_attrs,
                                );
                                if tx.send(SupplyEvent::Emit(conn_val)).is_err() {
                                    break;
                                }
                            }
                            Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                                // No pending connection, sleep briefly
                                std::thread::sleep(std::time::Duration::from_millis(10));
                            }
                            Err(_) => {
                                // Fatal accept error, stop
                                let _ = tx.send(SupplyEvent::Done);
                                break;
                            }
                        }
                    }
                });

                // Build a Supply instance that the react event loop can subscribe to
                let mut supply_attrs = HashMap::new();
                supply_attrs.insert("values".to_string(), Value::array(Vec::new()));
                supply_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                supply_attrs.insert("live".to_string(), Value::TRUE);
                supply_attrs.insert("supply_id".to_string(), Value::int(supply_id as i64));
                let supply_val = Value::make_instance(Symbol::intern("Supply"), supply_attrs);

                // If we are inside a react block, register the subscription so the
                // react event loop drains the accept channel. Otherwise this is a
                // BARE `.tap()`: react/whenever has an event loop that drains the
                // channel, a bare tap does not, so drive the callback ourselves on
                // a worker thread (like `start {}` — a cloned interpreter runs the
                // callback per accepted connection; the callback writes to the real
                // TcpStream, so a client on the main thread sees the data over the
                // OS socket).
                if !self.supply_emit_buffer.is_empty() {
                    let sub = Value::array(vec![supply_val, callback]);
                    if let Some(last) = self.supply_emit_buffer.last_mut() {
                        last.push(sub);
                    }
                } else if !matches!(callback.view(), ValueView::Nil) {
                    let cb = callback.clone();
                    let mut driver = self.clone_for_thread();
                    crate::runtime::builtins_system::spawn_user_thread(move || {
                        let Some(rx) = take_supply_channel(supply_id) else {
                            return;
                        };
                        while let Ok(event) = rx.recv() {
                            match event {
                                SupplyEvent::Emit(conn) => {
                                    let _ = crate::vm::guard_worker_panic(|| {
                                        driver.call_sub_value(cb.clone(), vec![conn], true)
                                    });
                                }
                                SupplyEvent::Done | SupplyEvent::Quit(_) => break,
                            }
                        }
                    });
                }

                let socket_port = SharedPromise::new();
                socket_port.keep(Value::int(actual_port as i64), String::new(), String::new());
                let socket_host = SharedPromise::new();
                socket_host.keep(Value::str(host), String::new(), String::new());

                let mut tap_attrs = HashMap::new();
                tap_attrs.insert("listener-id".to_string(), Value::int(listener_id as i64));
                tap_attrs.insert(
                    "socket-port".to_string(),
                    Value::promise(socket_port.clone()),
                );
                tap_attrs.insert("socket-host".to_string(), Value::promise(socket_host));
                Ok(Value::make_instance(Symbol::intern("Tap"), tap_attrs))
            }
            _ => Err(RuntimeError::new(format!(
                "No method '{}' on IO::Socket::Async::Listener",
                method
            ))),
        }
    }

    /// Handle instance methods on UDP sockets (created by bind-udp / udp).
    pub(super) fn native_socket_async_udp(
        &mut self,
        attributes: &AttrMap,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let udp_id = attributes
            .get("udp-socket-id")
            .and_then(|v| match v.view() {
                ValueView::Int(i) => Some(i as u64),
                _ => None,
            });

        match method {
            "socket-port" => Ok(attributes
                .get("socket-port")
                .cloned()
                .unwrap_or(Value::int(0))),
            "socket-host" => Ok(attributes
                .get("socket-host")
                .cloned()
                .unwrap_or_else(|| Value::str_from("0.0.0.0"))),
            "close" => {
                if let Some(id) = udp_id
                    && let Some(state) = get_udp_bound_socket(id)
                {
                    update_udp_bound_socket(id, |s| s.closed = true);
                    for sid in &state.supply_ids {
                        self.async_supplier_done_value(*sid);
                    }
                }
                Ok(Value::NIL)
            }
            "Supply" => {
                let id =
                    udp_id.ok_or_else(|| RuntimeError::new("Missing UDP socket id for Supply"))?;
                let supply_id = next_supply_id();
                register_async_supply(
                    supply_id,
                    AsyncSocketSupplyState {
                        is_bin: false,
                        encoding: "utf-8".to_string(),
                        text_buffer: String::new(),
                        byte_buffer: Vec::new(),
                    },
                );
                update_udp_bound_socket(id, |s| s.supply_ids.push(supply_id));

                let mut attrs = HashMap::new();
                attrs.insert("values".to_string(), Value::array(Vec::new()));
                attrs.insert("taps".to_string(), Value::array(Vec::new()));
                attrs.insert("live".to_string(), Value::TRUE);
                attrs.insert("supplier_id".to_string(), Value::int(supply_id as i64));
                Ok(Value::make_instance(Symbol::intern("Supply"), attrs))
            }
            "print-to" => {
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
                let text = args
                    .get(2)
                    .map(|v| self.render_str_value(v))
                    .unwrap_or_default();

                self.udp_deliver_data(&host, port, text.as_bytes())?;

                let promise = SharedPromise::new();
                promise.keep(
                    Self::async_socket_kept(Value::TRUE),
                    String::new(),
                    String::new(),
                );
                Ok(Value::promise(promise))
            }
            "write-to" => {
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
                let bytes = args
                    .get(2)
                    .and_then(Self::extract_bytes)
                    .unwrap_or_else(|| {
                        args.get(2)
                            .map(Value::to_string_value)
                            .unwrap_or_default()
                            .into_bytes()
                    });

                self.udp_deliver_data(&host, port, &bytes)?;

                let promise = SharedPromise::new();
                promise.keep(
                    Self::async_socket_kept(Value::TRUE),
                    String::new(),
                    String::new(),
                );
                Ok(Value::promise(promise))
            }
            _ => Err(RuntimeError::new(format!(
                "No method '{}' on IO::Socket::Async (UDP)",
                method
            ))),
        }
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
