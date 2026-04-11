use crate::runtime::*;
use crate::symbol::Symbol;

use super::state::*;
use super::state_supplier::*;

impl Interpreter {
    fn async_socket_status_result(status: &str, result: Value, cause: Value) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("status".to_string(), Value::str_from(status));
        attrs.insert("result".to_string(), result);
        attrs.insert("cause".to_string(), cause);
        Value::make_instance(Symbol::intern("IO::Socket::Async::StatusResult"), attrs)
    }

    fn async_socket_kept(result: Value) -> Value {
        Self::async_socket_status_result("Kept", result, Value::Nil)
    }

    fn async_socket_broken(cause: Value) -> Value {
        Self::async_socket_status_result("Broken", Value::Nil, cause)
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
                        let _ = self.call_sub_value(done_cb, Vec::new(), true);
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
                            .unwrap_or(Value::Nil)
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
            let _ = self.call_sub_value(done_cb, Vec::new(), true);
        }
    }

    fn async_supplier_quit_value(&mut self, supplier_id: u64, reason: Value) {
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
            let _ = self.call_sub_value(done_cb, Vec::new(), true);
        }
    }

    pub(in crate::runtime) fn native_socket_async_listener(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "tap" | "act" => {
                let callback = args.first().cloned().unwrap_or(Value::Nil);
                let quit_cb = Self::named_value(&args, "quit");
                let host = attributes
                    .get("host")
                    .map(Value::to_string_value)
                    .unwrap_or_else(|| "0.0.0.0".to_string());
                let requested_port = attributes
                    .get("port")
                    .and_then(|v| match v {
                        Value::Int(i) => Some(*i as u16),
                        Value::Num(n) if n.is_finite() => Some(*n as u16),
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

                if async_port_in_use(&host, port) {
                    if let Some(q) = quit_cb.clone() {
                        let mut attrs = HashMap::new();
                        attrs.insert(
                            "message".to_string(),
                            Value::str_from("Address already in use"),
                        );
                        let ex = Value::make_instance(Symbol::intern("Exception"), attrs);
                        let _ = self.call_sub_value(q, vec![ex], true);
                    }
                    return Ok(Value::make_instance(Symbol::intern("Tap"), HashMap::new()));
                }

                let listener_id = next_async_listener_id();
                register_async_listener(
                    listener_id,
                    AsyncSocketListenerState {
                        host: host.clone(),
                        port,
                        callback,
                        closed: false,
                        encoding: enc,
                    },
                );

                let socket_port = SharedPromise::new();
                socket_port.keep(Value::Int(port as i64), String::new(), String::new());
                let socket_host = SharedPromise::new();
                socket_host.keep(Value::str(host), String::new(), String::new());

                let mut tap_attrs = HashMap::new();
                tap_attrs.insert("listener-id".to_string(), Value::Int(listener_id as i64));
                tap_attrs.insert(
                    "socket-port".to_string(),
                    Value::Promise(socket_port.clone()),
                );
                tap_attrs.insert("socket-host".to_string(), Value::Promise(socket_host));
                Ok(Value::make_instance(Symbol::intern("Tap"), tap_attrs))
            }
            _ => Err(RuntimeError::new(format!(
                "No method '{}' on IO::Socket::Async::Listener",
                method
            ))),
        }
    }

    pub(in crate::runtime) fn native_socket_async(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let conn_id = attributes.get("conn-id").and_then(|v| match v {
            Value::Int(i) => Some(*i as u64),
            _ => None,
        });

        match method {
            "socket-port" => Ok(attributes
                .get("socket-port")
                .cloned()
                .unwrap_or(Value::Int(0))),
            "peer-port" => Ok(attributes
                .get("peer-port")
                .cloned()
                .unwrap_or(Value::Int(0))),
            "socket-host" => Ok(attributes
                .get("socket-host")
                .cloned()
                .unwrap_or_else(|| Value::str_from("0.0.0.0"))),
            "peer-host" => Ok(attributes
                .get("peer-host")
                .cloned()
                .unwrap_or_else(|| Value::str_from("0.0.0.0"))),
            "close" => {
                if let Some(id) = conn_id
                    && let Some(state) = get_async_connection(id)
                {
                    update_async_connection(id, |conn| {
                        conn.closed = true;
                    });
                    for sid in &state.supply_ids {
                        self.async_supplier_done_value(*sid);
                    }
                    if let Some(peer_id) = state.peer_id
                        && let Some(peer) = get_async_connection(peer_id)
                    {
                        update_async_connection(peer_id, |conn| {
                            conn.peer_closed = true;
                        });
                        if let Some((callback, socket)) = take_deferred_accept_callback(peer_id) {
                            let _ = self.call_sub_value(callback, vec![socket], true);
                        }
                        for sid in &peer.supply_ids {
                            if let Some(supply) = get_async_supply(*sid)
                                && !supply.is_bin
                                && !supply.text_buffer.is_empty()
                            {
                                let _ = self.async_supplier_emit_value(
                                    *sid,
                                    Value::str(supply.text_buffer.clone()),
                                );
                                update_async_supply(*sid, |st| st.text_buffer.clear());
                            }
                            self.async_supplier_done_value(*sid);
                        }
                    }
                }
                Ok(Value::Nil)
            }
            "Supply" => {
                let id = conn_id.ok_or_else(|| RuntimeError::new("Missing async conn-id"))?;
                let is_bin = Self::named_bool(&args, "bin");
                let supply_enc = Self::named_value(&args, "enc")
                    .map(|v| v.to_string_value())
                    .or_else(|| attributes.get("enc").map(Value::to_string_value))
                    .unwrap_or_else(|| "utf-8".to_string());
                let supply_id = next_supply_id();
                register_async_supply(
                    supply_id,
                    AsyncSocketSupplyState {
                        is_bin,
                        encoding: supply_enc,
                        text_buffer: String::new(),
                        byte_buffer: Vec::new(),
                    },
                );
                update_async_connection(id, |conn| conn.supply_ids.push(supply_id));

                let mut initial_values: Vec<Value> = Vec::new();
                let mut is_supplier_done = false;
                let mut attrs = HashMap::new();
                attrs.insert("values".to_string(), Value::array(Vec::new()));
                attrs.insert("taps".to_string(), Value::array(Vec::new()));
                attrs.insert("live".to_string(), Value::Bool(true));
                attrs.insert("supplier_id".to_string(), Value::Int(supply_id as i64));
                if let Some(conn_state) = get_async_connection(id) {
                    is_supplier_done = conn_state.closed || conn_state.peer_closed;
                    if !conn_state.pending_bytes.is_empty() {
                        if is_bin {
                            let mut battrs = HashMap::new();
                            battrs.insert(
                                "bytes".to_string(),
                                Value::array(
                                    conn_state
                                        .pending_bytes
                                        .iter()
                                        .map(|b| Value::Int(*b as i64))
                                        .collect(),
                                ),
                            );
                            initial_values
                                .push(Value::make_instance(Symbol::intern("Buf"), battrs));
                        } else if let Some(supply) = get_async_supply(supply_id) {
                            let decoded = if supply.encoding.eq_ignore_ascii_case("utf-8") {
                                String::from_utf8_lossy(&conn_state.pending_bytes).to_string()
                            } else if supply.encoding.eq_ignore_ascii_case("latin-1")
                                || supply.encoding.eq_ignore_ascii_case("iso-8859-1")
                            {
                                conn_state
                                    .pending_bytes
                                    .iter()
                                    .map(|b| char::from_u32(*b as u32).unwrap_or('\u{FFFD}'))
                                    .collect()
                            } else {
                                self.decode_with_encoding(
                                    &conn_state.pending_bytes,
                                    &supply.encoding,
                                )
                                .unwrap_or_default()
                            };
                            if !decoded.is_empty() {
                                let mut start = 0usize;
                                for (idx, ch) in decoded.char_indices() {
                                    if ch == '\n' {
                                        initial_values
                                            .push(Value::str(decoded[start..=idx].to_string()));
                                        start = idx + ch.len_utf8();
                                    }
                                }
                                let tail = decoded[start..].to_string();
                                update_async_supply(supply_id, |st| st.text_buffer = tail);
                            }
                        }
                        update_async_connection(id, |conn| conn.pending_bytes.clear());
                    }
                }
                if is_supplier_done
                    && !is_bin
                    && let Some(supply) = get_async_supply(supply_id)
                    && !supply.text_buffer.is_empty()
                {
                    initial_values.push(Value::str(supply.text_buffer.clone()));
                    update_async_supply(supply_id, |st| st.text_buffer.clear());
                }
                attrs.insert("values".to_string(), Value::array(initial_values));
                if is_supplier_done {
                    supplier_done(supply_id);
                    attrs.insert("supplier_done".to_string(), Value::Bool(true));
                    attrs.insert("live".to_string(), Value::Bool(false));
                }
                Ok(Value::make_instance(Symbol::intern("Supply"), attrs))
            }
            "write" | "print" => {
                let promise = SharedPromise::new();
                let result = (|| -> Result<Value, RuntimeError> {
                    let id = conn_id.ok_or_else(|| RuntimeError::new("Missing async conn-id"))?;
                    let state = get_async_connection(id)
                        .ok_or_else(|| RuntimeError::new("Unknown async socket connection"))?;
                    if state.closed {
                        return Ok(Self::async_socket_broken(Value::str_from(
                            "Socket is closed",
                        )));
                    }
                    let peer_id = state
                        .peer_id
                        .ok_or_else(|| RuntimeError::new("Peer missing"))?;
                    let peer = get_async_connection(peer_id)
                        .ok_or_else(|| RuntimeError::new("Unknown peer connection"))?;

                    let bytes = if method == "write" {
                        args.last()
                            .and_then(Self::extract_bytes)
                            .unwrap_or_else(|| {
                                args.last()
                                    .map(Value::to_string_value)
                                    .unwrap_or_default()
                                    .into_bytes()
                            })
                    } else {
                        let text = args
                            .last()
                            .map(|v| self.render_str_value(v))
                            .unwrap_or_default();
                        self.encode_with_encoding(&text, &state.encoding)?
                    };
                    if bytes.is_empty() {
                        return Ok(Self::async_socket_kept(Value::Bool(true)));
                    }
                    if peer.closed {
                        return Ok(Self::async_socket_broken(Value::str_from(
                            "Socket is closed",
                        )));
                    }

                    if peer.supply_ids.is_empty() {
                        update_async_connection(peer_id, |conn| {
                            conn.pending_bytes.extend_from_slice(&bytes);
                        });
                        return Ok(Self::async_socket_kept(Value::Bool(true)));
                    }

                    for sid in &peer.supply_ids {
                        if let Some(supply) = get_async_supply(*sid) {
                            if supply.is_bin {
                                let mut battrs = HashMap::new();
                                battrs.insert(
                                    "bytes".to_string(),
                                    Value::array(
                                        bytes.iter().map(|b| Value::Int(*b as i64)).collect(),
                                    ),
                                );
                                let buf = Value::make_instance(Symbol::intern("Buf"), battrs);
                                let _ = self.async_supplier_emit_value(*sid, buf);
                            } else {
                                let mut pending = supply.byte_buffer.clone();
                                pending.extend_from_slice(&bytes);
                                let (decoded, remainder) = if supply
                                    .encoding
                                    .eq_ignore_ascii_case("utf-8")
                                {
                                    match std::str::from_utf8(&pending) {
                                        Ok(s) => (s.to_string(), Vec::new()),
                                        Err(e) => {
                                            if e.error_len().is_none() {
                                                let valid = &pending[..e.valid_up_to()];
                                                let valid_decoded = std::str::from_utf8(valid)
                                                    .unwrap_or("")
                                                    .to_string();
                                                (valid_decoded, pending[e.valid_up_to()..].to_vec())
                                            } else {
                                                self.async_supplier_quit_value(
                                                    *sid,
                                                    Value::str_from(
                                                        "Malformed UTF-8 on socket Supply",
                                                    ),
                                                );
                                                continue;
                                            }
                                        }
                                    }
                                } else if supply.encoding.eq_ignore_ascii_case("latin-1")
                                    || supply.encoding.eq_ignore_ascii_case("iso-8859-1")
                                {
                                    (
                                        pending
                                            .iter()
                                            .map(|b| {
                                                char::from_u32(*b as u32).unwrap_or('\u{FFFD}')
                                            })
                                            .collect(),
                                        Vec::new(),
                                    )
                                } else {
                                    match self.decode_with_encoding(&pending, &supply.encoding) {
                                        Ok(s) => (s, Vec::new()),
                                        Err(e) => {
                                            self.async_supplier_quit_value(
                                                *sid,
                                                Value::str(e.message),
                                            );
                                            continue;
                                        }
                                    }
                                };
                                let mut merged = supply.text_buffer.clone();
                                merged.push_str(&decoded);
                                let mut start = 0usize;
                                for (idx, ch) in merged.char_indices() {
                                    if ch == '\n' {
                                        let chunk = merged[start..=idx].to_string();
                                        let _ =
                                            self.async_supplier_emit_value(*sid, Value::str(chunk));
                                        start = idx + ch.len_utf8();
                                    }
                                }
                                let tail = merged[start..].to_string();
                                update_async_supply(*sid, |st| {
                                    st.text_buffer = tail;
                                    st.byte_buffer = remainder;
                                });
                            }
                        }
                    }
                    Ok(Self::async_socket_kept(Value::Bool(true)))
                })();

                match result {
                    Ok(v) => promise.keep(v, String::new(), String::new()),
                    Err(e) => promise.keep(
                        Self::async_socket_broken(Value::str(e.message)),
                        String::new(),
                        String::new(),
                    ),
                }
                Ok(Value::Promise(promise))
            }
            _ => Err(RuntimeError::new(format!(
                "No method '{}' on IO::Socket::Async",
                method
            ))),
        }
    }
}
