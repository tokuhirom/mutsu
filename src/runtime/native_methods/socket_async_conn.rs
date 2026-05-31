use crate::runtime::*;
use crate::symbol::Symbol;

use std::sync::mpsc;

use super::state::*;

impl Interpreter {
    pub(in crate::runtime) fn native_socket_async(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // Dispatch to UDP handler if this is a UDP socket
        if matches!(attributes.get("is-udp"), Some(Value::Bool(true))) {
            return self.native_socket_async_udp(attributes, method, args);
        }

        // Check if this is a real TCP connection (from listener accept)
        let is_real_tcp = matches!(attributes.get("tcp-real"), Some(Value::Bool(true)));

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
            "close" if is_real_tcp => {
                if let Some(id) = conn_id {
                    if let Some(stream) = get_tcp_stream(id)
                        && let Ok(s) = stream.lock()
                    {
                        let _ = s.shutdown(std::net::Shutdown::Both);
                    }
                    remove_tcp_stream(id);
                }
                Ok(Value::Nil)
            }
            "close" => {
                self.async_socket_close_in_memory(conn_id)?;
                Ok(Value::Nil)
            }
            "Supply" if is_real_tcp => self.async_socket_supply_real_tcp(conn_id),
            "Supply" => self.async_socket_supply_in_memory(conn_id, &args, attributes),
            "write" | "print" if is_real_tcp => {
                self.async_socket_write_real_tcp(conn_id, method, &args)
            }
            "write" | "print" => self.async_socket_write_in_memory(conn_id, method, &args),
            _ => Err(RuntimeError::new(format!(
                "No method '{}' on IO::Socket::Async",
                method
            ))),
        }
    }

    fn async_socket_close_in_memory(&mut self, conn_id: Option<u64>) -> Result<(), RuntimeError> {
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
        Ok(())
    }

    fn async_socket_supply_real_tcp(
        &mut self,
        conn_id: Option<u64>,
    ) -> Result<Value, RuntimeError> {
        let id = conn_id.ok_or_else(|| RuntimeError::new("Missing async conn-id"))?;
        let supply_id = next_supply_id();
        let (tx, rx) = mpsc::channel::<SupplyEvent>();
        if let Ok(mut map) = supply_channel_map().lock() {
            map.insert(supply_id, rx);
        }
        // Start a reader thread for this connection
        if let Some(stream_arc) = get_tcp_stream(id) {
            let stream_clone = stream_arc.lock().ok().and_then(|s| s.try_clone().ok());
            if let Some(mut reader) = stream_clone {
                std::thread::spawn(move || {
                    use std::io::Read;
                    let mut buf = [0u8; 4096];
                    loop {
                        match reader.read(&mut buf) {
                            Ok(0) => {
                                let _ = tx.send(SupplyEvent::Done);
                                break;
                            }
                            Ok(n) => {
                                let data = String::from_utf8_lossy(&buf[..n]).to_string();
                                if tx.send(SupplyEvent::Emit(Value::str(data))).is_err() {
                                    break;
                                }
                            }
                            Err(_) => {
                                let _ = tx.send(SupplyEvent::Done);
                                break;
                            }
                        }
                    }
                });
            }
        }
        let mut attrs = HashMap::new();
        attrs.insert("values".to_string(), Value::array(Vec::new()));
        attrs.insert("taps".to_string(), Value::array(Vec::new()));
        attrs.insert("live".to_string(), Value::Bool(true));
        attrs.insert("supply_id".to_string(), Value::Int(supply_id as i64));
        Ok(Value::make_instance(Symbol::intern("Supply"), attrs))
    }

    fn async_socket_supply_in_memory(
        &mut self,
        conn_id: Option<u64>,
        args: &[Value],
        attributes: &HashMap<String, Value>,
    ) -> Result<Value, RuntimeError> {
        let id = conn_id.ok_or_else(|| RuntimeError::new("Missing async conn-id"))?;
        let is_bin = Self::named_bool(args, "bin");
        let supply_enc = Self::named_value(args, "enc")
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
                self.decode_pending_bytes_to_supply(
                    &conn_state.pending_bytes,
                    supply_id,
                    is_bin,
                    &mut initial_values,
                );
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

    fn decode_pending_bytes_to_supply(
        &mut self,
        pending_bytes: &[u8],
        supply_id: u64,
        is_bin: bool,
        initial_values: &mut Vec<Value>,
    ) {
        if is_bin {
            let mut battrs = HashMap::new();
            battrs.insert(
                "bytes".to_string(),
                Value::array(
                    pending_bytes
                        .iter()
                        .map(|b| Value::Int(*b as i64))
                        .collect(),
                ),
            );
            initial_values.push(Value::make_instance(Symbol::intern("Buf"), battrs));
        } else if let Some(supply) = get_async_supply(supply_id) {
            let decoded = if supply.encoding.eq_ignore_ascii_case("utf-8") {
                String::from_utf8_lossy(pending_bytes).to_string()
            } else if supply.encoding.eq_ignore_ascii_case("latin-1")
                || supply.encoding.eq_ignore_ascii_case("iso-8859-1")
            {
                pending_bytes
                    .iter()
                    .map(|b| char::from_u32(*b as u32).unwrap_or('\u{FFFD}'))
                    .collect()
            } else {
                self.decode_with_encoding(pending_bytes, &supply.encoding)
                    .unwrap_or_default()
            };
            if !decoded.is_empty() {
                let mut start = 0usize;
                for (idx, ch) in decoded.char_indices() {
                    if ch == '\n' {
                        initial_values.push(Value::str(decoded[start..=idx].to_string()));
                        start = idx + ch.len_utf8();
                    }
                }
                let tail = decoded[start..].to_string();
                update_async_supply(supply_id, |st| st.text_buffer = tail);
            }
        }
    }

    fn async_socket_write_real_tcp(
        &mut self,
        conn_id: Option<u64>,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let promise = SharedPromise::new();
        let result = (|| -> Result<Value, RuntimeError> {
            let id = conn_id.ok_or_else(|| RuntimeError::new("Missing async conn-id"))?;
            let stream_arc =
                get_tcp_stream(id).ok_or_else(|| RuntimeError::new("TCP stream not found"))?;
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
                text.into_bytes()
            };
            if bytes.is_empty() {
                return Ok(Self::async_socket_kept(Value::Bool(true)));
            }
            let mut stream = stream_arc
                .lock()
                .map_err(|_| RuntimeError::new("Failed to lock TCP stream"))?;
            use std::io::Write;
            stream
                .write_all(&bytes)
                .map_err(|e| RuntimeError::new(format!("write failed: {}", e)))?;
            stream
                .flush()
                .map_err(|e| RuntimeError::new(format!("flush failed: {}", e)))?;
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

    fn async_socket_write_in_memory(
        &mut self,
        conn_id: Option<u64>,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
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

            self.deliver_bytes_to_peer_supplies(&peer.supply_ids, &bytes);
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

    fn deliver_bytes_to_peer_supplies(&mut self, supply_ids: &[u64], bytes: &[u8]) {
        for sid in supply_ids {
            if let Some(supply) = get_async_supply(*sid) {
                if supply.is_bin {
                    let mut battrs = HashMap::new();
                    battrs.insert(
                        "bytes".to_string(),
                        Value::array(bytes.iter().map(|b| Value::Int(*b as i64)).collect()),
                    );
                    let buf = Value::make_instance(Symbol::intern("Buf"), battrs);
                    let _ = self.async_supplier_emit_value(*sid, buf);
                } else {
                    let mut pending = supply.byte_buffer.clone();
                    pending.extend_from_slice(bytes);
                    let (decoded, remainder) = if supply.encoding.eq_ignore_ascii_case("utf-8") {
                        match std::str::from_utf8(&pending) {
                            Ok(s) => (s.to_string(), Vec::new()),
                            Err(e) => {
                                if e.error_len().is_none() {
                                    let valid = &pending[..e.valid_up_to()];
                                    let valid_decoded =
                                        std::str::from_utf8(valid).unwrap_or("").to_string();
                                    (valid_decoded, pending[e.valid_up_to()..].to_vec())
                                } else {
                                    self.async_supplier_quit_value(
                                        *sid,
                                        Value::str_from("Malformed UTF-8 on socket Supply"),
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
                                .map(|b| char::from_u32(*b as u32).unwrap_or('\u{FFFD}'))
                                .collect(),
                            Vec::new(),
                        )
                    } else {
                        match self.decode_with_encoding(&pending, &supply.encoding) {
                            Ok(s) => (s, Vec::new()),
                            Err(e) => {
                                self.async_supplier_quit_value(*sid, Value::str(e.message));
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
                            let _ = self.async_supplier_emit_value(*sid, Value::str(chunk));
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
    }
}
