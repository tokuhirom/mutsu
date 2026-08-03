use crate::runtime::*;
use crate::symbol::Symbol;

use super::state::*;
use crate::value::AttrMap;

impl Interpreter {
    pub(in crate::runtime) fn native_socket_async(
        &mut self,
        attributes: &AttrMap,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // Dispatch to UDP handler if this is a UDP socket
        if attributes.get("is-udp").and_then(Value::as_bool) == Some(true) {
            return self.native_socket_async_udp(attributes, method, args);
        }

        let conn_id = attributes
            .get("conn-id")
            .and_then(|v| v.as_int().map(|i| i as u64));

        match method {
            "socket-port" => Ok(attributes
                .get("socket-port")
                .cloned()
                .unwrap_or(Value::int(0))),
            "peer-port" => Ok(attributes
                .get("peer-port")
                .cloned()
                .unwrap_or(Value::int(0))),
            "socket-host" => Ok(attributes
                .get("socket-host")
                .cloned()
                .unwrap_or_else(|| Value::str_from("0.0.0.0"))),
            "peer-host" => Ok(attributes
                .get("peer-host")
                .cloned()
                .unwrap_or_else(|| Value::str_from("0.0.0.0"))),
            // The OS-level file descriptor of the underlying TCP stream, used
            // by NativeCall consumers (Cro::TCP::NoDelay's setsockopt).
            "native-descriptor" => {
                #[cfg(unix)]
                if let Some(id) = conn_id
                    && let Some(stream) = get_tcp_stream(id)
                    && let Ok(s) = stream.lock()
                {
                    use std::os::unix::io::AsRawFd;
                    return Ok(Value::int(s.as_raw_fd() as i64));
                }
                Ok(Value::int(-1))
            }
            "close" => {
                if let Some(id) = conn_id {
                    if let Some(stream) = get_tcp_stream(id)
                        && let Ok(s) = stream.lock()
                    {
                        let _ = s.shutdown(std::net::Shutdown::Both);
                    }
                    remove_tcp_stream(id);
                }
                Ok(Value::NIL)
            }
            "Supply" => self.async_socket_supply_real_tcp(conn_id, &args),
            "write" | "print" => self.async_socket_write_real_tcp(conn_id, method, &args),
            _ => Err(RuntimeError::new(format!(
                "No method '{}' on IO::Socket::Async",
                method
            ))),
        }
    }

    fn async_socket_supply_real_tcp(
        &mut self,
        conn_id: Option<u64>,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let id = conn_id.ok_or_else(|| RuntimeError::new("Missing async conn-id"))?;
        // `.Supply(:bin)` must emit `Buf[uint8]` chunks (raw bytes), not decoded
        // `Str`. This is the exact point HTTP::Server::Tiny's handler needs: it
        // feeds the emitted value to `parse-http-request`, which expects a Blob.
        let is_bin = Self::named_bool(args, "bin");
        let supply_id = next_supply_id();
        let (tx, rx) = super::supply_channel::supply_event_channel();
        if let Ok(mut map) = supply_channel_map().lock() {
            map.insert(supply_id, rx);
        }
        // Start a reader thread for this connection
        if let Some(stream_arc) = get_tcp_stream(id) {
            let stream_clone = stream_arc.lock().ok().and_then(|s| s.try_clone().ok());
            if let Some(mut reader) = stream_clone {
                // Registered spawn: emits freshly built `Gc` values (Buf
                // instances) whose drops on a failed send must not race a
                // cycle scan; the blocking read is a quiescent safe region.
                crate::runtime::builtins_system::spawn_gc_helper_thread(move || {
                    use std::io::Read;
                    let mut buf = [0u8; 4096];
                    loop {
                        match crate::gc::block_quiescent(|| reader.read(&mut buf)) {
                            Ok(0) => {
                                let _ = tx.send(SupplyEvent::Done);
                                break;
                            }
                            Ok(n) => {
                                let value = if is_bin {
                                    Self::make_buf(buf[..n].to_vec())
                                } else {
                                    Value::str(String::from_utf8_lossy(&buf[..n]).to_string())
                                };
                                if tx.send(SupplyEvent::Emit(value)).is_err() {
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
        attrs.insert("live".to_string(), Value::TRUE);
        attrs.insert("supply_id".to_string(), Value::int(supply_id as i64));
        Ok(Value::make_instance(Symbol::intern("Supply"), attrs))
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
                return Ok(Self::async_socket_kept(Value::TRUE));
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
            Ok(Self::async_socket_kept(Value::TRUE))
        })();
        match result {
            Ok(v) => promise.keep(v, String::new(), String::new()),
            Err(e) => promise.keep(
                Self::async_socket_broken(Value::str(e.message)),
                String::new(),
                String::new(),
            ),
        }
        Ok(Value::promise(promise))
    }
}
