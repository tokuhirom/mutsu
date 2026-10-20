use crate::runtime::*;
use crate::symbol::Symbol;

use super::state::*;
use crate::value::AttrMap;

impl Interpreter {
    /// The id of a TCP connection's stream, `None` for a UDP socket.
    // Cost: O(1).
    fn async_conn_id(attributes: &AttrMap) -> Option<u64> {
        attributes
            .get("conn-id")
            .and_then(|v| v.as_int().map(|i| i as u64))
    }

    /// Whether an `IO::Socket::Async` receiver is a UDP socket (made by
    /// `bind-udp` / `udp`).
    // Cost: O(1).
    pub(crate) fn async_socket_is_udp(attributes: &AttrMap) -> bool {
        attributes.get("is-udp").and_then(Value::as_bool) == Some(true)
    }

    /// `IO::Socket::Async.native-descriptor`: the OS-level file descriptor of
    /// the underlying TCP stream, used by NativeCall consumers
    /// (Cro::TCP::NoDelay's setsockopt); -1 when there is none.
    // Cost: O(1).
    pub(crate) fn async_socket_native_descriptor(attributes: &AttrMap) -> Value {
        #[cfg(unix)]
        if let Some(id) = Self::async_conn_id(attributes)
            && let Some(stream) = get_tcp_stream(id)
            && let Ok(s) = stream.lock()
        {
            use std::os::unix::io::AsRawFd;
            return Value::int(s.as_raw_fd() as i64);
        }
        #[cfg(not(unix))]
        let _ = attributes;
        Value::int(-1)
    }

    /// `IO::Socket::Async.close`.
    // Cost: O(1), plus O(s) for a UDP socket, s = its supplies.
    pub(crate) fn async_socket_close(&mut self, attributes: &AttrMap) -> Value {
        if Self::async_socket_is_udp(attributes) {
            self.udp_socket_close(attributes);
        } else if let Some(id) = Self::async_conn_id(attributes) {
            if let Some(stream) = get_tcp_stream(id)
                && let Ok(s) = stream.lock()
            {
                let _ = s.shutdown(std::net::Shutdown::Both);
            }
            remove_tcp_stream(id);
        }
        Value::NIL
    }

    /// `IO::Socket::Async.Supply(:bin, :enc)`: what the socket receives.
    // Cost: O(1) to set up; the reads run on a worker.
    pub(crate) fn async_socket_supply(
        &mut self,
        attributes: &AttrMap,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if Self::async_socket_is_udp(attributes) {
            return self.udp_socket_supply(attributes);
        }
        self.async_socket_supply_real_tcp(Self::async_conn_id(attributes), args, attributes)
    }

    /// `IO::Socket::Async.write($buf)` / `.print($str)` on a TCP connection.
    // Cost: O(n), n = bytes sent.
    pub(crate) fn async_socket_send(
        &mut self,
        attributes: &AttrMap,
        args: &[Value],
        text: bool,
    ) -> Result<Value, RuntimeError> {
        let method = if text { "print" } else { "write" };
        if Self::async_socket_is_udp(attributes) {
            return Err(crate::runtime::did_you_mean::method_not_found(
                method,
                "IO::Socket::Async",
            ));
        }
        self.async_socket_write_real_tcp(Self::async_conn_id(attributes), method, args, attributes)
    }

    /// The socket's text encoding: `.Supply(:enc(...))` / `.print(:enc(...))`
    /// wins over the `:enc` the socket was created with, which defaults to
    /// UTF-8.
    fn socket_encoding(args: &[Value], attributes: &AttrMap) -> String {
        Self::named_value(args, "enc")
            .map(|v| v.to_string_value())
            .or_else(|| attributes.get("enc").map(Value::to_string_value))
            .unwrap_or_else(|| "utf-8".to_string())
            .to_lowercase()
    }

    fn async_socket_supply_real_tcp(
        &mut self,
        conn_id: Option<u64>,
        args: &[Value],
        attributes: &AttrMap,
    ) -> Result<Value, RuntimeError> {
        let id = conn_id.ok_or_else(|| RuntimeError::new("Missing async conn-id"))?;
        // `.Supply(:bin)` must emit `Buf[uint8]` chunks (raw bytes), not decoded
        // `Str`. This is the exact point HTTP::Server::Tiny's handler needs: it
        // feeds the emitted value to `parse-http-request`, which expects a Blob.
        let is_bin = Self::named_bool(args, "bin");
        let enc = Self::socket_encoding(args, attributes);
        let supply_id = next_supply_id();
        let (tx, rx) = super::supply_channel::supply_event_channel();
        // A connection's read Supply is NOT a broadcast point: rakudo hands
        // each incoming chunk to exactly one of its taps (measured 2026-09-07
        // against v2026.07 -- tapping `$conn.Supply` twice gives one tap the
        // bytes and the other nothing, with the winner varying between runs).
        // Mark it exclusive so `take_supply_channel` keeps handing the whole
        // stream to the first consumer instead of fanning out (ADR-0074).
        rx.mark_exclusive();
        if let Ok(mut map) = supply_channel_map().lock() {
            map.insert(supply_id, rx);
        }
        // Read the connection into the Supply (see `socket_reader`).
        if let Some(stream) = get_tcp_stream(id) {
            let reader = super::socket_reader::SocketReader::new(stream, tx, is_bin, enc);
            // A refused reader thread (#9401) is a catchable X::AdHoc.
            if let Err(e) = super::socket_reader::start_socket_reader(reader) {
                discard_supply_channel(supply_id);
                return Err(e);
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
        attributes: &AttrMap,
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
                // `.print` encodes with the socket's `:enc`, so a latin-1 socket
                // puts one byte per codepoint on the wire rather than UTF-8.
                let enc = Self::socket_encoding(args, attributes);
                if enc == "utf-8" || enc == "utf8" {
                    text.into_bytes()
                } else {
                    self.encode_with_encoding(&text, &enc)?
                }
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
                Self::async_socket_broken(Value::str(e.message.into_owned())),
                String::new(),
                String::new(),
            ),
        }
        Ok(Value::promise(promise))
    }
}
