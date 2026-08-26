use super::*;
use crate::value::ValueView;

impl Interpreter {
    pub(in crate::runtime) fn dispatch_socket_connect(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let host = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let port = args
            .get(1)
            .map(|v| match v.view() {
                ValueView::Int(i) => i as u16,
                ValueView::Num(f) => f as u16,
                _ => v.to_string_value().parse::<u16>().unwrap_or(0),
            })
            .unwrap_or(0);
        let addr = format!("{}:{}", host, port);
        let stream = std::net::TcpStream::connect_timeout(
            &addr
                .to_socket_addrs()
                .map_err(|e| RuntimeError::new(format!("Failed to resolve '{}': {}", addr, e)))?
                .next()
                .ok_or_else(|| RuntimeError::new(format!("No addresses found for '{}'", addr)))?,
            Duration::from_secs(10),
        )
        .map_err(|e| RuntimeError::new(format!("Failed to connect to '{}': {}", addr, e)))?;
        let state = IoHandleState {
            target: IoHandleTarget::Socket,
            mode: IoHandleMode::ReadWrite,
            path: None,
            line_separators: self.default_line_separators(),
            line_chomp: true,
            encoding: "utf-8".to_string(),
            file: None,
            socket: Some(SocketStream::Tcp(stream)),
            listener: None,
            closed: false,
            out_buffer_capacity: None,
            out_buffer_pending: Vec::new(),
            bin: false,
            nl_out: "\n".to_string(),
            bytes_written: 0,
            read_attempted: false,
            stream_hit_eof: false,
            utf16_bom_written: false,
            utf16_detected_be: None,
            argfiles_index: 0,
            argfiles_reader: None,
            argfiles_paths: None,
            pending_words: std::collections::VecDeque::new(),
            close_on_word_exhaust: false,
        };
        let id = self.insert_handle_state(state);
        let mut attrs = HashMap::new();
        attrs.insert("handle".to_string(), Value::int(id as i64));
        attrs.insert("host".to_string(), Value::str(host));
        attrs.insert("port".to_string(), Value::int(port as i64));
        Ok(Value::make_instance(
            Symbol::intern("IO::Socket::INET"),
            attrs,
        ))
    }

    /// IO::Socket::INET.listen($host, $port, family => ...)
    pub(in crate::runtime) fn dispatch_socket_inet_listen(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Extract positional and named args
        let mut positional = Vec::new();
        let mut family: Option<i64> = None;
        for arg in args {
            match arg.view() {
                ValueView::Pair(key, value) if key.as_str() == "family" => {
                    family = Some(match value.view() {
                        ValueView::Int(i) => i,
                        ValueView::Enum { value: v, .. } => v.as_i64(),
                        _ => value.to_string_value().parse::<i64>().unwrap_or(0),
                    });
                }
                _ => positional.push(arg.clone()),
            }
        }
        let host = positional
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let port = positional
            .get(1)
            .map(|v| match v.view() {
                ValueView::Int(i) => i as u16,
                ValueView::Num(f) => f as u16,
                _ => v.to_string_value().parse::<u16>().unwrap_or(0),
            })
            .unwrap_or(0);

        // Build args for dispatch_socket_inet_new in listen mode
        let mut new_args = vec![
            Value::pair("listen".to_string(), Value::TRUE),
            Value::pair("localhost".to_string(), Value::str(host)),
            Value::pair("localport".to_string(), Value::int(port as i64)),
        ];
        if let Some(f) = family {
            new_args.push(Value::pair("family".to_string(), Value::int(f)));
        }
        self.dispatch_socket_inet_new(&new_args)
    }

    /// IO::Socket::INET.connect($host, $port, family => ...)
    pub(in crate::runtime) fn dispatch_socket_inet_connect(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Extract positional and named args
        let mut positional = Vec::new();
        let mut family: Option<i64> = None;
        for arg in args {
            match arg.view() {
                ValueView::Pair(key, value) if key.as_str() == "family" => {
                    family = Some(match value.view() {
                        ValueView::Int(i) => i,
                        ValueView::Enum { value: v, .. } => v.as_i64(),
                        _ => value.to_string_value().parse::<i64>().unwrap_or(0),
                    });
                }
                _ => positional.push(arg.clone()),
            }
        }

        if family == Some(3) {
            // PF_UNIX — delegate to dispatch_socket_inet_new with family
            let host = positional
                .first()
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            let port = positional
                .get(1)
                .map(|v| match v.view() {
                    ValueView::Int(i) => i as u16,
                    ValueView::Num(f) => f as u16,
                    _ => v.to_string_value().parse::<u16>().unwrap_or(0),
                })
                .unwrap_or(0);
            let new_args = vec![
                Value::pair("host".to_string(), Value::str(host)),
                Value::pair("port".to_string(), Value::int(port as i64)),
                Value::pair("family".to_string(), Value::int(3)),
            ];
            return self.dispatch_socket_inet_new(&new_args);
        }

        // TCP connect — delegate to existing dispatch_socket_connect
        self.dispatch_socket_connect(&positional)
    }

    pub(in crate::runtime) fn dispatch_socket_async_listen(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let host = args
            .first()
            .map(Value::to_string_value)
            .unwrap_or_else(|| "0.0.0.0".to_string());
        let port = args
            .get(1)
            .map(|v| match v.view() {
                ValueView::Int(i) => i as u16,
                ValueView::Num(f) => f as u16,
                _ => v.to_string_value().parse::<u16>().unwrap_or(0),
            })
            .unwrap_or(0);
        let enc = Self::named_value(args, "enc")
            .map(|v| v.to_string_value())
            .unwrap_or_else(|| "utf-8".to_string());
        let mut attrs = HashMap::new();
        attrs.insert("host".to_string(), Value::str(host));
        attrs.insert("port".to_string(), Value::int(port as i64));
        attrs.insert("enc".to_string(), Value::str(enc));
        Ok(Value::make_instance(
            Symbol::intern("IO::Socket::Async::Listener"),
            attrs,
        ))
    }

    pub(in crate::runtime) fn dispatch_socket_async_connect(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
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
        let enc = Self::named_value(args, "enc")
            .map(|v| v.to_string_value())
            .unwrap_or_else(|| "utf-8".to_string());

        let promise = SharedPromise::new();
        // Every `connect` opens a real TCP stream, including one aimed at a
        // listener in THIS process. `.listen` binds a real `TcpListener` and
        // its accept thread produces `tcp-real` connections, so a loopback
        // connect goes through exactly the same OS plumbing as an outbound
        // one — which is what gives both ends a real file descriptor.
        // `.native-descriptor` used to answer -1 for the in-process case (it
        // was short-circuited to an in-memory socket pair with no fd), so
        // NativeCall consumers of it died: Cro::TCP::NoDelay's
        // `setsockopt(TCP_NODELAY)` failed on the bogus fd, taking down every
        // Cro client that talks to a server in its own process.
        match Self::connect_real_tcp(&host, port) {
            Ok(socket) => {
                let conn_id = super::super::native_methods::next_async_socket_id();
                let local = socket.local_addr().ok();
                let peer = socket.peer_addr().ok();
                super::super::native_methods::state::register_tcp_stream(conn_id, socket);
                let mut attrs = HashMap::new();
                attrs.insert("conn-id".to_string(), Value::int(conn_id as i64));
                attrs.insert("tcp-real".to_string(), Value::TRUE);
                attrs.insert(
                    "socket-host".to_string(),
                    Value::str(
                        local
                            .map(|a| a.ip().to_string())
                            .unwrap_or_else(|| "0.0.0.0".to_string()),
                    ),
                );
                attrs.insert(
                    "socket-port".to_string(),
                    Value::int(local.map(|a| a.port()).unwrap_or(0) as i64),
                );
                attrs.insert("peer-host".to_string(), Value::str(host.clone()));
                attrs.insert(
                    "peer-port".to_string(),
                    Value::int(peer.map(|a| a.port()).unwrap_or(port) as i64),
                );
                attrs.insert("enc".to_string(), Value::str(enc));
                promise.keep(
                    Value::make_instance(Symbol::intern("IO::Socket::Async"), attrs),
                    String::new(),
                    String::new(),
                );
            }
            Err(e) => {
                // Break with a real exception, not a Str: a consumer that
                // catches the failure may `.rethrow` it (Cro's pipeline
                // QUIT handler does).
                let mut ex_attrs = HashMap::new();
                ex_attrs.insert(
                    "message".to_string(),
                    Value::str(format!("Failed to connect to '{}:{}': {}", host, port, e)),
                );
                let _ =
                    promise.try_break(Value::make_instance(Symbol::intern("X::AdHoc"), ex_attrs));
            }
        }

        Ok(Value::promise(promise))
    }

    /// Open a real outbound TCP connection for `IO::Socket::Async.connect`.
    /// Every resolved address is tried in turn, so a host that resolves to both
    /// IPv6 and IPv4 (`localhost` on a dual-stack box) still connects when only
    /// one family has a listener.
    fn connect_real_tcp(host: &str, port: u16) -> std::io::Result<std::net::TcpStream> {
        let addr = format!("{}:{}", host, port);
        let addrs: Vec<_> = addr.to_socket_addrs()?.collect();
        let mut last_err = std::io::Error::new(
            std::io::ErrorKind::AddrNotAvailable,
            format!("no addresses found for '{}'", addr),
        );
        for sock_addr in addrs {
            match std::net::TcpStream::connect_timeout(&sock_addr, Duration::from_secs(10)) {
                Ok(s) => return Ok(s),
                Err(e) => last_err = e,
            }
        }
        Err(last_err)
    }

    /// IO::Socket::Async.bind-udp($host, $port)
    pub(in crate::runtime) fn dispatch_socket_async_bind_udp(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let host = args
            .first()
            .map(Value::to_string_value)
            .unwrap_or_else(|| "0.0.0.0".to_string());
        let port = args
            .get(1)
            .map(|v| match v.view() {
                ValueView::Int(i) => i as u16,
                ValueView::Num(f) => f as u16,
                _ => v.to_string_value().parse::<u16>().unwrap_or(0),
            })
            .unwrap_or(0);

        if super::super::native_methods::udp_port_in_use(&host, port) {
            return Err(RuntimeError::new(format!(
                "Address already in use: {}:{}",
                host, port
            )));
        }

        let socket_id = super::super::native_methods::next_async_socket_id();
        super::super::native_methods::register_udp_bound_socket(
            socket_id,
            super::super::native_methods::UdpBoundSocketState {
                host: host.clone(),
                port,
                closed: false,
                supply_ids: Vec::new(),
            },
        );

        let mut attrs = HashMap::new();
        attrs.insert("udp-socket-id".to_string(), Value::int(socket_id as i64));
        attrs.insert("socket-host".to_string(), Value::str(host));
        attrs.insert("socket-port".to_string(), Value::int(port as i64));
        attrs.insert("is-udp".to_string(), Value::TRUE);
        Ok(Value::make_instance(
            Symbol::intern("IO::Socket::Async"),
            attrs,
        ))
    }

    /// IO::Socket::Async.udp() - create an unbound UDP socket for sending
    pub(in crate::runtime) fn dispatch_socket_async_udp(
        &mut self,
        _args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let socket_id = super::super::native_methods::next_async_socket_id();
        let mut attrs = HashMap::new();
        attrs.insert("udp-socket-id".to_string(), Value::int(socket_id as i64));
        attrs.insert("is-udp".to_string(), Value::TRUE);
        attrs.insert("is-udp-sender".to_string(), Value::TRUE);
        Ok(Value::make_instance(
            Symbol::intern("IO::Socket::Async"),
            attrs,
        ))
    }
}
