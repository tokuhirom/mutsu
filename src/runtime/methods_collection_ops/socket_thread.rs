use super::*;

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
            .map(|v| match v {
                Value::Int(i) => *i as u16,
                Value::Num(f) => *f as u16,
                other => other.to_string_value().parse::<u16>().unwrap_or(0),
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
        let id = self.next_handle_id;
        self.next_handle_id += 1;
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
            utf16_bom_written: false,
            utf16_detected_be: None,
            argfiles_index: 0,
            argfiles_reader: None,
        };
        self.handles.insert(id, state);
        let mut attrs = HashMap::new();
        attrs.insert("handle".to_string(), Value::Int(id as i64));
        attrs.insert("host".to_string(), Value::str(host));
        attrs.insert("port".to_string(), Value::Int(port as i64));
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
            match arg {
                Value::Pair(key, value) if key.as_str() == "family" => {
                    family = Some(match value.as_ref() {
                        Value::Int(i) => *i,
                        Value::Enum { value: v, .. } => v.as_i64(),
                        other => other.to_string_value().parse::<i64>().unwrap_or(0),
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
            .map(|v| match v {
                Value::Int(i) => *i as u16,
                Value::Num(f) => *f as u16,
                other => other.to_string_value().parse::<u16>().unwrap_or(0),
            })
            .unwrap_or(0);

        // Build args for dispatch_socket_inet_new in listen mode
        let mut new_args = vec![
            Value::Pair("listen".to_string(), Box::new(Value::Bool(true))),
            Value::Pair("localhost".to_string(), Box::new(Value::str(host))),
            Value::Pair("localport".to_string(), Box::new(Value::Int(port as i64))),
        ];
        if let Some(f) = family {
            new_args.push(Value::Pair("family".to_string(), Box::new(Value::Int(f))));
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
            match arg {
                Value::Pair(key, value) if key.as_str() == "family" => {
                    family = Some(match value.as_ref() {
                        Value::Int(i) => *i,
                        Value::Enum { value: v, .. } => v.as_i64(),
                        other => other.to_string_value().parse::<i64>().unwrap_or(0),
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
                .map(|v| match v {
                    Value::Int(i) => *i as u16,
                    Value::Num(f) => *f as u16,
                    other => other.to_string_value().parse::<u16>().unwrap_or(0),
                })
                .unwrap_or(0);
            let new_args = vec![
                Value::Pair("host".to_string(), Box::new(Value::str(host))),
                Value::Pair("port".to_string(), Box::new(Value::Int(port as i64))),
                Value::Pair("family".to_string(), Box::new(Value::Int(3))),
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
            .map(|v| match v {
                Value::Int(i) => *i as u16,
                Value::Num(f) => *f as u16,
                other => other.to_string_value().parse::<u16>().unwrap_or(0),
            })
            .unwrap_or(0);
        let enc = Self::named_value(args, "enc")
            .map(|v| v.to_string_value())
            .unwrap_or_else(|| "utf-8".to_string());
        let mut attrs = HashMap::new();
        attrs.insert("host".to_string(), Value::str(host));
        attrs.insert("port".to_string(), Value::Int(port as i64));
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
            .map(|v| match v {
                Value::Int(i) => *i as u16,
                Value::Num(f) => *f as u16,
                other => other.to_string_value().parse::<u16>().unwrap_or(0),
            })
            .unwrap_or(0);
        let enc = Self::named_value(args, "enc")
            .map(|v| v.to_string_value())
            .unwrap_or_else(|| "utf-8".to_string());

        let promise = SharedPromise::new();
        let result = if let Some((_listener_id, listener)) =
            super::super::native_methods::lookup_async_listener(&host, port)
        {
            let defer_accept = Self::callback_uses_supply_list(&listener.callback);
            let client_id = super::super::native_methods::next_async_socket_id();
            let server_id = super::super::native_methods::next_async_socket_id();
            let client_local_port = super::super::native_methods::allocate_async_listen_port();
            let server_host = if listener.host == "0.0.0.0" {
                host.clone()
            } else {
                listener.host.clone()
            };

            super::super::native_methods::register_async_connection(
                client_id,
                super::super::native_methods::AsyncSocketConnState {
                    peer_id: Some(server_id),
                    encoding: enc.clone(),
                    closed: false,
                    peer_closed: false,
                    supply_ids: Vec::new(),
                    pending_bytes: Vec::new(),
                    deferred_accept_callback: None,
                    deferred_accept_socket: None,
                },
            );
            super::super::native_methods::register_async_connection(
                server_id,
                super::super::native_methods::AsyncSocketConnState {
                    peer_id: Some(client_id),
                    encoding: listener.encoding.clone(),
                    closed: false,
                    peer_closed: false,
                    supply_ids: Vec::new(),
                    pending_bytes: Vec::new(),
                    deferred_accept_callback: None,
                    deferred_accept_socket: None,
                },
            );

            let mut client_attrs = HashMap::new();
            client_attrs.insert("conn-id".to_string(), Value::Int(client_id as i64));
            client_attrs.insert("socket-host".to_string(), Value::str(host.clone()));
            client_attrs.insert(
                "socket-port".to_string(),
                Value::Int(client_local_port as i64),
            );
            client_attrs.insert("peer-host".to_string(), Value::str(server_host.clone()));
            client_attrs.insert("peer-port".to_string(), Value::Int(port as i64));
            client_attrs.insert("enc".to_string(), Value::str(enc));
            let client_socket =
                Value::make_instance(Symbol::intern("IO::Socket::Async"), client_attrs);

            let mut server_attrs = HashMap::new();
            server_attrs.insert("conn-id".to_string(), Value::Int(server_id as i64));
            server_attrs.insert("socket-host".to_string(), Value::str(server_host.clone()));
            server_attrs.insert("socket-port".to_string(), Value::Int(port as i64));
            server_attrs.insert("peer-host".to_string(), Value::str(host.clone()));
            server_attrs.insert(
                "peer-port".to_string(),
                Value::Int(client_local_port as i64),
            );
            server_attrs.insert("enc".to_string(), Value::str(listener.encoding));
            let server_socket =
                Value::make_instance(Symbol::intern("IO::Socket::Async"), server_attrs);

            if defer_accept {
                super::super::native_methods::update_async_connection(server_id, |conn| {
                    conn.deferred_accept_callback = Some(listener.callback.clone());
                    conn.deferred_accept_socket = Some(server_socket.clone());
                });
            } else {
                let _ = self.call_sub_value(listener.callback, vec![server_socket], true);
            }

            let mut status_attrs = HashMap::new();
            status_attrs.insert("status".to_string(), Value::str_from("Kept"));
            status_attrs.insert("result".to_string(), client_socket);
            status_attrs.insert("cause".to_string(), Value::Nil);
            Value::make_instance(
                Symbol::intern("IO::Socket::Async::StatusResult"),
                status_attrs,
            )
        } else {
            let mut status_attrs = HashMap::new();
            status_attrs.insert("status".to_string(), Value::str_from("Broken"));
            status_attrs.insert("result".to_string(), Value::Nil);
            status_attrs.insert(
                "cause".to_string(),
                Value::str(format!("Failed to connect to '{}:{}'", host, port)),
            );
            Value::make_instance(
                Symbol::intern("IO::Socket::Async::StatusResult"),
                status_attrs,
            )
        };

        promise.keep(result, String::new(), String::new());
        Ok(Value::Promise(promise))
    }

    /// Thread.start({ block }) — spawn a real OS thread
    pub(in crate::runtime) fn dispatch_thread_start(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let block = args.first().cloned().unwrap_or(Value::Nil);
        let thread_id = NEXT_THREAD_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        let mut thread_interp = self.clone_for_thread();
        let handle = std::thread::spawn(move || {
            match thread_interp.call_sub_value(block, vec![], false) {
                Ok(_) => {}
                Err(e) => {
                    eprintln!("Thread error: {}", e.message);
                }
            }
            // Flush any output from the thread
            let output = std::mem::take(&mut thread_interp.output);
            if !output.is_empty() {
                print!("{}", output);
            }
            let stderr = std::mem::take(&mut thread_interp.stderr_output);
            if !stderr.is_empty() {
                eprint!("{}", stderr);
            }
        });

        THREAD_HANDLES.lock().unwrap().insert(thread_id, handle);

        let mut attrs = HashMap::new();
        attrs.insert("thread_id".to_string(), Value::Int(thread_id as i64));
        Ok(Value::make_instance(Symbol::intern("Thread"), attrs))
    }

    /// Thread.finish — join the thread (block until it completes)
    pub(in crate::runtime) fn dispatch_thread_finish(
        &mut self,
        attributes: &HashMap<String, Value>,
    ) -> Result<Value, RuntimeError> {
        let thread_id = attributes
            .get("thread_id")
            .and_then(|v| {
                if let Value::Int(i) = v {
                    Some(*i as u64)
                } else {
                    None
                }
            })
            .ok_or_else(|| RuntimeError::new("Thread has no thread_id"))?;

        let handle = THREAD_HANDLES.lock().unwrap().remove(&thread_id);
        if let Some(handle) = handle {
            handle
                .join()
                .map_err(|_| RuntimeError::new("Thread panicked"))?;
        }
        Ok(Value::Bool(true))
    }
}
