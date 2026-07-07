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
        if let Some((_listener_id, listener)) =
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
            client_attrs.insert("conn-id".to_string(), Value::int(client_id as i64));
            client_attrs.insert("socket-host".to_string(), Value::str(host.clone()));
            client_attrs.insert(
                "socket-port".to_string(),
                Value::int(client_local_port as i64),
            );
            client_attrs.insert("peer-host".to_string(), Value::str(server_host.clone()));
            client_attrs.insert("peer-port".to_string(), Value::int(port as i64));
            client_attrs.insert("enc".to_string(), Value::str(enc));
            let client_socket =
                Value::make_instance(Symbol::intern("IO::Socket::Async"), client_attrs);

            let mut server_attrs = HashMap::new();
            server_attrs.insert("conn-id".to_string(), Value::int(server_id as i64));
            server_attrs.insert("socket-host".to_string(), Value::str(server_host.clone()));
            server_attrs.insert("socket-port".to_string(), Value::int(port as i64));
            server_attrs.insert("peer-host".to_string(), Value::str(host.clone()));
            server_attrs.insert(
                "peer-port".to_string(),
                Value::int(client_local_port as i64),
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

            // Keep the promise with the client socket directly
            promise.keep(client_socket, String::new(), String::new());
        } else {
            // Break the promise on connection failure
            let err_msg = format!("Failed to connect to '{}:{}'", host, port);
            let _ = promise.try_break(Value::str(err_msg));
        };

        Ok(Value::promise(promise))
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

    /// Thread.start({ block }) -- spawn a real OS thread
    /// Supports named params: :name("..."), :app_lifetime
    pub(in crate::runtime) fn dispatch_thread_start(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Extract the block and named parameters from args
        let mut block = Value::NIL;
        let mut thread_name = "<anon>".to_string();
        let mut app_lifetime = false;

        for arg in args {
            match arg.view() {
                ValueView::Pair(k, v) if k.as_str() == "name" => {
                    thread_name = v.to_string_value();
                }
                ValueView::Pair(k, v) if k.as_str() == "app_lifetime" => {
                    app_lifetime = v.truthy();
                }
                _ => {
                    block = arg.clone();
                }
            }
        }

        let thread_id = NEXT_THREAD_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // A thread spawned *inside* a subtest must keep buffering its output
        // through `shared_thread_output` (as `clone_for_thread` already
        // arranges) rather than writing straight to stdout: its TAP lines are
        // subtest-internal and must be drained (indented) into the subtest by
        // `Thread.finish`, not leaked to the real top-level stream ("tests out
        // of sequence"). Only threads spawned at top level get immediate
        // stdout so their output lands in real chronological order relative
        // to the main thread's direct writes.
        let parent_in_subtest = self.tap.subtest_depth() != 0;
        let mut thread_interp = self.clone_for_thread();
        if !parent_in_subtest {
            thread_interp.set_immediate_stdout(true);
        }
        let mutsu_tid = thread_id as i64;
        // Use the large user-code stack (matches `start {}` / Promise / Supply
        // worker threads): `Thread.start` runs arbitrary user code, and the
        // default ~2-8 MiB thread stack overflows on deep VM nesting (e.g. an
        // async server whose react loop constructs objects whose BUILD re-enters
        // the VM -- HTTP::Server::Tiny). See `USER_THREAD_STACK_SIZE`.
        let handle = crate::runtime::builtins_system::spawn_user_thread(move || {
            // Set the mutsu thread ID for $*THREAD.id consistency
            super::set_current_mutsu_thread_id(mutsu_tid);
            match thread_interp.call_sub_value(block, vec![], false) {
                Ok(_) => {}
                Err(e) => {
                    eprintln!("Thread error: {}", e.message);
                }
            }
            // Flush any remaining buffered output from the thread
            let output = std::mem::take(&mut thread_interp.output_sink_mut().output);
            if !output.is_empty() {
                use std::io::Write;
                let _ = std::io::stdout().write_all(output.as_bytes());
                let _ = std::io::stdout().flush();
            }
            let stderr = std::mem::take(&mut thread_interp.output_sink_mut().stderr_output);
            if !stderr.is_empty() {
                use std::io::Write;
                let _ = std::io::stderr().write_all(stderr.as_bytes());
                let _ = std::io::stderr().flush();
            }
        });

        if app_lifetime {
            // For app_lifetime threads, don't store the handle -- the thread
            // will be killed when the main thread exits
            drop(handle);
        } else {
            THREAD_HANDLES.lock().unwrap().insert(thread_id, handle);
        }

        let mut attrs = HashMap::new();
        attrs.insert("id".to_string(), Value::int(thread_id as i64));
        attrs.insert("name".to_string(), Value::str(thread_name));
        attrs.insert("app_lifetime".to_string(), Value::truth(app_lifetime));
        Ok(Value::make_instance(Symbol::intern("Thread"), attrs))
    }

    /// Thread.finish -- join the thread (block until it completes)
    pub(in crate::runtime) fn dispatch_thread_finish(
        &mut self,
        attributes: &HashMap<String, Value>,
    ) -> Result<Value, RuntimeError> {
        let thread_id = attributes
            .get("id")
            .or_else(|| attributes.get("thread_id"))
            .and_then(|v| {
                if let ValueView::Int(i) = v.view() {
                    Some(i as u64)
                } else {
                    None
                }
            })
            .ok_or_else(|| RuntimeError::new("Thread has no thread_id"))?;

        let handle = THREAD_HANDLES.lock().unwrap().remove(&thread_id);
        if let Some(handle) = handle {
            // STW-aware: a thread blocked in `.finish`/join counts as quiescent
            // for the GC's cooperative stop-the-world (it cannot mutate the Gc
            // graph until the join returns).
            crate::gc::block_quiescent(|| handle.join())
                .map_err(|_| RuntimeError::new("Thread panicked"))?;
        }
        // Sync shared variables back to env after thread completes
        self.sync_shared_vars_to_env();
        // Joining a thread is a synchronization point: flush any output the
        // thread buffered into `shared_thread_output` (e.g. TAP lines from a
        // thread spawned inside a subtest, which cannot use immediate stdout —
        // see `dispatch_thread_start`) so it lands in real chronological order
        // now, rather than at some later, possibly out-of-order sync point.
        self.drain_shared_thread_output();
        Ok(Value::TRUE)
    }
}
