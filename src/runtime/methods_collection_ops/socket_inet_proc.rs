use super::*;

impl Interpreter {
    /// IO::Socket::INET.new — handles both :listen (server) and client modes.
    pub(in crate::runtime) fn dispatch_socket_inet_new(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut host = String::new();
        let mut localhost = String::new();
        let mut port: u16 = 0;
        let mut localport: u16 = 0;
        let mut listen = false;
        let mut family: Option<i64> = None;

        for arg in args {
            if let Value::Pair(key, value) = arg {
                match key.as_str() {
                    "host" => host = value.to_string_value(),
                    "localhost" => localhost = value.to_string_value(),
                    "port" => {
                        let raw = match value.as_ref() {
                            Value::Int(i) => *i,
                            Value::Num(f) => *f as i64,
                            other => other.to_string_value().parse::<i64>().unwrap_or(0),
                        };
                        if !(0..=65535).contains(&raw) {
                            return Err(RuntimeError::new(format!(
                                "Failed to create socket: invalid port {}",
                                raw
                            )));
                        }
                        port = raw as u16;
                    }
                    "localport" => {
                        let raw = match value.as_ref() {
                            Value::Int(i) => *i,
                            Value::Num(f) => *f as i64,
                            other => other.to_string_value().parse::<i64>().unwrap_or(0),
                        };
                        if !(0..=65535).contains(&raw) {
                            return Err(RuntimeError::new(format!(
                                "Failed to create socket: invalid port {}",
                                raw
                            )));
                        }
                        localport = raw as u16;
                    }
                    "listen" => listen = value.as_ref().truthy(),
                    "family" => {
                        family = Some(match value.as_ref() {
                            Value::Int(i) => *i,
                            Value::Enum { value: v, .. } => v.as_i64(),
                            other => other.to_string_value().parse::<i64>().unwrap_or(0),
                        });
                    }
                    _ => {}
                }
            }
        }

        // Validate family if provided — must match a known ProtocolFamily value
        if let Some(f) = family {
            let valid_families = [0, 1, 2, 3, 4]; // PF_UNSPEC, PF_INET, PF_INET6, PF_LOCAL/PF_UNIX, PF_MAX
            if !valid_families.contains(&f) {
                return Err(RuntimeError::new(format!(
                    "Failed to create socket: unsupported family {}",
                    f
                )));
            }
        }

        if !host.is_empty() && port == 0 {
            let (parsed_host, parsed_port) = Self::split_host_port_literal(&host);
            if let Some(p) = parsed_port {
                host = parsed_host;
                port = p;
            }
        }
        if !localhost.is_empty() && localport == 0 {
            let (parsed_host, parsed_port) = Self::split_host_port_literal(&localhost);
            if let Some(p) = parsed_port {
                localhost = parsed_host;
                localport = p;
            }
        }

        if listen {
            // Server mode: bind and listen
            #[cfg(unix)]
            if family == Some(3) {
                // PF_UNIX / PF_LOCAL
                let path = if !localhost.is_empty() {
                    localhost.clone()
                } else if !host.is_empty() {
                    host.clone()
                } else {
                    return Err(RuntimeError::new("PF_UNIX listen requires a socket path"));
                };
                let listener = std::os::unix::net::UnixListener::bind(&path).map_err(|e| {
                    RuntimeError::new(format!("Failed to bind UNIX socket '{}': {}", path, e))
                })?;
                let id = self.next_handle_id;
                self.next_handle_id += 1;
                let state = IoHandleState {
                    target: IoHandleTarget::Socket,
                    mode: IoHandleMode::ReadWrite,
                    path: Some(path.clone()),
                    line_separators: self.default_line_separators(),
                    line_chomp: true,
                    encoding: "utf-8".to_string(),
                    file: None,
                    socket: None,
                    listener: Some(SocketListener::Unix(listener)),
                    closed: false,
                    out_buffer_capacity: None,
                    out_buffer_pending: Vec::new(),
                    bin: false,
                    nl_out: "\n".to_string(),
                    bytes_written: 0,
                    read_attempted: false,
                    argfiles_index: 0,
                    argfiles_reader: None,
                };
                self.handles.insert(id, state);
                let mut attrs = HashMap::new();
                attrs.insert("handle".to_string(), Value::Int(id as i64));
                attrs.insert("localport".to_string(), Value::Int(0i64));
                attrs.insert("localhost".to_string(), Value::str(path));
                return Ok(Value::make_instance(
                    Symbol::intern("IO::Socket::INET"),
                    attrs,
                ));
            }
            let bind_addr = if localhost.is_empty() {
                format!("0.0.0.0:{}", localport)
            } else {
                format!("{}:{}", localhost, localport)
            };
            let listener = std::net::TcpListener::bind(&bind_addr).map_err(|e| {
                RuntimeError::new(format!("Failed to bind to '{}': {}", bind_addr, e))
            })?;
            let actual_port = listener.local_addr().map(|a| a.port()).unwrap_or(localport);
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
                socket: None,
                listener: Some(SocketListener::Tcp(listener)),
                closed: false,
                out_buffer_capacity: None,
                out_buffer_pending: Vec::new(),
                bin: false,
                nl_out: "\n".to_string(),
                bytes_written: 0,
                read_attempted: false,
                argfiles_index: 0,
                argfiles_reader: None,
            };
            self.handles.insert(id, state);
            let mut attrs = HashMap::new();
            attrs.insert("handle".to_string(), Value::Int(id as i64));
            attrs.insert("localport".to_string(), Value::Int(actual_port as i64));
            attrs.insert(
                "localhost".to_string(),
                Value::str(if localhost.is_empty() {
                    "0.0.0.0".to_string()
                } else {
                    localhost
                }),
            );
            Ok(Value::make_instance(
                Symbol::intern("IO::Socket::INET"),
                attrs,
            ))
        } else {
            // Client mode: connect
            #[cfg(unix)]
            if family == Some(3) {
                // PF_UNIX / PF_LOCAL
                let path = if !host.is_empty() {
                    host.clone()
                } else {
                    return Err(RuntimeError::new("PF_UNIX connect requires a socket path"));
                };
                let stream = std::os::unix::net::UnixStream::connect(&path).map_err(|e| {
                    RuntimeError::new(format!(
                        "Failed to connect to UNIX socket '{}': {}",
                        path, e
                    ))
                })?;
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
                    socket: Some(SocketStream::Unix(stream)),
                    listener: None,
                    closed: false,
                    out_buffer_capacity: None,
                    out_buffer_pending: Vec::new(),
                    bin: false,
                    nl_out: "\n".to_string(),
                    bytes_written: 0,
                    read_attempted: false,
                    argfiles_index: 0,
                    argfiles_reader: None,
                };
                self.handles.insert(id, state);
                let mut attrs = HashMap::new();
                attrs.insert("handle".to_string(), Value::Int(id as i64));
                attrs.insert("host".to_string(), Value::str(path));
                attrs.insert("port".to_string(), Value::Int(port as i64));
                return Ok(Value::make_instance(
                    Symbol::intern("IO::Socket::INET"),
                    attrs,
                ));
            }
            if host.is_empty() {
                host = "127.0.0.1".to_string();
            }
            self.dispatch_socket_connect(&[Value::str(host), Value::Int(port as i64)])
        }
    }

    /// Replay deferred Proc::Async taps on the main thread.
    /// Called when a Proc result is retrieved via .result or await.
    pub(in crate::runtime) fn replay_proc_taps(
        &mut self,
        attributes: &Arc<crate::value::InstanceAttrs>,
    ) {
        let mut stdout_taps = match attributes.get("stdout_taps") {
            Some(Value::Array(taps, ..)) => taps.to_vec(),
            _ => Vec::new(),
        };
        let mut stderr_taps = match attributes.get("stderr_taps") {
            Some(Value::Array(taps, ..)) => taps.to_vec(),
            _ => Vec::new(),
        };
        if let Some(Value::Int(sid)) = attributes.get("stdout_supply_id") {
            let live = super::super::native_methods::get_supply_taps(*sid as u64);
            if !live.is_empty() {
                stdout_taps = live;
            }
        }
        if let Some(Value::Int(sid)) = attributes.get("stderr_supply_id") {
            let live = super::super::native_methods::get_supply_taps(*sid as u64);
            if !live.is_empty() {
                stderr_taps = live;
            }
        }
        let collected_stdout = match attributes.get("collected_stdout") {
            Some(Value::Str(s)) => s.to_string(),
            _ => String::new(),
        };
        let collected_stderr = match attributes.get("collected_stderr") {
            Some(Value::Str(s)) => s.to_string(),
            _ => String::new(),
        };
        let mut supply_taps = match attributes.get("supply_taps") {
            Some(Value::Array(taps, ..)) => taps.to_vec(),
            _ => Vec::new(),
        };
        if let Some(Value::Int(sid)) = attributes.get("supply_id") {
            let live = super::super::native_methods::get_supply_taps(*sid as u64);
            if !live.is_empty() {
                supply_taps = live;
            }
        }
        let collected_merged = match attributes.get("collected_merged") {
            Some(Value::Str(s)) => s.to_string(),
            _ => format!("{}{}", collected_stdout, collected_stderr),
        };

        if !collected_stdout.is_empty() && !stdout_taps.is_empty() {
            for tap in &stdout_taps {
                let _ = self.call_sub_value(
                    tap.clone(),
                    vec![Value::str(collected_stdout.clone())],
                    true,
                );
            }
        }
        if !collected_stderr.is_empty() && !stderr_taps.is_empty() {
            for tap in &stderr_taps {
                let _ = self.call_sub_value(
                    tap.clone(),
                    vec![Value::str(collected_stderr.clone())],
                    true,
                );
            }
        }
        if !collected_merged.is_empty() && !supply_taps.is_empty() {
            for tap in &supply_taps {
                let _ = self.call_sub_value(
                    tap.clone(),
                    vec![Value::str(collected_merged.clone())],
                    true,
                );
            }
        }
    }

    /// Returns Some(class_name) if target is Promise or a Promise subclass package.
    pub(in crate::runtime) fn promise_class_name(&mut self, target: &Value) -> Option<String> {
        match target {
            Value::Package(name) => {
                if name == "Promise" {
                    Some("Promise".to_string())
                } else if self
                    .class_mro(&name.resolve())
                    .contains(&"Promise".to_string())
                {
                    Some(name.resolve())
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}
