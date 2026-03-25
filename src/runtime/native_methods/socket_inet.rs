use crate::runtime::*;

impl Interpreter {
    // --- IO::Socket::INET ---

    pub(in crate::runtime) fn native_socket_inet(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let handle_id = attributes.get("handle").and_then(|v| {
            if let Value::Int(i) = v {
                Some(*i as usize)
            } else {
                None
            }
        });
        match method {
            "getpeername" => {
                let id =
                    handle_id.ok_or_else(|| RuntimeError::new("IO::Socket::INET has no handle"))?;
                let state = self
                    .handles
                    .get(&id)
                    .ok_or_else(|| RuntimeError::new("Invalid socket handle"))?;
                if state.closed {
                    return Err(RuntimeError::new("Socket is closed"));
                }
                if let Some(ref stream) = state.socket {
                    let addr = stream
                        .peer_addr()
                        .map_err(|e| RuntimeError::new(format!("getpeername failed: {}", e)))?;
                    Ok(Value::str(addr))
                } else {
                    Err(RuntimeError::new("Socket not connected"))
                }
            }
            "close" => {
                let id =
                    handle_id.ok_or_else(|| RuntimeError::new("IO::Socket::INET has no handle"))?;
                let state = self
                    .handles
                    .get_mut(&id)
                    .ok_or_else(|| RuntimeError::new("Invalid socket handle"))?;
                state.closed = true;
                state.socket = None;
                state.listener = None;
                Ok(Value::Bool(true))
            }
            "localport" => {
                // Return localport from attributes
                Ok(attributes
                    .get("localport")
                    .cloned()
                    .unwrap_or(Value::Int(0)))
            }
            "accept" => {
                let id =
                    handle_id.ok_or_else(|| RuntimeError::new("IO::Socket::INET has no handle"))?;
                // Take listener out temporarily to avoid borrow issues
                let listener = {
                    let state = self
                        .handles
                        .get_mut(&id)
                        .ok_or_else(|| RuntimeError::new("Invalid socket handle"))?;
                    state.listener.as_ref().and_then(|l| l.try_clone().ok())
                };
                let listener =
                    listener.ok_or_else(|| RuntimeError::new("Socket is not listening"))?;
                let stream = listener
                    .accept()
                    .map_err(|e| RuntimeError::new(format!("accept failed: {}", e)))?;
                // Create a new handle for the accepted connection
                let new_id = self.next_handle_id;
                self.next_handle_id += 1;
                let state = super::super::IoHandleState {
                    target: super::super::IoHandleTarget::Socket,
                    mode: super::super::IoHandleMode::ReadWrite,
                    path: None,
                    line_separators: self.default_line_separators(),
                    line_chomp: true,
                    encoding: "utf-8".to_string(),
                    file: None,
                    socket: Some(stream),
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
                self.handles.insert(new_id, state);
                let mut attrs = HashMap::new();
                attrs.insert("handle".to_string(), Value::Int(new_id as i64));
                Ok(Value::make_instance(
                    crate::symbol::Symbol::intern("IO::Socket::INET"),
                    attrs,
                ))
            }
            "print" | "say" | "put" => {
                let id =
                    handle_id.ok_or_else(|| RuntimeError::new("IO::Socket::INET has no handle"))?;
                let mut data = String::new();
                for arg in &args {
                    data.push_str(&arg.to_string_value());
                }
                if method == "say" || method == "put" {
                    data.push('\n');
                }
                let state = self
                    .handles
                    .get_mut(&id)
                    .ok_or_else(|| RuntimeError::new("Invalid socket handle"))?;
                if let Some(ref mut stream) = state.socket {
                    use std::io::Write;
                    stream
                        .write_all(data.as_bytes())
                        .map_err(|e| RuntimeError::new(format!("print failed: {}", e)))?;
                    stream
                        .flush()
                        .map_err(|e| RuntimeError::new(format!("flush failed: {}", e)))?;
                    Ok(Value::Bool(true))
                } else {
                    Err(RuntimeError::new("Socket not connected"))
                }
            }
            "write" => {
                let id =
                    handle_id.ok_or_else(|| RuntimeError::new("IO::Socket::INET has no handle"))?;
                let buf_data = if let Some(arg) = args.first() {
                    Self::extract_bytes(arg)
                        .ok_or_else(|| RuntimeError::new("write expects a Buf argument"))?
                } else {
                    return Err(RuntimeError::new("write expects a Buf argument"));
                };
                let state = self
                    .handles
                    .get_mut(&id)
                    .ok_or_else(|| RuntimeError::new("Invalid socket handle"))?;
                if let Some(ref mut stream) = state.socket {
                    use std::io::Write;
                    stream
                        .write_all(&buf_data)
                        .map_err(|e| RuntimeError::new(format!("write failed: {}", e)))?;
                    stream
                        .flush()
                        .map_err(|e| RuntimeError::new(format!("flush failed: {}", e)))?;
                    Ok(Value::Bool(true))
                } else {
                    Err(RuntimeError::new("Socket not connected"))
                }
            }
            "recv" => {
                let id =
                    handle_id.ok_or_else(|| RuntimeError::new("IO::Socket::INET has no handle"))?;
                // Check for :bin named arg
                let mut bin_mode = false;
                let mut max_chars: Option<usize> = None;
                for arg in &args {
                    match arg {
                        Value::Int(n) => max_chars = Some(*n as usize),
                        Value::Pair(key, value) => {
                            if key.as_str() == "bin" {
                                bin_mode = value.as_ref().truthy();
                            }
                        }
                        _ => {}
                    }
                }
                self.socket_recv(id, max_chars, bin_mode)
            }
            "read" => {
                let id =
                    handle_id.ok_or_else(|| RuntimeError::new("IO::Socket::INET has no handle"))?;
                let nbytes = args
                    .first()
                    .map(|v| match v {
                        Value::Int(i) => *i as usize,
                        _ => 0,
                    })
                    .unwrap_or(0);
                self.socket_read_bytes(id, nbytes)
            }
            "get" => {
                let id =
                    handle_id.ok_or_else(|| RuntimeError::new("IO::Socket::INET has no handle"))?;
                self.socket_get_line(id)
            }
            "lines" => {
                let id =
                    handle_id.ok_or_else(|| RuntimeError::new("IO::Socket::INET has no handle"))?;
                self.socket_lines(id)
            }
            "nl-in" => {
                // Getter for nl-in
                let id =
                    handle_id.ok_or_else(|| RuntimeError::new("IO::Socket::INET has no handle"))?;
                let state = self
                    .handles
                    .get(&id)
                    .ok_or_else(|| RuntimeError::new("Invalid socket handle"))?;
                let seps: Vec<Value> = state
                    .line_separators
                    .iter()
                    .map(|s| Value::str(String::from_utf8_lossy(s).to_string()))
                    .collect();
                if seps.len() == 1 {
                    Ok(seps.into_iter().next().unwrap_or(Value::str_from("\n")))
                } else {
                    Ok(Value::array(seps))
                }
            }
            _ => Err(RuntimeError::new(format!(
                "No method '{}' on IO::Socket::INET",
                method
            ))),
        }
    }
}
