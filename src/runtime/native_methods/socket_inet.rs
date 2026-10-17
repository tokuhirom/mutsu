use crate::runtime::*;
use crate::value::AttrMap;
use crate::value::ValueView;

/// The handle id an `IO::Socket::INET` receiver stands for.
// Cost: O(1).
fn inet_handle(attributes: &AttrMap) -> Result<usize, RuntimeError> {
    match attributes.get("handle").map(Value::view) {
        Some(ValueView::Int(i)) => Ok(i as usize),
        _ => Err(RuntimeError::new("IO::Socket::INET has no handle")),
    }
}

/// The methods of an `IO::Socket::INET`, each the handler of one row
/// (ADR-11276 §9.158).
impl Interpreter {
    /// `getpeername`: the peer address of a connected socket.
    // Cost: O(1).
    pub(crate) fn inet_getpeername(&mut self, attributes: &AttrMap) -> Result<Value, RuntimeError> {
        let id = inet_handle(attributes)?;
        let table = self.io_handles();
        let state = table
            .map
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

    /// `close`.
    // Cost: O(1).
    pub(crate) fn inet_close(&mut self, attributes: &AttrMap) -> Result<Value, RuntimeError> {
        let id = inet_handle(attributes)?;
        let mut table = self.io_handles_mut();
        let state = table
            .map
            .get_mut(&id)
            .ok_or_else(|| RuntimeError::new("Invalid socket handle"))?;
        state.closed = true;
        state.socket = None;
        state.listener = None;
        Ok(Value::TRUE)
    
    }

    /// `accept`: wait for a connection and answer a socket for it.
    // Cost: O(1) plus the wait for a connection.
    pub(crate) fn inet_accept(&mut self, attributes: &AttrMap) -> Result<Value, RuntimeError> {
        let id = inet_handle(attributes)?;
        // Take listener out temporarily to avoid borrow issues
        let listener = {
            let mut table = self.io_handles_mut();
            let state = table
                .map
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
            stream_hit_eof: false,
            utf16_bom_written: false,
            utf16_detected_be: None,
            argfiles_index: 0,
            argfiles_reader: None,
            argfiles_paths: None,
            pending_words: std::collections::VecDeque::new(),
            close_on_exhaust: false,
            seq_reader: None,
        };
        let new_id = self.insert_handle_state(state);
        let mut attrs = HashMap::new();
        attrs.insert("handle".to_string(), Value::int(new_id as i64));
        Ok(Value::make_instance(
            crate::symbol::Symbol::intern("IO::Socket::INET"),
            attrs,
        ))
    
    }

    /// `print` / `put`: send the arguments' text, `put` with a newline.
    // Cost: O(n), n = bytes sent.
    pub(crate) fn inet_print(&mut self, attributes: &AttrMap, args: &[Value], newline: bool) -> Result<Value, RuntimeError> {
        let id = inet_handle(attributes)?;
        let mut data = String::new();
        for arg in args {
            data.push_str(&arg.to_string_value());
        }
        if newline {
            data.push('\n');
        }
        let mut table = self.io_handles_mut();
        let state = table
            .map
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
            Ok(Value::TRUE)
        } else {
            Err(RuntimeError::new("Socket not connected"))
        }
    
    }

    /// `write($buf)`: send the bytes.
    // Cost: O(n), n = bytes sent.
    pub(crate) fn inet_write(&mut self, attributes: &AttrMap, args: &[Value]) -> Result<Value, RuntimeError> {
        let id = inet_handle(attributes)?;
        let buf_data = if let Some(arg) = args.first() {
            Self::extract_bytes(arg)
                .ok_or_else(|| RuntimeError::new("write expects a Buf argument"))?
        } else {
            return Err(RuntimeError::new("write expects a Buf argument"));
        };
        let mut table = self.io_handles_mut();
        let state = table
            .map
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
            Ok(Value::TRUE)
        } else {
            Err(RuntimeError::new("Socket not connected"))
        }
    
    }

    /// `recv($limit?, :bin)`.
    // Cost: O(n), n = bytes received.
    pub(crate) fn inet_recv(&mut self, attributes: &AttrMap, args: &[Value]) -> Result<Value, RuntimeError> {
        let id = inet_handle(attributes)?;
        // Check for :bin named arg
        let mut bin_mode = false;
        let mut max_chars: Option<usize> = None;
        for arg in args {
            match arg.view() {
                ValueView::Int(n) => max_chars = Some(n as usize),
                ValueView::Pair(key, value) if key.as_str() == "bin" => {
                    bin_mode = value.truthy();
                }
                _ => {}
            }
        }
        self.socket_recv(id, max_chars, bin_mode)
    
    }

    /// `read($bytes)`.
    // Cost: O(n), n = bytes read.
    pub(crate) fn inet_read(&mut self, attributes: &AttrMap, args: &[Value]) -> Result<Value, RuntimeError> {
        let id = inet_handle(attributes)?;
        let nbytes = args
            .first()
            .map(|v| match v.view() {
                ValueView::Int(i) => i as usize,
                _ => 0,
            })
            .unwrap_or(0);
        self.socket_read_bytes(id, nbytes)
    
    }

    /// `get`: the next line.
    // Cost: O(l), l = bytes of the line.
    pub(crate) fn inet_get(&mut self, attributes: &AttrMap) -> Result<Value, RuntimeError> {
        let id = inet_handle(attributes)?;
        self.socket_get_line(id)
    
    }

    /// `lines`.
    // Cost: O(n), n = bytes read.
    pub(crate) fn inet_lines(&mut self, attributes: &AttrMap) -> Result<Value, RuntimeError> {
        let id = inet_handle(attributes)?;
        self.socket_lines(id)
    
    }

    /// `nl-in`: the line separators.
    // Cost: O(s), s = separators.
    pub(crate) fn inet_nl_in(&mut self, attributes: &AttrMap) -> Result<Value, RuntimeError> {
        // Getter for nl-in
        let id = inet_handle(attributes)?;
        let table = self.io_handles();
        let state = table
            .map
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
}
