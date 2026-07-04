use crate::runtime::*;

impl Interpreter {
    /// Extract bytes from a Buf/Blob instance or array
    pub(in crate::runtime) fn extract_bytes(val: &Value) -> Option<Vec<u8>> {
        match val {
            Value::Instance {
                class_name,
                attributes,
                ..
            } if {
                let cn = class_name.resolve();
                cn == "Buf"
                    || cn == "Blob"
                    || cn == "utf8"
                    || cn == "utf16"
                    || cn.starts_with("buf")
                    || cn.starts_with("blob")
                    || cn.starts_with("Buf[")
                    || cn.starts_with("Blob[")
            } =>
            {
                if let Some(Value::Array(items, ..)) = attributes.as_map().get("bytes") {
                    Some(
                        items
                            .iter()
                            .map(|v| match v {
                                Value::Int(i) => *i as u8,
                                _ => 0,
                            })
                            .collect(),
                    )
                } else {
                    Some(Vec::new())
                }
            }
            Value::Array(elems, ..) => Some(
                elems
                    .iter()
                    .map(|v| match v {
                        Value::Int(i) => *i as u8,
                        _ => 0,
                    })
                    .collect(),
            ),
            _ => None,
        }
    }

    /// Socket recv: read N chars (or all available) in text or binary mode
    pub(in crate::runtime) fn socket_recv(
        &mut self,
        handle_id: usize,
        max_chars: Option<usize>,
        bin_mode: bool,
    ) -> Result<Value, RuntimeError> {
        use std::io::Read;
        let mut table = self.io_handles_mut();
        let state = table
            .map
            .get_mut(&handle_id)
            .ok_or_else(|| RuntimeError::new("Invalid socket handle"))?;
        let stream = state
            .socket
            .as_mut()
            .ok_or_else(|| RuntimeError::new("Socket not connected"))?;

        if bin_mode {
            let max = max_chars.unwrap_or(65536);
            let mut buf = vec![0u8; max];
            let n = stream
                .read(&mut buf)
                .map_err(|e| RuntimeError::new(format!("recv failed: {}", e)))?;
            buf.truncate(n);
            return Ok(Self::make_buf(buf));
        }

        // Text mode
        match max_chars {
            Some(n) => {
                // `recv($limit)` is an UPPER limit, not a lower one (roast
                // S32-io/socket-recv-vs-read.t): it must not block waiting for
                // the full `$limit`. Do a single OS read of up to `n` bytes
                // (blocking only until the first data arrives) and decode it.
                // `n` bytes decode to at most `n` characters, so the character
                // limit is respected, and any further bytes stay buffered in the
                // OS socket for the next `recv`/`read` (no over-read).
                let mut buf = vec![0u8; n];
                let read = stream
                    .read(&mut buf)
                    .map_err(|e| RuntimeError::new(format!("recv failed: {}", e)))?;
                buf.truncate(read);
                Ok(Value::str(String::from_utf8_lossy(&buf).to_string()))
            }
            None => {
                // Read all available data
                let mut buf = vec![0u8; 65536];
                let n = stream
                    .read(&mut buf)
                    .map_err(|e| RuntimeError::new(format!("recv failed: {}", e)))?;
                buf.truncate(n);
                Ok(Value::str(String::from_utf8_lossy(&buf).to_string()))
            }
        }
    }

    /// Socket read: read N bytes, return Buf
    pub(in crate::runtime) fn socket_read_bytes(
        &mut self,
        handle_id: usize,
        nbytes: usize,
    ) -> Result<Value, RuntimeError> {
        use std::io::Read;
        let mut table = self.io_handles_mut();
        let state = table
            .map
            .get_mut(&handle_id)
            .ok_or_else(|| RuntimeError::new("Invalid socket handle"))?;
        let stream = state
            .socket
            .as_mut()
            .ok_or_else(|| RuntimeError::new("Socket not connected"))?;

        let mut buf = vec![0u8; nbytes];
        let mut total = 0;
        while total < nbytes {
            let n = stream
                .read(&mut buf[total..])
                .map_err(|e| RuntimeError::new(format!("read failed: {}", e)))?;
            if n == 0 {
                break;
            }
            total += n;
        }
        buf.truncate(total);
        Ok(Self::make_buf(buf))
    }

    /// Socket get: read one line using the handle's line separators
    pub(in crate::runtime) fn socket_get_line(
        &mut self,
        handle_id: usize,
    ) -> Result<Value, RuntimeError> {
        use std::io::Read;
        let separators = {
            let table = self.io_handles();
            let state = table
                .map
                .get(&handle_id)
                .ok_or_else(|| RuntimeError::new("Invalid socket handle"))?;
            state.line_separators.clone()
        };

        let mut table = self.io_handles_mut();
        let state = table
            .map
            .get_mut(&handle_id)
            .ok_or_else(|| RuntimeError::new("Invalid socket handle"))?;
        let stream = state
            .socket
            .as_mut()
            .ok_or_else(|| RuntimeError::new("Socket not connected"))?;

        let mut line_bytes = Vec::new();
        let mut byte_buf = [0u8; 1];
        loop {
            let n = stream.read(&mut byte_buf).unwrap_or(0);
            if n == 0 {
                break;
            }
            line_bytes.push(byte_buf[0]);

            // Check if line ends with any separator
            for sep in &separators {
                if line_bytes.len() >= sep.len() && line_bytes.ends_with(sep) {
                    // Remove separator
                    line_bytes.truncate(line_bytes.len() - sep.len());
                    return Ok(Value::str(String::from_utf8_lossy(&line_bytes).to_string()));
                }
            }
        }
        if line_bytes.is_empty() {
            Ok(Value::Nil)
        } else {
            Ok(Value::str(String::from_utf8_lossy(&line_bytes).to_string()))
        }
    }

    /// Socket lines: read all remaining lines, return as a Seq/Array
    pub(in crate::runtime) fn socket_lines(
        &mut self,
        handle_id: usize,
    ) -> Result<Value, RuntimeError> {
        let mut lines = Vec::new();
        loop {
            let line = self.socket_get_line(handle_id)?;
            if matches!(line, Value::Nil) {
                break;
            }
            lines.push(line);
        }
        Ok(Value::Seq(Arc::new(lines)))
    }

    /// Create a Buf instance from raw bytes
    pub(crate) fn make_buf(bytes: Vec<u8>) -> Value {
        let byte_vals: Vec<Value> = bytes.into_iter().map(|b| Value::Int(b as i64)).collect();
        let mut attrs = HashMap::new();
        attrs.insert("bytes".to_string(), Value::array(byte_vals));
        Value::make_instance(crate::symbol::Symbol::intern("Buf[uint8]"), attrs)
    }
}
