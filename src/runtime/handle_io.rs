//! Interpreter-side handle output: text/byte writes, the `with_handle_mut`
//! confined-borrow helpers, line-separator parsing, and `close`.
use super::*;
use num_traits::ToPrimitive;

impl Interpreter {
    pub(crate) fn parse_out_buffer_size(value: &Value) -> Option<usize> {
        match value {
            Value::Int(i) if *i >= 0 => Some(*i as usize),
            Value::BigInt(i) if i.sign() != num_bigint::Sign::Minus => i.to_usize(),
            _ => None,
        }
    }

    pub(super) fn default_line_separators(&self) -> Vec<Vec<u8>> {
        match self.newline_mode {
            NewlineMode::Lf => vec![b"\r\n".to_vec(), b"\n".to_vec()],
            NewlineMode::Cr => vec![b"\r".to_vec()],
            NewlineMode::Crlf => vec![b"\r\n".to_vec()],
        }
    }

    pub(super) fn normalize_line_separators(mut separators: Vec<Vec<u8>>) -> Vec<Vec<u8>> {
        separators.retain(|sep| !sep.is_empty());
        if separators.is_empty() {
            return vec![b"\r\n".to_vec(), b"\n".to_vec()];
        }
        separators.sort_by_key(|sep| std::cmp::Reverse(sep.len()));
        separators.dedup();
        separators
    }

    pub(super) fn parse_nl_in_value(value: &Value) -> Vec<Vec<u8>> {
        match value {
            Value::Array(items, _) => items
                .iter()
                .map(|item| item.to_string_value().into_bytes())
                .collect(),
            _ => vec![value.to_string_value().into_bytes()],
        }
    }

    /// Split a string into lines using the given line separators and chomp
    /// setting.  Used by IO::Path.lines to honour :nl-in / :!chomp.
    pub(super) fn split_content_by_separators(
        content: &str,
        separators: &[Vec<u8>],
        chomp: bool,
    ) -> Vec<Value> {
        let bytes = content.as_bytes();
        let mut result = Vec::new();
        let mut pos = 0;
        while pos < bytes.len() {
            // Scan for the next separator starting from current position
            let mut found: Option<(usize, usize)> = None; // (offset, sep_len)
            'outer: for i in pos..bytes.len() {
                for sep in separators {
                    if bytes[i..].starts_with(sep) {
                        found = Some((i, sep.len()));
                        break 'outer;
                    }
                }
            }
            match found {
                Some((offset, sep_len)) => {
                    if chomp {
                        let line = &bytes[pos..offset];
                        result.push(Value::str(String::from_utf8_lossy(line).to_string()));
                    } else {
                        let line = &bytes[pos..offset + sep_len];
                        result.push(Value::str(String::from_utf8_lossy(line).to_string()));
                    }
                    pos = offset + sep_len;
                }
                None => {
                    // Remaining text (no trailing separator)
                    let line = &bytes[pos..];
                    if !line.is_empty() {
                        result.push(Value::str(String::from_utf8_lossy(line).to_string()));
                    }
                    break;
                }
            }
        }
        result
    }

    pub(crate) fn handle_id_from_value(value: &Value) -> Option<usize> {
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = value
            && (class_name == "IO::Handle" || class_name == "IO::Socket::INET")
            && let Some(Value::Int(id)) = attributes.as_map().get("handle")
            && *id >= 0
        {
            return Some(*id as usize);
        }
        None
    }

    /// Run `f` with a mutable borrow of the handle's `IoHandleState`, returning
    /// whatever the closure produces. The `&mut IoHandleState` borrow is
    /// confined to the closure body, so a caller can never hold it across
    /// another `self.*` handle operation. That re-entry-free discipline is what
    /// makes a later `Arc<RwLock>` lift (PR-B) safe: the lock is taken, the
    /// closure runs, the lock is released — no same-thread re-acquisition.
    ///
    /// Returns `Err("Expected IO::Handle" / "Invalid IO::Handle")` when the
    /// value is not a usable handle. Use [`with_handle_mut_opt`] instead when a
    /// missing handle should fall back to a default rather than error.
    pub(super) fn with_handle_mut<R>(
        &mut self,
        handle_value: &Value,
        f: impl FnOnce(&mut IoHandleState) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        let id = Self::handle_id_from_value(handle_value)
            .ok_or_else(|| RuntimeError::new("Expected IO::Handle"))?;
        // The write guard is held across `f`, which may perform blocking IO but
        // (by construction — `f` only receives `&mut IoHandleState`, never
        // `self`) cannot re-enter another handle op. Per-thread snapshot
        // semantics make holding the lock across blocking IO deadlock-free; the
        // debug reentry guard catches any same-thread re-acquire.
        let mut table = self.io_handles_mut();
        let state = table
            .map
            .get_mut(&id)
            .ok_or_else(|| RuntimeError::new("Invalid IO::Handle"))?;
        f(state)
    }

    /// Like [`with_handle_mut`], but yields `Ok(None)` when the value is not a
    /// live handle (instead of an error). Errors raised *inside* the closure
    /// still propagate as `Err`, so callers can distinguish "no such handle"
    /// (`Ok(None)`) from "the operation failed" (`Err`).
    pub(super) fn with_handle_mut_opt<R>(
        &mut self,
        handle_value: &Value,
        f: impl FnOnce(&mut IoHandleState) -> Result<R, RuntimeError>,
    ) -> Result<Option<R>, RuntimeError> {
        let Some(id) = Self::handle_id_from_value(handle_value) else {
            return Ok(None);
        };
        let mut table = self.io_handles_mut();
        let Some(state) = table.map.get_mut(&id) else {
            return Ok(None);
        };
        f(state).map(Some)
    }

    pub(super) fn write_to_handle_value(
        &mut self,
        handle_value: &Value,
        content: &str,
        newline: bool,
    ) -> Result<(), RuntimeError> {
        self.write_to_handle_value_trying(handle_value, content, newline, "write")
    }

    pub(super) fn write_to_handle_value_trying(
        &mut self,
        handle_value: &Value,
        content: &str,
        newline: bool,
        trying: &str,
    ) -> Result<(), RuntimeError> {
        // Phase 1: validate the handle, build the payload (appending the
        // handle's `nl_out` when requested) and account the bytes. Done inside
        // a confined borrow so no `&mut IoHandleState` is held across the
        // self-re-entrant `encode_with_encoding` / `emit_output` calls below.
        let (target, encoding, payload) = self.with_handle_mut(handle_value, |state| {
            if state.closed {
                return Err(RuntimeError::io_closed(trying));
            }
            let mut payload = String::from(content);
            if newline {
                payload.push_str(&state.nl_out);
            }
            state.bytes_written += payload.len() as i64;
            Ok((state.target, state.encoding.clone(), payload))
        })?;

        // Phase 2: encode (only File targets honour a non-UTF-8 encoding). This
        // re-enters `self`, hence it must run outside the handle borrow.
        let needs_encoding =
            !encoding.is_empty() && encoding != "utf-8" && encoding != "utf8" && encoding != "bin";
        let encoded_bytes = if needs_encoding && matches!(target, IoHandleTarget::File) {
            Some(self.encode_with_encoding(&payload, &encoding)?)
        } else {
            None
        };

        // Phase 3: dispatch. Stdout/Stderr touch `self`; File/Socket touch only
        // the handle state (a fresh confined borrow).
        match target {
            IoHandleTarget::Stdout => {
                self.emit_output(&payload);
                Ok(())
            }
            IoHandleTarget::Stderr => {
                let subtest_active = self.subtest_active();
                self.output_sink_mut().emit_stderr(&payload, subtest_active);
                Ok(())
            }
            IoHandleTarget::File => self.with_handle_mut(handle_value, |state| {
                let payload_bytes = if let Some(ref enc_bytes) = encoded_bytes {
                    enc_bytes.as_slice()
                } else {
                    payload.as_bytes()
                };
                state.write_file_payload(payload_bytes)
            }),
            IoHandleTarget::Socket => self.with_handle_mut(handle_value, |state| {
                if let Some(sock) = state.socket.as_mut() {
                    sock.write_all(payload.as_bytes()).map_err(|err| {
                        RuntimeError::new(format!("Failed to write to socket: {}", err))
                    })?;
                    Ok(())
                } else {
                    Err(RuntimeError::new("Socket not connected"))
                }
            }),
            IoHandleTarget::Stdin | IoHandleTarget::ArgFiles => {
                Err(RuntimeError::new("Cannot write to read-only handle"))
            }
        }
    }

    pub(super) fn write_bytes_to_handle_value(
        &mut self,
        handle_value: &Value,
        bytes: &[u8],
    ) -> Result<(), RuntimeError> {
        // Validate + account the bytes inside a confined borrow, then dispatch.
        // Stdout/Stderr re-enter `self`, so the handle borrow is dropped first.
        let target = self.with_handle_mut(handle_value, |state| {
            if state.closed {
                return Err(RuntimeError::io_closed("write"));
            }
            if matches!(state.mode, IoHandleMode::Read) {
                return Err(RuntimeError::new("Handle not open for writing"));
            }
            state.bytes_written += bytes.len() as i64;
            Ok(state.target)
        })?;
        match target {
            IoHandleTarget::Stdout => {
                // For stdout, convert bytes to lossy string and emit
                let content = String::from_utf8_lossy(bytes).to_string();
                self.emit_output(&content);
                Ok(())
            }
            IoHandleTarget::Stderr => {
                if self.tap.subtest_depth() == 0 && self.output_sink().immediate_stdout {
                    use std::io::Write;
                    let _ = std::io::stderr().write_all(bytes);
                    let _ = std::io::stderr().flush();
                } else {
                    let content = String::from_utf8_lossy(bytes).to_string();
                    self.output_sink_mut().stderr_output.push_str(&content);
                }
                Ok(())
            }
            IoHandleTarget::File => {
                self.with_handle_mut(handle_value, |state| state.write_all_to_file(bytes))
            }
            IoHandleTarget::Socket => self.with_handle_mut(handle_value, |state| {
                if let Some(sock) = state.socket.as_mut() {
                    sock.write_all(bytes).map_err(|err| {
                        RuntimeError::new(format!("Failed to write to socket: {}", err))
                    })?;
                    Ok(())
                } else {
                    Err(RuntimeError::new("Socket not connected"))
                }
            }),
            IoHandleTarget::Stdin | IoHandleTarget::ArgFiles => {
                Err(RuntimeError::new("Cannot write to read-only handle"))
            }
        }
    }

    pub(super) fn close_handle_value(
        &mut self,
        handle_value: &Value,
    ) -> Result<bool, RuntimeError> {
        self.with_handle_mut(handle_value, |state| state.close())
    }
}
