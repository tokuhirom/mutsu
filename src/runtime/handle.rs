use super::*;
use num_traits::ToPrimitive;

/// Pure per-handle operations that touch only the handle's own state — no
/// `Interpreter` state (no `emit_output`, env, or encoding helpers). These are
/// the single authoritative implementation shared by the interpreter's
/// `*_handle_value` wrappers and the VM-native IO dispatch
/// (`try_native_io_handle_method`), so the §1 native-IO fork for these methods
/// can resolve in the VM via its own `io_handles` handle without bouncing
/// through `self.interpreter` (PLAN.md ③ native IO PR-C).
impl IoHandleState {
    /// Flush any buffered (`:out-buffer`) bytes to the underlying file.
    pub(crate) fn flush_buffer(&mut self) -> Result<(), RuntimeError> {
        if self.out_buffer_pending.is_empty() {
            return Ok(());
        }
        let Some(file) = self.file.as_mut() else {
            return Err(RuntimeError::new("IO::Handle is not attached to a file"));
        };
        file.write_all(&self.out_buffer_pending)
            .map_err(|err| RuntimeError::new(format!("Failed to write to file: {}", err)))?;
        self.out_buffer_pending.clear();
        Ok(())
    }

    /// Write already-encoded payload bytes to a **File-target** handle,
    /// honoring the `:out-buffer` capacity (flush-and-write, bypass-on-large,
    /// or append-to-pending). Pure handle state — no `Interpreter` access — so
    /// it is the single authoritative impl shared by the interpreter's
    /// `write_to_handle_value_trying` File branch and the VM-native output
    /// dispatch (③ native IO PR-D Tier-2a). The caller guarantees the target is
    /// `File`; Stdout/Stderr/Socket are handled elsewhere.
    pub(crate) fn write_file_payload(&mut self, bytes: &[u8]) -> Result<(), RuntimeError> {
        if matches!(self.mode, IoHandleMode::Read) {
            return Err(RuntimeError::new("Handle not open for writing"));
        }
        if self.file.is_none() {
            return Err(RuntimeError::new("IO::Handle is not attached to a file"));
        }
        if let Some(capacity) = self.out_buffer_capacity {
            // capacity 0 (unbuffered) or a payload larger than the buffer:
            // flush queued data, then write straight through.
            if capacity == 0 || bytes.len() > capacity {
                self.flush_buffer()?;
                if let Some(file) = self.file.as_mut() {
                    file.write_all(bytes).map_err(|err| {
                        RuntimeError::new(format!("Failed to write to file: {}", err))
                    })?;
                }
                return Ok(());
            }
            if self.out_buffer_pending.len() + bytes.len() > capacity {
                self.flush_buffer()?;
            }
            self.out_buffer_pending.extend_from_slice(bytes);
            Ok(())
        } else if let Some(file) = self.file.as_mut() {
            file.write_all(bytes)
                .map_err(|err| RuntimeError::new(format!("Failed to write to file: {}", err)))?;
            Ok(())
        } else {
            Err(RuntimeError::new("IO::Handle is not attached to a file"))
        }
    }

    /// Whether a text payload can be written to this handle entirely in the VM
    /// (③ native IO PR-D Tier-2a): a `File` target with a UTF-8 / binary
    /// encoding, where the bytes are the payload verbatim (no `encode_with_encoding`
    /// re-entry) and the write touches only handle state (no `emit_output`).
    /// Stdout/Stderr (need `emit_output` / `stderr_output`), Socket, and any
    /// non-UTF-8 File encoding return `false` and fall through to the interpreter.
    pub(crate) fn can_native_text_write(&self) -> bool {
        matches!(self.target, IoHandleTarget::File)
            && (self.encoding.is_empty()
                || self.encoding == "utf-8"
                || self.encoding == "utf8"
                || self.encoding == "bin")
    }

    /// VM-native text write to a `File` handle (`print`/`say`/`put`): error on a
    /// closed handle, append `nl_out` when `newline`, account the bytes, and do
    /// the buffered file write. Mirrors `write_to_handle_value_trying`'s File
    /// path exactly for the UTF-8 case. `trying` names the op for the
    /// closed-handle error. Caller guarantees [`can_native_text_write`].
    pub(crate) fn native_text_write(
        &mut self,
        content: &str,
        newline: bool,
        trying: &str,
    ) -> Result<(), RuntimeError> {
        if self.closed {
            return Err(RuntimeError::io_closed(trying));
        }
        let mut payload = String::from(content);
        if newline {
            payload.push_str(&self.nl_out);
        }
        self.bytes_written += payload.len() as i64;
        self.write_file_payload(payload.as_bytes())
    }

    /// `.flush` — flush the `:out-buffer` pending bytes and the OS file buffer.
    /// Pure handle state (no `Interpreter`), so the VM-native dispatch and the
    /// interpreter's `flush` handler share it (③ native IO PR-D Tier-2b). Works
    /// for any target: Stdout/Stderr have no file, so only `flush_buffer` (a
    /// no-op when nothing is pending) runs.
    pub(crate) fn flush_for_method(&mut self) -> Result<(), RuntimeError> {
        self.flush_buffer()?;
        if let Some(file) = self.file.as_mut() {
            file.flush()
                .map_err(|err| RuntimeError::new(format!("Failed to flush handle: {}", err)))?;
        }
        Ok(())
    }

    /// VM-native `.print-nl` on a `File` handle: write the handle's `nl_out`
    /// terminator. Mirrors the interpreter's `print-nl` (read `nl_out`, then
    /// `write_to_handle_value(.., newline = false)`). Caller guarantees
    /// [`can_native_text_write`].
    pub(crate) fn native_print_nl(&mut self) -> Result<(), RuntimeError> {
        if self.closed {
            return Err(RuntimeError::io_closed("write"));
        }
        let nl = self.nl_out.clone();
        self.bytes_written += nl.len() as i64;
        self.write_file_payload(nl.as_bytes())
    }

    /// `.tell` — current byte offset (file position, or bytes written for
    /// non-file targets).
    pub(crate) fn tell(&mut self) -> Result<i64, RuntimeError> {
        if self.closed {
            return Err(RuntimeError::io_closed("handle operation"));
        }
        match self.target {
            IoHandleTarget::File => {
                let file = self
                    .file
                    .as_mut()
                    .ok_or_else(|| RuntimeError::new("IO::Handle is not attached to a file"))?;
                let pos = file.stream_position().map_err(|err| {
                    RuntimeError::new(format!("Failed to query position: {}", err))
                })?;
                Ok(pos as i64)
            }
            _ => Ok(self.bytes_written),
        }
    }

    /// `.eof` — whether the read position is at end of file.
    pub(crate) fn eof(&mut self) -> Result<bool, RuntimeError> {
        if self.closed {
            return Err(RuntimeError::io_closed("handle operation"));
        }
        match self.target {
            IoHandleTarget::File => {
                let file = self
                    .file
                    .as_mut()
                    .ok_or_else(|| RuntimeError::new("IO::Handle is not attached to a file"))?;
                let pos = file.stream_position().map_err(|err| {
                    RuntimeError::new(format!("Failed to query position: {}", err))
                })?;
                let end = file
                    .metadata()
                    .map_err(|err| RuntimeError::new(format!("Failed to stat file: {}", err)))?
                    .len();
                // Raku semantics: a freshly opened file where pos == 0 and the
                // reported size is 0 returns False for .eof until a read or seek
                // has been performed (handles truly empty files and /proc
                // virtual files that report size 0).
                if pos == 0 && end == 0 && !self.read_attempted {
                    return Ok(false);
                }
                Ok(pos >= end)
            }
            IoHandleTarget::Stdin | IoHandleTarget::ArgFiles => Ok(false),
            _ => Err(RuntimeError::new("Handle not readable")),
        }
    }

    /// `.seek($offset, $whence)` — reposition the file cursor. `mode`: 0 = from
    /// beginning, 1 = from current, 2 = from end. Returns the new position.
    pub(crate) fn seek(&mut self, pos: i64, mode: i32) -> Result<i64, RuntimeError> {
        if self.closed {
            return Err(RuntimeError::io_closed("handle operation"));
        }
        self.read_attempted = true;
        let file = self
            .file
            .as_mut()
            .ok_or_else(|| RuntimeError::new("IO::Handle is not attached to a file"))?;
        let seek_from = match mode {
            1 => SeekFrom::Current(pos),
            2 => SeekFrom::End(pos),
            _ => {
                if pos < 0 {
                    return Err(RuntimeError::new(
                        "X::IO::Seek: Cannot seek to a negative position",
                    ));
                }
                SeekFrom::Start(pos as u64)
            }
        };
        if mode == 1 || mode == 2 {
            let current = file
                .stream_position()
                .map_err(|err| RuntimeError::new(format!("Failed to seek: {}", err)))?;
            let target_pos = match mode {
                1 => current as i64 + pos,
                2 => {
                    let end = file
                        .seek(SeekFrom::End(0))
                        .map_err(|err| RuntimeError::new(format!("Failed to seek: {}", err)))?;
                    file.seek(SeekFrom::Start(current))
                        .map_err(|err| RuntimeError::new(format!("Failed to seek: {}", err)))?;
                    end as i64 + pos
                }
                _ => unreachable!(),
            };
            if target_pos < 0 {
                return Err(RuntimeError::new(
                    "X::IO::Seek: Cannot seek to a negative position",
                ));
            }
        }
        let new_pos = file
            .seek(seek_from)
            .map_err(|err| RuntimeError::new(format!("Failed to seek: {}", err)))?;
        Ok(new_pos as i64)
    }

    /// `.close` — flush + close a file handle. Returns false if already closed.
    pub(crate) fn close(&mut self) -> Result<bool, RuntimeError> {
        if self.closed {
            return Ok(false);
        }
        if matches!(self.target, IoHandleTarget::File) {
            self.flush_buffer()?;
            if let Some(file) = self.file.as_mut() {
                file.flush()
                    .map_err(|err| RuntimeError::new(format!("Failed to flush handle: {}", err)))?;
            }
        }
        self.closed = true;
        self.file = None;
        Ok(true)
    }

    /// `.opened` — whether the handle is still open.
    pub(crate) fn is_opened(&self) -> bool {
        !self.closed
    }

    /// `.t` — whether the handle is attached to a TTY.
    pub(crate) fn is_tty(&self) -> bool {
        use std::io::IsTerminal;
        match self.target {
            IoHandleTarget::Stdin => std::io::stdin().is_terminal(),
            IoHandleTarget::Stdout => std::io::stdout().is_terminal(),
            IoHandleTarget::Stderr => std::io::stderr().is_terminal(),
            IoHandleTarget::File => self.file.as_ref().is_some_and(|f| f.is_terminal()),
            _ => false,
        }
    }

    /// `.chomp` getter/setter — auto-chomp on line reads. `set` carries the new
    /// truthy value when called as a setter; returns the resulting setting.
    pub(crate) fn chomp_setting(&mut self, set: Option<bool>) -> bool {
        if let Some(v) = set {
            self.line_chomp = v;
        }
        self.line_chomp
    }

    /// `.nl-out` getter/setter — the output line terminator. Returns the
    /// resulting terminator string.
    pub(crate) fn nl_out_setting(&mut self, set: Option<String>) -> String {
        if let Some(v) = set {
            self.nl_out = v;
        }
        self.nl_out.clone()
    }

    /// `.out-buffer` getter/setter — output buffer capacity. When `set` is
    /// `Some(cap)` the pending buffer is flushed first and the capacity is
    /// replaced (a `None` capacity means unbounded/disabled). Returns the
    /// resulting capacity (0 when unset).
    pub(crate) fn out_buffer_setting(
        &mut self,
        set: Option<Option<usize>>,
    ) -> Result<usize, RuntimeError> {
        if let Some(cap) = set {
            self.flush_buffer()?;
            self.out_buffer_capacity = cap;
        }
        Ok(self.out_buffer_capacity.unwrap_or(0))
    }

    /// `.encoding` getter/setter — the handle's character encoding (`"bin"` for
    /// binary mode). On set, returns the *previous* encoding; on get, the
    /// current one (mirrors the interpreter's `set_handle_encoding`).
    pub(crate) fn encoding_setting(&mut self, set: Option<String>) -> String {
        match set {
            Some(enc) => std::mem::replace(&mut self.encoding, enc),
            None => self.encoding.clone(),
        }
    }

    /// `.native-descriptor` — the underlying OS file descriptor. Std streams map
    /// to 0/1/2; file handles report their real `fd` on Unix.
    pub(crate) fn native_descriptor(&self) -> Result<i64, RuntimeError> {
        match self.target {
            IoHandleTarget::Stdin => Ok(0),
            IoHandleTarget::Stdout => Ok(1),
            IoHandleTarget::Stderr => Ok(2),
            _ => {
                #[cfg(unix)]
                {
                    if let Some(ref file) = self.file {
                        use std::os::unix::io::AsRawFd;
                        Ok(file.as_raw_fd() as i64)
                    } else {
                        Err(RuntimeError::new(
                            "native-descriptor: handle has no file descriptor",
                        ))
                    }
                }
                #[cfg(not(unix))]
                {
                    Err(RuntimeError::new(
                        "native-descriptor: not supported on this platform",
                    ))
                }
            }
        }
    }
}

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

    fn normalize_line_separators(mut separators: Vec<Vec<u8>>) -> Vec<Vec<u8>> {
        separators.retain(|sep| !sep.is_empty());
        if separators.is_empty() {
            return vec![b"\r\n".to_vec(), b"\n".to_vec()];
        }
        separators.sort_by_key(|sep| std::cmp::Reverse(sep.len()));
        separators.dedup();
        separators
    }

    fn parse_nl_in_value(value: &Value) -> Vec<Vec<u8>> {
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
                if self.tap.subtest_depth() == 0 && self.immediate_stdout {
                    use std::io::Write;
                    let _ = std::io::stderr().write_all(payload.as_bytes());
                    let _ = std::io::stderr().flush();
                } else {
                    self.stderr_output.push_str(&payload);
                }
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
                if self.tap.subtest_depth() == 0 && self.immediate_stdout {
                    use std::io::Write;
                    let _ = std::io::stderr().write_all(bytes);
                    let _ = std::io::stderr().flush();
                } else {
                    let content = String::from_utf8_lossy(bytes).to_string();
                    self.stderr_output.push_str(&content);
                }
                Ok(())
            }
            IoHandleTarget::File => self.with_handle_mut(handle_value, |state| {
                if let Some(file) = state.file.as_mut() {
                    file.write_all(bytes).map_err(|err| {
                        RuntimeError::new(format!("Failed to write to file: {}", err))
                    })?;
                    Ok(())
                } else {
                    Err(RuntimeError::new("IO::Handle is not attached to a file"))
                }
            }),
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

    fn read_record_bytes<R: Read>(
        reader: &mut R,
        separators: &[Vec<u8>],
        chomp: bool,
    ) -> Result<Option<Vec<u8>>, RuntimeError> {
        let mut buffer = Vec::new();
        let mut byte = [0u8];
        let mut read_any = false;
        loop {
            let n = reader
                .read(&mut byte)
                .map_err(|err| RuntimeError::new(format!("Failed to read: {}", err)))?;
            if n == 0 {
                break;
            }
            read_any = true;
            buffer.push(byte[0]);
            if let Some(matched_len) = separators
                .iter()
                .find_map(|sep| buffer.ends_with(sep).then_some(sep.len()))
            {
                if chomp {
                    buffer.truncate(buffer.len().saturating_sub(matched_len));
                }
                break;
            }
        }
        if !read_any {
            return Ok(None);
        }
        Ok(Some(buffer))
    }

    fn read_record_with_separators<R: Read>(
        reader: &mut R,
        separators: &[Vec<u8>],
        chomp: bool,
    ) -> Result<Option<String>, RuntimeError> {
        match Self::read_record_bytes(reader, separators, chomp)? {
            Some(buffer) => Ok(Some(String::from_utf8_lossy(&buffer).to_string())),
            None => Ok(None),
        }
    }

    pub(crate) fn read_line_from_handle_value(
        &mut self,
        handle_value: &Value,
    ) -> Result<Option<String>, RuntimeError> {
        // Pre-extract @*ARGS file list for ArgFiles handle before borrowing state
        let argfiles_list: Vec<String> = self
            .env
            .get("@*ARGS")
            .and_then(|v| {
                if let Value::Array(items, ..) = v {
                    Some(items.iter().map(|v| v.to_string_value()).collect())
                } else {
                    None
                }
            })
            .unwrap_or_default();

        // Pre-extract $*IN's line separators for use when ArgFiles falls back to stdin
        let stdin_seps: Option<(Vec<Vec<u8>>, bool)> =
            self.get_dynamic_handle("$*IN").and_then(|in_handle| {
                let id = Self::handle_id_from_value(&in_handle)?;
                let table = self.io_handles();
                let in_state = table.map.get(&id)?;
                Some((in_state.line_separators.clone(), in_state.line_chomp))
            });

        // The File branch may need to decode the raw record via
        // `self.decode_with_encoding`, which re-enters `self`. Read the record
        // inside the confined handle borrow, then decode outside it.
        enum LineOutcome {
            Done(Option<String>),
            NeedsDecode(Vec<u8>, String),
        }
        let outcome = self.with_handle_mut(handle_value, |state| {
            if state.closed {
                return Err(RuntimeError::io_closed("handle operation"));
            }
            state.read_attempted = true;
            let encoding = state.encoding.clone();
            let needs_decode = !encoding.is_empty()
                && encoding != "utf-8"
                && encoding != "utf8"
                && encoding != "bin";
            match state.target {
                IoHandleTarget::Stdout | IoHandleTarget::Stderr => {
                    Err(RuntimeError::new("Handle not readable"))
                }
                IoHandleTarget::Stdin => {
                    let seps = state.line_separators.clone();
                    let chomp = state.line_chomp;
                    let mut stdin = std::io::stdin().lock();
                    Self::read_record_with_separators(&mut stdin, &seps, chomp)
                        .map(LineOutcome::Done)
                }
                IoHandleTarget::ArgFiles => {
                    let seps = state.line_separators.clone();
                    let chomp = state.line_chomp;
                    // An `IO::ArgFiles.new(@files)` handle carries its own file
                    // list; a plain `$*ARGFILES` handle falls back to `@*ARGS`.
                    let effective_list = match &state.argfiles_paths {
                        Some(paths) => paths.clone(),
                        None => argfiles_list,
                    };
                    if effective_list.is_empty() {
                        // No file args — read from stdin, using $*IN's nl-in.
                        let (effective_seps, effective_chomp) =
                            stdin_seps.unwrap_or((seps.clone(), chomp));
                        let mut stdin = std::io::stdin().lock();
                        return Self::read_record_with_separators(
                            &mut stdin,
                            &effective_seps,
                            effective_chomp,
                        )
                        .map(LineOutcome::Done);
                    }
                    // Read from files listed in the effective list sequentially
                    loop {
                        if state.argfiles_index >= effective_list.len() {
                            return Ok(LineOutcome::Done(None));
                        }
                        // Open the next file if we don't have a reader
                        if state.argfiles_reader.is_none() {
                            let path = &effective_list[state.argfiles_index];
                            if path == "-" {
                                // `-` means read from stdin
                                let mut stdin = std::io::stdin().lock();
                                match Self::read_record_with_separators(&mut stdin, &seps, chomp)? {
                                    Some(line) => return Ok(LineOutcome::Done(Some(line))),
                                    None => {
                                        state.argfiles_index += 1;
                                        continue;
                                    }
                                }
                            }
                            let file = std::fs::File::open(path).map_err(|e| {
                                RuntimeError::new(format!("Failed to open file '{}': {}", path, e))
                            })?;
                            state.argfiles_reader = Some(std::io::BufReader::new(file));
                        }
                        let reader = state.argfiles_reader.as_mut().unwrap();
                        match Self::read_record_with_separators(reader, &seps, chomp)? {
                            Some(line) => return Ok(LineOutcome::Done(Some(line))),
                            None => {
                                // Current file exhausted, move to next
                                state.argfiles_reader = None;
                                state.argfiles_index += 1;
                            }
                        }
                    }
                }
                IoHandleTarget::File => {
                    let seps = state.line_separators.clone();
                    let chomp = state.line_chomp;
                    let file = state
                        .file
                        .as_mut()
                        .ok_or_else(|| RuntimeError::new("IO::Handle is not attached to a file"))?;
                    if needs_decode {
                        match Self::read_record_bytes(file, &seps, chomp)? {
                            Some(bytes) => Ok(LineOutcome::NeedsDecode(bytes, encoding)),
                            None => Ok(LineOutcome::Done(None)),
                        }
                    } else {
                        Self::read_record_with_separators(file, &seps, chomp).map(LineOutcome::Done)
                    }
                }
                IoHandleTarget::Socket => {
                    let sock = state
                        .socket
                        .as_mut()
                        .ok_or_else(|| RuntimeError::new("Socket not connected"))?;
                    Self::read_record_with_separators(
                        sock,
                        &state.line_separators,
                        state.line_chomp,
                    )
                    .map(LineOutcome::Done)
                }
            }
        })?;
        match outcome {
            LineOutcome::Done(line) => Ok(line),
            LineOutcome::NeedsDecode(bytes, encoding) => {
                let decoded = self.decode_with_encoding(&bytes, &encoding)?;
                Ok(Some(decoded))
            }
        }
    }

    /// Read the next whitespace-delimited word from a handle, buffering the
    /// leftover words of each line in `pending_words`. Returns `None` at EOF,
    /// auto-closing the handle if `close_on_word_exhaust` was requested
    /// (Raku's `words($fh, :close)` close-on-exhaust semantics). Because the
    /// handle only closes when iteration actually reaches EOF, a partial
    /// consumer (e.g. `words($fh, :close)[1,2]`) leaves the handle open.
    pub(crate) fn read_word_from_handle_value(
        &mut self,
        handle_value: &Value,
    ) -> Result<Option<String>, RuntimeError> {
        loop {
            if let Some(word) =
                self.with_handle_mut(handle_value, |state| Ok(state.pending_words.pop_front()))?
            {
                return Ok(Some(word));
            }
            match self.read_line_from_handle_value(handle_value)? {
                Some(line) => {
                    let words: Vec<String> =
                        line.split_whitespace().map(|s| s.to_string()).collect();
                    if words.is_empty() {
                        continue;
                    }
                    self.with_handle_mut(handle_value, |state| {
                        state.pending_words.extend(words);
                        Ok(())
                    })?;
                }
                None => {
                    let should_close = self.with_handle_mut(handle_value, |state| {
                        Ok(state.close_on_word_exhaust && !state.closed)
                    })?;
                    if should_close {
                        let _ = self.close_handle_value(handle_value);
                    }
                    return Ok(None);
                }
            }
        }
    }

    pub(super) fn read_bytes_from_handle_value(
        &mut self,
        handle_value: &Value,
        count: usize,
    ) -> Result<Vec<u8>, RuntimeError> {
        // Validate, mark the handle read, and capture the target plus any
        // handle-owned ArgFiles list inside a confined borrow. The ArgFiles
        // fallback needs `@*ARGS` from `self.env`, so it runs between borrows.
        let (target, own_paths) = self.with_handle_mut(handle_value, |state| {
            if state.closed {
                return Err(RuntimeError::io_closed("handle operation"));
            }
            state.read_attempted = true;
            Ok((state.target, state.argfiles_paths.clone()))
        })?;
        if matches!(target, IoHandleTarget::ArgFiles) {
            // Read bytes from files listed in @*ARGS sequentially, unless the
            // handle carries its own explicit list (`IO::ArgFiles.new(@files)`).
            let argfiles_list: Vec<String> = match own_paths {
                Some(paths) => paths,
                None => self
                    .env
                    .get("@*ARGS")
                    .and_then(|v| {
                        if let Value::Array(items, ..) = v {
                            Some(items.iter().map(|v| v.to_string_value()).collect())
                        } else {
                            None
                        }
                    })
                    .unwrap_or_default(),
            };
            if argfiles_list.is_empty() {
                // No file args — read from stdin
                use std::io::Read;
                let mut stdin = std::io::stdin().lock();
                let mut buffer = vec![0u8; count];
                let bytes_read = stdin.read(&mut buffer).map_err(|err| {
                    RuntimeError::new(format!("Failed to read from stdin: {}", err))
                })?;
                buffer.truncate(bytes_read);
                return Ok(buffer);
            }
            return self.with_handle_mut(handle_value, |state| {
                let mut result = Vec::new();
                loop {
                    if state.argfiles_index >= argfiles_list.len() {
                        break;
                    }
                    if state.argfiles_reader.is_none() {
                        let path = &argfiles_list[state.argfiles_index];
                        let file = std::fs::File::open(path).map_err(|e| {
                            RuntimeError::new(format!("Failed to open file '{}': {}", path, e))
                        })?;
                        state.argfiles_reader = Some(std::io::BufReader::new(file));
                    }
                    let reader = state.argfiles_reader.as_mut().unwrap();
                    let mut buffer = vec![0u8; count.min(8192)];
                    let bytes_read = reader
                        .read(&mut buffer)
                        .map_err(|err| RuntimeError::new(format!("Failed to read: {}", err)))?;
                    if bytes_read == 0 {
                        state.argfiles_reader = None;
                        state.argfiles_index += 1;
                        continue;
                    }
                    buffer.truncate(bytes_read);
                    result.extend(buffer);
                    if result.len() >= count {
                        break;
                    }
                }
                result.truncate(count);
                Ok(result)
            });
        }
        // Non-ArgFiles targets touch only the handle state.
        self.with_handle_mut(handle_value, |state| match state.target {
            IoHandleTarget::Stdout | IoHandleTarget::Stderr => {
                Err(RuntimeError::new("Handle not readable"))
            }
            IoHandleTarget::Stdin => {
                use std::io::Read;
                let mut stdin = std::io::stdin().lock();
                let mut buffer = vec![0u8; count];
                let bytes_read = stdin.read(&mut buffer).map_err(|err| {
                    RuntimeError::new(format!("Failed to read from stdin: {}", err))
                })?;
                buffer.truncate(bytes_read);
                Ok(buffer)
            }
            IoHandleTarget::File => {
                let file = state
                    .file
                    .as_mut()
                    .ok_or_else(|| RuntimeError::new("IO::Handle is not attached to a file"))?;
                let mut buffer = vec![0u8; count];
                let bytes = file
                    .read(&mut buffer)
                    .map_err(|err| RuntimeError::new(format!("Failed to read: {}", err)))?;
                buffer.truncate(bytes);
                Ok(buffer)
            }
            IoHandleTarget::Socket => {
                let sock = state
                    .socket
                    .as_mut()
                    .ok_or_else(|| RuntimeError::new("Socket not connected"))?;
                let mut buffer = vec![0u8; count];
                let bytes = sock
                    .read(&mut buffer)
                    .map_err(|err| RuntimeError::new(format!("Failed to read: {}", err)))?;
                buffer.truncate(bytes);
                Ok(buffer)
            }
            IoHandleTarget::ArgFiles => unreachable!("ArgFiles handled above"),
        })
    }

    fn read_utf8_char<R: Read>(reader: &mut R) -> Result<Option<String>, RuntimeError> {
        let mut first = [0u8; 1];
        let n = reader
            .read(&mut first)
            .map_err(|err| RuntimeError::new(format!("Failed to read: {}", err)))?;
        if n == 0 {
            return Ok(None);
        }

        let expected_len = match first[0] {
            0x00..=0x7F => 1usize,
            0xC0..=0xDF => 2usize,
            0xE0..=0xEF => 3usize,
            0xF0..=0xF7 => 4usize,
            _ => 1usize,
        };

        let mut bytes = vec![first[0]];
        if expected_len > 1 {
            let mut rest = vec![0u8; expected_len - 1];
            let mut total = 0usize;
            while total < rest.len() {
                let read = reader
                    .read(&mut rest[total..])
                    .map_err(|err| RuntimeError::new(format!("Failed to read: {}", err)))?;
                if read == 0 {
                    break;
                }
                total += read;
            }
            bytes.extend_from_slice(&rest[..total]);
        }

        Ok(Some(String::from_utf8_lossy(&bytes).to_string()))
    }

    /// Read one character from a UTF-16 stream.
    /// `big_endian` controls byte order. Handles surrogate pairs.
    fn read_utf16_char<R: Read>(
        reader: &mut R,
        big_endian: bool,
    ) -> Result<Option<String>, RuntimeError> {
        let mut buf = [0u8; 2];
        let n = reader
            .read(&mut buf)
            .map_err(|err| RuntimeError::new(format!("Failed to read: {}", err)))?;
        if n == 0 {
            return Ok(None);
        }
        if n < 2 {
            // Incomplete code unit — replacement char
            return Ok(Some("\u{FFFD}".to_string()));
        }
        let unit = if big_endian {
            u16::from_be_bytes(buf)
        } else {
            u16::from_le_bytes(buf)
        };
        // Check for surrogate pair
        if (0xD800..=0xDBFF).contains(&unit) {
            // High surrogate — read low surrogate
            let mut buf2 = [0u8; 2];
            let n2 = reader
                .read(&mut buf2)
                .map_err(|err| RuntimeError::new(format!("Failed to read: {}", err)))?;
            if n2 < 2 {
                return Ok(Some("\u{FFFD}".to_string()));
            }
            let low = if big_endian {
                u16::from_be_bytes(buf2)
            } else {
                u16::from_le_bytes(buf2)
            };
            let codepoint = 0x10000 + ((unit as u32 - 0xD800) << 10) + (low as u32 - 0xDC00);
            match char::from_u32(codepoint) {
                Some(ch) => Ok(Some(ch.to_string())),
                None => Ok(Some("\u{FFFD}".to_string())),
            }
        } else {
            match char::from_u32(unit as u32) {
                Some(ch) => Ok(Some(ch.to_string())),
                None => Ok(Some("\u{FFFD}".to_string())),
            }
        }
    }

    pub(super) fn read_chars_from_handle_value(
        &mut self,
        handle_value: &Value,
        count: Option<usize>,
    ) -> Result<String, RuntimeError> {
        self.with_handle_mut(handle_value, |state| {
            if state.closed {
                return Err(RuntimeError::io_closed("handle operation"));
            }
            state.read_attempted = true;
            let encoding = state.encoding.to_lowercase();
            // Determine if this is a utf16 stream and its byte order
            let utf16_auto = matches!(encoding.as_str(), "utf-16" | "utf16");
            let utf16_mode = match encoding.as_str() {
                "utf-16be" | "utf16be" => Some(true),  // big endian
                "utf-16le" | "utf16le" => Some(false), // little endian
                "utf-16" | "utf16" => {
                    // Use previously detected endianness, or default to native
                    Some(
                        state
                            .utf16_detected_be
                            .unwrap_or(cfg!(target_endian = "big")),
                    )
                }
                _ => None,
            };
            match state.target {
                IoHandleTarget::Stdout | IoHandleTarget::Stderr => {
                    Err(RuntimeError::new("Handle not readable"))
                }
                IoHandleTarget::Stdin => {
                    let mut stdin = std::io::stdin().lock();
                    if let Some(limit) = count {
                        if limit == 0 {
                            return Ok(String::new());
                        }
                        let mut out = String::new();
                        for _ in 0..limit {
                            let Some(ch) = Self::read_utf8_char(&mut stdin)? else {
                                break;
                            };
                            out.push_str(&ch);
                        }
                        Ok(out)
                    } else {
                        let mut bytes = Vec::new();
                        stdin
                            .read_to_end(&mut bytes)
                            .map_err(|err| RuntimeError::new(format!("Failed to read: {}", err)))?;
                        Ok(String::from_utf8_lossy(&bytes).to_string())
                    }
                }
                IoHandleTarget::ArgFiles => Ok(String::new()),
                IoHandleTarget::File => {
                    let file = state
                        .file
                        .as_mut()
                        .ok_or_else(|| RuntimeError::new("IO::Handle is not attached to a file"))?;
                    if let Some(mut big_endian) = utf16_mode {
                        // For utf16 auto-detect: detect BOM on first read
                        if utf16_auto && state.utf16_detected_be.is_none() {
                            let mut bom_buf = [0u8; 2];
                            let n = file.read(&mut bom_buf).map_err(|err| {
                                RuntimeError::new(format!("Failed to read: {}", err))
                            })?;
                            if n >= 2 {
                                if bom_buf[0] == 0xFE && bom_buf[1] == 0xFF {
                                    big_endian = true;
                                    state.utf16_detected_be = Some(true);
                                    // BOM consumed, don't include in output
                                } else if bom_buf[0] == 0xFF && bom_buf[1] == 0xFE {
                                    big_endian = false;
                                    state.utf16_detected_be = Some(false);
                                    // BOM consumed, don't include in output
                                } else {
                                    // No BOM, seek back
                                    state.utf16_detected_be = Some(cfg!(target_endian = "big"));
                                    use std::io::Seek;
                                    let _ = file.seek(std::io::SeekFrom::Current(-2));
                                }
                            }
                        }
                        if let Some(limit) = count {
                            if limit == 0 {
                                return Ok(String::new());
                            }
                            let mut out = String::new();
                            for _ in 0..limit {
                                let Some(ch) = Self::read_utf16_char(file, big_endian)? else {
                                    break;
                                };
                                out.push_str(&ch);
                            }
                            Ok(out)
                        } else {
                            let mut bytes = Vec::new();
                            file.read_to_end(&mut bytes).map_err(|err| {
                                RuntimeError::new(format!("Failed to read: {}", err))
                            })?;
                            // Decode all bytes using the utf16 decoder
                            let units: Vec<u16> = bytes
                                .chunks_exact(2)
                                .map(|c| {
                                    if big_endian {
                                        u16::from_be_bytes([c[0], c[1]])
                                    } else {
                                        u16::from_le_bytes([c[0], c[1]])
                                    }
                                })
                                .collect();
                            Ok(String::from_utf16_lossy(&units))
                        }
                    } else if let Some(limit) = count {
                        if limit == 0 {
                            return Ok(String::new());
                        }
                        let mut out = String::new();
                        for _ in 0..limit {
                            let Some(ch) = Self::read_utf8_char(file)? else {
                                break;
                            };
                            out.push_str(&ch);
                        }
                        Ok(out)
                    } else {
                        let mut bytes = Vec::new();
                        file.read_to_end(&mut bytes)
                            .map_err(|err| RuntimeError::new(format!("Failed to read: {}", err)))?;
                        Ok(String::from_utf8_lossy(&bytes).to_string())
                    }
                }
                IoHandleTarget::Socket => {
                    let sock = state
                        .socket
                        .as_mut()
                        .ok_or_else(|| RuntimeError::new("Socket not connected"))?;
                    if let Some(limit) = count {
                        if limit == 0 {
                            return Ok(String::new());
                        }
                        let mut out = String::new();
                        for _ in 0..limit {
                            let Some(ch) = Self::read_utf8_char(sock)? else {
                                break;
                            };
                            out.push_str(&ch);
                        }
                        Ok(out)
                    } else {
                        let mut bytes = Vec::new();
                        sock.read_to_end(&mut bytes)
                            .map_err(|err| RuntimeError::new(format!("Failed to read: {}", err)))?;
                        Ok(String::from_utf8_lossy(&bytes).to_string())
                    }
                }
            }
        })
    }

    pub(super) fn seek_handle_value(
        &mut self,
        handle_value: &Value,
        pos: i64,
        mode: i32,
    ) -> Result<i64, RuntimeError> {
        self.with_handle_mut(handle_value, |state| state.seek(pos, mode))
    }

    pub(super) fn tell_handle_value(&mut self, handle_value: &Value) -> Result<i64, RuntimeError> {
        self.with_handle_mut(handle_value, |state| state.tell())
    }

    pub(super) fn handle_eof_value(&mut self, handle_value: &Value) -> Result<bool, RuntimeError> {
        self.with_handle_mut(handle_value, |state| state.eof())
    }

    pub(super) fn set_handle_encoding(
        &mut self,
        handle_value: &Value,
        encoding: Option<String>,
    ) -> Result<String, RuntimeError> {
        self.with_handle_mut(handle_value, |state| Ok(state.encoding_setting(encoding)))
    }

    pub(super) fn system_time_to_int(time: SystemTime) -> i64 {
        match time.duration_since(UNIX_EPOCH) {
            Ok(duration) => duration.as_secs() as i64,
            Err(_) => 0,
        }
    }

    #[allow(clippy::type_complexity)]
    pub(super) fn parse_io_flags_values(
        &self,
        args: &[Value],
    ) -> (
        bool,
        bool,
        bool,
        bool,
        bool,
        Vec<Vec<u8>>,
        Option<usize>,
        Option<String>,
        Option<String>,
        bool,
        bool,
    ) {
        let mut read = false;
        let mut write = false;
        let mut append = false;
        let mut bin = false;
        let mut chomp = true;
        let mut create = false;
        let mut exclusive = false;
        let mut nl_in: Option<Vec<Vec<u8>>> = None;
        let mut out_buffer: Option<usize> = None;
        let mut nl_out: Option<String> = None;
        let mut enc: Option<String> = None;
        for arg in args {
            if let Value::Pair(name, value) = arg {
                let truthy = value.truthy();
                match name.as_str() {
                    "r" => read = truthy,
                    "w" => write = truthy,
                    // `:rw` == `:mode<rw>, :create` — unlike a bare `:mode<rw>`
                    // (== `:update`), the `:rw` flag creates the file if missing.
                    "rw" => {
                        read = truthy;
                        write = truthy;
                        if truthy {
                            create = true;
                        }
                    }
                    "a" | "append" => append = truthy,
                    // `:update` == `:mode<rw>`; `:ra` == :mode<rw>,:create,:append;
                    // `:rx` == :mode<rw>,:create,:exclusive; `:x` == :mode<wo>,
                    // :create,:exclusive (named-flag spellings of the open modes).
                    "update" => {
                        if truthy {
                            read = true;
                            write = true;
                        }
                    }
                    "ra" => {
                        if truthy {
                            read = true;
                            write = true;
                            create = true;
                            append = true;
                        }
                    }
                    "rx" => {
                        if truthy {
                            read = true;
                            write = true;
                            create = true;
                            exclusive = true;
                        }
                    }
                    "x" => {
                        if truthy {
                            write = true;
                            create = true;
                            exclusive = true;
                        }
                    }
                    "exclusive" => exclusive = truthy,
                    "truncate" => {}
                    "bin" => bin = truthy,
                    "chomp" => chomp = truthy,
                    "create" => create = truthy,
                    "nl-in" => nl_in = Some(Self::parse_nl_in_value(value)),
                    "nl-out" => nl_out = Some(value.to_string_value()),
                    "out-buffer" => out_buffer = Self::parse_out_buffer_size(value),
                    "enc" => enc = Some(value.to_string_value()),
                    "mode" => {
                        // :mode<ro> = read-only, :mode<wo> = write-only, :mode<rw> = read-write
                        let mode_str = value.to_string_value();
                        match mode_str.as_str() {
                            "ro" => read = true,
                            "wo" => write = true,
                            "rw" => {
                                read = true;
                                write = true;
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                }
            }
        }
        if !read && !write && !append {
            read = true;
        }
        (
            read,
            write,
            append,
            bin,
            chomp,
            Self::normalize_line_separators(
                nl_in.unwrap_or_else(|| self.default_line_separators()),
            ),
            out_buffer,
            nl_out,
            enc,
            create,
            exclusive,
        )
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn open_file_handle(
        &mut self,
        path: &Path,
        read: bool,
        write: bool,
        append: bool,
        bin: bool,
        line_chomp: bool,
        line_separators: Vec<Vec<u8>>,
        out_buffer_capacity: Option<usize>,
        nl_out: Option<String>,
        enc: Option<String>,
        create: bool,
        exclusive: bool,
    ) -> Result<Value, RuntimeError> {
        // Opening a directory as a file handle is an error in Raku
        if path.is_dir() {
            return Err(RuntimeError::new(format!(
                "Failed to open '{}': Is a directory",
                path.display()
            )));
        }
        let mut options = fs::OpenOptions::new();
        options.read(read);
        options.write(write || append);
        if append {
            options.append(true).create(true);
        } else if write && !read {
            if create {
                // :create without truncate - create if missing but don't truncate
                options.create(true);
            } else {
                options.create(true).truncate(true);
            }
        }
        // Read-write (`:mode<rw>` / `:update`) does NOT create the file on its
        // own — only `:create` (i.e. `:rw`) does. Opening a missing file for
        // read-write without `:create` must fail (return a Failure), matching
        // rakudo. So creation is driven solely by the explicit `:create` flag.
        if create {
            options.create(true);
        }
        // :exclusive / :x — fail if the file already exists (O_EXCL). Takes
        // precedence over plain :create so `open(:create, :exclusive)` errors
        // on an existing file instead of opening it.
        if exclusive {
            options.create(false).create_new(true);
        }
        let file = options.open(path).map_err(|err| {
            RuntimeError::new(format!("Failed to open '{}': {}", path.display(), err))
        })?;
        let mode = if read && (write || append) {
            IoHandleMode::ReadWrite
        } else if append {
            IoHandleMode::Append
        } else if write {
            IoHandleMode::Write
        } else {
            IoHandleMode::Read
        };
        let mut state = IoHandleState {
            target: IoHandleTarget::File,
            mode,
            path: Some(Self::stringify_path(path)),
            line_separators: Self::normalize_line_separators(line_separators),
            line_chomp,
            encoding: if bin {
                "bin".to_string()
            } else {
                enc.unwrap_or_else(|| "utf-8".to_string())
            },
            file: Some(file),
            socket: None,
            listener: None,
            closed: false,
            out_buffer_capacity,
            out_buffer_pending: Vec::new(),
            bin,
            nl_out: nl_out.unwrap_or_else(|| "\n".to_string()),
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
        // For utf16 encoding in write/append mode, write BOM at start
        let enc_lower = state.encoding.to_lowercase();
        let is_utf16_no_endian = enc_lower == "utf-16" || enc_lower == "utf16";
        if is_utf16_no_endian && (write || append) {
            // For append mode, only write BOM if file is empty
            let should_write_bom = if append {
                state
                    .file
                    .as_ref()
                    .is_some_and(|f| f.metadata().is_ok_and(|m| m.len() == 0))
            } else {
                true
            };
            if should_write_bom {
                if let Some(ref mut file) = state.file {
                    use std::io::Write;
                    let bom = if cfg!(target_endian = "little") {
                        [0xFF_u8, 0xFE]
                    } else {
                        [0xFE_u8, 0xFF]
                    };
                    let _ = file.write_all(&bom);
                }
                state.utf16_bom_written = true;
            }
        }
        let id = self.insert_handle_state(state);
        Ok(self.make_handle_instance_with_bin(id, bin))
    }
}
