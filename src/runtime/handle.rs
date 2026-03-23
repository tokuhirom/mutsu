use super::*;
use num_traits::ToPrimitive;

impl Interpreter {
    pub(super) fn parse_out_buffer_size(value: &Value) -> Option<usize> {
        match value {
            Value::Int(i) if *i >= 0 => Some(*i as usize),
            Value::BigInt(i) if i.sign() != num_bigint::Sign::Minus => i.to_usize(),
            _ => None,
        }
    }

    pub(super) fn flush_file_handle_buffer(state: &mut IoHandleState) -> Result<(), RuntimeError> {
        if state.out_buffer_pending.is_empty() {
            return Ok(());
        }
        let Some(file) = state.file.as_mut() else {
            return Err(RuntimeError::new("IO::Handle is not attached to a file"));
        };
        file.write_all(&state.out_buffer_pending)
            .map_err(|err| RuntimeError::new(format!("Failed to write to file: {}", err)))?;
        state.out_buffer_pending.clear();
        Ok(())
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

    pub(super) fn handle_id_from_value(value: &Value) -> Option<usize> {
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = value
            && (class_name == "IO::Handle" || class_name == "IO::Socket::INET")
            && let Some(Value::Int(id)) = attributes.get("handle")
            && *id >= 0
        {
            return Some(*id as usize);
        }
        None
    }

    pub(super) fn handle_state_mut(
        &mut self,
        handle_value: &Value,
    ) -> Result<&mut IoHandleState, RuntimeError> {
        let id = Self::handle_id_from_value(handle_value)
            .ok_or_else(|| RuntimeError::new("Expected IO::Handle"))?;
        self.handles
            .get_mut(&id)
            .ok_or_else(|| RuntimeError::new("Invalid IO::Handle"))
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
        let id = Self::handle_id_from_value(handle_value)
            .ok_or_else(|| RuntimeError::new("Expected IO::Handle"))?;
        let state = self
            .handles
            .get_mut(&id)
            .ok_or_else(|| RuntimeError::new("Invalid IO::Handle"))?;
        if state.closed {
            return Err(RuntimeError::io_closed(trying));
        }
        let mut payload = String::from(content);
        if newline {
            payload.push_str(&state.nl_out.clone());
        }
        // Encode payload using the handle's encoding
        let encoding = state.encoding.clone();
        let needs_encoding =
            !encoding.is_empty() && encoding != "utf-8" && encoding != "utf8" && encoding != "bin";
        let encoded_bytes = if needs_encoding && matches!(state.target, IoHandleTarget::File) {
            // Drop mutable borrow on state before calling encode_with_encoding
            Some(self.encode_with_encoding(&payload, &encoding)?)
        } else {
            None
        };
        // Re-borrow state after encoding
        let state = self.handles.get_mut(&id).unwrap();
        state.bytes_written += payload.len() as i64;
        match state.target {
            IoHandleTarget::Stdout => {
                self.emit_output(&payload);
                Ok(())
            }
            IoHandleTarget::Stderr => {
                if self.subtest_depth == 0 && self.immediate_stdout {
                    use std::io::Write;
                    let _ = std::io::stderr().write_all(payload.as_bytes());
                    let _ = std::io::stderr().flush();
                } else {
                    self.stderr_output.push_str(&payload);
                }
                Ok(())
            }
            IoHandleTarget::File => {
                if matches!(state.mode, IoHandleMode::Read) {
                    return Err(RuntimeError::new("Handle not open for writing"));
                }
                let Some(_) = state.file.as_mut() else {
                    return Err(RuntimeError::new("IO::Handle is not attached to a file"));
                };
                let payload_bytes = if let Some(ref enc_bytes) = encoded_bytes {
                    enc_bytes.as_slice()
                } else {
                    payload.as_bytes()
                };
                if let Some(capacity) = state.out_buffer_capacity {
                    if capacity == 0 {
                        Self::flush_file_handle_buffer(state)?;
                        if let Some(file) = state.file.as_mut() {
                            file.write_all(payload_bytes).map_err(|err| {
                                RuntimeError::new(format!("Failed to write to file: {}", err))
                            })?;
                        }
                        return Ok(());
                    }
                    if payload_bytes.len() > capacity {
                        // Large payloads bypass buffering after flushing queued data.
                        Self::flush_file_handle_buffer(state)?;
                        if let Some(file) = state.file.as_mut() {
                            file.write_all(payload_bytes).map_err(|err| {
                                RuntimeError::new(format!("Failed to write to file: {}", err))
                            })?;
                        }
                        return Ok(());
                    }
                    if state.out_buffer_pending.len() + payload_bytes.len() > capacity {
                        Self::flush_file_handle_buffer(state)?;
                    }
                    state.out_buffer_pending.extend_from_slice(payload_bytes);
                    Ok(())
                } else if let Some(file) = state.file.as_mut() {
                    file.write_all(payload_bytes).map_err(|err| {
                        RuntimeError::new(format!("Failed to write to file: {}", err))
                    })?;
                    Ok(())
                } else {
                    Err(RuntimeError::new("IO::Handle is not attached to a file"))
                }
            }
            IoHandleTarget::Socket => {
                if let Some(sock) = state.socket.as_mut() {
                    sock.write_all(payload.as_bytes()).map_err(|err| {
                        RuntimeError::new(format!("Failed to write to socket: {}", err))
                    })?;
                    Ok(())
                } else {
                    Err(RuntimeError::new("Socket not connected"))
                }
            }
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
        let id = Self::handle_id_from_value(handle_value)
            .ok_or_else(|| RuntimeError::new("Expected IO::Handle"))?;
        let state = self
            .handles
            .get_mut(&id)
            .ok_or_else(|| RuntimeError::new("Invalid IO::Handle"))?;
        if state.closed {
            return Err(RuntimeError::io_closed("write"));
        }
        if matches!(state.mode, IoHandleMode::Read) {
            return Err(RuntimeError::new("Handle not open for writing"));
        }
        state.bytes_written += bytes.len() as i64;
        match state.target {
            IoHandleTarget::Stdout => {
                // For stdout, convert bytes to lossy string and emit
                let content = String::from_utf8_lossy(bytes).to_string();
                self.emit_output(&content);
                Ok(())
            }
            IoHandleTarget::Stderr => {
                if self.subtest_depth == 0 && self.immediate_stdout {
                    use std::io::Write;
                    let _ = std::io::stderr().write_all(bytes);
                    let _ = std::io::stderr().flush();
                } else {
                    let content = String::from_utf8_lossy(bytes).to_string();
                    self.stderr_output.push_str(&content);
                }
                Ok(())
            }
            IoHandleTarget::File => {
                if let Some(file) = state.file.as_mut() {
                    file.write_all(bytes).map_err(|err| {
                        RuntimeError::new(format!("Failed to write to file: {}", err))
                    })?;
                    Ok(())
                } else {
                    Err(RuntimeError::new("IO::Handle is not attached to a file"))
                }
            }
            IoHandleTarget::Socket => {
                if let Some(sock) = state.socket.as_mut() {
                    sock.write_all(bytes).map_err(|err| {
                        RuntimeError::new(format!("Failed to write to socket: {}", err))
                    })?;
                    Ok(())
                } else {
                    Err(RuntimeError::new("Socket not connected"))
                }
            }
            IoHandleTarget::Stdin | IoHandleTarget::ArgFiles => {
                Err(RuntimeError::new("Cannot write to read-only handle"))
            }
        }
    }

    pub(super) fn close_handle_value(
        &mut self,
        handle_value: &Value,
    ) -> Result<bool, RuntimeError> {
        let state = self.handle_state_mut(handle_value)?;
        if state.closed {
            return Ok(false);
        }
        if matches!(state.target, IoHandleTarget::File) {
            Self::flush_file_handle_buffer(state)?;
            if let Some(file) = state.file.as_mut() {
                file.flush()
                    .map_err(|err| RuntimeError::new(format!("Failed to flush handle: {}", err)))?;
            }
        }
        state.closed = true;
        state.file = None;
        Ok(true)
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

    pub(super) fn read_line_from_handle_value(
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

        let state = self.handle_state_mut(handle_value)?;
        if state.closed {
            return Err(RuntimeError::io_closed("handle operation"));
        }
        state.read_attempted = true;
        let encoding = state.encoding.clone();
        let needs_decode =
            !encoding.is_empty() && encoding != "utf-8" && encoding != "utf8" && encoding != "bin";
        match state.target {
            IoHandleTarget::Stdout | IoHandleTarget::Stderr => {
                Err(RuntimeError::new("Handle not readable"))
            }
            IoHandleTarget::Stdin => {
                let seps = state.line_separators.clone();
                let chomp = state.line_chomp;
                let mut stdin = std::io::stdin().lock();
                Self::read_record_with_separators(&mut stdin, &seps, chomp)
            }
            IoHandleTarget::ArgFiles => {
                let seps = state.line_separators.clone();
                let chomp = state.line_chomp;
                if argfiles_list.is_empty() {
                    // No file args — read from stdin
                    let mut stdin = std::io::stdin().lock();
                    Self::read_record_with_separators(&mut stdin, &seps, chomp)
                } else {
                    // Read from files listed in @*ARGS sequentially
                    loop {
                        if state.argfiles_index >= argfiles_list.len() {
                            return Ok(None);
                        }
                        // Open the next file if we don't have a reader
                        if state.argfiles_reader.is_none() {
                            let path = &argfiles_list[state.argfiles_index];
                            let file = std::fs::File::open(path).map_err(|e| {
                                RuntimeError::new(format!("Failed to open file '{}': {}", path, e))
                            })?;
                            state.argfiles_reader = Some(std::io::BufReader::new(file));
                        }
                        let reader = state.argfiles_reader.as_mut().unwrap();
                        match Self::read_record_with_separators(reader, &seps, chomp)? {
                            Some(line) => return Ok(Some(line)),
                            None => {
                                // Current file exhausted, move to next
                                state.argfiles_reader = None;
                                state.argfiles_index += 1;
                            }
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
                        Some(bytes) => {
                            let decoded = self.decode_with_encoding(&bytes, &encoding)?;
                            Ok(Some(decoded))
                        }
                        None => Ok(None),
                    }
                } else {
                    Self::read_record_with_separators(file, &seps, chomp)
                }
            }
            IoHandleTarget::Socket => {
                let sock = state
                    .socket
                    .as_mut()
                    .ok_or_else(|| RuntimeError::new("Socket not connected"))?;
                Self::read_record_with_separators(sock, &state.line_separators, state.line_chomp)
            }
        }
    }

    pub(super) fn read_bytes_from_handle_value(
        &mut self,
        handle_value: &Value,
        count: usize,
    ) -> Result<Vec<u8>, RuntimeError> {
        let state = self.handle_state_mut(handle_value)?;
        if state.closed {
            return Err(RuntimeError::io_closed("handle operation"));
        }
        state.read_attempted = true;
        match state.target {
            IoHandleTarget::Stdout | IoHandleTarget::Stderr => {
                Err(RuntimeError::new("Handle not readable"))
            }
            IoHandleTarget::Stdin | IoHandleTarget::ArgFiles => Ok(Vec::new()),
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
        }
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

    pub(super) fn read_chars_from_handle_value(
        &mut self,
        handle_value: &Value,
        count: Option<usize>,
    ) -> Result<String, RuntimeError> {
        let state = self.handle_state_mut(handle_value)?;
        if state.closed {
            return Err(RuntimeError::io_closed("handle operation"));
        }
        state.read_attempted = true;
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
                if let Some(limit) = count {
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
    }

    pub(super) fn seek_handle_value(
        &mut self,
        handle_value: &Value,
        pos: i64,
        mode: i32,
    ) -> Result<i64, RuntimeError> {
        let state = self.handle_state_mut(handle_value)?;
        if state.closed {
            return Err(RuntimeError::io_closed("handle operation"));
        }
        state.read_attempted = true;
        let file = state
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

    pub(super) fn tell_handle_value(&mut self, handle_value: &Value) -> Result<i64, RuntimeError> {
        let state = self.handle_state_mut(handle_value)?;
        if state.closed {
            return Err(RuntimeError::io_closed("handle operation"));
        }
        match state.target {
            IoHandleTarget::File => {
                let file = state
                    .file
                    .as_mut()
                    .ok_or_else(|| RuntimeError::new("IO::Handle is not attached to a file"))?;
                let pos = file.stream_position().map_err(|err| {
                    RuntimeError::new(format!("Failed to query position: {}", err))
                })?;
                Ok(pos as i64)
            }
            _ => Ok(state.bytes_written),
        }
    }

    pub(super) fn handle_eof_value(&mut self, handle_value: &Value) -> Result<bool, RuntimeError> {
        let state = self.handle_state_mut(handle_value)?;
        if state.closed {
            return Err(RuntimeError::io_closed("handle operation"));
        }
        match state.target {
            IoHandleTarget::File => {
                let file = state
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
                // Raku semantics: a freshly opened file where pos == 0 and
                // the reported size is 0 returns False for .eof until a read
                // or seek has been performed. This handles both truly empty
                // files and /proc virtual files that report size 0.
                if pos == 0 && end == 0 && !state.read_attempted {
                    return Ok(false);
                }
                Ok(pos >= end)
            }
            IoHandleTarget::Stdin | IoHandleTarget::ArgFiles => Ok(false),
            _ => Err(RuntimeError::new("Handle not readable")),
        }
    }

    pub(super) fn set_handle_encoding(
        &mut self,
        handle_value: &Value,
        encoding: Option<String>,
    ) -> Result<String, RuntimeError> {
        let state = self.handle_state_mut(handle_value)?;
        if let Some(enc) = encoding {
            let prev = state.encoding.clone();
            state.encoding = enc;
            Ok(prev)
        } else {
            Ok(state.encoding.clone())
        }
    }

    pub(super) fn metadata_is_executable(metadata: &fs::Metadata) -> bool {
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            metadata.permissions().mode() & 0o111 != 0
        }
        #[cfg(not(unix))]
        {
            metadata.is_file()
        }
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
    ) {
        let mut read = false;
        let mut write = false;
        let mut append = false;
        let mut bin = false;
        let mut chomp = true;
        let mut create = false;
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
                    "rw" => {
                        read = truthy;
                        write = truthy;
                    }
                    "a" => append = truthy,
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
        } else if write && read {
            options.create(true);
        }
        // Explicit :create flag ensures the file is created if it doesn't exist
        if create {
            options.create(true);
        }
        let file = options.open(path).map_err(|err| {
            RuntimeError::new(format!("Failed to open '{}': {}", path.display(), err))
        })?;
        let id = self.next_handle_id;
        self.next_handle_id += 1;
        let mode = if read && (write || append) {
            IoHandleMode::ReadWrite
        } else if append {
            IoHandleMode::Append
        } else if write {
            IoHandleMode::Write
        } else {
            IoHandleMode::Read
        };
        let state = IoHandleState {
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
            argfiles_index: 0,
            argfiles_reader: None,
        };
        self.handles.insert(id, state);
        Ok(self.make_handle_instance_with_bin(id, bin))
    }
}
