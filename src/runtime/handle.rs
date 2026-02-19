use super::*;

impl Interpreter {
    pub(super) fn handle_id_from_value(value: &Value) -> Option<usize> {
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = value
            && class_name == "IO::Handle"
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
        let id = Self::handle_id_from_value(handle_value)
            .ok_or_else(|| RuntimeError::new("Expected IO::Handle"))?;
        let state = self
            .handles
            .get_mut(&id)
            .ok_or_else(|| RuntimeError::new("Invalid IO::Handle"))?;
        if state.closed {
            return Err(RuntimeError::new("X::IO::Closed: IO::Handle is closed"));
        }
        let mut payload = String::from(content);
        if newline {
            payload.push('\n');
        }
        match state.target {
            IoHandleTarget::Stdout => {
                self.output.push_str(&payload);
                Ok(())
            }
            IoHandleTarget::Stderr => {
                self.stderr_output.push_str(&payload);
                self.output.push_str(&payload);
                Ok(())
            }
            IoHandleTarget::File => {
                if matches!(state.mode, IoHandleMode::Read) {
                    return Err(RuntimeError::new("Handle not open for writing"));
                }
                if let Some(file) = state.file.as_mut() {
                    file.write_all(payload.as_bytes()).map_err(|err| {
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

    pub(super) fn close_handle_value(
        &mut self,
        handle_value: &Value,
    ) -> Result<bool, RuntimeError> {
        let state = self.handle_state_mut(handle_value)?;
        if state.closed {
            return Ok(false);
        }
        state.closed = true;
        state.file = None;
        Ok(true)
    }

    pub(super) fn read_line_from_handle_value(
        &mut self,
        handle_value: &Value,
    ) -> Result<String, RuntimeError> {
        let state = self.handle_state_mut(handle_value)?;
        if state.closed {
            return Err(RuntimeError::new("X::IO::Closed: IO::Handle is closed"));
        }
        match state.target {
            IoHandleTarget::Stdout | IoHandleTarget::Stderr => {
                Err(RuntimeError::new("Handle not readable"))
            }
            IoHandleTarget::Stdin | IoHandleTarget::ArgFiles => Ok(String::new()),
            IoHandleTarget::File => {
                let file = state
                    .file
                    .as_mut()
                    .ok_or_else(|| RuntimeError::new("IO::Handle is not attached to a file"))?;
                let mut buffer = Vec::new();
                let mut byte = [0u8];
                loop {
                    let n = file
                        .read(&mut byte)
                        .map_err(|err| RuntimeError::new(format!("Failed to read: {}", err)))?;
                    if n == 0 {
                        break;
                    }
                    buffer.push(byte[0]);
                    if byte[0] == b'\n' {
                        break;
                    }
                }
                if buffer.is_empty() {
                    return Ok(String::new());
                }
                Ok(String::from_utf8_lossy(&buffer).to_string())
            }
            IoHandleTarget::Socket => {
                let sock = state
                    .socket
                    .as_mut()
                    .ok_or_else(|| RuntimeError::new("Socket not connected"))?;
                let mut buffer = Vec::new();
                let mut byte = [0u8];
                loop {
                    let n = sock
                        .read(&mut byte)
                        .map_err(|err| RuntimeError::new(format!("Failed to read: {}", err)))?;
                    if n == 0 {
                        break;
                    }
                    buffer.push(byte[0]);
                    if byte[0] == b'\n' {
                        break;
                    }
                }
                if buffer.is_empty() {
                    return Ok(String::new());
                }
                Ok(String::from_utf8_lossy(&buffer).to_string())
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
            return Err(RuntimeError::new("X::IO::Closed: IO::Handle is closed"));
        }
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

    pub(super) fn seek_handle_value(
        &mut self,
        handle_value: &Value,
        pos: i64,
    ) -> Result<i64, RuntimeError> {
        let state = self.handle_state_mut(handle_value)?;
        if state.closed {
            return Err(RuntimeError::new("X::IO::Closed: IO::Handle is closed"));
        }
        let file = state
            .file
            .as_mut()
            .ok_or_else(|| RuntimeError::new("IO::Handle is not attached to a file"))?;
        let new_pos = file
            .seek(SeekFrom::Start(pos.max(0) as u64))
            .map_err(|err| RuntimeError::new(format!("Failed to seek: {}", err)))?;
        Ok(new_pos as i64)
    }

    pub(super) fn tell_handle_value(&mut self, handle_value: &Value) -> Result<i64, RuntimeError> {
        let state = self.handle_state_mut(handle_value)?;
        if state.closed {
            return Err(RuntimeError::new("X::IO::Closed: IO::Handle is closed"));
        }
        let file = state
            .file
            .as_mut()
            .ok_or_else(|| RuntimeError::new("IO::Handle is not attached to a file"))?;
        let pos = file
            .stream_position()
            .map_err(|err| RuntimeError::new(format!("Failed to query position: {}", err)))?;
        Ok(pos as i64)
    }

    pub(super) fn handle_eof_value(&mut self, handle_value: &Value) -> Result<bool, RuntimeError> {
        let state = self.handle_state_mut(handle_value)?;
        if state.closed {
            return Err(RuntimeError::new("X::IO::Closed: IO::Handle is closed"));
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

    pub(super) fn parse_io_flags_values(args: &[Value]) -> (bool, bool, bool, bool) {
        let mut read = false;
        let mut write = false;
        let mut append = false;
        let mut bin = false;
        for arg in args {
            if let Value::Pair(name, value) = arg {
                let truthy = value.truthy();
                match name.as_str() {
                    "r" => read = truthy,
                    "w" => write = truthy,
                    "a" => append = truthy,
                    "bin" => bin = truthy,
                    _ => {}
                }
            }
        }
        if !read && !write && !append {
            read = true;
        }
        (read, write, append, bin)
    }

    pub(super) fn open_file_handle(
        &mut self,
        path: &Path,
        read: bool,
        write: bool,
        append: bool,
        bin: bool,
    ) -> Result<Value, RuntimeError> {
        let mut options = fs::OpenOptions::new();
        options.read(read);
        options.write(write || append);
        if append {
            options.append(true).create(true);
        } else if write {
            options.create(true).truncate(true);
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
            encoding: "utf-8".to_string(),
            file: Some(file),
            socket: None,
            closed: false,
            bin,
        };
        self.handles.insert(id, state);
        Ok(self.make_handle_instance_with_bin(id, bin))
    }
}
