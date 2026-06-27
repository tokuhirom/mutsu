//! Handle positioning (seek/tell/eof), per-handle settings (chomp, nl-out,
//! out-buffer, encoding, native descriptor), open-flag parsing, and file open.
use super::*;

impl IoHandleState {
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
