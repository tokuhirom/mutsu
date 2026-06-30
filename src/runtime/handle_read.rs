//! Interpreter-side handle reading: records, lines, words, and raw bytes.
use super::*;

impl Interpreter {
    pub(crate) fn read_record_bytes<R: Read>(
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
            // Prefer the LONGEST matching separator so `"\r\n"` is treated as one
            // line ending rather than splitting at the trailing `"\n"` and leaving
            // a stray `"\r"` (when `nl-in` carries both `"\n"` and `"\r\n"`).
            if let Some(matched_len) = separators
                .iter()
                .filter(|sep| buffer.ends_with(sep.as_slice()))
                .map(|sep| sep.len())
                .max()
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

    pub(crate) fn read_record_with_separators<R: Read>(
        reader: &mut R,
        separators: &[Vec<u8>],
        chomp: bool,
    ) -> Result<Option<String>, RuntimeError> {
        match Self::read_record_bytes(reader, separators, chomp)? {
            Some(buffer) => {
                let s = String::from_utf8_lossy(&buffer).to_string();
                // Raku text-mode reads normalize the CR-LF grapheme to a single
                // "\n" (universal newline / NFG), even when CR-LF is not itself
                // the line separator (e.g. reading "6\r\n" with `nl-in => "♥"`
                // yields "6\n").
                Ok(Some(if s.contains("\r\n") {
                    s.replace("\r\n", "\n")
                } else {
                    s
                }))
            }
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
}
