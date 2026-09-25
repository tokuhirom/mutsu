//! Interpreter-side handle reading: records, lines, words, and raw bytes.
use super::*;
use crate::value::ValueView;

/// A handle's line separators and chomp setting.
type LineSettings = (Vec<Vec<u8>>, bool);

impl Interpreter {
    pub(crate) fn read_record_bytes<R: Read + ?Sized>(
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

    pub(crate) fn read_record_with_separators<R: Read + ?Sized>(
        reader: &mut R,
        separators: &[Vec<u8>],
        chomp: bool,
    ) -> Result<Option<String>, RuntimeError> {
        match Self::read_record_bytes(reader, separators, chomp)? {
            Some(buffer) => Self::utf8_record_text(buffer).map(Some),
            None => Ok(None),
        }
    }

    /// Read the rest of a Seq's private UTF-8 handle (the one `IO::Path.lines`
    /// / `.words` opens, #9257) under a single lock and close it -- the whole
    /// read `force_lazy_io_lines` would otherwise do one lock, id lookup and
    /// `@*ARGS` check per line. `Ok(None)` for any other handle, which the
    /// caller then reads record by record.
    // Cost: O(b), b = bytes left in the file.
    pub(crate) fn drain_seq_private_handle(
        &mut self,
        handle_value: &Value,
        words: bool,
    ) -> Result<Option<Vec<Value>>, RuntimeError> {
        let drained = self.with_handle_mut_opt(handle_value, |state| {
            let utf8 = matches!(state.encoding.as_str(), "utf-8" | "utf8");
            if state.closed || !utf8 || state.line_separators.iter().any(|sep| sep.is_empty()) {
                return Ok(None);
            }
            let Some(reader) = state.seq_reader.as_mut() else {
                return Ok(None);
            };
            state.read_attempted = true;
            let mut items: Vec<Value> = state.pending_words.drain(..).map(Value::str).collect();
            let read_err =
                |err: std::io::Error| RuntimeError::new(format!("Failed to read: {}", err));
            if words {
                // Words do not depend on where the lines end, so the rest of
                // the file is decoded once and split, not line by line.
                let mut rest = Vec::new();
                reader.read_to_end(&mut rest).map_err(read_err)?;
                let text = Self::utf8_record_text(rest)?;
                items.extend(text.split_whitespace().map(|w| Value::str(w.to_string())));
            } else {
                while let Some(bytes) = reader
                    .read_record(&state.line_separators, state.line_chomp)
                    .map_err(read_err)?
                {
                    items.push(Value::str(Self::utf8_record_text(bytes)?));
                }
            }
            // The read reached EOF: close on exhaust, as the record-by-record
            // read would.
            state.close()?;
            Ok(Some(items))
        })?;
        Ok(drained.flatten())
    }

    /// A record read off a UTF-8 text handle, as text.
    // Cost: O(n), n = bytes of the record.
    fn utf8_record_text(buffer: Vec<u8>) -> Result<String, RuntimeError> {
        let s = crate::builtins::decode_utf8_handle_text_owned(buffer)?;
        // Raku text-mode reads normalize the CR-LF grapheme to a single
        // "\n" (universal newline / NFG), even when CR-LF is not itself
        // the line separator (e.g. reading "6\r\n" with `nl-in => "♥"`
        // yields "6\n").
        Ok(if s.contains("\r\n") {
            s.replace("\r\n", "\n")
        } else {
            s
        })
    }

    /// `@*ARGS` as strings, and `$*IN`'s line separators and chomp setting:
    /// what an `ArgFiles` handle's line read falls back to.
    fn argfiles_read_context(&self) -> (Vec<String>, Option<LineSettings>) {
        let argfiles_list: Vec<String> = self
            .env
            .get("@*ARGS")
            .and_then(|v| {
                if let ValueView::Array(items, ..) = v.view() {
                    Some(items.iter().map(|v| v.to_string_value()).collect())
                } else {
                    None
                }
            })
            .unwrap_or_default();
        let stdin_seps = self.get_dynamic_handle("$*IN").and_then(|in_handle| {
            let id = Self::handle_id_from_value(&in_handle)?;
            let table = self.io_handles();
            let in_state = table.map.get(&id)?;
            Some((in_state.line_separators.clone(), in_state.line_chomp))
        });
        (argfiles_list, stdin_seps)
    }

    pub(crate) fn read_line_from_handle_value(
        &mut self,
        handle_value: &Value,
    ) -> Result<Option<String>, RuntimeError> {
        // Pre-extract @*ARGS and $*IN's line separators before borrowing the
        // handle state -- only an `ArgFiles` handle reads them, and a per-line
        // env lookup is most of the cost of reading a short line elsewhere.
        let is_argfiles = Self::handle_id_from_value(handle_value).is_some_and(|id| {
            self.io_handles()
                .map
                .get(&id)
                .is_some_and(|state| matches!(state.target, IoHandleTarget::ArgFiles))
        });
        let (argfiles_list, stdin_seps) = if is_argfiles {
            self.argfiles_read_context()
        } else {
            (Vec::new(), None)
        };

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
            let needs_decode = !state.encoding.is_empty()
                && state.encoding != "utf-8"
                && state.encoding != "utf8"
                && state.encoding != "bin";
            let encoding = if needs_decode {
                state.encoding.clone()
            } else {
                String::new()
            };
            match state.target {
                IoHandleTarget::Stdout | IoHandleTarget::Stderr => {
                    Err(RuntimeError::new("Handle not readable"))
                }
                IoHandleTarget::Stdin => {
                    let seps = state.line_separators.clone();
                    let chomp = state.line_chomp;
                    let line = {
                        let mut stdin = std::io::stdin().lock();
                        Self::read_record_with_separators(&mut stdin, &seps, chomp)?
                    };
                    if line.is_none() {
                        state.stream_hit_eof = true;
                    }
                    Ok(LineOutcome::Done(line))
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
                        let (effective_seps, effective_chomp) = stdin_seps.unwrap_or((seps, chomp));
                        let line = {
                            let mut stdin = std::io::stdin().lock();
                            Self::read_record_with_separators(
                                &mut stdin,
                                &effective_seps,
                                effective_chomp,
                            )?
                        };
                        if line.is_none() {
                            state.stream_hit_eof = true;
                        }
                        return Ok(LineOutcome::Done(line));
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
                                let line = {
                                    let mut stdin = std::io::stdin().lock();
                                    Self::read_record_with_separators(&mut stdin, &seps, chomp)?
                                };
                                match line {
                                    Some(line) => return Ok(LineOutcome::Done(Some(line))),
                                    None => {
                                        state.stream_hit_eof = true;
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
                    // The private handle of `IO::Path.lines` / `.words` reads
                    // through its buffer; every other file handle reads the
                    // file directly.
                    if let Some(reader) = state.seq_reader.as_mut()
                        && state.line_separators.iter().all(|sep| !sep.is_empty())
                    {
                        let record = reader
                            .read_record(&state.line_separators, state.line_chomp)
                            .map_err(|err| RuntimeError::new(format!("Failed to read: {}", err)))?;
                        return match record {
                            None => Ok(LineOutcome::Done(None)),
                            Some(bytes) if needs_decode => {
                                Ok(LineOutcome::NeedsDecode(bytes, encoding))
                            }
                            Some(bytes) => {
                                Self::utf8_record_text(bytes).map(|s| LineOutcome::Done(Some(s)))
                            }
                        };
                    }
                    let seps = &state.line_separators;
                    let chomp = state.line_chomp;
                    let file: &mut dyn Read = match state.seq_reader.as_mut() {
                        Some(reader) => reader,
                        None => state.file.as_mut().ok_or_else(|| {
                            RuntimeError::new("IO::Handle is not attached to a file")
                        })?,
                    };
                    if needs_decode {
                        match Self::read_record_bytes(file, seps, chomp)? {
                            Some(bytes) => Ok(LineOutcome::NeedsDecode(bytes, encoding)),
                            None => Ok(LineOutcome::Done(None)),
                        }
                    } else {
                        Self::read_record_with_separators(file, seps, chomp).map(LineOutcome::Done)
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
        let line = match outcome {
            LineOutcome::Done(line) => line,
            LineOutcome::NeedsDecode(bytes, encoding) => {
                Some(self.decode_with_encoding(&bytes, &encoding)?)
            }
        };
        if line.is_none() {
            // Close-on-exhaust (`words($fh, :close)`, and the handle
            // `IO::Path.lines` / `.words` open): only a read that actually
            // reached EOF closes, so a partial consumer leaves it open.
            let should_close = self.with_handle_mut(handle_value, |state| {
                Ok(state.close_on_exhaust && !state.closed)
            })?;
            if should_close {
                let _ = self.close_handle_value(handle_value);
            }
        }
        Ok(line)
    }

    /// Read the next whitespace-delimited word from a handle, buffering the
    /// leftover words of each line in `pending_words`. Returns `None` at EOF,
    /// auto-closing the handle if `close_on_exhaust` was requested
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
                // `read_line_from_handle_value` already closed the handle if
                // it was asked to close on exhaust.
                None => return Ok(None),
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
                        if let ValueView::Array(items, ..) = v.view() {
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
                let mut buffer = vec![0u8; count];
                let bytes_read = {
                    let mut stdin = std::io::stdin().lock();
                    stdin.read(&mut buffer).map_err(|err| {
                        RuntimeError::new(format!("Failed to read from stdin: {}", err))
                    })?
                };
                buffer.truncate(bytes_read);
                if bytes_read == 0 && count > 0 {
                    self.with_handle_mut(handle_value, |state| {
                        state.stream_hit_eof = true;
                        Ok(())
                    })?;
                }
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
                let mut buffer = vec![0u8; count];
                let bytes_read = {
                    let mut stdin = std::io::stdin().lock();
                    stdin.read(&mut buffer).map_err(|err| {
                        RuntimeError::new(format!("Failed to read from stdin: {}", err))
                    })?
                };
                buffer.truncate(bytes_read);
                if bytes_read == 0 && count > 0 {
                    state.stream_hit_eof = true;
                }
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
