use super::*;

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
        let payload = self.prepare_text_payload(content, newline, trying)?;
        self.write_file_payload(payload.as_bytes())
    }

    /// Shared phase-1 of a text write (`write_to_handle_value_trying`): error on
    /// a closed handle, append `nl_out` when `newline`, account the bytes on this
    /// (receiver) handle, and return the payload. Target-agnostic — File then
    /// writes it via `write_file_payload`, Stdout/Stderr emit it (③後段 PR-C).
    pub(crate) fn prepare_text_payload(
        &mut self,
        content: &str,
        newline: bool,
        trying: &str,
    ) -> Result<String, RuntimeError> {
        if self.closed {
            return Err(RuntimeError::io_closed(trying));
        }
        let mut payload = String::from(content);
        if newline {
            payload.push_str(&self.nl_out);
        }
        self.bytes_written += payload.len() as i64;
        Ok(payload)
    }

    /// Whether this handle targets a regular file (the gate for the VM-native
    /// byte-write methods `write`/`spurt`, which write raw bytes straight to the
    /// file independent of `:out-buffer` and encoding — ③ native IO PR-D
    /// Tier-2c). Stdout/Stderr/Socket return `false` and fall through.
    pub(crate) fn is_file_target(&self) -> bool {
        matches!(self.target, IoHandleTarget::File)
    }

    /// VM-native `.get` line read on a `File`+UTF8 handle (PR-D read side): read
    /// the next record using the handle's `nl-in` separators and `chomp`, decoded
    /// UTF-8-lossy. The same record reader the interpreter's File branch uses, so
    /// it is the single authoritative impl. Caller guarantees a File target with
    /// a UTF-8/binary encoding (`can_native_text_write`); ArgFiles / Stdin /
    /// non-UTF8 (which need `@*ARGS` / `decode_with_encoding`) fall through.
    pub(crate) fn read_line_native(&mut self) -> Result<Option<String>, RuntimeError> {
        if self.closed {
            return Err(RuntimeError::io_closed("handle operation"));
        }
        self.read_attempted = true;
        let seps = self.line_separators.clone();
        let chomp = self.line_chomp;
        let file = self
            .file
            .as_mut()
            .ok_or_else(|| RuntimeError::new("IO::Handle is not attached to a file"))?;
        Interpreter::read_record_with_separators(file, &seps, chomp)
    }

    /// Whether `.slurp` can resolve to a `Str` natively (PR-D2): a `File` target
    /// in text mode with a UTF-8 encoding (no `:bin`, no binary handle). A `:bin`
    /// / binary-mode / non-UTF-8 slurp (which needs a `Buf` or
    /// `decode_with_encoding`) falls through.
    pub(crate) fn can_native_slurp_string(&self, has_bin_arg: bool) -> bool {
        matches!(self.target, IoHandleTarget::File)
            && !has_bin_arg
            && !self.bin
            && (self.encoding.is_empty() || self.encoding == "utf-8" || self.encoding == "utf8")
    }

    /// VM-native `.slurp` (Str) on a `File`+UTF8 handle: read everything from the
    /// current position to EOF, UTF-8-lossy. Caller guarantees
    /// [`can_native_slurp_string`].
    pub(crate) fn slurp_string_native(&mut self) -> Result<String, RuntimeError> {
        if self.closed {
            return Err(RuntimeError::io_closed("handle operation"));
        }
        self.read_attempted = true;
        let file = self
            .file
            .as_mut()
            .ok_or_else(|| RuntimeError::new("IO::Handle is not attached to a file"))?;
        let mut bytes = Vec::new();
        file.read_to_end(&mut bytes)
            .map_err(|err| RuntimeError::new(format!("Failed to read: {}", err)))?;
        Ok(String::from_utf8_lossy(&bytes).to_string())
    }

    /// VM-native `.read` on a `File` handle (PR-D2): `count > 0` reads up to
    /// `count` bytes in one `read` (a short read is fine); `count == 0` reads to
    /// EOF. Encoding-independent (returns raw bytes). Mirrors
    /// `read_bytes_from_handle_value`'s File branch. Caller guarantees
    /// [`is_file_target`].
    pub(crate) fn read_bytes_native(&mut self, count: usize) -> Result<Vec<u8>, RuntimeError> {
        if self.closed {
            return Err(RuntimeError::io_closed("handle operation"));
        }
        self.read_attempted = true;
        let file = self
            .file
            .as_mut()
            .ok_or_else(|| RuntimeError::new("IO::Handle is not attached to a file"))?;
        if count > 0 {
            let mut buffer = vec![0u8; count];
            let n = file
                .read(&mut buffer)
                .map_err(|err| RuntimeError::new(format!("Failed to read: {}", err)))?;
            buffer.truncate(n);
            Ok(buffer)
        } else {
            let mut bytes = Vec::new();
            file.read_to_end(&mut bytes)
                .map_err(|err| RuntimeError::new(format!("Failed to read: {}", err)))?;
            Ok(bytes)
        }
    }

    /// VM-native character read on a `File`+UTF8 handle (`getc`/`readchars`,
    /// PR-D3): `count = Some(n)` reads up to n UTF-8 characters, `None` reads the
    /// rest of the file UTF-8-lossy. Mirrors `read_chars_from_handle_value`'s
    /// File branch for the UTF-8/binary case. Caller guarantees
    /// [`can_native_text_write`] (which excludes utf16 — that path needs the
    /// interpreter's BOM/endianness handling).
    pub(crate) fn read_chars_native(
        &mut self,
        count: Option<usize>,
    ) -> Result<String, RuntimeError> {
        if self.closed {
            return Err(RuntimeError::io_closed("handle operation"));
        }
        self.read_attempted = true;
        let file = self
            .file
            .as_mut()
            .ok_or_else(|| RuntimeError::new("IO::Handle is not attached to a file"))?;
        match count {
            Some(limit) => {
                if limit == 0 {
                    return Ok(String::new());
                }
                let mut out = String::new();
                for _ in 0..limit {
                    let Some(ch) = Interpreter::read_utf8_char(file)? else {
                        break;
                    };
                    out.push_str(&ch);
                }
                Ok(out)
            }
            None => {
                let mut bytes = Vec::new();
                file.read_to_end(&mut bytes)
                    .map_err(|err| RuntimeError::new(format!("Failed to read: {}", err)))?;
                Ok(String::from_utf8_lossy(&bytes).to_string())
            }
        }
    }

    /// Add to this handle's `bytes_written` counter. Used by the VM's Stdout
    /// emit to mirror `emit_output`'s Stdout-handle accounting (③後段 PR-C).
    pub(crate) fn add_bytes_written(&mut self, n: i64) {
        self.bytes_written += n;
    }

    /// Whether this handle targets Stdout (the VM emits via the shared output
    /// sink — ③後段 PR-C).
    pub(crate) fn is_stdout_target(&self) -> bool {
        matches!(self.target, IoHandleTarget::Stdout)
    }

    /// Whether this handle targets Stderr.
    pub(crate) fn is_stderr_target(&self) -> bool {
        matches!(self.target, IoHandleTarget::Stderr)
    }

    /// Raw `file.write_all` to a File handle — the shared leaf used by the
    /// interpreter's `write_bytes_to_handle_value` File branch and the VM-native
    /// `write`/`spurt` byte-write. Deliberately bypasses the `:out-buffer`
    /// (matching the existing `write_bytes_to_handle_value` semantics).
    pub(crate) fn write_all_to_file(&mut self, bytes: &[u8]) -> Result<(), RuntimeError> {
        if let Some(file) = self.file.as_mut() {
            file.write_all(bytes)
                .map_err(|err| RuntimeError::new(format!("Failed to write to file: {}", err)))?;
            Ok(())
        } else {
            Err(RuntimeError::new("IO::Handle is not attached to a file"))
        }
    }

    /// VM-native raw byte write to a File handle (`write`/`spurt`): closed-check,
    /// not-open-for-writing check, byte accounting, then the raw file write.
    /// Mirrors `write_bytes_to_handle_value`'s phase-1 validation + File branch
    /// for a File target. Caller guarantees [`is_file_target`].
    pub(crate) fn native_write_bytes_file(&mut self, bytes: &[u8]) -> Result<(), RuntimeError> {
        if self.closed {
            return Err(RuntimeError::io_closed("write"));
        }
        if matches!(self.mode, IoHandleMode::Read) {
            return Err(RuntimeError::new("Handle not open for writing"));
        }
        self.bytes_written += bytes.len() as i64;
        self.write_all_to_file(bytes)
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
}
