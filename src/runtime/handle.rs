use super::*;

/// Pure per-handle operations that touch only the handle's own state — no
/// `Interpreter` state (no `emit_output`, env, or encoding helpers). These are
/// the single authoritative implementation shared by the interpreter's
/// `*_handle_value` wrappers and the VM-native IO dispatch
/// (the `IO::Handle` rows), so the §1 native-IO fork for these methods
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
