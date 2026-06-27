use super::*;

impl Interpreter {
    pub fn output(&self) -> String {
        self.output_sink().output.clone()
    }

    /// Clear the output buffer and reset the output-emitted flag.
    pub fn clear_output(&mut self) {
        let mut sink = self.output_sink_mut();
        sink.output.clear();
        sink.output_emitted = false;
    }

    /// Take the output buffer, leaving it empty.
    pub(crate) fn take_output(&mut self) -> String {
        std::mem::take(&mut self.output_sink_mut().output)
    }

    /// Take the stderr buffer, leaving it empty.
    pub(crate) fn take_stderr_output(&mut self) -> String {
        std::mem::take(&mut self.output_sink_mut().stderr_output)
    }

    /// Returns true if any output was emitted since the last `clear_output`.
    pub fn has_output_emitted(&self) -> bool {
        self.output_sink().output_emitted
    }

    /// Write to the output buffer and also flush to real stdout
    /// when not inside a subtest.
    pub(crate) fn emit_output(&mut self, text: &str) {
        let byte_count = text.len() as i64;
        if let Some(stdout_handle) = self
            .io_handles_mut()
            .map
            .values_mut()
            .find(|h| matches!(h.target, IoHandleTarget::Stdout))
        {
            stdout_handle.bytes_written += byte_count;
        }
        // The Stdout `bytes_written` accounting above touches `io_handles`; the
        // write decision + buffers live in `output_sink`.
        let subtest_active = self.tap.subtest_depth() != 0;
        self.output_sink_mut().emit(text, subtest_active);
    }

    /// Enable immediate flushing of output to stdout.
    pub fn set_immediate_stdout(&mut self, val: bool) {
        self.output_sink_mut().immediate_stdout = val;
    }

    pub fn flush_stderr_buffer(&mut self) {
        let stderr = std::mem::take(&mut self.output_sink_mut().stderr_output);
        if !stderr.is_empty() {
            eprint!("{}", stderr);
            let _ = std::io::stderr().flush();
        }
    }

    /// Enable or disable module precompilation cache.
    pub fn set_precomp_enabled(&mut self, val: bool) {
        self.precomp_enabled = val;
    }

    /// Check if MONKEY-TYPING pragma is active.
    pub(crate) fn monkey_typing_enabled(&self) -> bool {
        self.monkey_typing
    }

    pub fn exit_code(&self) -> i64 {
        self.exit_code
    }

    /// Return the value of `%*ENV<RAKU_EXCEPTIONS_HANDLER>`, if set.
    /// This selects the format used to print uncaught exceptions (e.g. "JSON").
    pub fn exceptions_handler(&self) -> Option<String> {
        let env_hash = self.env.get("%*ENV")?;
        if let Value::Hash(map) = env_hash
            && let Some(v) = map.get("RAKU_EXCEPTIONS_HANDLER")
        {
            let s = v.to_string_value();
            if !s.is_empty() {
                return Some(s);
            }
        }
        None
    }

    pub(crate) fn is_halted(&self) -> bool {
        self.halted
    }

    pub(crate) fn is_thread_clone(&self) -> bool {
        self.output_sink().is_thread_clone
    }

    /// Write a message to stderr, respecting nested mode.
    /// In nested mode the output is buffered for later inspection;
    /// otherwise it is emitted directly so `flush_stderr_buffer` does
    /// not duplicate it.
    pub(crate) fn emit_stderr(&mut self, text: &str) {
        if self.nested_mode {
            self.output_sink_mut().stderr_output.push_str(text);
        } else {
            eprint!("{}", text);
        }
    }

    pub(crate) fn write_warn_to_stderr(&mut self, message: &str) {
        let msg = format!("{}\n", message);
        // Read the thread-clone shared stderr Arc out under a scoped guard so it
        // is dropped before `self.warn_output` / `emit` re-borrow self.
        let thread_shared_stderr = {
            let sink = self.output_sink();
            if sink.is_thread_clone {
                sink.shared_thread_stderr.clone()
            } else {
                None
            }
        };
        if let Some(shared) = thread_shared_stderr {
            shared.lock().unwrap().push_str(&msg);
            self.warn_output.push_str(&msg);
            return;
        }
        self.warn_output.push_str(&msg);
        // In nested mode (e.g. in-process `is_run`), buffer to
        // `stderr_output` so the caller can inspect captured stderr.
        // Otherwise emit directly to the real stderr; if we also pushed
        // into `stderr_output`, the final flush would duplicate it.
        if self.nested_mode {
            self.output_sink_mut().stderr_output.push_str(&msg);
        } else {
            eprint!("{}", msg);
        }
    }

    pub(crate) fn push_warn_suppression(&mut self) {
        self.warn_suppression_depth += 1;
    }

    pub(crate) fn pop_warn_suppression(&mut self) {
        self.warn_suppression_depth = self.warn_suppression_depth.saturating_sub(1);
    }

    pub(crate) fn warning_suppressed(&self) -> bool {
        self.warn_suppression_depth > 0
    }

    pub fn flush_all_handles(&mut self) {
        for state in self.io_handles_mut().map.values_mut() {
            if state.closed {
                continue;
            }
            if !state.out_buffer_pending.is_empty()
                && let Some(file) = state.file.as_mut()
            {
                let _ = file.write_all(&state.out_buffer_pending);
                let _ = file.flush();
                state.out_buffer_pending.clear();
            }
        }
    }
}
