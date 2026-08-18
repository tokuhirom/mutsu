use super::*;
use crate::value::ValueView;

/// Normalize a parse-warning origin file for use as a dedup key.
///
/// The parser's own module resolver (used by the export scan) and the
/// runtime's module resolver (used by the actual `use` load) are two
/// independent implementations; they agree on which *file* a module is but
/// are not guaranteed to render its path identically (relative vs.
/// canonical, `./foo` vs. `foo`, ...). Canonicalizing before comparing makes
/// the dedup robust to that instead of relying on the two resolvers
/// happening to produce byte-identical strings. Falls back to the raw string
/// when canonicalization fails (e.g. a synthetic tag like `<test>`, or a
/// path that no longer exists).
fn canonicalize_warning_file(file: Option<String>) -> Option<String> {
    file.map(|f| {
        std::fs::canonicalize(&f)
            .map(|p| p.to_string_lossy().into_owned())
            .unwrap_or(f)
    })
}

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

    /// Drain the shared stdout/stderr buffers that thread clones (`start`
    /// blocks, Promise callbacks) write into, emitting any pending text to this
    /// interpreter's own sinks. Called on `await` (join) and at program exit so
    /// fire-and-forget thread output is not lost. The Arc is cloned out so the
    /// `output_sink` guard is dropped before `emit_output` re-borrows `self`.
    pub(crate) fn drain_shared_thread_output(&mut self) {
        let shared_out = self.output_sink().shared_thread_output.clone();
        if let Some(shared) = shared_out {
            let drained = std::mem::take(&mut *shared.lock().unwrap());
            if !drained.is_empty() {
                self.emit_output(&drained);
            }
        }
        let shared_err = self.output_sink().shared_thread_stderr.clone();
        if let Some(shared) = shared_err {
            let drained = std::mem::take(&mut *shared.lock().unwrap());
            if !drained.is_empty() {
                self.output_sink_mut().stderr_output.push_str(&drained);
            }
        }
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
        if let ValueView::Hash(map) = env_hash.view()
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

    /// Everything `warn` has emitted on this interpreter so far, whatever sink
    /// it went to. Lets a test assert on warnings without capturing stderr.
    #[cfg(test)]
    pub(crate) fn warnings_emitted(&self) -> &str {
        &self.warn_output
    }

    /// Emit a batch of parse warnings (module export scan, module load, EVAL,
    /// `require`, precompilation-cache replay, ...), skipping any `(file,
    /// message)` pair already surfaced during the current top-level `run()`.
    ///
    /// mutsu's module system parses the same source more than once for a
    /// single `use` (an export scan at the importer's parse time, then the
    /// real load once the `use` executes; a precompilation-cache hit adds a
    /// third replayed copy) — draining `PARSE_WARNINGS` naively at each of
    /// those sites would print the same warning once per parse. The file tag
    /// (see `parser::add_parse_warning`) keeps this from conflating two
    /// *different* files that happen to produce identical warning text.
    /// `self.surfaced_parse_warnings` is reset at the top of `run()`, so a
    /// later, separate top-level program sharing this `Interpreter` (a new
    /// REPL line, for instance) still sees its own warnings independently.
    /// See `todo/tickets/module-parse-warning-reported-twice.md`.
    pub(crate) fn emit_parse_warnings<I>(&mut self, warnings: I)
    where
        I: IntoIterator<Item = (Option<String>, String)>,
    {
        for (file, message) in warnings {
            let key = (canonicalize_warning_file(file), message);
            if self.surfaced_parse_warnings.insert(key.clone()) {
                self.write_warn_to_stderr(&key.1);
            }
        }
    }

    /// Emit a batch of *untagged* parse warnings (plain message strings,
    /// e.g. `precomp::ParseEffects::warnings` replayed from the on-disk
    /// cache, which does not persist the origin-file tag) against a single
    /// known origin file. See `emit_parse_warnings`.
    pub(crate) fn emit_parse_warnings_for_file<I>(&mut self, file: &str, warnings: I)
    where
        I: IntoIterator<Item = String>,
    {
        let file = Some(file.to_string());
        self.emit_parse_warnings(warnings.into_iter().map(|w| (file.clone(), w)));
    }

    pub(crate) fn write_warn_to_stderr(&mut self, message: &str) {
        // Rakudo appends the warn location ("  in sub foo at file line N") to
        // every warning. Skip when the message already carries location lines
        // (some warn sites bake their own "  in block <unit> at ..." suffix;
        // every parser-level warning bakes a "\n    at FILE:LINE" suffix via
        // `parser::add_parse_warning` — appending the current-execution
        // backtrace on top of that would print the WRONG location, since a
        // parse warning fires while the VM is mid-executing an unrelated
        // `use`/`EVAL`/module-load statement, not the line the warning is
        // actually about).
        let msg = if message.contains("\n  in ") || message.contains("\n    at ") {
            format!("{}\n", message)
        } else {
            let bt = self.build_backtrace_string();
            if bt.is_empty() {
                format!("{}\n", message)
            } else {
                format!("{}\n{}\n", message, bt)
            }
        };
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
