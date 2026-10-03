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

    /// Write to the output buffer and also flush to real stdout.
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
        self.output_sink_mut().emit(text);
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
        // The parser's module export scan cache runs before this interpreter
        // is reachable, so it reads a process-wide mirror of this switch —
        // otherwise `--no-precomp` would silently leave half the caching on.
        crate::precomp::set_process_enabled(val);
    }

    /// Check if MONKEY-TYPING pragma is active.
    pub(crate) fn monkey_typing_enabled(&self) -> bool {
        self.monkey_typing
    }

    /// Install the callback that prints an uncaught mainline exception. `run`
    /// calls it *before* the END phasers, matching rakudo (the exception is
    /// reported first, then END runs). Without one, `run` just returns the
    /// error and the caller prints it.
    pub fn set_uncaught_reporter(&mut self, reporter: super::UncaughtReporter) {
        self.control.uncaught_reporter = Some(reporter);
    }

    /// True when the installed reporter already printed the error `run` returned.
    pub fn uncaught_reported(&self) -> bool {
        self.control.uncaught_reported
    }

    // Cost: O(1) plus the reporter's own rendering.
    pub(crate) fn report_uncaught_early(&mut self, err: &RuntimeError) {
        if let Some(mut reporter) = self.control.uncaught_reporter.take() {
            reporter(self, err);
            self.control.uncaught_reported = true;
            self.control.uncaught_reporter = Some(reporter);
        }
    }

    pub fn exit_code(&self) -> i64 {
        self.control.exit_code
    }

    /// Return the value of `%*ENV<RAKU_EXCEPTIONS_HANDLER>`, if set.
    /// This selects the format used to print uncaught exceptions (e.g. "JSON").
    pub fn exceptions_handler(&self) -> Option<String> {
        self.env_hash_var("RAKU_EXCEPTIONS_HANDLER")
            .filter(|s| !s.is_empty())
    }

    /// The string value of `%*ENV<name>`, if the key exists. `%*ENV` is the
    /// program's view of the environment: writes are never mirrored into the
    /// process environment (#11241), so a run-time environment read goes
    /// through this rather than `std::env::var`.
    // Cost: O(1) expected, one hash probe plus the value's stringification.
    pub(crate) fn env_hash_var(&self, name: &str) -> Option<String> {
        match self.env.get("%*ENV").map(Value::view) {
            Some(ValueView::Hash(map)) => map.get(name).map(Value::to_string_value),
            _ => None,
        }
    }

    /// Like [`Self::env_hash_var`], but before `%*ENV` exists (early start-up)
    /// it reads the process environment, which `%*ENV` is built from.
    // Cost: O(1) expected, one hash probe (or one `getenv`) plus stringification.
    pub(crate) fn env_var_or_process(&self, name: &str) -> Option<String> {
        match self.env.get("%*ENV").map(Value::view) {
            Some(ValueView::Hash(map)) => map.get(name).map(Value::to_string_value),
            _ => std::env::var(name).ok(),
        }
    }

    pub(crate) fn is_halted(&self) -> bool {
        self.control.halted
    }

    /// True when the program asked to `exit` rather than running off its end.
    /// rakudo's `exit` terminates the process immediately, without waiting for
    /// outstanding non-`app_lifetime` `Thread`s — see
    /// [`Self::join_outstanding_threads`].
    pub fn exit_requested(&self) -> bool {
        self.control.halted
    }

    /// Wait for every still-running non-`app_lifetime` `Thread` before the
    /// process terminates (`Type/Thread.rakudoc`: with `:!app_lifetime`, the
    /// default, "the process will only terminate when the thread has
    /// finished"). Called from `main` on the normal-completion path only —
    /// verified against raku v2026.06, neither `exit` nor an uncaught
    /// exception waits.
    pub fn join_outstanding_threads(&mut self) {
        crate::runtime::methods_collection_ops::join_outstanding_threads();
        // Same post-join synchronization `Thread.finish` performs: publish the
        // threads' shared-variable writes and flush anything they buffered
        // (e.g. TAP lines from a thread spawned inside a subtest).
        self.sync_shared_vars_to_env();
        self.drain_shared_thread_output();
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
        // Rakudo's default `warn` handler prints through the dynamic `$*ERR`,
        // so a `my $*ERR = Trap.new` (silently, Test::Output) captures the
        // warning too. Only the process stderr handle takes the direct path.
        if self.dynamic_err_redirected() && self.write_to_named_handle("$*ERR", &msg, false).is_ok()
        {
            self.warn_output.push_str(&msg);
            return;
        }
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

    /// Whether the dynamic `$*ERR` currently names something other than the
    /// process stderr: a user object with a `print` method, a file handle, or
    /// `$*OUT`'s handle.
    // Cost: O(1) — one dynamic-variable lookup and one handle-table probe.
    fn dynamic_err_redirected(&mut self) -> bool {
        let Some(handle) = self.get_dynamic_handle("$*ERR") else {
            return false;
        };
        if handle.is_nil() {
            return false;
        }
        match self.with_handle_mut_opt(&handle, |state| Ok(state.is_stderr_target())) {
            Ok(Some(is_stderr)) => !is_stderr,
            Ok(None) => Self::handle_id_from_value(&handle).is_none(),
            Err(_) => false,
        }
    }

    pub(crate) fn push_warn_suppression(&mut self) {
        self.warn_suppression_depth += 1;
        self.warn_suppression_boundaries
            .push(self.control.control_handlers.len());
    }

    pub(crate) fn pop_warn_suppression(&mut self) {
        self.warn_suppression_depth = self.warn_suppression_depth.saturating_sub(1);
        self.warn_suppression_boundaries.pop();
    }

    /// The current warning-suppression state, to hand back to
    /// [`Self::restore_warn_suppression`] when an unwind leaves a region.
    pub(crate) fn warn_suppression_mark(&self) -> (usize, usize) {
        (
            self.warn_suppression_depth,
            self.warn_suppression_boundaries.len(),
        )
    }

    /// Drop every suppression frame pushed since `mark` was taken — the
    /// frames of `quietly` regions an error unwound out of before their
    /// `WarnSuppressPop` ran.
    pub(crate) fn restore_warn_suppression(&mut self, mark: (usize, usize)) {
        self.warn_suppression_depth = mark.0;
        self.warn_suppression_boundaries.truncate(mark.1);
    }

    pub(crate) fn warning_suppressed(&self) -> bool {
        self.warn_suppression_depth > 0
    }

    /// How far `try_control_inline` may search `control_handlers` for a `warn`
    /// raised right now: every handler, when nothing is suppressed, or only
    /// those registered since the innermost active suppression began.
    pub(crate) fn warn_control_handler_floor(&self) -> usize {
        if self.warning_suppressed() {
            self.warn_suppression_boundaries
                .last()
                .copied()
                .unwrap_or(0)
        } else {
            0
        }
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
