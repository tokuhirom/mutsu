use std::sync::{Arc, Mutex};

/// Read guard for the shared [`OutputSink`]; a [`ReentrantReadGuard`] keyed by
/// the `"output_sink"` lock name (PR-B lift — same playbook as `io_handles`).
///
/// [`ReentrantReadGuard`]: crate::runtime::lock_reentry::ReentrantReadGuard
pub(crate) type OutputSinkReadGuard<'a> =
    crate::runtime::lock_reentry::ReentrantReadGuard<'a, OutputSink>;

/// Write guard for the shared [`OutputSink`]; a [`ReentrantWriteGuard`] keyed by
/// the `"output_sink"` lock name.
///
/// [`ReentrantWriteGuard`]: crate::runtime::lock_reentry::ReentrantWriteGuard
pub(crate) type OutputSinkWriteGuard<'a> =
    crate::runtime::lock_reentry::ReentrantWriteGuard<'a, OutputSink>;

/// Program output-sink state, extracted from [`Interpreter`](super::Interpreter)
/// so its ownership can later move to the VM (③後段/④ — see
/// `docs/vm-output-ownership.md`). Mirrors the `TapState` extraction pattern:
/// the fields live in one struct now (PR-A, a plain `Interpreter` field), to be
/// lifted into an `Arc<RwLock<OutputSink>>` shared handle (PR-B) and reached
/// from the VM for native Stdout/Stderr output dispatch (PR-C).
///
/// `emit` / `emit_stderr` carry the full write decision (immediate real-stdout
/// flush vs. the in-memory buffer vs. a thread clone's shared buffer). The TAP
/// "is a subtest in progress" gate is passed in as `subtest_active` rather than
/// pulling the whole TAP state machine in here.
pub(crate) struct OutputSink {
    /// Accumulated stdout, returned by `run()` / `take_output` when output is
    /// buffered (i.e. not flushed immediately to the real stdout).
    pub(crate) output: String,
    /// Accumulated stderr.
    pub(crate) stderr_output: String,
    /// Whether any output was emitted since the last `clear` (useful when
    /// `immediate_stdout` bypasses the buffer).
    pub(crate) output_emitted: bool,
    /// When true, stdout is flushed to the real stdout immediately (CLI / REPL /
    /// Proc::Async children); otherwise it accumulates in `output`.
    pub(crate) immediate_stdout: bool,
    /// True in a thread clone, whose output interleaves through
    /// `shared_thread_output` to preserve chronological order across promises.
    pub(crate) is_thread_clone: bool,
    /// Shared real-time stdout buffer used by thread clones.
    pub(crate) shared_thread_output: Option<Arc<Mutex<String>>>,
    /// Shared real-time stderr buffer used by thread clones.
    pub(crate) shared_thread_stderr: Option<Arc<Mutex<String>>>,
}

impl OutputSink {
    pub(crate) fn new() -> Self {
        OutputSink {
            output: String::new(),
            stderr_output: String::new(),
            output_emitted: false,
            immediate_stdout: false,
            is_thread_clone: false,
            shared_thread_output: None,
            shared_thread_stderr: None,
        }
    }

    /// Emit `text` to stdout. `subtest_active` is whether a TAP subtest is in
    /// progress (it suppresses the immediate real-stdout flush). Behavior is
    /// identical to the former `Interpreter::emit_output` body.
    pub(crate) fn emit(&mut self, text: &str, subtest_active: bool) {
        self.output_emitted = true;
        if !subtest_active && self.immediate_stdout {
            use std::io::Write;
            let _ = std::io::stdout().write_all(text.as_bytes());
            let _ = std::io::stdout().flush();
        } else if self.is_thread_clone && self.shared_thread_output.is_some() {
            // Thread clones write to the shared buffer so concurrent output is
            // interleaved in real chronological order (not grouped per-promise).
            self.shared_thread_output
                .as_ref()
                .unwrap()
                .lock()
                .unwrap()
                .push_str(text);
        } else {
            self.output.push_str(text);
        }
    }

    /// Emit `text` to stderr. `subtest_active` suppresses the immediate
    /// real-stderr flush. Mirrors the `IoHandleTarget::Stderr` branch of
    /// `write_to_handle_value_trying` (no `output_emitted` flag, no Stdout-style
    /// thread-clone interleaving — stderr buffers per interpreter).
    pub(crate) fn emit_stderr(&mut self, text: &str, subtest_active: bool) {
        if !subtest_active && self.immediate_stdout {
            use std::io::Write;
            let _ = std::io::stderr().write_all(text.as_bytes());
            let _ = std::io::stderr().flush();
        } else {
            self.stderr_output.push_str(text);
        }
    }
}
