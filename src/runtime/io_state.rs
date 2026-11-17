//! The `io` subsystem of ADR-10779: the program output sink and `warn`
//! suppression, the open IO handles, the program path and chroot, the newline
//! mode and encoding registry, and the TAP (`Test`) state.

use super::*;

pub(crate) struct IoState {
    /// Program output sink — stdout/stderr buffers, the immediate-flush flag,
    /// and thread-clone interleaving. Lifted behind `Arc<RwLock<…>>` (PR-B) so
    /// the VM and the Interpreter can reach it as peers, exactly like
    /// `io_handles` (③後段/④; see `docs/vm-output-ownership.md`). Access through
    /// the `output_sink()` / `output_sink_mut()` guarded accessors.
    pub(crate) output_sink: Arc<RwLock<OutputSink>>,
    pub(crate) warn_output: String,
    pub(crate) warn_suppression_depth: usize,
    /// `control_handlers.len()` recorded at each active `push_warn_suppression`
    /// call (`quietly`, the Hash hyper). A `warn` raised while suppressed must
    /// resume in place rather than reach a CONTROL handler registered outside
    /// the suppressed region -- rakudo's `quietly` installs its own
    /// resume-everything CONTROL, so an outer handler never sees the warning
    /// at all (#9607). The innermost entry bounds how far `try_control_inline`
    /// searches; a CONTROL declared *inside* the suppressed region is still
    /// above the boundary and gets first look, matching rakudo's nesting order.
    pub(crate) warn_suppression_boundaries: Vec<usize>,
    /// Parse warnings (e.g. "Duplicate 'is export' trait") already surfaced
    /// during the current top-level `run()` invocation, keyed by (origin
    /// file, message text). A module's source can be parsed more than once
    /// within a single run — once during the importer's export scan, once
    /// more when the `use` actually loads it — and each parse's warnings are
    /// drained and printed independently, so without this the same warning
    /// prints once per parse. Reset at the top of `run()` (not left to
    /// accumulate for the process lifetime), so a *separate* top-level
    /// program sharing this Interpreter instance (a later REPL line, e.g.)
    /// still sees its own warnings rather than having them silently
    /// swallowed by a stale entry. See
    /// `todo/tickets/module-parse-warning-reported-twice.md`.
    pub(crate) surfaced_parse_warnings: std::collections::HashSet<(Option<String>, String)>,
    /// All TAP / `Test` module runtime state (counter, subtest stack, bail-out).
    /// See [`TapState`] — extracted out of this struct so its ownership can later
    /// move (lever B). Access only through `self.tap`'s methods.
    pub(crate) tap: TapState,
    /// Open IO handles (files/sockets/listeners): the one process-wide table,
    /// shared by every thread's interpreter (ADR-17216 §2.4; see the
    /// [`io_handles`] module docs).
    pub(crate) io_handles: Arc<io_handles::IoHandleTable>,
    pub(crate) program_path: Option<String>,
    /// [`Self::program_path`] interned, set with it (`set_program_path`), so
    /// the per-call unit lookup (`unit_of_source_sym`) compares two symbols
    /// instead of resolving one back to text.
    pub(crate) program_path_sym: Option<Symbol>,
    pub(crate) test_assertion_line_stack: Vec<i64>,
    pub(crate) chroot_root: Option<PathBuf>,
    /// Bytes a user `IO::Handle` subclass's `READ` handed back BEYOND what the
    /// caller asked for, keyed by the handle instance's id.
    ///
    /// `IO::Handle.read($n)` is specified to keep the excess and serve the next
    /// read from it, which is what makes `Type/IO/Handle.rakudoc`'s second
    /// worked example work: its `READ` ignores the byte count and returns the
    /// whole buffer every time, and rakudo still prints `one` then `two`.
    /// Without the buffer the first `.get` swallowed both lines.
    /// TODO: entries are never reclaimed; a custom read handle is rare and the
    /// buffer is bounded by one `READ` call's result.
    pub(crate) user_io_read_buffers: HashMap<u64, Vec<u8>>,
    pub(crate) newline_mode: NewlineMode,
    /// Registry of encodings (both built-in and user-registered).
    /// Each entry maps a canonical name to an EncodingEntry.
    pub(crate) encoding_registry: std::sync::Arc<Vec<EncodingEntry>>,
}

impl IoState {
    pub(crate) fn new() -> Self {
        Self {
            output_sink: Arc::new(RwLock::new(OutputSink::new())),
            warn_output: String::new(),
            warn_suppression_depth: 0,
            warn_suppression_boundaries: Vec::new(),
            surfaced_parse_warnings: std::collections::HashSet::new(),
            tap: TapState::default(),
            io_handles: Arc::new(io_handles::IoHandleTable::default()),
            program_path: None,
            program_path_sym: None,
            test_assertion_line_stack: Vec::new(),
            chroot_root: None,
            user_io_read_buffers: HashMap::new(),
            newline_mode: NewlineMode::Lf,
            encoding_registry: Interpreter::shared_builtin_encodings(),
        }
    }

    /// The spawned thread's copy: it writes through the parent's shared
    /// stdout/stderr buffers, shares the process-wide handle table (ADR-17216
    /// §2.4) and starts the per-run warning and assertion-line state fresh.
    pub(crate) fn fork_for_thread(&mut self) -> Self {
        // Thread clones write through the parent's shared stdout/stderr buffers
        // so concurrent output interleaves in real chronological order.
        let thread_output_sink = {
            let mut parent_sink =
                output_sink::OutputSinkWriteGuard::new(&self.output_sink, "output_sink");
            // When the parent flushes stdout immediately (CLI / REPL mode) and the
            // thread is spawned at top level, the clone must do the same so its
            // `say`/`pass` output lands in real chronological order relative to the
            // main thread's direct writes. Otherwise the clone buffers into
            // `shared_thread_output` and is only drained at the next sync point
            // (`await` / `.result`), which lands a worker-thread test line *after*
            // an intervening main-thread one — producing TAP "tests out of
            // sequence". In buffered/capture mode (parent `immediate_stdout ==
            // false`) the shared buffer is still used, so `run()` capture is
            // unaffected.
            let parent_immediate = parent_sink.immediate_stdout;
            let shared_out = Arc::clone(
                parent_sink
                    .shared_thread_output
                    .get_or_insert_with(|| Arc::new(Mutex::new(String::new()))),
            );
            let shared_err = Arc::clone(
                parent_sink
                    .shared_thread_stderr
                    .get_or_insert_with(|| Arc::new(Mutex::new(String::new()))),
            );
            Arc::new(RwLock::new(OutputSink {
                output: String::new(),
                stderr_output: String::new(),
                output_emitted: false,
                immediate_stdout: parent_immediate,
                is_thread_clone: true,
                shared_thread_output: Some(shared_out),
                shared_thread_stderr: Some(shared_err),
                last_handle_result: None,
            }))
        };
        Self {
            output_sink: thread_output_sink,
            warn_output: String::new(),
            warn_suppression_depth: 0,
            warn_suppression_boundaries: Vec::new(),
            surfaced_parse_warnings: std::collections::HashSet::new(),
            tap: self.tap.clone_for_thread(),
            io_handles: Arc::clone(&self.io_handles),
            program_path: self.program_path.clone(),
            program_path_sym: self.program_path_sym,
            test_assertion_line_stack: Vec::new(),
            chroot_root: self.chroot_root.clone(),
            user_io_read_buffers: self.user_io_read_buffers.clone(),
            newline_mode: self.newline_mode,
            encoding_registry: self.encoding_registry.clone(),
        }
    }
}
