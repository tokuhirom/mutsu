//! The `control` subsystem of ADR-10779: control-flow and lifecycle state --
//! the CONTROL/CATCH handler stacks, `let`/`temp` saves, END/LEAVE/CHECK/BEGIN
//! phaser bookkeeping, `once` blocks, and the program's halt/exit status and
//! uncaught-exception reporting.

use super::*;

#[derive(Default)]
pub(crate) struct ControlState {
    pub(crate) halted: bool,
    /// Prints an uncaught mainline exception; `run` calls it before the END
    /// phasers, as rakudo's top-level handler does. See [`Interpreter::set_uncaught_reporter`].
    pub(crate) uncaught_reporter: Option<UncaughtReporter>,
    /// Set once `uncaught_reporter` has printed the error `run` returns.
    pub(crate) uncaught_reported: bool,
    pub(crate) exit_code: i64,
    /// Set while the END phasers run for a program that is already exiting, and
    /// once any END phaser has itself called `exit`. A further `exit` still
    /// unwinds but leaves [`Self::exit_code`] alone — rakudo latches the process
    /// status at the first `exit` (`the-end-is-nigh`), so `exit 42; END { exit 7 }`
    /// exits 42. See `Interpreter::finish` and `builtin_exit`.
    pub(crate) exit_status_locked: bool,
    /// True while the main compilation unit's BEGIN prologue (ADR-0134) is
    /// still running: `run` raises it before the mainline starts and the
    /// `EndBeginPrologue` opcode lowers it once the prologue and its
    /// undeclared-routine guards are done. An error that escapes the mainline
    /// while it is still raised is a compile-time failure, so `run` skips the
    /// END phasers for it (#10977).
    pub(crate) begin_prologue_pending: bool,
    /// Number of active CONTROL handlers in the current VM stack. Tracked
    /// on the interpreter (rather than per-VM) so that nested VMs (e.g.
    /// EVAL) can observe handlers installed by the outer VM and propagate
    /// warn/control signals appropriately.
    pub(crate) control_handler_depth: u32,
    /// Registered END phasers, in registration order (they run in reverse).
    pub(crate) end_phasers: Vec<EndPhaser>,
    /// Monotonic tie-breaker for [`EndPhaser::order`], so phasers within one
    /// [`end_order`] class keep the order they were registered in.
    pub(crate) end_phaser_seq: u64,
    /// One entry per module body currently executing, holding the [`end_order`]
    /// class the END phasers it registers belong to. Empty while the main
    /// compunit runs. See `load_module` for why a `use` reached from an `EVAL`
    /// is not `end_order::MODULE`.
    pub(crate) module_load_order: Vec<u64>,
    /// Tracks END phaser site_ids to ensure each is registered only once.
    /// Only consulted for phasers that were NOT pre-installed by
    /// `preregister_main_end_phasers` (a module's, an `EVAL`'s, an rvalue
    /// `END`): a pre-installed one owns a fixed slot in `end_phasers`, so
    /// re-reaching its declaration re-captures into that slot rather than
    /// adding a phaser.
    pub(crate) end_phaser_sites: HashSet<u64>,
    /// `ast::Stmt::Phaser::end_index` -> position in `end_phasers`, for the
    /// main compunit's ENDs, which `preregister_main_end_phasers` installs in
    /// source order before the body runs. Reaching such a declaration updates the slot's
    /// captured env instead of installing a second phaser; never reaching it
    /// still leaves the phaser installed, which is what makes an END inside a
    /// never-entered block (or a never-called sub) run at exit, as it does in
    /// rakudo.
    pub(crate) main_end_slots: HashMap<u32, usize>,
    /// Monotonic counter stamped into `EndPhaser::capture_seq` each time a
    /// phaser captures its declaring scope's env. See that field.
    pub(crate) end_phaser_capture_seq: u64,
    /// One frame per module body currently being loaded, innermost last; the
    /// main program is the implicit frame below them
    /// (`mainline_leave_phasers`). See `runtime::attach_target`.
    pub(crate) compunit_leave_frames: Vec<attach_target::CompunitLeaveFrame>,
    /// LEAVE phasers a `use` attached to the main program's compunit, run when
    /// the mainline finishes (`Interpreter::finish`).
    pub(crate) mainline_leave_phasers: Vec<Value>,
    /// Fired `once { ... }` results, keyed by `(routine-clone-id, op-position)`.
    /// Shared by `Arc` handle into every spawned thread's clone so a `once` in a
    /// sub run from multiple `start` blocks fires exactly once across threads
    /// (see [`once_store::OnceStore`]).
    pub(crate) once_values: Arc<once_store::OnceStore>,
    pub(crate) once_scope_stack: Vec<u64>,
    pub(crate) next_once_scope_id: u64,
    /// `let`/`temp` save stack; see [`LetSaveEntry`].
    pub(crate) let_saves: Vec<LetSaveEntry>,
    /// Active CONTROL handlers on the dynamic call stack (one per executing
    /// `CONTROL { }` block). Kept in lock-step with `control_handler_depth` so
    /// a `warn` raised deep inside a protected body can find the innermost
    /// handler via `.last()` and, if it is `resume_safe`, run it inline at the
    /// raise site (cross-frame resumable warn). See `vm::ControlHandlerEntry`.
    pub(crate) control_handlers: Vec<crate::vm::ControlHandlerEntry>,
    /// ADR-0072: active exception-absorbing regions on the dynamic call stack —
    /// every `try` and every block with a `CATCH { }`. A `die` raised deep inside
    /// a protected body consults `.last()`: when that innermost region's CATCH is
    /// resume-capable, the handler runs INLINE at the throw site so `.resume`
    /// returns to the `die`'s own call site with every intervening Rust frame
    /// still live. See `vm::CatchHandlerEntry`.
    pub(crate) catch_handlers: Vec<crate::vm::CatchHandlerEntry>,
    /// Monotonic id source for `CatchHandlerEntry::token`.
    pub(crate) catch_handler_seq: u64,
    pub(crate) check_phaser_depth: u32,
    /// Phaser word (`BEGIN`/`CHECK`) of each open `CheckPhaserStart`, aligned
    /// with `check_phaser_depth`; names the phaser in X::Comp::BeginTime.
    pub(crate) check_phaser_kinds: Vec<&'static str>,
    /// One frame per open BEGIN-time region: the registry entries that region
    /// hid, and the defs to put back when it closes. Depth-aligned with
    /// `check_phaser_depth`.
    pub(crate) begin_time_hidden: Vec<Vec<(Symbol, Option<Arc<FunctionDef>>)>>,
}

impl ControlState {
    /// The main interpreter's state: everything empty, `once` scope ids
    /// starting at 1.
    // Cost: O(1).
    pub(crate) fn new() -> Self {
        Self {
            next_once_scope_id: 1,
            ..Default::default()
        }
    }

    /// The state a spawned thread starts with: nothing in progress, but the
    /// same `once` store.
    // Cost: O(1).
    pub(crate) fn fork_for_thread(&self) -> Self {
        Self {
            // Share the store by handle (not a per-thread copy) so a `once` in a
            // sub run from several `start` blocks fires once across all threads.
            once_values: Arc::clone(&self.once_values),
            next_once_scope_id: self.next_once_scope_id,
            ..Default::default()
        }
    }
}
