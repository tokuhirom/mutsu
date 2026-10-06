//! The `lexicals` subsystem of ADR-10779: variable storage and lexical
//! bookkeeping that lives outside the VM frames -- `our` variables and the
//! package/unit lexical tables, `state` variables and their scope ids, the
//! escaping-`our` cells, lexical-sub aliasing, nested method captures,
//! readonly tracking and the per-block declaration sets.

use super::*;

pub(crate) struct LexicalState {
    /// The class or role whose package body (or role body, re-run at a
    /// composition) is running, innermost last. A block's
    /// `OpCode::CaptureNestedMethodEnv` files its capture under the innermost
    /// owner: a role body runs in the COMPOSING class's package, so the
    /// current package alone would mix up a class's own nested-block methods
    /// with a composed role's.
    pub(crate) nested_capture_owners: Vec<Symbol>,
    /// Lexical captures of `method`s declared in nested blocks of a package
    /// body, keyed by (owner, per-body declaration index): written by
    /// `OpCode::CaptureNestedMethodEnv` when the block runs, taken when the
    /// hoisted method with that index is installed (a class) or composed (a
    /// role). See `vm_nested_method_capture`.
    pub(crate) nested_method_captures: HashMap<(Symbol, u32), crate::env::Env>,
    /// The nested-block method captures each class/role composition's role
    /// body filed, keyed by (composing class, role). A role body runs once per
    /// composition (`Registry::composed_role_bodies`), but the class may be
    /// registered again (the in-place registration after a nested
    /// declaration's compile-time shell, or a redeclaration in a loop); the
    /// re-registration rebuilds the composed methods and gives them these
    /// captures back. See `apply_nested_method_captures`.
    pub(crate) composed_nested_method_captures:
        HashMap<(Symbol, Symbol), rustc_hash::FxHashMap<u32, crate::env::Env>>,
    /// Persistent store for `our`-scoped variables.  Values are saved here
    /// by `SetGlobal` so they survive block-scope restoration (which only
    /// preserves env keys that existed before the block).
    /// `FxHashMap`, not the SipHash default: `our_package_var_key` probes this
    /// store on EVERY `@`/`%` container read once a program declares any `our`
    /// variable, and hashing the (already short) key cryptographically was a
    /// measurable slice of that walk (#7571). Same reasoning as `env::SymMap`.
    pub(crate) our_vars: rustc_hash::FxHashMap<String, Value>,
    /// The UNQUALIFIED spelling (`@words`, `$x`) of every key ever stored in
    /// [`Self::our_vars`] — the sigil plus the segment after the last `::`.
    ///
    /// `our_package_var_key` reconstructs a package-qualified candidate for a
    /// bare name by walking up to four candidate packages' `::` chains, one
    /// `format!` + store probe per step, on EVERY `@`/`%` container read once a
    /// program declares any `our` variable at all. Every candidate it builds
    /// ends in `<sigil><bare>`, so a name absent from this set cannot match any
    /// stored key and the whole walk can be skipped — which is the common case
    /// even in a module that does declare `our` variables (#7571).
    ///
    /// Append-only, exactly like `our_vars` itself (which is only ever inserted
    /// into, never removed from), so a membership test can never be stale.
    pub(crate) our_var_unqualified: rustc_hash::FxHashSet<Symbol>,
    /// The process-level dynamics written at run time (`$PROCESS::OUT = ...`,
    /// `PROCESS::<$name> := value`, a `$*name = ...` that lands on the process
    /// binding), keyed by the dynamic-var env key (`*name`/`@*name`/`%*name`).
    ///
    /// One store for the whole lineage: thread clones share it, so a write on
    /// one thread is seen by a thread that was already running (ADR-11318,
    /// #11318). It is also what makes such a write durable across frames: it
    /// never lands in a frame's env overlay, which a nested block/sub frame
    /// drops on exit (#8682). Reads reach it through
    /// [`Interpreter::resolve_process_dynamic`].
    pub(crate) process_dynamics: process_stash::ProcessStash,
    /// The NQP/MoarVM HLL symbol table (`nqp::bindhllsym`/`nqp::gethllsym`),
    /// keyed by `(hll, name)`. Real MoarVM keeps one such table per process,
    /// shared by every HLL; mutsu instead seeds it fresh on every
    /// [`Interpreter::new`] / thread-spawn construction (see
    /// `bootstrap_hll_syms`) rather than sharing it across interpreter
    /// clones, mirroring how `process_dynamics` above already does not
    /// propagate to a spawned thread's interpreter. That is sufficient for
    /// the one binding mutsu itself installs (`"default"` -> `"SysConfig"`,
    /// Rakudo core's own bootstrap symbol) and for any nqp code that binds
    /// and reads a symbol within one interpreter's lifetime.
    pub(crate) hll_syms: rustc_hash::FxHashMap<(String, String), Value>,
    /// Package-block `my` lexicals, keyed by package name then env var name.
    /// A named sub defined in a `package Foo { my $x = ...; sub f { $x } }` block
    /// closes over `$x`, but mutsu's registry subs have no per-sub closure env and
    /// resolve free vars from the call-time env; the block scope is dropped on exit
    /// (`exec_package_scope_op`) so a by-name/exported call (`Foo::f`, an exported
    /// `MAIN`) can no longer see `$x`. After the block runs, its `my` lexicals are
    /// recorded here keyed by the package, and a `GetGlobal` miss falls back to
    /// `package_lexicals[current_package]`. This fires ONLY inside that package's
    /// subs (where `current_package == Foo`), so it does not leak the lexical to
    /// bare references after the block (which run under `GLOBAL`).
    pub(crate) package_lexicals: std::sync::Arc<PackageLexicals>,
    /// Names in `package_lexicals` that are class-body `my` statics
    /// (`class C { my $x = ...; method m { $x } }`), keyed by class. These are
    /// stored in `package_lexicals` so a method's BARE `$x` read/write and the
    /// `writeback_package_scope_var` mutation path reuse the existing machinery,
    /// but — unlike a `package Foo { my $x }` block lexical — they must NOT be
    /// reachable through a QUALIFIED `$C::x`, which is a distinct package variable
    /// (see t/package-lookup.t). `package_scope_lexical`'s qualified branch skips
    /// any (class, name) recorded here.
    pub(crate) class_body_static_names:
        std::sync::Arc<HashMap<String, std::collections::HashSet<String>>>,
    /// File-scope `my` lexicals of a loaded `unit` compunit, keyed by the unit
    /// package name then env var name, each holding a shared `ContainerRef` cell.
    ///
    /// A module body runs in the env of whatever frame loaded it, so a file-scope
    /// `my $output` lands in that flat env under the plain key `output` — the SAME
    /// storage a script's own `my $output` uses. The two then alias one another and
    /// writes go both ways (`todo/deep/module-file-scope-my-shares-the-callers-env.md`).
    /// After the module body has run, `load_module` moves those names out of `env`
    /// into this store and restores whatever the loading scope had under them; the
    /// module's own routines — which run with `current_package` set to the unit
    /// package — resolve them here instead, read through `unit_scope_lexical` and
    /// written through `unit_scope_lexical_write`. Cells, not snapshots, so a write
    /// from one routine is seen by every other (`_init_io` sets `$output`, `proclaim`
    /// reads it).
    ///
    /// Distinct from `module_scope_lexicals`, which is a *last-resort* read-only
    /// snapshot keeping a module's bare names reachable once the loading frame is
    /// gone; this store is authoritative and consulted BEFORE `env`.
    pub(crate) unit_lexicals: std::sync::Arc<PackageLexicals>,
    /// Bumped by [`Interpreter::unit_lexicals_cow_mut`] and
    /// [`Interpreter::package_lexicals_cow_mut`], the funnels through which
    /// `unit_lexicals` and `package_lexicals` are mutated. TRIR's per-routine
    /// free-variable cache (`trir_outer_cache`) is keyed on it, so a bucket
    /// or binding added anywhere invalidates every cached resolution.
    /// Writing *through* a cell already in the table does not bump it, and
    /// must not: the cell is what the cache holds, so such a write is
    /// visible without re-resolving.
    pub(crate) unit_lexical_gen: u64,
    /// Named subs that captured at least one enclosing-scope `my` free
    /// variable into `unit_lexicals` at registration time (ADR-0024), mapped to
    /// the `unit_lexicals` bucket key holding their cells.
    ///
    /// A sub declared at mainline maps to [`MAINLINE_UNIT_KEY`] (all mainline
    /// subs share one bucket, because mainline is one scope). A sub declared
    /// inside a *bare block* maps to its own
    /// [`BLOCK_LEXICAL_UNIT_PREFIX`]-keyed bucket, because sibling blocks are
    /// distinct scopes that may declare the same name.
    ///
    /// A free-variable read/write resolves through those cells ONLY while the
    /// last (non-block) routine frame's name is a key here AND its package is
    /// `GLOBAL` — see `Interpreter::active_unit_lexical_bucket`. Empty for a
    /// program with no such capture: zero cost beyond the map-presence check
    /// already paid by `unit_lexical_slot`.
    pub(crate) mainline_lexical_subs: std::sync::Arc<std::collections::HashMap<String, String>>,
    /// mutsu#9111: a sub declared inside a ROUTINE maps to its free variables
    /// and the hidden local of the declaring frame each is aliased to. A
    /// free-variable access from such a sub's frame reads the alias from env
    /// first (see `vm/vm_lexsub_aliases.rs`). Empty unless a routine declared
    /// a `my sub` with free variables.
    pub(crate) lexsub_free_aliases: std::sync::Arc<crate::vm::LexSubAliasTable>,
    /// The latest activation's cell per routine-nested sub free variable
    /// (see `vm::LexSubLatestCells`).
    pub(crate) lexsub_latest_cells: std::sync::Arc<crate::vm::LexSubLatestCells>,
    /// Shared cells for block lexicals captured by an `our`-scoped named sub
    /// declared inside a *bare* block (not a package block). Unlike a `my sub`, an
    /// `our sub` is installed into the package registry and stays callable after
    /// the block exits, but a registry routine carries no per-sub closure env. When
    /// the captured local (`my $a`) is declared, the VM boxes it into a shared
    /// `ContainerRef` cell and records it here keyed by the variable name; a
    /// free-var read inside the escaped sub resolves through this cell
    /// (`escaping_our_read`), so `our sub f { $a }` called after the block sees the
    /// live value (Raku semantics). Populated by the sub's source-order
    /// `RegisterSub` (`exec_register_sub_op`) once the captured local has been boxed
    /// — keyed to the sub's declaration, not the box site, so a same-named sibling-
    /// block `my` cannot pollute it. A read BEFORE the block runs misses (the cell is
    /// not yet recorded), correctly yielding the undefined value.
    pub(crate) escaped_our_lexical_cells: ValueMap,
    /// Names of block lexicals that are captured by an `our`-scoped named sub
    /// (the union of every code's `needs_cell_escaping_our_sub`). Seeded once at the
    /// start of `run()` from the top-level code, so it is known BEFORE the declaring
    /// block executes. A free-variable read of such a name from inside a routine
    /// resolves through `escaped_our_lexical_cells` ONLY — never the shared env —
    /// so an unrelated leaked `env` value from a sibling block cannot shadow it, and
    /// a read before the block correctly yields the undefined value (the cell is not
    /// recorded yet). Empty for ordinary programs: zero cost.
    pub(crate) escaping_our_lexical_names: std::sync::Arc<std::collections::HashSet<String>>,
    /// The subset of `escaping_our_lexical_names` that are slotless `for`
    /// parameters (`CompiledCode::escaping_our_env_params`): `RegisterSub`
    /// boxes their env binding itself, as there is no declaration to do it.
    pub(crate) escaping_our_env_param_names: std::sync::Arc<std::collections::HashSet<String>>,
    /// Names of the `our`-scoped subs declared in bare blocks (the subs whose
    /// free-variable reads/writes may resolve through `escaped_our_lexical_cells`).
    /// The cell resolution fires ONLY while the innermost named routine frame is
    /// one of these subs — a plain `my sub` that merely shares a captured
    /// variable's name must keep resolving through its own live env capture.
    pub(crate) escaped_our_sub_names: std::sync::Arc<std::collections::HashSet<String>>,
    /// Bare (sigil-less) names of the plain `our` SCALARS whose canonical home
    /// is a shared `ContainerRef` cell published under a package-qualified key
    /// (`OpCode::DeclareOurScalar` — see `vm_our_package_vars`). Recorded only
    /// for a declaration inside a real package (a file-scope `our $x` collapses
    /// its qualified name to the bare name and is therefore never redirected).
    ///
    /// This is a cheap pre-gate, not the resolution itself: a bare-name read or
    /// write consults `our_package_scalar_*` only when the name is in this set,
    /// so the ordinary program — which never declares a package `our` scalar —
    /// pays a single empty-set check on the variable hot path.
    pub(crate) our_scalar_cell_names: std::sync::Arc<std::collections::HashSet<String>>,
    /// Keyed by `(base key symbol, closure scope id)` instead of a formatted
    /// `String` — see `scoped_state_key`/`state_key_display`. The `Option<u64>`
    /// distinguishes an un-scoped (named-sub/module-level) `state` var from one
    /// scoped to a specific closure clone.
    pub(crate) state_vars: HashMap<(Symbol, Option<u64>), Value>,
    /// Keys inserted into `state_vars` since the last `start` spawn migrated
    /// them into the cross-thread store (see `seed_unmigrated_state_vars`).
    /// Every key whose entry existed at an earlier spawn was already seeded
    /// then (`seed_if_absent`, so a second seed is a no-op), so the spawn only
    /// has to visit what was inserted since: O(new keys) per spawn instead of
    /// O(every state entry the program ever created) (#9504).
    pub(crate) state_vars_unmigrated: Vec<(Symbol, Option<u64>)>,
    /// Cells a hoisted named-sub registration seeded for a free variable whose
    /// declaration has not run yet (#9911, ADR-0024's textual-order edge); the
    /// declaration's store adopts its cell. See `vm/vm_hoist_capture_cells.rs`.
    /// Empty unless a sub is called before a variable it reads is declared.
    pub(crate) hoist_pending_cells: Vec<crate::vm::HoistPendingCell>,
    /// `@`/`%` names bound as **parameters through the env-level (runtime)
    /// binding path** — a destructuring sub-signature (`-> [$a, @K] { ... }`)
    /// or a runtime-invoked callback's plain parameter (`reduce -> $h, @words
    /// { ... }`) — with their sigils, each mapped to every live container a
    /// binding of that name stored in `env` (held weakly; see
    /// [`param_bound_aggregates::ParamBoundAggregates`]).
    ///
    /// Such a name is a fresh per-invocation binding, never the one shared
    /// object the name-keyed `shared_vars` lane exists to represent. Left on that
    /// lane it is seeded once (`seed_if_absent`) and then frozen at the first
    /// spawn's value, so two `start` blocks created by two iterations of the same
    /// block both read the same `@K`/`@words`. `clone_for_thread_for_block`
    /// consults this map — intersected with the spawned block's free variables
    /// AND checked for container identity against the current env value, so an
    /// unrelated outer aggregate that merely shares a name (or a later `my`
    /// re-binding of it) is unaffected — to keep those names off the lane and
    /// mask them in the child instead.
    ///
    /// Populated unconditionally (not gated on `shared_vars_active`): the
    /// *first* spawn in a process consults it before any thread exists, and a
    /// gate would leave exactly that spawn's binding to be seeded — and frozen —
    /// on the lane.
    pub(crate) param_bound_aggregates: param_bound_aggregates::ParamBoundAggregates,
    /// Union of every executed `CompiledCode::type_body_written_lexicals`:
    /// lexicals written by a registered class/role method body. These keep the
    /// name-keyed `shared_vars` lane even when a spawned block also captures
    /// them — the capture analysis cannot see such a write (PLAN.md §6).
    /// Populated at `RegisterClass` / `RegisterRole`, which always run before
    /// the type can be instantiated.
    pub(crate) type_body_written_lexicals: std::sync::Arc<std::collections::HashSet<String>>,
    /// Per-closure-instance captured-variable state, keyed by
    /// (closure instance id, captured variable Symbol). This is the hot
    /// closure-call persistence store (loaded/saved on every closure call for
    /// its free variables); a typed key avoids the per-call
    /// `format!("__mutsu_closure_cap::{id}::{name}")` String allocation and the
    /// String hashing that dominated the closure dispatch profile.
    pub(crate) closure_captured_state: HashMap<(u64, Symbol), Value>,
    /// Variable dynamic-scope metadata used by `.VAR.dynamic`.
    pub(crate) var_dynamic_flags: HashMap<String, bool>,
    /// Variable binding aliases: maps target name -> source name.
    /// When target is read, the value of source is returned instead.
    /// Set up by $CALLER::target := $source binding.
    pub(crate) var_bindings: HashMap<String, String>,
    /// Monotonic flag: set once any `atomicint` variable / atomic storage has been
    /// registered in this interpreter (or inherited from a parent thread). The
    /// per-`GetGlobal`/`GetLocal` atomic-variable check is expensive (a `format!`
    /// plus two `var_type_constraint` lookups, each itself a `format!`), yet
    /// atomics are exotic; when this flag is clear the entire check is skipped,
    /// which removes that cost from the hot variable-read path. Never cleared, so
    /// a program that stops using an atomic still resolves correctly. See also the
    /// process-global `atomic_var_seen_anywhere`, which the reset path needs
    /// because a worker thread's `cas` marks only the WORKER's copy of this field.
    /// pub(crate) so `vm_jit_layout` can `offset_of!` it: the Tier B inline
    /// GetLocal fast path reads this flag from native code.
    pub(crate) atomic_var_seen: bool,
    /// Monotonic flag: set once any sigilless-parameter alias
    /// (`__mutsu_sigilless_alias::name` env key, created when binding a `\target`
    /// raw/sigilless parameter or a `:=`-style alias) has been registered. The hot
    /// write-back path calls `propagate_sigilless_alias_chain` on every inc-dec /
    /// compound-assign, which builds the `__mutsu_sigilless_alias::<name>` key
    /// plus an env lookup to walk the alias chain. Sigilless aliases are rare; when
    /// this flag is clear no alias key exists, the chain is empty, and the whole
    /// walk (and its key construction) is skipped. Set at every alias-insert site (see
    /// `sigilless_alias_key`). Never cleared, so removing an alias still resolves.
    pub(crate) sigilless_alias_seen: bool,
    /// Set of variable names that are readonly (default parameter binding).
    /// Copy-on-write and `Symbol`-keyed — see [`ReadonlySet`]. Boxed (its own
    /// heap allocation, separate from `Interpreter`'s) so
    /// [`crate::vm::vm_call_state_guard::ReadonlyFrameGuard`] can hold a raw
    /// pointer into it that survives intervening `&mut self` calls — see that
    /// guard's doc comment and the module doc in `vm_call_state_guard.rs`
    /// ("v3": each guarded field is its own separate heap allocation).
    pub(crate) readonly_vars: Box<std::cell::RefCell<ReadonlySet>>,
    /// Journal of readonly-set mutations made while at least one readonly
    /// scope is open (newest last); `exit_readonly_frame` replays the
    /// inverses back to its scope's mark. `Scope` sentinels bound each open
    /// frame's entries (see `enter_readonly_frame`). Journaling is off at top
    /// level (`readonly_frames == 0`), so the journal cannot grow across a
    /// program's lifetime. Boxed for the same reason as [`Self::readonly_vars`].
    pub(crate) readonly_undo: Box<std::cell::RefCell<Vec<ReadonlyUndo>>>,
    /// Number of currently-open readonly scopes (see `enter_readonly_frame`).
    /// Boxed for the same reason as [`Self::readonly_vars`].
    pub(crate) readonly_frames: Box<Cell<u32>>,
    /// The aggregate an `is rw` routine's tail handed back through a READONLY
    /// binding (`sub w($p) is rw { $p }` with `w(%r)`), set by
    /// `OpCode::MarkReadonlyRwTail`. The routine-call assignment checks the call result
    /// against it by identity and refuses the store (#11108): such a tail is
    /// a value, while an `@`/`%`/sigilless tail aliasing the same aggregate
    /// would be a container.
    pub(crate) readonly_rw_tail: Option<Value>,
    /// `Box<Cell<_>>`-backed for the same reason as `bind_context` et al.
    /// above: `vm_call_state_guard::StateScopeGuard::Drop` restores it via a
    /// raw pointer into this separate heap allocation, immune to Stacked
    /// Borrows retags of `Interpreter`'s own memory.
    pub(crate) state_scope_id: Box<Cell<Option<u64>>>,
    /// One-shot handoff of a `state` scope into the next nested run: the
    /// interpreter-fallback call path runs a routine body via `run_nested`,
    /// whose register reset clears `state_scope_id` — this field survives the
    /// reset and is consumed by `with_nested_registers` as the nested run's
    /// initial scope, so a fallback-dispatched named sub still keys its state
    /// by its registration clone id (per-clone `state` in nested named subs).
    pub(crate) pending_nested_state_scope: Option<u64>,
    /// Bare names that appear as a `&`-sigil parameter in some registered sub
    /// (e.g. `foo` from `sub callit(&foo) {...}`). A call to such a name may be
    /// shadowed by a lexical `&name` binding in the current frame, so the
    /// name-keyed light-call caches must be bypassed for it (the slow path's
    /// `lexical_override` check resolves the correct callable). Populated at sub
    /// registration; checked cheaply (guarded by `is_empty()`) on each call.
    pub(crate) amp_param_shadowed_names: std::collections::HashSet<Symbol>,
    /// Frame-lexical routines (ADR-0113) whose definition this interpreter
    /// has derived, keyed by the compile-time identity both their
    /// declaration and their call sites carry. A declaration derives its
    /// entry the first time it runs; every later declaration and every call
    /// is a lookup. Shared (copy-on-write) with thread clones, which run
    /// closures that call routines their parent already declared.
    pub(crate) frame_lexical_routines:
        Arc<rustc_hash::FxHashMap<crate::opcode::FrameLexicalRef, crate::vm::FrameLexicalTarget>>,
    /// Closure bodies whose compiled chunk calls a frame-lexical routine
    /// (ADR-0113), keyed by the address of the body's shared statement
    /// buffer. A carrier path that compiles such a body again from its AST
    /// looks its origin chunk up here and inherits the call table. The entry
    /// keeps the body `Arc` alive, so an address is never reused for another
    /// body while it is in the table.
    pub(crate) frame_lexical_closure_bodies: Arc<crate::vm::FrameLexicalClosureBodies>,
    pub(crate) block_declared_vars: ScopeStack<NameSet>,
    /// Names of every `constant $name = ...` scalar ever declared in this run
    /// (ADR-0022 Slice 5's `__mutsu_constant_var::` marker). Lets
    /// `exec_set_local_op_inner` skip the marker-removal `format!` + env
    /// lookup on an ordinary (non-constant) scalar `my`/`state` whose name was
    /// never used by a `constant` — the overwhelming common case, and NOT the
    /// same as "no constant has been declared anywhere": a single
    /// program-wide bool here previously made every subsequent `my`/`state`
    /// in the whole program (any name) pay the removal cost the instant just
    /// one `constant` existed anywhere (`benchmarks/debug-guard.raku`'s
    /// `constant DEBUG = False` followed by a hot-loop `my $y`, e.g.). Per-name
    /// membership is the actual precondition for the marker existing at all.
    /// Entries are never removed; a name is either "never a constant" (never
    /// pays the removal) or "was once a constant" (pays it — still correct,
    /// just no longer free to skip for THAT name).
    pub(crate) constant_var_names_seen: rustc_hash::FxHashSet<String>,
    /// ADR-0041 §9: hoist-pass sub registrations whose own in-sequence
    /// `RegisterDecl` has not executed yet, keyed by `Pkg::name`. A BEGIN-time
    /// region (`constant` initializer, `BEGIN`/`CHECK` body) rolls these back
    /// so a name reference evaluated there sees only what the program has
    /// textually reached, as rakudo's compile-time pad install does.
    pub(crate) hoisted_unreached_decls:
        rustc_hash::FxHashMap<Symbol, crate::runtime::hoist_visibility::HoistedDeclRecord>,
}

impl LexicalState {
    /// The main interpreter's state.
    // Cost: O(1).
    pub(crate) fn new() -> Self {
        Self {
            nested_capture_owners: Vec::new(),
            nested_method_captures: Default::default(),
            composed_nested_method_captures: Default::default(),
            our_vars: rustc_hash::FxHashMap::default(),
            our_var_unqualified: rustc_hash::FxHashSet::default(),
            process_dynamics: process_stash::ProcessStash::default(),
            hll_syms: rustc_hash::FxHashMap::default(),
            package_lexicals: std::sync::Arc::new(PackageLexicals::default()),
            class_body_static_names: Default::default(),
            unit_lexicals: std::sync::Arc::new(PackageLexicals::default()),
            mainline_lexical_subs: Default::default(),
            lexsub_free_aliases: Default::default(),
            lexsub_latest_cells: Default::default(),
            escaped_our_lexical_cells: ValueMap::default(),
            escaping_our_lexical_names: Default::default(),
            escaping_our_env_param_names: Default::default(),
            escaped_our_sub_names: Default::default(),
            our_scalar_cell_names: Default::default(),
            state_vars: HashMap::new(),
            state_vars_unmigrated: Vec::new(),
            hoist_pending_cells: Vec::new(),
            param_bound_aggregates: Default::default(),
            type_body_written_lexicals: Default::default(),
            closure_captured_state: HashMap::new(),
            var_dynamic_flags: HashMap::new(),
            var_bindings: HashMap::new(),
            atomic_var_seen: false,
            sigilless_alias_seen: false,
            readonly_vars: Box::new(std::cell::RefCell::new(
                crate::runtime::ReadonlySet::default(),
            )),
            readonly_undo: Box::new(std::cell::RefCell::new(Vec::new())),
            readonly_frames: Box::new(std::cell::Cell::new(0)),
            unit_lexical_gen: 0,
            readonly_rw_tail: None,
            state_scope_id: Box::new(std::cell::Cell::new(None)),
            pending_nested_state_scope: None,
            amp_param_shadowed_names: std::collections::HashSet::new(),
            frame_lexical_routines: Default::default(),
            frame_lexical_closure_bodies: Default::default(),
            block_declared_vars: crate::runtime::ScopeStack::new(),
            constant_var_names_seen: rustc_hash::FxHashSet::default(),
            hoisted_unreached_decls: rustc_hash::FxHashMap::default(),
        }
    }

    /// The state a spawned thread starts with: the program-wide tables
    /// (package/unit lexicals, escaping-`our` cells, lexical-sub aliases,
    /// frame-lexical routines, ...) carried over, mostly as `Arc` shares, and
    /// the per-execution state (`state` vars, readonly frames, block
    /// declarations) fresh.
    // Cost: O(c), c = entries of the tables cloned by value (the `Arc`-held
    // ones are refcount bumps).
    pub(crate) fn fork_for_thread(&self) -> Self {
        Self {
            nested_capture_owners: Vec::new(),
            nested_method_captures: Default::default(),
            composed_nested_method_captures: self.composed_nested_method_captures.clone(),
            our_vars: rustc_hash::FxHashMap::default(),
            our_var_unqualified: rustc_hash::FxHashSet::default(),
            // ADR-11318: one `PROCESS::` stash for the whole lineage.
            process_dynamics: self.process_dynamics.clone(),
            hll_syms: rustc_hash::FxHashMap::default(),
            package_lexicals: self.package_lexicals.clone(),
            class_body_static_names: self.class_body_static_names.clone(),
            unit_lexicals: self.unit_lexicals.clone(),
            mainline_lexical_subs: self.mainline_lexical_subs.clone(),
            lexsub_free_aliases: self.lexsub_free_aliases.clone(),
            lexsub_latest_cells: self.lexsub_latest_cells.clone(),
            escaped_our_lexical_cells: self.escaped_our_lexical_cells.clone(),
            escaping_our_lexical_names: self.escaping_our_lexical_names.clone(),
            escaping_our_env_param_names: self.escaping_our_env_param_names.clone(),
            escaped_our_sub_names: self.escaped_our_sub_names.clone(),
            our_scalar_cell_names: self.our_scalar_cell_names.clone(),
            state_vars: HashMap::new(),
            state_vars_unmigrated: Vec::new(),
            // Pending hoist cells belong to the parent's frames.
            hoist_pending_cells: Vec::new(),
            // The child re-binds its own env-bound parameters if it runs any.
            param_bound_aggregates: Default::default(),
            // A worker can instantiate a type registered on the parent, so the
            // set of method-written lexicals travels with the clone.
            type_body_written_lexicals: self.type_body_written_lexicals.clone(),
            // Mirror state_vars: a thread clone starts with no persisted
            // closure captured state (falls back to the captured-env initial
            // values), exactly as before this store existed.
            closure_captured_state: HashMap::new(),
            var_dynamic_flags: self.var_dynamic_flags.clone(),
            var_bindings: HashMap::new(),
            // Inherit monotonically: if the parent ever registered an atomic var,
            // the child (which shares the atomic storage via shared_vars) must keep
            // running the atomic-variable read check.
            // Inherit monotonically: the parent's sigilless-alias env keys are
            // copied into the child env, so the child must keep walking the chain.
            atomic_var_seen: self.atomic_var_seen,
            sigilless_alias_seen: self.sigilless_alias_seen,
            readonly_vars: Box::new(std::cell::RefCell::new(
                crate::runtime::ReadonlySet::default(),
            )),
            readonly_undo: Box::new(std::cell::RefCell::new(Vec::new())),
            readonly_frames: Box::new(std::cell::Cell::new(0)),
            unit_lexical_gen: 0,
            readonly_rw_tail: None,
            state_scope_id: Box::new(std::cell::Cell::new(None)),
            pending_nested_state_scope: None,
            amp_param_shadowed_names: std::collections::HashSet::new(),
            frame_lexical_routines: Arc::clone(&self.frame_lexical_routines),
            frame_lexical_closure_bodies: Arc::clone(&self.frame_lexical_closure_bodies),
            block_declared_vars: crate::runtime::ScopeStack::new(),
            constant_var_names_seen: rustc_hash::FxHashSet::default(),
            hoisted_unreached_decls: rustc_hash::FxHashMap::default(),
        }
    }
}
