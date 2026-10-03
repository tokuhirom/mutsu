//! The `threads` subsystem of ADR-10779: how variables are shared between
//! the interpreter of a thread and the threads it spawns (the `shared_vars`
//! store and its dirty sets, ADR-0010 / ADR-0039), which names a thread keeps
//! env-local instead, and the bookkeeping of `Lock::Async` and critical
//! sections.

use super::*;

pub(crate) struct ThreadSharing {
    /// Lock ids this caller chain has entered through
    /// `Lock::Async.protect-or-queue-on-recursion` (see
    /// `runtime::lock_async_recursion`). A spawned thread starts with an empty
    /// stack, which is precisely the "the lock is held by something outside the
    /// caller chain" case that method distinguishes.
    pub(crate) lock_async_recursion: Vec<u64>,
    /// Blocks queued by a *recursive* `protect-or-queue-on-recursion` call,
    /// drained by the outermost such frame once it has released the lock.
    /// Held here (rather than in a thread-local) so the queued `Value`s are
    /// enumerated by `visit_roots` while they wait.
    pub(crate) lock_async_deferred: Vec<(u64, Value, crate::value::SharedPromise)>,
    /// Names re-declared (`my $x` / `if ... -> $x`) in THIS thread while the
    /// cross-thread shared store is active. A re-declaration is a fresh
    /// binding shadowing the captured outer lexical, so subsequent writes to
    /// the name must stay thread-local: `set_shared_var_sym` skips the shared
    /// write and `sync_shared_vars_to_env` skips the pull for these names.
    /// Reset to empty in `clone_for_thread` (a child thread captures the
    /// parent's *current* bindings). Only populated while
    /// `shared_vars_active`; empty (zero-cost) for single-threaded programs.
    /// Boxed and wrapped in `RefCell` (not a plain `HashSet<String>` field) so
    /// `ThreadParamMaskGuard` (`vm::vm_call_state_guard`) can hold a raw
    /// pointer into this field's OWN heap allocation -- disjoint from
    /// `Interpreter`'s own allocation -- and mutate it on `Drop` (including
    /// during a Rust panic unwind) without ever needing a reference to
    /// `Interpreter` itself. See that module's doc comment ("v3") for why a
    /// pointer taken directly into a field embedded in `Interpreter`'s own
    /// struct is unsound. `RefCell` (not `Cell`, unlike `state_scope_id`/
    /// `when_matched`) because `HashSet` isn't `Copy`, so `Cell`'s get/set API
    /// is awkward for it; `RefCell` gives the same disjoint-allocation
    /// property while keeping ordinary `insert`/`remove`/`contains` methods
    /// available through `borrow`/`borrow_mut`.
    pub(crate) thread_redeclared_vars: Box<std::cell::RefCell<rustc_hash::FxHashSet<String>>>,
    /// Subset of [`Self::thread_redeclared_vars`] whose declaration is still
    /// *in flight*: the `my` has run but its initializer has not stored a value
    /// yet, so neither the slot nor `env` holds the new binding — both still
    /// carry the shadowed OUTER value.
    ///
    /// `clone_for_thread` normally drops a re-declaration mask because it
    /// force-seeds the name's *current* value into the child lineage first. That
    /// premise fails for a name in this set: a spawn that happens **inside the
    /// initializer** (`my $tap = Supply.tap(...)`, whose `.tap` starts a worker)
    /// would seed the outer binding's value and then unmask the name, so the
    /// next `sync_shared_vars_to_env` pulls that stale value back over the
    /// binding the initializer is about to create. Keeping the mask for the
    /// in-flight window closes that hole; the store is republished normally once
    /// the initializer's value lands. Empty for single-threaded programs.
    pub(crate) thread_decl_in_flight: std::collections::HashSet<String>,
    /// Plain-lexical `@`/`%` names this frame's spawns put on the bare-name
    /// cross-thread lane **only because every spawn publishes every live
    /// container**, not because any spawned block actually names them
    /// (ADR-0039 §8.6).
    ///
    /// Such an entry is needed only for as long as a worker might reach the
    /// container *indirectly* — through a routine the block calls rather than
    /// names. Once the next cross-thread drain (`sync_shared_vars_to_env`) has
    /// merged whatever the workers did back into `env`, it has served its whole
    /// purpose, and keeping it is what let a callee's own `my @items` outlive
    /// its frame in a process-visible, bare-name-keyed store and hijack an
    /// unrelated caller's same-named binding. So the drain withdraws them.
    ///
    /// A name a later spawn's block DOES reference is removed from this set at
    /// that spawn: it is then a genuinely shared container and keeps the lane.
    /// Empty for single-threaded programs.
    pub(crate) transient_lane_containers: std::collections::HashSet<String>,
    /// Bare scalar names currently masked in [`Self::thread_redeclared_vars`]
    /// because of a **parameter binding** (`mask_thread_redeclared_params`),
    /// not a `my` declaration. `clone_for_thread_excluding` must treat the two
    /// differently: a `my` re-declaration's mask means "this spawn should see
    /// MY new value as authoritative for the rest of the block", so it force-
    /// `declare`s the value into the shared lineage. A parameter's shadow is
    /// scoped to exactly this call and must never overwrite an unrelated
    /// caller's live entry for the same bare name — it should always take the
    /// `seed_if_absent` (no-op-if-already-visible) branch instead, even for a
    /// nested spawn *inside this call's own body*.
    ///
    /// `thread_decl_in_flight` looked like the same "always seed_if_absent"
    /// signal, but it is unsuitable here: `exec_set_local_op` clears an entry
    /// from it as soon as ANY `SetLocal` targets a same-named slot — which the
    /// call body's own bytecode does routinely (e.g. a coercion or a
    /// re-assignment of the parameter), silently un-suppressing the force-
    /// `declare` behavior partway through the call before any nested spawn.
    /// A dedicated set, touched only by
    /// [`mask_thread_redeclared_params`](Interpreter::mask_thread_redeclared_params) /
    /// `unmask_thread_redeclared_params`,
    /// has no such interference. Empty for single-threaded programs.
    /// Same `Box<RefCell<...>>` wrapping and same reason as
    /// [`Self::thread_redeclared_vars`] -- `ThreadParamMaskGuard` needs a
    /// stable, `Interpreter`-disjoint pointer into this field too.
    pub(crate) thread_param_shadow_vars: Box<std::cell::RefCell<rustc_hash::FxHashSet<String>>>,
    /// Set while an *incidental* locals -> env mirror is running: the regex
    /// interpolation pre-sync before a `~~`. It exists purely so a name-based
    /// reader in THIS interpreter can observe the frame's live slots through
    /// `env`. (The I/O ops' own pre-sync was dropped by #9169: a `$*OUT`
    /// override or a user `.gist` reads its free variables the way any method
    /// body does, through the per-store mirror.)
    ///
    /// `set_env_with_main_alias` does double duty: it writes `env` AND publishes
    /// to the cross-thread shared store. Publishing from such a mirror is wrong,
    /// because the store is keyed by BARE NAME while the mirror walks *whichever
    /// frame happens to be printing*: a callee's parameter `$url` overwrote the
    /// lane belonging to the caller's own `my $url`, and the caller's next
    /// `sync_shared_vars_to_env` pulled it back — `Cro::HTTP::Client.get("$url/")`
    /// grew a `/` on the caller's URL on every request, so the third server on a
    /// port answered 404.
    ///
    /// Frame *teardown* (`sync_env_from_locals`) is deliberately NOT suppressed;
    /// see the comment there.
    pub(crate) suppress_shared_publish: bool,
    /// Cross-thread lexical store for THIS spawn lineage (ADR-0010). `start`
    /// and friends give the child a store chained to this one, so a child sees
    /// and can write the parent's lexicals while its own declarations stay
    /// private to it — sibling threads (e.g. hyper workers each declaring
    /// `my $uri`) cannot clobber each other, which one process-global bare-name
    /// map allowed.
    pub(crate) shared_vars: Arc<crate::runtime::shared_store::SharedStore>,
    /// True when this interpreter participates in cross-thread variable sharing.
    /// Set by `clone_for_thread` on both parent and child.
    pub(crate) shared_vars_active: bool,
    /// True once any sigilless attribute alias (`has $x`) has been materialized.
    /// Sigilless attributes are read/written through a bare `Var("x")` that is
    /// disambiguated only by the runtime `__mutsu_sigilless_alias::` table, so
    /// the cell-direct read/write routing must consult that table. This flag
    /// gates that extra lookup so programs without sigilless attributes (the vast
    /// majority) pay nothing on the hot variable-read path. Process-sticky: set
    /// true on first use, never reset (Phase 3 Stage 2c (ii)).
    pub(crate) sigilless_attrs_active: bool,
    /// Keys in shared_vars that were explicitly updated (not just initialized by
    /// `clone_for_thread`). `sync_shared_vars_to_env` only syncs these keys so
    /// that function parameters aren't overwritten with stale values.
    pub(crate) shared_vars_dirty: Arc<RwLock<HashSet<String>>>,
    /// Keys in shared_vars that were written by some thread *while it held a
    /// critical section* (Semaphore/Lock). Entering a critical section syncs
    /// exactly these scalars back into the local env, so a bare
    /// read-modify-write of a shared accumulator (`$s.acquire; $r += $i;
    /// $s.release`) reads the value the previous holder committed — while a
    /// per-iteration loop lexical (`my $i = $_`, written outside any critical
    /// section) keeps this thread's own captured snapshot.
    pub(crate) shared_critical_dirty: Arc<RwLock<HashSet<String>>>,
    /// Depth of nested critical sections (Semaphore/Lock) this interpreter
    /// currently holds. Writes performed while > 0 mark `shared_critical_dirty`.
    pub(crate) critical_section_depth: usize,
}

impl ThreadSharing {
    /// The main interpreter's state: the root of the shared store, sharing not
    /// yet active, nothing masked.
    // Cost: O(1).
    pub(crate) fn root() -> Self {
        Self {
            lock_async_recursion: Vec::new(),
            lock_async_deferred: Vec::new(),
            thread_redeclared_vars: Box::new(std::cell::RefCell::new(
                rustc_hash::FxHashSet::default(),
            )),
            thread_decl_in_flight: std::collections::HashSet::new(),
            transient_lane_containers: std::collections::HashSet::new(),
            thread_param_shadow_vars: Box::new(std::cell::RefCell::new(
                rustc_hash::FxHashSet::default(),
            )),
            suppress_shared_publish: false,
            shared_vars: crate::runtime::shared_store::SharedStore::root(),
            shared_vars_active: false,
            sigilless_attrs_active: false,
            shared_vars_dirty: Arc::new(RwLock::new(HashSet::new())),
            shared_critical_dirty: Arc::new(RwLock::new(HashSet::new())),
            critical_section_depth: 0,
        }
    }

    /// The state a spawned thread starts with. `captured_scalars` are the
    /// spawned block's own captured scalar names.
    // Cost: O(c + s), c = captured scalars, s = the parent's active parameter
    // shadows.
    pub(crate) fn fork_for_thread(&self, captured_scalars: &HashSet<String>) -> Self {
        Self {
            lock_async_recursion: Vec::new(),
            lock_async_deferred: Vec::new(),
            // The spawned block's own captured scalars were NOT seeded into the
            // store (the closure machinery owns them per binding), so the child
            // must treat them exactly like re-declared names: reads and writes
            // stay env-local, and a stale same-named ancestor entry must not be
            // pulled over the captured copy at a sync point.
            //
            // A name the PARENT currently has masked as a slurpy `@`/`%`
            // parameter (`thread_param_shadow_vars`) must carry the same
            // treatment into the child: the child's env was just cloned from
            // the parent's, so it already holds THIS call's own value under
            // that bare name. Without inheriting the mask here, the child's
            // read gate (`container_name_is_redeclared`) sees the name as
            // unmasked and falls through to the shared-store fallback, which
            // may hold a DIFFERENT, now-stale value seeded by an earlier
            // sequential call to the same routine (the mask itself is lifted
            // on the parent as soon as that call's synchronous body returns,
            // long before an async `start` block spawned from inside it gets
            // around to reading the parameter — see
            // `slurpy-hash-param-in-start-block-reads-stale-value-across-
            // sequential-calls.md`). The mask must instead survive for as
            // long as the CHILD's own body runs, independent of the parent's
            // lifetime.
            thread_redeclared_vars: Box::new(std::cell::RefCell::new(
                captured_scalars
                    .iter()
                    .cloned()
                    .chain(self.thread_param_shadow_vars.borrow().iter().cloned())
                    .collect(),
            )),
            // The child starts no declaration of its own; its own `my`s populate
            // this as they run.
            thread_decl_in_flight: std::collections::HashSet::new(),
            // ADR-0039 §8.6: withdrawal is the *parent's* bookkeeping — the
            // child must not retire an entry it depends on. Its own spawns
            // populate this as they run.
            transient_lane_containers: std::collections::HashSet::new(),
            // The child starts no call of its own, but it inherits the
            // parent's currently-active parameter shadows (see the
            // `thread_redeclared_vars` comment above) — its own subsequent
            // parameter bindings union in as they run.
            thread_param_shadow_vars: Box::new(std::cell::RefCell::new(
                self.thread_param_shadow_vars.borrow().clone(),
            )),
            suppress_shared_publish: false,
            // ADR-0010: a child lineage, not a share of one process-wide map.
            shared_vars: crate::runtime::shared_store::SharedStore::child_of(&self.shared_vars),
            shared_vars_active: true,
            sigilless_attrs_active: self.sigilless_attrs_active,
            shared_vars_dirty: Arc::clone(&self.shared_vars_dirty),
            shared_critical_dirty: Arc::clone(&self.shared_critical_dirty),
            critical_section_depth: 0,
        }
    }
}
