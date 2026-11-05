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
    /// Whether an element store into the shared `@`/`%` container `key` goes
    /// through the `__mutsu_atomic_*` lane rather than the base-key entry: a
    /// plain lexical, or a dynamic (`@*a` / `%*h`).
    ///
    /// The read side (`GetLocal`'s lane probe in `vm_var_assign_local_get`)
    /// consults the atomic lane for EVERY `@`/`%` name and never the base-key
    /// entry on the main thread, so a store the base-key path takes is lost to
    /// the frame that owns the variable: `my %*h; await start {...};` then a
    /// closure (an inline `.then` callback) assigning `%*h<k>` cloned the
    /// base-key hash, and the owner kept reading its old one (#13772).
    // Cost: O(1).
    pub(crate) fn routes_through_atomic_lane(key: &str) -> bool {
        Interpreter::is_plain_lexical_name(key) || crate::env::is_dynamic_var_name(key)
    }

    /// Note `name` as explicitly updated in `shared_vars_dirty`. A store into
    /// a shared container runs this on every write, and the set already holds
    /// the name from the second write on, so look before building the owned
    /// `String` an insert needs.
    // Cost: O(l), l = name length (O(1) allocations only on the first note).
    pub(crate) fn note_dirty(&self, name: &str) {
        if self
            .shared_vars_dirty
            .read()
            .is_ok_and(|dirty| dirty.contains(name))
        {
            return;
        }
        if let Ok(mut dirty) = self.shared_vars_dirty.write() {
            dirty.insert(name.to_string());
        }
    }

    /// The main interpreter's state: the root of the shared store, sharing not
    /// yet active, nothing masked.
    // Cost: O(1).
    pub(crate) fn root() -> Self {
        Self {
            lock_async_recursion: Vec::new(),
            lock_async_deferred: Vec::new(),
            suppress_shared_publish: false,
            shared_vars: crate::runtime::shared_store::SharedStore::root(),
            shared_vars_active: false,
            sigilless_attrs_active: false,
            shared_vars_dirty: Arc::new(RwLock::new(HashSet::new())),
            shared_critical_dirty: Arc::new(RwLock::new(HashSet::new())),
            critical_section_depth: 0,
        }
    }

    /// The state a spawned thread starts with.
    // Cost: O(1).
    pub(crate) fn fork_for_thread(&self) -> Self {
        Self {
            lock_async_recursion: Vec::new(),
            lock_async_deferred: Vec::new(),
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
