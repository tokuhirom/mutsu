use super::*;
use crate::meta_ns::MetaNs;
use crate::runtime::shared_store::atomic_lane_str_key;

impl Interpreter {
    /// The scalar lexicals a spawned block captures itself, as bare env keys.
    ///
    /// These are exactly the names the closure machinery already owns per
    /// binding, so `clone_for_thread_for_block` keeps them out of the
    /// name-keyed `shared_vars` lane (PLAN.md §6). A name the block does NOT
    /// capture is invisible to that analysis — a `submethod DESTROY`, a
    /// `closing => {...}` handler, or any other separately-registered routine
    /// that closes over an outer lexical and runs on the worker — so it keeps
    /// using the name lane.
    fn block_captured_scalars(&self, block: &Value) -> std::collections::HashSet<String> {
        let mut out = std::collections::HashSet::new();
        if let ValueView::Sub(data) = block.view()
            && let Some(cc) = data.compiled_code.as_ref()
        {
            for sym in &cc.free_var_syms {
                let name = sym.resolve();
                if name.starts_with('@') || name.starts_with('%') || name.starts_with('&') {
                    continue;
                }
                let bare = name.trim_start_matches('$');
                // A name a registered class/role method writes keeps the name
                // lane: that write is invisible to the capture analysis, so the
                // closure machinery does NOT own the name after all. Applies
                // whether or not the local got a cell — roast
                // S12-construction/roles-6e.t's `$order` holds a List, a shape
                // `box_captured_lexicals` declines to box at all.
                if self.lexicals.type_body_written_lexicals.contains(bare) {
                    continue;
                }
                // ADR-0023: a name currently bound as a for-loop parameter is a
                // fresh, readonly, per-iteration binding — the spawn-time env
                // clone is its correct per-binding home for ANY value type.
                // Keeping it off the lane is what lets two sibling iterations'
                // spawns each hold their own value
                // (todo/deep/concurrent-for-loop-siblings-...).
                if self
                    .topic_state
                    .active_loop_param_names
                    .iter()
                    .any(|s| s.contains(bare))
                {
                    out.insert(bare.to_string());
                    continue;
                }
                // The same holds for any other readonly binding live in the
                // spawning frame — a routine, method or pointy-block parameter
                // (`-> $job { start { await $g; $job.id } }`): it can never be
                // reassigned, so the spawn-time clone cannot miss a write, and
                // leaving it on the lane let an unrelated same-named lexical (a
                // caller's loop `my $job`) be pulled over it at the worker's
                // next sync point. `mask_thread_redeclared_params` covers this
                // only once the shared store is active; the first spawn of a
                // program happens before that, so the mask alone misses it.
                if self.is_readonly(&name) || self.is_readonly(bare) {
                    out.insert(bare.to_string());
                    continue;
                }
                // Only a genuinely PLAIN scalar is owned per binding by the
                // closure machinery — either boxed into a shared cell by
                // `box_captured_lexicals` or correctly frozen by value. An
                // ALLOW-list, deliberately: everything else (a Channel, a
                // Promise, a Lock, an Array/List/Hash, a Sub, a type object, ...)
                // is a shape `box_captured_lexicals` declines to box, so the name
                // lane is still the only thing keeping the parent and the worker
                // on ONE object. Dropping it for those hangs
                // `my $c = Channel.new; start { $c.send(42) }; $c.receive`
                // (pin: t/concurrency-threading.t test 4).
                let plain = self
                    .env
                    .get(bare)
                    .or_else(|| self.env.get(&name))
                    .is_some_and(|v| {
                        matches!(
                            v.view(),
                            ValueView::Int(_)
                                | ValueView::BigInt(_)
                                | ValueView::Num(_)
                                | ValueView::Str(_)
                                | ValueView::Bool(_)
                                | ValueView::Rat(..)
                                | ValueView::FatRat(..)
                                | ValueView::BigRat(..)
                                | ValueView::Complex(..)
                                | ValueView::ContainerRef(_)
                        )
                    });
                if !plain {
                    continue;
                }
                out.insert(bare.to_string());
            }
            // An `@`/`%` free variable that the env-level parameter binding path
            // bound — a destructuring sub-signature (`-> [$a, @K]`) or a
            // runtime-invoked callback's plain parameter (`reduce -> $h,
            // @words`) — is a fresh per-invocation binding, not the one shared
            // object the name lane represents. Left on the lane it is seeded
            // once and frozen at the first spawn's value, so
            // `map -> [$a, @K] { start { @K[0] } }, ...` and
            // `reduce -> $h, @words { $h + await start { [+] @words } }, ...`
            // had every worker read the first binding's value.
            //
            // What narrows this to exactly that case is that the container
            // `param_bound_aggregates` recorded for the name must be the SAME
            // container the env currently holds — so an unrelated outer
            // aggregate that merely shares the name (or a later `my` re-binding
            // of it), and its `__mutsu_atomic_*` CAS copies, keep the lane
            // exactly where `docs/recursive-start-shared-vars.md` requires.
            //
            // This deliberately does NOT also require the free variable to
            // resolve to no parent slot. That extra condition once stood in for
            // "the env-level binder is the only parameter path that writes
            // `env` without a local slot behind it", but the property that
            // matters is being a PARAMETER binding, not how it is stored: a
            // slot-bound `@`/`%` parameter (`sub f(@P) { start { @P[0] } }`, and
            // now `-> @P { start { @P[0] } }` too, since a one-parameter pointy
            // block keeps its sigil and gets a real `ParamDef`) is just as
            // fresh per invocation, and leaving it on the once-seeded name lane
            // froze every worker at the first call's argument.
            //
            // Nor does it require the block to NAME the parameter (#10031).
            // A parameter the block never mentions was still seeded, as an
            // ADR-0039 §8.6 transient entry, and the spawning frame's own later
            // `%g{$k} = ...` then saw the name in the store and took the atomic
            // hash lane, which writes a COPY and rebinds `%g` to it — detaching
            // the parameter from the caller's container it aliases:
            // `sub mk(%g) { -> $k { %g{$k} = 1; start { 1 } } }` lost every
            // store after the first spawn. Being a per-invocation binding does
            // not depend on who names it, and the parameter's own container is
            // already what every alias holds, so no lane is needed to reach it.
            // The frame's env may hold the parameter boxed in the closure
            // machinery's `ContainerRef` cell, so compare through it.
            //
            // Every live binding of the name counts, not just the latest one:
            // two closures made by one routine, each capturing its `%g` bound
            // to a different caller hash, both hold a parameter binding, and
            // letting the older one onto the lane merged both closures' stores
            // into one copy (#10076). `ParamBoundAggregates` records them
            // weakly, so it neither pins arguments alive nor mistakes a reused
            // address for a recorded one.
            for name in self.lexicals.param_bound_aggregates.names() {
                if self
                    .env
                    .get(name)
                    .is_some_and(|cur| self.lexicals.param_bound_aggregates.holds(name, cur))
                {
                    out.insert(name.clone());
                }
            }
        }
        out
    }

    /// The plain-lexical `@`/`%` containers a spawned block's compiled subtree
    /// actually names, or `None` when the spawn has no known block (the
    /// block-less [`clone_for_thread`](Self::clone_for_thread) entry point,
    /// used by supply drivers, `.then`, socket and proc readers).
    ///
    /// ADR-0039 §8.6 uses this to decide *how long* a container's bare-name
    /// lane entry is needed, NOT whether to create one. A container the block
    /// names is genuinely shared and keeps the lane for good. A container it
    /// never names still gets an entry — a routine the block *calls* may reach
    /// it, which no analysis over the block's own free variables can see — but
    /// that entry is marked transient and retired at the next cross-thread
    /// drain. `free_var_syms` already folds up nested closures
    /// (`compute_free_vars`, "Fold nested closures"), so
    /// `start { start { @a.push(1) } }` keeps `@a` permanently.
    fn block_referenced_containers(block: &Value) -> Option<std::collections::HashSet<String>> {
        let mut out = std::collections::HashSet::new();
        if let ValueView::Sub(data) = block.view()
            && let Some(cc) = data.compiled_code.as_ref()
        {
            for sym in cc
                .free_var_syms
                .iter()
                .chain(cc.free_var_writes.iter())
                .chain(cc.free_var_container_writes.iter())
            {
                let name = sym.resolve();
                if name.starts_with(['@', '%']) {
                    out.insert(name);
                }
            }
            return Some(out);
        }
        None
    }

    /// Whether `key` is a plain-lexical `@`/`%` name the ADR-0039 §8.6
    /// transient-lane rule applies to. Twigil'd, dynamic and attribute
    /// containers (`@!x`, `@*y`, `%?RESOURCES`) are not plain lexicals and keep
    /// their existing lifetimes — `@*x`/`%*x` in particular depend on a durable
    /// name-lane entry for their cross-thread propagation (see the
    /// aggregate-dynamic carve-out in the seeding loop). The anonymous
    /// container slot names and `::`-qualified names are excluded for the same
    /// reason `collect_unit_lexical_names` excludes them.
    pub(crate) fn transient_lane_candidate(key: &str) -> bool {
        Self::is_plain_lexical_name(key)
            && !key.contains("__ANON")
            && !crate::qualified::is_qualified_str(key)
    }

    /// Union a frame's `type_body_written_lexicals` into the interpreter-wide
    /// set. Called at `RegisterClass` / `RegisterRole`, which always run before
    /// the type can be instantiated (and so before any of its methods can run).
    pub(crate) fn note_type_body_written_lexicals(&mut self, code: &CompiledCode) {
        for sym in &code.type_body_written_lexicals {
            let name = sym.resolve();
            crate::runtime::cow_table_mut(&mut self.lexicals.type_body_written_lexicals)
                .insert(name.trim_start_matches('$').to_string());
        }
    }

    /// [`Self::clone_for_thread`] for a spawn that runs a known block (`start { ... }`,
    /// `Promise.start`, `Thread.start`): the block's own captured scalars are
    /// excluded from the name-keyed shared store. See `block_captured_scalars`.
    pub(crate) fn clone_for_thread_for_block(&mut self, block: &Value) -> Self {
        let captured = self.block_captured_scalars(block);
        // ADR-0039 §8.6: which containers this block names decides whether its
        // lane entry is durable or transient. See `block_referenced_containers`.
        let containers = Self::block_referenced_containers(block);
        self.clone_for_thread_excluding(&captured, containers.as_ref())
    }

    /// Create a lightweight clone of this interpreter for use in a spawned thread.
    /// Shares function/class/role/enum definitions but starts with fresh output and test state.
    /// Array (`@`) and scalar (`$`) variables are shared between parent and child via `shared_vars`
    /// so that mutations are visible across threads.
    pub(crate) fn clone_for_thread(&mut self) -> Self {
        self.clone_for_thread_excluding(&std::collections::HashSet::new(), None)
    }

    /// The workhorse behind every thread clone.
    ///
    /// Most of what this builds is cheap, and deliberately so: the program-global
    /// symbol tables it carries over are `Arc<...>` copy-on-write shares (see the
    /// note on [`Interpreter`]), so cloning them is a refcount bump rather than a
    /// deep copy of every `String` key in the program. Before that, a spawn cost
    /// grew with the size of the loaded program instead of with the work: with
    /// Cro's stack loaded, the identical `Promise(supply { whenever ... })` that
    /// `Cro::MessageWithBody.body-blob` builds cost 5x what it costs in a bare
    /// program, purely because there were more modules to copy tables for.
    fn clone_for_thread_excluding(
        &mut self,
        captured_scalars: &std::collections::HashSet<String>,
        referenced_containers: Option<&std::collections::HashSet<String>>,
    ) -> Self {
        // A worker has no mainline callframe of its own. Preserve the spawning
        // source location separately so an anonymous callback can still expose
        // a useful bottom backtrace frame without duplicating a located entry
        // block (see `thread_origin_frame`).
        let thread_spawn_origin = self
            .current_source_file_sym()
            .zip(self.current_source_line());
        // A thread spawned BEFORE the first test call must still share the TAP
        // counter: the first `ok` of the whole program can run on the spawned
        // thread (e.g. supply-tap check closures in Cro's HTTP/2 serializer
        // tests, where every assertion of the first `test()` call fires inside
        // the tap). Without a pre-created `TestState`,
        // `TapState::clone_for_thread` has nothing to share; the child then
        // lazily creates a private state whose increments never reach the
        // parent, and the parent restarts numbering at 1 — "Tests out of
        // sequence" under prove. Gate on the Test module being loaded: an
        // empty `TestState` flips `test_mode_active`, which would otherwise
        // change bare-word resolution for non-Test programs that spawn threads.
        if self.test_module_loaded() && !self.io.tap.active() {
            // `clone_for_thread` below shares the counter of an EXISTING state.
            self.io.tap.ensure_state();
        }
        // Collapse a scoped (multi-tier overlay) env to a flat one first: the
        // shared-var seeding and the child's env clone below iterate the env
        // overlay-only, which would miss parent-chain lexicals on a scoped env.
        if self.env.is_scoped() {
            self.env = self.env.flattened();
        }
        // Copy user variables into shared_vars so both parent and child see mutations.
        // The compiler stores locals with bare names (no sigil), so we share everything
        // except internal/special variables that should remain thread-local.
        // ADR-0010: seed into THIS lineage's store. The parent's lexicals belong
        // to the parent's lineage; the child (created below) chains to it, so it
        // sees them and its writes resolve back here. A sibling thread seeds into
        // its OWN store, which is why two hyper workers that each declare
        // `my $uri` no longer collide on one bare-name entry.
        let shared = Arc::clone(&self.threads.shared_vars);
        // Merged with the lineage-seeding walk below (was a separate `for value
        // in self.env.values()` pass): both need one full traversal of the
        // parent env per spawn, so collecting handle ids here — BEFORE any of
        // the seeding loop's `continue`s — does both jobs in one pass. Every
        // value must still be checked for a handle id even when the seeding
        // side skips it (e.g. `$*CWD`/`self`/`__mutsu_*` are never handles, but
        // an excluded captured scalar or an already-visible name legitimately
        // could be).
        let mut referenced_handle_ids = std::collections::HashSet::new();
        // The built-in dynamics live in the per-interpreter base tier, which
        // `&self.env` (map-only) does not yield (ADR-0086) — but `$*OUT` /
        // `$*ERR` / `$*IN` / `$*ARGFILES` are exactly the handles the child
        // must be able to keep using, so collect their ids explicitly. The
        // seeding walk below deliberately skips these names anyway (a dynamic
        // is thread-local), so only the handle-id half applies to them.
        if let Some(base) = self.env.dyn_base() {
            for val in base.values() {
                if let Some(id) = Self::handle_id_from_value(val) {
                    referenced_handle_ids.insert(id);
                }
            }
        }
        {
            // Slice 5 step A instrumentation: how many env entries this walk
            // visits vs. how many actually land in the store. Accumulated
            // locally and recorded once per spawn (one atomic add per counter).
            let mut seed_keys_walked: u64 = 0;
            let mut seed_inserts: u64 = 0;
            // ADR-0039 §8.6 classification, collected while `self.env` is
            // borrowed by the walk and applied to `self` once it ends.
            //
            // Only the top-level interpreter classifies. On a WORKER thread the
            // lane is not an optional publication channel that a container
            // might or might not need — it is the storage: `push @a, ...`
            // routes through `__mutsu_atomic_arr::` unconditionally when
            // `is_thread_clone()` (`vm/vm_data_push_ops.rs`), precisely so
            // concurrent appends serialize, and `shared_array_mutate` then
            // drops the worker's own `env` copy so `make_mut` can work in
            // place. Retiring an entry there would be withdrawing a deliberate
            // mechanism's backing store mid-use (measured: it emptied worker A's
            // accumulator in `t/sibling-thread-array-lane-scope.t`). It also
            // buys nothing: a worker's lineage store is its own (ADR-0010), so
            // its entries cannot outlive into an unrelated frame the way a
            // root-store entry published by the main interpreter does — which
            // is the collision §8.2 records.
            let referenced_containers = referenced_containers.filter(|_| !self.is_thread_clone());
            let mut transient_marks: Vec<String> = Vec::new();
            let mut transient_unmarks: Vec<String> = Vec::new();
            for (key, val) in &self.env {
                seed_keys_walked += 1;
                Self::collect_handle_ids(val, 3, &mut referenced_handle_ids);
                // Skip internal variables and topic variables.
                // Also skip $*CWD/*CWD — in Raku, dynamic variables like $*CWD
                // are thread-local; mutations inside `start` blocks must not
                // propagate back to the parent thread.
                // `self` is a per-invocation binding, never a shared mutable
                // variable: seeding it lets one start-block's invocant leak into
                // a sibling thread's env via the await-time shared-var sync
                // (Cro::CompositeConnector read another connector's attributes).
                // `?`-prefixed compile-time pseudo-lexicals (?CLASS, ?ROLE,
                // ?LINE, ...) are per-scope constants with the same problem.
                if key == "_"
                    || key == "@_"
                    || key == "%_"
                    || key == "/"
                    || key == "!"
                    || key == "$/"
                    || key == "$!"
                    || key == "$*CWD"
                    || key == "*CWD"
                    || key == "self"
                    || key.starts_with("__mutsu_")
                    || key.starts_with("&")
                    || key.starts_with("?")
                    // Every SCALAR dynamic variable (`*x` / `$*x`), not just
                    // `$*CWD`: dynamics are thread-local in Raku, so a `start`
                    // block must not seed the parent frame's binding into the
                    // lineage-shared store, or it leaks process-wide after the
                    // frame returns (the child env is a clone of the parent's,
                    // so a spawned worker still reads the current dynamic fine
                    // without this name-lane sharing; and a block that closes
                    // over the dynamic gets it via the normal captured-scalar
                    // cell below regardless).
                    //
                    // `@*x`/`%*x` aggregate dynamics are deliberately NOT
                    // excluded here: unlike scalars, aggregates have no
                    // cell-based closure capture in this codebase — their
                    // cross-thread mutation visibility (e.g. `.then`
                    // callbacks chained onto the SAME promise, roast
                    // S17-promise/then.t's `@*FOO`) depends entirely on this
                    // name lane's `__mutsu_atomic_*` CAS mechanism, same as
                    // any other captured aggregate (see the comment on that
                    // exclusion below). Excluding them here would silently
                    // break that live-chain propagation, not just the leak.
                    || (key.with_str(crate::env::is_dynamic_var_name)
                        && !key.starts_with("@")
                        && !key.starts_with("%"))
                    // An attribute alias (`!x`, `@!x`, `%!x`) is a view of
                    // the invocant's storage, not a lexical of this frame: the
                    // instance is already shared, so seeding a snapshot of the
                    // alias into the lineage store only shadows it — an inline
                    // `$!lock.protect: { @!x ... }` in the spawning method then
                    // read the spawn-time copy and lost every write another
                    // thread made to the attribute since (#8380's
                    // `Test::Scheduler.run-due` dropping a `FutureEvent`).
                    || key.with_str(is_attribute_alias_name)
                {
                    continue;
                }
                let key = key.resolve();
                // A scalar the spawned block captures itself is NOT shared by
                // name. `start` compiles its block as escaping
                // (`compiler/expr_call.rs`), so the closure machinery already
                // gives such a scalar a per-binding home: a shared
                // `ContainerRef` cell from `box_captured_lexicals` when it is
                // mutated, a frozen value when it is read-only. The store is
                // keyed by the BARE NAME, so it cannot represent two
                // concurrently-live bindings of the same name — exactly what a
                // recursive frame chain is (`sub f($n) { start { ... f($n-1)
                // ... } }`). Seeding those names here ran a second, lossy
                // mechanism in parallel with the working one and silently
                // overwrote its correct answer (PLAN.md §6).
                // `@`/`%` aggregates keep this lane by default: their
                // `__mutsu_atomic_*` CAS copies are keyed off these entries. The
                // one exception is an aggregate bound by the env-level parameter
                // binding path (a destructuring sub-signature, or a
                // runtime-invoked callback's plain parameter), which
                // `block_captured_scalars` puts in this set *with* its sigil — a
                // fresh per-invocation binding the lane cannot represent (it
                // would freeze at the first spawn's value).
                if !captured_scalars.is_empty()
                    && (!key.starts_with('@') && !key.starts_with('%')
                        || captured_scalars.contains(key.as_str()))
                    && captured_scalars.contains(key.trim_start_matches('$'))
                {
                    continue;
                }
                // Only seed if not already visible — an entry an earlier thread
                // already updated must not be reset to this env's copy.
                // EXCEPT names this thread re-declared: their entry (if any)
                // belongs to the shadowed outer binding, so bind the current one
                // into this lineage, shadowing the ancestor's.
                // A declaration still in flight has no "current" value to bind:
                // `val` is the binding this `my` is about to shadow, so publishing
                // it would overwrite the lane with a value that is about to be
                // stale. Seed it only to give the child *something* under the name
                // (it captured the outer binding), and keep the mask below.
                //
                // A name masked because of a *parameter* binding
                // (`thread_param_shadow_vars`, see `mask_thread_redeclared_params`)
                // never takes the force-`declare` branch, even outside any
                // in-flight window: the parameter's shadow is scoped to exactly
                // the call that bound it, not "the rest of this block", so a
                // nested spawn inside that call must not overwrite an unrelated
                // caller's live entry for the same bare name.
                let published = if self.threads.thread_redeclared_vars.borrow().contains(&key)
                    && !self.threads.thread_decl_in_flight.contains(&key)
                    && !self
                        .threads
                        .thread_param_shadow_vars
                        .borrow()
                        .contains(&key)
                {
                    shared.declare(&key, val.clone());
                    seed_inserts += 1;
                    true
                } else if shared.seed_if_absent(&key, || val.clone()) {
                    seed_inserts += 1;
                    true
                } else {
                    false
                };
                // ADR-0039 §8.6: classify the lane entry. A container this
                // block NAMES is genuinely shared and keeps its entry for good
                // — including promoting one an earlier spawn had marked
                // transient. A container it never names, whose entry THIS spawn
                // created, was published only because this loop publishes
                // everything, so mark it for withdrawal at the next cross-thread
                // drain.
                //
                // Marking, rather than declining to seed, is deliberate and
                // measured: a routine the block CALLS can still reach the
                // container, which no analysis over the block's own free
                // variables can see (see §8.6). Only marking entries this spawn
                // created matters just as much — an entry an earlier, naming
                // spawn established stays durable no matter how many unrelated
                // spawns walk past it afterwards.
                if let Some(refs) = referenced_containers
                    && Self::transient_lane_candidate(&key)
                {
                    if refs.contains(&key) {
                        transient_unmarks.push(key);
                    } else if published {
                        transient_marks.push(key);
                    }
                }
            }
            crate::vm::vm_stats::record_spawn_seeding(seed_keys_walked, seed_inserts);
            for key in transient_unmarks {
                self.threads.transient_lane_containers.remove(&key);
            }
            for key in transient_marks {
                self.threads.transient_lane_containers.insert(key);
            }
            // Track C: migrate the parent's `state` variables into shared cells.
            // Incremental: only the keys created since the previous spawn.
            self.seed_unmigrated_state_vars(&shared);
        }
        self.threads.shared_vars_active = true;
        // The child captures the parent's CURRENT bindings — including any
        // name the parent re-declared since an earlier spawn (its current
        // value was force-seeded above). From this spawn on, writes to those
        // names must flow both ways again, so drop the parent-side masks.
        //
        // EXCEPT the names excluded from seeding above: their premise ("its
        // current value was force-seeded") does not hold, so the mask must
        // survive. The store can hold a STALE entry under such a name — the
        // blanket `sync_env_from_locals` mirror writes a frame's declared-but-
        // not-yet-initialized slots (Nil) into the store, and the re-declaration
        // mask then blocks the post-assignment refresh. The old force-seed
        // repaired that bomb at every spawn; without it, dropping the mask lets
        // the next `await`'s `sync_shared_vars_to_env` pull the stale Nil back
        // over the live binding (`$port` in roast S17-promise/
        // nonblocking-await.t's socket server went Nil → connect to port 0).
        //
        // A name whose declaration is still IN FLIGHT is excluded for the same
        // reason from the other direction: the seed above took the value of the
        // binding this `my` is about to SHADOW, because the initializer that
        // produced this very spawn has not stored anything yet. Unmasking would
        // let the next `sync_shared_vars_to_env` pull that outer value back over
        // the new binding — `my $tap = IO::Socket::Async.listen(...).tap({...})`
        // in a loop reverted to the previous iteration's `Tap`, which is what
        // made a restarted `Cro::HTTP::Server` keep answering from the old one.
        //
        // A parameter-binding mask (`thread_param_shadow_vars`) survives this
        // pass unconditionally for the same reason as an in-flight `my`: the
        // shadow is scoped to the still-executing call, not to "since the last
        // spawn", so dropping it here (because the call didn't happen to be the
        // very first spawn) would let the next `sync_shared_vars_to_env` pull
        // the caller's unrelated value back over the parameter for the
        // remainder of the call. Explicit `unmask_thread_redeclared_params` at
        // the call's return is the only thing that ever clears it.
        self.threads
            .thread_redeclared_vars
            .borrow_mut()
            .retain(|n| {
                captured_scalars.contains(n.trim_start_matches('$'))
                    || self.threads.thread_decl_in_flight.contains(n)
                    || self.threads.thread_param_shadow_vars.borrow().contains(n)
            });
        let mut cloned = Self {
            types: self.types.fork_for_thread(),
            literal_native_args: 0,
            static_call_args: false,
            env: self.env.clone(),
            io: self.io.fork_for_thread(&referenced_handle_ids),
            control: self.control.fork_for_thread(),
            main_hidden_from_usage: self.main_hidden_from_usage.clone(),
            explicit_run_main: self.explicit_run_main,
            nested_mode: self.nested_mode,
            module: self.module.fork_for_thread(),
            dispatch: self.dispatch.fork_for_thread(),
            current_unit: self.current_unit,
            closures_created: 0,
            // Snapshot (fresh lock), not a shared handle: thread-local registry
            // semantics — child sees a copy, writes don't leak to the parent.
            current_package_sym: Arc::new(std::sync::atomic::AtomicU32::new(
                self.current_package_sym().id(),
            )),
            routine_stack: crate::runtime::routine_stack::RoutineStack::default(),
            callframe_stack: Vec::new(),
            pending_call_arg_sources: None,
            pending_where_exception: None,
            pending_skip_constraint_recheck: false,
            pending_raw_invocant: None,
            pending_call_arg_source_slots: std::collections::HashMap::new(),
            pending_rw_writeback_slots: std::collections::HashMap::new(),
            test_pending_callsite_line: None,
            nqp_arg_scratch: Vec::new(),
            cur_source_line: 1,
            thread_spawn_origin,
            args_scratch_pool: Vec::new(),
            regex_quant_scratch: Vec::new(),
            block_stack: Vec::new(),
            declarator_docs: declarator_docs::DeclaratorDocs::default(),
            topic_state: self.topic_state.fork_for_thread(),
            async_state: self.async_state.fork_for_thread(),
            block_scope_depth: self.block_scope_depth,
            pending_sigilless_store: None,
            regex_state: self.regex_state.fork_for_thread(),
            closure_env_overrides: self.closure_env_overrides.clone(),
            caches: self.caches.fork_for_thread(),
            pending_eval_sigilless: Vec::new(),
            pending_eval_placeholder_params: Vec::new(),
            pending_eval_rw_tail: false,
            pending_eval_context_routine: None,
            repl_compiler: Default::default(),
            pending_supply_block_body: false,
            pending_supply_emitter_sym: None,
            pending_supply_authoritative_free_vars: Vec::new(),
            pending_whenever_inherited_owned: Vec::new(),
            last_block_my_declared: Vec::new(),
            pending_runtime_name_writes: Vec::new(),
            threads: self.threads.fork_for_thread(captured_scalars),
            lexicals: self.lexicals.fork_for_thread(),
            in_lvalue_assignment: false,
            rw_return_context: false,
            trait_mod_writeback_key: None,
            trait_mod_writeback_value: None,
            trait_mod_attr_writeback_value: None,
            trait_mod_default_writeback: None,
            hash_autovivify: false,
            caller_env_stack: Vec::new(),
            raku_cycle_guards: self.raku_cycle_guards.fork_for_thread(),
            last_value: None,
            pending_local_updates: Vec::new(),

            // Merged VM execution registers (CP-3 collapse): a thread clone starts
            // with fresh per-execution registers, exactly as the former
            // `VM::new(thread_interp)` did for a spawned thread.
            stack: Vec::new(),
            locals: crate::runtime::locals::Locals::new(),
            trir: crate::trir::frame::TrStacks::default(),
            trir_outer_cache: rustc_hash::FxHashMap::default(),
            upvalues: Vec::new(),
            frame_authoritative: Vec::new(),
            frame_owned: Vec::new(),
            call_frames: Vec::new(),
            stack_check_countdown: 0,
            handler_fns_snapshot: None,
            current_code: 0,
            numeric_op_site: (0, 0),
            carrier_writes: None,
            resume_ip: None,
            #[cfg(feature = "jit")]
            jit_error: None,
            accessor_ref_pending: false,
            sigilless_bind_source: None,
            mark_ctx: Box::default(),
            array_share_active: false,
            element_share_pending: false,
            vardecl_init_raw: None,
            pending_rw_writeback_sources: Vec::new(),
            pending_caller_var_writeback: rustc_hash::FxHashSet::default(),
            inline_control_env_writes: Vec::new(),
            local_bind_pairs: Vec::new(),
            rw_param_rebinds: Vec::new(),
            call_ic: [crate::opcode::CallIcSlot::EMPTY; crate::opcode::CALL_IC_WAYS],
            pos_light_ic_epoch: 1,
            outer_scope_locals: Vec::new(),
            enter_result_stack: Vec::new(),
            pending_alias_bind_names: Vec::new(),
            nested_run_depth: 0,
        };
        // Raku gives each start block fresh $/ and $! (they are lexically scoped).
        cloned.env.insert("/".to_string(), Value::NIL);
        cloned.env.insert("!".to_string(), Value::NIL);
        cloned.env.insert("$/".to_string(), Value::NIL);
        cloned.env.insert("$!".to_string(), Value::NIL);
        cloned.init_io_environment_for_thread_clone();
        // The rebuild above writes the child's own `$*CWD`/`$*TMPDIR`/… into
        // its env map; hoist them straight back down into a base tier of the
        // child's own, so a closure created inside the spawned block captures
        // no more than one created on the parent would (ADR-0086). The child's
        // base starts as a copy of the parent's, so an inherited `$*OUT`
        // redirection is preserved and a later write on either side promotes
        // into that side's overlay only.
        cloned.hoist_builtin_dynamics();
        cloned.bootstrap_hll_syms();
        cloned
    }

    /// Push values into a shared array variable in-place, avoiding full-array
    /// clones on every push.  When the variable lives in `shared_vars` the
    /// lock is held for the entire read-modify-write so concurrent pushes are
    /// safe and O(1) amortised instead of O(n).
    pub(crate) fn push_to_shared_var(
        &mut self,
        key: &str,
        mut values: Vec<Value>,
        target_fallback: &Value,
    ) -> Value {
        // ADR-0039 slice 1: a compunit's own file-scope `@`/`%` (or a
        // mainline named sub's captured free variable) resolves through
        // `unit_lexicals` BEFORE `env` (`get_env_with_main_alias`'s doc
        // comment) — `env[key]` can hold a completely unrelated same-named
        // binding (e.g. the loading scope's own `my @items`, restored there
        // once the module's mainline finished running). Falling through to
        // the plain-env / `real_array` paths below for such a name would
        // either push onto that WRONG array, or replace the cell's contents
        // with a detached copy that the cell's own readers never see again
        // — the "miss -> construct -> insert" hazard this ADR is about.
        // Mutate the shared node in place instead (container mutation is
        // write-through-the-node, ADR-0013 §7) and return; no write-back is
        // needed since the cell and this dereferenced value share the same
        // `Gc<ArrayData>`. Excludes the anonymous-container slot names for
        // the same reason `collect_unit_lexical_names` does (they are never
        // actually placed in `unit_lexicals`, so this is defense in depth).
        if !key.contains("__ANON")
            && let Some(mut existing) = self.unit_lexical_container(key)
            && matches!(existing.view(), ValueView::Array(..))
        {
            return existing
                .with_array_mut(|arc_items, kind| {
                    let items = crate::value::gc_data_mut(arc_items);
                    items.extend(values);
                    if key.starts_with('@') && *kind == ArrayKind::List {
                        *kind = ArrayKind::Array;
                    }
                    Value::array_with_kind(crate::gc::Gc::clone(arc_items), *kind)
                })
                .unwrap();
        }
        let package_array = crate::qualified::is_package_array(Symbol::intern(key));
        if package_array
            && let Some(stored) = self
                .get_our_var(key)
                .cloned()
                .or_else(|| self.our_var_pseudo_unqualified(key))
        {
            // A prior call's env overlay has gone away. Restore the package
            // container before mutating so the same array kind and data survive.
            self.env.insert(key.to_string(), stored);
        }
        // A plain lexical `@name` already present in the shared store routes
        // through the atomic store (see `push_to_existing_shared_array`); when
        // absent it is thread-local and falls through to the env path below.
        // A name this lineage re-declared skips BOTH shared branches: the entry
        // under it belongs to the shadowed outer binding.
        if self.container_name_is_redeclared(key) {
            // fall through to the env path below
        } else if key.starts_with('@')
            && self.threads.shared_vars_active
            && Self::is_plain_lexical_array_name(key)
        {
            let in_shared = {
                let atomic_key = atomic_lane_str_key(key, false);
                self.threads
                    .shared_vars
                    .get(atomic_key)
                    .is_some_and(|v| matches!(v.view(), ValueView::Array(..)))
                    || self
                        .threads
                        .shared_vars
                        .get(key)
                        .is_some_and(|v| matches!(v.view(), ValueView::Array(..)))
            };
            if in_shared {
                return self.shared_array_extend(key, values, false);
            }
        } else if key.starts_with('@') && self.threads.shared_vars_active && !package_array {
            // Attribute / twigil'd arrays keep the base-key in-place path
            // (per-instance identity — see `push_to_existing_shared_array`).
            // Drop env's copy of the Arc first so that shared_vars holds
            // the only strong reference (refcount=1). This keeps repeated
            // shared pushes in-place instead of degenerating into O(n²) COW.
            self.env.remove(key);
            let is_thread_clone = self.is_thread_clone();
            // In-place read-modify-write under the owning lineage's lock, so
            // concurrent pushes stay safe and O(1) amortised instead of O(n).
            // Lend the values to the in-place attempt and take them back if the
            // closure never ran (the branch below can still fall through).
            let mut pending = Some(std::mem::take(&mut values));
            let in_place = self
                .threads
                .shared_vars
                .with_entry_mut(key, |v| {
                    v.with_array_mut(|arc_items, kind| {
                        let items = crate::gc::Gc::make_mut(arc_items);
                        items.extend(pending.take().unwrap_or_default());
                        if *kind == ArrayKind::List {
                            *kind = ArrayKind::Array;
                        }
                        Value::array_with_kind(crate::gc::Gc::clone(arc_items), *kind)
                    })
                })
                .flatten();
            if let Some(result) = in_place {
                self.mark_shared_var_dirty(key);
                if !is_thread_clone {
                    self.env.insert(key.to_string(), result.clone());
                }
                return result;
            }
            // `with_array_mut` did not run (absent, or not an Array yet), so the
            // values were never consumed — take them back.
            values = pending.take().unwrap_or_default();
            // Fallback: the value might exist but not be an Array yet
            if let Some(shared_value) = self.threads.shared_vars.get(key)
                && matches!(shared_value.view(), ValueView::Array(..))
            {
                {
                    let (mut arc_items, kind) = shared_value.into_array().unwrap();
                    let items = crate::gc::Gc::make_mut(&mut arc_items);
                    items.extend(values);
                    let normalized_kind = if kind == ArrayKind::List {
                        ArrayKind::Array
                    } else {
                        kind
                    };
                    let result =
                        Value::array_with_kind(crate::gc::Gc::clone(&arc_items), normalized_kind);
                    self.threads
                        .shared_vars
                        .set(key, Value::array_with_kind(arc_items, normalized_kind));
                    self.mark_shared_var_dirty(key);
                    if !is_thread_clone {
                        self.env.insert(key.to_string(), result.clone());
                    }
                    return result;
                }
            }
        }
        // Fallback for non-shared arrays: write through the shared node so
        // same-thread by-value holders observe the push (container identity §3).
        //
        // `env_root_descended_mut` (not a raw `self.env.get`/`get_mut`) is
        // required here for the same reason the `append`/`unshift`/`prepend`
        // arms in `call_method_mut_with_values` already use it: a captured,
        // escape-boxed `@`/`%` local (ADR-0039's `needs_cell_unvouched_containers`
        // — e.g. `my Str:D @a` captured by a block passed as a `.map`/`.grep`
        // argument) lives in a `ContainerRef` cell, not a plain `Array`, so a
        // raw `self.env.get(key)` never matched `ValueView::Array` and fell
        // through to the detached-array fallback below, which rebuilds a
        // fresh array and overwrites `env[key]` with it — severing the cell.
        // Every OTHER holder of that cell (in particular the closure's own
        // captured-env snapshot that an eager `.map` loop restores its
        // temporary bindings from) still saw the stale, pre-push cell
        // contents, so the pushed elements vanished the moment the loop's
        // env restore ran (#8503).
        if let Some(slot) = self.env_root_descended_mut(key)
            && matches!(slot.view(), ValueView::Array(..))
        {
            let result = slot
                .with_array_mut(|arc_items, kind| {
                    let items = crate::value::gc_data_mut(arc_items);
                    items.extend(values);
                    // Normalize @-variables only from List to Array while preserving Shaped.
                    if key.starts_with('@') && *kind == ArrayKind::List {
                        *kind = ArrayKind::Array;
                    }
                    Value::array_with_kind(crate::gc::Gc::clone(arc_items), *kind)
                })
                .unwrap();
            if package_array {
                self.set_our_var(key.to_string(), result.clone());
            }
            return result;
        }
        let mut items = match target_fallback.view() {
            ValueView::Array(v, ..) => v.to_vec(),
            _ => Vec::new(),
        };
        items.extend(values);
        let result = Value::real_array(items);
        let stored = if package_array {
            Self::itemize_scalar_store_value(result.clone())
        } else {
            result.clone()
        };
        self.env.insert(key.to_string(), stored.clone());
        if package_array {
            self.set_our_var(key.to_string(), stored);
        }
        result
    }

    pub(crate) fn push_to_existing_shared_array(
        &mut self,
        key: &str,
        values: Vec<Value>,
    ) -> Option<Value> {
        if !key.starts_with('@') || !self.threads.shared_vars_active {
            return None;
        }
        // A plain lexical `@name` routes through the `__mutsu_atomic_arr::`
        // store (single authoritative container; `set_shared_var` refuses to
        // clobber it with a stale parent snapshot — the t/lock.t lost-push
        // race lived here: this helper used to mutate the *base* key, which a
        // parent-thread env sync could wipe wholesale). "Existing" contract:
        // only handle a var already present in the shared store.
        if Self::is_plain_lexical_array_name(key) {
            let in_shared = {
                let atomic_key = atomic_lane_str_key(key, false);
                self.threads
                    .shared_vars
                    .get(atomic_key)
                    .is_some_and(|v| matches!(v.view(), ValueView::Array(..)))
                    || self
                        .threads
                        .shared_vars
                        .get(key)
                        .is_some_and(|v| matches!(v.view(), ValueView::Array(..)))
            };
            if !in_shared {
                return None;
            }
            return Some(self.shared_array_extend(key, values, false));
        }
        // Attribute / twigil'd arrays have per-instance identity and must NOT
        // funnel into the name-keyed atomic store (roles-6e.t: every C1
        // instance's `@!order` would accumulate cross-object). They keep the
        // base-key in-place path, serialized by the shared_vars write lock.
        let is_thread_clone = self.is_thread_clone();
        if is_thread_clone {
            self.env.remove(key);
        }
        let result = self
            .threads
            .shared_vars
            .with_entry_mut(key, |v| {
                v.with_array_mut(|arc_items, kind| {
                    let items = crate::gc::Gc::make_mut(arc_items);
                    items.extend(values);
                    if *kind == ArrayKind::List {
                        *kind = ArrayKind::Array;
                    }
                    Value::array_with_kind(crate::gc::Gc::clone(arc_items), *kind)
                })
            })
            .flatten()?;
        if is_thread_clone {
            let dirty_marker = MetaNs::SharedDirty.owned_key_for_str(key);
            if !self.env.contains_key(&dirty_marker) {
                self.mark_shared_var_dirty(key);
                self.env.insert(dirty_marker, Value::TRUE);
            }
        } else {
            self.mark_shared_var_dirty(key);
            self.env.insert(key.to_string(), result.clone());
        }
        Some(result)
    }
}

/// Whether an env key is an attribute alias (`!x`, `@!x`, `%!x`, `&!x`) rather
/// than a lexical.
fn is_attribute_alias_name(key: &str) -> bool {
    let bare = key.trim_start_matches(['$', '@', '%', '&']);
    bare.len() > 1 && bare.starts_with('!')
}
