# ② VM ownership of the declaration registry (structural refactor design)

**Step ②** of the final goal in [PLAN.md](../PLAN.md), "remove the tree-walking Interpreter execution path →
eliminate the dual store". ① (eradication of individual fallbacks) has exhausted the cheap sites; the rest presuppose
the structural blockers (②/③). This document is the settled design for ②. The primary ledger is
[vm-interpreter-fallback-ledger.md](vm-interpreter-fallback-ledger.md);
the dual store is [vm-dual-store.md](vm-dual-store.md).

## The problem to solve: the bidirectional ownership knot

- `VM` **owns `interpreter: Interpreter` by value** (`src/vm.rs`).
- `Interpreter` holds no reference to the VM, but **exclusively holds the shared execution state (env, declaration
  registry, type system)**. VM-native code can only touch them via `self.interpreter.<field>`.
- Execution control **ping-pongs between VM↔Interpreter** via `Interpreter.run_block_raw → VM::new(self) → vm.run() →
  return (interp,result) → *self=interp`. The state is "locked inside" the Interpreter.

② = of this locked-in state, lift the **declaration registry** (class/role/enum/subset/sub/token + related meta, ~30
fields) to a place both the VM and the Interpreter can touch as peers. It is a prerequisite for ③ (env/type/state
migration), and also for eradicating the ledger §2 function-dispatch fallback (multi/sub resolution).

## Constraints binding the design (settled by investigation)

1. **`Value` is `Send + Sync`** (compile-time assert in `src/value/mod.rs`; internally everything is `Arc`).
   The `Interpreter` itself is moved into `std::thread::spawn` closures (`spawn_user_thread<F: Send>`).
   → **`Rc<RefCell>` violates Send and is out.** Interior-mutable sharing means `Arc<RwLock>`/`Arc<Mutex>` only.
2. **The registry is currently deep-cloned (snapshotted) per thread** (`clone_for_thread`, no write-back).
   → ② **strictly preserves this semantics** (declarations inside `start {}` do not leak to the parent / the parent's
   declarations are visible from the child).
3. **Registration re-enters user code while registering.** `register_class_decl`/`register_sub_decl`/
   `register_enum_decl` call `eval_block_value`/`run_block_raw`/`call_function("trait_mod:<is>")` in the middle of
   registration, executing class body statements, BEGIN phasers, trait handlers, attribute defaults, enum variant values,
   and parameterized role bodies. That re-entrant code may **recursively read/write** the registry.
   → A "plain field + &mut" plan that borrows `&mut Registry` for the whole function **breaks down with borrow errors**
   in the transition period. Together with the registry crossing VM↔Interpreter in the ping-pong,
   **a shared handle is the only option for the transition period**.

## Decision (user policy 2026-06-08)

- **End state = a plain field owned solely by the VM** (after the Interpreter object disappears). This is the settled direction.
- **Transition representation = an `Arc<RwLock<Registry>>` scaffold**. The Interpreter holds it and hands a handle clone
  to each VM. Once ③/④ remove the Interpreter execution path and the ping-pong, **fold it into a plain VM field**
  (the Arc/lock is needed only "because two parties share it"; once there is one owner it naturally disappears).
- **Discipline (important)**: never hold the RwLock guard **across re-entry** (short critical sections: acquire→read/write→
  drop; always drop before `eval_block_value`/`run_block_raw`/`call_function`). Mandatory since the RwLock is non-reentrant.
- **Scope = structural refactor only (behavior-preserving)**. Actually consuming the ledger §2 fallbacks is a separate PR after ② completes.

## The `Registry` struct (`src/runtime/registry.rs`)

The declaration-registry fields moved from `Interpreter` into `Registry` (logical groups):

- **Functions/Subs**: functions, our_scoped_functions, proto_functions, proto_subs, token_defs, proto_tokens
- **Classes**: classes, cunion_classes, hidden_classes, class_stubs, package_stubs, hidden_defer_parents,
  class_trusts, class_how_values, class_composed_roles, class_enum_roles, class_subs,
  attribute_build_overrides, class_attribute_defaults, class_attribute_is_types, class_attribute_deprecated
- **Roles**: roles, user_declared_roles, role_candidates, role_parents, role_hides, role_type_params,
  class_role_param_bindings
- **Enums**: enum_types — **Subsets**: subsets

Holding/duplication: `Interpreter.registry: Arc<RwLock<Registry>>` + `registry()`/`registry_mut()` helpers.
`clone_for_thread` deep-clones the contents via `Arc::new(RwLock::new(self.registry.read().clone()))`
(strictly preserving the snapshot semantics). After all fields migrate, the current ~30 lines of individual `.clone()` fold into 1 line.

**Boundary (excluded from ②)**: the `Arc::as_ptr`-keyed side tables such as `type_metadata`/`array_type_metadata`
(🟣 the first-class container matter), env/type-check body/readonly/let/state/current_package (= ③).

## Migration procedure (strangler-fig; each PR behavior-preserving; CI is the safety net)

- **PR-A (extraction)**: move the field groups into Registry and route all access through the lock. Incremental, group by group:
  1. enum/subset (smallest, shallow re-entry; establishes the machinery)
  2. class meta group (small, clone-cheap)
  3. classes (hot path, large `ClassDef`. **Avoid naive clones** = Registry accessors return the minimal data)
  4. roles group (perf-sensitive)
  5. functions/subs/tokens (after migration, `clone_for_thread` becomes 1 line)
- **PR-B (read-side migration)**: turn the registry-read parts of lookup/MRO/type matching into Registry methods, the
  VM's ~15-20 read sites to `self.registry.read()`. This sets up the prerequisites for ③/§2 dispatch eradication.
- **PR-C (write-through tidying)**: organize register_*_decl's registry writes into write-lock blocks,
  guarantee no guards across re-entry, update the ledger annotations (registration-triggers-execution CARRIER).

## Progress

- **PR-A slice 1 (#2760)**: machinery established + enum/subset migrated. Introduced `src/runtime/registry.rs`,
  the `registry()`/`registry_mut()` helpers, `clone_for_thread` wiring (snapshot preserved), ~75 sites converted,
  re-entry hazards (subset `where` evaluation / base-chain walk) made safe by hoisting the guards; `resolve_subset_base_type`
  changed to return `String`. build/clippy/make test green, 15 whitelist enum/subset/coercion files PASS.
- **PR-A slice 2 (this PR)**: migrated the 14 class-meta fields (cunion_classes, hidden_classes, class_stubs,
  package_stubs, hidden_defer_parents, class_trusts, class_how_values, class_composed_roles, class_enum_roles,
  class_subs, attribute_build_overrides, class_attribute_defaults, class_attribute_is_types,
  class_attribute_deprecated). Converted ~130 sites to read=`registry()`/write=`registry_mut()`.
  Removed `clone_for_thread`'s 14 individual clones (absorbed into the whole-Registry clone). The builtin seed of
  `class_composed_roles` is injected into `Registry::default()` in `Interpreter::new`. The 3 reference-returning accessors
  (`class_composed_roles`/`class_attribute_default`/`class_attribute_deprecated`) changed to owned returns
  (`.cloned()`) with the VM/runtime call sites following suit (a reference into the guard cannot be returned). Re-entry
  hazards made safe by guard hoisting: the `class_composed_roles` clone before `collect_transitive_roles`, the
  `attribute_build_overrides` clone before `call_sub_value`, and `has_class_scoped_subs`'s double read into a single
  let-bound guard. VM direct access: routed `vm.rs`'s `package_stubs.insert/remove` through `registry_mut()`. build/clippy/make test
  green, whitelist class/role/attribute roast green (the pre-existing non-whitelist private-method failures unchanged).
- **PR-A slice 3 (this PR, after #2762)**: migrated `classes: HashMap<String, ClassDef>` (~178 sites).
  Made `ClassDef` `pub(crate)`, removed the `Debug` derive from Registry (ClassDef/MethodDef/AST graph are non-Debug).
  Builtin classes are injected via `Registry { classes, ..default() }` in `Interpreter::new`. Removed `clone_for_thread`'s
  `classes.clone()` (absorbed into the whole-Registry clone = snapshot preserved). **Perf policy (avoiding the #2746 rut)**:
  no whole-`ClassDef` naive clones whatsoever; the hot paths (`resolve_method_with_owner_impl`/`class_mro`/
  `compute_class_mro`/type matching) keep the previous targeted projection clones (`mro.clone()`/`methods.get(m).cloned()`)
  but through the `registry()` guard. Release microbench (a pathological pure-method-dispatch loop of 300k iterations) shows **+2–4%** =
  only the transitional RwLock acquisition cost (<1% on real workloads; disappears once folded into a plain field after Interpreter removal).
  **Re-entry safety**: ① `clone_for_thread` creates an independent Arc per thread, so the lock is not shared across
  threads → recursive reads within a single thread are safe with the futex implementation. ② The real danger = a write on the same lock
  while a read guard is held (read→write upgrade deadlock) was confirmed absent (writes are either on the separate object `nested`, or
  after the guard drop). ③ Read guards spanning `&mut self` re-entry, including the edition-2021 `if let` temporary-scope
  trap, were all resolved via `let` hoisting / clone-out (resolution.rs's
  multi/private dispatch, methods_object's BUILD, and the private-zeroarg fast-path's cache write moved outside the guard).
  Confirmed all `get_mut` bodies are registry-non-reentrant. build/clippy/make test green.
- **PR-A slice 4 (this PR, after #2763)**: migrated the roles group's 7 fields (roles, user_declared_roles, role_candidates,
  role_parents, role_hides, role_type_params, class_role_param_bindings; ~200 sites). Made `RoleCandidateDef`
  `pub(crate)` (`RoleDef` was already Debug/Clone/pub(crate)). The builtin roles seed is injected via `registry.roles = {..}` in
  `Interpreter::new`'s registry block; removed `clone_for_thread`'s 7 individual clones. **The 2 reference-returning accessors**
  (`get_role_def -> Option<&RoleDef>`, `class_role_param_bindings -> Option<&HashMap>`) changed to owned returns.
  The VM side only goes through both accessors (`.is_some()`/`.map()`, so call sites unchanged). Re-entry hardening: ① **found and
  fixed 2 read→write same-lock deadlocks** (registration_class.rs's role hidden flag→hidden_classes, role_hides→
  hidden_defer_parents. Calling `registry_mut()` while a read guard is held = a deadlock the borrow checker does not catch).
  Exhaustively scanned all registry fields' read-get bodies for `registry_mut` with a Python brace-aware scanner and confirmed 0.
  ② Read guards spanning `&mut self` re-entry (an `if let` scrutinee lives for the whole body even in this crate's edition2024)
  resolved via clone-out/`let` hoisting (methods.rs role type punning, methods_object's new() role bindings, smart_match's
  parametric parents, single-guard-ification of resolution's class+role method-table or_else). ③ Two read guards in one statement
  (`classes.contains_key || roles.contains_key`, 3 places) into a single bound guard. build/clippy/make test green.
  Behavior-preservation confirmed (role composition/inheritance/parameterization/punning/conflict/`but`; the Nil for a Stack array
  attribute is the same pre-existing gap as main).
- **PR-A slice 5 = PR-A final (this PR, after #2764)**: migrated the functions/subs/tokens group's 6 fields (functions,
  our_scoped_functions, proto_functions, token_defs, proto_subs, proto_tokens). `FunctionDef` is already
  Debug/Clone/pub(crate). No builtin seeds (all user-defined). **With this, the entire declaration registry is inside Registry, and
  `clone_for_thread` has zero individual field clones = only the single whole-registry clone** (achieving ②'s definition of done).
  No VM direct access. snapshot_routine_registry became a single read guard; restore was split into two stages: read (our_scoped
  collection) → guard drop → write. The 9 struct-literal sites building regex/grammar sub-interpreters (`Interpreter { functions:..,
  token_defs:.., ..Default }`) go through a `copy_decl_registry_into(&mut interp)` helper (snapshot copies between distinct Arcs).
  **★ Critical: found and fixed a write→write same-lock deadlock**: `match self.registry_mut().functions.entry(k) {
  Occupied => { ... self.registry_mut().functions.entry(k2) ... } }` = calling
  `registry_mut()` again inside an arm while the outer write guard is held. **The borrow checker treats separate `registry_mut()`
  calls as separate borrows, so it goes uncaught**, and **the scanner also missed it at first → multi sub declaration hung
  at runtime** (surfaced by a smoke test). Consolidated into the `insert_multi_overload` helper (base→`__m{N}` fallback under a
  single write guard). **Lesson: not just read→write — write→write and write→read are dangerous too.
  The detection scanner must also scan for registry re-access inside `match/if-let self.registry_mut()` block bodies (below).
  The last line of defense is the make roast timeout** (make test + 51823 roast samples green, zero regressions).

## PR-B (read-side migration) progress

- **PR-B slice 1 = Registry-methodizing MRO/class lookup (this PR)**: MRO computation is the hottest registry read,
  hit by all of the VM's method dispatch. Lowered it into pure methods on `impl Registry` (`src/runtime/registry.rs`):
  - `Registry::compute_class_mro(&self, name, stack) -> Result<Vec<String>>` — pure C3 linearization referencing only
    `self.classes`. The recursion stays inside the registry; no user-code re-entry.
  - `Registry::class_mro(&mut self, name) -> Vec<String>` — builtin hierarchy (Match/Capture/CompUnit…) +
    parameterized (`Foo[Bar]`) + `ClassDef::mro` cache lookup + compute + cache write-back, all under a **single
    write guard**. The previous `Interpreter::class_mro` acquired up to 5 individual `registry()`/`registry_mut()`;
    consolidated into 1 (fewer lock acquisitions on the hot path).
  - `Registry::class_mro_cached(&self, name) -> Option<Vec<String>>` — readonly (the cache or a single element).
  - `Registry::class_has_method(&mut self, name, method) -> bool` — method-presence determination via an MRO walk.
  The Interpreter-side `class_mro`/`compute_class_mro`/`class_mro_readonly`/`class_has_method` became thin delegation
  wrappers (65 + numerous call sites untouched). Behavior-preserving, build/clippy/make test green, S12/S14 roast green.
- **On VM direct reads (investigation result)**: currently there are zero sites where the VM reads registry fields
  directly; only the 2 `package_stubs` writes (`vm.rs`). All of the VM's registry-family accesses go via
  `self.interpreter.<wrapper>()` (`type_matches_value` 40+ sites, `class_mro`, etc.). So "the VM's ~15-20 read sites to
  `self.registry.read()`" does not match the real code = adding a registry handle to the VM is not needed in this slice
  (it goes to ③/④ together with the ② non-goal Arc→plain folding). The substance of the read-side migration is
  Registry-methodizing the wrappers = this slice.
- **PR-B slice 2 = Registry-read methodization of method resolution/type matching (this PR)**: consolidated the
  same-shaped pure-registry reads scattered across the dispatch hot path into owned-returning methods on `impl Registry`
  (maintaining the drop-the-guard-immediately discipline):
  - `Registry::get_method_overloads(&self, class, method) -> Option<Vec<MethodDef>>` — `classes.get(c).methods.get(m).cloned()`.
    Converted the 5 same-shaped sites in `resolution.rs` (the overload read in `resolve_method_with_owner_impl`,
    the read + any_multi recheck in `resolve_methods_per_mro_level`, 2 private-resolution sites).
  - `Registry::get_role_param_bindings(&self, class) -> Option<HashMap<String,Value>>` — the same 4 sites converted.
  - `Registry::is_hidden_class` / `is_hidden_defer_parent(class, owner) -> bool` (the latter predicatized instead of
    returning the owning set = avoids a gratuitous clone at `&self`-only call sites). Converted `should_skip_defer_method_candidate`.
  - `Registry::composed_roles_seed(&self, mro) -> Vec<String>` — **only the seed** of the composed-role transitive walk
    (MRO→`class_composed_roles`; push order preserved, dedup/sort forbidden = the LIFO walk's first-match-wins is load-bearing).
    Applied to the seeds of both Block A (Package, `resolved_constraint.is_some()`) / Block B (Instance,
    `roles.contains_key`) in `type_matching.rs`. **The transitive-walk body itself stays inline**: the gates are
    block A=`resolve_role_key` (env+lock re-entry) / block B=`roles.contains_key`, and the match is `type_matches` /
    exact `== constraint`, differing non-trivially, and calling the `resolve_role_key` gate from inside a Registry method
    (while the read guard is held) would deadlock since std `RwLock` is non-reentrant.
  - **Not converted (deliberately)**: `resolve_all_methods_with_owner`'s class-OR-role `.or_else` fallback (not same-shaped),
    the private zero-arg fast path (a hot-path optimization that clones only the 1 matched item via a borrow scan = avoids a
    full-Vec clone regression).
  - **Out of scope (separate matter)**: the composed-role walks in `methods_classhow.rs`/`methods_walk.rs` differ structurally
    (VecDeque BFS vs LIFO Vec, base-strip timing difference, double map closure), which would change the walk order — left as-is.
    The `functions.get(&Symbol)` lookup is left on the Interpreter side because the surrounding key construction depends
    on `current_package`/`env`.
  Behavior-preserving, build/clippy/make test green, S12/S14 (including typecheck/parameterized/generic-subtyping) roast green.
- **Remaining (PR-B follow-ups)**: the remaining tie-break/candidate collection in `resolve_method_with_owner_impl`
  (type-distance calculation etc.) is already pure in-memory processing after owned clones, and the registry reads were
  consolidated in this slice. Next is **PR-C** (write-through tidying of register_*).

## PR-C (write-through tidying) progress — **② complete**

- **A *runtime* guarantee of no guards across re-entry (the core of this PR)**: `registry()`/`registry_mut()` previously
  returned std's `RwLockReadGuard`/`RwLockWriteGuard` directly, so re-acquiring the same lock on re-entry **silently
  deadlocked** and could only be caught by make roast's ~13-minute timeout. The static scanner has blind spots in code
  shape (PR-A slice 5's write→write inside a `match self.registry_mut()...{}` arm slipped past both the borrow checker and
  the scanner and hung at runtime). This PR replaces `registry()`/`registry_mut()` with **re-entry-detecting wrapper guards**
  (`RegistryReadGuard`/`RegistryWriteGuard`, `src/runtime/registry.rs`):
  - Debug builds only: immediately before acquisition, inspect the thread-local held state and panic with a location
    **before the blocking `.read()`/`.write()` call** (falls over immediately and explicitly instead of deadlocking).
  - **Keyed by the lock's address** (not thread-global). To avoid false positives on the legitimate case of a single
    thread simultaneously holding a guard on a *different* registry
    (`self.registry_mut().classes = nested.registry().classes.clone();` = self's write guard plus
    sub-interpreter `nested`'s read guard. Different locks, so no deadlock).
  - The allow/deny matrix matches std `RwLock`'s actual deadlock conditions: on the same lock, write-while-any / read-while-write
    panic; read-while-read (nested read; in `a().x && b().y` both temporary guards live to end of statement) is allowed.
  - In release it disappears completely under `#[cfg(debug_assertions)]` (no thread-local bookkeeping on the hot
    `registry()` read path). CI's release make roast keeps the timeout as the last line of defense.
  - Verification: with the debug binary, ran 6000+ files across t/ + S10/S11/S12/S14/S05/S04 etc. — **zero re-entry panics**
    (no false positives, no latent re-entry bugs). Threading (start/promise/channel) also clean.
- **Write-through tidying (register_class_decl)**: consolidated consecutive re-entry-free write/read clusters into single
  guard blocks to reduce the number of lock acquisitions and to syntactically demarcate "this span holds the guard = re-entry
  forbidden". ① the prologue's 5 prev_* reads into a single read-guard block (all owned/cloned). ② the 10
  `this.registry_mut()` calls in the `restore_previous_state` rollback closure into a single write guard (no re-entry in the body).
  ③ the consecutive 2 writes of stub clearing (`class_stubs.remove` +
  `package_stubs.remove`) into a single write guard. The runtime guard above assures safety (calling the closure while
  erroneously holding a read guard would panic immediately, but it never fired over 6000+ runs = confirmed no held guards at the call sites).
- **Ledger annotation**: recorded in the progress log of `docs/vm-interpreter-fallback-ledger.md` that ②'s extraction
  phase is complete, plus the registration-triggers-execution CARRIER nature.

## Definition of done (②) — **PR-A/B/C complete (#2760/2762/2763/2764/2767/2769/2772/this PR)**

All declaration-registry fields are gone from `Interpreter`, leaving a single `registry: Arc<RwLock<Registry>>`. `clone_for_thread`'s
registry duplication is just the single whole-registry clone line (zero individual field clones, snapshot unchanged). The VM's
registry-family lookups go through accessors (many already owned-returning). Fully complete through **PR-A** (extraction) →
**PR-B** (Registry-methodizing lookup/MRO/type matching) → **PR-C** (write-through tidying of register_* + a runtime-guard
guarantee of no guards across re-entry). **Next is ③** (env/type/state migration = the biggest mountain of interpreter-bridge removal).

## PR-D (③ follow-up: VM-nativizing registry dispatch reads) progress

With ② the registry is VM-handled, but the VM's dispatch predicates (`has_proto`/`has_multi_candidates`) went through the
`self.interpreter.has_proto(...)` bounce (these wrappers were coupled to `current_package`, and
`current_package` was an Interpreter-owned plain field). Built on top of the slice landed right after `current_package`
was made a shared handle (the current_package migration entry in `docs/vm-state-ownership.md`):

- **Made `has_proto`/`has_multi_candidates` pure `impl Registry` methods** (`registry.rs`; they take `current_package`
  as an argument; pure registry+scope reads, no env/re-entry). `Interpreter::has_proto`/`has_multi_candidates` became
  thin delegations to `self.registry().has_proto(&self.current_package(), name)` (**1 operation = 1 implementation**).
- **Added a `registry()` read accessor to the VM** (`RegistryReadGuard`, same discipline as `registry_mut()` = never hold
  across re-entry) + VM-native `has_proto`/`has_multi_candidates` (reading via the VM's own `registry`+`current_package`
  handles). Converted ~21 VM sites from `self.interpreter.has_*` → `self.has_*`. `has_multi_candidates_cached`
  also switched to the VM-native call. Re-exported `RegistryReadGuard` from `runtime`.
- **`resolve_function_with_types` left as-is** (the prefix-package visibility determination of `self.env.get(...)` +
  the `where` clauses in `choose_best_matching_candidate` = coupled to user-code re-entry = cannot be purified. The interpreter
  bounce is correct until the env migration). **`has_proto_token` also left as-is** (coupled to the `mro_readonly` MRO walk).
- Behavior-preserving, build/clippy/make test green, S06-multi (proto/syntax/type-based/subsignature/lexical-multis/callsame)
  /S10-packages/S11-modules roast green. (PR #2929)

- **Second slice (`has_function`/`has_multi_function`, PR #2933)**: VM-nativized the remaining pure predicates in the same shape.
  Single-implementation `Registry::has_declared_function(current_package, name)` / `has_multi_function(current_package, name)`;
  `Interpreter::has_declared_function`/`has_multi_function` became thin delegations; added VM-native
  `has_declared_function`/`has_function`(alias)/`has_multi_function` and converted ~15 VM sites. Behavior-preserving,
  make test green, S06-multi/S10-packages/S11-modules/S02-names roast green.

  **The pure-predicate dispatch reads are exhausted** (has_proto/has_multi_candidates/has_function/has_multi_function done).
  All remaining dispatch bounces presuppose the ③ env/type-check migration:
  - `resolve_function_with_types` (env + `where` re-entry coupling)
  - `has_proto_token` (coupled to the `mro_readonly` MRO walk. `Registry::class_mro` exists, but the builtin-hierarchy
    resolution and the `proto_tokens` walk are Interpreter-side = purification would require migrating the entire MRO
    into Registry, which does not pay off)
  - `type_matches_value` (mixed coupling of subset `where` re-entry + type metadata + env)

## Non-goals

env/type/state migration (③), full VM-nativization of registration (after ③/④), actual consumption of ledger §1/§2
(a separate PR after ② completes), resolving `type_metadata`'s Arc-ptr keying (🟣 first-class container), the final
Arc<RwLock>→plain-field fold (after Interpreter removal = ④/⑤), true cross-thread sharing of the registry
(dropping the snapshot; PLAN §8.3 concurrency).
