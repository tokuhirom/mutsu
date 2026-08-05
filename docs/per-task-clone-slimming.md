# Per-task `clone_for_thread` slimming — implementation plan

- Status: **design ready, implementation open** (2026-08-05)
- Owner ticket: `todo/tickets/digest-ripemd-start-per-block-overhead.md`
- Context: [ADR-0020](adr/0020-shared-worker-pool.md) §1.3 identified the per-task
  `Interpreter` clone as the dominant per-`start` cost that the worker pool
  deliberately did NOT address. This document is the implementation plan for that
  companion lever.
- Related: [ADR-0010](adr/0010-cross-thread-lexical-sharing-scope.md) (lineage
  store seeded per spawn), [ADR-0018](adr/0018-slot-addressed-lexical-capture-and-env-sync.md)
  (slot-authoritative locals).

This plan is written to be executable slice-by-slice by an implementer without
deep prior context. Each slice is an independent PR with its own verification.
Follow the slices IN ORDER — later slices assume earlier ones landed. Read the
"Global rules" and the per-slice "Pitfalls" before writing code.

## 1. Goal and exit criterion

Every `start {}` / `Promise.start` / pooled task pays `clone_for_thread`
(`src/runtime/runtime_thread.rs`), which today deep-clones the declaration
`Registry` (~40 maps of `ClassDef`s etc.), deep-copies the env map, iterates the
whole parent env twice (lineage seeding + IO-handle scan), and rebuilds the IO
environment (`init_io_environment`) including five `make_instance` calls and a
`current_dir()` syscall — per task. The pool (ADR-0020) made workers warm; this
campaign makes the per-task payload small.

**Benchmark (the "ripemd shape"):**

```sh
# tmp/bench-start-shape.p6
for ^2000 { await map -> $k { start { $k * 2 } }, 1, 2 }
```

Baseline on main + ADR-0020 slice 3 (2026-08-05, profiling build, 12 cores):
**1.66s** wall for 4000 tasks vs raku **0.33s** (~5×). Flat `perf` profile:

- P-cores (spawning side): `_int_malloc` 16.9%, `hash_one` 10.1%,
  `malloc_consolidate` 9.2%, `HashMap::insert` 7.1%, `malloc` 6.6%,
  `clone_for_thread_excluding` (self) 6.2%, SipHash `write` 5.7%.
- E-cores (worker/drop side): `_int_free` **25.7%**,
  `drop_in_place<ClassDef>` 4.9%, `malloc_consolidate` 4.7%, `cfree` 3.8%.

i.e. roughly half the benchmark is *allocating and freeing the per-task
Interpreter*, and the single largest identifiable component is the `Registry`
deep clone and its drop (`ClassDef` appears by name in the drop profile).

**Exit criterion:** the ripemd shape under 3× raku (< ~1.0s for the benchmark),
and `t/ripemd.t` from the Digest battery under the 120s batteries-gate budget
(see the owner ticket for the repro). Progress well short of that is still worth
landing slice-by-slice.

**Non-goals:** reusing a previous task's `Interpreter` (per-task clone is a
correctness contract — ADR-0010 lineage seeding is per spawn); continuation-ified
`await` (ADR-0020 fork (b)); changing wasm32 behavior.

## 2. How to measure (do this before and after every slice)

```sh
# Counters are optimization-level independent — iterate on the DEBUG build:
cargo build
MUTSU_VM_STATS=1 ./target/debug/mutsu tmp/bench-start-shape.p6
# -> note the dual-store, worker-pool, and (new) clone-slimming counter lines

# Wall-clock: release build ONLY for the final number (median of 5):
cargo build --release
for i in 1 2 3 4 5; do /usr/bin/time -f %e ./target/release/mutsu tmp/bench-start-shape.p6; done
```

Baseline counters (2026-08-05): `clone_env=4000` (O(1) Arc bumps),
`env_deep_copies=8000` (2/task: one real full copy at the child's first insert,
one cheap empty-overlay clone per block frame), `worker-pool: tasks=4000
spawns=3 warm_reuses=3997`.

Write the tmp benchmark file with the Write tool (never heredoc), run with
`timeout 30`. Numbers quoted in the eventual news entry must come from bench CI,
not local runs (CLAUDE.md rule) — local A/B is for development decisions only.

## 3. Facts you must know before touching the code

1. **All registry access goes through exactly two accessors** —
   `Interpreter::registry()` (read, ~819 call sites) and
   `Interpreter::registry_mut()` (write, ~326 call sites) in
   `src/runtime/runtime_class_query.rs:324/341`. The call sites deref the guard
   as if it were `&Registry` / `&mut Registry`. This is what makes slice 1
   possible without touching 1100 call sites.
2. **Lock discipline (CRITICAL):** never hold a registry/io-handles guard across
   a call that re-enters user code (`eval_block_value`, `run_block_raw`,
   `call_function`). Debug builds panic on re-entry via the
   `lock_reentry` machinery (`src/runtime/lock_reentry.rs`). Slice 1 must keep
   that machinery in place.
3. **`Env` is already copy-on-write** (`src/env.rs`: `Arc<SymMap>` +
   `Arc::make_mut` in `cow_mut()`); `env: self.env.clone()` in
   `clone_for_thread_excluding` is O(1). The real per-task env copy happens at
   the child's FIRST env write (the `cloned.env.insert("/")` at the bottom of
   `clone_for_thread_excluding`), tracked by the `env_deep_copies` counter.
4. **`registry_write_gen`** (`src/runtime/runtime_class_query.rs:333`) is bumped
   on every `registry_mut()` acquisition and consulted by several resolution
   caches. Do not remove it; slice 1 keeps it exactly as is.
5. **Precedent for the COW idea already exists** in
   `src/runtime/regex/regex_eval.rs::copy_full_registry_into` — a
   generation-cached registry snapshot introduced because deep-cloning the
   registry per regex-closure eval was "catastrophic in a hot loop". Slice 1
   makes that mechanism obsolete (O(1) share replaces it).
6. `MUTSU_POOL=off` disables the worker pool (thread-per-task); useful to check
   a slice's effect is pool-independent.

## 4. Slices

### Slice 0 (warm-up): `SharedStore` map → `FxHashMap`

**What:** `src/runtime/shared_store.rs` keys its maps by `String` in
`std::collections::HashMap` (SipHash). The per-spawn seeding loop does one
lookup per env key per spawn — SipHash shows at 5.7% flat on the benchmark.
Variable names are not attacker-controlled; use `rustc_hash::FxHashMap` like
`src/runtime/registry.rs:27` already does (copy that justification comment).

**How:** in `shared_store.rs`, replace the `use std::collections::HashMap;`
import with `use rustc_hash::FxHashMap as HashMap;` and fix the handful of
construction sites (`HashMap::new()` stays textually valid with the alias;
`HashMap::default()` if `new()` is not provided — the compiler will tell you).
Check `own_map()`'s return type mentions the concrete type; update it.

**Pitfalls:** none semantic — both map types have arbitrary iteration order, so
no ordering guarantee is lost. If some caller names the concrete type
(`&RwLock<HashMap<String, Value>>`), update the type there too; let the
compiler drive.

**Verify:** `cargo build && cargo clippy -- -D warnings && make test`, then
bench A/B. Expected: a few percent off the benchmark; no behavior change.

### Slice 1 (the big one): Registry copy-on-write

**What:** change the registry field from `Arc<RwLock<Registry>>` to
`Arc<RwLock<Arc<Registry>>>`. A spawn then shares the inner `Arc` (O(1))
instead of deep-cloning ~40 maps; the first *write* on either side after a
share pays one deep clone (`Arc::make_mut`). Semantics are IDENTICAL to
today's eager deep clone — each Interpreter still has its own outer lock and
its own logical snapshot; the clone is merely lazy. In spawn-heavy loops
neither parent nor child writes the registry, so the deep clone (and its drop)
disappears from the per-task cost entirely. This attacks the largest measured
share (§1: `_int_free` 25.7% + `drop_in_place<ClassDef>` + the malloc/insert
share of the clone).

**Where (exhaustive list of edit sites):**

1. `src/runtime/mod.rs:1088` — field type:
   `registry: Arc<RwLock<Arc<Registry>>>`.
2. `src/runtime/runtime_init.rs:1864` — the constructor builds a `Registry`;
   wrap it: `Arc::new(RwLock::new(Arc::new(<existing expr>)))`. If other
   Interpreter constructors exist (`grep -rn "registry:" src/runtime/` and let
   the compiler find the rest), wrap them the same way.
3. `src/runtime/registry.rs:790-802` — replace the two guard **type aliases**
   with newtype structs (sketch below). This is the core of the slice: the
   1100 call sites keep compiling because the new guards `Deref`/`DerefMut` to
   `Registry`.
4. `src/runtime/runtime_class_query.rs:324/341` — accessors construct the new
   guards (constructor signature below).
5. `src/runtime/runtime_thread.rs:408` — the per-spawn clone becomes:
   ```rust
   registry: Arc::new(RwLock::new(Arc::clone(&self.registry.read().unwrap()))),
   ```
6. `src/runtime/regex/regex_eval.rs::copy_full_registry_into` — replace the
   whole generation-cache body with the same O(1) share as (5)
   (`target.registry = Arc::new(RwLock::new(Arc::clone(...)))`). Then remove
   the now-dead `regex_registry_snapshot` field (declared in
   `src/runtime/mod.rs`, initialized in `runtime_init.rs` and
   `runtime_thread.rs`) — the compiler lists the sites. NOTE this is also a
   small correctness improvement: the old shared snapshot let one
   sub-interpreter's (rare) registry write leak into the next sub-interpreter;
   the COW share isolates each one.
7. `src/vm/vm_stats.rs` — add a `registry_cow_clones` counter following the
   `record_pool_task` pattern (`src/vm/vm_stats.rs:210`), printed on the same
   stats line style. Incremented from the write-guard sketch below.

**Guard sketch (for `src/runtime/registry.rs`):**

```rust
pub(crate) struct RegistryReadGuard<'a> {
    inner: crate::runtime::lock_reentry::ReentrantReadGuard<'a, Arc<Registry>>,
}

impl<'a> RegistryReadGuard<'a> {
    pub(crate) fn new(lock: &'a std::sync::RwLock<Arc<Registry>>, name: &'static str) -> Self {
        Self { inner: crate::runtime::lock_reentry::ReentrantReadGuard::new(lock, name) }
    }
}

impl std::ops::Deref for RegistryReadGuard<'_> {
    type Target = Registry;
    #[inline]
    fn deref(&self) -> &Registry {
        // guard -> Arc<Registry> -> Registry
        &self.inner
    }
}

pub(crate) struct RegistryWriteGuard<'a> {
    inner: crate::runtime::lock_reentry::ReentrantWriteGuard<'a, Arc<Registry>>,
}

impl<'a> RegistryWriteGuard<'a> {
    pub(crate) fn new(lock: &'a std::sync::RwLock<Arc<Registry>>, name: &'static str) -> Self {
        Self { inner: crate::runtime::lock_reentry::ReentrantWriteGuard::new(lock, name) }
    }
}

impl std::ops::Deref for RegistryWriteGuard<'_> {
    type Target = Registry;
    #[inline]
    fn deref(&self) -> &Registry {
        &self.inner
    }
}

impl std::ops::DerefMut for RegistryWriteGuard<'_> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Registry {
        let arc: &mut Arc<Registry> = &mut self.inner;
        if Arc::strong_count(arc) > 1 {
            crate::vm::vm_stats::record_registry_cow_clone();
        }
        Arc::make_mut(arc)
    }
}
```

(If `&self.inner` does not coerce through the two `Deref` steps, write the
explicit form `&**self.inner` / for `DerefMut` first bind
`let arc = &mut *self.inner;`. The compiler error will make it obvious.)

**Pitfalls:**

- `Registry` already derives `Clone` (needed by `Arc::make_mut`) — do not
  remove the derive.
- `self.registry().clone()` call sites (if any besides the regex snapshot you
  are deleting): after this slice such a call still deep-clones a `Registry`
  through the guard's `Deref`. Grep `registry().clone()` and replace each with
  an `Arc` share if the receiver just wants a snapshot.
- Do NOT change `registry_write_gen` behavior (fact §3.4).
- Do NOT introduce a shared mutable registry between threads — each child MUST
  get its own outer `Arc<RwLock<...>>` (that is what keeps a child's
  declarations invisible to siblings; sharing the outer lock would be an
  observable semantic change and a flake source).
- The reentry debug machinery keys on the lock address; the newtype guards must
  keep constructing the reentrant guards (do not bypass with raw `.read()`),
  except the two O(1)-share sites (5)/(6) where a plain
  `self.registry.read().unwrap()` on a fresh, never-user-reentrant path is fine
  (it only clones an `Arc`).

**Verify:**

1. `cargo build && cargo clippy -- -D warnings`.
2. `MUTSU_VM_STATS=1 ./target/debug/mutsu tmp/bench-start-shape.p6` —
   `registry_cow_clones` must be **≈ 0** (0–5, not thousands). If it is per-task
   (~4000), a write path is touching `registry_mut()` per task — find it with
   `rust-gdb -batch -ex 'break <the record fn>' ...` before proceeding.
3. `make test` (full local suite), plus targeted:
   `prove -e target/debug/mutsu t/supply-*.t t/hyper.t t/concurrency-threading.t`
   and `MUTSU_FUDGE=1 prove -e target/debug/mutsu roast/S17-promise/start.t
   roast/S17-supply/act.t roast/S05-grammar/action-methods.t
   roast/S05-metasyntax/regex.t` (the regex files cover the
   `copy_full_registry_into` rewrite; grammar actions cover the full-copy path).
4. Release bench A/B (median of 5). Expected: the largest single-slice win of
   the campaign — plausibly 20–40% off the benchmark.
5. Push, let CI run full roast.

### Slice 2: single-pass env iteration in `clone_for_thread_excluding`

**What:** `src/runtime/runtime_thread.rs::clone_for_thread_excluding` iterates
the parent env twice per spawn: once for lineage seeding (`for (key, val) in
&self.env` at the top) and once for the IO-handle scan (`for value in
self.env.values()` at `referenced_handle_ids`). Merge them into ONE loop that
does both jobs (collect `handle_id_from_value` hits while walking the seeding
loop, including for keys the seeding `continue`s past — the handle scan must
see EVERY value, so perform it before any `continue`).

**Pitfalls:** the seeding loop skips entries via several `continue`s; the
handle-id collection must happen before the first `continue`. Behavior must be
byte-identical — this is purely a traversal merge.

**Verify:** `make test`; bench A/B (expect a small single-digit % win; skip the
slice if it measures at noise level AND the code reads worse — record the
measurement in the PR either way).

### Slice 3: cache process-constant IO env singletons

**What:** `src/runtime/io_env.rs::init_io_environment` runs per task and
rebuilds process-constant values every time: `make_distro_instance`,
`make_perl_instance` (×2: `*PERL`, `*RAKU`), `make_vm_instance`,
`make_kernel_instance` (five `make_instance` + `AttrMap` allocations), the
`$*EXECUTABLE` path resolution, `env::temp_dir()`, `env::var("HOME")`, and an
`env::current_dir()` **syscall**. Cache the constant ones in `OnceLock`
statics; keep per-clone what is genuinely per-interpreter (the four IO handles,
`$*SPEC` which reads `self`, `$*CWD`).

**How:** for each of DISTRO / PERL-RAKU / VM / KERNEL and the
`$*EXECUTABLE`(+`-NAME`) / `$*TMPDIR` / `$*HOME` values: build once inside
`static X: OnceLock<Value> = OnceLock::new();` accessors, `get_or_init` with the
existing `make_*` body, and `clone()` the cached `Value` into env (Value clone
is a cheap handle copy).

**Pitfalls:**

- Sharing the cached `Value` across threads means those instances share one
  `AttrMap` identity process-wide. That matches Rakudo (these dynvars are
  process singletons) but IS a behavior change if user code mutates e.g.
  `$*VM` attributes per-thread. Accepted risk; if a roast failure appears in
  `S02-magicals` or similar, fall back to caching only the *expensive inputs*
  (the strings / the `current_dir()` result) and still rebuilding the small
  instance per clone.
- `$*CWD` must stay per-clone (it is deliberately thread-local — see the
  comment in `clone_for_thread_excluding`), but its VALUE for a thread clone
  should come from the parent env entry (already present in the cloned env)
  instead of a fresh `current_dir()` syscall — only the top-level
  `Interpreter::new` path needs the real syscall. Easiest split: give
  `init_io_environment` a `for_thread_clone: bool` parameter (or a sibling
  method) that skips the syscall when the cloned env already carries `$*CWD`.

**Verify:** `make test`; `MUTSU_FUDGE=1 prove -e target/debug/mutsu
roast/S02-magicals/*.t` (dynvar introspection); bench A/B (expect ~5-10%).

### Slice 4: `instance_type_metadata` COW

**What:** same disease, much smaller organ: `clone_for_thread_excluding` deep
clones `instance_type_metadata` (`Arc::new(RwLock::new(map.read().clone()))`).
Apply the identical `RwLock<Arc<...>>` + make_mut-on-write pattern from slice 1
(the field is accessed via `.read()`/`.write()` directly — fewer sites; let the
compiler list them, wrap writes in a tiny accessor mirroring
`registry_mut()`).

**Verify:** `make test`; bench A/B. If the win is at noise level, still land it
— it removes a per-task allocation family and its drop for free.

### Slice 5 (measure-first; STOP-AND-ASK before implementing the skip):
seeding-loop generation skip

**What:** even after slices 0-4, every spawn still walks every env entry to
`seed_if_absent` into the ADR-0010 lineage store. For same-scope spawn loops
(the ripemd shape) the loop re-seeds an unchanged env 4000 times.

**Step A (safe, do first):** add counters `spawn_seed_keys` (entries walked)
and `spawn_seed_inserts` (entries actually inserted) to the seeding loop.
Report on the bench: expected `keys` ≈ env_size × 4000, `inserts` ≈ env_size.
Land the counters as their own tiny PR — they quantify the remaining headroom.

> **Step A result (2026-08-05, PR #5933):** `keys_walked=120000` (30 env
> entries × 4000 spawns), `inserts=23` — the walk is ~99.98% redundant
> re-walk, exactly as predicted.

**Step B (design sketch — REVIEW WITH THE USER / a capable model before
implementing):** give `Env` a monotonically increasing `write_gen: u64`
(bumped in `cow_mut`, `insert*`, `remove`); remember on the Interpreter the
`(env_write_gen, Arc::as_ptr(&self.shared_vars))` of the last completed
seeding; skip the seeding loop (NOT the rest of the function) when both are
unchanged AND `state_vars` is unchanged AND `thread_redeclared_vars`-related
bookkeeping is provably a no-op for the unchanged env. The risk here is exactly
the CLAUDE.md "incomplete static analysis" trap: a missed invalidation source
becomes a *flaky* cross-thread bug, the worst outcome class in this codebase.
That is why step B is gated on explicit review — do not implement it in the
same PR as step A, and do not implement it at all without sign-off recorded in
the PR description.

> **Step B RETIRED by measurement (2026-08-05) — do not implement.** A
> local-only measurement hack (skip the seed/`declare` calls while keeping the
> walk, after the store is saturated) bounded the win at **zero**: debug
> baseline 8.2s vs 8.16s hacked; release baseline 0.70–0.74s vs 0.71–0.73s
> hacked (bench shape, 4000 tasks). After slice 0 made the per-key lookups
> FxHashMap-cheap, 120k walked keys cost single-digit milliseconds total — the
> walk is no longer a cost center, so the generation-skip machinery (an Env
> write-gen, a store-removal counter, equality memos on
> `captured_scalars`/`thread_redeclared_vars`/`thread_decl_in_flight`, a
> memoized handle-id set) would add flake-risk surface for no measurable
> return. The redundancy the step A counters show is real but free.
> (Incidental finding from the measurement: over-approximating
> `referenced_handle_ids` to "all handles in the table" made the bench 14×
> slower — per-spawn handle cloning is very sensitive to the referenced-only
> filter; keep it exact.)

### Slice 6 (correctness-led, separate discussion): inherit parent dynamic IO
vars in thread clones

Measured divergence (2026-08-05):

```raku
my $out = ""; { my $*OUT = class { method print(*@a) { $out ~= @a.join };
method flush {} }.new; await start { print "X" } }; say "captured=[$out]"
# raku:  captured=[X]      (start inherits the redirected $*OUT)
# mutsu: X + captured=[]   (init_io_environment clobbers the child's $*OUT)
```

`init_io_environment` in a thread clone OVERWRITES inherited `$*OUT`/`$*ERR`/
`$*IN` env entries with fresh default handles — a Raku-compat bug that also
costs four handle creations per task. The fix direction: in the thread-clone
path, create the default handles ONLY for names not already present in the
cloned env (the parent's referenced handles are already carried over by the
`cloned_handles` block in `clone_for_thread_excluding`). This intersects TAP
subtest output-ordering machinery (`OutputSink`, `shared_thread_output`), so
treat it as its own investigation: write `t/start-inherits-dynamic-out.t` from
the oracle above first, then make it pass without breaking
`t/thread-*.t` / `t/subtest*.t` / `roast/S17-promise/start.t`. If output
ordering breaks, stop and record findings in the ticket.

## 5. Per-PR protocol

Standard repo rules apply (feature branch, `gh pr create`, auto-merge with
`--merge`, verify mergeable immediately, background CI watch — see CLAUDE.md).
Additionally for every slice in this campaign:

1. Quote the before/after bench numbers (median of 5, release) and the
   before/after `MUTSU_VM_STATS` counter lines in the PR description.
2. Confirm `worker-pool: tasks=4000 spawns=3 warm_reuses=3997` stays intact on
   the bench (a regression there means the slice broke pool reuse, not clone
   cost).
3. Run the slice's listed targeted tests locally; leave full roast to CI.
4. Never quarantine a test that starts failing — a cross-thread state leak from
   this campaign is a real bug by definition (see "Known flaky tests" triage).

## 6. Stop-and-ask conditions

Stop and ask the user (do not improvise) when:

- `registry_cow_clones` on the bench is per-task rather than ~0 (slice 1).
- Any S17/S02 roast file fails differently across two runs (flake shape) after
  your change — that is the incomplete-invalidation trap; revert to the last
  green state and report.
- Slice 5 step B or slice 6 is next — both are gated on explicit review.
- A slice needs to touch `shared_store.rs` seeding SEMANTICS (not just its map
  type or traversal) — the ADR-0010 invariants live there.
