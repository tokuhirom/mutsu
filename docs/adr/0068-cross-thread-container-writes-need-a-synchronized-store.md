# ADR-0068: A cross-thread aliased container write needs a synchronized store, not a name-keyed lane

- Status: **Proposed**
- Date: 2026-09-05
- Relates to: [ADR-0001](0001-gc-strategy-and-phasing.md) §7 (layer 3c),
  [ADR-0013](0013-container-interior-mutability-cellvalue.md) §1.3-2 / §3 / §5 Q2,
  [ADR-0010](0010-cross-thread-lexical-sharing-scope.md),
  [ADR-0039](0039-container-lexicals-resolve-lexically.md) §8.6
- Supersedes nothing. Fires the revisit condition that ADR-0013 §5 Q2 set for itself.

## 1. Context

`crate::gc::gc_contents_mut` (`src/gc/gc_ptr.rs:786`) is the codebase's single
aliased-container-write primitive: given a `Gc<T>` whose strong count is greater than one it
hands out a `&mut T`, so a write through one alias is visible through every holder of the
node. That is how mutsu implements Raku container identity, and it is correct on one thread.
Its own `# Safety` clause names the gap:

> ... concurrent structural mutation from another thread remains routed through the
> synchronized shared-store lanes (the narrow cross-thread race deferred to ADR-0001 layer 3c).

There are **149 call sites** and none of them establishes that. The lanes
(`runtime/runtime_shared_vars.rs` — `assign_array_elem_to_shared_var`, `shared_array_elem_set`,
`push_to_existing_shared_var`, `__mutsu_atomic_arr::`) are keyed by **variable name** and seeded
from a spawning frame's env walk (`clone_for_thread_excluding`). A container that becomes
aliased by two VM threads any other way is written through `Vec::resize` / `HashMap::insert`
concurrently, which is a double free or a use-after-free.

ADR-0013 §5 Q2 resolved to defer this, with an explicit revisit condition:

> **Cross-thread race: defer to layer 3c.** ... Revisit only if gc-stress/S17 surfaces an actual race.

**That condition has now fired twice.** `news/2026-09/supply-act-serialization-and-the-concurrency-crash-cluster.md`
root-caused a months-old CI crash cluster (glibc corrupted-chunk abort in `__libc_free`,
SIGSEGV, both with `Failed: 0`) to exactly this hazard reached through a `Supply.act` tap's
captured `my @seen`; and the route audit in §3 below reproduces the same corruption through
routes that the `.act` fix cannot cover. ADR-0013 §1.3-2's premise — *"the `gc_contents_mut`
sites are overwhelmingly same-thread aliased writes, not live cross-thread races"* — is
measurably wrong for any program that shares one container across taps or `.then` callbacks.

### 1.1 A calibrated reproduction harness (this is the reusable part)

The previous investigation spent six sessions failing to reproduce, and its recorded next
steps (memcheck, then helgrind) were both dead ends. The harness that works is:

- the **real workload**, run many times as **separate processes**, under the `gc-stress` job's
  environment (`MUTSU_GC=on MUTSU_GC_EVERY_CANDIDATE=1024 MUTSU_GC_VERIFY=1`), on a
  `--profile profiling` build;
- **CPU oversubscription is the necessary ingredient, not concurrency.** At 8-way on 12 cores
  the pre-fix `roast/integration/advent2014-day05.t` was 0/64 and 0/240 — clean. At **24-way on
  12 cores** the same binary and file failed 6/960 within seconds per batch. Every earlier
  "clean" measurement in this area was taken below the oversubscription threshold and means
  nothing.
- **mutsu's own `Vec` bounds check and glibc's allocator are the detectors.** Observed failure
  shapes in this session: `free(): double free detected in tcache 2`,
  `double free or corruption (out)` with a core dump, `double free or corruption (fasttop)`,
  SIGILL, and silent lost updates.

Negative results worth keeping (they narrow the next attempt):

- `valgrind --tool=memcheck` — 0 errors on a demonstrably racing workload (prior session). It
  serializes threads onto one core, which suppresses exactly the interleaving this class needs.
  **Do not start here.**
- `valgrind --tool=helgrind` — could not symbolize the optimized binary or unwind a frame; its
  1000 contexts were unusable (prior session).
- **AddressSanitizer** reports nothing, but its ~10x slowdown widens the window; it is a
  *scheduler*, not a detector, for this bug.
- **A hand-written minimal probe is not a substitute for the real file.** Five standalone
  probes written from the racing test's own source — same three writers, same
  `sleep rand`, same 20 emitters, at file scope and in a loop — were clean over 1440+ block
  executions at full power *with the fix disabled*, because they took the **synchronized lane**
  instead of the racing path (see §2). Shrinking the repro silently changed which store path ran.

### 1.2 The path oracle — how to tell exposure from coverage in one command

Rather than guessing, ask the binary which store path a workload takes. `rust-gdb`'s ignore
counter turns a breakpoint into a free call counter, with no rebuild:

```bash
MUTSU_FUDGE=1 rust-gdb -batch \
  -ex 'break src/vm/vm_var_assign_index_named.rs:2353' \
  -ex 'break mutsu::runtime::Interpreter::shared_array_elem_set' \
  -ex 'ignore 1 1000000' -ex 'ignore 2 1000000' \
  -ex 'run' -ex 'info breakpoints' \
  --args ./target/debug/mutsu <file>
```

Breakpoint 1 is the unsynchronized aliased element store (the site the crash backtrace named);
breakpoint 2 is the synchronized lane. `already hit N times` on 1 with nothing on 2 means the
workload is **exposed**. This oracle is deterministic, costs one debug run, and settled every
route in §3 in minutes — where the stress harness needs hundreds of runs to say the same thing
probabilistically.

## 2. The actual root cause is narrower and more specific than "149 unsynchronized sites"

The lane is not merely *incomplete*; it **hands the write to a mechanism it believes is
synchronized and is not.** `assign_array_elem_to_shared_var` bows out here
(`runtime/runtime_shared_vars.rs:181-186`):

```rust
// See `assign_hash_elem_to_shared_var`: an array already boxed into a
// shared `ContainerRef` cell is already shared through the Mutex; let
// the general assignment path write through it.
if matches!(self.env.get(key).map(|v| v.view()), Some(ValueView::ContainerRef(_))) {
    return None;
}
```

`ContainerCell` (`src/value/mod.rs:429`) is `{ value: Mutex<Value>, constraint: Mutex<...>, ... }`.
**That `Mutex` protects the cell's `Value`, not the container the `Value` points at.** The
element store clones the inner `Gc<ArrayData>` out from under the lock, releases it, and then
performs the aliased in-place mutation — `Gc::strong_count(items) > 1` ⇒
`gc_contents_mut(items)` ⇒ `autoviv_resize` ⇒ `Vec::resize`
(`vm/vm_var_assign_index_named.rs:2349-2357`) — with no lock held at all. The comment's
"already shared through the Mutex" is a **false premise in the code**, and it is the single
condition that fired on the confirmed-racing workload (verified with the §1.2 oracle: the
`return None` at line 185 fired 21/21 times).

A container reaches that state whenever the closure machinery boxes it into a shared cell —
most commonly because a **named sub closes over it** (ADR-0024's unit-lexical cells). In
`roast/integration/advent2014-day05.t` that is literally the first line of the file's first
block, `sub print($a) { @seen.push: $a }`, and it is why the racing shape could not be shrunk:
removing that sub moved the workload back onto the synchronized lane.

So the exposure surface is not "any of 149 sites" — it is **the set of ways a container becomes
cross-thread reachable while the name lane declines**, and the lane declines for five known
reasons: the name is not a plain lexical `@`/`%` (attributes, twigils); the name is masked as
re-declared; **the env entry is a `ContainerRef` cell** (this ADR's finding); the container was
never in a spawning frame's env; or the write is not name-keyed at all (`$obj.attr[$i]`,
`%h<k>[$i]`, a container returned from a method).

## 3. Route audit (measured this session)

Every route was probed in the day05 idiom — three writers (`=`, `~=`, `//=`) into one
container, 20 concurrent producers with `sleep rand` — and classified with the §1.2 oracle,
then confirmed with the §1.1 stress harness at 24-way on 12 cores.

| # | Route | Oracle: unsynchronized / lane | Stress result | Verdict |
|---|---|---|---|---|
| 0 | `Supply.act` tap captures (**fixed** by #7336) | — | 0 / 240 after the fix; 6 / 960 with the fix disabled, incl. `double free or corruption (fasttop)` + SIGILL | **Closed** — and this is the harness calibration |
| 1 | **plain `.tap` callback captures** | 20 / 0 | **4 / 960**, incl. `free(): double free detected in tcache 2` and `double free or corruption (out)` with core dumps | **RACES.** Not fixable by locking dispatch — `.tap` has no serialization guarantee in Raku |
| 2 | **`Promise.then` combinator callback captures** | 21 / 0 | **3 / 240**, lost updates | **RACES** |
| 3 | Object attribute (`has @.seen is rw`) written from two threads | 0 / 0 — reaches **neither** probed aliased-store site, nor `gc_data_mut`, nor the computed-attr sites | 1 clean run | **Unresolved** — the write takes some other path; needs its own trace before it can be called covered or exposed |
| 4 | `Thread.start` bodies (spawn via block-less `clone_for_thread`) | 21 / 0 | run incomplete at wind-down | **Exposed** on the oracle; same site as routes 1/2 |
| 5 | `Channel.Supply` tap captures | 20 / 0 | fails on a *single* run | **Exposed** on the oracle, but the single-run failure is a separate, deterministic Channel-supply delivery bug (values dropped/misordered), which must be fixed first before this route's race is measurable |

Route 1 is the important one: it is the same shape as the fixed `.act` route, it races at the
same rate (0.42% vs 0.63% per run — statistically indistinguishable), it produces genuine
allocator corruption, and **the `.act` remedy cannot be applied to it**, because Raku
deliberately gives `.tap` no serialization guarantee. Serializing `.tap` dispatch would be a
private-dialect divergence, not a fix.

Routes 1, 2 and 4 all corrupt through the **same** site (`vm_var_assign_index_named.rs:2353`),
which is what makes a store-side remedy tractable: they are three doors into one room.

### 3.1 The unexplained `S17-procasync/stress.t` SIGSEGV

CI run 30590633128 (2026-07-30) killed the interpreter in the rakudo#3299 block (1200
`Proc::Async` instances inside a `react`). It has never reproduced (~130 targeted runs across
four configurations, plus this session's). It has no `Supply.act`, and `react`/`whenever`
already had a serialize group, so the `.act` fix does not obviously cover it. **This audit does
not explain it.** What it does supply is a reason the earlier hunts failed — every one of them
ran below the oversubscription threshold §1.1 identifies — and a cheap next step: run that file
under the §1.1 harness at 24-way, and run the §1.2 oracle over it to see whether its containers
are on the lane at all. Left recorded, not chased.

## 4. Decision (proposed)

**Adopt remedy (B) — synchronize the store at the container, gated by (C) — and reject (A).**
Concretely, in this order:

1. **(C) A process-global "more than one VM mutator thread is live" flag** (`AtomicBool`,
   `Relaxed` load), set on the first spawn and never cleared. Every synchronization added below
   is a no-op behind it, so a single-threaded program pays one relaxed load per aliased write.
   This is a prerequisite, not an optimization: it is what makes (B) affordable.
2. **(B) Repair the false premise in §2 first, as the smallest provable slice.** A container
   reached through a `ContainerRef` cell must have its structural mutation performed **while
   holding that cell's own lock**, or the lane must stop deferring to it. This is one lock,
   already owned by the aliasing edge, taken at a site that the code already documents as
   synchronized. Acceptance: routes 1, 2 and 4 go to 0 failures under the §1.1 harness at
   24-way, and their §1.2 oracle shows the write no longer racing.
3. **Then widen from there** to the remaining four lane-decline reasons in §2, each with its own
   oracle-classified probe and stress acceptance, rather than as one 149-site sweep.

### Why not (A), "synchronize the primitive"

`gc_contents_mut` returns a `&mut T` that **outlives the call**, so a guard cannot live inside
it; the lock would have to be taken by each of the 149 callers around its own mutation region.
That is not merely a large mechanical diff — it carries a deadlock-analysis obligation at every
site that can call user code while holding the lock. Quantifying that obligation, over the 149
sites:

| Bucket | Files | Character |
|---|---|---|
| VM store/assign paths | `vm/vm_var_assign_index_named.rs` (13), `vm_var_assign_ops.rs` (8), `vm_exec_dispatch.rs` (8), `vm_var_assign_post_incdec.rs` (4), `vm_var_assign_computed_attr.rs` (4), `vm_var_assign_coerce.rs` (3), `vm_var_index_tracking.rs` (2), `vm_var_elem_mutate.rs` (2) | **Can call user code while holding it** — `Proxy` STORE, `where` constraints, `AT-POS`/`ASSIGN-POS` overloads, tied accessors |
| Method dispatch | `runtime/methods_mut_dispatch.rs` (5), `vm_call_method_mut_ops.rs` (3), `vm_hyper_method_ops.rs` (4), `vm_call_method_ops.rs` (1), `runtime/methods_call_dispatch.rs` (2), `methods_classhow_dispatch.rs` (2) | **Can call user code** — the mutation region *is* a dispatch |
| Collection builtins | `runtime/builtins_collection_deepmap.rs` (4), `builtins_multidim_assign.rs` (2), `builtins.rs` (1), `utils/shaped.rs` (2) | `deepmap`/`deepmap`-alikes take a **user callback** |
| Value-layer mechanics | `value/value_methods_a.rs` (8), `value_methods_b.rs` (14), `entry_path.rs` (7), `native_backing.rs` (4), `aliased_mut.rs` (4), `native_cache_shapes.rs` (2), `view.rs`, `value_gc.rs`, `value_buf.rs`, `sync_cell.rs`, `mod.rs` | Mostly leaf; the safe part of the diff |
| NativeCall / NQP | `runtime/nativecall.rs` (2), `cstruct_layout.rs` (2), `nqp_ops.rs` (1), `methods_mixin_what_cache.rs` (1) | Can re-enter through an **outbound callback** (ADR-0063) |

**Roughly 45 of the 149 sites sit in buckets that can re-enter user code**, and mutsu's
identity-write pattern is deliberately re-entrant and aliased (`@a[$i] = @a[$j]`, a `Proxy`
whose STORE touches the same container). A non-reentrant lock there deadlocks; a reentrant one
does not actually exclude the second writer. This is the same objection ADR-0013 §3 raised
against option 2a (per-container `RwLock`) and it has not weakened. (A) is rejected on the
record.

### Why (B) and not "widen the lanes"

The finding's remedy 2 — register any container that *becomes cross-thread reachable* — is the
attractive framing, but "becomes cross-thread reachable" is a **whole-heap reachability
question mutsu has no answer to today**: it would need either a write barrier on every store of
a container into anything a worker can see, or a mark phase over each spawn's transitive
capture set. ADR-0039 §8.6 already documents why the cheap approximation fails — a routine the
block merely *calls* can reach a container no analysis over the block's own free variables can
see, which is why lane entries are published for everything and then retired as *transient*
rather than withheld. Widening that to full reachability is a GC-scale change, and it lands in
the territory ADR-0001 keeps rejecting.

(B) sidesteps the question: it does not ask *which* containers are shared, it makes the *write*
safe wherever the aliasing edge already carries a lock. §2 shows the first and largest such edge
already exists and is merely unused.

### What is explicitly NOT proposed

Level 2 — a full VM redesign for a MoarVM-style precise moving/tracing GC — remains rejected per
ADR-0001 §4.1 / CLAUDE.md. Nothing measured here is a *refcount ceiling*; it is a missing
mutual-exclusion edge. This ADR does not argue that case and must not be read as opening it.

## 5. Consequences

- ADR-0013 §5 Q2's deferral is **spent**: gc-stress/S17-class workloads have surfaced an actual
  race, with allocator corruption, on routes the `.act` fix cannot reach. The `# Safety` clause
  on `gc_contents_mut` and the header of `value/aliased_mut.rs` should stop describing the
  cross-thread race as "already lane-governed" and instead point at §2 and §3 of this ADR.
- The comment at `runtime_shared_vars.rs:181-186` ("already shared through the Mutex") is
  **wrong as written** and should be corrected even before a fix lands, because it is the reason
  the hole was invisible.
- The §1.1 harness and the §1.2 oracle become the standing acceptance gate for this class. A
  future "we could not reproduce it" is only meaningful if it was measured at oversubscription
  *and* the oracle confirms the workload is on the racing path.

## 6. Open questions

1. **Is the `ContainerCell` lock the right lock to hold across a structural mutation?** It is a
   `Mutex<Value>` whose guard is currently taken only to read/replace the `Value`. Holding it
   across the mutation excludes other writers through *the same cell*, but not a holder that
   obtained the `Gc<ArrayData>` by another route. Slice (B)-2's acceptance must therefore be the
   stress harness, not the argument.
2. **Route 3 (object attributes) is unclassified.** The probe reached none of the aliased-store
   sites this session probed. Trace it before assuming either coverage or exposure.
3. **Route 5 is blocked by a separate Channel-supply delivery bug** (values dropped/misordered
   on a single, unloaded run). Fix that first; until then this route's race rate is unmeasurable.
4. **Does the (C) flag belong in `gc_contents_mut` itself** (one relaxed load at the primitive,
   for every site at once) or at each synchronized store? Measuring the primitive-level load
   against the bench CI is the deciding datum, and it was not taken this session.
