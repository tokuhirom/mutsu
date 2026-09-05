# `gc_contents_mut` aliased writes are unsynchronized across VM threads

This is the residue of `todo/deep/procasync-stress-segv.md`, which was closed on
2026-09-05 by finding and fixing the path that actually crashed
(`news/2026-09/supply-act-serialization-and-the-concurrency-crash-cluster.md`). Read that
entry first: it has the evidence, the measurements, and the negative results from the
memory checkers, none of which is repeated here.

**Design status (2026-09-05): [ADR-0068](../../docs/adr/0068-cross-thread-container-writes-need-a-synchronized-store.md)
is the `Proposed` design for this finding.** It carries the route audit, the calibrated
reproduction harness, the path oracle, the deadlock quantification that rejects
"synchronize the primitive", and the proposed remedy. This file is now the *open work
item*; the ADR is the reasoning. Do not re-derive either from scratch.

## The general hazard

`crate::gc::gc_contents_mut` is, in its own words, *"the codebase's single
aliased-container-write primitive"*: given a `Gc<T>` whose strong count is greater than
one, it hands out a `&mut T` so that a write through one alias is visible through every
holder of the same node. That is how mutsu implements Raku container identity, and it is
correct on one thread.

Its `# Safety` clause already names the gap:

> The caller must ensure that ... concurrent structural mutation from another thread
> remains routed through the synchronized shared-store lanes (the narrow cross-thread
> race deferred to ADR-0001 layer 3c).

There are **149 call sites** and none of them establishes that. The shared-store lanes
(`runtime/runtime_shared_vars.rs`) are keyed by variable name and seeded from a spawning
frame's env walk. Any *other* way a container comes to be aliased by two VM threads
bypasses them entirely, and then two threads can call `Vec::resize` / `HashMap::insert` on
the same allocation at once.

## What the 2026-09-05 audit established

**The root cause is narrower and more specific than "149 unsynchronized sites".** The lane
does not merely fail to cover the container — it *hands the write to a mechanism it
believes is synchronized and is not*. `assign_array_elem_to_shared_var` returns `None` when
the env entry is a `ContainerRef`, on the recorded premise that such an array *"is already
shared through the Mutex"*. `ContainerCell`'s `Mutex` protects the cell's **`Value`**, not
the container the `Value` points at: the element store clones the inner `Gc<ArrayData>` out
from under the lock and then runs `gc_contents_mut` → `autoviv_resize` → `Vec::resize` with
no lock held. On the confirmed-racing workload that `return None` fired 21/21 times. A
container reaches that state whenever the closure machinery boxes it into a shared cell —
most commonly because a **named sub closes over it**. See ADR-0068 §2.

### Route audit (measured, ADR-0068 §3)

| Route | Verdict |
|---|---|
| `Supply.act` tap captures | **Closed** by #7336 (0/240 after; 6/960 with the fix disabled) |
| plain `.tap` callback captures | **RACES** — 4/960, with `free(): double free detected in tcache 2` and `double free or corruption (out)` core dumps. Not fixable by locking dispatch: `.tap` has no serialization guarantee in Raku |
| `Promise.then` combinator callback captures | **RACES** — 3/240, lost updates |
| `Thread.start` bodies | **Exposed** on the path oracle (21/0), same site as the two above |
| `Channel.Supply` tap captures | **Exposed** on the path oracle (20/0), but blocked behind a separate deterministic Channel-supply delivery bug that drops/misorders values on a single unloaded run — fix that first or the race rate is unmeasurable |
| Object attribute (`has @.seen is rw`) | **Unresolved** — reaches none of the probed aliased-store sites, nor `gc_data_mut`, nor the computed-attr sites. Needs its own path trace before it can be called covered or exposed |

Routes 1, 2 and 4 all corrupt through the **same** site
(`vm/vm_var_assign_index_named.rs:2353`) — three doors into one room, which is what makes a
store-side remedy tractable.

## Reproduction harness — this is the reusable part

Previous attempts failed for a measurable reason, and the correction matters more than the
old advice did:

- **CPU oversubscription is the necessary ingredient, not concurrency.** At 8-way on 12
  cores the *known-racing* pre-fix workload was 0/64 and 0/240 — clean. At **24-way on 12
  cores** it failed 6/960 within seconds per batch. Every earlier "could not reproduce" in
  this area was taken below that threshold and means nothing.
- Run the **real workload** as separate processes under the `gc-stress` environment
  (`MUTSU_GC=on MUTSU_GC_EVERY_CANDIDATE=1024 MUTSU_GC_VERIFY=1`) on a `--profile profiling`
  build. mutsu's own `Vec` bounds check and glibc's allocator are the detectors.
- **A hand-shrunk probe is not a substitute for the real file.** Five standalone probes
  written from the racing test's own source were clean over 1440+ block executions at full
  power *with the fix disabled*, because shrinking silently moved them onto the
  **synchronized** lane.
- **Use the path oracle before the stress harness.** `rust-gdb`'s ignore counter turns a
  breakpoint into a free call counter with no rebuild; breaking on
  `vm_var_assign_index_named.rs:2353` (unsynchronized) and
  `Interpreter::shared_array_elem_set` (lane) classifies a workload as exposed-or-covered in
  one debug run. See ADR-0068 §1.2 for the exact command.
- **Do not start with `valgrind --tool=memcheck`** (0 errors on a demonstrably racing
  workload — it serializes threads onto one core) or `helgrind` (cannot symbolize the
  optimized binary). AddressSanitizer reports nothing either; it is useful only as a
  *scheduler*, because its ~10x slowdown widens the window.

## The one instance that is NOT explained by the fixed path

`roast/S17-procasync/stress.t` died of SIGSEGV once, on CI run 30590633128 (2026-07-30), in
the rakudo#3299 block that starts 1200 `Proc::Async` instances inside a `react`. It has
never reproduced — ~130 targeted local runs across four configurations, plus the 2026-09-05
session's — and it has no `Supply.act` tap, so the `.act` fix does not obviously cover it.
The 2026-09-05 audit does **not** explain it either. What it adds is a reason the hunts
failed (all of them ran below the oversubscription threshold above) and a cheap next step:
run that file under the 24-way harness, and run the path oracle over it to see whether its
containers are on the lane at all. Treat it as an open, unexplained instance of the general
hazard rather than as its own investigation.
