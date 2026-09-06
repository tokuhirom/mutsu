# `gc_contents_mut` aliased writes are unsynchronized across VM threads

This is the residue of `todo/deep/procasync-stress-segv.md`, closed on 2026-09-05 by
finding and fixing the path that actually crashed
(`news/2026-09/supply-act-serialization-and-the-concurrency-crash-cluster.md`).

**Design status: [ADR-0068](../../docs/adr/0068-cross-thread-container-writes-need-a-synchronized-store.md)
is `Accepted`, and its §4 steps 1 and 2 are IMPLEMENTED** (2026-09-06,
`news/2026-09/celled-container-cross-thread-store-exclusion.md`). Read ADR-0068 §7
before anything else here: it records three premises of the original design that
measurement contradicted, including the one this file used to lead with. This
file is now only the *open remainder* — §4 step 3.

## What is closed

The celled-container route. An element store that reaches its container through a
shared `ContainerRef` cell now takes a cell-keyed stripe lock
(`src/value/container_lock.rs`), and so do the read chokepoints
(`Value::with_deref` / `into_deref`). ADR-0068's routes 1 (`.tap` captures) and 4
(`Thread.start` bodies) went from 17/96 and 64/64 failures to 0/96 and 0/64; the
named-sub array and hash probes went from 93/96 and 95/96 to 0/240 at 24-way.
`t/concurrent-celled-container-store.t` pins all four shapes.

Two corrections that the fix turned up, and that anyone continuing this work needs:

- **The shared thing is the CELL, not the container node.** Twenty threads writing
  one celled array reach *thirteen* distinct `Gc<ArrayData>` addresses, because
  `Gc::make_mut` copies an aliased node. A node-keyed lock excludes nothing, and
  measurably did not move the failure rate.
- **The dominant race was writer-versus-reader.** The store derives a raw pointer
  into the cell's slot and releases the cell's `Mutex`; the readers take that same
  `Mutex` to clone the inner `Value` out. So they never excluded each other, and a
  reader could take a refcount on a node the writer had already dropped. Locking
  only the write side left 13/96 failures standing.

## What is still open — ADR-0068 §4 step 3

`gc_contents_mut` has 149 call sites and the exclusion is applied at three of them.
The remaining exposure is the rest of the ways a container becomes cross-thread
reachable while the name-keyed lane declines (ADR-0068 §2 lists five):

- the name is not a plain lexical `@`/`%` (attributes, twigils);
- the name is masked as re-declared;
- the container was never in a spawning frame's env;
- the write is not name-keyed at all (`$obj.attr[$i]`, `%h<k>[$i]`, a container
  returned from a method);
- ~~mutating *methods* rather than element stores~~ — **done**, see above.

Each wants its own oracle-classified probe and its own stress acceptance, per
ADR-0068 §4 step 3 — not one 149-site sweep. The evidence so far is that this
class has **three funnels**, not 149 sites: the named element store, the
attribute-rooted element store, and the mutating method. Expect a new route to
arrive at one of them; probe before assuming it needs a fourth.

Three specific loose ends from the route audit:

- ~~**Route 3 (object attributes) is unclassified.**~~ **CLASSIFIED AND HALF
  FIXED (2026-09-06, ADR-0068 §8).** It was the worst route in the ADR: an
  element store through an array attribute corrupted the heap on **96 of 96**
  runs. The earlier probe missed because it looked for the *aliased-store* sites
  and this route uses none of them — `$obj.attr[$i] = v` lowers to
  `__mutsu_index_assign_method_lvalue`, whose whole body writes through the
  container the accessor hands back, so the exclusion goes on that container.
  Element stores (array and hash) are now 0/240 at 24-way.

  **A mutating METHOD on an attribute container was a third funnel**, also
  fixed: `$obj.attr.push($v)` raced at 95/96 and goes through neither store
  funnel. Four breakpoint probes came back cold; the ADR-0068 §1.2 oracle showed
  it arriving as an ordinary VALUE dispatch (`exec_call_method_op` ->
  `call_method_with_values`), and excluding a small allowlist of mutator method
  names there takes it to 0/240 at 24-way.
- **Route 5 (`Channel.Supply` tap captures)** is exposed on the path oracle but
  blocked behind a separate deterministic Channel-supply delivery bug that
  drops/misorders values on a single unloaded run. Fix that first.
- **`roast/S17-procasync/stress.t` SIGSEGV** (CI run 30590633128, 2026-07-30, the
  rakudo#3299 block) has never reproduced and is still unexplained. Run it under
  the §1.1 harness at 24-way and the §1.2 oracle to see whether its containers are
  on the lane at all.

## How to reproduce this class cheaply — read this before building a harness

ADR-0068 §1.1's requirement of a `--profile profiling` build, the `gc-stress`
environment and 24-way oversubscription is **not** necessary, and §7.1 records why.
The discriminator is which store path the workload takes, not how loaded the box
is. Historically a `start` block that mentioned the container lexically was
excluded from celling by `thread_escaping_captures` and landed on the name-keyed
lane — which is what made five earlier hand-shrunk probes come back clean. That
exclusion is retired (2026-09-07, ADR-0068 §9), so such a block now takes the
guarded cell path; the probe below still discriminates, because it reaches the
container through a **named sub the thread body merely calls** (a route the
capture analysis cannot see, ADR-0039 §8.6). On an ordinary debug build with the
GC off it failed on the first run before the guards landed:

```raku
{
    my @a;
    sub put-it($i) { @a[$i] = 1 }
    await (^20).map: -> $t { start { for ^50 -> $k { put-it($t * 50 + $k) } } };
    say @a.grep(*.defined).elems;   # want 1000
}
```

Use ADR-0068 §1.2's `rust-gdb` ignore-counter oracle to confirm a new probe is on
the racing path before trusting a clean result. Do not start with
`valgrind --tool=memcheck` (it serializes threads onto one core and reports
nothing) or helgrind (cannot symbolize the optimized binary).

## A follow-up worth measuring

The read-side guard sits in `Value::with_deref` / `into_deref`, which are hot. It
is gated on "a VM mutator thread has been spawned", so a single-threaded program
pays one `Relaxed` load — but a *threaded* program now takes a mutex on every
celled-container read. No slowdown showed up in `make test`, and nothing measured
it directly. If a concurrency-heavy benchmark regresses, that gate is the first
place to look, and per-cell striping granularity is the first knob.
