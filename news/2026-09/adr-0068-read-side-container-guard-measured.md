# ADR-0068's read-side container guard, measured: no threaded regression, and a shared-cell speedup

ADR-0068 §7 put the cross-thread exclusion on the **read** side as well as the
write side, because the dominant race was writer-versus-reader: the element
store derives a raw pointer into a `ContainerCell`'s slot and releases the
cell's `Mutex`, while readers take that same `Mutex` to clone the inner `Value`
out, so the two never excluded each other and a reader could take a refcount on
a node the writer had already dropped. Locking only the write side left 13/96
failures standing.

That guard therefore sits in `Value::with_deref` / `Value::into_deref`, which
are hot, and a threaded program now takes a striped mutex on every celled
container read. §13.4 closed step 3 by naming the one thing left as a
measurement rather than a correctness question, and
[#7613](https://github.com/tokuhirom/mutsu/issues/7613) tracked it. This is that
measurement.

## The obvious probe measures nothing — a repeat of §1.1's lesson

The first four workloads written for this — four `start` blocks hammering a
captured `my @shared`, a captured scalar, a captured scalar holding an array,
and an attribute-rooted `$b.items[...]` — took **zero** guards. A plain
`@`/`%` lexical named by a `start` block is served by the name-keyed
shared-variable lanes in `runtime/runtime_shared_vars.rs`; it never becomes a
`ContainerRef`, so `with_deref` never sees a cell and the guard is not on its
path at all. Benchmarking those would have produced a clean null result that
meant nothing, which is exactly the trap ADR-0068 §1.1 recorded for the
correctness work ("a hand-written minimal probe is not a substitute for the real
file").

The §1.2 path oracle settles it in one debug run and no rebuild, using gdb's
ignore counter as a free call counter. `stripe_for` is reached only after both
gates in `ContainerStructGuard::acquire` pass, so its hit count is the number of
mutexes actually taken:

```bash
rust-gdb -batch \
  -ex 'break mutsu::value::container_lock::stripe_for' \
  -ex 'break mutsu::value::container_lock::ContainerStructGuard::acquire_for_cell' \
  -ex 'ignore 1 100000000' -ex 'ignore 2 100000000' \
  -ex 'run' -ex 'info breakpoints' \
  --args ./target/debug/mutsu <file>
```

What does reach the guard is a **`:=`-bound** container read across threads:
`my @alias := @data` makes the workers share a cell, and then every read of
`@alias` locks — 160,000 acquisitions for 80,000 loop iterations reading two
bound variables, i.e. 100% of them, with the two breakpoints in lockstep.

## The A/B

Two release binaries off the same tree, differing only in whether
`with_deref`/`into_deref` acquire the guard, run alternately (A/B/A/B, so
thermal and scheduler drift hits both arms), median of 9, on a 4-core container.
Negative means the guard is **faster**.

| workload | celled reads | JIT on | JIT off |
|---|---|---|---|
| 1 thread, never spawned (gate off) | 6.4M would-be | −0.5% | — |
| 1 thread, after one trivial spawn (gate on, uncontended) | 6.4M | **+7.1%** | **+5.3%** |
| 4 workers, 4 independent cells | 3.2M | −0.6% | +3.2% |
| 4 workers, one shared cell | 6.4M | **−12.0%** | −0.5% |
| 12 workers on 4 cores, one shared cell | 4.8M | **−13.0%** | **−9.0%** |
| `benchmarks/bench-threads.raku` as shipped | 1.2M + stores | −3.7% | −1.7% |

On provenance: `CLAUDE.md` requires numbers in documents to come from the bench
CI history, and this table deliberately cannot — the CI builds one binary, and
an A/B needs two. What the bench CI *can* carry is the absolute concurrency
series, which is what `benchmarks/bench-threads.raku` adds below; the deltas
here are local, taken on an idle box with the arms interleaved so drift cancels,
and are reported as deltas rather than absolute times for exactly that reason.

Three things fall out of it.

**The §4 step 1 gate is free, as claimed.** A program that never spawns a VM
mutator thread measures −0.5% — noise — over 6.4M reads that would each have
locked. ADR-0068 §7.4 answered §6 question 4 "by placement rather than
measurement"; the placement is now measured too.

**The cost is real but small, and it is not where the ticket expected it.** The
worst case is not the threaded program: it is the program that spawns one worker
early and then does heavy single-threaded work through a bound container, which
pays the *uncontended* lock on every read for the rest of its life (the gate is
deliberately sticky). +5–7% on a loop that does essentially nothing but celled
reads is the ceiling for that shape; any real program dilutes it with work that
does not touch a cell.

**On genuinely concurrent programs the guard is a net win.** Four workers on one
shared cell run 12% faster *with* it, and oversubscribed 12-on-4 the margin is
13% (9% with JIT off). Serializing the readers on a stripe is cheaper than
letting four cores contend on the cell's own `Mutex<Value>` and on the atomic
refcount of the inner node that `into_deref`'s clone touches. The guard removes
a contention storm it was never designed to remove.

**Per-cell striping granularity — the ticket's named first knob — is not the
lever.** With four independent cells the delta is −0.6% / +3.2%, so 64 stripes
leave no false-sharing cost worth chasing; and where the cost *is* concentrated
(one hot cell) more stripes cannot help, because the exclusion on that cell is
what correctness requires. Nothing to tune.

## What shipped

`benchmarks/bench-threads.raku`, so the bench CI records a concurrency series in
`bench-history.tsv` from the next main push onward — there was none before, which
is why the ticket's "measure against the bench CI history" had nothing to measure
against. It is built out of `:=`-bound containers on purpose, with a comment
saying why: the plain-lexical shape would leave it measuring a path the guard is
not on. Its read half stresses `with_deref`/`into_deref` on one shared cell; its
write half drives the store-side guard through disjoint slots, so the checksum
does not depend on how the workers interleave (mutsu and rakudo agree on it).

No code changed. The read-side guard stays exactly as ADR-0068 §7.4 built it.
