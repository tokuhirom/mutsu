# The cross-thread container-write hazard, audited: three live routes and a calibrated repro

`todo/deep/gc-contents-mut-cross-thread-aliased-writes.md` was the residue left behind when
`Supply.act` was made to serialize (`news/2026-09/supply-act-serialization-and-the-concurrency-crash-cluster.md`):
one crashing route was closed, but the general hazard — 149 `gc_contents_mut` call sites, none of
which establishes the primitive's own cross-thread safety contract — was neither enumerated nor
designed. This session enumerated it, measured it, and wrote the design as
[ADR-0068](../../docs/adr/0068-cross-thread-container-writes-need-a-synchronized-store.md)
(`Proposed`).

## A reproduction harness that actually reproduces

The single most reusable result is a correction to how this class is hunted. Six earlier sessions
and roughly 130 targeted runs had failed to reproduce the crash, and the standing advice was to
reach for a memory checker. Both parts were wrong.

**CPU oversubscription is the necessary ingredient, not concurrency.** Running the *known-racing*
pre-fix workload eight ways on twelve cores gave 0 failures in 64 runs and 0 in 240 — clean, and
exactly the shape of every historical "could not reproduce". Running the same binary and the same
file **24 ways on twelve cores** produced 6 failures in 960 runs, within seconds per batch, with
`double free or corruption (fasttop)`, a SIGILL, and lost updates. Every earlier clean measurement
in this area was taken below the threshold and carries no information.

The harness is otherwise plain: the *real* workload, run as separate processes under the
`gc-stress` job's environment on a `--profile profiling` build, with mutsu's own `Vec` bounds check
and glibc's allocator as the detectors. `valgrind --tool=memcheck` remains useless here (it
serializes threads onto one core and suppresses the interleaving); helgrind cannot symbolize the
optimized binary; AddressSanitizer reports nothing and is useful only because its ~10x slowdown
widens the window.

A second correction: **a hand-shrunk probe is not a substitute for the real file.** Five standalone
probes written from the racing test's own source — same three writers, same `sleep rand`, same
twenty emitters — were clean over 1440+ block executions at full power *with the fix deliberately
disabled*. Shrinking had silently moved them onto the *synchronized* store path. That is what made
the earlier hunts feel like noise.

## A path oracle, so exposure is a fact and not a guess

Rather than infer coverage from a probabilistic stress result, ask the binary which store path a
workload takes. `rust-gdb`'s ignore counter turns a breakpoint into a free call counter with no
rebuild: break on the unsynchronized aliased element store
(`vm/vm_var_assign_index_named.rs:2353`) and on the synchronized lane
(`Interpreter::shared_array_elem_set`), set both ignore counts high, run, and read
`already hit N times`. One debug run classifies a workload as exposed or covered. This settled
every route in minutes where the stress harness needs hundreds of runs to say the same thing.

## The root cause is a false premise in the code, not 149 anonymous sites

`assign_array_elem_to_shared_var` declines the synchronized lane when the variable's env entry is a
`ContainerRef`, on the recorded premise that such an array *"is already shared through the Mutex"*.
It is not. `ContainerCell`'s `Mutex` protects the cell's `Value` — the pointer — not the container
the `Value` points at. The element store clones the inner `Gc<ArrayData>` out from under that lock,
releases it, and then performs `gc_contents_mut` → `autoviv_resize` → `Vec::resize` with no lock
held at all. On the confirmed-racing workload that `return None` fired 21 times out of 21.

A container reaches that state whenever the closure machinery boxes it into a shared cell — most
commonly because a **named sub closes over it**. In `roast/integration/advent2014-day05.t` that is
literally the first line of the file, `sub print($a) { @seen.push: $a }`, and it is why the racing
shape resisted shrinking: deleting that sub moved the workload back onto the safe lane.

## Route audit

- **plain `.tap` callback captures — RACES.** 4 failures in 960 runs, including
  `free(): double free detected in tcache 2` and `double free or corruption (out)` with core dumps,
  at a rate statistically indistinguishable from the `.act` route that was just fixed. This one
  cannot be fixed the way `.act` was: Raku deliberately gives `.tap` no serialization guarantee, so
  locking the dispatch would be a private-dialect divergence.
- **`Promise.then` combinator callback captures — RACES.** 3 failures in 240 runs, lost updates.
- **`Thread.start` bodies — exposed** on the path oracle, through the same site.
- **`Channel.Supply` tap captures — exposed** on the path oracle, but blocked behind a separate,
  deterministic Channel-supply delivery bug that drops or misorders values even on a single
  unloaded run.
- **Object attributes (`has @.seen is rw`) — unresolved.** The write reaches none of the probed
  aliased-store sites, nor `gc_data_mut`, nor the computed-attribute sites; it needs its own trace
  before it can be called either covered or exposed.

Three of those routes corrupt through the *same* site, which is what makes a store-side remedy
tractable rather than a 149-site sweep.

## What the ADR decides

It rejects "synchronize the primitive" on the record, with the count that makes the objection
concrete: `gc_contents_mut` returns a `&mut T` that outlives the call, so the lock must be taken by
each caller around its own mutation region — and roughly 45 of the 149 sites sit in buckets that
can re-enter user code (`Proxy` STORE, `where` constraints, `AT-POS`/`ASSIGN-POS` overloads,
`deepmap` callbacks, NativeCall outbound callbacks). A non-reentrant lock there deadlocks; a
reentrant one does not exclude the second writer. It also declines "widen the lanes to every
cross-thread-reachable container", because that is a whole-heap reachability question mutsu has no
answer to and ADR-0039 §8.6 already documents why the cheap approximation fails.

What it proposes instead is to repair the false premise above as the smallest provable slice —
perform the structural mutation while holding the cell's own lock, or stop deferring to it — gated
behind a process-global "more than one VM mutator thread is live" flag so single-threaded programs
pay one relaxed atomic load. ADR-0013 §5 Q2 had deferred this cross-thread race with an explicit
revisit condition, *"revisit only if gc-stress/S17 surfaces an actual race"*. That condition has now
fired, with allocator corruption, on routes the `.act` fix cannot reach.

No code landed. The deliverable is the design plus the audit, and in particular the harness and the
oracle, so the next attempt does not spend its first several sessions failing to reproduce.
