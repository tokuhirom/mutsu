# A stale-spawned thread can no longer publish an old value as the authoritative atomic

A Tier-S soundness bug is fixed: an atomic operation on a plain (untyped)
cross-thread scalar used to read the acting thread's *private* `env` snapshot
as "the current value" and then publish it process-wide. A thread spawned
before a write it never observed could therefore overwrite a strictly newer
value with older data — a lost update — and, in the three-argument `cas`
shape, could make a compare-and-swap **succeed against a value that was not
there**. The decision and its rejected alternatives are recorded in
[ADR-0062](../../docs/adr/0062-atomic-lane-anchors-to-the-published-value.md);
`t/atomic-lane-stale-thread-anchor.t` pins it.

## The two lanes, and which one was broken

mutsu resolves an atomic scalar through one of two mechanisms. A binding that
lives in a shared `ContainerRef` cell — `my atomicint $x`, an instance
attribute — *is* the atomic primitive: its mutex serializes the
read-modify-write and every alias, including a spawned thread's clone, holds
the same cell. That lane was always correct.

Everything else — notably a plain `my $x` that some code `cas`es — goes
through the legacy name-keyed lane: a process-global indirection in the root
`shared_vars` store, `__mutsu_atomic_name::<name>` pointing at
`__mutsu_atomic_value::<N>`, whose whole reason to exist is to give one
process-wide answer to "what is the current value of this atomic". It did not.

## The real root cause, and how the filed one was wrong

The finding was originally recorded as a *resurrection* bug: `reset_atomic_var_key`
retires the lane on every plain scalar assignment, and a thread whose `env`
predates that reset would find nothing in its own mirror or in the cleared
store, allocate a brand-new mapping, and thereby resurrect a lane another
thread believed was gone. That description is accurate as far as it goes, and
`rust-gdb` confirmed the sequence exactly — breakpoints on the allocation site,
on the reset's removal, and on the blanket reconcile's update push showed
`Thread 2 "mutsu-main"` allocating `__mutsu_atomic_value::1`, retiring it at
`$x = 4`, `Thread 3 "pool"` allocating `__mutsu_atomic_value::2`, and the
reconcile at `$pB.result` resolving the dirty bare name `x` through the
resurrected mapping and clobbering `5` with `1`.

But reducing the repro showed the framing was too narrow. **Delete the priming
`cas` and the bug still fires**, with no mapping to retire and no resurrection
at all — the worker simply creates the *first* generation of the lane. The
retire/resurrect cycle is a symptom. The cause is one fallback in
`atomic_current_value`:

```rust
let current = shared.get(value_key).cloned()
    .or_else(|| self.env.get(name).cloned())   // <- a private snapshot
    .unwrap_or(Value::NIL);
```

A freshly created lane generation has nothing under its `value_key`, so it
bootstraps from the acting thread's `env` — a snapshot taken when that thread
was cloned, arbitrarily old — and that value immediately becomes the
process-wide authority. Alongside it sat a second unsoundness:
`atomic_value_key_for_name` trusted its own `env` mirror of the mapping
*before* consulting the store, and since `reset_atomic_var_key` can only reach
the `env` of the thread that ran the assignment, other threads kept handing out
a retired slot that nothing writes any more.

## The fix

The lane is a process-global mechanism, so both its mapping and the value a new
generation starts from now come from process-global state:

- **The root store is the sole authority for the mapping.** The `env` entry is
  demoted to a pure mirror, refreshed from the store rather than consulted
  ahead of it. A read lock keeps a plain read of an atomic-touched variable off
  the writer lock.
- **A new generation is anchored to the published value.** When (and only when)
  the lookup actually creates a generation, the new slot is seeded from the
  process-global published value for the bare name, using precisely the
  definition of "published" the rest of the shared-var machinery already
  applies: the shared store is active, the name is not in
  `thread_redeclared_vars` (a re-declared name is a fresh frame-local binding),
  and the name is *dirty* — `clone_for_thread`'s spawn-time seeding
  deliberately does not mark a key dirty, so a merely-seeded entry carries no
  more information than the thread's own `env` and must not displace it. With
  no published value, the `env` fallback stands, which is correct because
  nothing was ever published.

The originating record proposed either a generation counter to detect the stale
re-creation, or refusing to let a stale thread create a mapping. Neither was
taken. A counter detects staleness but does not repair it — a thread ruled
stale still has to read *something*, and its `env` is all a counter offers —
and it cannot reach the no-priming-`cas` shape at all, since there is no
previous generation to compare against. Refusal lands on the same question and
answers it the same way. Re-anchoring unconditionally at creation reaches the
intent of both without classifying threads at all, so no necessarily-incomplete
staleness oracle is involved: per CLAUDE.md's gain/risk definitions, a
mechanism whose correctness does not depend on an enumeration of thread states
cannot go flaky when that enumeration turns out to be incomplete.

Fixing the *read* rather than the reconcile was deliberate. The reconcile was
doing its job — propagating a worker's write back to the awaiting thread; the
defect was that the worker's write was computed from a value it was never
entitled to call current. A reconcile-side veto would have left `cas` itself
still comparing against a stale value and still returning the wrong answer to
the worker.

## A durable lesson: the filed repro's oracle was never verified

The repro recorded with the finding used `my $x = [1,2,3]` with
`cas $x, -> @c { @c }` and asserted raku answers `1 2 3 4 5`. **That program
hangs under `raku`** (exit 124 on a 30-second timeout), so its expected value
was never established — an unverified oracle presented as fact. It was
replaced with a plain-scalar version with no `Array`, no `Seq` and no cell,
whose oracle is confirmed (`raku`: `5`, mutsu before the fix: `1`, three runs
of three). The replacement is strictly better evidence: it confirms the
finding's own claim that the bug "reproduces with plain scalars and no cell
involved at all", and it is what made the too-narrow resurrection framing
visible in the first place. Verify the reference implementation actually
*runs* the repro before trusting an expected value written next to it.

The `Channel`-forced ordering is essential in every case: the bug needs a
thread that starts *before* a write and executes its own atomic op *after* it,
which ordinary sequential-`await` code never produces. That is exactly why the
pre-existing pins — `t/cross-thread-shared-var-writeback-coherence.t`,
`t/atomic-cell-shape-refusal-symmetry.t`, `t/lock.t` — all passed while this
was broken: none of them exercises a stale-spawned, still-running thread.

## Verification

Six shapes were checked against `raku`: the primary 2-argument `cas`, the
no-priming-`cas` variant, the 3-argument `cas` (where mutsu previously returned
`1` and swapped in `99` against a real value of `5`), three stale-spawned
threads rather than two, and the two cell-lane `atomicint` shapes (`⚛++` and
`atomic-fetch-add`), which were already correct and are pinned for symmetry.
All match. The four legacy-lane repros ran 60 times each after the fix:
240/240 correct, zero failures, where all four had been wrong on every run
before. The new pinning test passes under `raku` itself as well as under
mutsu.

Two residuals are recorded rather than silently left: `builtin_cas_var` still
resolves its `value_key` once at entry instead of inside the retry loop
(ADR-0062 §"Not addressed", marked with a `// TODO:` at the site; the program
that triggers it is already racy by Raku's own rules), and retired
`__mutsu_atomic_value::N` keys accumulate in the process-global
`shared_vars_dirty` set — inert, but tracked as
`todo/tickets/retired-atomic-value-keys-leak-into-shared-vars-dirty.md`.
