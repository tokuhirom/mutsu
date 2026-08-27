# ADR-0062: A generation of the legacy atomic lane anchors to the published value, and the root store is the lane's only authority

- Status: Accepted (implemented)
- Date: 2026-08-27
- Related: ADR-0010 (cross-thread lexical sharing is scoped to a spawn lineage),
  ADR-0013 (container interior mutability), ADR-0025 (captured scalar cells)
- Addresses: `todo/deep/stale-env-thread-can-resurrect-legacy-atomic-lane-mapping.md`

## Context

mutsu has two mechanisms for an atomic scalar:

1. **The cell lane.** A binding that lives in a shared `ContainerRef`
   (`atomic_scalar_cell` / `self_attr_cell_target`) *is* the atomic primitive:
   its mutex serializes the RMW and every alias — a sibling closure, a spawned
   thread's clone — holds the same cell. `my atomicint $x` takes this lane.
2. **The legacy name-keyed lane.** Everything else — notably a plain untyped
   `my $x` that some code `cas`es — is routed through a process-global
   indirection in the root `shared_vars` store:
   `__mutsu_atomic_name::<name>` -> `__mutsu_atomic_value::<N>`, with the
   value living under the second key. `atomic_value_key_for_name`
   (`src/runtime/builtins_atomic.rs`) lazily creates that mapping and mirrors
   it into the calling frame's `env`; `reset_atomic_var_key`
   (`src/runtime/runtime_shared_vars.rs`) retires both entries on every plain
   scalar assignment.

The legacy lane's whole reason to exist is that it gives one process-wide
answer to "what is the current value of this atomic scalar", the way the cell
does for lane 1.

### The bug

It did not. A thread whose `env` snapshot predates writes other threads
published could publish an arbitrarily old value into the lane as
authoritative, and the blanket reconcile (`sync_shared_vars_to_env`) then
overwrote the awaiting thread's strictly newer value with it. This is a lost
update, and in the 3-argument `cas` shape it is worse than a lost update: the
compare *succeeds* against a value that is not there.

```raku
my $x = 1;
my $go = Channel.new;
my $pB = start { $go.receive; cas $x, 1, 99 };   # B captures $x's env NOW
$x = 4;
Promise.allof(start { $x = 5 }).result;          # $x is 5 by every account
$go.send(1);
say "cas returned: ", $pB.result;                # raku: 5   mutsu: 1
say $x;                                          # raku: 5   mutsu: 99
```

B's `cas` expected `1`, the real value was `5`, and mutsu swapped anyway.

### Root cause, established with `rust-gdb` rather than assumed

Breakpoints on the lane-allocation site (`builtins_atomic.rs:168`), on
`reset_atomic_var_key`'s removal (`runtime_shared_vars.rs:861`) and on the
reconcile's update push (`runtime_shared_vars.rs:655`) produced the whole
sequence directly:

- `Thread 2 "mutsu-main"` allocates `__mutsu_atomic_value::1` for name `x`
  from `builtin_cas_var`.
- `Thread 2` runs `$x = 4` -> `exec_set_local_op_inner` ->
  `reset_atomic_var_key(name="x")`, which removes
  `__mutsu_atomic_value::1` and `__mutsu_atomic_name::x` from the root store.
- **`Thread 3 "pool"`** — the stale-spawned worker — reaches
  `atomic_value_key_for_name(name="x")`, finds nothing in its own `env` and
  nothing in the root store, and allocates a *brand-new* mapping
  (`__mutsu_atomic_value::2`).
- Back on `Thread 2`, `$pB.result` -> `dispatch_promise_method` ->
  `sync_shared_vars_to_env` resolves the dirty bare name through the
  resurrected mapping: `name_key = "__mutsu_atomic_name::x"`,
  `value_key = Some("__mutsu_atomic_value::2")`, and pushes that value into
  main's `env`, clobbering `5` with `1`.

The originating ticket named this "resurrection of a retired mapping" and
proposed either (a) a generation counter to detect the stale re-creation or
(b) refusing to let a stale thread create a mapping at all. Reducing the
repro showed **the ticket's framing is narrower than the actual defect**: the
priming `cas` is not needed. Delete it and the bug still fires, because there
was never a mapping to retire — thread B simply creates the *first*
generation:

```raku
my $x = 1;
my $go = Channel.new;
my $pB = start { $go.receive; cas $x, -> $v { $v } };
$x = 4;
Promise.allof(start { $x = 5 }).result;
$go.send(1); $pB.result;
say $x;   # raku: 5   mutsu: 1
```

So the retirement/resurrection cycle is a *symptom*, not the cause. The cause
is one line in `atomic_current_value`:

```rust
let current = shared.get(value_key).cloned()
    .or_else(|| self.env.get(name).cloned())   // <- private snapshot
    .unwrap_or(Value::NIL);
```

When a lane generation is freshly created, nothing lives under its
`value_key`, so the lane bootstraps its value from **the acting thread's own
`env`** — a snapshot taken when that thread was cloned. For any thread that
was spawned before a write it did not observe, that is an arbitrarily old
value, and it immediately becomes the process-wide authority.

A second, independent unsoundness sits alongside it:
`atomic_value_key_for_name` short-circuited on its own `env` mirror of
`__mutsu_atomic_name::<name>` *before* consulting the store. Since
`reset_atomic_var_key` can only reach the `env` of the thread that ran the
assignment, another thread keeps handing out a retired slot that nothing
writes any more.

## Decision

**The legacy atomic lane is a process-global mechanism, so both its mapping
and the value a new generation starts from must come from process-global
state — never from a frame's private `env`.**

Concretely, two changes in `atomic_value_key_for_name`:

### D1 — the root store is the sole authority for the mapping

The `env` entry is demoted to a pure mirror. The lookup consults the root
store first (a read lock, so a plain read of an atomic-touched variable
through `exec_get_local_op_inner` -> `builtin_atomic_fetch_var` stays off the
writer lock), and refreshes the `env` mirror from what it finds. A mapping
another thread retired can no longer be resurrected out of a stale mirror,
and a mapping another thread created is picked up immediately.

### D2 — a new generation is anchored to the published value

When, and only when, the lookup actually creates a new generation, the new
`value_key` slot is seeded from the process-global *published* value for the
bare name instead of being left empty for `atomic_current_value` to fill from
`env`. "Published" is deliberately the definition the rest of the shared-var
machinery already uses (`published_atomic_seed`):

- `shared_vars_active` — otherwise there is no cross-thread state at all and
  the thread's `env` is the only truth (single-threaded programs are
  byte-identical to before).
- **not** in `thread_redeclared_vars` — a re-declared name is a fresh
  frame-local binding that merely shares a spelling with the store's entry.
  This is the same exclusion `sync_shared_vars_to_env` applies to its own
  dirty-key list.
- **dirty** (`is_shared_var_dirty`) — `clone_for_thread`'s spawn-time seeding
  deliberately does *not* mark a key dirty, so a merely-seeded entry carries
  no more information than the thread's own `env` and must not displace it.
  Only an explicit `set_shared_var` write marks a name dirty, and that is
  exactly "some thread published a value here".

If no published value exists, nothing changes: the `env` fallback remains,
and it is correct, because "absent from the store" means no other thread ever
published this name.

### Why this and not the ticket's two candidates

- **(a) A generation/version counter.** It detects the stale re-creation but
  does not repair it: a thread identified as stale still has to read *some*
  value, and its `env` is the only thing a counter gives it. It also cannot
  reach the no-priming-`cas` repro at all — there is no previous generation to
  compare against. Worse, bumping a per-name counter would have to happen on
  every plain scalar assignment, because a lane can be created for the first
  time *after* the assignment; `reset_atomic_var_key` is on the hot path of
  every scalar store and is guarded by `atomic_var_seen_anywhere()` precisely
  so it costs nothing before any atomic exists. Taking a root-store write lock
  per assignment to maintain a counter is a real cost for a mechanism that
  still would not answer the question.
- **(b) Refuse to let a stale thread create a mapping.** This forces an
  "explicitly stale" path that has to be defined anyway, and defining it lands
  on exactly D2's answer — read the published value. Refusal without D2 leaves
  the thread with no value to operate on.

D2 is (b)'s intent achieved without needing to *classify threads at all*: the
lane is re-anchored to the published consensus unconditionally at creation, so
a stale thread and an up-to-date thread converge on the same reading and no
staleness oracle — necessarily an incomplete one — is involved. Per CLAUDE.md's
gain/risk definitions this is the point: a mechanism whose correctness does not
depend on an enumeration of thread states cannot go flaky when the enumeration
turns out to be incomplete.

### Why the fix is at the read side and not at the reconcile

The reconcile is doing its job: it propagates a worker's write back to the
awaiting thread. The defect was that the worker's write was computed from a
value it was never entitled to call current. Fixing the read makes every
downstream consumer — the reconcile, `cas`'s compare, `atomic-fetch-add`'s
addend — correct at once, whereas a reconcile-side veto would leave `cas`
itself still comparing against a stale value and still returning the wrong
answer to the worker.

## Consequences

- A `cas`/`atomic-*` on a cross-thread-shared plain scalar now reads the
  published value on the first touch of each lane generation, instead of the
  acting thread's snapshot.
- Single-threaded programs, and threaded programs whose atomic scalar was
  never explicitly published cross-thread, take an identical path to before.
- One extra root-store read-lock acquisition per `atomic_value_key_for_name`
  call in exchange for dropping the `env` fast path. Every caller takes the
  root lock immediately afterwards regardless, so this is not a new lock in
  any atomic operation's critical path.

## Not addressed (recorded residuals, not regressions)

1. **The legacy lane is keyed by bare name, process-wide.** Two unrelated
   `my $x` bindings in different spawn lineages share one lane, because
   `__mutsu_atomic_name::<name>` is an internal key and therefore resolves at
   the root store (`SharedStore::is_internal_key`). The code already
   acknowledges this (`atomic_cell_update`'s doc comment; the reason
   `reset_atomic_var_key_decl` exists). The real cure is to retire the legacy
   lane in favour of the cell lane for every atomic scalar, which is an
   ADR-0025/ADR-0013-scale campaign and not this decision.
2. **`builtin_cas_var` resolves `value_key` once at entry** and does not
   re-resolve inside its retry loop, so a lane retired by another thread's
   plain assignment *during* a `cas` leaves the loop writing a retired slot.
   That program is already racy by Raku's own rules (an unordered plain
   assignment concurrent with a `cas`), and re-resolving mid-flight changes
   which slot the compare is against — a separate semantic decision. Left as
   a `// TODO:` at the site.
3. **`reset_atomic_var_key` does not clear the retired `value_key` from
   `shared_vars_dirty`**, so that process-global set grows by one entry per
   lane generation. Harmless for correctness (the reconcile finds nothing
   under a retired key and pushes no update), tracked in
   `todo/tickets/retired-atomic-value-keys-leak-into-shared-vars-dirty.md`.

## Verification

- The primary repro and five neighbouring shapes (no-priming-`cas` variant,
  3-argument `cas`, three stale-spawned threads, `atomicint` `⚛++`,
  `atomicint` `atomic-fetch-add`) all match `raku` exactly. The two
  `atomicint` shapes take the cell lane and were already correct — they are
  kept as the symmetry pin.
- 60 consecutive runs each of the four legacy-lane repros: 240/240 correct,
  0 failures. Before the fix all four were wrong on every run.
- Pinned by `t/atomic-lane-stale-thread-anchor.t`.
- `t/cross-thread-shared-var-writeback-coherence.t`,
  `t/atomic-cell-shape-refusal-symmetry.t`, `t/lock.t`, and the full
  atomic/`cas`/lane group (`t/atomic-*.t`, `t/cas*.t`,
  `t/gate-b-atomic-var-env-sync.t`, `t/shared-store-lineage-scope.t`,
  `t/sibling-thread-array-lane-scope.t`, `t/shared-var-lane-param-rebind.t`,
  `t/thread-uncaptured-container-lane.t`) all pass.
- `make test` and a targeted `roast/S17-*` sweep are green.
