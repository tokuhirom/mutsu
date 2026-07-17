# ADR-0010: Cross-thread lexical sharing is scoped to a spawn lineage, not the process

- Status: Accepted
- Date: 2026-07-17
- Related: ADR-0001 (GC strategy — the cell/by-value guidance), PLAN §6 (dual-store campaign)

## Context

mutsu shares lexicals across threads through `shared_vars`:

```rust
shared_vars: Arc<RwLock<HashMap<String, Value>>>,
```

`clone_for_thread()` — called from all 14 spawn sites (`start`, `Promise`,
`Proc::Async`, supplies, sockets, the scheduler, and the hyper/race batch
threads) — hands the child `Arc::clone(&self.shared_vars)` and copies every env
lexical it can see into that map, keyed by **bare name**. Reads consult it when a
local slot looks stale (`@`/`%` always; scalars when the slot holds `Nil`).

This exists because the child Interpreter gets a *copy* of the parent's env, so a
child's mutation would otherwise be invisible. The name-keyed map is the
band-aid that makes `my $counter = 0; await start { $counter = 42 }` work.

Two properties follow from the shape, and both are defects:

1. **The map is global to the process.** Every clone shares the one `Arc`. There
   is exactly one `depends` key, one `uri` key, for the whole program.
2. **The key is a bare name.** Two unrelated lexicals that happen to share a name
   are the same entry.

### The bugs this actually caused

- **#4650** — zef's early spawns migrated `depends => True` (a `Zef::Client`
  flag). Much later, an unrelated `Zef::Distribution` did
  `my $depends := system-collapse($.depends)`; when that returned `Nil`, the read
  path treated the Nil slot as "uninitialized, refresh from the store" and
  returned the foreign `True`. `zef install` died with
  `Invalid dependency specification: True`. Fixed by masking the read on
  `thread_redeclared_vars` — the same mask the *write* side already applied.
- **The current mzef blocker (#4652)** — `Zef::Client.!fetch` hypers over
  candidates; each hyper worker's block declares `my $uri` and then runs a nested
  `start`. That inner `start` calls `clone_for_thread` *from the batch thread*,
  migrating that worker's `uri` into the same process-global map. N workers race
  on the one key, last writer wins, and every candidate downloads whichever URL
  won. `zef install` cannot fetch a dist that has dependencies.

The per-name mask cannot fix the second one: each batch thread has its **own**
`Interpreter` and therefore its own `thread_redeclared_vars`, but they all share
the one map. The mask is a band-aid on a band-aid.

## Decision

**Scope the shared store to a spawn lineage.** `clone_for_thread` stops
`Arc::clone`-ing one process-wide map. Instead the child gets its own store that
**chains to its parent's**:

```rust
struct SharedStore {
    own:    RwLock<HashMap<String, Value>>,
    parent: Option<Arc<SharedStore>>,
    root:   Option<Arc<SharedStore>>,  // None when self is the root
}
```

(The `root` link is for the internal-key rule that landing this surfaced — see
"Internal keys are not lexicals" below.)

- **Read**: resolve the name in `own`, else walk up the parent chain. A child
  still observes a lexical the parent shared with it.
- **Write**: resolve to the **nearest ancestor that already has the name** and
  write there, so a child's mutation of the parent's `$counter` reaches the
  parent (the property `t/cross-thread-shared-var-writeback-coherence.t` and
  friends pin). If no ancestor has it, the name is this lineage's own.
- **Declaration** (`SetVarDynamic`, i.e. `my $x`): binds the name into the
  **current** lineage's `own`, shadowing any ancestor entry.

Sibling lineages have no path to each other, which is exactly the fix: hyper
workers W1..W5 are siblings, so W1's `uri` and W2's `uri` are different entries
and cannot collide. The nested `start` inside W1 chains to W1, so it still sees
W1's `uri` — the sharing that must work still works.

The declaration rule subsumes `thread_redeclared_vars`: "this name was
re-declared here, so its writes must not leak to the parent" becomes structural
(the name lives in this lineage) instead of a per-name flag consulted at two
call sites that must be kept in agreement — an agreement that was in fact broken,
and was #4650.

## Alternatives considered

### A. Keep the global map, add more masking

Rejected. It cannot address the hyper collision at all (siblings have separate
masks, one map), and #4650 is direct evidence that keeping two call sites in
agreement about a mask is not something we do reliably.

### B. Kill the name-keyed store — share captured lexicals as cells

This is the *correct* end state and ADR-0001's guidance: a captured lexical
becomes a shared `ContainerRef` cell, so parent and child hold one cell and
sharing follows **lexical capture** rather than a name. No map, no keys, no
collisions possible by construction; it is also where the dual-store campaign
(PLAN §6) is heading.

Not chosen *now*, but not rejected as the destination. It requires the capture
analysis to be complete — mutsu's is not (ADR-0001 records that it misses writes
from separately-registered role/class methods and rw-arg sinks), so flipping
every cross-thread lexical onto it today trades a deterministic collision for a
class of silent lost updates. Lineage scoping is compatible with it and shrinks
its blast radius: once captures are cells, the store's remaining users can be
retired lineage by lineage rather than in one flag-day change.

### C. Per-thread store with copy-in/copy-out at join

Rejected. `start` is not joined — the parent reads the lexical after `await`, or
never — so there is no reliable point to copy back. Chaining resolves the write
to the owner at write time, which needs no join hook.

## Consequences

- **The 148 thread/shared/atomic `t/` files are the specification.** They pin the
  sharing that must survive (`t/cross-thread-shared-var-writeback-coherence.t`,
  `t/concurrent-shared-cell.t`, `t/concurrent-state-var.t`, `t/lock.t`, the
  atomic element stores). A red one is a design error here, not a flake.
- `shared_vars_dirty`, the critical-section writeback, and the atomic
  array/hash element stores (`__mutsu_atomic_arr::` etc.) are keyed into the same
  map; the atomic stores in particular are *intentionally* process-wide. See
  "Internal keys are not lexicals" below for how that landed.
- The hyper/race rollback (`shared_var_keys_snapshot` /
  `retain_shared_var_keys`, which exists precisely to stop this op's migrated
  read-only lexicals from shadowing a *later* op's same-named lexical) becomes
  redundant for the batch threads' own entries: those die with the lineage.
  Remove it only once the lineage store is proven, not in the same step.
- Cost: a read that misses walks the chain. Chains are as deep as the spawn
  nesting (2 for the zef case), and the store is only consulted off the fast
  path, so this is not a hot-path concern. Confirm with `MUTSU_VM_STATS` rather
  than assuming.

### Internal keys are not lexicals — they resolve at the root

Landing this surfaced a distinction the "everything is lineage-scoped" framing
missed. The `__mutsu_*` keys — the atomic element stores (`__mutsu_atomic_arr::`,
`__mutsu_atomic_name::`), the `state`-variable cells
(`__mutsu_shared_state::`), the dirty markers — are **not** lexical sharing.
They are explicitly process-wide primitives: `my atomicint $x; await (^4).map: {
start { $x⚛++ } }` must have all four threads hit one counter, and a routine's
`state` is one cell for the whole program. Lineage-scoping them gives each
sibling its own counter/cell and silently loses updates — which is exactly how
`t/concurrent-state-var.t`, `t/module-state-sub-shared-cell.t` and
`t/state-aggregate-shared-cell.t` failed on the first attempt.

So `SharedStore` routes any `is_internal_key` (`__mutsu_` prefix) to the **root**
lineage, and only user lexical names are scoped. `clone_for_thread` never
migrated those keys from the env either, so this is not a special case bolted on
— it is the same line the rest of the runtime already drew.

## Outcome

Landed. The hyper repro prints each worker's own value, and the invariants hold
(child→parent writeback, atomics, #4650's pin) — verified against raku.

**The mask is mostly gone, but not entirely** — recording this honestly rather
than claiming the clean result. Two of `thread_redeclared_vars`' four uses were
removed and the decision is what removed them:

- the `set_shared_var_sym` **write** gate and the `GetLocal` **read** gate (the
  asymmetric pair that *was* #4650) are gone — a re-declared name now lives in
  this lineage structurally, so there is nothing to mask;
- the `sync_shared_vars_to_env` dirty-key filter is gone.

One use remains, and it is a **different concern** from the one this ADR
addresses: `clone_for_thread` still consults the set to decide `declare` vs
`seed_if_absent` when migrating the env. That is about **seed freshness** — if
the parent re-declared `my $x` since its last spawn, the store still holds the
old binding's value and `seed_if_absent` would skip, handing the child a stale
value. It is not about a write leaking to the wrong lineage. Retiring it needs
declaration to push into the store eagerly at `SetVarDynamic` rather than lazily
at the next spawn; out of scope here.

## Validation

- `t/shared-store-lineage-scope.t` — the new pin (hyper siblings, nested `start`,
  child→parent writeback, atomics).
- `t/shared-var-nil-redeclared-mask.t` (#4650's pin) passes **with both gates
  removed** — the proof the declaration rule subsumes them.
- The 149 thread/shared/atomic/promise/lock/supply/state `t/` files: all pass.
- `make test`: 1785 files / 17461 tests, all pass.
- Full `make roast`, plus the gc-stress/jit-stress jobs, via CI.
- **`zef install Test::META` gets past fetch**, the goal that motivated this: all
  16 resolved candidates now download their own archive (every `Fetching [FAIL]`
  is gone). It stops at the *next*, unrelated blocker — `Zef::Extract`'s
  `extract-matcher` rejects the fetched `.tar.gz` ("Enabled extracting backends
  [git tar unzip path] don't understand ..."), which single-dist extract does not
  hit. That is the new frontier, tracked in `docs/mzef-install-pipeline.md`.
