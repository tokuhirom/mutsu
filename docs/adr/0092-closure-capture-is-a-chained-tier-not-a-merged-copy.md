# ADR-0092: Closure capture should be a chained tier, not a per-call merged copy

- **Status**: Proposed
- **Date**: 2026-09-12
- **Related**: [ADR-0018](0018-slot-addressed-lexical-capture-and-env-sync.md)
  (slot-addressed lexical capture and env sync — the capture's other half),
  and the env-tier memo shipped for
  [#7565](https://github.com/tokuhirom/mutsu/issues/7565) (`src/env_tier.rs`),
  whose thesis — *identity is structural, not positional* — this ADR tests and
  finds insufficient here
- **Addresses**: [GitHub issue #7565](https://github.com/tokuhirom/mutsu/issues/7565),
  item 1 of its remaining list

## 1. Context

Every closure call merges the closure's captured env into the callee frame's
overlay, one key at a time, in `call_compiled_closure_in_unit`:

```rust
for (k, v) in data.env.iter() {
    // ... explicit overwrite cases: ContainerRef cells, `self`,
    //     a block's topic and `$!` ...
    self.env_mut().entry_or_insert_sym_with(*k, || v.clone());
}
```

`entry_or_insert_sym_with` is *don't overwrite*: it inserts only when the key is
not visible anywhere in the callee frame's chain. The frame env is a scoped
child of the caller, so "visible in the chain" means "visible to the caller".

**The merge is, in the ordinary case, a complete no-op.** #8019 measured
`insert_sym` reached **zero** times on the loop this ticket is about, over 31
captured keys per call: every captured name is already visible through the
caller's frame chain, because the capture was filtered out of that very chain
moments earlier. The whole cost is `contains_key_sym` proving that, 31 times a
call.

Four slices of #7565 have narrowed what *goes into* the capture (#7707, #7964)
and what the filter *walks* to build it (#8019, #8060), and one has narrowed
what the END-phaser refresh does with it (#8042). None of them touched the
merge, because the fix is its shape rather than its per-key cost.

### 1.1 What it costs, measured

Warm-run callgrind instruction slopes (6 000 → 14 000 iterations,
`MUTSU_JIT=off MUTSU_GC=off`, release) on #7565's loop, against `main` at
`1a49a887` with the merge loop gated out:

| | floor (no `use Test`) | `+ use Test` |
| --- | --- | --- |
| baseline | 77 395 | 88 183 |
| merge loop gated out | 73 518 | 83 352 |
| **saved** | **3 877** | **4 831** |

Two things to read off that table.

**It is the largest remaining item on #7565** — 4 831 against a 10 788-instruction
`use Test` tax.

**It is mostly a *floor* cost, not a tax.** Only 954 of the 4 831 scales with the
import list; the rest is paid by any closure call in any program. The capture of
a closure with **no free variables at all** (`sub ($v) { $v + 1000 }`) is still
31 entries — the built-in dynamics, `Any`, `?FILE`, the topic — and the merge
probes every one of them on every call. So this is not a `use Test` problem that
happens to show up in a benchmark; it is a per-closure-call cost of the
interpreter, and #7565 is merely where it was measured.

For scale, gating out the *entire* capture machinery (build and merge) on the
same loop is 13 363 instructions per iteration on the floor — 17% of it.

## 2. Decision (proposed)

Stop merging the capture into the callee overlay key by key. Make it a **tier**
that the frame's env consults after its parent chain and before `GLOBAL_BASE`:

```
overlay -> caller chain -> capture -> GLOBAL_BASE
```

That is the precedence the merge already implements by hand — caller chain
wins, except for an explicit overwrite list — so the ordinary case needs no
per-key work at all. The overwrite cases (a captured `ContainerRef` cell, a
lexical `self`, a non-routine block's topic and `$!`, the authoritative and
owned capture lists) keep inserting into the overlay, which is above both.

This ADR does not fix the shape of that tier; §3 records three candidate
mechanisms, two of which are already measured out.

## 3. Mechanisms considered

### 3.1 An identity token, keeping the merge — REJECTED (unsound)

The tempting shortcut is to leave the merge in place and skip it when the caller
chain is provably the chain the capture was filtered out of. It does not work,
and the reason is worth recording because the idea recurs:

- **Bare tier addresses are unsound** (ABA: a freed overlay map re-allocated at
  the same address).
- **Pinning each tier's `Arc` makes it sound and expensive** — this is
  `vm_capture_cache`'s mechanism, and holding the creating frame's overlay
  `Arc` forces `Arc::make_mut` to clone that overlay on the *next* write to it.
  In this loop that write is the `my &return = ...` that stores the closure
  itself, so the token would trade a 31-key probe loop for an O(frame overlay)
  map clone per iteration.
- **A key-set generation on `Tier`** — the natural extension of `src/env_tier.rs`'s
  thesis, since the property needed ("every captured name is still visible") is
  a key-set property that a value write cannot disturb — **fails on
  supersets**. Storing the closure into the creating scope *adds* a key between
  capture and call, so exact generation equality never holds on the very
  workload this is for. Relaxing it to "same lineage, no removals since, at
  least as many additions" is unsound: two clones of one tier can each take a
  *different* addition and reach the same counters with different key sets, and
  a closure captured under one but called under the other would silently lose a
  captured name. A globally unique add-stamp fixes the collision and destroys
  the ordering test that the superset case needs.

The general shape of the failure: the merge's precondition is a *superset*
relation between two key sets, and a memo that is cheap enough to be worth
having can only certify *equality*.

### 3.2 Append the capture at the chain's tail — REJECTED (fights `scoped_child`)

Chain the capture below the existing tail, so no `Env` field and no lookup
change are needed: it is just another tier. But `parent` is `Arc<Env>`, so
appending at the tail means rebuilding every tier above it — O(depth) `Arc::new`
allocations per closure call. That is the exact cost `Env::scoped_child` was
tuned to remove ("steady-state recursion therefore allocates nothing here — the
two `Arc::new`s per call were ~14% of fib with the JIT on"), and it also adds a
tier per call, so `MAX_OVERLAY_DEPTH`'s O(env) flatten fires twice as often in
recursion.

### 3.3 A `fallback` tier on the frame env — the candidate

Give `Env` an `Option<Arc<Tier>>` consulted after the chain walk misses and
before `GLOBAL_BASE`. One `Arc` clone per call, no chain rebuild, no depth
change.

It needs two things settled, and they are why this is an ADR rather than a perf
slice:

1. **The lookup split.** `get_sym`/`contains_key_sym` recurse to the chain's
   tail, which is where `GLOBAL_BASE` is consulted. A fallback on the leaf has
   to be consulted between those two, so the recursion needs a base-less form
   with the public entry point wrapping it. These are the hottest functions in
   the interpreter; the change must be measured, not assumed free.

2. **A real semantic change: capture entries stop being visible to callees.**
   Today the merge puts them in the frame's *overlay*, so a routine called from
   the closure body can resolve them by name through the chain. As a fallback
   tier consulted only at the closure's own level, they cannot. That is
   arguably the more correct behaviour — the leakage is dynamic scoping where
   Raku wants lexical — but it is a behaviour change, and `EVAL` (which
   resolves against the live env chain rather than its own lexical scope) is
   the consumer most likely to depend on it.

## 4. Consequences

- The merge's explicit overwrite lists stay exactly as they are; only the
  don't-overwrite default is replaced.
- The exit writeback is unaffected: it iterates the frame overlay, and capture
  entries leaving the overlay is what it already tries to filter out by hand
  ("a captured name, unchanged since capture").
- `flattened`/`filtered_flat_capture`/`tier_addrs`/`tier_maps` must treat the
  fallback as an outermost tier, or a closure created *inside* the closure body
  loses the outer capture.
- If §3.3's item 2 turns out to be depended on, the fallback can be consulted
  by the whole chain rather than by the leaf alone, at the cost of carrying it
  down the recursion.

## 5. What this ADR deliberately does not decide

Whether to do it. The measurement says 4 831 instructions per closure call on
one loop, most of it floor; that is worth a redesign of this size only if
§3.3's lookup split measures neutral on the no-closure path. Whoever takes this
should build §3.3 behind the existing bench harness and measure
`benchmarks/bench-ctor.raku` and `bench-class.raku` (neither imports anything,
both create closures inside method frames) before touching semantics.

## 6. Also measured, and relevant to whoever reads this

**#7565's item 2 — the process-global reflective latch on `skip_env_write` — is
dead.** The ticket body calls it "the worse of the two" costs and #8019 named it
as the prerequisite for the env-tier memo. Gating `skip_env_write` to ignore
`opcode::reflective_name_access_possible()` entirely, on the same loop and
binary as §1.1, is worth **57 instructions per iteration** — 0.5% of the tax,
inside the noise band this filter is documented to have. The memo it was said to
block shipped without it (`src/env_tier.rs`). Nobody should spend a large,
unsound-by-default change on it for this ticket's metric.
