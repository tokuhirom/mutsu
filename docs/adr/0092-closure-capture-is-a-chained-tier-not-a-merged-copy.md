# ADR-0092: Closure capture should be a chained tier, not a per-call merged copy

- **Status**: Accepted (implemented 2026-09-12; see §7 for what the
  implementation found)
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

## 5. What this ADR deliberately did not decide

Whether to do it. That is settled by §7: §3.3 was built and shipped.

## 6. Also measured, and relevant to whoever reads this

**#7565's item 2 — the process-global reflective latch on `skip_env_write` — is
dead.** The ticket body calls it "the worse of the two" costs and #8019 named it
as the prerequisite for the env-tier memo. Gating `skip_env_write` to ignore
`opcode::reflective_name_access_possible()` entirely, on the same loop and
binary as §1.1, is worth **57 instructions per iteration** — 0.5% of the tax,
inside the noise band this filter is documented to have. The memo it was said to
block shipped without it (`src/env_tier.rs`). Nobody should spend a large,
unsound-by-default change on it for this ticket's metric.

## 7. What the implementation found (2026-09-12)

§3.3 shipped. Both of the two things it said had to be settled first turned out
not to be costs at all, and the real cost was somewhere neither was looking.

### 7.1 The lookup split (§3.3 item 1) was not needed

§3.3 assumed the capture had to be consulted *between* the chain's tail and
`GLOBAL_BASE`, which would have meant a base-less recursion with the public
entry point wrapping it. It does not. The merge's default was
`entry_or_insert_sym_with`, which asks `contains_key_sym` — and that consults
the base tier at the chain's tail. So the base already beat every captured name,
and the correct order is `overlay -> chain -> GLOBAL_BASE -> fallback`, with the
base still consulted exactly once where it always was.

### 7.2 The semantic change (§3.3 item 2) was avoidable

§3.3 warned that capture entries would stop being visible to *callees* of the
closure. They do not. `get_sym`'s fallback pass walks every tier in the chain,
not just the leaf, and answers from the tail-most fallback that has the key, so
a callee chained over the closure frame resolves captured names exactly as it
did when the merge left them in that frame's overlay. The same rule settles
precedence between nested closures for free: the outer closure's frame is the
inner one's enclosing lexical scope, and it is the one nearer the tail.

### 7.3 The cost was the shape of the chain walk, not the fallback

`get_sym` ended in `return parent.get_sym(key)` — a self-call in tail position,
which the optimizer turned into a loop. The smallest correct expression of
"consult my fallback once the chain has missed" puts work *after* that call,
which destroys the tail call and costs a stack frame per tier: **+1 591
instructions per iteration**, against the 2 926 that dropping `contains_key_sym`
had just saved. Spelling the walk out as an explicit loop and moving the
fallback to a `#[cold]` pass behind a `chain_has_fallback` latch keeps the chain
cost unchanged.

The latch matters a second time: gating `flattened` and the `filtered_flat*`
family on a chain *walk* rather than the latch cost **0.8% of
`benchmarks/bench-ctor.raku`** by itself. Those run per closure creation; a walk
that finds nothing is pure loss.

### 7.4 A `Sub` can hold a live scoped env, and §4's list was one short

§4 listed the readers that must treat the fallback as a tier. It missed the one
that broke: a `Sub` built straight from a *live scoped* env rather than from a
flattened capture (`react_whenever.rs` passes `self.env.clone()`; ~80 sites
could). The merge iterated such an env's own tier only, which was correct while
a closure frame kept its capture *in* its overlay; once the capture moved to a
fallback, a callback built inside a closure body lost every name that frame had
captured. `Env::capture_tier` is the fix — ordinarily an `Arc` bump, the union
of overlay over fallback when the env has one. Pinned by the three-level nested
`whenever` in `t/concurrency/supply/promise-of-supply-completion.t`.

### 7.5 Measured

**§1.1's prize was taken by another change before this landed.** That ablation
put the merge at 3 877 (floor) / 4 831 (`use Test`) over a 31-entry capture, of
which ~19 were the built-in dynamics. #8079 then moved those into a
per-interpreter base tier (ADR-0086) and out of the capture, leaving ~12
entries and a merge worth ~1 000. Measured against `main` at `8be9152e`,
warm-run callgrind slopes, 10 000 -> 50 000 iterations, `MUTSU_JIT=off
MUTSU_GC=off`, release:

| | base | after | |
| --- | --- | --- | --- |
| leaf loop, no `use Test` | 67 656 | 67 764 | +0.16% |
| leaf loop `+ use Test` | 75 871 | 75 574 | -0.39% |
| **its tax** | **8 215** | **7 810** | **-4.9%** |
| calling loop, no `use Test` | 74 175 | 74 376 | +0.27% |
| calling loop `+ use Test` | 84 053 | 83 477 | -0.69% |
| **its tax** | **9 878** | **9 101** | **-7.9%** |

`bench-class.raku` +0.16%. So the decision stands on §2's architecture — a
per-call probe loop that provably did nothing is gone — rather than on a
speedup: import-using closure code is 0.4-0.7% faster, import-free closure code
0.2% slower (one more `Env` field, taking it to 96 bytes, plus one branch at the
top of `get_sym`), and the import tax itself is 5-8% smaller.

**One-walk lesson.** Putting the fallback on its own `#[cold]` second pass over
the chain, entered after the main walk missed, cost **+426 on the floor** —
worse than the merge it replaces. Inside a closure body the latch is always on
and a *miss* is the common outcome, so the chain got walked twice for it.
Collecting the fallbacks during the same walk, in a separate copy of the loop
selected by the latch at the top, brings it to +108.

An ablation of what remains at this site, taken before #8079 landed (so read the
ratio, not the absolutes) says the rest is the `ContainerRef` scan §2
deliberately kept:

| gated out | floor | `+ use Test` |
| --- | --- | --- |
| the remaining `ContainerRef` scan | -1 405 | -1 736 |
| the fallback install itself | -269 | -123 |

The scan survives because `ContainerRef`-ness is a property of each *value*, not
of its key, so `src/env_tier.rs`'s key-set index cannot memoize it. Making it
cheap means a value-derived memo on `Tier`, invalidated by every mutator that
can change a value rather than only by those that can add a key. That is
tractable — `Tier`'s map is private and every mutator is in that one file, which
is the property the module was built for — but it widens that module's stated
contract, so it is a separate decision rather than a continuation of this one.
