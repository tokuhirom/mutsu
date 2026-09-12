# A closure's captured env is a chained tier, not a per-call merged copy

Every closure call used to merge the closure's captured env into the callee
frame's overlay one key at a time, in `call_compiled_closure_in_unit`:

```rust
for (k, v) in data.env.iter() {
    // ... explicit overwrite cases ...
    self.env_mut().entry_or_insert_sym_with(*k, || v.clone());
}
```

`entry_or_insert_sym_with` is *don't overwrite*: it inserts only when the key is
not visible anywhere in the frame's chain, base tier included. The frame env is
a scoped child of the caller, so that means "not visible to the caller" — and
[#8019](https://github.com/tokuhirom/mutsu/pull/8019) measured `insert_sym`
reached **zero** times on the loop
[#7565](https://github.com/tokuhirom/mutsu/issues/7565) is about, over 31
captured keys per call. The whole cost was `contains_key_sym` proving that, 31
times a call, because the capture had been filtered out of the very chain it was
then compared against.

[ADR-0092](../../docs/adr/0092-closure-capture-is-a-chained-tier-not-a-merged-copy.md)
proposed replacing the merge with a tier the frame's env consults after its
parent chain: `overlay -> caller chain -> GLOBAL_BASE -> capture`. That is the
precedence the merge already implemented by hand, so the ordinary case needs no
per-key work at all. `Env` now carries an `Option<Arc<Tier>>` for it, and the
explicit OVERWRITE cases (a captured `ContainerRef` cell, a lexical `self`, a
non-routine block's topic and `$!`, the authoritative and owned capture lists,
per-instance state) still insert into the overlay, which is above both.

## Both of the ADR's open questions dissolved

The ADR left §3.3 conditional on two things, and neither turned out to be a
cost:

**The lookup split is not needed.** The ADR assumed the capture had to be
consulted *between* the chain and `GLOBAL_BASE`, which would have meant a
base-less recursion with the public entry point wrapping it — a change to the
hottest functions in the interpreter. It does not: the merge's
`entry_or_insert_sym_with` asked `contains_key_sym`, which consults the base
tier at the chain's tail, so the base already beat every captured name. The
fallback therefore goes *after* the base, and the base keeps being consulted
exactly once, where it always was.

**The semantic change is avoidable.** The ADR warned that capture entries would
stop being visible to *callees* of the closure, because they would no longer sit
in the frame overlay a callee chains over. They do not stop: the fallback pass
walks every tier in the chain and answers from the tail-most fallback that has
the key, so a callee resolves a captured name exactly as it did when the merge
put it in the overlay. That also settles precedence between nested closures for
free — the outer closure's frame is the inner one's enclosing lexical scope, and
it is the one nearer the tail.

## The shape of the chain walk mattered more than the fallback did

The first working version was a **regression on most of the saving**, and the
reason is worth recording. `get_sym` ended in `return parent.get_sym(key)` — a
self-call in tail position, which the optimizer turned into a loop. The smallest
correct expression of "consult my fallback once the chain has missed" puts work
*after* that call, which destroys the tail call and costs a real stack frame per
tier. Measured at **+1 591 instructions per iteration**, against the 2 926 that
dropping `contains_key_sym` had just saved.

Spelling the chain walk out as an explicit loop fixes that, and a
`chain_has_fallback` latch keeps the question "is there a capture below me?"
free: it is a one-way flag maintained by `scoped_child` (inherits the parent's)
and `set_capture_fallback` (sets it), and every env built flat has it false,
because `flattened` and the `filtered_flat*` family fold any fallback into the
map they return.

**Where the fallback pass goes then costs more than the fallback does.** Putting
it behind that latch as a `#[cold]` *second* walk of the chain, entered once the
main walk has missed, is the obvious shape — a capture is rare, so keep it off
the fast path — and it cost **+426 on the floor**, worse than the merge it
replaces. The reason is that inside a closure body the latch is *always* on, and
a **miss** is the common outcome there: every speculative metadata probe walked
the chain twice for it. What ships collects the fallbacks during the same walk,
in a separate copy of the loop selected by the latch at the top — so the common
path still pays no per-tier test, and only a closure frame runs the second copy.
That brings the floor to +108.

The same lesson applied a third time: gating `flattened` and `filtered_flat*`
on a chain *walk* rather than the latch cost **0.8% of
`benchmarks/bench-ctor.raku`** on its own — those run per closure creation, and
a walk that finds nothing is pure loss.

## The bug only the full suite found

A `Sub` can be built straight from a *live scoped* env rather than from a
flattened capture — a `whenever` callback is (`react_whenever.rs` passes
`self.env.clone()`), and ~80 sites could be. The merge iterated such an env's own
tier only, which was fine while a closure frame kept its capture *in* its
overlay. Once the capture moved to a fallback, a callback built inside a closure
body silently lost every name that frame had captured.

`Env::capture_tier` is the fix: ordinarily an `Arc` bump, and the union of the
overlay over the fallback when the env has one. `t/concurrency/supply/promise-of-supply-completion.t`'s
three-level nested `whenever` is the pin — `$x` from the outermost `whenever`
reached the second level and not the third.

## The prize shrank while this was in flight

The measurement below is **not** the one ADR-0092 §1.1 predicted, and the reason
matters more than the numbers. §1.1 ablated the merge at 3 877 (floor) / 4 831
(`use Test`) instructions per closure call, over a 31-entry capture of which ~19
were the built-in dynamics. While this was being built,
[#8079](https://github.com/tokuhirom/mutsu/pull/8079) moved those dynamics into
a per-interpreter base tier (ADR-0086) and out of the capture entirely. The
capture is ~12 entries now, so the merge that this removes costs ~1 000, not
~4 000 — and what is left is a near-wash.

Measured against `main` at `8be9152e`, warm-run callgrind slopes on #7565's loop
(10 000 -> 50 000 iterations, `MUTSU_JIT=off MUTSU_GC=off`, release):

| | base | after | |
| --- | --- | --- | --- |
| leaf loop, no `use Test` (the floor) | 67 656 | 67 764 | **+0.16%** |
| leaf loop `+ use Test` | 75 871 | 75 574 | -0.39% |
| **its tax** | **8 215** | **7 810** | **-4.9%** |
| calling loop, no `use Test` | 74 175 | 74 376 | **+0.27%** |
| calling loop `+ use Test` | 84 053 | 83 477 | -0.69% |
| **its tax** | **9 878** | **9 101** | **-7.9%** |

`benchmarks/bench-class.raku` (import-free, closures inside method frames) is
+0.16%. So the honest summary is: **import-using closure code gets 0.4-0.7%
faster, import-free closure code 0.2% slower, and the import tax itself drops
5-8%.** The floor cost is one more `Env` field (`Env` goes to 96 bytes, and it
is moved at ~50 sites) plus one branch at the top of `get_sym`; it is at the
edge of the +-0.8% codegen drift band #7964 documented for this area, but two
independent programs agree on its sign, so it is treated here as real rather
than rounded away.

The case for landing it anyway is architectural rather than numeric: the merge
was a per-call probe loop that provably did nothing, and ADR-0092 §2 is the
shape it should have had. An earlier revision of this change measured -2.0% on
the floor against `main` at `e552c505`; that number is gone, taken by #8079, not
by anything here.

## What is left at this site

One ablation, from a single gated build taken *before* #8079 landed (gated
baseline 75 377 / 84 741, so read the ratio rather than the absolute numbers):

| gated out | floor | `+ use Test` |
| --- | --- | --- |
| the remaining `ContainerRef` scan | -1 405 | -1 736 |
| the fallback install itself | -269 | -123 |

So the install is nearly free and the **scan is now the whole remainder of this
site**. It survives because `ContainerRef`-ness is a property of each value, not
of its key, so the loop cannot be memoized by the key-set index `src/env_tier.rs`
was built for. Making it cheap means a value-derived memo on `Tier`, cleared by
every mutator that can change a value rather than only by those that can add a
key — tractable, because `Tier`'s map is private and all of its mutators live in
that one file, but a deliberate widening of that module's contract rather than a
mechanical change.
