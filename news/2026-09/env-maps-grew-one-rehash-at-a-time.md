# The env maps knew their size and grew one rehash at a time

`bench-ctor`'s round 7, third slice (issue #7568). Rounds 5 and 6 each found
work hidden inside the profile's flat `malloc`/`hashbrown` bucket — a per-call
compile, then per-call name re-derivation. This is a third resident of that
bucket, and the plainest one: **two hot maps were built by inserting into a
zero-capacity `HashMap`, so each one climbed hashbrown's growth ladder on every
single call.**

The tool that showed it is the deterministic allocation counter
(`--features alloc-stats` + `MUTSU_ALLOC_STATS=1`), not a profiler:
`benchmarks/bench-ctor.raku` was spending **1,320,688 allocations on 5000
constructions — 264 per constructed object.**

## What changed

**`Env::filtered_flat` pre-sizes its output.** This is the closure-capture
primitive: it walks every visible env tier and copies the kept entries into a
fresh map. `#5571` already stopped it materializing `GLOBAL_BASE`, and
`vm_capture_cache` memoizes the result — but that memo is keyed on the tier
addresses, so it can never hit for a closure created inside a *method* frame,
whose env is fresh on every call. `@!resources.map(*.flat)` inside `TWEAK` is
exactly that shape. So on this bench it ran in full 5000 times, inserting 31
entries into a map starting at zero capacity: five reallocations and ~52 entry
moves per construction.

The chain's total overlay count is an upper bound on the result (`keep` can only
reject entries, and a shadowing leaf entry overwrites a parent's rather than
adding to it), so one walk up the parent chain gives a capacity that is right by
construction.

**The method frame's env overlay is reserved for its entry-time writes.** A
method frame's overlay starts empty and immediately takes a known, fixed set:
`self`, `__ANON_STATE__`, `?CLASS`, the topic, `$!`, the callable id, then one
per bound parameter. That is 0 -> 3 -> 7 -> 14 on the growth ladder, per call.
The call site knows the count, so a new `Env::reserve` pays one allocation
instead. It goes through the same copy-on-write path the first `insert` would,
so it does not bring the dual-store deep copy forward.

**One thing measured and NOT kept.** `Env::flattened` looks like the same bug —
it clones the root tier's map (exact capacity) and then inserts each tier's
entries on top. Reserving the tiers' total there made the bench *slower*
(+0.06% instructions): most tier keys shadow a root key rather than adding one,
so the reserve buys a guaranteed rehash of an already-large cloned map to avoid
a growth step that usually does not happen. Left alone, with this note so the
next reader does not re-derive it.

## Measured

`benchmarks/bench-ctor.raku`, 5000 constructions, release, callgrind
(deterministic instruction counts; this container has no `perf`):

**1,351,955,875 -> 1,330,520,039 instructions, −1.59%.** Attributed by
building each half on its own: the `filtered_flat` pre-size is about −1.3% of
it and the method-frame reserve about −0.3%.

Allocations over the run: **1,320,691 -> 1,290,691** (264 -> 258 per
construction, −2.3%); bytes allocated **85.5 MB -> 78.8 MB, −7.9%**.

Every other benchmark improves or is neutral — this is env machinery, not
construction machinery (callgrind, same pair of binaries):

| benchmark | before | after | |
|---|---:|---:|---:|
| `bench-ctor`  | 1,351,955,875 | 1,330,520,039 | −1.59% |
| `poly-call`   |   579,516,389 |   574,559,327 | −0.86% |
| `bench-class` | 1,179,747,564 | 1,171,770,757 | −0.68% |
| `method-call` |   849,558,100 |   845,464,187 | −0.48% |
| `bench-fib`   | 1,406,202,385 | 1,406,203,987 | +0.0001% |

**Wall clock:** an interleaved same-session A/B (`taskset -c 2`, best of 15)
reads 0.394s -> 0.391s, about −0.6%. Do not read that as the size of the win in
either direction — this 4-core container's wall clock swings several percent
between runs of the same binary (an earlier best-of-9 pair of the same two
binaries read −2.8%), which is exactly why the ticket's measurement notes send
document numbers to the bench CI (`bench-history.tsv` on `bench-data`) and why
this write-up leads with instruction and allocation counts instead.

## Lesson

`alloc-stats` answers "how much does this region allocate" exactly and
load-independently, and it named this in one run where three rounds of flat perf
profiles had filed it under "malloc, no dominant function". When a profile is
flat and allocation-heavy, count the allocations before reading the samples
again.
