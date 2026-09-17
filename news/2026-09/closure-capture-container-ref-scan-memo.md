# Closure-capture merge no longer scans the whole tier for boxed cells

Every closure call scanned its whole captured tier (`Env::capture_tier()`)
looking for `ContainerRef` (box-on-capture) entries to overwrite in the
caller's overlay — even though on the overwhelming majority of calls that
scan finds nothing at all. `ContainerRef`-ness is a property of each value,
not of its key, so it could not ride the existing key-set-derived
`Tier::capture_candidates` memo from earlier #7565 work.

`Tier::container_ref_keys()` (`src/env_tier.rs`) adds a lazily-built memo of
exactly the keys currently bound to a `ContainerRef` cell. It needs no
invalidation logic at all, for a narrower reason than the key-set memo's
"nothing added a key" contract: it is read only through
`Env::capture_tier()`, which only ever looks at a closure's own
already-captured env (`SubData::env`) — set once when the closure literal is
created and never written to again, the same discipline `SubData::body`
already relies on. A tier reached the ordinary way (a live per-call frame
overlay its own frame keeps writing to) never calls this method, so the
"value changed after the memo was built" case this would need to guard
against cannot arise.

The merge site in `call_compiled_closure_in_unit`
(`src/vm/vm_closure_dispatch.rs`) now walks this memoized subset instead of
the whole tier for the `ContainerRef` overwrite exception, and resolves
`self`/the topic/`$!` via direct well-known-key lookups instead of folding
them into the same scan (each is a single specific key, not something that
needs a full-tier walk either).

## Measured effect

Real, closure-and-capture-heavy benchmarks (release, `MUTSU_JIT=off
MUTSU_GC=off`, callgrind instruction counts against `main`):

| benchmark | change |
| --- | --- |
| `benchmarks/bench-ctor.raku` (closures via `.map`, TWEAK submethods) | **-0.19%** |
| `benchmarks/bench-class.raku` | +0.07% (noise-level) |
| `benchmarks/bench-fib.raku` (no closures, a sanity check) | +0.0004% (no effect) |

A synthetic closure created once and called 150 000 times (a stored
callback / dispatch-table shape) improved by **~1.1%** (197 fewer
instructions per call) — the pattern this change amortizes the scan for.

A synthetic closure created *fresh on every loop iteration and called
exactly once* (`#7565`'s own named regression case, `rebound-return-hot-loop`
style) got very slightly slower (~0.3%, ~150-200 instructions/iteration):
there is no repeat use to amortize the memo against there, and the new
`OnceLock` field on every `Tier` costs a small fixed amount per closure
creation regardless. The real-benchmark numbers above say this trade is a
net win in practice; the synthetic single-use case is worth recording so a
future round does not have to re-measure it from scratch.

Full `t/` suite (47042 tests) and `make roast` (aside from the four
container-only failures documented in `docs/agent-environments.md`) both
pass unchanged.

Part of the ongoing `use Test` tax-reduction tracked in
[#7565](https://github.com/tokuhirom/mutsu/issues/7565), which stays open:
the built-in dynamics term and the reflective latch term were struck by
earlier work in this thread, and the residual capture-merge cost is now
small enough that further slices here have diminishing returns.
