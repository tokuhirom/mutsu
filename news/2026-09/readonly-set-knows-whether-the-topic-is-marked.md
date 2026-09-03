# The readonly set now knows whether `$_` is marked, without hashing

Every routine and method call has to clear a readonly mark on the topic before
binding its own `$_`: a caller's `given`/`with`/`for` may have marked `_`
read-only against a literal, and that must not leak into the callee
(`given 'x' { f() }` where `sub f { $_ = ... }` must not hit "Cannot assign to
a readonly variable").

The guard on that work was `!self.no_readonly_vars()` — "is the readonly set
non-empty". That is true for **any** program with a single readonly parameter
live anywhere on the stack, which is to say almost always: a recursive
`sub fib(Int $n)` marks `n`, so every one of its calls then paid a full hash
`remove` of `_` that missed. Two of the five sites went further and called
`unmark_readonly("_")`, which re-interned the name string as well.

`ReadonlySet` is now a struct carrying the map plus a `topic: bool`, and
`Interpreter::unmark_readonly_topic()` reads it in one branch.

The flag lives on **the set**, not on the `Interpreter`, so every mutation path
maintains it by construction — including `replay_readonly_undo`, which reaches
the set through a raw pointer from a `Drop` impl and never sees the
`Interpreter` at all. `topic_marked()` re-derives the slow answer under
`debug_assert`, and CI runs the whole `t/` suite on a debug binary (ADR-0014),
so 3600+ files check the invariant on every push. This is the same shape as
`Env` carrying its own `?FILE` symbol
(`news/2026-09/env-carries-its-source-file-symbol.md`), and for the same
reason: an `Interpreter`-side mirror of a value the runtime swaps wholesale
goes stale.

## Measurement

This one is small, and the honest evidence is the instruction count rather than
the clock. Interleaved A/B of two release builds, median over nine alternating
runs on a pinned P-core:

| benchmark | cycles | instructions |
| --- | ---: | ---: |
| `fib` | −1.0% | −2.4% |
| `bench-fib` | −0.5% | −2.3% |
| `bench-tak` | −1.5% | −1.0% |
| `method-call` | +1.1% | −0.1% |
| `bench-class` | +0.5% | |

Retired instructions fall on every benchmark measured and rise on none, so
real work was removed and none was added. The cycle deltas at this magnitude
are inside the binary-layout noise floor (`bench-fib` reads −0.5% in one
ordering and +0.5% in the other, i.e. nothing), which is exactly what a change
that removes a handful of cache-resident hash operations per call should look
like.
