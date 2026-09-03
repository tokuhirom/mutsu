# Routine calls no longer take a process-global atomic to number themselves

Every `RoutineFrame` carries an `invocation_id`, a monotonic number that
distinguishes one call of a routine from the next. It exists for one narrow
purpose: a per-call anonymous state variable (`$++` inside a block inside a
routine) keys its storage on it, so the block's counter restarts when the
enclosing routine is called again.

It was minted with an `AtomicU64::fetch_add` on a process-global counter — on
the entry path of *every* routine call, including the fast positional-light
path. A `lock xadd` is not free: in a `benchmarks/fib.raku` profile the
instruction immediately after it carried 7.7% of `call_compiled_function_positional_light_at`'s
self time, the usual way a locked RMW's latency is attributed.

The id is an opaque discriminator. All it has to be is unique among
concurrently live frames and never 0 (0 means "the mainline is the innermost
scope"). Global monotonicity across threads was never a requirement — only
uniqueness. So each interpreter now claims a *block* of 4096 ids and counts
inside it with a plain increment, refilling from the global counter when the
block runs out. Ids stay globally unique; the atomic fires once per 4096 calls
per thread instead of once per call. Blocks are never returned, which at 2^64
ids is not a budget worth managing.

Measured at **−1.3% cycles** on `fib` and `bench-fib`, and unmoved on
`bench-tak`, `method-call` and `bench-class`. Retired instructions are flat
(+0.08%) and *cannot* judge this change — replacing one locked instruction with
three unlocked ones removes latency, not work — so the figures come from a
same-binary A/B: one build carrying a temporary `MUTSU_ID_ATOMIC=1` switch that
takes the old path, alternated against itself. That matters here, because a
cross-build comparison of the same change reported −4.0% / −3.4% / −3.5% on the
same three benchmarks; the extra 2-3 points were build-to-build codegen
variance, the same trap documented in
[the dispatch-cache entry](call-dispatch-inline-cache.md).

It is also a reminder to read a profile's attribution sceptically: 7.7% next to
the `lock xadd` turned into 1.3% when the atomic was actually removed. The
locked RMW's latency was overlapping with surrounding work, so most of what the
sample pointed at was never on the critical path.
