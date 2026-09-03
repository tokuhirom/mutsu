# `Env` carries its own `?FILE` symbol, so a routine-frame push no longer walks the overlay chain

Every `RoutineFrame` push records the file of its call site, and ADR-0037
Slice 1 put a push on all four compiled-call paths — so
`Interpreter::current_source_file_sym()` went from a cold-path helper to one of
the hottest functions in the VM. `news/2026-09/adr0037-routine-frame-push-intern-cost.md`
removed the `Symbol::intern` of the path from it; what remained was the lookup
itself, `env.get("?FILE")` walking the scoped-overlay chain on every call.
Under `perf` that was `Env::get_sym` at 3.7% plus `current_source_file_sym` at
1.6% of `benchmarks/bench-fib.raku`.

`Env` now carries the answer: a `file_sym: Option<Symbol>` field holding the
env's visible `?FILE`, read in O(1) through `Env::source_file_sym()`.

## Why on `Env` and not on `Interpreter`

The obvious shape — mirror `?FILE` on the interpreter next to `cur_source_line`,
maintained by a single write funnel every `?FILE` write goes through — was
built first and **does not work**. `?FILE` is written at only eight sites, all
tidy save/insert/restore pairs, so the funnel itself was easy; but the runtime
swaps whole envs in and out at some fifty sites (`self.env = saved_env`), and a
mirror on the interpreter silently goes stale at every one of them. A debug
assertion added at the same time caught it immediately: six `t/` files failed
with the mirror reading `None` while `env` still held the script path.

Living on `Env` makes all fifty of those swaps correct for free, because the
answer travels with the env it describes. The maintenance surface collapses to
`env.rs` itself: the constructors (`new`, `scoped_child`, `flattened`,
`filtered_flat`, the two `From<HashMap>` impls) and the mutators (`insert_sym`
— now the single funnel for `insert` and the `entry_or_insert*` family —
`insert_through_sym`, `remove_sym`, `retain`, `retain_overlay`). A fresh
`scoped_child` inherits the parent's value, which is correct on the
empty-tier-reuse and `MAX_OVERLAY_DEPTH` flatten paths too: both only skip or
collapse tiers that were already invisible to lookups.

## How it is kept honest

`current_source_file_sym()` carries a `debug_assert_eq!` that re-derives the
answer the slow way — a full chain walk plus an intern — on **every call**. CI
runs the whole `t/` suite on the debug binary (ADR-0014), so an `Env` mutator
that forgets the hook fails 3625 test files loudly rather than silently
mis-attributing backtrace frames in a release build. That is what turned the
interpreter-mirror design's staleness into a five-minute finding instead of a
subtly wrong `.backtrace` shipped to users.

The suite passes with the assertion active, including the paths that broke the
mirror: `require`, `EVAL`, `EVALFILE`, module loading, thread clones, and
`.run`-style script dispatch.

## Result

Local interleaved release A/B (median of 15 alternating runs, idle box):
`hash-access` −5.4%, `fib` −4.9%, `bench-fib` −4.6%, `bench-ctor` −4.5%,
`bench-hash` −3.2%, `bench-tak` −3.0%, `method-call` neutral. Nothing
regressed. The bench CI is the authority for figures that end up in documents.

`Env::get_sym` and `current_source_file_sym` are both gone from the top of the
`bench-fib` profile.
