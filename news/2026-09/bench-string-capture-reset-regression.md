# `bench-string` regression: the per-match capture reset scanned the whole visible env

The bench CI's `bench-string` row stepped from a raku ratio of ~0.30 to ~0.42
between 2026-08-31 and 2026-09-01 and stayed there — a 45% slowdown, and by far
the largest mover in that window (the next worst was `hash-access` at 1.12x).
Both the interpreter series and the `+jit` series moved by the same amount, so
the cost was in the interpreter, not in the JIT.

## What happened

`fix: keep named captures scoped to routines` (13191415) fixed a real bug:
`reset_capture_env_vars` shadows the previous match's capture variables before
a new match installs its own, but it only looked at `Env::keys()` — this
frame's own overlay — so a capture inherited from a *caller* frame survived
into the callee. The fix switched it to `Env::visible_keys_where`, which walks
the entire visible chain (every overlay tier plus the process-wide global base)
and is correct.

It is also enormously expensive on a hot path. `visible_keys_where` resolves
every key it walks into an owned `String`, collects them into a
`HashSet<String>`, and the caller then re-interns each survivor back into a
`Symbol`. That is O(visible env) work with an allocation per key, run twice per
match. Callgrind put it at **40% of `bench-string`**: 5000 matches x ~75,600
instructions each, and roughly 380 of the benchmark's ~1.9M `malloc`/`free`
pairs per match.

## The fix

The scan asks the wrong question. The names it looks for — all-digit (`$0`,
`$1`) and angle-wrapped (`$<name>`) — are a tiny, fixed vocabulary, while the
env it scans is large and mostly irrelevant.

So the answer now comes from a registry instead of a scan. `Symbol::intern`
classifies each *newly interned* name by capture shape and records it in an
append-only, process-wide list. A name has to be interned before it can be an
env key, so that list is a guaranteed superset of the capture keys any env can
hold: `reset_capture_env_vars` iterates it and probes `Env::contains_key_sym`,
which is O(capture names in the program) with no per-key allocation. Recording
happens while the symbol table's write lock is still held, so a thread cannot
observe a name as interned before it is registered.

The classification runs once per distinct string ever interned — the intern
caches short-circuit every repeat — so the hot path pays nothing for it.

`visible_keys_where` itself is unchanged and still used by the two cold callers
that need a genuine full-env view (`EVAL`'s redeclaration check and the
pseudo-stash walk).

## Result

Instructions retired for `benchmarks/bench-string.raku` (`MUTSU_JIT=off`,
callgrind):

| build | Ir | vs pre-regression |
| --- | --- | --- |
| a0932728 (pre-regression, 2026-08-31) | 578,335,273 | — |
| main before this change | 945,506,769 | +63% |
| main with this change | 568,702,466 | -1.7% |

`reset_capture_env_vars` drops from 378.2M Ir (40.0%) to 1.85M Ir (0.33%), a
204x reduction, and the regression is fully recovered — slightly past the
pre-regression baseline. Wall clock on the dev box goes 0.106s -> 0.065s.
`word-count`, which also matches regexes in a loop, improves 4%.

`hash-access` / `bench-hash` remain ~12% above their 2026-08-31 baseline. That
is a separate, diffuse regression (every frame in the profile grew
proportionally, with no single hotspot) and is recorded in
`todo/perf/hash-access-diffuse-regression-2026-09.md`.

## Pins

`t/match-vars-are-routine-scoped.t` and `t/capture-var-topic-slot.t` — the two
tests that pin the scoping behaviour the original fix introduced — still pass,
as does the rest of the `t/` suite.
