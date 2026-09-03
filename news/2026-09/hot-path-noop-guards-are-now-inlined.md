# Three hot-path helpers paid a function call to discover they had nothing to do

Every compiled call runs `apply_pending_rw_writeback`,
`apply_pending_caller_var_writeback` and `resolve_let_saves_on_success`. All
three begin with a guard that is true in the overwhelmingly common case — the
pending list is empty, or no `let`/`temp` was pushed — and their comments say
as much ("no cost for any program that never made a runtime-name write").

But the guard lived *inside* an out-of-line function, so the common case was
never free: it cost a call, argument setup and a return, plus (for
`resolve_let_saves_on_success`) the construction and drop of an empty `Vec`.
On `bench-fib` the three showed up at 1.31% + 1.27% + 1.26% of self time —
entirely to answer "nothing to do".

Each is now an `#[inline]` wrapper holding just the guard, delegating to an
`#[inline(never)]` body. The guard compiles into the caller as a load and a
branch.

## The bug this nearly shipped with

`apply_pending_rw_writeback`'s trailing statement —
`self.apply_pending_caller_var_writeback(code)` — sat *outside* the
`if !...is_empty()` block, so it ran unconditionally. Moving the block wholesale
into the slow body captured that call with it, and the caller-var drain then
only ran when the *rw* list happened to be non-empty. `make test` caught it
immediately (16 files, including `t/runtime-name-write-to-outer-lexical.t` and
the `warn`-resume family). The wrapper now keeps the unconditional drain where
it belongs.

Worth noting for the next such split: the first A/B was run against the broken
binary, and it looked *better* (`bench-fib` −12.2%) precisely because it was
skipping work. Re-measuring after the fix is not optional.

## Measurement

Interleaved A/B of two release builds, median over nine alternating runs on a
pinned P-core, measured after the fix:

| benchmark | cycles | instructions |
| --- | ---: | ---: |
| `fib` | −8.7% | −5.1% |
| `bench-fib` | −8.2% | −5.1% |
| `bench-tak` | −5.3% | −2.6% |
| `bench-hash` | −0.6% | |
| `bench-class` | +0.7% | |

Both orderings were measured on `bench-fib` and `bench-tak`; both signs
flipped. Retired instructions drop 2.6–5.1%, confirming the call overhead was
real work and not a layout artefact.
