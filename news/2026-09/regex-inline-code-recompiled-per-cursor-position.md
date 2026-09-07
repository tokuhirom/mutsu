# An embedded regex code block is compiled once, not once per cursor position

`todo/perf/closure-sequence-evolution-performance-gap.md` reported a
closure-driven evolutionary search running ~85x slower than `raku`. Two things
came out of re-measuring it: the headline number was a **debug**-build number,
and underneath it sat a real, general defect — every evaluation of an embedded
regex code block recompiled its AST from scratch.

## The ticket's numbers were debug, the defect underneath was real

The repro (a Weasel-program generation loop whose mutation step is
`$string.subst: /<?{ rand < $chance }> . /, @alphabet.pick, :global`) was quoted
at ~48s against raku's ~0.57s. Re-measured on `cargo build --release` it was
**2.92s against raku's 0.87s** — a 3.4x gap, not an 85x one. The 48s reproduces
exactly on the debug binary (44.8s measured), which is where the ticket's
figures came from.

Decomposing the release run against raku separated the one component that was
actually slower:

| 1000 iterations | raku | mutsu (before) |
| --- | --- | --- |
| `fitness($seed)` | 0.04s | 0.02s |
| `@alphabet.pick` | ~0s | ~0s |
| `max :by(&fitness), \|@candidates` | 0.34s | 0.36s |
| `$seed.subst(/. /, "X", :global)` (plain) | 0.44s | 0.25s |
| `$seed.subst(/<?{ rand < .001 }> . /, …)` | 0.23s | **1.53s** |

Everything except the code-assertion substitution was at parity or better. The
ticket's other hypothesis — that the *combination* was superlinear (components
summing to 7.5s but the combined loop taking 48s) — was also a debug artifact:
in release the combined loop is within a few percent of the sum of its parts.

## Root cause

`eval_regex_inline_code` is the carrier for `{ … }` blocks, `<?{ … }>` /
`<!{ … }>` assertions and `<{ … }>` interpolations. Its *parse* was already
memoized per code string (`REGEX_CODE_PARSE_CACHE`, hits 299,999 of 300,000 on
this benchmark), but it then handed the resulting statements to the uncached
`eval_block_value`, which builds a fresh `Compiler` and compiles them again on
**every** evaluation — and an assertion is evaluated once per cursor position,
so a 29-character subject with `:global` pays 29 compiles per `subst`.

A `perf` profile of 10,000 such substitutions put `Compiler::compile` (6.2%),
`CompiledCode::compute_free_vars` (3.3%), `CompiledCode::new` (1.5%) and
`Compiler::new` (1.2%) at the top, ahead of anything that actually ran the
assertion, plus the malloc/free traffic those four generate.

## Fix

The mechanism to avoid this already existed and was simply not reached from
here: `eval_block_value_cached(body, cache_id)` reuses a compiled `Arc` for the
same `(cache_id, ambient compile context)` pair, and `vm_subst_repl.rs` already
mints such an id for a dynamic replacement body out of the same global counter
as `SubData::id`.

So `REGEX_CODE_PARSE_CACHE`'s entry now carries a third field: a stable
compile-cache id, minted from `crate::value::next_instance_id()` when the code
string is parsed. `parse_regex_code_cached_with_id` hands it back alongside the
statements, and both branches of `eval_regex_inline_code` — the assertion path
and the `writes_back_to_caller` `{ … }` path through
`eval_regex_code_block_body` — go through `eval_block_value_cached` with it.

A **fresh** id is minted on every (re)parse, so an entry invalidated by a
`registry_write_gen` bump can never serve the compile made from the statements
it replaced. Everything else about the cache's soundness is the pre-existing
`CarrierCompileCtxKey` argument: a mismatched ambient context falls back to a
fresh compile rather than serving a wrong one.

Two callers keep the plain entry point on purpose: `eval_regex_repeat_code` and
the `<{ … }>` pattern interpolation build a *scratch* `Interpreter` per call, so
a compile cached in it dies with it.

## Result

Release, `taskset -c 2`, median of 5:

| | before | after | |
| --- | --- | --- | --- |
| the ticket's repro (1000 generations) | 2.92s | **2.03s** | −30% |
| 10,000 `<?{ … }>` `:global` substitutions | 1.53s | **0.83s** | −46% |
| `mutate($seed, .001)` x1000 | 0.25s | **0.16s** | −36% |

`make test` (3,823 files / 40,376 tests) is green, as is a targeted 106-file
roast sweep of every whitelisted S02-names/S03/S05 file containing an embedded
code block or assertion.

## What is left, and what turned up on the way

The remaining gap (2.03s vs raku's 0.87s) is *not* a single hotspot; an LBR
profile attributes 37% of the assertion benchmark to actually running the body
and the other ~55% to setting up and tearing down a carrier block scope around
it. That is recorded, with its measurements and two disproved hypotheses, in
`todo/perf/regex-inline-code-carrier-prologue-overhead.md`.

Writing the regression test also turned up an unrelated correctness divergence —
an assertion's write to an outer **scalar** lexical never reaches the caller —
filed as `todo/tickets/regex-assertion-scalar-write-to-outer-lexical-is-lost.md`.
