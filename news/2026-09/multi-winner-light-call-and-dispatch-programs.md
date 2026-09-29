# A multi call runs its winner like a plain sub, and value checks run from a per-type program

Two costs dominated a `multi` call after #10107's first two slices, and both were structural.

**The winner took the heavy call path.** A plain `sub` call binds through the positional-light
path (or TRIR). A resolved multi candidate was barred from both and always went through
`call_compiled_function_named`, the general binder that handles every signature shape. That cost
~13k instructions per call, even for a nominal multi whose winner comes from a cache. The light
paths were closed to multis for two reasons. Their caches are keyed by name, so a later call with
different argument types could be handed the wrong candidate. And they never pushed the multi
dispatch frame that `nextsame`/`callsame`/`samewith` read. A resolved winner now takes the
positional-light bind whenever its signature and the call qualify. It skips the name-keyed caches
and pushes the dispatch frame and `samewith` context around the call. A winner with a `where`
clause qualifies too, because resolution has just run that clause against these exact arguments
(the same trust `pending_skip_where_recheck` gives the general binder, #8697).

**Every candidate was re-checked from scratch.** A value-dependent multi's plan (#9967) already
fixes, per argument-type key, which candidates are gathered and in what order. Each call still
re-derived what that key decides, for every candidate: arity, the nominal type checks, and
native-literal admission. A plan stage now builds a dispatch program from the first call with its
key. The program records each candidate's nominal verdict and keeps only the checks that read a
value: a `subset` constraint and an inline `where` predicate. Later calls run just those checks
and hand the matches to the same settling code the general walk uses (`settle_ranked_matches`,
split out of `bind_ranked_candidates`). So ties, `is default` and `X::Multi::Ambiguous` come out
exactly as before. A stage gets a program only when all of its candidates have plain positional
signatures and a rank that does not read values; any other stage keeps the general walk.

On the #10107 repro (a subset candidate, a `where` candidate and a nominal one), measured with
callgrind as the 3000-iteration loop minus `^0`:

| shape | before | after |
|---|---|---|
| #10107 repro | 45.2k | **29.7k** |
| cached nominal multi (`Int`/`Str`) | 24.9k | **14.4k** |

That is 178.7k → 29.7k over the three slices of #10107.

On `benchmarks/bench-multi-dispatch.raku` (release build, 4-core container, noisy wall clock), the
nominal section went from ~0.19 s to ~0.09 s. The subset/`where` section went from 2.53 s before
#10107 to ~0.15–0.24 s; rakudo runs it in ~0.06–0.09 s.
