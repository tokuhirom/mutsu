# Matching a nominally-typed multi method candidate costs ~1k instructions

`method_args_match_for_invocant` opened a full speculative window (scoped env,
captured-scope copy, `self`, the receiver's package, role bindings) for every
candidate of a multi method, then ran the general signature walk. A candidate
whose parameters are plain positionals typed with core classes (`multi method
pick(Int)`) reads none of that window, so it is now matched on the argument
types alone (`match_candidate_by_type_alone`), and a plain `Int`/`Num`/`Str`/`Rat`
value is rejected against a different one of those classes without the general
type checker's gauntlet.

Measured with callgrind on the `Grabonize` repro of #12942 (profiling build, warm,
n=1 vs n=1001, 3 candidates per call): `method_args_match_for_invocant` went from
37.0M to 3.3M instructions over 3003 candidate matches, i.e. ~12.3k to ~1.1k per
candidate; the whole 1001-call run from 152.1M to 118.6M.
