# A proto call ranks its candidates with one NFA run

A call to `<value>` in a grammar with a proto `value` and seven `:sym<…>` candidates used to measure
each candidate's declarative prefix with an NFA run of its own (`rx_rank_proto` in the compiled
engine and the walk's eager arm, both calling `ltm_measure` once per candidate), so one proto call
was seven runs over the same position, and each candidate had its own NFA with its own copy of every
rule it can reach. Rakudo builds one NFA for the whole proto and runs it once; so does mutsu now
([#10487](https://github.com/tokuhirom/mutsu/issues/10487)).

`NfaBuilder::build_proto` compiles all of a proto's candidates into one `LtmNfa` with one *root* per
candidate. A root is entered through a call frame of its own, and every call stack grown from it
remembers which root it came from (`Stacks::origins`); the rules the candidates call are compiled once
for all of them. The run records what each root found separately (`NfaRun::origins`): its accept, its
fate, whether a `||` was crossed, and the `_LL` literals it crossed. A fate that a leaf's matcher
records into the run's frame (a code block inside a token a `<+name>` class calls) is read after each
leaf of such a run, while the root it belongs to is known. `LtmMeasure::of` turns those into the same
`(prefix_len, litlen)` key a single-pattern measurement gives, so ranking stays one definition, and the
two former copies of the ranking loop (compiled engine, eager arm) are one function,
`Interpreter::ltm_rank_proto`.

The NFA is cached on the candidate list itself (`TokenCandidates`, replacing the bare
`Arc<Vec<ParsedTokenCandidate>>`), so it cannot outlive the candidates it was built from and needs no
invalidation of its own beyond the token generation.

Measured on `benchmarks/bench-grammar-parse-big.raku` (callgrind, profiling build, `BENCH_DET=1`,
second run, same box, both sides rebuilt from the same `main`):

| | before | after |
| --- | ---: | ---: |
| whole parse | 123,079,843 Ir | 110,492,747 Ir (-10.2%) |
| proto ranking, inclusive | 34.8 M (29,127 `ltm_measure` calls) | 22.3 M (4,161 `ltm_rank_proto` calls, one NFA run each) |
| NFA construction, inclusive | 4.27 M (12 builds) | 3.4 M (5 builds + one `build_proto`) |

`ltm_measure` still shows 6,400 calls: those are the branches of `[ 0 | <[1..9]> <[0..9]>* ]`, ranked
one NFA run per branch by `rx_ltm_order` — the issue counted them as proto candidates. They are the same
shape of problem and are filed as [#10648](https://github.com/tokuhirom/mutsu/issues/10648).

`t/grammar/proto-ranking-one-nfa.t` pins that one candidate's fate, `_LL` literal or shared-rule walk
never leaks into another's.
