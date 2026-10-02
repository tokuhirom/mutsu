# A compiled `|` ranks its branches with one NFA run

The compiled engine's `|` (`RxOp::LtmAlt`, `rx_ltm_order`) ranked its branches by measuring each one's
declarative prefix with an NFA run of its own: `ltm_branch_rank_key` once per branch, so
`[ 0 | <[1..9]> <[0..9]>* ]` was two runs per evaluation, and each branch had its own NFA with its own copy of
every rule it can reach. It now builds one NFA for all the branches and runs it once, the way a proto's
candidates have been ranked since [#10671](https://github.com/tokuhirom/mutsu/pull/10671)
([#10648](https://github.com/tokuhirom/mutsu/issues/10648)).

`NfaBuilder::build_roots` is the one builder behind both: `build_proto` (a proto's candidates, each in its own
package) and the new `build_alternation` (a `|`'s branches, in the package the match runs in) compile every
pattern as a root of one `LtmNfa`, and `Interpreter::ltm_measure_roots` runs it once and hands back each
root's `LtmMeasure`. `ltm_rank_proto` filters and sorts those as before; the new `ltm_rank_alternation` keeps
every branch and sorts by `LtmMeasure::branch_rank`, the `(prefix_len, litlen)` formula that
`ltm_branch_rank_key` (the walk's per-branch ranking) now shares instead of repeating. The NFA is cached on
the `|`'s own `LtmAltTable`, per package and token generation (`LtmNfaSlots`, the same slot list a pattern
keeps for itself).

Measured on `benchmarks/bench-grammar-parse-big.raku` (callgrind, profiling build, `BENCH_DET=1`, second run,
both sides built from the same `main`):

| | before | after |
| --- | ---: | ---: |
| whole parse | 122,208,826 Ir | 121,409,026 Ir (-0.65%) |
| `rx_ltm_order`, inclusive (3,200 calls) | 9.02 M | 7.71 M |
| `ltm_measure` calls | 6,401 | 1 |
| NFA construction, inclusive | 2.65 M (5 builds) | 2.65 M (1 build + 2 `build_alternation`) |

The call count falls from one run per branch to one run per `|`, which is what the issue asked for, but the
instruction count barely moves: a merged run still matches every root's leaves, and that is the cost
(`match_consuming_atom`: 55,271 calls from the NFA runs of this benchmark, 8.8 M, 7.3% of the parse), not the
per-run setup this removes. The follow-up for that is [#10710](https://github.com/tokuhirom/mutsu/issues/10710).

`t/regex/syntax/regex-ltm-alternation-one-nfa.t` pins that a branch's fate, `_LL` literal or walk through a
shared rule never leaks into another's, and that a derived grammar ranks with its own rule.
