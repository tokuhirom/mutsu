# An LTM measurement drops a thread whose first leaf cannot match

After a proto's candidates and a `|`'s branches were each measured by one NFA run
([#10671](https://github.com/tokuhirom/mutsu/pull/10671), [#10724](https://github.com/tokuhirom/mutsu/pull/10724)),
LTM ranking was still about a quarter of `bench-grammar-parse-big`, and what it spent was the leaves: every
root of a ranking is walked at the same position and nearly all of them fail at their first leaf, each failure
a call into the single-atom matcher (`match_consuming_atom`, about 160 instructions). `value`'s seven
candidates start with seven different literals or classes, and only one of them can start at the character in
front of the run ([#10710](https://github.com/tokuhirom/mutsu/issues/10710)).

A node of the NFA whose every path begins with a leaf that consumes a character (a literal, a class, `\n`),
reached through splits alone, now has a *guard* (`regex_ltm_nfa_guard.rs`): the set of characters that leaf
could match at a start position, which is the ADR-0099 prefilter's `FirstSet`, built the same way, from the
engine's own predicates over the ASCII range and coarse about everything else, never from a table of its own.
The run drops a thread that comes to such a node at a character the guard rejects, before it asks the matcher.
That cannot change a measurement: the leaves it would have asked all fail, and a failing leaf records nothing
(no end, no fate, no `||`, no `_LL` literal). Nodes that do anything besides test a character (calls, returns,
fates, anchors, `<.ws>`, a path that can reach an accept without consuming), `:i` leaves, composite classes
(which can run grammar tokens that record fates), `.`, `\N` and property tests have no guard and run as before.

Measured on `benchmarks/bench-grammar-parse-big.raku` (callgrind, profiling build, `BENCH_DET=1`, second run,
both sides built from the same `main`):

| | before | after |
| --- | ---: | ---: |
| whole parse | 122,159,737 Ir | 112,389,297 Ir (-8.0%) |
| `match_consuming_atom` calls from the NFA runs | 55,271 | 7,894 |
| leaf matches per ranking (7,361 rankings) | 7.5 | 1.07 |
| `ltm_rank_proto`, inclusive (4,161 calls) | 23.5 M | 15.0 M |
| `rx_ltm_order`, inclusive (3,200 calls) | 7.99 M | 6.61 M |
| `LtmNfa::run`, self | 12.2 M | 8.8 M |
| computing the guards, once per NFA | | 0.41 M |

`t/regex/regex-ltm-first-step-guard.t` pins that a guard that is too narrow would be seen: a non-ASCII digit
for `\d`, a no-break space for `\s`, a multi-character grapheme class entry, CR LF for `\n`, the end of the
subject, and a fate or a later leaf after a guarded one (identical output under `raku`); a unit test checks
for 13 atoms and over a thousand characters, each followed by what can change how it groups, that whenever the
matcher accepts a character the guard admits it.

The test surfaced a separate, older bug, not caused by this: a negated class tests only the base character of
a grapheme with a combining mark (`"x\x[301]" ~~ /<-[a..z]>/` is `False`, `True` in Rakudo),
[#10748](https://github.com/tokuhirom/mutsu/issues/10748).
