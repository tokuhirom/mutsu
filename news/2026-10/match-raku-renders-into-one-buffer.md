# `Match.raku` renders into one buffer and escapes `orig` once

`Match.raku` prints every nested node with the whole subject (`:orig(...)`).
It built each node's text as its own `String` and copied the child text into
the parent's, so a match nested `d` deep copied the same text `d` times, and
escaped the subject again at every node with six `replace` passes. A wrapper
that prints `$result.raku` at every level of a recursive token (the shape of
Grammar::Extractor's debug tracing) therefore cost cubic time in the
recursion depth.

The text is now written straight into one buffer, and a match escapes its
`orig` once and copies the result for every node that shares it. The output is
byte-identical (`t/regex/match/match-raku-nested-escape.t`, plus the
existing `match-raku-*` tests).

Measured with callgrind on the `profiling` build, second run, on
`Grammar::Extractor` parsing `"." x 150 ~ "42"` with `grammar { token TOP { . <TOP> | \d+ } }`
under its wrapper: 1,425.6M instructions before, 638.7M after the first
change; wall clock for depth 400 went from 2.0 s to 0.93 s. Time is still
superlinear in the depth (250: 0.40 s, 500: 1.17 s), so the issue's linear-time
close condition is not met yet: the rest is the output volume itself (each of
the `d` nodes prints `d` characters, per call) plus per-level allocation.
