# A regex literal's smartmatch stops publishing what its closure carries

`$line ~~ / $needle \d+ /` wrote `$needle` from its slot into the env twice
per match: once before the RHS ran, so the regex engine could read it by
name, and once more from the RHS value, whose pattern is that same literal.
Neither write was visible to the match. The literal compiles to a
`LoadRegexClosure` that captures `$needle` from its slot, and the match
installs the closure's scope over the env for exactly those by-name reads
(`install_regex_closure_scope`).

The op now skips both publishes when the RHS is a lone regex literal load
whose closure captures every name the pattern reads from a slot that holds a
value. In that case `capture_regex_closure` takes each value from the slot,
never from the env. Any other RHS publishes as before: a computed regex, an
adverbed literal, an unset variable, or a name the closure does not capture.

`bench-regex-match` line 7 shape, `MUTSU_JIT=off`, callgrind slope: **47.1k ->
40.5k instructions per match (-14%)**, and 55 -> 51 allocations per match
(#19296).
