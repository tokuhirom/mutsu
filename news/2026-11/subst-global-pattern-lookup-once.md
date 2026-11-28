# `.subst(rx, ..., :g)` looks a static pattern up once per walk

`.subst` finds every occurrence first (`subst_scan_matches`) and evaluates the
replacements afterwards. Each search went through
`regex_match_with_captures_from_target`, which re-ran the match-time-scalar
scan, the static-pattern scan and the parse-cache probe for every occurrence:
about 5.2k instructions per match on a short pattern. No user code runs between
those searches, so a static pattern is now resolved once for the whole walk, as
`split` already did (`regex_walk_pattern`). A pattern that interpolates a
variable keeps the per-match lookup.

On the closure-replacement line of `bench-regex-global.raku` (`profiling`
build, `MUTSU_JIT=off`, callgrind, per operation of 40 occurrences): pattern
lookups 40 -> 1, **1.89M -> 1.68M instructions (-11%)**. The wall clock barely
moves (10.2 -> 9.9 ms locally for 45 operations). What remains on that line is
the closure call itself and the per-occurrence `$/` publication (#19296).
