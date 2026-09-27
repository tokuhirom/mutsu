# Ecosystem ledger: per-file accepted divergences

DSL::Shared's `t/Array-of-regexes-matches.rakutest` test 6 passes on rakudo only
because of how rakudo caches a regex. It caches a Str interpolated as `/<$rx>/`
by mixing `Match::CachedCompiledRegex` into that Str object in place, which
changes its `.WHICH`, so a later `'string' (elem) @list` is False
([#9746](https://github.com/tokuhirom/mutsu/issues/9746)). No spec requires
this. Reproducing it would mean giving mutsu's value-typed Str an object
identity and an in-place rebless, so the decision is not to copy it.

To keep the ecosystem tools from offering that file again,
`ecosystem/accepted-divergences.toml` now lists such decided divergences per test
file, each pinned to the exact shape of mutsu's failure
([ADR-0130](../../docs/adr/0130-ecosystem-accepted-divergences.md)).
`scripts/ecosystem-sweep.py` grades a matching file `accepted`. That takes it out
of the KPI denominator (the rollup reports the count as `accepted_files`) and out
of the roulette draw and the tickets report. The rest of the distribution stays
measured: DSL::Shared remains `partial` for its other, genuine failure.

A file that starts failing in any other way is graded normally again, so an entry
cannot hide a new bug. `--regrade` applies an edit to the list to the stored
records without re-measuring anything.
