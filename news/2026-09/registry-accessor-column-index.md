# sync_accessor_entries no longer scans the whole method table

`Registry::sync_accessor_entries` re-derives one class's auto-generated
attribute accessors into the canonical `method_entries` table. To clear the
previous derivation it scanned **every row of the table** looking for rows
owned by that class with an `accessor` column — an O(total methods) sweep per
call, documented at the time as "deliberately not index-accelerated" because
accessor-only rows are not covered by `owner_method_names` (that index is
scoped to the user-method column).

Measurement caught up with the note. `Interpreter::new` calls it once per
built-in class, so interpreter construction was quadratic in the built-in
method table, and a callgrind profile of `benchmarks/bench-yaml-parse.raku`
attributed **12.7%** of the whole run to the `Vec::from_iter` behind that one
scan (92955 calls — the benchmark's regex paths were building ~200 scratch
interpreters per parse).

`Registry::owner_accessor_names` is the accessor-column twin of the existing
reverse index, so the stale set is now read directly and the call is
O(the owner's attributes). `sync_accessor_entries` is the only writer of the
`accessor` column, and no other mutator can drop such a row behind its back —
`entry_is_live` keeps a row alive while its `accessor` is set — so the index
is exact. `replace_method_entries_from` copies it alongside the table it
derives from, and the `MUTSU_CHECK_METHOD_INDEX` debug verifier checks both
directions of it the way it already did for the method half.

Found while profiling `todo/perf/yaml-parse-throughput.md` (round 10); the
same round then removed most of the calls themselves by not building the
built-in registry for scratch interpreters at all
(`news/2026-09/scratch-interpreter-skips-builtin-registry.md`), which is why
the residual cost of this site is now ~2.5% of that benchmark rather than 12.7%.
