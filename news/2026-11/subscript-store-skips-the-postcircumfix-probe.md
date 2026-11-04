# A subscript store skips the user `postcircumfix` probe when no such routine is in scope

Since #16294 every `@a[$i] = $v` / `%h{$k} = $v` into an `Array`, instance or mixin cloned its
arguments two or three times and asked three places for a user `postcircumfix:<[ ]>` candidate
(the block's lexical operator scopes, a `sub EXPORT` family, the function table) to learn whether
one decides the stored element: ~2.6k instructions and ~3 allocations per store, with no such
routine anywhere in nearly every program. The probe now first asks whether any of the three can
answer: the lexical scope and the exported family are one lookup each, and the function table
is scanned for the operator's name once per table version (`FunctionTable::version`, renewed by
every routine install), so a routine declared later is still seen.

`scripts/bench-det.sh benchmarks/bench-index-store.raku` (profiling build): 6.69e9 instructions /
1,715,502 allocations on main -> 5.34e9 / 216,080 (JIT on: 6.45e9 / 1,718,765 -> 5.09e9 /
219,341). Allocations are back at the pre-#16294 level (~215k); instructions are within 4% of the
pre-regression 5.15e9. Refs #16320.
