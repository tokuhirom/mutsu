# A subscript read skips the user `postcircumfix` resolution when no such routine is in scope

Every `@a[$i]` / `$obj[$i]` read whose target is an `Array`, instance or mixin built an argument
vector, cloned it twice, and then asked three places for a user `postcircumfix:<[ ]>` (the
block's lexical operator scopes, a `sub EXPORT` family, the function table; #15277 added the
exported-family arm and its second clone). With no such routine anywhere, which is nearly every
program, all of it found nothing. The read now asks the same gate the store side uses
(`user_subscript_candidate_possible`, #16320: the lexical scope and the exported family are one
lookup each, the function table is scanned once per table version) and skips the whole block when
it answers no, so a routine declared later is still seen.

`scripts/bench-det.sh benchmarks/bench-index-read.raku` (profiling build): 6.74e9 instructions /
517,206 allocations on main -> 6.56e9 / 217,794 (JIT on: 5.32e9 / 227,124). Both are under the
~6.60G / ~417,950 the series had before #15277. Closes #15394.
