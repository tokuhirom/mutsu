# WhateverCode grep/map callbacks read their topic through a slot

`.grep({ $_ %% 2 })` already read its topic through a local slot that the
loop seeds per element, with no env binding (#19360). `.grep(* %% 2)` did
not get that path. A WhateverCode is built as a block with one parameter, a
plain `$_`, and the path required a block with no parameters at all.

A block whose only parameter is a plain `$_` now qualifies too, with the
same other conditions: untyped, no default, `where`, trait or sub-signature,
and nothing supplied by `.assuming`. Its parameter bind is the topic bind.

On a 10,000-element file, `@arr.grep(* %% 2)` goes from 38.7M to 28.3M
instructions (-27%) and `@arr.map(* * 2)` from 32.2M to 30.4M.
`benchmarks/bench-array.raku` (`BENCH_DET=1`) goes from 218.0M to 200.4M
(-8%). All were measured with callgrind, JIT on.
