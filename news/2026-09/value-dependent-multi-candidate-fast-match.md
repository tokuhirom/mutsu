# A plain positional multi candidate is matched without the general candidate walk

Every value-dependent `multi` call (one with a `where` clause or `subset` among its candidates)
checks each candidate against the arguments through `args_match_param_types_inner`. That walk
handles every signature shape there is, and it pays for the generality on every candidate of every
call: an env snapshot and scoped child, four scratch vectors, and a sibling-parameter bind per
`where`.

A candidate made only of required scalar positionals now takes a short path, if each parameter is
a nominal type or a `subset` with at most a one-argument WhateverCode `where` (`multi size(Small
$x)`, `multi size(Int $x where * < 10_000)`). The path runs the type checks and the precompiled
inline predicates with `$_` bound, and nothing else. A predicate that names one of the signature's
own parameters (`$x where * > $lo`) still takes the general walk, which binds the earlier
parameters first.

Smaller trims on the same path:

- a subset records its package symbol, and whether its predicate can run inline, once at
  registration, instead of re-interning and re-walking on every check;
- `SetTopic`, which every block runs to publish its value, writes the pre-interned `$_` symbol
  instead of allocating and interning `"_"`;
- a type name's term-binding probe interns the name once.

The #10107 repro went from 56.1k to 45.2k instructions per call (callgrind, the 3000-iteration
loop minus `^0`). On the same binary, the cached nominal multi costs 24.9k and a plain `sub` costs
10.6k. So the value-dependent part of the dispatch is now ~20k over the nominal one; it was ~154k
before #10107's first slice.
