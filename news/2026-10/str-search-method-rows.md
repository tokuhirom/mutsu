# Built-in method rows take arguments: Str's search methods

The built-in method table (ADR-11276) now holds rows that take arguments. The
first family to use them is Str's search family: `contains`, `starts-with`,
`ends-with`, `index` and `rindex` with one needle, and `substr` with a start
and an optional length. Each of them is a row owned by `Str` and by `Cool`,
and both rows share one handler.

A row is keyed by its argument count, so `substr` has a one-argument row and a
two-argument row. The table hands a row only plain `Str` or number arguments.
Calls with anything else keep their existing path: a named argument, a
Junction (which has to autothread), a Regex needle, a `WhateverCode` or
`Range` position. A row may also decline arguments outside its signature.
`substr`'s row takes only a non-negative `Int` start inside the string; a
start past the end still answers its `Failure` through the general path.

The call-site lane in front of `CallMethodMut` now answers these calls when
they have up to two positional arguments. Its per-site memo also remembers
misses, and the table's first test checks the argument count. Without those, a
call whose name has a row for some other receiver type or argument count
repeated the lookup on every call.

Callgrind, profiling build, 200,000 calls each, second run:

| benchmark | before | after | change |
| --- | ---: | ---: | ---: |
| `$s.contains($n)` | 2,168M | 623M | -71.3% |
| `$s.starts-with($n)` | 2,182M | 612M | -71.9% |
| `$s.rindex($n)` | 2,099M | 595M | -71.7% |
| `$s.index($n)` | 2,149M | 633M | -70.5% |
| `$s.substr(6)` | 1,552M | 534M | -65.6% |
| `$s.substr(6, 5)` | 1,544M | 549M | -64.4% |
| `$i.chars` (Int, no row) | 1,140M | 1,098M | -3.7% |
| `$s.index($n, 3)` (no row) | 5,522M | 5,531M | +0.2% |
