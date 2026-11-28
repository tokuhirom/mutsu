# `.map` over an Array reads an unobservable topic through its slot

`@arr.map(* * 2)` over an `Array` runs the rw map loop, which aliases `$_`
to each element so a block like `{ $_ *= 10 }` writes through to the source.
For every element it did the following, even for a body that can never write
its topic:

- bound the `_` parameter and `$_` in the env;
- cleared and re-read a write-back key;
- opened a readonly frame scope.

#19360 and #19387 gave the plain map loop a topic slot for exactly such
bodies, but the rw loop never got it.

Now, when the callback is a single implicit topic (no parameters, or the plain
`$_` a WhateverCode has) and its body cannot observe its topic's container
(`topic_alias_unobservable`), the rw loop runs it as the plain loop does. The
element's value is seeded into the body's topic slot. There is no env binding
when the body reads `$_` only from that slot, no write-back, and no readonly
scope. A body that writes `$_`, an `is rw` parameter or a multi-element block
keeps the aliasing loop.

callgrind, profiling build, JIT on, `BENCH_DET=1`:

| file | before | after |
|---|---:|---:|
| `benchmarks/bench-array.raku` | 105,864,458 Ir | 94,904,683 Ir (-10.4%) |
| 10,000 × `@arr.map(* * 2)` | 60,105,570 Ir | 48,217,094 Ir |
