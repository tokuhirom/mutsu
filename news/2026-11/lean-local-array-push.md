# `@a.push($v)` on a plain local appends without the general push's probes

`@arr.push($i)` compiles to `ArrayPush`. Its handler asked, on every push,
every question the general push needs:

- whether the argument aliases the target;
- the shared-thread lane;
- whether the target is lazy, immutable, shaped or jagged;
- whether a `Nil` decays to the element default;
- the declared element type and its native integer width;
- whether this is a reference push;
- whether the target is held in a cell.

Most of those went through by-name env probes, at about 3,500 instructions
per push.

A new lane in front, `try_lean_local_array_push`, answers the plain case from
the chunk, the process latches, the container's own metadata and the value:
a declared untyped `@` lexical, an ordinary `Array`, and a plain scalar value.
It then appends in place. It is the push twin of the lean element store
(#19306) and shares its checks. Every other shape still takes the general
push: a typed or native or `is default` array, a reference push, a `Nil`, a
shared array, an immutable `List`.

A 10,000-iteration `for 1..10000 -> $i { @arr.push($i) }` file goes from
54.8M to 25.0M instructions (-54%), and `benchmarks/bench-array.raku`
(`BENCH_DET=1`) from 218.0M to 184.4M (-15.4%). Both were measured with
callgrind, JIT on.
